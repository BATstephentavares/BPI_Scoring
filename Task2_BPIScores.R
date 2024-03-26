###############################################################################
## Purpose: 
## 1. To pull in cleaned data set from Stata.
## 2. To run BPI factor, Empathy Capacity factor, and Anxiety factor.
## 3. To normalize scores for different Receptiviti measures, based on normalized
## 4. Calculate Awareness calculation which is the correlation of BPI and (Drags - Drivers)
##############################################################################

rm(list=ls()) 

require(lavaan)
require(stringr)
require(scales)
require(haven)
require(dplyr)

## A. Set up folder directories

data.folder <- file.path(getwd(),"Data")

res.folder <- file.path(getwd(),"AllScores")

## B. Load in data. 

data <- as.data.frame(read_dta(file.path(data.folder,"BurnTemp.dta")))

data[,c("input_day_c", "overwhelmed","detached","drained","sluggish", "temperature")] <- 
  lapply(data[,c("input_day_c", "overwhelmed","detached","drained","sluggish", "temperature")], as.numeric)

max_day <- max(data$input_day_c, na.rm =TRUE)

### Importing new data so model is set for previous round of data but predicted on future values

data.update <- as.data.frame(read_dta(file.path(data.folder,"BurnTemp_march2024.dta")))

data.update[,c("input_day_c", "overwhelmed","detached","drained","sluggish", "temperature")] <- 
  lapply(data.update[,c("input_day_c", "overwhelmed","detached","drained","sluggish", "temperature")], as.numeric)


### Creates a key to connect the panel and userids and outputs to file. 

#sum <- data[,c("panel",'userid')]
#sum <- distinct(sum,panel, userid)
#sum <- sum[order(sum$panel),]
#write.csv(sum, file.path(getwd(),"Userid_Panel_Key.csv"))

## C. Rescale the Receptiviti measures in questions so they sit on the same 0 - 1 scale. 

##### Drives
# sallee.gratitude
# personality.extraversion
# personality.emotionally_aware
# personality.empathetic
# liwc.positive_emotion_words
# liwcextension_high_empathy
# sallee__joy

##### Drags
# liwc.cognitive_processes
# personality.neuroticism
# personality.anxiety_prone
# personality.melancholy
# personality.stress_prone
# additional_indicators.self_focus
# liwc.negative_emotion_words

drives <- c("salleegratitude",
            "personality__extraversion", 
            "personality__emotionally_aware", 
            "personality__empathetic", 
            "liwc__positive_emotion_words", 
            "liwc_extension__high_empathy",
            "sallee__joy")

drags <- c("liwc__cognitive_processes", 
           "personality__neuroticism", 
           "liwc__anxiety_words", 
           "personality__melancholy", 
           "personality__stress_prone", 
           "additional_indicatorsself_focus",
           "liwc_extension__low_empathy",
           "liwc__negative_emotion_words")

data[,c(drives,drags)] <- 
  lapply(data[,c(drives, drags)], as.numeric)

### Items to rescale

rescale <- c("salleegratitude",
             "liwc__positive_emotion_words", 
             "liwc_extension__high_empathy",
             "liwc__cognitive_processes", 
             "liwc__anxiety_words", 
             "additional_indicatorsself_focus",
             "liwc_extension__low_empathy",
             "liwc__negative_emotion_words",
             "sallee__joy")

### Use the TAT responses to norm receptiviti measures. See excel LIWC-22.Descriptive.Statistic-Test.Kitchen.xlsx

### "liwc__positive_emotion_words" - mean = 0.82, sd = 0.71
### "liwc_extension__high_empathy" - Not in catalogue
### "liwc__cognitive_processes" - mean = 11.17, sd = 3.28
### "liwc__anxiety_words" - mean = 0.26, sd = 0.37
### "additional_indicatorsself_focus" - Not in catalogue
### "liwc__negative_emotion_words"- mean = 0.78, sd =	0.68
### "liwc_extension__low_empathy" - Not in catalogue

### Create z-score (mean 0, sd 1) for each of the non-normed measures.

for(col in rescale){
  
  avg <- mean(data[,col], na.rm =TRUE)
  std <- sd(data[,col], na.rm = TRUE)
  data[,col] = (data[,col] - avg) / std

  avg <- mean(data.update[,col], na.rm =TRUE)
  std <- sd(data.update[,col], na.rm = TRUE)
  data.update[,col] = (data.update[,col] - avg) / std
  
}

### Rescale z-scores to be between 0 and 100. 

for(col in rescale){
  
  data[ ,col] <- rescale(data[ ,col], to = c(0,100))  
  data.update[ ,col] <- rescale(data.update[ ,col], to = c(0,100))  
  
}

## Split data set into old model, for use in CFA specification and subsequent predictions

new_data <- data.update[data.update$input_day_c > max_day,]

## 4. Develop BPI measurement model and add BPI measures to data frame. 

drives.model <-'drives.latent =~ salleegratitude + personality__extraversion + personality__emotionally_aware + personality__empathetic + liwc__positive_emotion_words + liwc_extension__high_empathy + sallee__joy
                
                drags.latent =~ liwc__cognitive_processes + personality__neuroticism + liwc__anxiety_words + personality__melancholy +  personality__stress_prone + liwc__negative_emotion_words

                slider.latent =~  overwhelmed + detached + drained + sluggish
'

fit <- cfa(drives.model, 
           data = data,
           ordered = FALSE,
           std.lv = TRUE,
           missing = "FIML")

summary(fit)

### Produce individual scores for each time point and append data to the data frame.

scores <- lavPredict(fit, 
                     method = "ml", 
                     type = "lv", 
                     newdata = data.update, 
                     append.data = FALSE) 

scores <- as.data.frame(scores)

### Calculate the BPI latent variable by adding the slider and drags together and subtracting the drivers.

scores$BO.latent.ss <- 
  scores$slider.latent + scores$drags.latent - scores$drives.latent

scores$BO.latent <- rescale(scores$BO.latent.ss, to = c(0,100))

scores$drags.latent <- rescale(scores$drags.latent, to = c(0,100))

scores$drives.latent <- rescale(scores$drives.latent, to = c(0,100))

### Merge the data set in with the 

data.update <- cbind(data.update, scores)

## 5. Develop Empathy Capacity measurement model. 

empathy.model <-'empathy.capacity =~ personality__empathetic + liwc_extension__high_empathy + liwc_extension__low_empathy
'

fit <- cfa(empathy.model, 
           data = data,
           std.lv = TRUE,
           ordered = FALSE,
           missing = "FIML")

scores <- lavPredict(fit, 
                     method = "ml", 
                     type = "lv", 
                     newdata = data.update, 
                     append.data = FALSE)

scores <- as.data.frame(scores)

scores$empathy.capacity <- rescale(scores$empathy.capacity, to = c(0,100))

### Merge the data set.

data.update <- cbind(data.update, scores)

## 6. Develop Empathy Capacity measurement model. 

anxiety.model <-'anxiety.blend =~ personality__neuroticism + 
                  liwc__anxiety_words + 
                  additional_indicatorsself_focus'

fit <- cfa(anxiety.model, 
           data = data,
           std.lv = TRUE,
           ordered = FALSE,
           missing = "FIML")

scores <- lavPredict(fit, 
                     method = "ml", 
                     type = "lv", 
                     newdata = data.update, 
                     append.data = FALSE)

scores <- as.data.frame(scores)

scores$anxiety.blend <- rescale(scores$anxiety.blend, to = c(0,100))

### Merge the data set.

data.update <- cbind(data.update, scores)

## Clear up the environment.
rm(fit)
rm(new_rows)
rm(new_data)
rm(data)
rm(scores)

data <- data.update

## 7. Update timestamp.

year <- substring(data$stringdate,3,6)
day <- substring(data$stringdate,7,8)
month <- substring(data$stringdate,1,2)

mdy <- paste0(month,day,year)

mdy <- as.data.frame(mdy)

colnames(mdy) <- "mdy"

data <- cbind(data,mdy)

## 8. Create data frame with only data of interest.

cols <- c("panel",
          "input_day_c",
          "mdy",
          "overwhelmed",
          "drained",
          "sluggish",
          "detached",
          "BO",
          "BO.latent",
          "temperature",
          "empathy.capacity",
          "anxiety.blend",
          "drags.latent",
          "drives.latent",
          "slider.latent",
          drags, 
          drives
          )

data.scores <- data[,cols]

write.csv(data.scores, 
          file = file.path(res.folder,"AllResponseScored.csv"), 
          na = "NA")






