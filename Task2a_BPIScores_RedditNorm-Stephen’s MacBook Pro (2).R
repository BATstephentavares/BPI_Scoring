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


### Import the reddit norm data as a new object.

reddit.norms <- as.data.frame(read.csv(file.path(data.folder,"SFnorms.csv")))

reddit.norms <- reddit.norms[,-c(1)] ## Remove first column

reddit.norms <- as.data.frame(t(as.matrix(reddit.norms)), col.names = colnames())

colnames(reddit.norms) <- reddit.norms[1,]

reddit.norms <- reddit.norms[-1,]

### Change the values of the columns in the reddit.norms data to match the colummns in the data set. 

col.index <- read.csv("colmatch.csv")
col.index <- col.index[, -c(1)]
colnames(col.index) <- c("oldname","newname")

for (i in 1:nrow(col.index)) {
  # Get current and new column names from the mapping object
  current_name <- col.index$oldname[i]
  new_name <- col.index$newname[i]
  
  # Rename the column in the data frame
  colnames(reddit.norms)[colnames(reddit.norms) == current_name] <- new_name
}

### C. Rescale all Receptiviti measures to be z-scores based on the mean and var of reddit posts. 

# for each column name in norms, locate column in data.update and data and produce the normed value base don the column in the norm data

for(i in 1:length(colnames(reddit.norms))){
  
  colname <- colnames(reddit.norms[i])
  
  reddit.norms[,colname] <- as.numeric(reddit.norms[,colname])
  
  mean <- reddit.norms[1, colname]
  std <- reddit.norms[2, colname]
  
  if(colname %in% colnames(data.update)){
    
    data.update[,colname] <- as.numeric(data.update[,colname])
    data[,colname] <- as.numeric(data[,colname])
      
    data.update[,colname] <- scale(data.update[,colname], center = mean, scale = std)
    data[,colname] <- scale(data[,colname], center = mean, scale = std)
    
  } else{
    
    next
  
  }
}


## C. Rescale the Receptiviti measures in questions so they sit on the same 0 - 1 scale. 

##### Drives
# sallee__gratitude
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

drives <- c("sallee__gratitude",
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

## Split data set into old model, for use in CFA specification and subsequent predictions

new_data <- data.update[data.update$input_day_c > max_day,]

## 4. Develop BPI measurement model and add BPI measures to data frame. 

drives.model <-'drives.latent =~ sallee__gratitude + personality__extraversion + personality__emotionally_aware + personality__empathetic + liwc__positive_emotion_words + liwc_extension__high_empathy + sallee__joy
                
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

BPIcols <- data.frame(sallee__gratitude = 0,
                             personality__extraversion = 0,
                             personality__emotionally_aware = 0,
                             personality__empathetic = 0, 
                             liwc__positive_emotion_words = 0,
                             liwc_extension__high_empathy = 0,
                             sallee__joy = 0,
                             liwc__cognitive_processes = 0,
                             personality__neuroticism = 0, 
                             liwc__anxiety_words = 0, 
                             personality__melancholy = 0,
                             personality__stress_prone = 0, 
                             liwc__negative_emotion_words = 0,
                             overwhelmed = mean(data.update$overwhelmed, na.rm = TRUE),
                             detached = mean(data.update$detached, na.rm = TRUE),
                             drained = mean(data.update$drained, na.rm = TRUE),
                             sluggish = mean(data.update$sluggish, na.rm = TRUE))

mean_BPI.model <- lavPredict(fit, 
                             method = "ml", 
                             type = "lv", 
                             newdata = BPIcols, 
                             append.data = FALSE)

mean_BPI.model <- as.data.frame(mean_BPI.model)

### Calculate the BPI latent variable by adding the slider and drags together and subtracting the drivers.

scores$BO.latent.ss <- 
  scores$slider.latent + scores$drags.latent - scores$drives.latent


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

empcols <- data.frame(personality__empathetic = 0,
                      liwc_extension__high_empathy = 0,
                      liwc_extension__low_empathy = 0)

mean_empathy.model <- lavPredict(fit,
                                method = "ml",
                                type = "lv",
                                newdata = empcols,
                                append.data = FALSE)

mean_empathy.model <- as.data.frame(mean_empathy.model)

mean_empathy.model$BO.latent <- mean_BPI.model$slider.latent + 
                                mean_BPI.model$drags.latent - 
                                mean_BPI.model$drives.latent

scores <- as.data.frame(scores)

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

axcols <- data.frame(personality__neuroticism = 0,
                     liwc__anxiety_words = 0,
                     additional_indicatorsself_focus = 0)

mean_anxiety.model <- lavPredict(fit,
                                 method = "ml",
                                 type = "lv",
                                 newdata = axcols,
                                 append.data = FALSE)

mean_anxiety.model <- as.data.frame(mean_anxiety.model)

scores <- as.data.frame(scores)


### Merge the data set.

data.update <- cbind(data.update, scores)

### Merge means of latent variables

latent.mean <- cbind(mean_BPI.model,
                     mean_anxiety.model,
                     mean_empathy.model)

## Clear up the environment.
rm(fit)
rm(new_rows)
rm(new_data)
rm(data)
rm(scores)

data <- data.update

rm(data.update)
rm(axcols)
rm(BPIcols)
rm(empcols)
rm(data.update)
rm(mean_anxiety.model)
rm(mean_BPI.model)
rm(mean_empathy.model)

## 7. Update timestamp.

year <- substring(data$stringdate,3,6)
day <- substring(data$stringdate,7,8)
month <- substring(data$stringdate,1,2)

mdy <- paste0(month,day,year)

mdy <- as.data.frame(mdy)

colnames(mdy) <- "mdy"

data <- cbind(data,mdy)

## Save data as is for further analysis. 

write.csv(data, "NormedScores.csv")
write.csv(latent.mean, "AverageBPI_AnxBlend_EmpBlend.csv")

## 8. Create data frame with only data of interest.
drives <- c("sallee__gratitude",
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
           "additional_indicatorsself_focu",
           "liwc_extension__low_empathy",
           "liwc__negative_emotion_words")

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
          drives,
          "salleegratitude_original", 
          "liwc__positive_emotion_words_original",
          "liwc_extension__high_empathy_original",
          "liwc__cognitive_processes_original",
          "liwc__anxiety_words_original",
          "additional_indicatorsself_focus_original",
          "liwc_extension__low_empathy_original",
          "liwc__negative_emotion_words_original"
          )

data.scores <- data[,cols]

write.csv(data.scores, 
          file = file.path(res.folder,"AllResponseScored.csv"), 
          na = "NA")






