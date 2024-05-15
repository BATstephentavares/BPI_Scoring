###############################################################################
## Purpose: 
## 1. To pull data set. 
## 2. Run metrics for personalized data frames.
##############################################################################

rm(list=ls()) 

require(stringr)
require(scales)
require(haven)
require(zoo)

## A. Set up folder directories

data.folder <- file.path(getwd(),"Data")

res.folder <- file.path(getwd(),"AllScores")

panel.folder <- file.path(getwd(),"Panel Tables","FullPanel")

rec.folder <- file.path(getwd(),"Panel Tables","TopReceptiviti")

## B. Load in data. 

data <- as.data.frame(read.csv(file.path(res.folder,"AllResponseScored.csv")))

## C. Calculate number of weeks and other metrics. 

weeks <- max(data$input_day_c, na.rm = TRUE) / 7

max <- max(data$panel, na.rm = TRUE)

full_data <- data.frame()

recept <- c("liwc__positive_emotion_words",
            "empathy.capacity",
            "salleegratitude",
            "personality__extraversion", 
            "personality__emotionally_aware",
            "sallee__joy",
            "anxiety.blend",
            "personality__melancholy",
            "personality__stress_prone",
            "liwc__negative_emotion_words",
            "liwc__cognitive_processes",
            "additional_indicatorsself_focus")
            
## D. Initiate loop to create panel-data for each person.

for(j in 1:max){
  
  Person <- j

  panel_data <- subset(data, panel == Person)
  
  panel_data <- panel_data[order(panel_data$input_day_c),]
  
  ## Create percent change for BPI. This is just the change from the last input date where data is available.
  
  panel_data$BPI.PercentChange <- 
    c(NA, 
    (diff(panel_data$BO.latent) / panel_data$BO.latent[-nrow(panel_data)])*100)
  
  ## Develop the BPI Moving average.
    
  if(nrow(panel_data) > 3){ ## If there are more than 3 data points.
    
    moving_avg <- 
      as.data.frame(rollapply(panel_data[,c("BO.latent")], 
                              FUN=function(x) mean(x, na.rm=TRUE),
                              width=3, 
                              fill = NA,
                              partial = TRUE,
                              by.column = TRUE,
                              by = 1,
                              align = "right"))
    
    colnames(moving_avg)[1] <- "movavg_BOlatent"
    
    panel_data <- cbind(panel_data, moving_avg) 
    
  } else if(nrow(panel_data)  == 3){ ## If there are 3 data points.
    
    moving_avg <- 
      as.data.frame(rollapply(panel_data[,c("BO.latent")], 
                              FUN=function(x) mean(x, na.rm=TRUE),
                              width=2, 
                              fill = NA,
                              partial = TRUE,
                              by.column = TRUE,
                              by = 1,
                              align = "right"))
    colnames(moving_avg)[1] <- "movavg_BOlatent"
    panel_data <- cbind(panel_data, moving_avg)
    
  } else if(nrow(panel_data) < 3){ ## If there are less than 3 data points
    
    moving_avg <- as.data.frame(matrix(NA, nrow = nrow(panel_data),ncol = 2))
    colnames(moving_avg)[1] <- "movavg_BOlatent"
    panel_data <- cbind(panel_data, moving_avg)
    
  }
  
  panel_data$BPI.PercentChange.mvgavg <- 
    c(NA, 
      (diff(panel_data$movavg_BOlatent) / 
         panel_data$movavg_BOlatent[-nrow(panel_data)])*100)
  
  rm(moving_avg)
  
  ### Develop the BPI average score for the population. 
  
  average <- tapply(data$BO.latent, 
                    data$panel, 
                    function(x) mean(x, na.rm=TRUE))
  
  avg_panel <- mean(average, na.rm = TRUE) ### equal weights to panel averages
  
  panel_data$PopulationBOaverage <- avg_panel
  
  ## Create the Wellness Wave.
  
  panel_sd <- sd(data$BO.latent[data$panel == Person], na.rm=TRUE)
  
  sd_all_panel <- tapply(data$BO.latent, data$panel, function(x) sd(x, na.rm=TRUE))
  
  sd_all_panel <- mean(sd_all_panel, na.rm = TRUE)
  
  wellness_wave <- panel_sd / sd_all_panel
  
  panel_data$wave <- wellness_wave
  
  ## Create Awareness Scores - calculates based on all of the observations to date without a time split. Recommend to perform a cut off of time in future iterations so can look at the most recent 3 observations compared to the entire sample. 
  
  tryCatch({
    awareness.total <- cor(panel_data$slider.latent, 
                   panel_data$drags.latent - panel_data$drives.latent, 
                   use = "complete.obs",
                   method = "pearson")
  }, error = function(e) {
    
    awareness.total <- NA
    
  })

  panel_data$awareness.score <- awareness.total
  
  if(is.na(awareness.total)){
    
    panel_data$overall_awareness <- 
      "Unable to Calculate - No Variation in Sliders"
    
  }else if(awareness.total <=.33){
    
    panel_data$overall_awareness <- "Blocking"
    
  }else if(awareness.total >.33 & awareness.total <= .67){
    
    panel_data$overall_awareness <- "Activating"
  
  }else if(awareness.total >.67){
    
    panel_data$overall_awareness <- "Aware"
  
  }
  
  panel_data.recent <- tail(panel_data, 4)
  
  tryCatch({
    awareness.recent <- cor(panel_data.recent$slider.latent, 
                            panel_data.recent$drags.latent - 
                              panel_data.recent$drives.latent, 
                            use = "pairwise.complete.obs",
                            method = "pearson")
    }, error = function(e) {
      
      awareness.recent <- NA
               
             })
  
  panel_data$awareness.recent.score <- awareness.recent
  
  if(is.na(awareness.recent)){
    
    panel_data$recent_awareness <- 
      "Unable to Calculate - No Variation in Sliders"
    
  }else if(awareness.recent >= -1 & awareness.recent <=.33){
    
    panel_data$recent_awareness <- "Blocking"
    
  }else if(awareness.recent >.33 & awareness.recent <= .67){
    
    panel_data$recent_awareness <- "Activating"
    
  }else if(awareness.recent >.67){
    
    panel_data$recent_awareness <- "Aware"
    
  }
  
  rm(panel_data.recent)
  
  ### Data Acuity
  
  input_count <- table(data$panel)
  
  avg_count <- mean(input_count) / weeks
  
  input_count <- data.frame(
    panel = as.integer(names(input_count)), Count = as.integer(input_count))
  
  panel_count <- 
    mean(input_count$Count[input_count$panel == Person], na.rm = TRUE) / weeks
  
  panel_data$inputperweek <- panel_count
  
  panel_data$totalweek <- weeks
  
  panel_data$avginput <- avg_count
  
  panel_data$inputcomparison <- (panel_count / avg_count)
  
  rm(input_count)
  
  ## Drags & Drive Factors
  
  for(i in 1:length(recept)){
    
    ## Moving Average of your own score
    
    if(nrow(panel_data) > 3){
      
      moving_avg <- 
        as.data.frame(rollapply(panel_data[,recept[i]], 
                                FUN=function(x) mean(x, na.rm=TRUE),
                                width=3, 
                                fill = NA,
                                partial = TRUE,
                                by.column = TRUE,
                                by = 1,
                                align = "right"))
      
      colnames(moving_avg) <- paste0("movavg_", recept[i])
      
      panel_data <- cbind(panel_data, moving_avg)
      
      col <- paste0("movavg_", recept[i])
      
      change <- ((panel_data[,recept[i]] - lag(panel_data[,col], n=1)) / 
        lag(panel_data[,col], n=1))*100
      
      change <- as.data.frame(change)
      
      colnames(change) <- paste0("%change_",recept[i])  
      
      panel_data <- cbind(panel_data, change) 
      
    }else if(nrow(panel_data)  == 3){
      
      moving_avg <- 
        as.data.frame(rollapply(panel_data[,recept[i]], 
                                FUN=function(x) mean(x, na.rm=TRUE),
                                width=2, 
                                fill = NA,
                                partial = TRUE,
                                by.column = TRUE,
                                by = 1,
                                align = "right"))
      
      colnames(moving_avg)[1] <- paste0("movavg_", recept[i])
      
      panel_data <- cbind(panel_data, moving_avg)
      
      col <- paste0("movavg_", recept[i])
      
      change <- ((panel_data[,recept[i]] - lag(panel_data[,col], n=1)) / 
                   lag(panel_data[,col], n=1))*100
      
      change <- as.data.frame(change)
      
      colnames(change)[1] <- paste0("%change_",recept[i])  
      
      panel_data <- cbind(panel_data, change) 
      
    }else if(nrow(panel_data) <3){
      
      moving_avg <- as.data.frame(matrix(NA, 
                                         nrow = nrow(panel_data),
                                         ncol = 2))
      
      col <- paste0("movavg_", recept[i])
      
      colnames(moving_avg)[1] <- paste0("movavg_",recept[i])
      colnames(moving_avg)[2] <- paste0("%change_",recept[i])
      panel_data <- cbind(panel_data, moving_avg)
    
    }
    
    ### Percent vs. peers
    
    peer_avg <- as.data.frame(
      tapply(data[,recept[i]], 
             data$panel, 
             function(x) mean(x, na.rm=TRUE)))
    
    peer_avg[peer_avg == 0] <- NA
    
    peer_avg[peer_avg == "NaN"] <- NA
    
    colnames(peer_avg)[1] <- "peer_avg"
    
    peer_avg <- mean(peer_avg$peer_avg, na.rm = TRUE)
    
    peer_compare <- as.data.frame((
      panel_data[,recept[i]] / peer_avg))
    
    colnames(peer_compare)[1] <- paste0("Peer Comparison_",recept[i]) 
    
    sd_all_panel <-  as.data.frame(
      tapply(data[,recept[i]], 
             data$panel, 
             function(x) sd(x, na.rm=TRUE)))  
   
    sd_all_panel[sd_all_panel == 0] <- NA
    
    sd_all_panel[sd_all_panel == "NaN"] <- NA
    
    colnames(sd_all_panel)[1] <- "stdev"
    
    sd_all_panel <- mean(as.numeric(sd_all_panel$stdev), na.rm = TRUE)
    
    peer_deviation <- as.data.frame(
      (panel_data[,c(paste0("movavg_",recept[i]))] - peer_avg)/sd_all_panel)
    
    colnames(peer_deviation)[1] <- paste0("Peer Deviation_",recept[i])
    
    panel_data <- cbind(panel_data, peer_compare, peer_deviation)
    
  }
  
  rm(moving_avg)
  rm(peer_compare)
  rm(peer_deviation)
  
  ### Select subset of columns for export to csv.
  
  ## Re-order column names.
  
  col_order <- c("panel", 
                 "input_day_c", "mdy",
                 "inputperweek", "inputcomparison", 
                 "temperature", 
                 "BO", "BO.latent", "BPI.PercentChange",
                 "movavg_BOlatent", "BPI.PercentChange.mvgavg", 
                 "PopulationBOaverage",
                 "wave",
                 "overall_awareness", "awareness.score",
                 "recent_awareness", "awareness.recent.score",
                 'liwc__positive_emotion_words',	
                 'movavg_liwc__positive_emotion_words',
                 '%change_liwc__positive_emotion_words',
                 'Peer Comparison_liwc__positive_emotion_words',
                 'Peer Deviation_liwc__positive_emotion_words',
                 'empathy.capacity',	
                 'movavg_empathy.capacity',
                 '%change_empathy.capacity',
                 'Peer Comparison_empathy.capacity',
                 'Peer Deviation_empathy.capacity',
                 'salleegratitude',
                 'movavg_salleegratitude',	
                 '%change_salleegratitude',
                 'Peer Comparison_salleegratitude',	
                 'Peer Deviation_salleegratitude',
                 'personality__extraversion',	
                 'movavg_personality__extraversion',	
                 '%change_personality__extraversion',
                 'Peer Comparison_personality__extraversion',	
                 'Peer Deviation_personality__extraversion',
                 'personality__emotionally_aware',
                 'movavg_personality__emotionally_aware',
                 '%change_personality__emotionally_aware',	
                 'Peer Comparison_personality__emotionally_aware',	
                 'Peer Deviation_personality__emotionally_aware',
                 'sallee__joy',	
                 'movavg_sallee__joy',
                 '%change_sallee__joy',	
                 'Peer Comparison_sallee__joy',	
                 'Peer Deviation_sallee__joy',
                 'anxiety.blend',	
                 'movavg_anxiety.blend',
                 '%change_anxiety.blend',	
                 'Peer Comparison_anxiety.blend',	
                 'Peer Deviation_anxiety.blend',
                 'personality__melancholy',	
                 'movavg_personality__melancholy',
                 '%change_personality__melancholy',	
                 'Peer Comparison_personality__melancholy',	
                 'Peer Deviation_personality__melancholy',
                 'personality__stress_prone',	
                 'movavg_personality__stress_prone',	
                 '%change_personality__stress_prone',
                 'Peer Comparison_personality__stress_prone',	
                 'Peer Deviation_personality__stress_prone',
                 'liwc__negative_emotion_words',
                 'movavg_liwc__negative_emotion_words',
                 '%change_liwc__negative_emotion_words',	
                 'Peer Comparison_liwc__negative_emotion_words',
                 'Peer Deviation_liwc__negative_emotion_words',
                 'liwc__cognitive_processes',	
                 'movavg_liwc__cognitive_processes',
                 '%change_liwc__cognitive_processes',	
                 'Peer Comparison_liwc__cognitive_processes',	
                 'Peer Deviation_liwc__cognitive_processes',
                 'additional_indicatorsself_focus',
                 'movavg_additional_indicatorsself_focus',
                 '%change_additional_indicatorsself_focus',	
                 'Peer Comparison_additional_indicatorsself_focus',	
                 'Peer Deviation_additional_indicatorsself_focus')
                 
  panel_data <- panel_data[,col_order]
  
  write.csv(panel_data, file = 
              file.path(panel.folder, paste0("Panel_",Person,".csv")),
            na = "NA")
  
  full_data <- rbind.data.frame(full_data,panel_data)
  
  ### Find the top 4 characteristics that are furthest from the average.
  
  names <- c('Peer Deviation_liwc__positive_emotion_words',
             'Peer Deviation_empathy.capacity',
             'Peer Deviation_salleegratitude',
             'Peer Deviation_personality__extraversion',
             'Peer Deviation_personality__emotionally_aware',
             'Peer Deviation_sallee__joy',
             'Peer Deviation_anxiety.blend',
             'Peer Deviation_personality__melancholy',
             'Peer Deviation_personality__stress_prone',
             'Peer Deviation_liwc__negative_emotion_words',
             'Peer Deviation_liwc__cognitive_processes',
             'Peer Deviation_additional_indicatorsself_focus'
             )
  
  deviations <- panel_data[,names]
  
  ### Get the index of the last good piece of data. 
  
  last_non_na_row_index <- max(which(!apply(deviations,1,anyNA)))
  
  ### Extract last non-na row from data frame
  
  last_non_na_row <- panel_data[last_non_na_row_index, ]
  
  deviations <- last_non_na_row[,names]
  
  ### Calculate columns with the greatest deviation from the mean.
  
  top_deviation_column <- names(deviations)[order(abs(unlist(deviations)),
                                                  decreasing = TRUE)[1:4]]
  
  ### Update panel_data to only include receptiviti measures with the highest deviation.
  
  find_similar_columns <- function(column_name, data_frame) {
    similar_columns <- grep(column_name, names(data_frame), value = TRUE)
    return(similar_columns)
  }
  
  top_deviation_column_1 <- find_similar_columns(sub("Peer Deviation_(.*)", "\\1", top_deviation_column[1]), panel_data)
  
  top_deviation_column_2 <- find_similar_columns(sub("Peer Deviation_(.*)", "\\1", top_deviation_column[2]), panel_data)
  
  top_deviation_column_3 <- find_similar_columns(sub("Peer Deviation_(.*)", "\\1", top_deviation_column[3]), panel_data)
  
  top_deviation_column_4 <- find_similar_columns(sub("Peer Deviation_(.*)", "\\1", top_deviation_column[4]), panel_data)
  
  panel_data <- panel_data[, c("panel", 
                               "input_day_c", "mdy",
                               "inputperweek", "inputcomparison", 
                               "temperature", 
                               "BO", "BO.latent", "BPI.PercentChange",
                               "movavg_BOlatent", "BPI.PercentChange.mvgavg", 
                               "PopulationBOaverage",
                               "wave",
                               "overall_awareness", "awareness.score",
                               "recent_awareness", "awareness.recent.score",
                               top_deviation_column_1, top_deviation_column_2,
                               top_deviation_column_3, top_deviation_column_4)]
  
  write.csv(panel_data, file.path(rec.folder,
                                  paste0("TopRecept_Panel_", Person,".csv")),
            na = "NA")
  
}

write.csv(full_data, file = file.path(res.folder,"AllPanels_.csv"), na = "NA")
