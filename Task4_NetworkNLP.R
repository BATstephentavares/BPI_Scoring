###############################################################################
## Purpose: 
## 1. To pull journal entry text
## 2. To run analytics based on journal entry text as an experiment.
##############################################################################

rm(list=ls()) 

### 1. Load packages

ifelse("EGAnet" %in% row.names(installed.packages())==TRUE,
       ifelse(packageVersion("EGAnet")=="2.0.3", 
              "The EGAnet package is installed, jump to step #3", 
              "The EGAnet package is installed, but you have a difference version. Please, install the EGAnet package from CRAN (see Step #2 below)."),
       "Please, install the EGAnet package (see Step #2 below)")

## Step 2:
## Install the devtools package
# install.packages("EGAnet")
library(EGAnet)

library(tm)
library(SnowballC)

#install.packages("readxl")
library(readxl)


### 2. Load in journal entry data.

data.path <- file.path(getwd(), "Data")

data <- read_excel(
          file.path(
            data.path,"Raw_Journals_DeIdentified_20240115 - Copy.xlsx"))


## Functions to identify specific data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

## Create corpus

corpus <- Corpus(VectorSource(data$entry))

corpus <- tm_map(corpus, removeURL)

corpus <- tm_map(corpus, removeNumPunct)

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

## Create doctument term matrix

dtm <- DocumentTermMatrix(corpus)

dtm2 <- removeSparseTerms(dtm, sparse = 0.90)
dtm2
## Convert dtm to a data frame

dtm2.df <- as.data.frame(as.matrix(dtm2))

### EGA Network Models
library(EGAnet)
topics.optimal <- EGA.fit(dtm2.df, model = "tmfg", algorithm = "walktrap", 
                          plot.EGA = TRUE, corr = "pearson")

## 5-steps minimum

topics.optimal$EntropyFit

## Run clustering on words
topics <- EGA(dtm2.df, model = "tmfg", steps = 5, corr = "pearson")
topics

## Check which words belongs to each topic:
topics$dim.variables

write.csv(topics$dim.variables, file = file.path(data.path,"TopicsModel.csv"))

netplot.topics <- plot(topics, label.size = 4, edge.size = 0.9,
                       node.size = colSums(topics$network)*4)

netplot.topics

## Network score per topic
net.scores.topics <- net.scores(data = dtm2.df, A = topics)

scores.topics <- as.data.frame(net.scores.topics$scores$std.scores)

## Change the column names to reflect the topics:
colnames(scores.topics) <- paste0("Topic", colnames(scores.topics))

### Test using LLM to assess the personality profile of represented in each journal
### entry

install.packages("keras")
keras::install_keras()

if(!"devtools" %in% unlist(lapply(.libPaths(), list.files))){
  install.packages("devtools")
}

# Get {transforEmotion} package
devtools::install_github("atomashevic/transforEmotion")

reticulate::py_install("tf-keras")
reticulate::import("transformers.models.roberta.modeling_tf_roberta")

#transforEmotion::setup_miniconda()

#install.packages("transforEmotion")
# Load packages
library(transforEmotion)

### Load in scores

personality_scores <- transformer_scores(
  text = data$entry,
  classes = c(
    "Agreeableness", "Conscientiousness", 
    "Extraversion", "Neuroticism",
    "Openess",   "friendly", "gregarious", "assertive",
    "active", "excitement", "cheerful"
  )
)

personality_scores
