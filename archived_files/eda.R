'''
###################################################
title: NFL QB Predictor
author: Kevin Huang
date: 20170519(original date), Edits made 20180720
output: Datasets used for Analysis - 6 Total Datasets 
Notes: Preprocessing: Exploratory Data Analysis
R version: 3.5.1: Feather spray # version
###################################################
'''
#---------------------------------
# Install packages 
#---------------------------------
library(plyr) #Always load plyr first before dplyr
library(dplyr)
library(tidyr)
library(stringr)

###################################################
# Read in the data created from preprocessing
###################################################
# Creating variables for the path of data files
file_path = "/Users/kevin8523/Desktop/Github/nfldraft_qb_study/data_created"

# Set working directory 
setwd(file_path)
getwd()

# Read in Data
Players_All <- read.csv('Players_All.csv', skip = 0, sep=",")
Probowl_All <- read.csv('Probowl_All.csv', skip = 0, sep=",")
Team_All <- read.csv('Team_All.csv', skip = 0, sep=",")
Coach_All <- read.csv('Coach_All.csv', skip = 0, sep=",")
Combine_All <- read.csv('Combine_All.csv', skip = 0, sep=",")
Wonderlic_All <- read.csv('Wonderlic_All.csv', skip = 0, sep=",")
Dataset_All <- read.csv('Dataset_All.csv', skip = 0, sep=",")

###################################################
# Subset the Dataset
###################################################
# Dataset_All - All the data with multiple years on Players
# Dataset_All_Dedup - All the data with the latest year, removed older years from Players
# Dataset_Subset - Data we will be using to train & test: From 95 - 11
# Dataset_Training - Training data removing years 2012-2017 - Years 95 - 11
# Dataset_Test - Test data to validate model - Years 95 - 11
# Dataset_Real - Real data we will test and see for future years Years 2012-2017

#Takes the latest year for multiple year Qbs
Dataset_All_Dedup <- Dataset_All %>%
  arrange(desc(Year.x)) %>%
  distinct(Player, .keep_all = T) 

# Only use data before 2012 for Analysis

# Dataset_Real <- Dataset_All_Dedup %>%
#   filter(Year.x %in% c('2012', '2013', '2014', '2015', '2016', '2017'))

# Dataset_Subset <- Dataset_All_Dedup %>%
#   filter(!(Year.x %in% c('2012', '2013', '2014', '2015', '2016', '2017')))

Dataset_Subset <- Dataset_All_Dedup


###################################################
# Explore the Dataset
###################################################

# Functions to use
head(Dataset_Subset)
tail(Dataset_Subset)
dim(Dataset_Subset)
str(Dataset_Subset)
summary(Dataset_Subset)
names(Dataset_Subset)
levels(Dataset_Subset$Player)
glimpse()


# Quick Analysis on the top stats
Dataset_Subset %>%
  select(Player, Cmp, Att, Pct, Yds, Y.A, AY.A, TD, Int,Pro_bowl) %>% 
#  filter(Pro_bowl == 'Yes') %>%
  arrange(desc(TD)) %>%
  head(100)

#***Summarize stats for all the pro-bowlers to get a good idea of what they look like***
#Questions to be answered
# Overview
# 1 What is the % of Qb in college that make the pro bowl ==> #1 31 Yes , 800 No ==> 3.875%
# 2 What is the % of Qb drafted that make the probowl ==> INCOMP
# 3 Which conference had the highest amount of probowlers ==> SEC-5, ACC-4, Big Ten-5, Big East-4, Pac10/Pac12-6 - Lots of changed Conferences, but didnt want to waste time digging into it
# 4 Which school had the most probowlers ==> Boston College-2, Michigan-2, Virginia Tech-2
# 5 Average of probowl Qb stats vs average of NON probowl qb stats
# 6 What seems to be the feature that all probowlers have?


#1 31 Yes , 800 No ==> 3.875% of College football QBs in this data set made the probowl
Dataset_Subset %>%
  group_by(Pro_bowl) %>%
  tally()

#2 Have to get better drafted data, quick qc shows lots of data missing

#3 SEC-5, ACC-4, Big Ten-5, Big East-4, Pac10/Pac12-6
Dataset_Subset %>%
  filter(Pro_bowl == 'Yes') %>%
  group_by(Conf) %>%
  tally()

#4 Boston College-2, Michigan-2, Virginia Tech-2
Dataset_Subset %>%
  filter(Pro_bowl == 'Yes') %>%
  group_by(School.x) %>%
  tally() %>%
  arrange(desc(n))

# 5 Average of probowl Qb stats vs average of NON probowl qb stats
#       Pro_bowl `mean(Pct)` `mean(Yds)` `mean(Y.A)` `mean(TD)` `mean(Int)` `mean(Rate)` `mean(Yds.1)`
#       <chr>       <dbl>       <dbl>       <dbl>      <dbl>       <dbl>        <dbl>         <dbl>
# 1      Yes    63.57097    3183.194    8.367742   25.67742    9.225806     151.6258      226.8387
# 2     <NA>    57.36637    2307.319    7.142750   16.05750   10.143750     126.8721      121.3350
Stats <- Dataset_Subset %>%
  group_by(Pro_bowl) %>%
  summarise(mean(Pct), mean(Yds), mean(Y.A), mean(TD), mean(Int), mean(TD_INT_Ratio), mean(Rate), mean(Yds.1),
            mean(Rushing_TD), mean(X40YD), mean(Vertical), mean(BenchReps), 
            mean(Broad.Jump), mean(X3Cone), mean(Shuttle), mean(Score))
colnames(Stats) <- c('Pro_bowl','Pct', 'Pass_yds', 'PYA', 'TD', 'Int', 'TD_INT_RATIO', 'QB_RATE', 'Rush_yds', 
                     'Rush_TD', '40yd', 'Vertical', 'Bench_Reps', 'Broad_Jump', '3_cone',
                     'Shuttle', 'Score')


# 6 What seems to be the feature that all probowlers have?

#Subset the data to have pro bowl and nonprobowl
Dataset_Subset_Probowl <- Dataset_Subset %>%
  filter(Pro_bowl =='Yes')

Dataset_Subset_NonProbowl <- Dataset_Subset %>%
  filter(is.na(Pro_bowl))




#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

#Quick Analysis
Dataset_Subset_Probowl %>%
  str()

#Histogram to plot skewness
hist(Dataset_Subset$TD)
hist(Dataset_Subset$TD, breaks = 100)

#Summary of non-numeric values
table(Dataset_Subset$Conf)

#Corrletaion plots
str(Dataset_Subset)
cor(Dataset_Subset[c("TD","Int")])
corrplot(cor(Dataset_Subset[c("TD","Int")]))
corrplot(cor(Dataset_Subset[c("Yds","Y.A","TD","Int","TD_INT_Ratio","Rate","Att.1",
                              "Yds.1","Rushing_TD")]))
#Scatterplot matrix
pairs(Dataset_Subset[c("TD","Int")])
pairs(Dataset_Subset[c("Yds","Y.A","TD","Int","TD_INT_Ratio","Rate","Att.1",
                       "Yds.1","Rushing_TD")])

#pairs.panels ==> Most useful for less than 6-10
pairs.panels(Dataset_Subset[c("TD","Int")]) 
pairs.panels(Dataset_Subset[c("Yds","Y.A","TD","Int","TD_INT_Ratio","Rate","Att.1",
                              "Yds.1","Rushing_TD")]) 


#Visualize
ggplot(data = Dataset_Subset_Probowl, aes(x=Rate)) + 
  geom_histogram(color = 'black', fill = 'white')

ggplot(data = Dataset_Subset_Probowl, aes(TD_INT_Ratio)) + 
  geom_histogram(color = 'black', fill = 'white')


#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

#More Feature Engineering
colnames(Dataset_Subset) <- c("Column_ID","Player","School","Conf","G","Cmp","Att",
                           "Pct", "Yds","Pass_Yds_A","Pass_AYds_A","TD","Int","Rate","Rush_A",
                           "Rush_Yds","Rush_Avg","Rush_TD","Year_1","Year_2","Pos","AV", 
                           "Ht","Wt","40Yd","Vert","Bench","Broad_Jump","3Cone","Shutte",
                           "Drafted","Wonderlic_Score", "Pos_Combine","Pro_Bowl","TD_INT_Ratio") 


Dataset_Subset <- Dataset_Subset %>%
  mutate(Total_Yds = Yds+Rush_Yds)
Dataset_Subset <- Dataset_Subset %>%
  mutate(Total_Tds = TD+Rush_TD)
Dataset_Subset <- Dataset_Subset %>%
  mutate(Total_TD_INT_Ratio = Total_Tds/(Total_Tds+Int))


colnames(Dataset_Subset) <- c("Column_ID","Player","School","Conf","G","Cmp","Att",
                              "Pct", "Yds","Pass_Yds_A","Pass_AYds_A","TD","Int","Rate","Rush_A",
                              "Rush_Yds","Rush_Avg","Rush_TD","Year_1","Year_2","Pos","AV", 
                              "Ht","Wt","40Yd","Vert","Bench","Broad_Jump","3Cone","Shutte",
                              "Drafted","Wonderlic_Score", "Pos_Combine","Pro_Bowl","TD_INT_Ratio",
                              "Total_Yds","Total_Tds","Total_TD_INT_Ratio") 

#Set up the data for Machine Learning: 
# Since finding a pro-bowler is quite difficult, I figured a ML algorithim
# that finds a rare event would be good for this task. My thought process is to start with a
# simple ML algorithim and from there try more complicated Algorihtims. We also could try some
# feature engineering and cleaning of the data to help the data make connections

#Using only features that include the whole dataset. Combine data is only for players invited.
#Could use this in the future to engineer better results
Dataset_Subset_All <- select(Dataset_Subset, Player, School, Conf, G, Cmp, Att, Pct, Yds, Pass_Yds_A,
       Pass_AYds_A, TD, Int, TD_INT_Ratio, Rate, Rush_A, Rush_Yds, Rush_Avg, Rush_TD,
       Total_Yds, Total_Tds, Total_TD_INT_Ratio, Year_1, Pro_Bowl)



# Remove all unnecssary Data Sets
rm(list = ls()[!ls() %in% c("Dataset_Subset_All", "Dataset_Subset", "Dataset_Subset_train",
                            "Dataset_Subset_test", "Dataset_Real")])

#Change all Yes to 1
Dataset_Subset_All$Pro_Bowl[Dataset_Subset_All$Pro_Bowl == 'Yes'] <- 1

#Count of # of probowlers in the list
Dataset_Subset_All %>%
  filter(Pro_Bowl == 1) %>%
  select(Player)

#Change all NA to 0 for pro bowl list
Dataset_Subset_All$Pro_Bowl[is.na(Dataset_Subset_All$Pro_Bowl)] <- 0

#Change Pro bowl column to numeric data type
Dataset_Subset_All$Pro_Bowl <- as.numeric(Dataset_Subset_All$Pro_Bowl)

#Set seed for reproducible results
set.seed(21)
#sample <- sample.int(n = nrow(Dataset_Subset_All), size = floor(.7*nrow(Dataset_Subset_All)), replace = F)

#Train on data gathered up to 2008 and will test on 2009-2011 
Dataset_Subset_train <- Dataset_Subset_All[480:1163, ]
Dataset_Subset_test <- Dataset_Subset_All[1:479, ]

# #normalize - Doesnt do it across the dataframe equally
# normalize <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }
# Dataset_Subset_Scale <- Dataset_Subset_All[(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))]
# Dataset_Subset_Scale <- as.data.frame(lapply(Dataset_Subset_Scale, normalize))
# colMeans(Dataset_Subset_Scale)
# apply(Dataset_Subset_Scale,2,sd)

# # Not needed for logisitc regression
# # Feature Scaling
# Dataset_Subset_train[, 4:21] = scale(Dataset_Subset_train[, 4:21])
# Dataset_Subset_test[, 4:21] = scale(Dataset_Subset_test[, 4:21])
# colMeans(Dataset_Subset_train[, 4:21])
# apply(Dataset_Subset_train[,4:21],2,sd)

str(Dataset_Subset_train)

# ML Algorithim - If error use: #control=glm.control(maxit=50)) # Number of iterations
# Model 1
classifier = glm(formula = Pro_Bowl ~ .,
                 family = binomial,
                 data = Dataset_Subset_train[,!colnames(Dataset_Subset_train)
                                             %in% c("Player", "School", "Conf", "Year_1",
                                                    #"Rush_A","Rush_Yds","Rush_Avg","Rush_TD")])
                                                    "Total_Yds","Total_Tds","Total_TD_INT_Ratio")])
# Model 2
classifier_2 = glm(formula = Pro_Bowl ~ .,
                 family = binomial,
                 data = Dataset_Subset_train[,!colnames(Dataset_Subset_train)
                                             %in% c("Player", "School", "Conf", "Year_1",
                                                    "Rush_A","Rush_Yds","Rush_Avg","Rush_TD",
                                                    "Total_Yds","Total_Tds","Total_TD_INT_Ratio")])


prob_pred = predict(classifier, type = 'response', newdata = Dataset_Subset_test[-23])
prob_pred
y_pred = ifelse(prob_pred > 0.5, 1, 0)
Dataset_Subset_test$Predictions <- y_pred
Dataset_Subset_test$Prediction_Prob <- prob_pred

#Confusion Matrix, Plot visual, More ML algorithims - maybe try pca dimension reduction, or poisson



#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================
#EXTRA


#WHAT I HAVE LEFT
#Additional Things
#3. Machine Learning ==> Train and test data
###SVM RANDOM FOREST LOGISTIC REGRESSION
# Try clustering it and see if similar qb's go together

#. More Visualization of variables to derive more info
### Plot single variables & relationships b/w variables
### plot the distribution of pro bowl qb
### plot relationships b/w key variables scatterplots ==> price carats

#5 Feature Engineering for better results and redo ML algorithims
# Create new variables
# Clean up the data, some of the data is incorrect like Robert Griffin making the pro bowl
# Find players from States, Create a States variable

#6. Write up Analysis