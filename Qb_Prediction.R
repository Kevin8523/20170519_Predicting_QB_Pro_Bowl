


#---------------------------------
# Install packages 
#---------------------------------
library(ggplot2)
library(plyr) #Always load plyr first before dplyr
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(caret) 
library(corrplot)
library(psych)


###################################################
# Read in the dataset 1 
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/Player_Stats")
getwd()

#Read in one file
#players_1995 <- read_csv('Player_1995.csv', col_names = TRUE, skip = 1, quote = "")
#players_1995 <- read.csv('Player_1995.csv', skip = 1, sep=",", quote = "")

#Since I have multiple files I need to import, I will import all the files at once
temp = list.files(pattern="*.csv")
for (i in 1:length(temp))
  assign(temp[i], read.csv(temp[i], skip=1, sep=",", quote =""))

###################################################
# Create a year variable for indexing
###################################################
#Create the Year varaible for each dataframe

#Done for one case
#mutate(Player_1995.csv, Year = '1995' )

#Now for all
#Create a list
df_list <- list(Player_1995.csv = Player_1995.csv, Player_1996.csv = Player_1996.csv, Player_1997.csv = Player_1997.csv,
                Player_1998.csv = Player_1998.csv, Player_1999.csv = Player_1999.csv, Player_2000.csv = Player_2000.csv,
                Player_2001.csv = Player_2001.csv, Player_2002.csv = Player_2002.csv, Player_2003.csv = Player_2003.csv,
                Player_2004.csv = Player_2004.csv, Player_2005.csv = Player_2005.csv, Player_2006.csv = Player_2006.csv,
                Player_2007.csv = Player_2007.csv, Player_2008.csv = Player_2008.csv, Player_2009.csv = Player_2009.csv,
                Player_2010.csv = Player_2010.csv, Player_2011.csv = Player_2011.csv, Player_2012.csv = Player_2012.csv,
                Player_2013.csv = Player_2013.csv, Player_2014.csv = Player_2014.csv, Player_2015.csv = Player_2015.csv,
                Player_2016.csv = Player_2016.csv)

#Extract the last digits to add as a column named Year
for (i in names(df_list)) {
  col <- unlist(strsplit(i, "_"))[2]
  df_list[[i]]$Year <- col
}

do.call("rbind", df_list)


###################################################
# Join the Dataset
###################################################

#Join the Dataset into one
Players_All <- join_all((df_list), type = 'full') #plyr function
#Players_All <- Reduce(function(...) merge(..., all=TRUE), df_list) #base r function


#Remove all the unnecessary data frames
rm(list = ls(pattern = ".csv"))



###################################################
# Explore the dataset
###################################################
head(Players_All)
tail(players_1995)
dim(players_1995)
str(players_1995)
summary(players_1995)
names(players_1995)
levels(players_1995$Player)
glimpse(players_1995)



###################################################
# Clean the dataset
###################################################

Players_All$Year <- str_replace_all(Players_All$Year, ".csv", "")
#lapply converts to a list, sapply will keep it as is
Players_All$X.Rk <- sapply(Players_All$X.Rk, gsub, pattern = '"', replacement = "", fixed = TRUE) #Remove all " in the column
Players_All$TD. <- sapply(Players_All$TD., gsub, pattern = '"', replacement = "", fixed = TRUE) 
#colnames(Players_All)[18] <- "Rushing_TD" #Changes column name
colnames(Players_All)[colnames(Players_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #
Players_All$Rushing_TD <- as.integer(Players_All$Rushing_TD)
Players_All$Player <- sapply(Players_All$Player, gsub, pattern = '*', replacement = "", fixed = TRUE) 
Players_All$Player <- sub("\\\\.*", "", Players_All$Player)

#Save the data
typeof(Players_All$Player)
sapply(Players_All,class) #Type for all columns

#If your data is in a list do the below
#Players_All$X.Rk<- unlist(Players_All$X.Rk)
#Players_All$Rushing_TD<- unlist(Players_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()
write.csv(Players_All, file = "Players_All.csv")




#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================



###################################################
# Read in the dataset 2
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/Team_Rankings")  
getwd()

#Read in one file
#rating_1995 <- read_csv('Rating_1995.csv', col_names = TRUE, skip = 1, quote = "")
#rating_1995 <- read.csv('Rating_1995.csv', skip = 1, sep=",", quote = "")

#Since I have multiple files I need to import, I will import all the files at once
temp = list.files(pattern="*.csv")
for (i in 1:length(temp))
  assign(temp[i], read.csv(temp[i], skip=1, sep=",", quote =""))

###################################################
# Create a year variable for indexing
###################################################
#Create the Year varaible for each dataframe

#Done for one case
#mutate(rating_1995.csv, Year = '1995' )

#Now for all
#Create a list
df_list <- list(Rating_1995.csv = Rating_1995.csv, Rating_1996.csv = Rating_1996.csv, Rating_1997.csv = Rating_1997.csv,
                Rating_1998.csv = Rating_1998.csv, Rating_1999.csv = Rating_1999.csv, Rating_2000.csv = Rating_2000.csv,
                Rating_2001.csv = Rating_2001.csv, Rating_2002.csv = Rating_2002.csv, Rating_2003.csv = Rating_2003.csv,
                Rating_2004.csv = Rating_2004.csv, Rating_2005.csv = Rating_2005.csv, Rating_2006.csv = Rating_2006.csv,
                Rating_2007.csv = Rating_2007.csv, Rating_2008.csv = Rating_2008.csv, Rating_2009.csv = Rating_2009.csv,
                Rating_2010.csv = Rating_2010.csv, Rating_2011.csv = Rating_2011.csv, Rating_2012.csv = Rating_2012.csv,
                Rating_2013.csv = Rating_2013.csv, Rating_2014.csv = Rating_2014.csv, Rating_2015.csv = Rating_2015.csv,
                Rating_2016.csv = Rating_2016.csv)

#Extract the last digits to add as a column named Year
for (i in names(df_list)) {
  col <- unlist(strsplit(i, "_"))[2]
  df_list[[i]]$Year <- col
}

do.call("rbind", df_list)


###################################################
# Join the Dataset
###################################################

#Join the Dataset into one
Team_All <- join_all((df_list), type = 'full') #plyr function
#Players_All <- Reduce(function(...) merge(..., all=TRUE), df_list) #base r function


#Remove all the unnecessary data frames
rm(list = ls(pattern = ".csv"))



###################################################
# Explore the dataset
###################################################
head(Team_All)
tail(Team_All)
dim(Team_All)
str(Team_All)
summary(Team_All)
names(Team_All)
levels(Team_All$Player)
glimpse(Team_All)



###################################################
# Clean the dataset
###################################################

Team_All$Year <- str_replace_all(Team_All$Year, ".csv", "")
#lapply converts to a list, sapply will keep it as is
Team_All$X.Rk <- sapply(Team_All$X.Rk, gsub, pattern = '"', replacement = "", fixed = TRUE) #Remove all " in the column
Team_All$Def. <- sapply(Team_All$Def., gsub, pattern = '"', replacement = "", fixed = TRUE) 
#colnames(Team_All)[18] <- "Rushing_TD" #Changes column name
#colnames(Team_All)[colnames(Team_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #
#Team_All$Rushing_TD <- as.integer(Team_All$Rushing_TD)


#Save the data
typeof(Team_All$X.Rk)
sapply(Team_All,class) #Type for all columns

#If your data is in a list do the below
#Team_All$X.Rk<- unlist(Team_All$X.Rk)
#Team_All$Rushing_TD<- unlist(Team_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()

write.csv(Team_All, file = "Team_All.csv")




#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

###################################################
# Read in the dataset 3
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data")  
getwd()

#Read in one file
#combine_1 <- read_csv('Combine_1.csv', col_names = TRUE, skip = 1, quote = "")

combine_1.csv <- read.csv('Combine_1.csv', skip = 0, sep=",", quote = "")
combine_2.csv <- read.csv('Combine_2.csv', skip = 0, sep=",", quote = "")


###################################################
# Create a year variable for indexing
###################################################
#Create the Year varaible for each dataframe

#Done for one case
#mutate(rating_1995.csv, Year = '1995' )

#Now for all
#Create a list
df_list <- list(combine_1.csv = combine_1.csv, combine_2.csv = combine_2.csv)

do.call("rbind", df_list)


###################################################
# Join the Dataset
###################################################

#Join the Dataset into one
Combine_All <- join_all((df_list), type = 'full') #plyr function

#Remove all the unnecessary data frames
rm(list = ls(pattern = ".csv"))



###################################################
# Explore the dataset
###################################################
head(Combine_All)
tail(Combine_All)
dim(Combine_All)
str(Combine_All)
summary(Combine_All)
names(Combine_All)
levels(Combine_All$Player)
glimpse(Combine_All)



###################################################
# Clean the dataset
###################################################

#lapply converts to a list, sapply will keep it as is
Combine_All$X.Rk <- sapply(Combine_All$X.Rk, gsub, pattern = '"', replacement = "", fixed = TRUE) #Remove all " in the column
Combine_All$Drafted..tm.rnd.yr.. <- sapply(Combine_All$Drafted..tm.rnd.yr.., gsub, pattern = '"', replacement = "", fixed = TRUE) 
#colnames(Combine_All)[18] <- "Rushing_TD" #Changes column name
#colnames(Combine_All)[colnames(Combine_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #
#Combine_All$Rushing_TD <- as.integer(Combine_All$Rushing_TD)
Combine_All$Player <- sapply(Combine_All$Player, gsub, pattern = '*', replacement = "", fixed = TRUE) 
Combine_All$Player <- sub("\\\\.*", "", Combine_All$Player)

#Save the data
typeof(Combine_All$X.Rk)
sapply(Combine_All,class) #Type for all columns

#If your data is in a list do the below
#Combine_All$X.Rk<- unlist(Combine_All$X.Rk)
#Combine_All$Rushing_TD<- unlist(Combine_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()

write.csv(Combine_All, file = "Combine_All.csv")






#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

###################################################
# Read in the dataset 4
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data")  
getwd()

#Read in one file
Coach_All <- read.csv('Coach.csv', skip = 1, sep=",", quote = "")


###################################################
# Explore the dataset
###################################################
head(Coach_All)
tail(Coach_All)
dim(Coach_All)
str(Coach_All)
summary(Coach_All)
names(Coach_All)
levels(Coach_All$Player)
glimpse(Coach_All)



###################################################
# Clean the dataset
###################################################

#lapply converts to a list, sapply will keep it as is
Coach_All$X.Rk <- sapply(Coach_All$X.Rk, gsub, pattern = '"', replacement = "", fixed = TRUE) #Remove all " in the column
Coach_All$Notes. <- sapply(Coach_All$Notes., gsub, pattern = '"', replacement = "", fixed = TRUE) 
#colnames(Coach_All)[18] <- "Rushing_TD" #Changes column name
#colnames(Coach_All)[colnames(Coach_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #
#Coach_All$Rushing_TD <- as.integer(Coach_All$Rushing_TD)


#Save the data
typeof(Coach_All$X.Rk)
sapply(Coach_All,class) #Type for all columns

#If your data is in a list do the below
#Coach_All$X.Rk<- unlist(Coach_All$X.Rk)
#Coach_All$Rushing_TD<- unlist(Coach_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()

write.csv(Coach_All, file = "Coach_All.csv")






#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

###################################################
# Read in the dataset 5
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data")
getwd()

#Read in one file
Probowl_All <- read.csv('Pro_Bowl.csv', skip = 0, sep=",", quote = "")


###################################################
# Explore the dataset
###################################################
head(Probowl_All)
tail(Probowl_All)
dim(Probowl_All)
str(Probowl_All)
summary(Probowl_All)
names(Probowl_All)
levels(Probowl_All$Player)
glimpse(Probowl_All)



###################################################
# Clean the dataset
###################################################

#lapply converts to a list, sapply will keep it as is
Probowl_All$Player <- sapply(Probowl_All$Player, gsub, pattern = '%', replacement = "", fixed = TRUE) #Remove all % in the column
Probowl_All$Player <- sapply(Probowl_All$Player, gsub, pattern = '+', replacement = "", fixed = TRUE) #Remove all % in the column
#colnames(Probowl_All)[18] <- "Rushing_TD" #Changes column name
#colnames(Probowl_All)[colnames(Probowl_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #
Probowl_All$Yrs <- as.integer(Probowl_All$Yrs)

#Save the data
typeof(Probowl_All$Player)
sapply(Probowl_All,class) #Type for all columns

#If your data is in a list do the below
#Probowl_All$X.Rk<- unlist(Probowl_All$X.Rk)
#Probowl_All$Rushing_TD<- unlist(Probowl_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()
write.csv(Probowl_All, file = "Probowl_All.csv")






#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

###################################################
# Read in the dataset 6
###################################################
setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data")
getwd()

#Read in one file
Wonderlic_All <- read.csv('Wonderlic.csv', skip = 0, sep=",", quote = "")


###################################################
# Explore the dataset
###################################################
head(Wonderlic_All)
tail(Wonderlic_All)
dim(Wonderlic_All)
str(Wonderlic_All)
summary(Wonderlic_All)
names(Wonderlic_All)
levels(Wonderlic_All$Player)
glimpse(Wonderlic_All)



###################################################
# Clean the dataset
###################################################
Wonderlic_All$Player <- as.character(Wonderlic_All$Player)
Wonderlic_All$Score <- as.integer(Wonderlic_All$Score)
Wonderlic_All <- filter(Wonderlic_All, Position == 'QB')
str(Wonderlic_All)
Wonderlic_All <- Wonderlic_All[order(Wonderlic_All$Player, abs(Wonderlic_All$Score) ), ] #sort by Player and shows lower score first
Wonderlic_All <- Wonderlic_All[!duplicated(Wonderlic_All$Player), ]# take the first of each player

#colnames(Wonderlic_All)[18] <- "Rushing_TD" #Changes column name
#colnames(Wonderlic_All)[colnames(Wonderlic_All) == 'TD.'] <- 'Rushing_TD' #Changes column name by using column names instead of column #

#Save the data
typeof(Wonderlic_All$Player)
sapply(Wonderlic_All,class) #Type for all columns

#If your data is in a list do the below
#Wonderlic_All$X.Rk<- unlist(Wonderlic_All$X.Rk)
#Wonderlic_All$Rushing_TD<- unlist(Wonderlic_All$Rushing_TD)
#df <- apply(df,2,as.character) quick hack to change all to character

setwd("~/Desktop/Windows Stuff Moved Over/Software Tools (Python, R, Tableau,etc)/Kaggle & Case studies/NFL_QB_20170519/Data/R_Created_Dataset")  
getwd()
write.csv(Wonderlic_All, file = "Wonderlic_All.csv")




#===================================================================================================================
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#===================================================================================================================

###################################################
# Join the Datasets
###################################################

Probowl_All$Pro_bowl <- 'Yes' #Creates a Yes column

#Subset the data to remove duplicates
Probowl_Subset <- Probowl_All %>%
  group_by(Player, Pro_bowl) %>%
  filter(Yrs == min(Yrs))

#Joins the dataset
Dataset_All <- left_join(Players_All, Combine_All, by = 'Player') %>%
  select(-X.Rk.y, -School.y, -College) %>%
  left_join(Wonderlic_All, by = 'Player') %>%
  left_join(select(Probowl_Subset,(Player), Pro_bowl), by ='Player') 

# ***Still need to add the coaches dataset with the team dataset****


###################################################
# Feature Engineering
###################################################
# Td - Int Ratio
Dataset_All <- Dataset_All %>%
  mutate(TD_INT_Ratio = TD/(TD+Int))


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

Dataset_Real <- Dataset_All_Dedup %>%
  filter(Year.x %in% c('2012', '2013', '2014', '2015', '2016', '2017'))

Dataset_Subset <- Dataset_All_Dedup %>%
  filter(!(Year.x %in% c('2012', '2013', '2014', '2015', '2016', '2017')))





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


# #normalize - Doesnt do it across the dataframe equally
# normalize <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }
# Dataset_Subset_Scale <- Dataset_Subset_All[(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))]
# Dataset_Subset_Scale <- as.data.frame(lapply(Dataset_Subset_Scale, normalize))
# colMeans(Dataset_Subset_Scale)
# apply(Dataset_Subset_Scale,2,sd)

#Normalize
Dataset_Subset_Scale <- Dataset_Subset_All[(c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))]
Dataset_Subset_Scale <- scale(Dataset_Subset_Scale)
colMeans(Dataset_Subset_Scale)
apply(Dataset_Subset_Scale,2,sd)

#Put it back together
Dataset_Subset_Scale <- as.data.frame(Dataset_Subset_Scale)
Dataset_Subset_Scale$Player <- Dataset_Subset$Player
Dataset_Subset_Scale$School <- Dataset_Subset$School
Dataset_Subset_Scale$Conf <- Dataset_Subset$Conf
Dataset_Subset_Scale$Year_1 <- Dataset_Subset$Year_1
Dataset_Subset_Scale$Pro_Bowl <- Dataset_Subset$Pro_Bowl
Dataset_Subset_All <- Dataset_Subset_Scale[,c(19,20,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                                15,16,17,18,22,23)]
