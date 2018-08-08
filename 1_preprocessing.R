'''
###################################################
title: NFL QB Predictor
author: Kevin Huang
date: 20170519(original date), Edits made 20180720
output: Datasets used for Analysis - 6 Total Datasets 
Notes: Preprocessing: Load and Clean Data to create the Datasets
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
# Read in the dataset 1 
###################################################
# Creating variables for the path of data files
data_path = "/Users/kevin8523/Desktop/Github/nfldraft_qb_study/raw_data"
save_path = "/Users/kevin8523/Desktop/Github/nfldraft_qb_study/data_created" # Since I am keeping data seperate from the save I just made an extra location

# Set working directory 
setwd(data_path)
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
tail(Players_All)
dim(Players_All)
str(Players_All)
summary(Players_All)
names(Players_All)
levels(Players_All$Player)
glimpse(Players_All)



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

setwd(save_path)  
getwd()
write.csv(Players_All, file = "Players_All.csv")

#===================================================================================================================
#===================================================================================================================

###################################################
# Read in the dataset 2
###################################################
setwd(data_path)
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

setwd(save_path)  
getwd()

write.csv(Team_All, file = "Team_All.csv")


#===================================================================================================================
#===================================================================================================================


###################################################
# Read in the dataset 3
###################################################
setwd(data_path)
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

setwd(save_path)  
getwd()

write.csv(Combine_All, file = "Combine_All.csv")


#===================================================================================================================
#===================================================================================================================


###################################################
# Read in the dataset 4
###################################################
setwd(data_path)
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

setwd(save_path)  
getwd()

write.csv(Coach_All, file = "Coach_All.csv")


#===================================================================================================================
#===================================================================================================================


###################################################
# Read in the dataset 5
###################################################
setwd(data_path)
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

setwd(save_path)  
getwd()
write.csv(Probowl_All, file = "Probowl_All.csv")


#===================================================================================================================
#===================================================================================================================


###################################################
# Read in the dataset 6
###################################################
setwd(data_path)
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

setwd(save_path)  
getwd()
write.csv(Wonderlic_All, file = "Wonderlic_All.csv")


#===================================================================================================================
#===================================================================================================================


###################################################
# Create the data for EDA - Join the Datasets
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


#Save the data
setwd(save_path)  
getwd()
write.csv(Dataset_All, file = "Dataset_All.csv")
