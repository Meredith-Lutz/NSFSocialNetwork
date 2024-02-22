library(RPostgreSQL)
library(chron)
library(stringr)
library(lme4)
library(lubridate)

setwd('C:/Users/mclutz/Box/GoogleDriveBackup/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

nn				<- read.csv('NearestNeighbor_TMM_ML_01Dec2021.xlsx - NearestNeighbor_TMM_ML_01Dec202.csv', stringsAsFactors = FALSE)
fullFocalListImport	<- read.csv('fullFocalListWithMissingSocialDataFocals2023-07-17Edited13UploadedForStudents.xlsx - fullFocalListWithMissingSocialD.csv', stringsAsFactors = FALSE)

fullFocalListSimp	<- fullFocalListImport[,c(1,10:14)]

nnWithSeconds	<- merge(nn, fullFocalListSimp, by.x = 'focalid', by.y = 'focalid', all.x = TRUE)
nnWithSeconds$X	<- nnWithSeconds$Second
nnWithSeconds$scanTimeWithSeconds	<- paste(nnWithSeconds$time.hour, nnWithSeconds$time.minute, nnWithSeconds$Second, sep = ":")

write.csv(nnWithSeconds, 'nnWithSeconds.csv', row.names = FALSE)