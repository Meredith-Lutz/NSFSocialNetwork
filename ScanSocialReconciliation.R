###########################################################
###########################################################
##### Reconciling scan and continuous datasets - KMNP #####
###########################################################
###########################################################
library(RPostgreSQL)
library(chron)
library(stringr)
library(lme4)
library(lubridate)

setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_2022_06_09_ML.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021_NoBlanks.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('NearestNeighborIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('FocalActivityIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
filemakerFocalList	<- read.csv('FileMakerIDs_ML_06Dec2021.csv', stringsAsFactors = FALSE)
nn				<- read.csv('NearestNeighbor_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actv				<- read.csv('FocalActivity_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
fm				<- read.csv('FileMaker_ML_01Dec2021.csv', stringsAsFactors = FALSE)
demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)

colnames(fm)[c(2, 4, 9)]	<- c('date', 'observer', 'group')

demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name

scanAll			<- rbind.data.frame(nn[,c('observer', 'date', 'group')], 
					actv[,c('observer', 'date', 'group')],
					fm[,c('observer', 'date', 'group')])

socialDataAllRaw		<- socialDataRaw[,c('OriginalFile', 'Observer', 'Obs.ID', 'Date', 'Month', 'Year', 'Focal', 'Start', 'Stop',
					'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context', 'Behavior', 'Species',
					'Tree.number', 'Response', 'To', 'Win', 'Comments', 'Cleaning.Comments', 'StudentOb.YN')]

#For Becca
socialDataAllRaw		<- socialDataRaw

# Change baby names to match LH file
socialDataAllRaw$Initiator	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Initiator	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Receiver	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Receiver)
socialDataAllRaw$Receiver	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Receiver)

# Remove lines that have non-identified individuals
socialData		<- socialDataAllRaw[socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames,]
socialDataRemoved	<- socialDataAllRaw[!(socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames),]

##############################
### Visits versus in group ###
##############################
groups$visit	<- 'N'
groups[groups$date == '2019-09-23' & groups$animal == 'Zavona' & groups$group == 'III', 'visit']	<- 'Y'
groups[groups$date == '2019-08-11' & groups$animal == 'Spirit' & groups$group == 'Solitary', 'visit']	<- 'Y'
groups[groups$date == '2019-07-26' & groups$animal == 'Spirit' & groups$group == 'III', 'visit']	<- 'Y'
groups[groups$date == '2019-02-04' & groups$animal == 'Spirit' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$date == '2019-02-01' & groups$animal == 'Spirit' & groups$group == 'IV', 'visit']	<- 'Y'
groups[groups$date == '2019-03-10' & groups$animal == 'Spirit' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$date == '2019-03-10' & groups$animal == 'Zavona' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$date == '2019-02-04' & groups$animal == 'Zavona' & groups$group == 'XI', 'visit']	<- 'Y'
groups[groups$date == '2019-02-01' & groups$animal == 'William' & groups$group == 'IV', 'visit']	<- 'Y'
groups[groups$date == '2018-06-02' & groups$animal == 'Zoma' & groups$group == 'II', 'visit']	<- 'Y'
groups[groups$date == '2017-10-16' & groups$animal == 'Valdes' & groups$group == 'IV', 'visit']	<- 'Y'

groupsNoVisits	<- groups[groups$visit == 'N',]

#Merging groupNames back onto socialData
socialData	<- merge(socialData, groupsNoVisits[,1:3], by.x = c("Date", "Focal"), by.y = c("date", "animal"), all.x = TRUE)

#################################################################
### Trying to figure out who has which datasets on which days ###
#################################################################
socialDayObserverSum	<- aggregate(socialData$Date, by = list(observer = socialData$Observer, date = socialData$Date), FUN = length)
scanDayObserverSum	<- aggregate(scanAll$date, by = list(observer = scanAll$observer, date = scanAll$date, group = scanAll$group), FUN = length)
fileMakerDayObserverSum	<- aggregate(filemaker$Date, by = list(observer = filemaker$Observer, date = filemaker$Date), FUN = length)

#allScanObserverSum	<- rbind.data.frame(scanDayObserverSum, fileMakerDayObserverSum)

andryScan			<- scan[scan$Observer == 'Andry',]
danielScan			<- scan[scan$Observer == 'Daniel',]
maxScan			<- scan[scan$Observer == 'Max',]
francisScan			<- scan[scan$Observer == 'Francis',]
patrickScan			<- scan[scan$Observer == 'Patrick',]

days		<- sort(unique(c(socialDayObserverSum$date, scanDayObserverSum$date)))
observers	<- sort(unique(c(socialDayObserverSum$observer, scanDayObserverSum$observer)))
daysToRemoveFromScan	<- data.frame(observer = character(), date = character())
daysToRemoveFromSocial	<- data.frame(observer = character(), date = character())
for(i in 1:length(observers)){
	obsNam	<- observers[i]
	print(paste('Checking', obsNam, "'s data for errors"))
	tempDaysToRemoveFromSocial	<- c()
	tempDaysToRemoveFromScan	<- c()
	for(j in days){ #0 means that data wasn't collected
		nScans	<- scanDayObserverSum[scanDayObserverSum$observer == obsNam & scanDayObserverSum$date == j, 3]
		#print(nScans)
		nBehav	<- socialDayObserverSum[socialDayObserverSum$observer == obsNam & socialDayObserverSum$date == j, 3]
		#print(nBehav)
		if(length(nScans) == 0 & length(nBehav) != 0){
			print(paste(obsNam, 'has social data but no scan data for', j))
			tempDaysToRemoveFromSocial	<- c(tempDaysToRemoveFromSocial, j)
		}
		if(length(nScans) != 0 & length(nBehav) == 0){
			print(paste(obsNam, 'has scan data but no social data for', j))
			tempDaysToRemoveFromScan	<- c(tempDaysToRemoveFromScan, j)
		}
	}
	print(paste('Finished checking', obsNam, "'s data for errors."))
	daysToRemoveFromScan		<- rbind(daysToRemoveFromScan, cbind(rep(obsNam, length(tempDaysToRemoveFromScan)), tempDaysToRemoveFromScan))
	daysToRemoveFromSocial		<- rbind(daysToRemoveFromSocial, cbind(rep(obsNam, length(tempDaysToRemoveFromSocial)), tempDaysToRemoveFromSocial))

}

##################################################
### Add Focal ID from Focal List to Social Data###
##################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
fullFocalList$yearMonth	<- substr(fullFocalList$date,1,7)

socialData$focalID		<- NA
fullFocalList$adjStopTime	<- NA
socialData$Start			<- format(as.POSIXlt(socialData$Start, format = "%H:%M:%S"), format = "%H:%M:%S")
socialData$Stop			<- format(as.POSIXlt(socialData$Stop, format = "%H:%M:%S"), format = "%H:%M:%S")
fullFocalList$start_time	<- format(as.POSIXlt(fullFocalList$start_time, format = "%H:%M:%S"), format = "%H:%M:%S")
fullFocalList$stop_time		<- format(as.POSIXlt(fullFocalList$stop_time, format = "%H:%M:%S"), format = "%H:%M:%S")

for (i in 1:nrow(fullFocalList)){
	print(i)
	focalObserver	<- fullFocalList[i,"observer"]
	#print(focalObserver)
	focalAnimal	<- fullFocalList[i,"focal_animal"]
	#print(focalAnimal)
	focalDate	<- fullFocalList[i,"date"]
	#print(focalDate)
	focalStartTime	<- fullFocalList[i,"start_time"]
	#print(focalStartTime)
	focalStopTime	<- fullFocalList[i,"stop_time"]
	#print(focalStopTime)
	focalid		<- fullFocalList[i,"focalid"]
	#print(class(focalStartTime))
	socialData[socialData$Observer == focalObserver &
			socialData$Focal == focalAnimal & socialData$Date == focalDate & is.na(socialData$Start) == FALSE &
			socialData$Start >= focalStartTime & socialData$Start <= focalStopTime, "focalID"]	<- focalid 	

	nLines	<- nrow(socialData[socialData$focalID == focalid & (is.na(socialData$focalID) == FALSE),])
	
	if(nLines > 0){
		#Calculate actual stop time of focal
		maxBehaviorStopTime	<- max(socialData[socialData$focalID == focalid & is.na(socialData$focalID) == FALSE, "Stop"], na.rm = TRUE)
		actualFocalStopTime	<- ifelse(maxBehaviorStopTime > focalStopTime, maxBehaviorStopTime, focalStopTime)

		fullFocalList[i,]$adjStopTime	<- actualFocalStopTime

		#Add in the extra lines
		socialData[socialData$Observer == focalObserver &
			socialData$Focal == focalAnimal & socialData$Date == focalDate & 
			socialData$Start >= focalStartTime & socialData$Start <= actualFocalStopTime, "focalID"]	<- fullFocalList[i,"focalid"]  
	}
}

socialData$Start	<- format(socialData$Start, format = '%H:%M:%S')
socialData$Stop	<- format(socialData$Stop, format = '%H:%M:%S')

socialData	<- socialData[order(socialData$Observer, socialData$Date, socialData$Start, socialData$Stop),]

write.csv(socialData[,c(1:48, 50)], "allSocialDataWithFocalIDs2022-06-13.csv", row.names = FALSE)

##############################################
### Identify Which Focals Need Social Data ###
##############################################
socialDataWithID		<- read.csv("allSocialDataWithFocalIDs2022-06-13.csv")

fullFocalList$start_time	<- format(fullFocalList$start_time, format = "%H:%M:%S")
fullFocalList$stop_time		<- format(fullFocalList$stop_time, format = "%H:%M:%S")

focalsNoSocialData		<- fullFocalList[!(fullFocalList$focalid %in% unique(socialDataWithID$focalID)),]

summarizeNNFocals			<- aggregate(nn$yes_socialdata, by = list(focalid = nn$focalid, date = nn$date, group = nn$group, focal = nn$focal_animal), FUN = sum, na.rm = TRUE)
trulyNoSocialDataNN		<- fullFocalList[fullFocalList$focalid %in% summarizeNNFocals[summarizeNNFocals$x == 0, "focalid"],]
socialDataNeedsEnteringNN	<- fullFocalList[fullFocalList$focalid %in% summarizeNNFocals[summarizeNNFocals$x > 0, "focalid"],]

summarizeActvFocals		<- aggregate(actv$yes_socialdata, by = list(focalid = actv$focalid, date = actv$date, group = actv$group, focal = actv$focal_animal), FUN = sum)
trulyNoSocialDataActv		<- fullFocalList[fullFocalList$focalid %in% summarizeActvFocals[summarizeActvFocals$x == 0, "focalid"],]
socialDataNeedsEnteringActv	<- fullFocalList[fullFocalList$focalid %in% summarizeActvFocals[summarizeActvFocals$x > 0, "focalid"],]

fileMakerTrulyNoSocialData		<- focalsNoSocialData[focalsNoSocialData$date < "2013-06-01",]

focalsWithSocialDataOrTrulyNone	<- rbind.data.frame(fullFocalList[fullFocalList$focalid %in% unique(socialDataWithID$focalID), ], trulyNoSocialDataActv, trulyNoSocialDataNN, fileMakerTrulyNoSocialData)

socialDataFinal			<- socialDataWithID[socialDataWithID$focalID %in% unique(focalsWithSocialDataOrTrulyNone$focalid), ]
socialDataScansNeedEntered	<- socialDataWithID[!socialDataWithID$focalID %in% unique(focalsWithSocialDataOrTrulyNone$focalid), ]

write.csv(socialDataFinal, 'socialDataFinalForBLAnalysis2021-12-13.csv', row.names = FALSE)
write.csv(focalsWithSocialDataOrTrulyNone[,1:9], 'focalListFinalForBLAnalysis2021-12-13.csv', row.names = FALSE)

write.csv(socialDataNeedsEnteringActv[,1:9], 'socialDataMissingFocalActivityEntered2021-12-13.csv', row.names = FALSE)
write.csv(socialDataNeedsEnteringNN[,1:9], 'socialDataMissingNearestNeighborEntered2021-12-13.csv', row.names = FALSE)
write.csv(trulyNoSocialDataNN[,1:9], 'NoSocialDataNearestNeighbor2021-12-13.csv', row.names = FALSE)
write.csv(trulyNoSocialDataActv[,1:9], 'NoSocialDataFocalActivity2021-12-13.csv', row.names = FALSE)
write.csv(socialDataScansNeedEntered, 'instantaneousDataMissingSocialEntered2022-06-13.csv', row.names = FALSE)


