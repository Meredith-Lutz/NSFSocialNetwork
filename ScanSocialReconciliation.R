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

#setwd('D:/Box/GoogleDriveBackup/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

setwd('C:/Users/mclutz/Box/GoogleDriveBackup/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

socialDataRaw		<- read.csv('socialDataWithIDEditedPartially46.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021_NoBlanks.csv', stringsAsFactors = FALSE)
nnFocalList			<- read.csv('NearestNeighborIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actvFocalList		<- read.csv('FocalActivityIDs_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
filemakerFocalList	<- read.csv('FileMakerIDs_ML_06Dec2021.csv', stringsAsFactors = FALSE)
nn				<- read.csv('NearestNeighbor_TMM_ML_01Dec2021.csv', stringsAsFactors = FALSE)
actv				<- read.csv('FocalActivity_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
fm				<- read.csv('FileMaker_ML_01Dec2021.csv', stringsAsFactors = FALSE)
demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)
fullFocalListImport	<- read.csv('fullFocalListWithMissingSocialDataFocals2023-07-17Edited34.csv', stringsAsFactors = FALSE)
focalListFromStudents	<- read.csv('FocalListFromStudents.csv', stringsAsFactors = FALSE)
dataUT			<- read.csv('UTSocialDataEntered2017_MLCleaned4.csv', stringsAsFactors = FALSE)
dataDavis2023		<- read.csv('UCD_SocialDataEntry_2023.csv', stringsAsFactors = FALSE)

colnames(fm)[c(2, 4, 9)]	<- c('date', 'observer', 'group')

demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name

scanAll			<- rbind.data.frame(nn[,c('observer', 'date', 'group')], 
					actv[,c('observer', 'date', 'group')],
					fm[,c('observer', 'date', 'group')])

socialDataAllRaw		<- socialDataRaw[,c('X...OriginalFile', 'Observer', 'Obs.ID', 'ï..Ã...Date', 'Month', 'Year', 'Focal', 'Start', 'Stop',
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

# For Becca
socialData	<- socialDataAllRaw

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


##################################################
### Add Focal ID from Focal List to Social Data###
##################################################
fullFocalList	<- rbind.data.frame(filemakerFocalList, nnFocalList, actvFocalList, stringsAsFactors = FALSE)
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
fullFocalList$yearMonth	<- substr(fullFocalList$date,1,7)

#fullFocalListImport$combinedStartTime	<- NA
#fullFocalListImport$combinedStopTime	<- NA
#for(i in 1:nrow(fullFocalListImport)){
#	if(fullFocalListImport[i, 10] == ''){
#		print('entered if')
#		fullFocalListImport[i, 18]	<- fullFocalListImport[i, 6]
#	}
#	else{
#		print('entered else')
#		fullFocalListImport[i, 18]	<- fullFocalListImport[i, 10]
#	}
#}

#for(i in 1:nrow(fullFocalListImport)){
#	if(fullFocalListImport[i, 11] == ''){
#		print('entered if')
#		fullFocalListImport[i, 19]	<- fullFocalListImport[i, 7]
#	}
#	else{
#		print('entered else')
#		fullFocalListImport[i, 19]	<- fullFocalListImport[i, 11]
#	}
#}

fullFocalList	<- fullFocalListImport

#socialData$focalIDNew		<- NA
#socialData$Start			<- format(as.POSIXlt(socialData$Start, format = "%H:%M:%S"), format = "%H:%M:%S")
#socialData$Stop			<- format(as.POSIXlt(socialData$Stop, format = "%H:%M:%S"), format = "%H:%M:%S")

dataDavis2023$focalIDNew		<- NA
dataDavis2023$Start			<- format(as.POSIXlt(dataDavis2023$Start, format = "%H:%M:%S"), format = "%H:%M:%S")
dataDavis2023$Stop			<- format(as.POSIXlt(dataDavis2023$Stop, format = "%H:%M:%S"), format = "%H:%M:%S")

#dataUT$focalID			<- dataUT$focalIDNew
#dataUT$focalIDNew		<- NA
#dataUT$Start			<- format(as.POSIXlt(dataUT$Start, format = "%H:%M:%S"), format = "%H:%M:%S")
#dataUT$Finish			<- format(as.POSIXlt(dataUT$Finish, format = "%H:%M:%S"), format = "%H:%M:%S")

fullFocalList$combinedStartTime	<- format(as.POSIXlt(fullFocalList$combinedStartTime, format = "%H:%M:%S"), format = "%H:%M:%S")
fullFocalList$combinedStopTime	<- format(as.POSIXlt(fullFocalList$combinedStopTime, format = "%H:%M:%S"), format = "%H:%M:%S")

socialData	<- socialData[,c(1:55)] #remove merged focal list columns

for (i in 1:nrow(fullFocalList)){
	print(i)
	focalObserver	<- fullFocalList[i, "observer"]
	focalAnimal		<- fullFocalList[i, "focal_animal"]
	focalDate		<- fullFocalList[i, "date"]
	focalStartTime	<- fullFocalList[i, "combinedStartTime"]
	focalStopTime	<- fullFocalList[i, "combinedStopTime"]
	focalid		<- fullFocalList[i, "ï..Ã...focalID"]

	#socialData[socialData$Observer == focalObserver &
	#		socialData$Focal == focalAnimal & socialData$ï..date == focalDate & is.na(socialData$Start) == FALSE &
	#		socialData$Start >= focalStartTime & socialData$Start <= focalStopTime, "focalIDNew"]	<- focalid 
	
	dataDavis2023[dataDavis2023$Observer == focalObserver &
			dataDavis2023$Focal == focalAnimal & dataDavis2023$Date == focalDate & is.na(dataDavis2023$Start) == FALSE &
			dataDavis2023$Start >= focalStartTime & dataDavis2023$Start <= focalStopTime, "focalIDNew"]	<- focalid

	#dataUT[dataUT$ï..Ã...Observer == focalObserver &
	#		dataUT$Focal == focalAnimal & dataUT$Date == focalDate & is.na(dataUT$Start) == FALSE &
	#		dataUT$Start >= focalStartTime & dataUT$Start <= focalStopTime, "focalIDNew"]	<- focalid 	
}

#socialData$Start	<- format(socialData$Start, format = '%H:%M:%S')
#socialData$Stop	<- format(socialData$Stop, format = '%H:%M:%S')
#socialData$Date	<- as.Date(socialData$ï..date, format = c("%m/%d/%Y"))

#socialDataWithID	<- socialData[order(socialData$Observer, socialData$ï..date, socialData$Start, socialData$Stop),]
#socialDataWithID$entry	<- 1:nrow(socialDataWithID)
#socialDatWithIDUnedited	<- socialDataWithID

dataDavis2023$Start	<- format(dataDavis2023$Start, format = '%H:%M:%S')
dataDavis2023$Stop	<- format(dataDavis2023$Stop, format = '%H:%M:%S')
dataDavis2023$Date	<- as.Date(dataDavis2023$Date, format = c("%m/%d/%Y"))

dataDavis2023WithID	<- dataDavis2023[order(dataDavis2023$Observer, dataDavis2023$Date, dataDavis2023$Start, dataDavis2023$Stop),]
dataDavis2023WithID$entry	<- 1:nrow(dataDavis2023WithID)

#UTDataWithID	<- dataUT[order(dataUT$ï..Ã...Observer, dataUT$Date, dataUT$Start, dataUT$Finish),]
#UTDataWithID$entry	<- 1:nrow(UTDataWithID)

#usedFocalIDs	<- unique(socialData$focalIDNew, dataUT$focalIDNew)

#write.csv(UTDataWithID, 'UTSocialDataEntered2017_MLCleaned5.csv', row.names = FALSE)

#fullFocalList$Social.data.entered5	<- NA
#fullFocalList[fullFocalList$ï..focalID %in% usedFocalIDs, 'Social.data.entered5']	<- 'Social data entered'
#alreadyMarked	<- focalListFromStudents[,c(1, 13)]
#fullFocalListWithAlreadyMarked	<- merge(fullFocalList, alreadyMarked, by.x = 'ï..focalID', by.y = 'Ã...Ãƒ...focalid', all.x = TRUE)

#write.csv(fullFocalListWithAlreadyMarked, 'fullFocalListWithMissingSocialDataFocals2023-07-17Edited31.csv', row.names = FALSE)

#################################################
### check match between approach and withdraw ###
#################################################
dataDavis2023WithID	<- merge(dataDavis2023WithID, fullFocalList, by.x = 'focalIDNew', by.y = 'ï..Ã...focalID', all.x = TRUE)

dataDavis2023WithID$dyadID	<- NA
for(m in 1:nrow(dataDavis2023WithID)){
	print(m)
	dataDavis2023WithID[m,'dyadID']	<- paste(sort(c(dataDavis2023WithID[m, 'Initiator'], dataDavis2023WithID[m,'Receiver']), decreasing = FALSE), collapse = '')
}

write.csv(dataDavis2023WithID, 'davisData2023_MLCleaned1.csv', row.names = FALSE)

errorRows	<- data.frame()
for(i in unique(dataDavis2023WithID$focalIDNew)){
	print(i)
	subset	<- socialDataWithFocalInfo[dataDavis2023WithID$focalIDNew == i & is.na(dataDavis2023WithID$focalIDNew) == FALSE,]
	usedDyads	<- unique(subset$dyadID)
	for(j in usedDyads){
		sub2	<- subset[subset$dyadID == j,]

		app1m	<- sub2[sub2$Behavior %in% c('Approach_1m', 'Within_1m'),]
		matchForApp1m	<- sub2[sub2$Behavior %in% c('Approach_contact', 'Withdraw_greater_than_1m', 'Contact_out_of_sight', 'Flee_less_than_2m', 'Approach_contact_out_of_sight', 'Flee_greater_than_2m', 'Within_1m', 'Withdraw_greater_than_1m_out_of_sight'),]
		if(nrow(app1m) >= 1){
			for(k in 1:nrow(app1m)){
				appEndTime	<- app1m[k, 'Stop']
				if(appEndTime == app1m[k, 'combinedStopTime']){
					next
				}
				else{
					matchingEnd	<- matchForApp1m[matchForApp1m$Start == appEndTime,]
					if(nrow(matchingEnd) != 1){
						errorRows	<- rbind.data.frame(errorRows, app1m[k,], matchingEnd)
					}
					else{
						print('app 1m matched correctly')
					}
				}
			}
		}

		appCnt		<- sub2[sub2$Behavior %in% c('Approach_contact', 'Contact_out_of_sight'),]
		matchForAppCnt	<- sub2[sub2$Behavior %in% c('Withdraw_within_1m', 'Withdraw_greater_than_1m', 'Flee_less_than_2m', 'Flee_greater_than_2m', 'Withdraw_within_1m_out_of_sight', 'Withdraw_greater_than_1m_out_of_sight'),]
		if(nrow(appCnt)>= 1){
			for(k in 1:nrow(appCnt)){
				appEndTime	<- appCnt[k, 'Stop']
				if(appEndTime == appCnt[k, 'combinedStopTime']){
					next
				}
				else{
					matchingEnd	<- matchForAppCnt[matchForAppCnt$Start == appEndTime,]
					if(nrow(matchingEnd) != 1){
						errorRows	<- rbind.data.frame(errorRows, appCnt[k,], matchingEnd)
					}
					else{
						print('app cnt matched correctly')
					}
				}
			}
		}
	}
}

write.csv(errorRows, 'davisNotMatchedApp1.csv', row.names = FALSE)

errorRowsWithdraw	<- data.frame()
for(i in unique(dataDavis2023WithID$focalIDNew)){
	print(i)
	subset	<- dataDavis2023WithID[dataDavis2023WithID$focalIDNew == i & is.na(dataDavis2023WithID$focalIDNew) == FALSE,]
	usedDyads	<- unique(subset$dyadID)
	for(j in usedDyads){
		sub2	<- subset[subset$dyadID == j,]

		with1m		<- sub2[sub2$Behavior %in% c('Withdraw_within_1m', 'Withdraw_within_1m_out_of_sight'),]
		matchForWith1m	<- sub2[sub2$Behavior %in% c('Approach_contact', 'Approach_contact_out_of_sight', 'Contact_out_of_sight'),]
		if(nrow(with1m) >= 1){
			for(k in 1:nrow(with1m)){
				withStartTime		<- with1m[k, 'Start']
				matchingStart		<- matchForWith1m[matchForWith1m$Stop == withStartTime,]
				if(nrow(matchingStart) != 1){
					errorRowsWithdraw	<- rbind.data.frame(errorRowsWithdraw, with1m[k,], matchingStart)
				}
				else{
					print('withdraw within 1m matched correctly')
				}

			}
		}

		withGreater1m		<- sub2[sub2$Behavior %in% c('Withdraw_greater_than_1m', 'Withdraw_greater_than_1m_out_of_sight', 'Withdraw_greater_than_1m, changed start time'),]
		matchForWithGreater1m	<- sub2[sub2$Behavior %in% c('Withdraw_within_1m', 'Withdraw_within_1m_out_of_sight', 'Approach_contact_out_of_sight', 'Approach_1m', 'Approach_contact', 'Contact_out_of_sight', 'Within_1m'),]
		if(nrow(withGreater1m)>= 1){
			for(k in 1:nrow(withGreater1m)){
				withTime		<- withGreater1m[k, 'Start']
				matchingStart2	<- matchForWithGreater1m[matchForWithGreater1m$Stop == withTime,]
				if(nrow(matchingStart2) != 1){
					errorRowsWithdraw	<- rbind.data.frame(errorRowsWithdraw, withGreater1m[k,], matchingStart2)
				}
				else{
					print('withdraw greater than 1m matched correctly')
				}
			}
		}
	}
}

write.csv(errorRowsWithdraw, 'davisNotMatchedWith.csv', row.names = FALSE)


#####################################
### Try and match remaining lines ###
#####################################
lineIDsWithNoFocalMatched	<- socialDataWithID[is.na(socialDataWithID$focalID) == TRUE,'entry']
notYetDone	<- lineIDsWithNoFocalMatched[9392:10239]

completelyMissingScanData	<- NA
for(i in lineIDsWithNoFocalMatched){
	print(i)
	observer	<- socialDataWithID[socialDataWithID$entry == i, 'Observer']
	date		<- socialDataWithID[socialDataWithID$entry == i, 'Date']
	focalIndiv	<- socialDataWithID[socialDataWithID$entry == i, 'Focal']
	possiblyRelevantFocals	<- fullFocalList[fullFocalList$observer == observer & fullFocalList$date == date & fullFocalList$focal_animal == focalIndiv,]
	print(nrow(possiblyRelevantFocals))	
	if(nrow(possiblyRelevantFocals) == 0){ ### no scan data exists
		completelyMissingScanData	<- c(completelyMissingScanData, i)
		#next
	}
	#else{ ## there is some possibly useful scan data
	#	socialLineToMatch		<- socialDataWithID[socialDataWithID$entry == i, c(1, 2, 4, 9:12, 13:17, 24:26, 29:30, 45, 49:51)]
	#	print(socialLineToMatch)
	#	print(possiblyRelevantFocals)
	#	choice <- readline(prompt="Enter which focal this line matches: ")
	#	chosenFocalID	<- possiblyRelevantFocals[as.numeric(choice), 'focalid']
	#	socialDataWithID[socialDataWithID$entry == i, 'focalID']	<- chosenFocalID
	#}
}

##Update focal list
fullFocalList$adjStartBasedOnSocial	<- NA
fullFocalList$adjStopBasedOnSocial	<- NA

for (i in 1:nrow(fullFocalList)){
	print(i)
	focalid		<- fullFocalList[i,"focalid"]
	focalStartTime	<- fullFocalList[i,"start_time"]
	#print(focalStartTime)
	focalStopTime	<- fullFocalList[i,"stop_time"]
	#print(focalStopTime)

	nLines	<- nrow(socialDataWithID[socialDataWithID$focalID == focalid & (is.na(socialDataWithID$focalID) == FALSE),])

	if(nLines > 0){
		#Calculate actual stop time of focal
		minBehaviorStartTime	<- min(socialDataWithID[socialDataWithID$focalID == focalid & is.na(socialDataWithID$focalID) == FALSE, "Start"], na.rm = TRUE)
		maxBehaviorStopTime	<- max(socialDataWithID[socialDataWithID$focalID == focalid & is.na(socialDataWithID$focalID) == FALSE, "Stop"], na.rm = TRUE)
		print(minBehaviorStartTime)
		actualFocalStartTime	<- ifelse(minBehaviorStartTime < focalStartTime, minBehaviorStartTime, focalStartTime)
		actualFocalStopTime	<- ifelse(maxBehaviorStopTime > focalStopTime, maxBehaviorStopTime, focalStopTime)

		fullFocalList[i,]$adjStartBasedOnSocial	<- actualFocalStartTime
		fullFocalList[i,]$adjStopBasedOnSocial	<- actualFocalStopTime
	}
}

adjustedStartTimes	<- fullFocalList[fullFocalList$adjStartBasedOnSocial != fullFocalList$start_time & is.na(fullFocalList$adjStartBasedOnSocial) == FALSE,]

write.csv(socialDataWithID, 'socialDataWithIDEditedPartially.csv')
write.csv(adjustedStartTimes, 'focalsWhereSocialDataBeginsBeforeScan.csv')
write.csv(completelyMissingScanData, 'missingLargeChunkofScanData.csv')

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

write.csv(socialDataFinal, 'socialDataFinalForBLAnalysis2023-07-17.csv', row.names = FALSE)
write.csv(focalsWithSocialDataOrTrulyNone[,1:9], 'focalListFinalForBLAnalysis2023-07-17.csv', row.names = FALSE)
write.csv(fullFocalList[,1:9], 'fullFocalListWithMissingSocialDataFocals2023-07-17.csv', row.names = FALSE)

write.csv(socialDataNeedsEnteringActv[,1:9], 'missingSocialDataFocalActivityEntered2023-07-17.csv', row.names = FALSE)
write.csv(socialDataNeedsEnteringNN[,1:9], 'missingSocialDataNearestNeighborEntered2023-07-17.csv', row.names = FALSE)
write.csv(trulyNoSocialDataNN[,1:9], 'NoSocialDataNearestNeighbor2023-07-17.csv', row.names = FALSE)
write.csv(trulyNoSocialDataActv[,1:9], 'NoSocialDataFocalActivity2023-07-17.csv', row.names = FALSE)
write.csv(socialDataScansNeedEntered, 'missingInstantaneousDataSocialEntered2023-07-17.csv', row.names = FALSE)


