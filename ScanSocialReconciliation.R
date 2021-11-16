#################################################
#################################################
##### Long term network demographics - KMNP #####
#################################################
#################################################

library(RPostgreSQL)
library(chron)
library(stringr)


setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
socialData		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_Francis duplicates deleted_Jul262021_ML_2021_11_10.csv', stringsAsFactors = FALSE)
focalActv		<- read.csv('FocalActivity_TMM_ML_11Nov2021.csv', stringsAsFactors = FALSE)
filemaker		<- read.csv('FileMaker_ML_10Nov2021.csv', stringsAsFactors = FALSE)
nn			<- read.csv('NearestNeighbor_TMM_ML_10Nov2021.csv', stringsAsFactors = FALSE)
colnames(filemaker)[c(2, 4, 9)]	<- c('date', 'observer', 'group')

socialData$monthNum	<- ifelse(socialData$Month == 'Jan', '01', 
					ifelse(socialData$Month == 'Feb', '02',
					ifelse(socialData$Month == 'Mar', '03',
					ifelse(socialData$Month == 'Apr', '04',
					ifelse(socialData$Month == 'May', '05',
					ifelse(socialData$Month == 'Jun', '06',
					ifelse(socialData$Month == 'Jul', '07',
					ifelse(socialData$Month == 'Aug', '08',
					ifelse(socialData$Month == 'Sep', '09',
					ifelse(socialData$Month == 'Oct', '10',
					ifelse(socialData$Month == 'Nov', '11', '12')))))))))))

focalActv$monthNum	<- ifelse(focalActv$Month == 'Jan', '01', 
					ifelse(focalActv$Month == 'Feb', '02',
					ifelse(focalActv$Month == 'Mar', '03',
					ifelse(focalActv$Month == 'Apr', '04',
					ifelse(focalActv$Month == 'May', '05',
					ifelse(focalActv$Month == 'Jun', '06',
					ifelse(focalActv$Month == 'Jul', '07',
					ifelse(focalActv$Month == 'Aug', '08',
					ifelse(focalActv$Month == 'Sep', '09',
					ifelse(focalActv$Month == 'Oct', '10',
					ifelse(focalActv$Month == 'Nov', '11', '12')))))))))))

socialData$yearMonth	<- paste(socialData$Year, socialData$monthNum, sep = '-')
scan$yearMonth		<- paste(scan$Year, scan$monthNum, sep = '-')

scanAll	<- rbind.data.frame(nn[,c('observer', 'date', 'group')], focalActv[,c('observer', 'date', 'group')], filemaker[,c('observer', 'date', 'group')])

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

