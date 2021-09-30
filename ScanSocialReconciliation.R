#################################################
#################################################
##### Long term network demographics - KMNP #####
#################################################
#################################################

library(RPostgreSQL)
library(chron)
library(stringr)


setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
socialData		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_2021_07_09_ML_BL edits for NSFanalysis_Francis duplicates deleted_Jul262021_MLEdits.csv', stringsAsFactors = FALSE)
scan		<- read.csv('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/Meredith Corrected KMNP Long Term Data/Cleaned Data/Focal Activity NN combined clean 11-16-20_ML.csv', stringsAsFactors = FALSE)
filemaker	<- read.csv('Instantaneous FileMaker data2.csv', stringsAsFactors = FALSE)

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

scan$monthNum	<- ifelse(scan$Month == 'Jan', '01', 
					ifelse(scan$Month == 'Feb', '02',
					ifelse(scan$Month == 'Mar', '03',
					ifelse(scan$Month == 'Apr', '04',
					ifelse(scan$Month == 'May', '05',
					ifelse(scan$Month == 'Jun', '06',
					ifelse(scan$Month == 'Jul', '07',
					ifelse(scan$Month == 'Aug', '08',
					ifelse(scan$Month == 'Sep', '09',
					ifelse(scan$Month == 'Oct', '10',
					ifelse(scan$Month == 'Nov', '11', '12')))))))))))

socialData$yearMonth	<- paste(socialData$Year, socialData$monthNum, sep = '-')
scan$yearMonth		<- paste(scan$Year, scan$monthNum, sep = '-')


#################################################################
### Trying to figure out who has which datasets on which days ###
#################################################################
socialDayObserverSum	<- aggregate(socialData$Date, by = list(observer = socialData$Observer, date = socialData$Date), FUN = length)
scanDayObserverSum	<- aggregate(scan$Date, by = list(observer = scan$Observer, date = scan$Date), FUN = length)
fileMakerDayObserverSum	<- aggregate(filemaker$Date, by = list(observer = filemaker$Observer, date = filemaker$Date), FUN = length)

allScanObserverSum	<- rbind.data.frame(scanDayObserverSum, fileMakerDayObserverSum)

andryScan			<- scan[scan$Observer == 'Andry',]
danielScan			<- scan[scan$Observer == 'Daniel',]
maxScan			<- scan[scan$Observer == 'Max',]
francisScan			<- scan[scan$Observer == 'Francis',]
patrickScan			<- scan[scan$Observer == 'Patrick',]

days		<- sort(unique(c(socialDayObserverSum$date, allScanObserverSum$date)))
observers	<- sort(unique(c(socialDayObserverSum$observer, allScanObserverSum$observer)))
daysToRemoveFromScan	<- data.frame(observer = character(), date = character())
daysToRemoveFromSocial	<- data.frame(observer = character(), date = character())
for(i in 1:length(observers)){
	obsNam	<- observers[i]
	print(paste('Checking', obsNam, "'s data for errors"))
	tempDaysToRemoveFromSocial	<- c()
	tempDaysToRemoveFromScan	<- c()
	for(j in days){ #0 means that data wasn't collected
		nScans	<- allScanObserverSum[allScanObserverSum$observer == obsNam & allScanObserverSum$date == j, 3]
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

