setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
data	<- read.csv('Instantaneous FileMaker data2.csv')

scannees	<- aggregate(data[,c('Date')], by = list(date = data$Date, ID = data$ScanneeID), FUN = length)
scannees	<- scannees[order(scannees$date),]

focals	<- aggregate(data[,c('Date')], by = list(date = data$Date, ID = data$Focal), FUN = length)
allObserved	<- rbind.data.frame(scannees, focals)

allTotals	<- aggregate(allObserved[,c('x')], by = list(date = allObserved$date, ID = allObserved$ID), FUN = sum)
allTotals	<- allTotals[order(allTotals$date),]
colnames(allTotals)	<- c('Date', 'ID', 'N_observed')
write.csv(allTotals, 'IDPerDayFileMakerInstantaneousData.csv')