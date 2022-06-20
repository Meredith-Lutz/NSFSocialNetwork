############################################
##### KMNP NSF Social Network Analyses #####
#####    Last updated by ML 10.11.21   #####
############################################
library(stringr)
library(igraph)
library(reshape2)
library(chron)

# Set working directory
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
source('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses/NSFSocialNetwork/ObservationTimeFunctions.R')

# Read in data
socialDataRaw		<- read.csv('All_nonSuppStudent_Social_Data_through_2019_2022_06_09_ML.csv', stringsAsFactors = FALSE)
matingSeasonStudent 	<- read.csv('studentMatingSeason_BL updates Jul232021_MLEdits.csv', stringsAsFactors = FALSE)
sleep				<- read.csv('All_Sleep_Tree_Data_Feb2020_corrected BL Sept2_2021_ML.csv', stringsAsFactors = FALSE)
census			<- read.csv('Census_File_Aug25_2020_chest status updated Dec10_2020.csv', stringsAsFactors = FALSE)
groups			<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021_NoBlanks.csv', stringsAsFactors = FALSE)
focalList			<- read.csv('focalListFinalForBLAnalysis2021-12-13.csv', stringsAsFactors = FALSE)
demo				<- read.csv('Copy of life.history.TMM with becca comments about conflicting info Feb10_2021_ML.csv', stringsAsFactors = FALSE)

demo$Name			<- str_to_title(demo$Name, locale = "en")
demo$Sex			<- ifelse(demo$Sex == '', 'unknown', as.character(demo$Sex))
sifakaNames			<- demo$Name

# Combine the mating season student data w/ other social data
matingSeasonStudent$StudentOb.YN	<- 'Y'
matingSeasonStudentSimp	<- matingSeasonStudent[,c('OriginalFile', 'Observer', 'Observation.ID', 'Date', 'Focal',
					'Start', 'Stop', 'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context',
					'Behavior', 'Species', 'Tree.number', 'Response', 'Response.to', 'Win', 'Comments',
					'Cleaning.changes', 'StudentOb.YN')]
socialDataRawSimp		<- socialDataRaw[,c('OriginalFile', 'Observer', 'Obs.ID', 'Date', 'Focal', 'Start', 'Stop',
					'Duration', 'Duration.Seconds', 'Initiator', 'Receiver', 'Context', 'Behavior', 'Species',
					'Tree.number', 'Response', 'To', 'Win', 'Comments', 'Cleaning.Comments', 'StudentOb.YN')]
colnames(socialDataRawSimp)	<- colnames(matingSeasonStudentSimp)
socialDataAllRaw			<- socialDataRawSimp #rbind(socialDataRawSimp, matingSeasonStudentSimp)

# Change baby names to match LH file
socialDataAllRaw$Initiator	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Initiator	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Initiator)
socialDataAllRaw$Receiver	<- gsub('Vanilla_baby_2011', 'Vanillababy2011', socialDataAllRaw$Receiver)
socialDataAllRaw$Receiver	<- gsub('Savannah_baby_2011', 'Savannahbaby2011', socialDataAllRaw$Receiver)

# Remove lines that have non-identified individuals, n=643 removed, n=125658 left
socialData		<- socialDataAllRaw[socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames,]
socialDataRemoved	<- socialDataAllRaw[!(socialDataAllRaw$Initiator %in% sifakaNames & socialDataAllRaw$Receiver %in% sifakaNames),]

# Focals by date
focalByDate		<- table(socialData$Observation.ID, socialData$Date)
focalByDate1s	<- focalByDate * (1/focalByDate)
nDatesPerFocal	<- apply(focalByDate1s, 1, sum, na.rm = TRUE)
length(nDatesPerFocal[nDatesPerFocal > 1]) #1 with multiple dates for a focal

############################################
### Create focal list for filemaker data ###
############################################
filemakerFocalList	<- data.frame()
#groupsToAddToFilemaker  <- groups[,1:3]
#filemakerWithRealGroup	<- merge(filemaker,groupsToAddToFilemaker,by.x = c('Focal','Date'),by.y = c('animal','date'))
#colnames(filemakerWithRealGroup)[23]	<- 'realGroup'

for(i in unique(filemaker$ObsID)){
	subset	<- filemaker[filemaker$ObsID == i & is.na(filemaker$ObsID)==FALSE,]
	observer	<- unique(subset$Observer)
      group       <- unique(subset$realGroup)
      date        <- unique(subset$Date)
      focal	      <- unique(subset$Focal)
      nScan		<- length(unique(subset$ScanTime))
      lastScan	<- max(unique(format(as.POSIXlt(subset$ScanTime, format = "%H:%M:%S"), "%H:%M:%S")))
      firstScan	<- min(unique(format(as.POSIXlt(subset$ScanTime, format = "%H:%M:%S"), "%H:%M:%S")))
	startTime	<- format(as.POSIXlt(firstScan, format = "%H:%M:%S")- 10*60,"%H:%M:%S")
	newline	<- cbind.data.frame(i,observer,date,group,focal,startTime,lastScan,nScan)
	filemakerFocalList	<- rbind.data.frame(filemakerFocalList,newline)
      
}

colnames(filemakerFocalList)	<- colnames(nnFocalList)

#write.csv(filemakerWithRealGroup[,c(1:8, 23, 10:22)], 'FileMaker_ML_11Oct2021.csv', row.names = FALSE)
#write.csv(filemakerFocalList, 'FileMakerIDs_ML_06Dec2021.csv', row.names = FALSE)

######################################################
### Combine Focal Lists and Create Observation MAT ###
######################################################
fullFocalList	<- fullFocalList[order(fullFocalList$date, fullFocalList$start_time),]
months		<- data.frame(str_split_fixed(as.character(fullFocalList$yearMonth), '-', n = 2))
fullFocalList$month	<- as.numeric(months[,2])
fullFocalList$season	<- ifelse(fullFocalList$month ==  1 | fullFocalList$month == 2 | fullFocalList$month == 3 , 'mating',
				ifelse(fullFocalList$month == 7 | fullFocalList$month == 8 | fullFocalList$month == 9, 'birthing', 'other'))

focalListMating		<- fullFocalList[fullFocalList$season == 'mating',]
focalListNonMating	<- fullFocalList[fullFocalList$season == 'birthing' | fullFocalList$season == 'other',]
focalListBirthing		<- fullFocalList[fullFocalList$season == 'birthing',]

obsMat	<- calculateObservationMatrix(focalListBirthing, groups, '2008-01-01', '2020-12-31', sifakaNames)

calculateObservationTimes(fullFocalList, '2008-01-01', '2020-12-31', sifakaNames)

####################################
### Seperate behavioral datasets ###
####################################
socialData$behavCat	<- ifelse(socialData$Behavior == 'Approach_contact' | socialData$Behavior == 'Contact_out_of_sight', 'prx',
					ifelse(socialData$Behavior == 'Approach_1m' | socialData$Behavior == 'Within_1m' | 
						socialData$Behavior == 'Withdraw_within_1m' | socialData$Behavior == 'Withdraw_within_1m_out_of_sight', 'prx',
					ifelse(socialData$Behavior == 'Groom' | socialData$Behavior == 'Mutual_groom' | socialData$Behavior == 'Mutual Groom [unknown initiator]', 'grm',
					ifelse(socialData$Behavior == 'Invite_to_groom', 'itg',
					ifelse(socialData$Behavior == 'Play', 'ply',
					ifelse(socialData$Behavior == 'Chatter', 'chat',
					ifelse(socialData$Behavior == 'Supplant', 'supp',
					ifelse(socialData$Behavior == 'Tail_curl', 'tc',
					ifelse(socialData$Behavior == 'Flee_less_than_2m' | socialData$Behavior == 'Flee_greater_than_2m', 'flee',
					ifelse(socialData$Behavior == 'Withdraw_greater_than_1m', 'with',
					ifelse(socialData$Behavior == 'Lunge' | socialData$Behavior == 'Bite' | 
						socialData$Behavior == 'Chase' | socialData$Behavior == 'Cuff' |
						socialData$Behavior == 'Nose_jab' | socialData$Behavior == 'Snap_at' |
						socialData$Behavior == 'Feign_to_cuff', 'agg', NA)))))))))))

dates	<- data.frame(str_split_fixed(as.character(socialData$Date), '/', n = 3))
socialData$month	<- as.numeric(dates[,1])
socialData$season	<- ifelse(socialData$month ==  1 | socialData$month == 2 | socialData$month == 3 , 'mating',
				ifelse(socialData$month == 7 | socialData$month == 8 | socialData$month == 9, 'birthing', 'other'))

socialDataMating		<- socialData[socialData$season == 'mating',]
socialDataNonMating	<- socialData[socialData$season == 'birthing' | socialData$season == 'other',]
socialDataBirthing	<- socialData[socialData$season == 'birthing',]

#Change this line to run on diff dataset
socialDataFinal	<- socialDataBirthing

#Create mutual grooming that is in both directions
mgrm			<- socialDataFinal[socialDataFinal$Behavior == 'Mutual_groom' | socialDataFinal$Behavior == 'Mutual Groom [unknown initiator]',]
mgrmExtraLines	<- mgrm[,c(1:9, 11, 10, 12:25)]
mgrmExtraLines$Behavior	<- 'Mutual_groom_added'
colnames(mgrmExtraLines)	<- colnames(mgrm)
allMgrm		<- rbind(mgrm, mgrmExtraLines)

cntND			<- socialDataFinal[socialDataFinal$Behavior == 'Approach_contact' | socialDataFinal$Behavior == 'Contact_out_of_sight',]
cntD			<- socialDataFinal[socialDataFinal$Behavior == 'Approach_contact',]
prxND			<- socialDataFinal[socialDataFinal$Behavior == 'Approach_1m' | socialDataFinal$Behavior == 'Within_1m' | 
				socialDataFinal$Behavior == 'Withdraw_within_1m' | socialDataFinal$Behavior == 'Withdraw_within_1m_out_of_sight',]
prxD			<- socialDataFinal[socialDataFinal$Behavior == 'Approach_1m',]
grmD			<- socialDataFinal[socialDataFinal$Behavior == 'Groom',]
playND		<- socialDataFinal[socialDataFinal$Behavior == 'Play' | socialDataFinal$Behavior == 'Play_out_of_sight',]
chatterD		<- socialDataFinal[socialDataFinal$Behavior == 'Chatter',]
grmND			<- rbind(grmD, allMgrm, socialDataFinal[socialDataFinal$Behavior == 'Groom_out_of_sight',])
itgD			<- socialDataFinal[socialDataFinal$Behavior == 'Invite_to_groom',]
supplant		<- socialDataFinal[socialDataFinal$Behavior == 'Supplant',]
aggNoSupplant	<- socialDataFinal[socialDataFinal$Behavior == 'Lunge' | socialDataFinal$Behavior == 'Cuff' |
				socialDataFinal$Behavior == 'Bite' | socialDataFinal$Behavior == 'Chase' | socialDataFinal$Behavior == 'Nose_jab' |
				socialDataFinal$Behavior == 'Feign_to_cuff' | socialDataFinal$Behavior == 'Snap_at',]
appD			<- socialDataFinal[socialDataFinal$Behavior == 'Approach_contact' | socialDataFinal$Behavior == 'Approach_1m',]
withD			<- socialDataFinal[socialDataFinal$Behavior == 'Withdraw_greater_than_1m',]
with1mD		<- socialDataFinal[socialDataFinal$Behavior == 'Withdraw_within_1m',]
tcD			<- socialDataFinal[socialDataFinal$Behavior == 'Tail_curl',]
fleeD			<- socialDataFinal[socialDataFinal$Behavior == 'Flee_less_than_2m' | socialDataFinal$Behavior == 'Flee_greater_than_2m',]
allPrxND		<- rbind(cntND, prxND)

####################################################
### Create network matrices from continuous data ###
####################################################

# This code takes a set of animals, a set of interactions, and a set of durations and can calculate the amount of
# time (or # of times) that a dyad interacts. The multiple behavior option averages across behaviors, which is
# not the goal, here, hence we will create a dummy behavioral category variable that lumps all of the relevant behaviors for a net
# There is more documentation for the function itself in the definition file
source('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses/createNetworkFunction.R')
#source('C:/Users/cecil/OneDrive/Desktop/SDC Work/Github Work/SeasonalNetworkAnalyses/createNetworkFunction.R')

appDMat	<- createNet(appD$Initiator, appD$Receiver, appD$behavCat, 'prx',
			subjects = sifakaNames, directional = TRUE, type = 'count')
cntDMat	<- createNet(cntD$Initiator, cntD$Receiver, cntD$behavCat, 'prx',
			subjects = sifakaNames, directional = TRUE, type = 'count')
withDMat	<- createNet(withD$Initiator, withD$Receiver, withD$behavCat, 'with',
			subjects = sifakaNames, directional = TRUE, type = 'count')
with1mDMat	<- createNet(with1mD$Initiator, with1mD$Receiver, with1mD$behavCat, 'prx',
			subjects = sifakaNames, directional = TRUE, type = 'count')
allprxNDMat	<- createNet(allPrxND$Initiator, allPrxND$Receiver, allPrxND$behavCat, 'prx',
			subjects = sifakaNames, directional = FALSE, type = 'duration', durs = allPrxND$Duration.Seconds)
cntNDMat	<- createNet(cntND$Initiator, cntND$Receiver, cntND$behavCat, 'prx',
			subjects = sifakaNames, directional = FALSE, type = 'duration', durs = cntND$Duration.Seconds)

grmDMat	<- createNet(grmD$Initiator, grmD$Receiver, grmD$behavCat, 'grm',
			subjects = sifakaNames, directional = TRUE, type = 'duration', durs = grmD$Duration.Seconds)
grmDFreqMat	<- createNet(grmD$Initiator, grmD$Receiver, grmD$behavCat, 'grm',
			subjects = sifakaNames, directional = TRUE, type = 'count')
grmNDMat	<- createNet(grmND$Initiator, grmND$Receiver, grmND$behavCat, 'grm',
			subjects = sifakaNames, directional = FALSE, type = 'duration', durs = grmND$Duration.Seconds)
grmNDFreqMat	<- createNet(grmND$Initiator, grmND$Receiver, grmND$behavCat, 'grm',
			subjects = sifakaNames, directional = FALSE, type = 'count')
itgDMat	<- createNet(itgD$Initiator, itgD$Receiver, itgD$behavCat, 'itg',
			subjects = sifakaNames, directional = TRUE, type = 'count')
playNDMat	<- createNet(playND$Initiator, playND$Receiver, playND$behavCat, 'ply',
			subjects = sifakaNames, directional = FALSE, type = 'duration', durs = playND$Duration.Seconds)

chatDMat	<- createNet(chatterD$Initiator, chatterD$Receiver, chatterD$behavCat, 'chat',
			subjects = sifakaNames, directional = TRUE, type = 'count')
fleeDMat	<- createNet(fleeD$Initiator, fleeD$Receiver, fleeD$behavCat, 'flee',
			subjects = sifakaNames, directional = TRUE, type = 'count')
tcDMat	<- createNet(tcD$Initiator, tcD$Receiver, tcD$behavCat, 'tc',
			subjects = sifakaNames, directional = TRUE, type = 'duration', durs = tcD$Duration.Seconds)
tcDFreqMat	<- createNet(tcD$Initiator, tcD$Receiver, tcD$behavCat, 'tc',
			subjects = sifakaNames, directional = TRUE, type = 'count')
aggDMat	<- createNet(aggNoSupplant$Initiator, aggNoSupplant$Receiver, aggNoSupplant$behavCat, 'agg',
			subjects = sifakaNames, directional = TRUE, type = 'count')
suppDMat	<- createNet(supplant$Initiator, supplant$Receiver, supplant$behavCat, 'supp',
			subjects = sifakaNames, directional = TRUE, type = 'count')

matList	<- list(appDMat, cntDMat, withDMat, with1mDMat, allprxNDMat, cntNDMat,
				grmDMat, grmDFreqMat, grmNDMat, grmNDFreqMat, itgDMat, playNDMat,
				chatDMat, fleeDMat, tcDMat, tcDFreqMat, aggDMat, suppDMat)
matListAdj	<- list()
for(i in 1:length(matList)){
	matAdj	<- matList[[i]]/(obsMat/60)
	matAdj[is.nan(matAdj)]	<- 0
	matListAdj[[i]]	<- matAdj
}

appDNet		<- graph_from_adjacency_matrix(matListAdj[[1]], mode = 'directed', weighted = TRUE)
cntDNet		<- graph_from_adjacency_matrix(matListAdj[[2]], mode = 'directed', weighted = TRUE)
withDNet		<- graph_from_adjacency_matrix(matListAdj[[3]], mode = 'directed', weighted = TRUE)
with1mDNet		<- graph_from_adjacency_matrix(matListAdj[[4]], mode = 'directed', weighted = TRUE)
allprxNDNet		<- graph_from_adjacency_matrix(matListAdj[[5]], mode = 'undirected', weighted = TRUE)
cntNDNet		<- graph_from_adjacency_matrix(matListAdj[[6]], mode = 'undirected', weighted = TRUE)

grmDNet		<- graph_from_adjacency_matrix(matListAdj[[7]], mode = 'directed', weighted = TRUE)
grmDFreqNet		<- graph_from_adjacency_matrix(matListAdj[[8]], mode = 'directed', weighted = TRUE)
grmNDNet		<- graph_from_adjacency_matrix(matListAdj[[9]], mode = 'undirected', weighted = TRUE)
grmNDFreqNet	<- graph_from_adjacency_matrix(matListAdj[[10]], mode = 'undirected', weighted = TRUE)
itgDNet		<- graph_from_adjacency_matrix(matListAdj[[11]], mode = 'directed', weighted = TRUE)
playNDNet		<- graph_from_adjacency_matrix(matListAdj[[12]], mode = 'undirected', weighted = TRUE)

chatDNet		<- graph_from_adjacency_matrix(matListAdj[[13]], mode = 'directed', weighted = TRUE)
fleeDNet		<- graph_from_adjacency_matrix(matListAdj[[14]], mode = 'directed', weighted = TRUE)
tcDNet		<- graph_from_adjacency_matrix(matListAdj[[15]], mode = 'directed', weighted = TRUE)
tcDFreqNet		<- graph_from_adjacency_matrix(matListAdj[[16]], mode = 'directed', weighted = TRUE)
aggDNet		<- graph_from_adjacency_matrix(matListAdj[[17]], mode = 'directed', weighted = TRUE)
suppDNet		<- graph_from_adjacency_matrix(matListAdj[[18]], mode = 'directed', weighted = TRUE)


write.csv(matListAdj[[1]], 'allApproachFreqBirthing.csv')
write.csv(matListAdj[[2]], 'approachContactFreqBirthing.csv')
write.csv(matListAdj[[3]], 'withdrawG1FreqBirthing.csv')
write.csv(matListAdj[[4]], 'withdrawWithin1mFreqBirthing.csv')
write.csv(matListAdj[[5]], 'timeSpentIn1mDurBirthing.csv')
write.csv(matListAdj[[6]], 'timeSpentInContactDurBirthing.csv')
write.csv(matListAdj[[7]], 'groomDirectionalDurBirthing.csv')
write.csv(matListAdj[[8]], 'groomDirectionalFreqBirthing.csv')
write.csv(matListAdj[[9]], 'groomNonDirectionalDurBirthing.csv')
write.csv(matListAdj[[10]], 'groomNonDirectionalFreqBirthing.csv')
write.csv(matListAdj[[11]], 'inviteToGroomFreqBirthing.csv')
write.csv(matListAdj[[12]], 'playDurBirthing.csv')
write.csv(matListAdj[[13]], 'chatterFreqBirthing.csv')
write.csv(matListAdj[[14]], 'fleeFreqBirthing.csv')
write.csv(matListAdj[[15]], 'tailCurlDurBirthing.csv')
write.csv(matListAdj[[16]], 'tailCurlFreqBirthing.csv')
write.csv(matListAdj[[17]], 'aggressionFreqBirthing.csv')
write.csv(matListAdj[[18]], 'supplantFreqBirthing.csv')


#########################################
##### Sleeping network calculations #####
#########################################

#Modified based on Damien's code + my original edits

sleepBallNet <- function(data, begin = "2011-01-01", end = "2019-12-31", dawn_dusk = "dawn_dusk", date_format = "%Y-%m-%d"){
		dat	<- data
	
		#Subset for given dates
		begin2 	<- as.Date(begin, "%Y-%m-%d")
		end2 		<- as.Date(end, "%Y-%m-%d")
		subdat 	<- dat[as.Date(dat$Date, date_format)>= begin2 & as.Date(dat$Date, date_format) <= end2,]
		
		#Mark entries as dawn or dusk
		hours 	<- as.numeric(unlist(lapply(strsplit(as.character(subdat$Time), ":"), function(v) v[1])))
		dawn 		<- factor(ifelse(hours < 12, "dawn", "dusk"))
		subdat	<- cbind(subdat, DawnOrDusk = dawn)
		subdat2 	<- subdat
		if(dawn_dusk == "dawn"){
			subdat2 <- subdat[subdat$DawnOrDusk == "dawn",]
		}
		if(dawn_dusk == "dusk"){
			subdat2 <- subdat[subdat$DawnOrDusk == "dusk",]
		}

		#Create sleep scan ID's
		scan_id 	<- factor(factor(subdat2$Date):subdat2$DawnOrDusk:factor(subdat2$Focal.Group))
		subdat2$ID 	<- factor(subdat2$ID)
		indivIDLevel<- levels(subdat2$ID)
		
		#Create matrix to fill in
		ans 			<- as.matrix(table(indivIDLevel) %*% t(table(indivIDLevel)))
		dimnames(ans) 	<- list(indivIDLevel, indivIDLevel)
		ans 			<- ans*0
		ans.adj 		<- ans.nb.contacts <- ans.denominator <- ans
		
		#i	<-levels(scan_id)[518]
		
		#Fill in the matrix
		for (i in levels(scan_id)){
			temp <- subdat2[scan_id == i,]
		
			##replace the 0s with values higher than max(temp$Ball)
			##Because Ball is numeric, R will add NA's if there is none written. 
			##These need to be converted to 0's to be consistent with rest of data
			if(sum(is.na(temp$Ball)) > 0){
				temp[is.na(temp$Ball) == TRUE,]$Ball	<- 0
				n0 <- sum(temp$Ball==0) #Calculate # of 0's
				temp$Ball[temp$Ball == 0] <- (max(temp$Ball)+1):(n0 + max(temp$Ball)) ##done
			}
			
			temp$BallTree	<- factor(factor(temp$Ball):factor(temp$Tree.Number))

			#Table 1 tells you who is in what ball
			table1 <- as.matrix(table(factor(temp$BallTree), temp$ID)) ##table 1 tells you who is in each ball
			seen <- seen1 <- apply(table1, 2, sum)

			##Error message if there's a duplicate for a particular animal
			if (sum(apply(table1, 2, sum)>1)>0) {
				warning(paste("Unexpected duplicate in ", i, ". Please fix in Excel and save a new CSV file."))
			} 
 			
			#Calculate animals who were seen coresident in a ball
			adjacency.matrix <- t(table1)%*%table1 ##neat trick : by multiplying table1 with the transposed table1, we get the adjacency matrix directly... 
			ans.nb.contacts <- ans.nb.contacts + adjacency.matrix
			
			#Create a matrix with the animals observed as 1's in the rows, 0's else
			for (j in 2:length(seen1)){ 
				seen <- cbind(seen, seen1)
			}
			
			#Seen %*% t(seen) is a matrix that is nonzero if pair was seen, 0 otherwise
			ans.denominator <- ans.denominator + 1*((seen%*%t(seen))>0)

		} #I also to add this stop brace
	
		ans.adj <- ans.nb.contacts/ans.denominator

	return(list(adjacency.matrix = ans.adj, nb.contacts = ans.nb.contacts, nb.scans.together = ans.denominator))
}

sleepTreeNet <- function(data, begin = "2011-01-01", end = "2019-12-31", dawn_dusk = "dawn_dusk", date_format = "%Y-%m-%d", animals){
		dat	<- data
	
		#Subset for given dates
		begin2 	<- as.Date(begin, "%Y-%m-%d")
		end2 		<- as.Date(end, "%Y-%m-%d")
		subdat 	<- dat[as.Date(dat$Date, date_format)>= begin2 & as.Date(dat$Date, date_format) <= end2,]
		
		#Mark entries as dawn or dusk
		hours 	<- as.numeric(unlist(lapply(strsplit(as.character(subdat$Time), ":"), function(v) v[1])))
		dawn 		<- factor(ifelse(hours < 12, "dawn", "dusk"))
		subdat	<- cbind(subdat, DawnOrDusk = dawn)
		subdat2 	<- subdat
		if(dawn_dusk == "dawn"){
			subdat2 <- subdat[subdat$DawnOrDusk == "dawn",]
		}
		if(dawn_dusk == "dusk"){
			subdat2 <- subdat[subdat$DawnOrDusk == "dusk",]
		}

		#Create sleep scan ID's
		scan_id 	<- factor(factor(subdat2$Date):subdat2$DawnOrDusk:factor(subdat2$Focal.Group))
		subdat2$ID 	<- factor(subdat2$ID)
		indivIDLevel<- levels(subdat2$ID)
		
		#Create matrix to fill in
		ans 			<- as.matrix(table(indivIDLevel) %*% t(table(indivIDLevel)))
		dimnames(ans) 	<- list(indivIDLevel, indivIDLevel)
		ans 			<- ans*0
		ans.adj 		<- ans.nb.contacts <- ans.denominator <- ans
		
		i	<-levels(scan_id)[517]
		
		#Fill in the matrix
		for (i in levels(scan_id)){
			temp <- subdat2[scan_id == i,]
		
			##replace the 0s with values higher than max(temp$Ball)
			##Because Ball is numeric, R will add NA's if there is none written. 
			##These need to be converted to 0's to be consistent with rest of data
			if(sum(is.na(temp$Ball)) > 0){
				temp[is.na(temp$Ball) == TRUE,]$Ball	<- 0
				n0 <- sum(temp$Ball==0) #Calculate # of 0's
				temp$Ball[temp$Ball == 0] <- (max(temp$Ball)+1):(n0 + max(temp$Ball)) ##done
			}
			
			#Table 1 tells you who is in what ball
			table1 <- as.matrix(table(factor(temp$Tree.Number), temp$ID)) ##table 1 tells you who is in each tree
			seen <- seen1 <- apply(table1, 2, sum)

			##Error message if there's a duplicate for a particular animal
			if (sum(apply(table1, 2, sum)>1)>0) {
				warning(paste("Unexpected duplicate in ", i, ". Please fix in Excel and save a new CSV file."))
			} 
 			
			#Calculate animals who were seen coresident in a ball
			adjacency.matrix <- t(table1)%*%table1 ##neat trick : by multiplying table1 with the transposed table1, we get the adjacency matrix directly... 
			ans.nb.contacts <- ans.nb.contacts + adjacency.matrix
			
			#Create a matrix with the animals observed as 1's in the rows, 0's else
			for (j in 2:length(seen1)){ 
				seen <- cbind(seen, seen1)
			}
			
			#Seen %*% t(seen) is a matrix that is nonzero if pair was seen, 0 otherwise
			ans.denominator <- ans.denominator + 1*((seen%*%t(seen))>0)

		} #I also to add this stop brace
	
		ans.adj <- ans.nb.contacts/ans.denominator

	return(list(adjacency.matrix = ans.adj, nb.contacts = ans.nb.contacts, nb.scans.together = ans.denominator))
}

sleep			<- merge(sleep, groups, by.x = c('Date', 'ID'), by.y = c('date', 'animal'), all.x = TRUE)
sleep$season	<- ifelse(sleep$Month == 'Jan' | sleep$Month == 'Feb' | sleep$Month == 'Mar' , 'mating',
				ifelse(sleep$Month == 'Jul' | sleep$Month == 'Aug' | sleep$Month == 'Sep', 'birthing', 'other'))  
#Clean up names
sleep$ID		<- gsub('Vanilla baby 2011', 'Vanillababy2011', sleep$ID)
sleep$ID		<- gsub('Savannah baby 2011', 'Savannahbaby2011', sleep$ID)
sleep$ID		<- gsub('Hester baby 2012', 'Hesterbaby2012', sleep$ID)
sleep$ID		<- gsub('Rossy', 'Rossi', sleep$ID)
sleep			<- sleep[sleep$ID %in% sifakaNames,]

#Remove a few problem lines that are duplicated across observer
sleep		<- sleep[sleep$Individual.Repeat != 'Y',]
sleep		<- sleep[sleep$Observation.Duplicated == '' | (sleep$Observation.Duplicated == 'Y' & sleep$Observer == sleep$Observer.Kept),]

#Remove a few problematic lines from the merge
sleep		<- sleep[!(sleep$Date == '2019-09-23' & sleep$ID == 'Zavona' & sleep$group == 'III'),]
sleep		<- sleep[!(sleep$Date == '2019-08-11' & sleep$ID == 'Spirit' & sleep$group == 'II'),]
sleep		<- sleep[!(sleep$Date == '2019-02-04' & sleep$ID == 'Spirit' & sleep$group == 'XI'),]

###############################
### Seasonal sleep analyses ###
###############################
#Seperate out datasets
sleepMatingSeason		<- sleep[sleep$season == 'mating',]
sleepNonMatingSeason	<- sleep[sleep$season == 'birthing' | sleep$season == 'other',]
sleepBirthingSeason	<- sleep[sleep$season == 'birthing',]

stillSleep				<- sleep[sleep$Found.Sleeping. == 1 & is.na(sleep$Found.Sleeping.) == FALSE,]
stillSleepMatingSeason		<- sleepMatingSeason[sleepMatingSeason$Found.Sleeping. == 1 & is.na(sleepMatingSeason$Found.Sleeping.) == FALSE,]
stillSleepNonMatingSeason	<- sleepNonMatingSeason[sleepNonMatingSeason$Found.Sleeping. == 1 & is.na(sleepNonMatingSeason$Found.Sleeping.) == FALSE,]
stillSleepBirthingSeason	<- sleepBirthingSeason[sleepBirthingSeason$Found.Sleeping. == 1 & is.na(sleepBirthingSeason$Found.Sleeping.) == FALSE,]

ballMatAll		<- sleepBallNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
treeMatAll		<- sleepTreeNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
ballMatAM		<- sleepBallNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAM		<- sleepTreeNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
ballMatAMSleep	<- sleepBallNet(data = stillSleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMSleep	<- sleepTreeNet(data = stillSleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")

ballMatAllMating		<- sleepBallNet(data = sleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
treeMatAllMating		<- sleepTreeNet(data = sleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
ballMatAMMating		<- sleepBallNet(data = sleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMMating		<- sleepTreeNet(data = sleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
ballMatAMSleepMating	<- sleepBallNet(data = stillSleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMSleepMating	<- sleepTreeNet(data = stillSleepMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")

ballMatAllNonMating	<- sleepBallNet(data = sleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
treeMatAllNonMating	<- sleepTreeNet(data = sleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
ballMatAMNonMating	<- sleepBallNet(data = sleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMNonMating	<- sleepTreeNet(data = sleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
ballMatAMSleepNonMating	<- sleepBallNet(data = stillSleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMSleepNonMating	<- sleepTreeNet(data = stillSleepNonMatingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")

ballMatAllBirthing	<- sleepBallNet(data = sleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
treeMatAllBirthing	<- sleepTreeNet(data = sleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
ballMatAMBirthing		<- sleepBallNet(data = sleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMBirthing		<- sleepTreeNet(data = sleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
ballMatAMSleepBirthing	<- sleepBallNet(data = stillSleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
treeMatAMSleepBirthing	<- sleepTreeNet(data = stillSleepBirthingSeason, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")

write.csv(ballMatAll$adjacency.matrix, 'sleepBallAllTime.csv')
write.csv(treeMatAll$adjacency.matrix, 'sleepTreeAllTime.csv')
write.csv(ballMatAM$adjacency.matrix, 'sleepBallAMOnlyAllTime.csv')
write.csv(treeMatAM$adjacency.matrix, 'sleepTreeAMOnlyAllTime.csv')
write.csv(ballMatAMSleep$adjacency.matrix, 'sleepBallAMOnlyStillSleepAllTime.csv')
write.csv(treeMatAMSleep$adjacency.matrix, 'sleepTreeAMOnlyStillSleepAllTime.csv')

write.csv(ballMatAllMating$adjacency.matrix, 'sleepBallMating.csv')
write.csv(treeMatAllMating$adjacency.matrix, 'sleepTreeMating.csv')
write.csv(ballMatAMMating$adjacency.matrix, 'sleepBallAMOnlyMating.csv')
write.csv(treeMatAMMating$adjacency.matrix, 'sleepTreeAMOnlyMating.csv')
write.csv(ballMatAMSleepMating$adjacency.matrix, 'sleepBallAMOnlyStillSleepMating.csv')
write.csv(treeMatAMSleepMating$adjacency.matrix, 'sleepTreeAMOnlyStillSleepMating.csv')

write.csv(ballMatAllNonMating$adjacency.matrix, 'sleepBallNonMating.csv')
write.csv(treeMatAllNonMating$adjacency.matrix, 'sleepTreeNonMating.csv')
write.csv(ballMatAMNonMating$adjacency.matrix, 'sleepBallAMOnlyNonMating.csv')
write.csv(treeMatAMNonMating$adjacency.matrix, 'sleepTreeAMOnlyNonMating.csv')
write.csv(ballMatAMSleepNonMating$adjacency.matrix, 'sleepBallAMOnlyStillSleepNonMating.csv')
write.csv(treeMatAMSleepNonMating$adjacency.matrix, 'sleepTreeAMOnlyStillSleepNonMating.csv')

write.csv(ballMatAllBirthing$adjacency.matrix, 'sleepBallBirthing.csv')
write.csv(treeMatAllBirthing$adjacency.matrix, 'sleepTreeBirthing.csv')
write.csv(ballMatAMBirthing$adjacency.matrix, 'sleepBallAMOnlyBirthing.csv')
write.csv(treeMatAMBirthing$adjacency.matrix, 'sleepTreeAMOnlyBirthing.csv')
write.csv(ballMatAMSleepBirthing$adjacency.matrix, 'sleepBallAMOnlyStillSleepBirthing.csv')
write.csv(treeMatAMSleepBirthing$adjacency.matrix, 'sleepTreeAMOnlyStillSleepBirthing.csv')

######################
### Get edge lists ###
######################
ballNetAll		<- graph_from_adjacency_matrix(ballMatAll$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAll		<- graph_from_adjacency_matrix(treeMatAll$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAM		<- graph_from_adjacency_matrix(ballMatAM$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAM		<- graph_from_adjacency_matrix(treeMatAM$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMSleep	<- graph_from_adjacency_matrix(ballMatAMSleep$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMSleep	<- graph_from_adjacency_matrix(treeMatAMSleep$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')

ballNetAllMating		<- graph_from_adjacency_matrix(ballMatAllMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAllMating		<- graph_from_adjacency_matrix(treeMatAllMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMMating		<- graph_from_adjacency_matrix(ballMatAMMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMMating		<- graph_from_adjacency_matrix(treeMatAMMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMSleepMating	<- graph_from_adjacency_matrix(ballMatAMSleepMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMSleepMating	<- graph_from_adjacency_matrix(treeMatAMSleepMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')

ballNetAllNonMating	<- graph_from_adjacency_matrix(ballMatAllNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAllNonMating	<- graph_from_adjacency_matrix(treeMatAllNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMNonMating	<- graph_from_adjacency_matrix(ballMatAMNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMNonMating	<- graph_from_adjacency_matrix(treeMatAMNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMSleepNonMating	<- graph_from_adjacency_matrix(ballMatAMSleepNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMSleepNonMating	<- graph_from_adjacency_matrix(treeMatAMSleepNonMating$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')

ballNetAllBirthing	<- graph_from_adjacency_matrix(ballMatAllBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAllBirthing	<- graph_from_adjacency_matrix(treeMatAllBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMBirthing		<- graph_from_adjacency_matrix(ballMatAMBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMBirthing		<- graph_from_adjacency_matrix(treeMatAMBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
ballNetAMSleepBirthing	<- graph_from_adjacency_matrix(ballMatAMSleepBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
treeNetAMSleepBirthing	<- graph_from_adjacency_matrix(treeMatAMSleepBirthing$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')

allNets	<- list(appDNet, cntDNet, withDNet, with1mDNet, allprxNDNet, cntNDNet,
				grmDNet, grmDFreqNet, grmNDNet, grmNDFreqNet, itgDNet, playNDNet,
				chatDNet, fleeDNet, tcDNet, tcDFreqNet, aggDNet, suppDNet,
				ballNetAllBirthing, treeNetAllBirthing, ballNetAMBirthing, treeNetAMBirthing, ballNetAMSleepBirthing, treeNetAMSleepBirthing)
edgeListAllNets	<- lapply(allNets, FUN = get.data.frame)

edgeListAllNetsDF	<- merge(edgeListAllNets[[1]], edgeListAllNets[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[7]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[8]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[9]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[10]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[11]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[12]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[13]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[14]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[15]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[16]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[17]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[18]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[19]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[20]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[21]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[22]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[23]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListAllNetsDF	<- merge(edgeListAllNetsDF, edgeListAllNets[[24]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
colnames(edgeListAllNetsDF)	<- c('from', 'to', 'allApproachFreq', 'approachContactFreq', 'withdrawG1Freq',
						'withdrawWithin1mFreq', 'timeSpentIn1mDur', 'timeSpentInContactDur',
						'groomDirectionalDur', 'groomDirectionalFreq', 'groomNonDirectionalDur',
						'groomNonDirectionalFreq', 'inviteToGroomFreq', 'playDur', 'chatterFreq',
						'fleeFreq', 'tailCurlDur', 'tailCurlFreq', 'aggressionFreq', 'supplantFreq',
						'sleepBall', 'sleepTree', 'sleepBallAM', 'sleepTreeAM', 'sleepBallAMStillSleeping',
						'sleepTreeAMStillSleeping')

write.csv(edgeListAllNetsDF, 'edgeListAllNetsBirthing.csv', row.names = FALSE)

########################################
### Initial plots for sleep networks ###
########################################

## All Time

allSleepNets	<- list(ballNetAll, treeNetAll, ballNetAM, treeNetAM, ballNetAMSleep, treeNetAMSleep)
edgeListSleepList	<- lapply(allSleepNets, FUN = get.data.frame)

edgeListSleepDF	<- merge(edgeListSleepList[[1]], edgeListSleepList[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDF	<- merge(edgeListSleepDF, edgeListSleepList[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDF	<- merge(edgeListSleepDF, edgeListSleepList[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDF	<- merge(edgeListSleepDF, edgeListSleepList[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDF	<- merge(edgeListSleepDF, edgeListSleepList[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))

edgeListSleepDF		<- merge(edgeListSleepDF, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'from', by.y = 'Name')
edgeListSleepDF		<- merge(edgeListSleepDF, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'to', by.y = 'Name')
colnames(edgeListSleepDF)	<- c('receiver', 'initiator', 'ballAll', 'treeAll', 'ballAM', 'treeAM', 'ballAMSleep', 'treeAMSleep', 'initiatorSex', 'receiverSex')
edgeListSleepDF$sexCombination	<- ifelse(edgeListSleepDF$initiatorSex == 'male' & edgeListSleepDF$receiverSex == 'male', 'M/M',
							ifelse(edgeListSleepDF$initiatorSex == 'female' & edgeListSleepDF$receiverSex == 'female', 'F/F',
							ifelse(edgeListSleepDF$initiatorSex == 'male' & edgeListSleepDF$receiverSex == 'female', 'M/F',
							ifelse(edgeListSleepDF$initiatorSex == 'female' & edgeListSleepDF$receiverSex == 'male', 'M/F',
							ifelse(edgeListSleepDF$initiatorSex == 'female' & edgeListSleepDF$receiverSex == 'unknown', 'Unk/F',
							ifelse(edgeListSleepDF$initiatorSex == 'unknown' & edgeListSleepDF$receiverSex == 'female', 'Unk/F',
							ifelse(edgeListSleepDF$initiatorSex == 'male' & edgeListSleepDF$receiverSex == 'unknown', 'Unk/M',
							ifelse(edgeListSleepDF$initiatorSex == 'unknown' & edgeListSleepDF$receiverSex == 'male', 'Unk/M', ''))))))))

edgeListSleepDF	<- edgeListSleepDF[,c(2, 1, 3:11)]
edgeListSleepDF	<- edgeListSleepDF[order(edgeListSleepDF$initiator, edgeListSleepDF$receiver),]
write.csv(edgeListSleepDF, 'edgeListSleepAllTime.csv', row.names = FALSE)

png('allTimeSleepBoxPlots.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow = c(2, 3))
boxplot(ballAll ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Ball All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAM ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Ball AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAMSleep ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Ball AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAll ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Tree All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAM ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Tree AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAMSleep ~ sexCombination, data = edgeListSleepDF, main = 'Sleep Tree AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
dev.off()

## Mating season

allSleepNetsMating	<- list(ballNetAllMating, treeNetAllMating, ballNetAMMating, treeNetAMMating, ballNetAMSleepMating, treeNetAMSleepMating)
edgeListSleepListMating	<- lapply(allSleepNetsMating, FUN = get.data.frame)

edgeListSleepDFMating	<- merge(edgeListSleepListMating[[1]], edgeListSleepListMating[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFMating	<- merge(edgeListSleepDFMating, edgeListSleepListMating[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFMating	<- merge(edgeListSleepDFMating, edgeListSleepListMating[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFMating	<- merge(edgeListSleepDFMating, edgeListSleepListMating[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFMating	<- merge(edgeListSleepDFMating, edgeListSleepListMating[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))

edgeListSleepDFMating		<- merge(edgeListSleepDFMating, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'from', by.y = 'Name')
edgeListSleepDFMating		<- merge(edgeListSleepDFMating, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'to', by.y = 'Name')
colnames(edgeListSleepDFMating)	<- c('receiver', 'initiator', 'ballAll', 'treeAll', 'ballAM', 'treeAM', 'ballAMSleep', 'treeAMSleep', 'initiatorSex', 'receiverSex')
edgeListSleepDFMating$sexCombination	<- ifelse(edgeListSleepDFMating$initiatorSex == 'male' & edgeListSleepDFMating$receiverSex == 'male', 'M/M',
							ifelse(edgeListSleepDFMating$initiatorSex == 'female' & edgeListSleepDFMating$receiverSex == 'female', 'F/F',
							ifelse(edgeListSleepDFMating$initiatorSex == 'male' & edgeListSleepDFMating$receiverSex == 'female', 'M/F',
							ifelse(edgeListSleepDFMating$initiatorSex == 'female' & edgeListSleepDFMating$receiverSex == 'male', 'M/F',
							ifelse(edgeListSleepDFMating$initiatorSex == 'female' & edgeListSleepDFMating$receiverSex == 'unknown', 'Unk/F',
							ifelse(edgeListSleepDFMating$initiatorSex == 'unknown' & edgeListSleepDFMating$receiverSex == 'female', 'Unk/F',
							ifelse(edgeListSleepDFMating$initiatorSex == 'male' & edgeListSleepDFMating$receiverSex == 'unknown', 'Unk/M',
							ifelse(edgeListSleepDFMating$initiatorSex == 'unknown' & edgeListSleepDFMating$receiverSex == 'male', 'Unk/M', ''))))))))

edgeListSleepDFMating	<- edgeListSleepDFMating[,c(2, 1, 3:11)]
edgeListSleepDFMating	<- edgeListSleepDFMating[order(edgeListSleepDFMating$initiator, edgeListSleepDFMating$receiver),]
write.csv(edgeListSleepDFMating, 'edgeListSleepMating.csv', row.names = FALSE)

png('matingSeasonSleepBoxPlots.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow = c(2, 3))
boxplot(ballAll ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Ball All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAM ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Ball AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAMSleep ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Ball AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAll ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Tree All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAM ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Tree AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAMSleep ~ sexCombination, data = edgeListSleepDFMating, main = 'Sleep Tree AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
dev.off()

## Non-Mating season

allSleepNetsNonMating	<- list(ballNetAllNonMating, treeNetAllNonMating, ballNetAMNonMating, treeNetAMNonMating, ballNetAMSleepNonMating, treeNetAMSleepNonMating)
edgeListSleepListNonMating	<- lapply(allSleepNetsNonMating, FUN = get.data.frame)

edgeListSleepDFNonMating	<- merge(edgeListSleepListNonMating[[1]], edgeListSleepListNonMating[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFNonMating	<- merge(edgeListSleepDFNonMating, edgeListSleepListNonMating[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFNonMating	<- merge(edgeListSleepDFNonMating, edgeListSleepListNonMating[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFNonMating	<- merge(edgeListSleepDFNonMating, edgeListSleepListNonMating[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFNonMating	<- merge(edgeListSleepDFNonMating, edgeListSleepListNonMating[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))

edgeListSleepDFNonMating		<- merge(edgeListSleepDFNonMating, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'from', by.y = 'Name')
edgeListSleepDFNonMating		<- merge(edgeListSleepDFNonMating, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'to', by.y = 'Name')
colnames(edgeListSleepDFNonMating)	<- c('receiver', 'initiator', 'ballAll', 'treeAll', 'ballAM', 'treeAM', 'ballAMSleep', 'treeAMSleep', 'initiatorSex', 'receiverSex')
edgeListSleepDFNonMating$sexCombination	<- ifelse(edgeListSleepDFNonMating$initiatorSex == 'male' & edgeListSleepDFNonMating$receiverSex == 'male', 'M/M',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'female' & edgeListSleepDFNonMating$receiverSex == 'female', 'F/F',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'male' & edgeListSleepDFNonMating$receiverSex == 'female', 'M/F',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'female' & edgeListSleepDFNonMating$receiverSex == 'male', 'M/F',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'female' & edgeListSleepDFNonMating$receiverSex == 'unknown', 'Unk/F',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'unknown' & edgeListSleepDFNonMating$receiverSex == 'female', 'Unk/F',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'male' & edgeListSleepDFNonMating$receiverSex == 'unknown', 'Unk/M',
							ifelse(edgeListSleepDFNonMating$initiatorSex == 'unknown' & edgeListSleepDFNonMating$receiverSex == 'male', 'Unk/M', ''))))))))

edgeListSleepDFNonMating	<- edgeListSleepDFNonMating[,c(2, 1, 3:11)]
edgeListSleepDFNonMating	<- edgeListSleepDFNonMating[order(edgeListSleepDFNonMating$initiator, edgeListSleepDFNonMating$receiver),]
write.csv(edgeListSleepDFNonMating, 'edgeListSleepNonMating.csv', row.names = FALSE)

png('nonMatingSeasonSleepBoxPlots.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow = c(2, 3))
boxplot(ballAll ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Ball All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAM ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Ball AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAMSleep ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Ball AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAll ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Tree All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAM ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Tree AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAMSleep ~ sexCombination, data = edgeListSleepDFNonMating, main = 'Sleep Tree AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
dev.off()

## Birthing season

allSleepNetsBirthing		<- list(ballNetAllBirthing, treeNetAllBirthing, ballNetAMBirthing, treeNetAMBirthing, ballNetAMSleepBirthing, treeNetAMSleepBirthing)
edgeListSleepListBirthing	<- lapply(allSleepNetsBirthing, FUN = get.data.frame)

edgeListSleepDFBirthing	<- merge(edgeListSleepListBirthing[[1]], edgeListSleepListBirthing[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFBirthing	<- merge(edgeListSleepDFBirthing, edgeListSleepListBirthing[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFBirthing	<- merge(edgeListSleepDFBirthing, edgeListSleepListBirthing[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFBirthing	<- merge(edgeListSleepDFBirthing, edgeListSleepListBirthing[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListSleepDFBirthing	<- merge(edgeListSleepDFBirthing, edgeListSleepListBirthing[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))

edgeListSleepDFBirthing		<- merge(edgeListSleepDFBirthing, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'from', by.y = 'Name')
edgeListSleepDFBirthing		<- merge(edgeListSleepDFBirthing, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'to', by.y = 'Name')
colnames(edgeListSleepDFBirthing)	<- c('receiver', 'initiator', 'ballAll', 'treeAll', 'ballAM', 'treeAM', 'ballAMSleep', 'treeAMSleep', 'initiatorSex', 'receiverSex')
edgeListSleepDFBirthing$sexCombination	<- ifelse(edgeListSleepDFBirthing$initiatorSex == 'male' & edgeListSleepDFBirthing$receiverSex == 'male', 'M/M',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'female' & edgeListSleepDFBirthing$receiverSex == 'female', 'F/F',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'male' & edgeListSleepDFBirthing$receiverSex == 'female', 'M/F',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'female' & edgeListSleepDFBirthing$receiverSex == 'male', 'M/F',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'female' & edgeListSleepDFBirthing$receiverSex == 'unknown', 'Unk/F',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'unknown' & edgeListSleepDFBirthing$receiverSex == 'female', 'Unk/F',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'male' & edgeListSleepDFBirthing$receiverSex == 'unknown', 'Unk/M',
							ifelse(edgeListSleepDFBirthing$initiatorSex == 'unknown' & edgeListSleepDFBirthing$receiverSex == 'male', 'Unk/M', ''))))))))

edgeListSleepDFBirthing	<- edgeListSleepDFBirthing[,c(2, 1, 3:11)]
edgeListSleepDFBirthing	<- edgeListSleepDFBirthing[order(edgeListSleepDFBirthing$initiator, edgeListSleepDFBirthing$receiver),]
write.csv(edgeListSleepDFBirthing, 'edgeListSleepBirthing.csv', row.names = FALSE)

png('birthingSeasonSleepBoxPlots.png', width = 12, height = 6, units = 'in', res = 300)
par(mfrow = c(2, 3))
boxplot(ballAll ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Ball All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAM ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Ball AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(ballAMSleep ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Ball AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAll ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Tree All', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAM ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Tree AM', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
boxplot(treeAMSleep ~ sexCombination, data = edgeListSleepDFBirthing, main = 'Sleep Tree AM Still Sleeping', cex.axis = 1.5, cex.lab = 1.5, pch = 20)
dev.off()

#Graph nets for II, III, IV, V, VI, XII
groups	<- c('II', 'III', 'IV', 'V', 'VI', 'XII')
for(i in groups){
	sleepSubset			<- sleep[sleep$Focal.Group == i,]
	sleepSubsetMating		<- sleepSubset[sleepSubset$season == 'mating',]
	sleepSubsetBirthing	<- sleepSubset[sleepSubset$season == 'birthing',]
	sleepSubsetNonMating	<- sleepSubset[sleepSubset$season == 'birthing' | sleepSubset$season == 'other',]
	ballMatAM			<- sleepBallNet(data = sleepSubset, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
	ballMatAMMating		<- sleepBallNet(data = sleepSubsetMating, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
	ballMatAMBirthing		<- sleepBallNet(data = sleepSubsetBirthing, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
	ballMatAMNonMating	<- sleepBallNet(data = sleepSubsetNonMating, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn",  date_format = "%Y-%m-%d")
	sleepBallMatsAMList	<- list(ballMatAM$adjacency.matrix, ballMatAMMating$adjacency.matrix, ballMatAMBirthing$adjacency.matrix, ballMatAMNonMating$adjacency.matrix)
	ballNetsAM			<- lapply(sleepBallMatsAMList, graph_from_adjacency_matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
	
	if(i == 'II'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll[c(1:2, 4, 6:8, 11:14, 16:18),] 
		layNonMating		<- layBirthing	<- layAll[c(1:6, 8:18),] 
	}

	if(i == 'III'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll[c(12:14, 18:20),] 
		layNonMating		<- layAll[c(1:2, 4:5, 7:15, 17:21, 23),] 
		layBirthing			<- layAll 

	}

	if(i == 'IV'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll[c(2, 5, 9:10, 12:13, 16),] 
		layNonMating		<- layAll[c(2:16),] 
		layBirthing			<- layAll 

	}

	if(i == 'V'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll[c(1:2, 4, 6:10, 12),] 
		layNonMating		<- layAll[c(1, 3:12),] 
		layBirthing			<- layAll[c(1, 3:12),]

	}

	if(i == 'VI'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll[c(2, 4:8, 10),] 
		layNonMating		<- layAll[c(1, 3:8, 10:13),] 
		layBirthing			<- layAll
	}

	if(i == 'XII'){
		layAll			<- layout.fruchterman.reingold(ballNetsAM[[1]])
		layMating			<- layAll
		layNonMating		<- layAll
		layBirthing			<- layAll
	}

	sexAll			<- demo[demo$Name %in% V(ballNetsAM[[1]])$name, 'Sex']
	sexMating			<- demo[demo$Name %in% V(ballNetsAM[[2]])$name, 'Sex']
	sexNonMating		<- demo[demo$Name %in% V(ballNetsAM[[3]])$name, 'Sex']
	sexBirthing			<- demo[demo$Name %in% V(ballNetsAM[[4]])$name, 'Sex']

	sexAll			<- ifelse(sexAll == 'female', 'lightgoldenrod1', ifelse(sexAll == 'male', 'midnightblue', 'black'))
	sexAllVertexLabelCol	<- ifelse(sexAll == 'lightgoldenrod1', 'black', 'white')
	sexMating			<- ifelse(sexMating== 'female', 'lightgoldenrod1', ifelse(sexMating== 'male', 'midnightblue', 'black'))
	sexMatingVertexLabelCol	<- ifelse(sexMating== 'lightgoldenrod1', 'black', 'white')
	sexNonMating			<- ifelse(sexNonMating == 'female', 'lightgoldenrod1', ifelse(sexNonMating == 'male', 'midnightblue', 'black'))
	sexNonMatingVertexLabelCol	<- ifelse(sexNonMating == 'lightgoldenrod1', 'black', 'white')
	sexBirthing				<- ifelse(sexBirthing == 'female', 'lightgoldenrod1', ifelse(sexBirthing == 'male', 'midnightblue', 'black'))
	sexBirthingVertexLabelCol	<- ifelse(sexBirthing == 'lightgoldenrod1', 'black', 'white')

	png('seasonalSleepBallAMGroupXII.png', width = 10, height = 10, units = 'in', res = 300)
	par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
	plot.igraph(ballNetsAM[[1]], main = paste('Group', i, 'All Time'), layout = layAll, vertex.size = 25, vertex.label.color = sexAllVertexLabelCol, vertex.color = sexAll, edge.width = E(ballNetsAM[[1]])$weight*5)
	plot.igraph(ballNetsAM[[2]], main = paste('Group', i, 'Mating'), layout = layMating, vertex.size = 25, vertex.label.color = sexMatingVertexLabelCol, vertex.color = sexMating, edge.width = E(ballNetsAM[[2]])$weight*5)
	plot.igraph(ballNetsAM[[3]], main = paste('Group', i, 'Non-Mating'), layout = layNonMating, vertex.size = 25, vertex.label.color = sexNonMatingVertexLabelCol, vertex.color = sexNonMating, edge.width = E(ballNetsAM[[3]])$weight*5)
	plot.igraph(ballNetsAM[[4]], main = paste('Group', i, 'Birthing'), layout = layBirthing, vertex.size = 25, vertex.label.color = sexBirthingVertexLabelCol, vertex.color = sexBirthing, edge.width = E(ballNetsAM[[4]])$weight*5)
	dev.off()
}

#############################
### Yearly sleep networks ###
#############################
yearlySleepNets	<- function(sleepData, typeOfYear, group, years){
	sleepDataSubset	<- sleepData[sleepData$Focal.Group == group,]
	sleepNets		<- list()
	if(typeOfYear == 'calendar'){
		startDates	<- paste(years, "-01-01", sep = '')
		endDates	<- paste(years, "-12-31", sep = '')
	}
	if(typeOfYear == 'lemur'){
		startDates	<- paste(years, "-07-01", sep = '')
		endDates	<- paste(years+1, "-06-30", sep = '')
	}
	#print(startDates)
	for(i in 1:length(startDates)){
		treeMatAll		<- sleepTreeNet(data = sleepDataSubset, begin = startDates[i], end = endDates[i], dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
		treeNetAll		<- graph_from_adjacency_matrix(treeMatAll$adjacency.matrix, weighted = TRUE, diag = FALSE, mode = 'undirected')
		sleepNets[[i]]	<- treeNetAll
	}
	return(sleepNets)
}
		
sleepNetsII <- yearlySleepNets(sleep, 'calendar', 'II', 2011:2019)

reshapeYearlyNets	<- function(nets){
	allVertices	<- unique(names(unlist(lapply(nets, V))))
	for(i in 1:length(nets)){
		newMat	<- matrix(, length(allVertices), length(allVertices), dimnames = list(allVertices, allVertices))
		oldMat	<- as.matrix(as_adjacency_matrix(nets[[i]], attr = "weight"))
		oldVertices	<- V(nets[[i]])$name
		#print(oldVertices)
		for(j in oldVertices){
			for(k in oldVertices){
				newMat[rownames(newMat) == j, colnames(newMat) == k]	<- oldMat[rownames(oldMat) == j, colnames(oldMat) == k]
			}
		}
		#print(newMat)
		newMat[is.na(newMat)] <- 0
		#print(newMat)
		nets[[i]]	<- graph_from_adjacency_matrix(newMat, weighted = TRUE, diag = FALSE, mode = 'undirected')
		V(nets[[i]])$present	<- rep(NA, length(allVertices))
		V(nets[[i]])$present	<- allVertices %in% oldVertices
		#print(V(nets[[i]])$present)
		V(nets[[i]])$sex		<- c(rep('male', 4), rep('female', 5), 'unknown', rep('female', 1), rep('male', 2), rep('female', 2), rep('male', 4), 'unknown')
		V(nets[[i]])$vertexColor	<- ifelse(V(nets[[i]])$sex == 'female', 'lightgoldenrod1', ifelse(V(nets[[i]])$sex == 'male', 'midnightblue', 'black'))
		V(nets[[i]])[V(nets[[i]])$present == FALSE]$vertexColor	<- NA
		#print(V(nets[[i]])$vertexColor)
	}
	return(nets)
}

sleepNetsIIReShape	<- reshapeYearlyNets(sleepNetsII)
lay	<- layout.fruchterman.reingold(sleepNetsIIReShape[[1]])

png('groupIICalendarYearSleepNets.png', width = 10, height = 10, units = 'in', res = 300)
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0))
for(m in 1:length(sleepNetsIIReShape)){
	#print(V(sleepNetsIIReShape[[m]])$vertexColor)
	vertexLabelColor	<- gsub('black', 'white', V(sleepNetsIIReShape[[m]])$vertexColor)
	vertexLabelColor	<- gsub('lightgoldenrod1', 'black', vertexLabelColor)
	vertexLabelColor	<- gsub('midnightblue', 'white', vertexLabelColor)
	#print(vertexLabelColor)
	vertexLabel	<- c()
	for(k in 1:20){
		vertexLabel[k]		<- ifelse(V(sleepNetsIIReShape[[m]])$present[k] == TRUE, V(sleepNetsIIReShape[[m]])$name[k], '')
	}

	#Plot edges first, then nodes
	#print(vertexLabelColor)
	plot.igraph(sleepNetsIIReShape[[m]], edge.width = E(sleepNetsIIReShape[[m]])$weight*3, vertex.size = 25, layout = lay, vertex.label = vertexLabel, vertex.frame.color = V(sleepNetsIIReShape[[m]])$vertexColor, vertex.label.color = vertexLabelColor, vertex.color = V(sleepNetsIIReShape[[m]])$vertexColor)

}
dev.off()

#######################################
### Manipulating instantaneous data ###
#######################################
allFocalActivity	<- rbind.data.frame(focalActivity, laura)

#Assuing scannee1 is the nearest neighbor - fix later
allFocalActivitySimp	<- allFocalActivity[,c(1, 4, 7:9, 11, 13, 16:17, 19:26)]
nnSimp			<- nn[,c(2, 5, 11, 9, 8, 26, 12, 15, 19, 18, 17, 20:25)]
colnames(nnSimp)		<- colnames(allFocalActivitySimp)
focalActivityNN		<- rbind.data.frame(allFocalActivitySimp, nnSimp)

filemakerWithGroup	<- merge(filemaker, filemakerGroup, by.x = c('Date', 'Focal'), by.y = c('Date', 'Animal'), all.x = TRUE)
filemakerWithGroup$Group.y	<- gsub(2, 'II', filemakerWithGroup$Group.y)
filemakerWithGroup$Group.y	<- gsub(3, 'III', filemakerWithGroup$Group.y)
filemakerWithGroup$Group.y	<- gsub(4, 'IV', filemakerWithGroup$Group.y)
filemakerWithGroup$Group.y	<- gsub(5, 'V', filemakerWithGroup$Group.y)
filemakerWithGroup$Group.y	<- gsub(6, 'VI', filemakerWithGroup$Group.y)
filemakerWithGroup$GroupSpread	<- NA
filemakerWithGroup$FocalTreeNum	<- NA
filemakerWithGroup$FocalTreeSp	<- NA

write.csv(filemakerWithGroup[,c(1:8, 23, 10:22)], 'FileMakerDataWithGroupNames.csv')

fileMakerColSimp		<- filemakerWithGroup[,c(4, 1, 2, 23, 8, 25, 12:14, 26:27, 15, 17, 21, 22, 19, 18)]

#Just select the first row for a scan right now
fileMakerRowSimp		<- fileMakerColSimp[!duplicated(fileMakerColSimp[,c(1:3, 7)]),]
colnames(fileMakerRowSimp)	<- colnames(focalActivityNN)

##All instantaneous data
allScan		<- rbind.data.frame(focalActivityNN, fileMakerRowSimp)

######################################
### Creation of prelim groups file ###
######################################
sleepGroups	<- aggregate(sleep[,11], by = list(date = sleep$Date, group = sleep$Focal.Group, animal = sleep$ID), FUN = length)
sleepGroups$dataset	<- 'Sleep'

fileMakerAnimalSummary	<- fileMakerColSimp[,c(2, 4, 3, 12)]
fileMakerAnimals		<- melt(fileMakerAnimalSummary, id.vars = c('Date', 'Group.y'))
fileMakerAnimalsGroups	<- aggregate(fileMakerAnimals[,4], by = list(date = fileMakerAnimals$Date, group = fileMakerAnimals$Group.y, animal = fileMakerAnimals$value), FUN = length)
fileMakerAnimalsGroups$dataset	<- 'FileMaker'

focalActivityAnimalSummary	<- allFocalActivity[,c(4, 8, 7, 21, 28, 35, 42, 49, 56, 63, 69, 75, 81, 87, 93, 100)]
focalActivityAnimals		<- melt(focalActivityAnimalSummary, id.vars = c('Date', 'Group'))
focalActivityAnimalsGroups	<- aggregate(focalActivityAnimals[,4], by = list(date = focalActivityAnimals$Date, group = focalActivityAnimals$Group, animal = focalActivityAnimals$value), FUN = length)
focalActivityAnimalsGroups$dataset	<- 'FocalActivity'

nnAnimalSummary	<- nn[,c(6, 10, 12, 21, 31)]
nnAnimals		<- melt(nnAnimalSummary, id.vars = c('date', 'group'))
nnAnimalsGroups	<- aggregate(nnAnimals[,4], by = list(date = nnAnimals$date, group = nnAnimals$group, animal = nnAnimals$value), FUN = length)
nnAnimalsGroups$dataset	<- 'NearestNeighbor'

censusGroups	<- aggregate(census[,5], by = list(date = census$Date, group = census$Group, animal = census$Animal), FUN = length)
censusGroups$dataset	<- 'Census'

allGroups	<- rbind.data.frame(focalActivityAnimalsGroups, nnAnimalsGroups, fileMakerAnimalsGroups, sleepGroups, censusGroups)
allGroupsNoDup	<- allGroups[!duplicated(allGroups[,1:3]),]
allGroupsNoDup	<- allGroupsNoDup[order(allGroupsNoDup$date, allGroupsNoDup$group, allGroupsNoDup$animal),]
write.csv(allGroupsNoDup, 'groupsFile.csv')

socialDataAnimalSummary	<- socialData[,c(4, 10, 11)]
socialAnimals		<- melt(socialDataAnimalSummary, id.vars = c('Date'))
socialAnimalsPerDate	<- aggregate(socialAnimals[,3], by = list(date = socialAnimals$Date, animal = socialAnimals$value), FUN = length)

groupsAnimalPerDate	<- aggregate(allGroupsNoDup[,4], by = list(date = allGroupsNoDup$date, animal = allGroupsNoDup$animal), FUN = sum)
animalsInSocialNotElsewhere	<- socialAnimalsPerDate[!factor(factor(socialAnimalsPerDate$date):factor(socialAnimalsPerDate$animal)) %in% factor(factor(groupsAnimalPerDate$date):factor(groupsAnimalPerDate$animal)),] 
write.csv(animalsInSocialNotElsewhere, 'animalsInSocialButNotElsewhere.csv')

#########################################################
### Pull edge lists together to get rates of behavior ###
#########################################################
allNets		<- list(cntNDNet, cntDNet, prxNDNet, prxDNet, grmNDNet, grmDNet, itgDNet, suppDNet, aggDNet)

edgeListList	<- lapply(allNets, FUN = get.data.frame)

# Creates one dataset with all of the behaviors as columns
edgeListDF		<- merge(edgeListList[[1]], edgeListList[[2]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[3]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[4]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[5]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[6]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[7]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[8]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))
edgeListDF		<- merge(edgeListDF, edgeListList[[9]], all.x = TRUE, all.y = TRUE, by.x = c('from', 'to'), by.y = c('from', 'to'))

# Adds initiator/receiver sex
edgeListDF		<- merge(edgeListDF, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'from', by.y = 'Name')
edgeListDF		<- merge(edgeListDF, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'to', by.y = 'Name')
colnames(edgeListDF)	<- c('receiver', 'initiator', 'cntND', 'cntD', 'prxND', 'prxD', 'grmND', 'grmD', 'itgD',
					'suppD', 'aggD', 'initiatorSex', 'receieverSex')

# There are still some dyads which have a NA in the ND version but a number in the direction possibly cause all app 1m
edgeListDF		<- edgeListDF[order(c(edgeListDF$initiator, edgeListDF$receiver)), c(2, 1, 3:13)]

##################
### Centrality ###
##################
prxCntNDNet		<- graph_from_adjacency_matrix(cntNDMat + prxNDMat, mode = 'lower', weighted = TRUE)

cntEigen	<- eigen_centrality(cntNDNet, directed = FALSE)$vector
cntBetween	<- betweenness(cntNDNet, directed = FALSE)
cntStrength	<- strength(cntNDNet, mode = 'all')
prxEigen	<- eigen_centrality(prxNDNet, directed = FALSE)$vector
prxBetween	<- betweenness(prxNDNet, directed = FALSE)
prxStrength	<- strength(prxNDNet, mode = 'all')
prxCntEigen		<- eigen_centrality(prxCntNDNet, directed = FALSE)$vector
prxCntBetween	<- betweenness(prxCntNDNet, directed = FALSE)
prxCntStrength	<- strength(prxCntNDNet, mode = 'all')

centralityDF	<- cbind.data.frame(V(cntNDNet)$name, cntEigen, cntBetween, cntStrength, prxEigen, prxBetween, prxStrength,
				prxCntEigen, prxCntBetween, prxCntStrength)
rownames(centralityDF)	<- NULL
colnames(centralityDF)	<- c('Name', 'cntEigen', 'cntBetween', 'cntStrength', 'prxEigen', 'prxBetween',
				'prxStrength', 'prxCntEigen', 'prxCntBetween', 'prxCntStrength')

centralityDF	<- merge(centralityDF, demo[,c('Name', 'Sex')], all.x = TRUE, by.x = 'Name', by.y = 'Name')