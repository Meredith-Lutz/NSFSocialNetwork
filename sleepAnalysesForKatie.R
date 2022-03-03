#########################################
##### Sleeping network calculations #####
#####  Last modified by ML 12/2021  #####
#########################################
sleep				<- read.csv('All_Sleep_Tree_Data_Feb2020_corrected BL Sept2_2021_ML.csv', stringsAsFactors = FALSE)

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

stillSleep				<- sleep[sleep$Found.Sleeping. == 1 & is.na(sleep$Found.Sleeping.) == FALSE,]

## This creates the matrix
## To extract the matrix with values = % of observations in sleep ball, use:
## ballMatAll$adjacency.matrix
ballMatAll		<- sleepBallNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")
treeMatAll		<- sleepTreeNet(data = sleep, begin = "2011-07-08", end = "2019-12-31", dawn_dusk = "dawn_dusk",  date_format = "%Y-%m-%d")

