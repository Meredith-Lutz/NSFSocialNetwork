#############################################################
#############################################################
##### Sifaka Research Project Observation Time Formulas #####
##### 		Last updated by ML 11.16.21	        #####
#############################################################
#############################################################

#Examples on how to use are in the how to use file - you shouldn't need to change anything here

calculateObservationMatrix	<- function(focalList, groupsFile, startDate, endDate, animals){
	#focalList is a dataframe of focals of the SRP format with columns "date", "focal_animal", "number_scans"
	#groupsFile is a dataframe of where individuals were of the SRP format with columns "date", "group", "animal"
	#startDate is the first date you'll like to use in your analysis in yyyy-mm-dd format
	#endDate is the last date you'll like to use in your analysis in yyyy-mm-dd format
	#animals is a vector of all of the animal names you'll want in your analysis 
	
	#Subset focal list to include just animals and dates desired 
	focalListDateSubset	<- focalList[as.Date(focalList$date) >= as.Date(startDate) & as.Date(focalList$date) <= endDate,]
	focalListSubset		<- focalListDateSubset[focalListDateSubset$focal_animal %in% animals,]
	
	#Initialize observation matrix
	obsMat		<- 0*as.matrix(table(animals)%*%t(table(animals)))
	
	uniqueDays		<- unique(groupsFile$date)
		
	#Calculate observation matrix for each day and then sum up across days 
	for(i in uniqueDays){
		print(paste("Starting Day", i))
		groupsObserved	<- unique(groupsFile[groupsFile$date == i, "group"])
		#print(groupsObserved)
		
		#handle each group separately
     		for(j in groupsObserved){
			finalSubset	<- focalListSubset[focalListSubset$date ==  i & focalListSubset$group == j,]
			#print(paste(j, "Has", dim(subsetFocalList)[1], "Focals on", i))
			if(dim(finalSubset)[1] == 0){ #If there's no focals for that group, move to next group
				next
			}

			#Use groupsFile to see who is present regardless of if they have a focal 
			animalsPresent	<- groupsFile[groupsFile$date == i & groupsFile$group == j, "animal"]
			
			for(k in animalsPresent){
 				for(m in animalsPresent){
					focals	<- finalSubset[finalSubset$focal_animal == k | finalSubset$focal_animal == m, ]
					obsTime	<- 10*sum(focals$number_scans,na.rm = TRUE) #Each scan is 10 minutes
					obsMat[rownames(obsMat) == k, colnames(obsMat) == m]	<- obsTime + obsMat[rownames(obsMat) == k, colnames(obsMat) == m]
					obsMat[rownames(obsMat) == m, colnames(obsMat) == k]	<- obsTime + obsMat[rownames(obsMat) == m, colnames(obsMat) == k]
				}
			}
		}
	}
	return(obsMat)
}

calculateObservationTimes	<- function(focalList, startDate, endDate, animals){
	#focalList is a dataframe of focals of the SRP format with columns "date", "focal_animal", "number_scans"
	#startDate is the first date you'll like to use in your analysis in yyyy-mm-dd format
	#endDate is the last date you'll like to use in your analysis in yyyy-mm-dd format
	#animals is a vector of all of the animal names you'll want in your analysis 
	
	#Subset focal list to include just animals and dates desired 
	focalListDateSubset	<- focalList[as.Date(focalList$date) >= as.Date(startDate) & as.Date(focalList$date) <= endDate,]
	focalListSubset		<- focalListDateSubset[focalListDateSubset$focal_animal %in% animals,]
	
	#Calculate number of scans per animal
	nScans			<- aggregate(focalListSubset$number_scans, by = list(focalListSubset$focal_animal), FUN = sum)
	colnames(nScans)		<- c('focal_animal', 'number_scans')
	nScans$nMin			<- nScans$number_scans * 10 #Each scan is 10 minutes
	return(nScans)
}

#calculateObservationMatrix(fullFocalList, groups, '2008-01-01', '2020-12-31', sifakaNames)
#calculateObservationTimes(fullFocalList, '2008-01-01', '2020-12-31', sifakaNames)
