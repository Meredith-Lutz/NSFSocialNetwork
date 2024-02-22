########## Clean Focal Nearest Neighbor dataset ##########

##### Set up workspace #####

rm(list=ls())
options(stringsAsFactors = F)
setwd('C:/Users/mclutz/Box/GoogleDriveBackup/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

library("here")
library("tidyverse")

##### Load nearest-neighbor data #####

#Load data
focal.nn <- read.csv("Daniel2020.01-2021.05.csv", na.strings=c("","NA")) 

#Create unique row id so can divide columns up
focal.nn <- unique(focal.nn)     #remove 0
focal.nn$unique <- as.numeric(row.names(focal.nn))

#Clean extra spaces
focal.nn$Observer <- gsub(" ", "", focal.nn$Observer)
focal.nn$Group <- gsub(" ", "", focal.nn$Group)
focal.nn$Focal <- gsub(" ", "", focal.nn$Focal)
focal.nn$Nearest.neighbor <- gsub(" ", "", focal.nn$Nearest.neighbor)
focal.nn$NN.2 <- gsub(" ", "", focal.nn$NN.2)

#Clean date columns
focal.nn$Date <- as.Date(focal.nn$Date, format = "%m/%d/%y")

#Clean time columns
focal.nn$Time <- as.POSIXct(focal.nn$Time, format = "%H:%M:%S")

##### Assign focal_id number #####
focal.nn$focal_id_new <- NA
focal.nn	<- focal.nn[order(focal.nn$Date, focal.nn$Time),]
focal.nnSort	<- focal.nn[order(focal.nn$Date, focal.nn$Time),]
focal.id.counter <- 10668
for(i in 1:nrow(focal.nn)){
	if(!is.na(focal.nn$focal_id_new[i]))
		next
	date.i <- focal.nn$Date[i]
	#print(i)
	#print(date.i)
	animal.i <- focal.nn$Focal[i]
	#print(animal.i)
	observer.i <- focal.nn$Observer[i]
	#print(observer.i)
	focal.indicies <- i
	focal.size <- 0
	while(focal.size != length(focal.indicies)){
		focal.size <- length(focal.indicies)
		#print(focal.size)
		time.min.i <- min(focal.nn$Time[focal.indicies])
		#print(time.min.i)
		time.max.i <- max(focal.nn$Time[focal.indicies])
		#print(class(time.min.i))
		#print(time.max.i)
   		focal.indicies <- unique(c(focal.indicies, 
                              	which(focal.nn$Observer == observer.i & 
                                    	focal.nn$Date == date.i & 
                                    	focal.nn$Focal == animal.i & 
                                    	focal.nn$Time >= (time.min.i - 20*60) & 
                                     	focal.nn$Time <= (time.max.i + 20*60))))
 	}
	focal.nn$focal_id_new[focal.indicies] <- focal.id.counter
	focal.id.counter <- focal.id.counter + 1
	rm(date.i)
	rm(animal.i)
	rm(observer.i)
	rm(time.min.i)
	rm(time.max.i)
}

focal.nn$fullFocalID	<- paste('NN', focal.nn$focal_id_new, sep = '')

write.csv(focal.nn, 'Daniel2020.01-2021.05WithFocalID.csv', row.names = FALSE)

rm(focal.id.counter)
rm(focal.indicies)
rm(focal.size)

##### Create summary file for checking focals #####

focal.ids <- focal.nn%>% group_by(fullFocalID) %>% summarise(observer = unique(Observer),
                                                              date = unique(Date), 
                                                              group = unique(Group),
                                                              start_time = min(Time),
                                                              stop_time = max(Time), 
                                                              focal_animal = unique(Focal),
                                                              num_scans = length(fullFocalID)) %>%
  ungroup()


focal.ids$interval <- lubridate::interval(focal.ids$start_time, focal.ids$stop_time)
sort(table(focal.ids$num_scans))

#Check for overlap between focals
focal.ids$overlap <- NA
for(i in 1:nrow(focal.ids)){
	print(i)
	interval.i <- focal.ids$interval[i]
	obs.i <- filter(focal.ids, observer == focal.ids$observer[i] & date == focal.ids$date[i] & 
				fullFocalID != focal.ids$fullFocalID[i])
	if(any(lubridate::int_overlaps(interval.i, obs.i$interval))){
		focal.ids$overlap[i] <- TRUE
	}
	else{
		focal.ids$overlap[i] <- FALSE
	}
	rm(interval.i)
	rm(obs.i)
}
summary(focal.ids$overlap) 
#248 overlapping; now 28

focal.ids$start_time <- format(focal.ids$start_time, format = "%H:%M:%S")
focal.ids$stop_time <- format(focal.ids$stop_time, format = "%H:%M:%S")

write.csv(focal.ids, 'focalListForDaniel2020.01-2021.05WithFocalID.csv', row.names = FALSE)
