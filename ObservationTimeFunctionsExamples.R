#################################################################
#################################################################
##### Sifaka Research Project Observation Time Calculations #####
##### 		 Last updated by ML 11.16.21		      #####
#################################################################
#################################################################

#Set your working directory to whereever you have the ObservationTimeFunctions.R file saved
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

#Let R read in the functions
source('ObservationTimeFunctions.R')

#Can pick as many or as few animals as you want
#If you want everyone, I'd suggest reading in the demographics file and saving the name variable to use
sifakaNames	<- c('Abby', 'William', 'Zipper', 'Ana', 'Walrus', 'Aristotle')

#Example calculation for observation matrix
#See functions for full details on the structure of files, but fullFocalList and groups should be of the standard SRP format
#fullFocalList needs "date", "focal_animal", and "number_scans"
#groups needs "date", "group", and "animal"
calculateObservationMatrix(fullFocalList, groups, '2008-01-01', '2020-12-31', sifakaNames)

#Example calculation for normal times per individual
calculateObservationTimes(fullFocalList, '2008-01-01', '2020-12-31', sifakaNames)