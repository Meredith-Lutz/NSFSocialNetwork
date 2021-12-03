#################################################################
#################################################################
##### Sifaka Research Project Observation Time Calculations #####
##### 		 Last updated by ML 11.16.21		      #####
#################################################################
#################################################################

#Read in data - list of focals and groups file
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')
groups		<- read.csv('Compiled Group File with some data deleted for BL analysis_Nov 3 2021_ML Corrected11Nov2021.csv')
focalListActv	<- read.csv('FocalActivityIDs_TMM_ML_11Nov2021.csv')

#Set your working directory to whereever you have the ObservationTimeFunctions.R file saved
setwd('G:/My Drive/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses/NSFSocialNetwork')

#Let R read in the functions
source('ObservationTimeFunctions.R')

#Can pick as many or as few animals as you want
#If you want everyone, I'd suggest reading in the demographics file and saving the name variable to use
sifakaNames	<- c('Abby', 'William', 'Zipper', 'Ana', 'Walrus', 'Aristotle')

#Example calculation for observation matrix
#See functions for full details on the structure of files, but fullFocalList and groups should be of the standard SRP format
#fullFocalList needs "date", "focal_animal", and "number_scans"
#groups needs "date", "group", and "animal"
obsMat	<- calculateObservationMatrix(focalListActv, groups, '2008-01-01', '2020-12-31', sifakaNames)

#Example calculation for normal times per individual
calculateObservationTimes(focalListActv, '2008-01-01', '2020-12-31', sifakaNames)