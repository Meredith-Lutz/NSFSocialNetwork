########## Clean Focal Nearest Neighbor dataset ##########

##### Set up workspace #####

rm(list=ls())
options(stringsAsFactors = F)
#setwd("C:/Users/tmontgomery/Dropbox/MPI/SifakaData")
#setwd("~/Dropbox/MPI/SifakaData")
setwd('C:/Users/mclutz/Box/GoogleDriveBackup/Graduate School/Research/Projects/KMNPLongTermData/NSF Analyses')

library("here")
library("tidyverse")
load("01.life_history.Rdata")
load("02.census.Rdata")
load("07.lookup_lists.Rdata")


##### Load nearest-neighbor data #####

#Load data
focal.nn <- read.csv("Nearest_Neighbor_All_Data_11-16-20.csv", 
                     na.strings=c("","NA")) 

oldColnames	<- colnames(focal.nn)

#Convert character columns to lower case
focal.nn <- data.frame(lapply(focal.nn,      
                              function(variables) {
                                if (is.character(variables)) {
                                  return(tolower(enc2utf8(variables)))
                                } else {
                                  return(variables)
                                }
                              }),
                       stringsAsFactors = FALSE)

#Convert variable names to lowercase and underscore
FormatVarNames <- function (df) {
  colnames(df) <- gsub("([a-z])([A-Z])", "\\1\\_\\2", colnames(df)) 
  colnames(df) <- gsub("\\.", "\\1\\_\\2", colnames(df))
  colnames(df) <- tolower(colnames(df))
  df = df
}

focal.nn <- FormatVarNames(focal.nn)

#Check for columns that are all NA
na.test <-  function (x) {
  w <- sapply(x, function(x)all(is.na(x)))
  if (any(w)) {
    stop(paste("All NA in columns", paste(which(w), collapse=", ")))
  }
}

na.test(focal.nn)     #none
str(focal.nn)

#Create unique row id so can divide columns up
focal.nn <- unique(focal.nn)     #remove 0
focal.nn$unique <- as.numeric(row.names(focal.nn))

#Clean observer & group columns
focal.nn$x <- NULL
focal.nn$file <- gsub(" ", "", focal.nn$file)
focal.nn$observer <- gsub(" ", "", focal.nn$observer)
focal.nn$group <- gsub(" ", "", focal.nn$group)

#Fix observer columns
summary(as.factor(focal.nn$observer))
filter(focal.nn, observer == "daniel" & 
         (grepl("francis", focal.nn$file) | grepl("patrick", focal.nn$file)))    #4
filter(focal.nn, observer == "francis" & 
         (grepl("daniel", focal.nn$file) | grepl("patrick", focal.nn$file)))    #0
filter(focal.nn, observer == "patrick" & 
         (grepl("francis", focal.nn$file) | grepl("daniel", focal.nn$file)))    #0
focal.nn[focal.nn$observer == "daniel" & grepl("francis", focal.nn$file),]$observer <- "francis"

#Fix group columns
summary(as.factor(focal.nn$group))
filter(focal.nn, group == "isaac")
filter(census, census_month == "october" & year == 2017)   #probably same as anthony's group - ask Becca


##### Animal columns #####

#Check animals
focal.nn$focal <- gsub(" ", "", focal.nn$focal)
focal.nn$nearest_neighbor <- gsub(" ", "", focal.nn$nearest_neighbor)
focal.nn$nn_2 <- gsub(" ", "", focal.nn$nn_2)
focal.nn[focal.nn$focal == "jipper" & !is.na(focal.nn$focal),]$focal <- "zipper"
focal.nn[focal.nn$nearest_neighbor == "packy" & !is.na(focal.nn$nearest_neighbor),]$nearest_neighbor <- "avana"
focal.nn[focal.nn$nearest_neighbor == "zacky" & !is.na(focal.nn$nearest_neighbor),]$nearest_neighbor <- "arrow"
focal.nn[focal.nn$nn_2 == "rossy" & !is.na(focal.nn$nn_2),]$nn_2 <- "rossi"

unique(anti_join(focal.nn, animals, by = c("focal" = "name"))[,c("focal")])
# [1] NA               "unmarkedfemale" "unmarkedmale"  

unique(anti_join(focal.nn, animals, by = c("nearest_neighbor" = "name"))[,c("nearest_neighbor")])
# [1] NA             "unmarkedmale"

unique(anti_join(focal.nn, animals, by = c("nn_2" = "name"))[,c("nn_2")])
# [1] NA


##### Date columns #####

#Clean date columns
focal.nn$date <- as.Date(focal.nn$date, format = "%Y-%m-%d")
focal.nn$year <- as.numeric(focal.nn$year)
focal.nn$month <- gsub(" ", "", focal.nn$month)
focal.nn[focal.nn$month == 2,]$month <- "feb"
focal.nn[focal.nn$month == 3,]$month <- "mar"
focal.nn[focal.nn$month == 4,]$month <- "apr"
focal.nn[focal.nn$month == 5,]$month <- "may"
focal.nn[focal.nn$month == 6,]$month <- "jun"
focal.nn[focal.nn$month == 7,]$month <- "jul"

#Check date columns
focal.nn$date_year <- format(focal.nn$date, "%Y")
focal.nn$date_month <- tolower(format(focal.nn$date, "%b"))
focal.nn$date_day <- tolower(format(focal.nn$date, "%d"))
focal.nn$date_mismatch <- ifelse((focal.nn$year != focal.nn$date_year) | 
                                   (focal.nn$month != focal.nn$date_month), TRUE, FALSE)
focal.nn$date_abb <- as.Date(paste(focal.nn$year, focal.nn$month, focal.nn$date_day, sep = "-"), 
                             format = "%Y-%b-%d")
nrow(filter(focal.nn, date_mismatch == T))     #66

#Fix date columns
focal.nn$date_cor <- NA
for(i in 1:nrow(focal.nn)){
  if(focal.nn$date_mismatch[i] == F){
    focal.nn$date_cor[i] <- as.character(focal.nn$date[i])
  }
  #Check to see which date has more lines by that observer in that group - assign whichever is more
  if(focal.nn$date_mismatch[i] == T){
    date.i <- focal.nn$date[i]
    abb.i <- focal.nn$date_abb[i]
    observer.i <- focal.nn$observer[i]
    group.i <- focal.nn$group[i]
    num.date.i <- nrow(filter(focal.nn, date == date.i & observer == observer.i & group == group.i))
    num.abb.i <- nrow(filter(focal.nn, date == abb.i & observer == observer.i & group == group.i))
    if(num.date.i > num.abb.i){
      focal.nn$date_cor[i] <- as.character(focal.nn$date[i])
    }
    if(num.date.i < num.abb.i){
      focal.nn$date_cor[i] <- as.character(focal.nn$date_abb[i])
    }
    rm(date.i)
    rm(abb.i)
    rm(observer.i)
    rm(group.i)
    rm(num.date.i)
    rm(num.abb.i)
  }
}
focal.nn$date_cor <- as.Date(focal.nn$date_cor, format = "%Y-%m-%d")

#Remove columns
focal.nn$date_year <- NULL
focal.nn$date_month <- NULL
focal.nn$date_day <- NULL
focal.nn$date_mismatch <- NULL
focal.nn$date_abb <- NULL

#Record changes
focal.nn$date_changed <- ifelse(focal.nn$date == focal.nn$date_cor, FALSE, TRUE)


##### Time columns #####

#Clean time columns
focal.nn$time <- as.POSIXct(focal.nn$time, format = "%H:%M:%S")
focal.nn$time <- format(focal.nn$time, format = "%H:%M")
focal.nn$time <- as.POSIXct(focal.nn$time, format = "%H:%M")
focal.nn$approximate_time <- as.POSIXct(strptime(focal.nn$approximate_time, "%I:%M %p"), 
                                        format = "%H:%M:%S")
focal.nn$approximate_time <- format(focal.nn$approximate_time, format = "%H:%M")
focal.nn$approximate_time <- as.POSIXct(focal.nn$approximate_time, format = "%H:%M")
focal.nn$difference <- as.POSIXct(focal.nn$difference, format = "%H:%M:%S")
focal.nn$difference <- format(focal.nn$difference, format = "%H:%M")
focal.nn$difference <- as.POSIXct(focal.nn$difference, format = "%H:%M")

#Fill in NAs
focal.nn$time_new <- ifelse(is.na(focal.nn$time), as.character(focal.nn$approximate_time), 
                            as.character(focal.nn$time))
focal.nn$time_new <- as.POSIXct(focal.nn$time_new, format = "%Y-%m-%d %H:%M:%S")

#Check time columns
focal.nn$time_mismatch <- ifelse((focal.nn$time != focal.nn$approximate_time), TRUE, FALSE)
focal.nn$time_diff <- as.numeric(abs(difftime(focal.nn$time, focal.nn$approximate_time, units = "mins")))

#Fix mistakes programmatically
focal.nn$time_cor <- NA
for(i in 1:nrow(focal.nn)){
  #If time_diff <= 4, don't change anything 
  if(!is.na(focal.nn$time_diff[i]) & focal.nn$time_diff[i] <= 4){
    focal.nn$time_cor[i] <- as.character(focal.nn$time_new[i])
  }
  #If time_diff > 4, look for correct answer
  else if(!is.na(focal.nn$time_diff[i]) & focal.nn$time_diff[i] > 4){
    #If times are already spaced out by 10 minutes, don't change anything
    if(!((focal.nn$time_new[i] - 10*60) == focal.nn$time_new[i-1] & 
         (focal.nn$time_new[i] + 10*60) == focal.nn$time_new[i+1])){
      #If date and focal is the same for current line, previous line, and next line, continue looking
      if(focal.nn$date_cor[i] == focal.nn$date_cor[i-1] & focal.nn$date_cor[i] == focal.nn$date_cor[i+1] & 
         focal.nn$focal[i] == focal.nn$focal[i-1] & focal.nn$focal[i] == focal.nn$focal[i+1]){
        #If time_new == approximate_time for both the line before and the line after
        if(focal.nn$approximate_time[i-1] == focal.nn$time_new[i-1] & 
           focal.nn$approximate_time[i+1] == focal.nn$time_new[i+1]){
          #If approximate_time is not already the correct time for the line before or the line after
          if(focal.nn$approximate_time[i] != focal.nn$time_new[i-1] & 
             focal.nn$approximate_time[i] != focal.nn$time_new[i+1]){
            #If approximate_time is spaced out by 10 minutes as appropriate
            if((focal.nn$approximate_time[i] - 10*60) == focal.nn$time_new[i-1] & 
               (focal.nn$approximate_time[i] + 10*60) == focal.nn$time_new[i+1]){
              focal.nn$time_cor[i] <- as.character(focal.nn$approximate_time[i])
            }
          }
        }
      }
    }
  }
}
focal.nn$time_cor <- ifelse(is.na(focal.nn$time_cor), as.character(focal.nn$time_new), 
                            as.character(focal.nn$time_cor))
focal.nn$time_cor <- as.POSIXct(focal.nn$time_cor, format = "%Y-%m-%d %H:%M:%S")

#Remove columns
focal.nn$time_new <- NULL
focal.nn$time_mismatch <- NULL

#Record changes
focal.nn$time_changed <- ifelse(focal.nn$time == focal.nn$time_cor, FALSE, TRUE)


##### Fix changes made by hand #####

#Load checked data
nn.checked <- read.csv("~/Downloads/check.focalid.nn_tmm_ML.csv", 
                       na.strings=c("","NA"))

#Format columns
nn.checked$focal.correct <- gsub(" ", "", nn.checked$focal.correct)
nn.checked$date.correct <- as.Date(nn.checked$date.correct, format = "%d-%b-%y")
nn.checked$time.correct <- as.POSIXct(nn.checked$time.correct, format = "%H:%M")
nn.checked$delete_if_duplicate <- as.logical(nn.checked$delete_if_duplicate)

#Check columns
unique(anti_join(nn.checked, animals, by = c("focal.correct" = "name"))[,c("focal.correct")])
summary(nn.checked)

#Combine with dataset
focal.nn <- left_join(focal.nn, nn.checked[,c(1,26:32)], by = "unique")
summary(focal.nn)
#date_changed = 10
#time_changed = 40

#Fix date & time
for(i in 1:nrow(focal.nn)){
  if(is.na(focal.nn$date.correct[i]) & is.na(focal.nn$time.correct[i])){
    next
  }
  #If date.correct is present, fix date_cor and date_changed
  if(!is.na(focal.nn$date.correct[i])){
    focal.nn$date_cor[i] <- focal.nn$date.correct[i]
    focal.nn$date_changed[i] <- TRUE
  }
  #If time.correct is present, fix time_cor and time_changed
  if(!is.na(focal.nn$time.correct[i])){
    focal.nn$time_cor[i] <- focal.nn$time.correct[i]
    focal.nn$time_changed[i] <- TRUE
  }
}
summary(focal.nn)
#date_changed = 198
#time_changed = 151

#Keep old columns for checking
focal.nn$focal_old <- focal.nn$focal
focal.nn$nn1_old <- focal.nn$nearest_neighbor
focal.nn$nn2_old <- focal.nn$nn_2

#Fix focal
summary(as.factor(focal.nn$focal.correct))
focal.nn$check.behav <- FALSE
focal.nn$focal_changed <- FALSE
for(i in 1:nrow(focal.nn)){
  #If focal.correct is not empty, do for loop; otherwise skip to next [i]
  if(!is.na(focal.nn$focal.correct[i])){
    #If focal.correct is already the nearest_neighbor or nn_2, inspect to see if behavior should be switched
    if((!is.na(focal.nn$focal[i]) & focal.nn$focal.correct[i] == focal.nn$focal[i]) | 
       (!is.na(focal.nn$nearest_neighbor[i]) & 
        focal.nn$focal.correct[i] == focal.nn$nearest_neighbor[i]) |
       (!is.na(focal.nn$nn_2[i]) & focal.nn$focal.correct[i] == focal.nn$nn_2[i])){
      #If focal.correct is same as nearest_neighbor but not nn2
      if((!is.na(focal.nn$nearest_neighbor[i]) & 
          focal.nn$focal.correct[i] == focal.nn$nearest_neighbor[i]) &
         !(!is.na(focal.nn$nn_2[i]) & focal.nn$focal.correct[i] == focal.nn$nn_2[i])){
        #If activity, tree, tree number, grid all match, doesn't matter - swap anyways 
        if((focal.nn$focal_activity[i] == focal.nn$nn_activity[i] |
            (is.na(focal.nn$focal_activity[i]) & is.na(focal.nn$nn_activity[i]))) &
           (focal.nn$focal_tree[i] == focal.nn$nn_tree[i] |
            (is.na(focal.nn$focal_tree[i]) & is.na(focal.nn$nn_tree[i]))) &
           (focal.nn$focal_tree__[i] == focal.nn$nn_tree__[i] |
            (is.na(focal.nn$focal_tree__[i]) & is.na(focal.nn$nn_tree__[i]))) &
           (focal.nn$focal_grid[i] == focal.nn$nn_grid[i] |
            (is.na(focal.nn$focal_grid[i]) & is.na(focal.nn$nn_grid[i])))){
          focal.nn$nearest_neighbor[i] <- focal.nn$focal[i]
          focal.nn$focal[i] <- focal.nn$focal.correct[i]
          focal.nn$focal_changed[i] <- TRUE
        }
        #If all columns don't match, flag for checking
        else{
          focal.nn$check.behav[i] <- TRUE
        }
      }
      #If focal.correct is same as nn_2 but not nearest neighbor
      else if(!(!is.na(focal.nn$nearest_neighbor[i]) & 
                focal.nn$focal.correct[i] == focal.nn$nearest_neighbor[i]) &
              (!is.na(focal.nn$nn_2[i]) & focal.nn$focal.correct[i] == focal.nn$nn_2[i])){
        #If activity, tree, tree number, grid all match, doesn't matter - swap anyways 
        if((focal.nn$focal_activity[i] == focal.nn$nn2_activity[i] |
            (is.na(focal.nn$focal_activity[i]) & is.na(focal.nn$nn2_activity[i]))) &
           (focal.nn$focal_tree[i] == focal.nn$nn_2_tree[i] |
            (is.na(focal.nn$focal_tree[i]) & is.na(focal.nn$nn_2_tree[i]))) &
           (focal.nn$focal_tree__[i] == focal.nn$nn2_tree_[i] |
            (is.na(focal.nn$focal_tree__[i]) & is.na(focal.nn$nn2_tree_[i]))) &
           (focal.nn$focal_grid[i] == focal.nn$nn2_grid[i] |
            (is.na(focal.nn$focal_grid[i]) & is.na(focal.nn$nn2_grid[i])))){
          focal.nn$nn_2[i] <- focal.nn$focal[i]
          focal.nn$focal[i] <- focal.nn$focal.correct[i]
          focal.nn$focal_changed[i] <- TRUE
        }
        #If all columns don't match, flag for checking
        else{
          focal.nn$check.behav[i] <- TRUE
        }
      }
    }
    #If focal.correct is not already present in the scan, change focal and note that it was changed
    else{
      focal.nn$focal[i] <- focal.nn$focal.correct[i]
      focal.nn$focal_changed[i] <- TRUE
    }
  }
}
summary(focal.nn)
#focal_changed: 196
#check_behav: 17
summary(filter(focal.nn, !is.na(focal.correct)))

#Remove duplicates
focal.nn$rows.duplicated <- FALSE
focal.nn$delete <- NA
for(i in 1:nrow(focal.nn)){
  if(!is.na(focal.nn$delete_if_duplicate[i])){
    obs.i <- focal.nn$observer[i]
    date.i <- focal.nn$date_cor[i]
    time.i <- focal.nn$time_cor[i]
    focal.i <- focal.nn$focal[i]
    focal.nn[focal.nn$observer == obs.i & focal.nn$date_cor == date.i & 
               focal.nn$time_cor == time.i & focal.nn$focal == focal.i,]$rows.duplicated <- TRUE
    rows.i <- filter(focal.nn, observer == obs.i & date_cor == date.i & 
                       time_cor == time.i & focal == focal.i)
    if(length(unique(rows.i$group)) == 1 & length(unique(rows.i$nearest_neighbor)) == 1 & 
       length(unique(rows.i$nn_2)) == 1 &
       length(unique(rows.i$focal_activity)) == 1 & length(unique(rows.i$nn_activity)) == 1 & 
       length(unique(rows.i$nn2_activity)) == 1 &
       length(unique(rows.i$focal_tree)) == 1 & length(unique(rows.i$nn_tree)) == 1 & 
       length(unique(rows.i$nn_2_tree)) == 1 &
       length(unique(rows.i$focal_tree__)) == 1 & length(unique(rows.i$nn_tree__)) == 1 & 
       length(unique(rows.i$nn2_tree_)) == 1 &
       length(unique(rows.i$focal_grid)) == 1 & length(unique(rows.i$nn_grid)) == 1 & 
       length(unique(rows.i$nn2_grid)) == 1 & 
       length(unique(rows.i$sky)) == 1 & length(unique(rows.i$nn_distance)) == 1 & 
       length(unique(rows.i$group_spread)) == 1 & length(unique(rows.i$number_in_group)) == 1){
      focal.nn[focal.nn$observer == obs.i & focal.nn$date_cor == date.i & 
                 focal.nn$time_cor == time.i & focal.nn$focal == focal.i,]$delete[1] <- "KEEP"
      focal.nn[focal.nn$observer == obs.i & focal.nn$date_cor == date.i & 
                 focal.nn$time_cor == time.i & focal.nn$focal == focal.i,]$delete[-1] <- "DELETE"
    }
    else{
      focal.nn[focal.nn$observer == obs.i & focal.nn$date_cor == date.i & 
                 focal.nn$time_cor == time.i,]$delete <- "CHECK"
    }
    rm(obs.i)
    rm(date.i)
    rm(time.i)
    rm(focal.i)
    rm(rows.i)
  }
}
summary(focal.nn)
#rows_duplicated: 217
summary(filter(focal.nn, !is.na(delete_if_duplicate)))
summary(as.factor(focal.nn$delete))
# CHECK DELETE   KEEP   NA's 
#     28     91     98  64875 

#Remove duplicated rows
focal.nn <- filter(focal.nn, delete != "DELETE" | is.na(delete))

#Remove columns
focal.nn$TMM.fixes <- NULL
focal.nn$ML.comments <- NULL
focal.nn$datasheets.check.TMM <- NULL
focal.nn$date.correct <- NULL
focal.nn$time.correct <- NULL
focal.nn$delete_if_duplicate <- NULL

#Make csvs for checking with Becca
check_dupl <- arrange(filter(focal.nn, rows.duplicated == T & delete == "CHECK"), 
                      observer, date_cor, time_cor)
write.csv(check_dupl, "check.dupl.nn_toBecca.csv", na = "", row.names = F)

check_behav <- focal.nn
check_behav$keep <- FALSE
keep <- filter(focal.nn, check.behav == T)
for(i in 1:nrow(keep)){
  obs.i <- keep$observer[i]
  date.i <- keep$date_cor[i]
  check_behav[check_behav$observer == obs.i & check_behav$date_cor == date.i,]$keep <- TRUE
}
rm(obs.i)
rm(date.i)
check_behav <- filter(check_behav, keep == T)
rm(keep)
write.csv(check_behav, "check.behav.nn_toBecca.csv", na = "", row.names = F)


##### Assign focal_id number #####

focal.nn$focal_id_new <- NA
focal.nn	<- focal.nn[order(focal.nn$date, focal.nn$time),]
#focal.nn <- arrange(focal.nn, date_cor, time_cor)  #observer, 
focal.id.counter <- 1
for(i in 1:nrow(focal.nn)){
  if(!is.na(focal.nn$focal_id_new[i]))
    next
  date.i <- focal.nn$date[i]
  animal.i <- focal.nn$focal[i]
  observer.i <- focal.nn$observer[i]
  focal.indicies <- i
  focal.size <- 0
  while(focal.size != length(focal.indicies)){
    focal.size <- length(focal.indicies)
    time.min.i <- min(focal.nn$time[focal.indicies])
    time.max.i <- max(focal.nn$time[focal.indicies])
    focal.indicies <- unique(c(focal.indicies, 
                               which(focal.nn$observer == observer.i & 
                                       focal.nn$date == date.i & 
                                       focal.nn$focal == animal.i & 
                                       focal.nn$time >= (time.min.i - 20*60) & 
                                       focal.nn$time <= (time.max.i + 20*60))))
  }
  focal.nn$focal_id_new[focal.indicies] <- focal.id.counter
  focal.id.counter <- focal.id.counter + 1
  rm(date.i)
  rm(animal.i)
  rm(observer.i)
  rm(time.min.i)
  rm(time.max.i)
}

write.csv(focal.nn, 'Nearest_Neighbor_All_Data_08-02-21WithFocalNumbers.csv')

rm(focal.id.counter)
rm(focal.indicies)
rm(focal.size)


##### Group columns #####

check.group <- focal.nn%>% group_by(focal_id_new) %>% summarise(num.group = length(unique(group))) %>%
  ungroup()
check.group <- filter(check.group, num.group != 1)
filter(focal.nn, focal_id_new %in% check.group$focal_id_new)
focal.nn[focal.nn$focal_id_new == 5933 & focal.nn$focal == "unmarkedmale" & 
           focal.nn$group == "solitary",]$focal_id_new <- max(focal.nn$focal_id_new) + 1
rm(check.group)


##### Create summary file for checking focals #####

focal.ids <- focal.nn%>% group_by(focal_id_new) %>% summarise(observer = unique(observer),
                                                              date = unique(date_cor), 
                                                              group = unique(group),
                                                              start_time = min(time_cor),
                                                              stop_time = max(time_cor), 
                                                              focal_animal = unique(focal),
                                                              num_scans = length(focal_id_new)) %>%
  ungroup()
focal.ids$interval <- lubridate::interval(focal.ids$start_time, focal.ids$stop_time)
sort(table(focal.ids$num_scans))

#Check for overlap between focals
focal.ids$overlap <- NA
for(i in 1:nrow(focal.ids)){
  interval.i <- focal.ids$interval[i]
  obs.i <- filter(focal.ids, observer == focal.ids$observer[i] & date == focal.ids$date[i] & 
                    focal_id_new != focal.ids$focal_id_new[i])
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

#Add to focal.nn
focal.nn <- left_join(focal.nn, focal.ids[,c("focal_id_new", "num_scans")], by = "focal_id_new")
focal.nn$overlap <- ifelse(focal.nn$focal_id_new %in% filter(focal.ids, overlap == T)$focal_id_new, 
                           TRUE, FALSE)

#Check for duplicate lines [same observer, date, time]
check <- focal.nn %>% group_by(observer, date_cor, time_cor) %>% 
  summarize(duplicates = length(unique(unique))) %>% ungroup()
focal.nn <- left_join(focal.nn, check, by = c("observer" = "observer", "date_cor" = "date_cor", 
                                              "time_cor" = "time_cor"))
focal.nn$duplicates <- ifelse(focal.nn$duplicates > 1, TRUE, FALSE)
summary(focal.nn$duplicates)        #401 duplicates; now 70
rm(check)

#Create file for checking data
check.focalid <- focal.nn[,c("unique", "file", "observer", "sb_obs_id", "focal_id", 
                             "group", "focal" , "nearest_neighbor", "nn_2", 
                             "date" ,"year", "month", "time", "approximate_time", "difference", 
                             "date_cor", "date_changed", "time_diff", "time_cor", "time_changed", 
                             "focal_id_new", "num_scans", "overlap", "duplicates")]

#Check for focals without 6 scans
check.focalid$to_check <- FALSE
for(i in 1:nrow(check.focalid)){
  #If focals have the wrong number of scans
  if(check.focalid$num_scans[i] != 6 & check.focalid$num_scans[i] != 12 & 
     check.focalid$num_scans[i] != 18 & check.focalid$num_scans[i] != 24 & 
     check.focalid$num_scans[i] != 30 & check.focalid$num_scans[i] != 36){
    check.focalid$to_check[i] <- TRUE
  }
}

#Only check days where multiple focals have the wrong number of scans
check <- check.focalid %>% filter(., to_check == T) %>% group_by(observer, date_cor) %>% 
  summarize(num.focal.check = length(unique(focal_id_new))) %>% ungroup()
check <- filter(check, num.focal.check != 1)

#Re-do to_check column
check.focalid$to_check <- FALSE
for(i in 1:nrow(check.focalid)){
  observer.i <- check.focalid$observer[i]
  date.i <- check.focalid$date_cor[i]
  time.i <- check.focalid$time_cor[i]
  #If focals have the wrong number of scans
  if(check.focalid$num_scans[i] != 6 & check.focalid$num_scans[i] != 12 & 
     check.focalid$num_scans[i] != 18 & check.focalid$num_scans[i] != 24 & 
     check.focalid$num_scans[i] != 30 & check.focalid$num_scans[i] != 36){
    #if multiple focals in the day have the wrong number of scans
    if(nrow(filter(check, observer == observer.i & date_cor == date.i)) > 0){
      check.focalid$to_check[i] <- TRUE
    }
  }
  #If only one scan
  if(check.focalid$num_scans[i] == 1){
    check.focalid$to_check[i] <- TRUE
  }
  #If overlap
  if(check.focalid$overlap[i] == T){
    check.focalid$to_check[i] <- TRUE
  }
  #If duplicate
  if(check.focalid$duplicates[i] == T){
    check.focalid$to_check[i] <- TRUE
  }
  rm(observer.i)
  rm(date.i)
  rm(time.i)
}
summary(check.focalid$to_check)   #to check: 3082;  now 936
rm(check)

#Save for checking
write.csv(check.focalid, "check.focalid.nn_tmm2.csv", na = "", row.names = F)


##### Scan-level columns #####

#Clean sky column
focal.nn$sky <- gsub(" ", "", focal.nn$sky)
sky <- focal.nn %>% group_by(focal_id_new) %>% summarise(num = length(unique(sky)))
nrow(filter(sky, num != 1))     #1205 - done by scan
rm(sky)
summary(as.factor(focal.nn$sky))
#  clear       cloudy mostlycloudy partlycloudy         rain         NA's 
#  52620         2832          253         8954          341            1 

#Combine "mostly cloudy" with "partly cloudy"
focal.nn[focal.nn$sky == "mostlycloudy" & !is.na(focal.nn$sky),]$sky <- "partlycloudy"
summary(as.factor(focal.nn$sky))

#Add temperature column
focal.nn$temperature <- NA

#Clean group_spread column
focal.nn$group_spread_old <- gsub(" ", "", focal.nn$group_spread)
focal.nn$group_spread <- as.numeric(focal.nn$group_spread_old)
summary(focal.nn$group_spread)
group_spread <- focal.nn %>% group_by(focal_id_new) %>% summarise(num = length(unique(group_spread)))
nrow(filter(group_spread, num != 1))     #9055 - done by scan
rm(group_spread)

#Fix errors in group_spread column
focal.nn[focal.nn$group_spread != focal.nn$group_spread_old & !is.na(focal.nn$group_spread) & 
           !is.na(focal.nn$group_spread_old),]
focal.nn[!is.na(focal.nn$group_spread_old) & is.na(focal.nn$group_spread),
         c("focal_id_new", "group_spread", "group_spread_old")]
arrange(filter(focal.nn, focal_id_new == 5870 | focal_id_new == 6006 | focal_id_new == 7777), 
        focal_id_new, date_cor, time_cor)[,c("focal_id_new", "group_spread", "group_spread_old")]
#  focal_id_new group_spread group_spread_old
#          5870           NA               2& - & is probably 4 (based on iPad) = 24, surrounding group_spread = 18, 13 -> think 24
#          6006           NA               6@ - @ is probably 1 (based on iPad) = 61, surround group_spread =  5, 25 -> think 6
#          7777           NA              m40 - same as grid, likely not group_spread
focal.nn[!is.na(focal.nn$group_spread_old) & focal.nn$group_spread_old == "2&" & 
           is.na(focal.nn$group_spread),]$group_spread <- 24
focal.nn[!is.na(focal.nn$group_spread_old) & focal.nn$group_spread_old == "6@" & 
           is.na(focal.nn$group_spread),]$group_spread <- 6

#Clean number in group column
focal.nn$number_in_group_old <- gsub(" ", "", focal.nn$number_in_group)
focal.nn$number_in_group <- as.numeric(focal.nn$number_in_group_old)
summary(focal.nn$number_in_group)
number_in_group <- focal.nn %>% group_by(focal_id_new) %>% summarise(num = length(unique(number_in_group)))
nrow(filter(number_in_group, num != 1))     #81 - done by scan??
rm(number_in_group)

#Fix errors in number_in_group column
focal.nn[focal.nn$number_in_group != focal.nn$number_in_group_old & 
           !is.na(focal.nn$number_in_group) & !is.na(focal.nn$number_in_group_old),]
focal.nn[is.na(focal.nn$number_in_group) & !is.na(focal.nn$number_in_group_old),
         c("focal_id_new", "number_in_group", "number_in_group_old")]
arrange(filter(focal.nn, focal_id_new == 7704 | focal_id_new == 7708), 
        focal_id_new, date_cor, time_cor)[,c("focal_id_new", "number_in_group", "number_in_group_old")]
#    focal_id_new number_in_group number_in_group_old
# 1          7704              NA                  4"
# 2          7704              NA                  4"
# 3          7704              NA                  4"
# 4          7704              NA                  4"
# 5          7704              NA                  4"
# 6          7704              NA                  4"
# 7          7708              NA                  4"
# 8          7708               4                   4
# 9          7708               4                   4
# 10         7708               4                   4
# 11         7708               4                   4
# 12         7708               4                   4
focal.nn[!is.na(focal.nn$number_in_group_old) & is.na(focal.nn$number_in_group),]$number_in_group_old
focal.nn[!is.na(focal.nn$number_in_group_old) & is.na(focal.nn$number_in_group),]$number_in_group <- 4

#Clean change in group column
focal.nn$change_in_group_old <- gsub(" ", "", focal.nn$change_in_group_)
focal.nn$change_in_group_ <- as.logical(as.numeric(focal.nn$change_in_group_old))
summary(as.factor(focal.nn$change_in_group_))     #398
summary(as.factor(focal.nn$change_in_group_old))     #398

#Clean nn distance column
focal.nn$nn_distance_old <- gsub("\\?\\?\\?", "", focal.nn$nn_distance)
focal.nn$nn_distance_old <- gsub(" ", "", focal.nn$nn_distance_old)
focal.nn$nn_distance <- as.numeric(focal.nn$nn_distance_old)
summary(focal.nn$nn_distance)

#Fix errors in nn_distance column
focal.nn[focal.nn$nn_distance != focal.nn$nn_distance_old & !is.na(focal.nn$nn_distance) & 
           !is.na(focal.nn$nn_distance_old),]
focal.nn[is.na(focal.nn$nn_distance) & !is.na(focal.nn$nn_distance_old),
         c("unique", "focal_id_new", "nn_distance", "nn_distance_old")]
check <- focal.nn[is.na(focal.nn$nn_distance) & !is.na(focal.nn$nn_distance_old),]$unique

arrange(filter(focal.nn, unique %in% check), focal_id_new, date_cor, time_cor)
#       focal_id_new nn_distance nn_distance_old
# 30586         5093          NA               a
# 37180         5999          NA             h17 - already have location_grid
# 40621         6560          NA              b9 - already have location_grid
# 45067         7250          NA             g37 - already have location_grid
# 47559         7669          NA               /
# 63322        10167          NA               o
# 63856        10256          NA               o

#Check cleaned_nn_dist
focal.nn[focal.nn$nn_distance != focal.nn$cleaned_nndist & !is.na(focal.nn$nn_distance) & 
           !is.na(focal.nn$cleaned_nndist),]
focal.nn[is.na(focal.nn$nn_distance) & !is.na(focal.nn$cleaned_nndist),
         c("unique", "focal_id_new", "nn_distance", "cleaned_nndist")]

#Check nn_within_group
focal.nn$nn_within_group <- gsub(" ", "", focal.nn$nn_within_group)
focal.nn[focal.nn$nearest_neighbor != focal.nn$nn_within_group & 
           !is.na(focal.nn$nearest_neighbor) & !is.na(focal.nn$nn_within_group) & 
           focal.nn$focal_changed == FALSE,]    #okay - all packy/zacky

#Combine old focal_id columns
focal.nn$sb_obs_id <- gsub(" ", "", focal.nn$sb_obs_id)
summary(as.factor(focal.nn$sb_obs_id))
focal.nn[focal.nn$sb_obs_id == "0" & !is.na(focal.nn$sb_obs_id),]$sb_obs_id <- NA

focal.nn$focal_id <- gsub(" ", "", focal.nn$focal_id)
summary(as.factor(focal.nn$focal_id))

focal.nn$focal_id_old <- paste(focal.nn$focal_id, focal.nn$sb_obs_id, sep = ';')
focal.nn$focal_id_old <- gsub("NA;", "", focal.nn$focal_id_old)
focal.nn$focal_id_old <- gsub(";NA", "", focal.nn$focal_id_old)
focal.nn[!is.na(focal.nn$focal_id_old) & 
           focal.nn$focal_id_old == "NA",]$focal_id_old <- NA

#Combine social_data columns
focal.nn$social_not_collect <- paste0("not_collected:", focal.nn$social_not_collect)
focal.nn$no_social_data <- paste0("no_data:", focal.nn$no_social_data)
focal.nn$yes_social_data <- paste0("yes_data:", focal.nn$yes_social_data)
focal.nn$social_data <- paste(focal.nn$social_not_collect, focal.nn$no_social_data, 
                              focal.nn$yes_social_data, sep = "; ")

#Combine comments columns
focal.nn$exclude_for_analysis <- ifelse(!is.na(focal.nn$exclude_for_analysis), 
                                        paste0("exclude_for_analysis:", focal.nn$exclude_for_analysis), 
                                        focal.nn$exclude_for_analysis)
focal.nn$comments_all <- paste(focal.nn$comments, focal.nn$cleaning_comments, 
                               focal.nn$exclude_for_analysis, sep = ";")
focal.nn$comments_all <- gsub(";NA", "", focal.nn$comments_all)
focal.nn$comments_all <- gsub("NA;", "", focal.nn$comments_all)
focal.nn[!is.na(focal.nn$comments_all) & 
           focal.nn$comments_all == "NA",]$comments_all <- NA


##### Focal columns #####

#Combine all scanees into one dataset
focal_scans <- focal.nn[,c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                           "focal", "focal_activity", "focal_tree", "focal_tree__", "focal_grid")]
colnames(focal_scans) <- c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                           "animal_id", "activity", "tree_species", "tree_number", "location_grid")
focal_scans$type <- "focal_animal"

nn_scans <- focal.nn[,c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                        "nearest_neighbor", "nn_activity", "nn_tree", "nn_tree__", "nn_grid")]
colnames(nn_scans) <- c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                        "animal_id", "activity", "tree_species", "tree_number", "location_grid")
nn_scans$type <- "nearest_neighbor"

nn2_scans <- focal.nn[,c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                         "nn_2", "nn2_activity", "nn_2_tree", "nn2_tree_", "nn2_grid")]
colnames(nn2_scans) <- c("unique", "focal_id_new", "observer", "date_cor", "time_cor", 
                         "animal_id", "activity", "tree_species", "tree_number", "location_grid")
nn2_scans$type <- "nn_2"

scans <- rbind(focal_scans, nn_scans, nn2_scans)

#Remove blanks
scans <- filter(scans, !(is.na(animal_id) & is.na(activity) & is.na(tree_species) & 
                           is.na(tree_number) & is.na(location_grid)))    #remove 65128 lines
scans <- unique(scans)     #remove 0
summary(as.factor(scans$type))

#Find animals in wrong places
unique(scans[scans$activity %in% animals$name | scans$tree_species %in% animals$name | 
               scans$tree_number %in% animals$name | scans$location_grid %in% animals$name | 
               scans$distance %in% animals$name | scans$actual_distance %in% animals$name,])

check <- scans[scans$activity %in% animals$name | scans$tree_species %in% animals$name | 
                 scans$tree_number %in% animals$name | scans$location_grid %in% animals$name | 
                 scans$distance %in% animals$name | scans$actual_distance %in% animals$name,
               c("unique")]

#Find activities and tree_species with numbers 
unique(scans[grepl("[[:digit:]]", scans$activity),
             c("unique", "activity")])     #ok

unique(scans[grepl("[[:digit:]]", scans$tree_species),
             c("unique","tree_species")])     #locations or tree numbers

#Find locations in wrong places
unique(scans[grepl("[[:alpha:]]", scans$animal_id) & grepl("[[:digit:]]", scans$animal_id),
             c("unique","animal_id", "location_grid")])     #ok
unique(scans[grepl("[[:alpha:]]", scans$activity) & grepl("[[:digit:]]", scans$activity),
             c("unique","activity", "location_grid")])     #ok
unique(scans[grepl("[[:alpha:]]", scans$tree_species) & grepl("[[:digit:]]", scans$tree_species),
             c("unique","tree_species", "location_grid")])     #ok
unique(scans[grepl("[[:alpha:]]", scans$tree_number) & grepl("[[:digit:]]", scans$tree_number) & 
               !grepl("-", scans$tree_number) & !grepl("s", scans$tree_number),
             c("unique","tree_number", "location_grid")])     #ok

check <- c(check, scans[grepl("[[:alpha:]]", scans$tree_number) & grepl("[[:digit:]]", scans$tree_number) & 
                          !grepl("-", scans$tree_number) & !grepl("s", scans$tree_number),
                        c("unique")])

check <- unique(check)     #0 lines
#check <- data.frame(unique = check, tocheck = TRUE)
#check.nn <- filter(focal.nn, unique %in% check$unique)
#write.csv(check.nn, "check.nn.scans.tmm.csv", na = "", row.names = F)

#Separate food_type from activity
scans$food_type <- NA
for(i in 1:nrow(scans)){
  if(grepl("feed", scans$activity[i])){
    scans$food_type[i] <- scans$activity[i]
    scans$activity[i] <- "feed"
  }
}

#Fix food_type
scans$food_type <- gsub("feed", "", scans$food_type)
scans$food_type <- gsub(" ", "", scans$food_type)
scans[scans$food_type == "matureleaves" & !is.na(scans$food_type),]$food_type <- "mature leaves"
scans[scans$food_type == "youngleaves" & !is.na(scans$food_type),]$food_type <- "young leaves"
scans[scans$food_type == "newleaves" & !is.na(scans$food_type),]$food_type <- "young leaves"
scans[scans$food_type == "caterpillar" & !is.na(scans$food_type),]$food_type <- "caterpillars"
unique(filter(scans, !(food_type %in% foodtype.list$food_type))$food_type)
# [1] NA         "branches" - ASK BECCA

#Fix activity
scans[scans$activity == "no value" & !is.na(scans$activity),]$activity <- NA
scans[(scans$activity == "groom" | scans$activity == "invite to groom" | scans$activity == "nose jab" | 
         scans$activity == "greet" | scans$activity == "chatter") & 
        !is.na(scans$activity),]$activity <- "social"

scans[(scans$activity == "baby" | scans$activity == "nurse") & !is.na(scans$activity),]
# nurse are all nancy in 2019 (adult) -> nurse = rest
# baby should be in id column -> think its walrusbaby2020 bc chloe doesn't have a 2020 baby
scans[scans$activity == "nurse" & !is.na(scans$activity),]$activity <- "rest"
scans[scans$activity == "baby" & is.na(scans$animal_id) & !is.na(scans$activity),]$animal_id <- "wapiti"
scans[scans$activity == "baby" & !is.na(scans$activity),]$activity <- NA

unique(filter(scans, !(activity %in% activity.list$activity))$activity)
# [1] "tail curl"    NA             "breefak call" "sifaak call" - ASK BECCA

#Fix tree species
sort(table(scans$tree_species), decreasing = T)
unique(scans$tree_species)

#Fix tree number
sort(table(scans$tree_number), decreasing = T)
scans[!is.na(scans$tree_number) & grepl("\\?", scans$tree_number) & 
        !grepl("s", scans$tree_number),]$tree_number
scans[!is.na(scans$tree_number) & grepl("\\?", scans$tree_number) & 
        !grepl("s", scans$tree_number),]$tree_number <- NA
unique(scans$tree_number)



##### Fix location #####

grep("<.*>", scans$location_grid, value = T)
scans$location_grid <- gsub("<.*>", "", scans$location_grid)

#Add column for direction off_grid
scans$off_grid_direction <- NA
for(i in 1:nrow(scans)){
  if(grepl("east", scans$location_grid[i]) | grepl("est", scans$location_grid[i])){
    scans$off_grid_direction[i] <- "east"
  }
  if(grepl("west", scans$location_grid[i]) | grepl("ouest", scans$location_grid[i]) ){
    scans$off_grid_direction[i] <- "west"
  }
  if(grepl("north", scans$location_grid[i]) | grepl("nord", scans$location_grid[i])){
    scans$off_grid_direction[i] <- "north"
  }
  if(grepl("south", scans$location_grid[i]) | grepl("sud", scans$location_grid[i])){
    scans$off_grid_direction[i] <- "south"
  }
}

#Fix others - based on Meredith's knowledge
scans[scans$location_grid == "out of the grid r41" & 
        !is.na(scans$location_grid),]$off_grid_direction <- "north"
scans[scans$location_grid == "out of the grid system r41" & 
        !is.na(scans$location_grid),]$off_grid_direction <- "north"

#Replace ' with _1 (prime grids)
scans$location_grid_old <- gsub("'", "_1", scans$location_grid)
scans$location_grid_old <- gsub("'", "_1", scans$location_grid)

#Match locations exactly with location.list
scans$location <- NA
scans$location <- ifelse(scans$location_grid_old %in% location.list$location, scans$location_grid_old, NA)

#Pull out off_grid locations
scans[scans$location_grid_old == "out of the grid" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "out of grid" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "hors du grid" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "hors du grille " & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "north" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "west" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "further" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "south" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "to the east" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "to the west" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "north of the grid" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"
scans[scans$location_grid_old == "east" & 
        !is.na(scans$location_grid_old) & is.na(scans$location),]$location <- "off_grid"

#Divide scans into solved (keep) vs unsolved (fix)
scans$num_matches <- NA
scans.fix <- filter(scans, is.na(location) & !is.na(location_grid))
scans.keep <- filter(scans, !(is.na(location) & !is.na(location_grid)))

#Figure out correct location for unsolved scans - match complete phrase (using //b)
location.list$location.grep <- paste0('\\b', location.list$location, '\\b')
scans.fix$num_matches <- 0
for(i in 1:nrow(location.list)){
  matches <- grepl(pattern = location.list$location.grep[i], x = scans.fix$location_grid_old)
  scans.fix$num_matches <- scans.fix$num_matches + as.numeric(matches) ## document if it somehow matches multiple locations
  scans.fix$location[matches] <- location.list$location[i]
}
rm(matches)

#Combine solved scans with past solved scans
### Consider a scan solved when it has a direction and a location [otherwise location should have matched exactly]
scans.fix.good <- filter(scans.fix, (!is.na(off_grid_direction) & !is.na(location) & num_matches == 1))
scans.keep <- rbind(scans.keep, scans.fix.good)

scans.fix <- filter(scans.fix, !(!is.na(off_grid_direction) & !is.na(location) & num_matches == 1))

#Revisit unsolved scans
# Check for primes - assume many typos due to computational interpretation of ' - assign primes to those
locations.prime <- filter(location.list, grepl("_1", location.list$location))
locations.prime$location.prime <- gsub("_1", "", locations.prime$location)
scans.fix$num_matches <- 0
for(i in 1:nrow(locations.prime)){
  matches <- grepl(pattern = locations.prime$location.prime[i], x = scans.fix$location_grid_old)
  scans.fix$num_matches <- scans.fix$num_matches + as.numeric(matches) ## document if it somehow matches multiple locations
  scans.fix$location[matches] <- locations.prime$location[i]
}
rm(matches)

#Combine solved scans with past solved scans
### Consider a scan solved when it has a location and no off_grid direction
scans.fix.good <- filter(scans.fix, (is.na(off_grid_direction) & !is.na(location) & num_matches == 1))
unique(scans.fix.good[,c("location", "off_grid_direction", "location_grid_old")])
scans.keep <- rbind(scans.keep, scans.fix.good)

#Create dataset for checking remaining scans by hand
scans.fix <- filter(scans.fix, !(is.na(off_grid_direction) & !is.na(location) & num_matches == 1))     #85 left

#List of locations for that scan
fix.scan.locations <- scans.keep %>% filter(., unique %in% scans.fix$unique) %>% 
  group_by(unique, location) %>% summarise(num.location = length(location)) %>% ungroup()
fix.focal.locations <- scans.keep %>% filter(., focal_id_new %in% scans.fix$focal_id_new) %>% 
  group_by(focal_id_new, location) %>% summarise(num.location = length(location)) %>% ungroup()

#Add columns for other locations for that scan
scans.fix$scan.location <- NA
scans.fix$focal.location <- NA
for(i in 1:nrow(scans.fix)){
  unique.i <- scans.fix$unique[i]
  focal.i <- scans.fix$focal_id_new[i]
  #Locations at that scan
  unique.locs <- filter(fix.scan.locations, unique == unique.i)
  if(nrow(unique.locs) == 1){
    scans.fix$scan.location[i] <- unique.locs$location
  }
  else if(nrow(unique.locs) > 1){
    scans.fix$scan.location[i] <- unique.locs[which.max(unique.locs$num.location),]$location
  }
  rm(unique.locs)
  #Locations in that focal
  focal.locs <- filter(fix.focal.locations, focal_id_new == focal.i)
  if(nrow(focal.locs) == 1){
    scans.fix$focal.location[i] <- focal.locs$location
  }
  else if(nrow(focal.locs) > 1){
    scans.fix$focal.location[i] <- paste(focal.locs$location, collapse = ",")
  }
  rm(focal.locs)
  
}
rm(fix.focal.locations)
rm(fix.scan.locations)
rm(unique.i)
rm(focal.i)

#Fix by hand
write.csv(scans.fix, "nn.scans.fix.csv", na = "", row.names = F)

#Eyeball location check
unique(scans.keep[!(scans.keep$location == scans.keep$location_grid),c("location", "off_grid_direction", "location_grid")])


##### Create final datasets #####

#Focal samples table
focals_table <- focal.ids
focals_table <- focals_table[,c("focal_id_new", "observer", "date", "group", "focal_animal", 
                                "start_time", "stop_time")]
colnames(focals_table) <- c("focal_id", "observer", "date", "group", "focal_animal", 
                            "focal_start_time", "focal_stop_time")
focals_table$focal_start_time <- focals_table$focal_start_time - 10*60 #subtract 10 minutes
focals_table$focal_id <- paste("nn", focals_table$focal_id, sep = "")
focals_table <- arrange(focals_table, observer, date, focal_start_time, focal_id)

#Scans table - new dataset
scans_table <- rbind(scans.keep, scans.fix[,c(1:16)])
scans_table <- scans_table[,c("unique", "focal_id_new", "type", "animal_id", "activity", "food_type", 
                              "tree_species", "tree_number", "location", "off_grid_direction", 
                              "location_grid")]
colnames(scans_table) <- c("unique", "focal_id_new", "type", "animal", "activity", "foodtype", 
                           "treespecies", "treenumber", "grid", "offgrid_direction", "grid_old")
scans_table[scans_table$type == "focal_animal",]$type <- "focal"
scans_table[scans_table$type == "nearest_neighbor",]$type <- "nn"
scans_table[scans_table$type == "nn_2",]$type <- "nn2"

#Make scans table
scans_table <- scans_table %>% 
  pivot_wider(names_from = type, 
              names_glue = "{type}_{.value}",
              values_from = c("animal", "activity", "foodtype", 
                              "treespecies", "treenumber", "grid", "offgrid_direction", "grid_old"))
head(focal.nn[,c(11,15,17:24,30:34,57)])
scans_table <- left_join(scans_table, focal.nn[,-c(11,15,17:24,30:34,57)], by = "unique")
scans_table$focal_id_new <- paste("nn", scans_table$focal_id_new, sep = "")
scans_table <- arrange(scans_table, observer, date, focal_id_new)

scans_table_new <- scans_table[,c("unique", "focal_id_new", "observer", "group", 
                                  "date_cor", "time_cor", 
                                  "sky", "group_spread", "number_in_group",
                                  "focal_animal", "focal_activity", "focal_foodtype", 
                                  "focal_treespecies", "focal_treenumber", 
                                  "focal_grid", "focal_offgrid_direction", 
                                  "nn_animal", "nn_activity", "nn_foodtype", 
                                  "nn_treespecies", "nn_treenumber", 
                                  "nn_grid", "nn_offgrid_direction", "nn_distance",
                                  "nn2_animal", "nn2_activity", "nn2_foodtype", 
                                  "nn2_treespecies", "nn2_treenumber", 
                                  "nn2_grid", "nn2_offgrid_direction", 
                                  "focal_id_old", "social_data", "comments_all")]
colnames(scans_table_new) <- c("unique", "focal_id", "observer", "group", 
                               "date", "time", 
                               "sky", "group_spread", "number_in_group",
                               "focal_animal", "focal_activity", "focal_foodtype", 
                               "focal_treespecies", "focal_treenumber", 
                               "focal_grid", "focal_offgrid_direction", 
                               "nn_animal", "nn_activity", "nn_foodtype", 
                               "nn_treespecies", "nn_treenumber", 
                               "nn_grid", "nn_offgrid_direction", "nn_distance",
                               "nn2_animal", "nn2_activity", "nn2_foodtype", 
                               "nn2_treespecies", "nn2_treenumber", 
                               "nn2_grid", "nn2_offgrid_direction", 
                               "focal_id_old", "social_data", "comments_all")

colnames(scans_table)
scans_table_full <- scans_table[,c("unique", "focal_id_new", "observer", "group", 
                                   "date_cor", "date", "date_changed", 
                                   "time_cor", "time", "approximate_time", "time_changed",
                                   "sky", "group_spread", "number_in_group", "change_in_group_", 
                                   "focal_animal", "focal_changed", "focal_activity", "focal_foodtype", 
                                   "focal_treespecies", "focal_treenumber", 
                                   "focal_grid", "focal_offgrid_direction", 
                                   "nn_animal", "nn_activity", "nn_foodtype", 
                                   "nn_treespecies", "nn_treenumber", 
                                   "nn_grid", "nn_offgrid_direction", "nn_distance",   
                                   "nn2_animal", "nn2_activity", "nn2_foodtype", 
                                   "nn2_treespecies", "nn2_treenumber", 
                                   "nn2_grid", "nn2_offgrid_direction", 
                                   "file", "focal_id_old", "social_data", "comments_all", 
                                   "focal_old", "focal_grid_old", 
                                   "nn1_old", "nn_grid_old", 
                                   "nn2_old","nn2_grid_old")]

colnames(scans_table_full) <- c("unique", "focal_id", "observer", "group", 
                                "date", "date_old", "date_changed", 
                                "time", "time_old", "approximate_time_old", "time_changed",
                                "sky", "group_spread", "number_in_group", "change_in_group", 
                                "focal_animal", "focal_changed", "focal_activity", "focal_foodtype", 
                                "focal_treespecies", "focal_treenumber", 
                                "focal_grid", "focal_offgrid_direction", 
                                "nn_animal", "nn_activity", "nn_foodtype", 
                                "nn_treespecies", "nn_treenumber", 
                                "nn_grid", "nn_offgrid_direction", "nn_distance", 
                                "nn2_animal", "nn2_activity", "nn2_foodtype", 
                                "nn2_treespecies", "nn2_treenumber", 
                                "nn2_grid", "nn2_offgrid_direction", 
                                "file_old", "focal_id_old", "social_data", "comments_all", 
                                "focal_animal_old", "focal_grid_old", 
                                "nn1_old", "nn_grid_old", 
                                "nn2_old", "nn2_grid_old")

write.csv(focals_table, "table_focal.ids.nn_tmm.mcl_26apr2021.csv", na = "", row.names = F)

write.csv(scans_table_new, "focal.nn_updated.tmm.mcl_26apr2021.csv", na = "", row.names = F)

write.csv(scans_table_full, "focal.nn_updated.tmm.mcl_26apr2021_allcolumns.csv", na = "", row.names = F)


