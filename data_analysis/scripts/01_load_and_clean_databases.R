




#########################################################################
#########################################################################
#                     LOAD AND CLEAN FISHERIES DATA                     #
#########################################################################
#########################################################################







# Landing surveys were conducted twice a week (Tuesdays and Fridays) by trained 
# fishers and traders from the studied coastal communities. Interviewers
# surveyed fishers at landing. Information from landing surveys is divided in 
# the following databases:

#   "tripinfo" <- General information of the fishing trip (i.e., n fishers, 
#                  time at the sea, litres of fuel...)
#   "effort"   <- Effort indicators and catch composition of each of the métiers
#                  (gears) used on each trip (e.g., number of hooks, length of 
#                  net, catch composition, etc.).
#   "catch"    <- Weight, number of fish and price of each of the species caught  
#                  on each trip.



# Information on species:
#   "species"  <- Information on each of the species, retrieved from fishbase.



# Number of fishing trips are counted by extension workers at the end of each 
# sampling day, disaggregating across several fishing categories:
#   "ntrips"   <- Counts of fishing trip on several fishing categories.








#########################################################################
#                     LOAD (AND INSTALL) PACKAGES                       #
#########################################################################



# Data manipulation
if(!require("dplyr"))   install.packages("dplyr",   repos = "https://cloud.r-project.org")
library(dplyr)



#########################################################################
#                           LOAD DATABASES                              #
#########################################################################


ntrips <- read.csv(file.path(getwd(),"data","ntrips.csv"))
catch    <-  read.table(file.path(getwd(),"data","catch.csv"),    header=TRUE, sep = ",")
effort   <-  read.table(file.path(getwd(),"data","effort.csv"),   header=TRUE, sep = ",")
tripinfo <-  read.table(file.path(getwd(),"data","tripinfo.csv"), header=TRUE, sep = ",")
species  <-  read.table(file.path(getwd(),"data","species.csv"),  header=TRUE, sep = ",")


### REMOVE ROWS WITHOUT TRIP ID INFORMATION
catch   <- catch[!is.na(catch$trip),]
ntrips <- ntrips[-which(is.na(ntrips$date) | 
                        ntrips$site %in% c("PP","MM","AG","SP","NO","SB") |  
                        as.character(ntrips$island) == ""),]







#########################################################################
#                           CLEAN DATABASES                             #
#########################################################################




# Function to convert multiple-response variables into a list
tolist <- function(splist)
  lapply(splist, function(x){
    commas = unlist(gregexpr(', ', x))
    return(  unlist(lapply(c(1:length(commas)), function(y){
      if(y == 1) {
        liv = substr(x,1,commas[y]-1)
      } else {
        liv = substr(x,commas[y-1]+2,commas[y]-1)
      }
    }))
    )})


effort$catch <- paste0(effort$catch,", ")









#########################################################################
#      TAG SPECIES CAUGHT WITH MORE THAN ONE GEAR IN THE SAME TRIP      #
#########################################################################





# If a species has been caught with more than one gear in the same trip, it is not possible
# to know how much of that species has been caught with one gear and how much has been caught
# with the other gear. Thus, those gears with the same species in the same trip are excluded
# from calculations of A) total weight, B) total nfish and C) Mean Trophic Level.

### STEP 1: TAG MÉTIERS THAT CAUGHT THE SAME SPECIES IN THE SAME TRIP
effort$species_effort_id <- unlist(lapply(c(1:nrow(effort)),function(i){
  gsub(", ",    paste0("#",effort$effort_id[i],", "),    effort$catch[i])
}))

effort$species_trip <- unlist(lapply(c(1:nrow(effort)),function(i){
  gsub(", ",    paste0(effort$trip[i],", "),    effort$catch[i])
}))

species_trip      <- unlist(tolist(effort$species_trip))
species_effort_id <- unlist(tolist(effort$species_effort_id))
specs             <- substr(species_effort_id,1,unlist(gregexpr('#', species_effort_id))-1)
effort_id         <- substr(species_effort_id,unlist(gregexpr('#', species_effort_id))+1,nchar(species_effort_id))
valid             <- unlist(lapply(species_trip,function(x)   length(which(species_trip == x))))-1
valid[which(specs == "NAO-APANHOU")] <- 0
valid[which(specs == "NA")] <- 1
temp <- data.frame(effort_id = effort_id,valid = valid)
temp <- as.data.frame(temp   %>%   group_by(effort_id)   %>%  summarise_all(sum)   %>%   ungroup)
effort <- effort    %>%   inner_join(temp,by=c("effort_id"))
effort$valid[which(effort$valid>0)] <- NA
effort$valid[which(effort$valid==0)] <- 1






### STEP 2: LINK CATCH AND EFFORT DATA (ADD "effort_id" TO CATCH DATASET WHEN VALID = 1)
temp <- data.frame(species_trip = species_trip, valid = valid)
temp <- as.data.frame(temp   %>%   group_by(species_trip)   %>%  summarise_all(sum)   %>%   ungroup)
temp <- temp    %>%   inner_join(data.frame(species_trip = species_trip, effort_id = effort_id), by=c("species_trip"))
temp$effort_id[which(!temp$valid == 0)] <- NA
temp <- as.data.frame(temp   %>%   group_by(species_trip)   %>%  summarise_all(max)   %>%   ungroup)


catch$species_trip <- paste0(catch$species, catch$trip)
catch              <- catch  %>%   inner_join(temp[c("effort_id","species_trip")],by=c("species_trip"))





rm(effort_id,species_effort_id,species_trip,specs,valid,temp)
