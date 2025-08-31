




#########################################################################
#########################################################################
#                          VARIABLE DEFINITION                          #
#########################################################################
#########################################################################



# This script calculates diversity indicators by métier ("effort") and trip 
# ("tripinfo") and adds them to their corresponding databases.




# Data manipulation
if(!require("vegan"))   install.packages("vegan",   repos = "https://cloud.r-project.org")
library(vegan)
if(!require("dplyr"))   install.packages("dplyr",   repos = "https://cloud.r-project.org")
library(dplyr)





### RUN SCRIPTS 1A AND 1B
# 1A -> LOADS & CLEANS DATA
# 1B -> ADDS COVARIATES TO CATCH AND EFFORT
#source(file.path("data_analysis","scripts","02A_variable_definition.R"))



ntrips <- read.csv(file.path(getwd(),"data_analysis", "data_with_covariates","ntrips.csv"))
catch    <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","catch.csv"),    header=TRUE, sep = ",")
effort   <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","effort.csv"),   header=TRUE, sep = ",")
tripinfo <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","tripinfo.csv"), header=TRUE, sep = ",")
species  <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","species.csv"),  header=TRUE, sep = ",")



startyear              <- c("2019-01-01","2020-01-01","2021-01-01","2022-01-01","2023-01-01")
startyear              <- as.integer(as.POSIXct(startyear,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)


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



#########################################################################
#---------------  CREATE gear / trip VS species MATRICES  --------------#
#########################################################################






### STEP 1: CREATE VECTOR WITH SPECIES NAMES
specs   <- levels(as.factor(catch$species))



### STEP 2: FUNCTION THAT CREATES gear / trip VS species MATRIX
sptrip_matrix <- function(rw,variable){
  catch          = catch
  catch$rw       = catch[,which(colnames(catch)==rw)]
  catch$variable = catch[,which(colnames(catch)==variable)]
  if(rw == "effort_id") {rws = levels(as.factor(effort$effort_id))} else  {rws = levels(as.factor(tripinfo$trip))}
  temp = as.data.frame(expand.grid(
    rw      = rws, 
    species  = levels(as.factor(catch$species)),
    variable = as.numeric(0)
  ))
  temp = rbind(temp, data.frame(rw = catch$rw,
                                species=catch$species,
                                variable = catch$variable))
  temp$species = as.character(temp$species)
  temp$rw      = as.character(temp$rw)
  temp$temp    = paste0(temp$species,temp$rw)
  temp = temp %>%   group_by(temp) %>%   summarise_all(max) %>%   ungroup
  temp = as.data.frame(temp)[c("rw","species","variable")]
  output = data.frame(rw = levels(as.factor(temp$rw)))
  for(i in 1:length(specs)){
    output = output %>%
      inner_join(temp[which(temp$species == specs[i]),-which(colnames(temp)=="species")],
                 by=c("rw"))}
  rownames(output) = output$rw
  colnames(output) = c(rw,specs)
  return(output)}



### STEP 4: CREATE gear / trip VS species MATRIX OF WEIGHT
# "effort_id" is a unique code given to each metier on each trip.
trip_weight <- sptrip_matrix(rw="trip",variable="weight")
gear_weight <- sptrip_matrix(rw="effort_id",variable="weight")



#########################################################################
#-----------  CALCULATE DIVERSITY INDICATORS OF effort DATA  -----------#
#########################################################################


# Sum weight
temp <- data.frame(effort_id = gear_weight$effort_id,  weight = gear_weight[,c(2:ncol(gear_weight))] %>% rowSums(.)) 
effort <- effort %>% inner_join(temp,   by=c("effort_id"))


# Species richness
effort$richness <- unlist(lapply(tolist(effort$catch),function(x){length(x)}))
effort$richness[which(effort$catch== "NAO-APANHOU, ")] <- 0




#########################################################################
#----------  CALCULATE DIVERSITY INDICATORS OF tripinfo DATA  ----------#
#########################################################################




### Weight
temp <- data.frame(trip = trip_weight$trip,  weight = trip_weight[,c(2:ncol(trip_weight))] %>% rowSums(.)) 
tripinfo <- tripinfo %>% inner_join(temp,   by=c("trip"))
tripinfo$weight[which(tripinfo$catch == "nao")] <- 0


### Richness
tripinfo$richness <- unlist(lapply(tripinfo$trip,function(trip){length(which(catch$trip == trip))}))
tripinfo$richness[which(tripinfo$participation == "nao")] <- NA


rm(evenness,gear_MTL,gear_number, gear_weight, temp, trip_MTL,trip_number,trip_weight,
   invsimpson, specs, calculateMTL)



