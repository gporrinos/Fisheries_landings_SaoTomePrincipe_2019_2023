
#########################################################################
#########################################################################
#                          VARIABLE DEFINITION                          #
#########################################################################
#########################################################################



# This script:
# A) creates inter- and intra-annual seasonality variables; 
# B) reclassifies fishing trips by main gear types (i.e., the same trip may use more than one métier);
# C) counts number of métiers per trip (variable "ngears")
# D) adds variables from "tripinfo" to "catch" and "effort" databases
# E) calculates average hook size of a line (a single line may bear hooks of different sizes)
# F) Retrieves information on the species from fish base







if(!require("rfishbase")) install.packages("rfishbase", repos = "https://cloud.r-project.org")
library(rfishbase)

source(file.path("data_analysis","scripts","01_load_and_clean_databases.R"))




#########################################################################
#          CONVERT DATE TO INTEGER AND CALCULATE ORDINAL DATE           #
#########################################################################



startyear              <- c("2019-01-01","2020-01-01","2021-01-01","2022-01-01","2023-01-01")
startyear              <- as.integer(as.POSIXct(startyear,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)
tripinfo$dateoriginal  <- tripinfo$date
tripinfo$date          <- as.integer(as.POSIXct(tripinfo$date,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)
tripinfo$ordinaldate   <- unlist(lapply(tripinfo$date,function(x){x-max(startyear[!startyear>x])}))






#########################################################################
#                  RECLASSIFY TRIPS BY MAIN GEAR TYPE                   #
#########################################################################





# Classify fishing trips into fishing gear categories
# Note that "hook_line" fishing trips may use more than one hook&line gear per trip

tripinfo$gear_general <- "hook_line"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "surface_gillnet")]]  <- "surface_gillnet"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "demersal_gillnet")]] <- "demersal_gillnet"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "spear_fishing")]]    <- "spear_fishing"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "seine_net")]]       <- "seine_net"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "circular_net")]]     <- "circular_net"
tripinfo$gear_general[tripinfo$trip %in% effort$trip[which(effort$gear_type == "beach_seine")]]      <- "beach_seine"
tripinfo$gear_general[which((tripinfo$trip %in% effort$trip[which(effort$shelf_area == "shore")]) & 
                              (tripinfo$vessel == "sem-embarcacao"))] <- "hook_line_shore"
tripinfo$gear_general[which(tripinfo$gear == 'hook_line' & tripinfo$vessel == 'sem-embarcacao')] <- "hook_line_shore"
tripinfo$gear_general[which(tripinfo$gear == 'hook_line' & tripinfo$propulsion == 'sem-embarcacao')] <- "hook_line_shore"
tripinfo$gear_general[which(is.na(tripinfo$participation))] <- NA
tripinfo$gear_general[which(tripinfo$participation == "nao")] <- NA







#########################################################################
#                   COUNT NUMBER OF MÉTIERS PER TRIP                    #
#########################################################################


tripinfo$ngears <- unlist(lapply(tripinfo$trip,function(trip){length(which(effort$trip == trip))}))
tripinfo$ngears[which(tripinfo$participation == "nao")] <- NA










#########################################################################
#               ADD CO VARIATES TO CATCH AND EFFORT DATA                #
#########################################################################

# Add gear category of the fishing trip to "catch" and "effort" databases
variables <- c("trip","gear_general","island","n_fishers")
catch <- catch    %>%   inner_join(tripinfo[variables],  by=c("trip"))



# Add gear category of the fishing trip to "catch" and "effort" databases
variables <- c("trip","gear_general","island","date","n_fishers","propulsion","ordinaldate")
effort <- effort %>% inner_join(tripinfo[variables],   by=c("trip"))

rm(variables)




#########################################################################
#                     TRANSFORM "hook_size" VARIABLE                    #
#########################################################################

# One line often uses hooks of several sizes, so I used the average size.

# Hook size was recorded as a multiple-response, closed-ended variable, so the
# variable was recorded as a text string, in which each response is separated 
# by ", ". The function "tolist" (see script 01A) is therefore used to separate 
# each response and convert it to a list.

effort$hook_size <- paste0(effort$hook_size,", ")
effort$hook_size <- gsub(", nao", "", effort$hook_size)
effort$hook_size <- gsub("nao, ", "", effort$hook_size)
effort$hook_size[which(effort$hook_size == "Brindado, ")] <- 0
effort$hook_size <- gsub("Brindado, ", "", effort$hook_size)
effort$hook_size[which(grepl("60",effort$hook_size))] <- NA
effort$hook_size_original <- effort$hook_size
size <- tolist(effort$hook_size)
suppressWarnings({
  effort$hook_size = unlist(lapply(size,function(x){mean(as.numeric(x))}))
                 })


rm(size)







#########################################################################
#                            RECLASSIFY BAIT                            #
#########################################################################



effort$bait_type <- effort$bait
artificial_lures <- c("BRINDADO", "BRINDADO, CEBOLA", "BRINDADO, LAGARTICA", "PALHA", "PALHA, BRINDADO",  "PLASTICO", 
                      "PLASTICO, BRINDADO",  "PLASTICO, PALHA",  "PLASTICO, PALHA, BRINDADO")
effort$bait_type[which(effort$bait_type %in% artificial_lures)] <- "LURE"
effort$bait_type[which(!effort$bait %in% artificial_lures)] <- "BAIT"
effort$bait_type[which(is.na(effort$bait))] <- NA
effort$bait_type[which(effort$gear_type=="demersal_line")] <- "BAIT"
effort$bait_type[which(effort$gear_type=="demersal_troll")] <- "LURE"

rm(artificial_lures)





#########################################################################
#                       PREPARE "species" DATABASE                      #
#########################################################################




speclist <- unlist(tolist(paste0(species$species_comp,", ")))
taxonomy <- rfishbase::load_taxa() %>% 
  filter(Species %in% speclist) %>%
  collect()
taxonomy <- as.data.frame(taxonomy)
ecolfish <- as.data.frame(ecology(speclist))
estTroph <- as.data.frame(estimate(speclist))



matchfish <- function(fishoutput,field)
  unlist(lapply(species$species,function(x){temp = fishoutput[which(fishoutput$Species ==x),which(colnames(fishoutput) == field)]
  if(length(temp) == 0) {NA} else {temp}}))
species$Family <- matchfish(taxonomy,"Family")
species$FoodTroph  <- matchfish(ecolfish,"FoodTroph")
species$DietTroph  <- matchfish(ecolfish,"DietTroph")
species$EstTroph  <-  matchfish(estTroph,"Troph")
species
for(i in which(grepl(",",species$species_comp))){
  speclist = unlist(tolist(paste0(species$species_comp[i],", ")))
  species$EstTroph[i] = mean(na.omit(estTroph$Troph[which(estTroph$Species %in% speclist)]))
  species$Family[i]   = taxonomy$Family[which(taxonomy$Species %in% speclist)][1]
}

species$Family[which(species$species == "Selachimorpha")] <- NA

species$FoodTroph[which(species$species == "Octopus sp.")] <- 3.74
species$EstTroph[which(species$species == "Octopus sp.")]  <- 3.74
species$Family[which(species$species == "Octopus sp.")]    <- "Octopodidae"


species$FoodTroph[which(species$species == "Sepia sp.")] <- 4.27
species$DietTroph[which(species$species == "Sepia sp.")] <- 3.71
species$EstTroph[which(species$species == "Sepia sp.")]  <- 4.27
species$Family[which(species$species == "Sepia sp.")]    <- "Sepiidae"


rm(ecolfish,estTroph,taxonomy,i,speclist,matchfish)







   #########################################################################
   #                      Fishing times and time at sea                    #
   #########################################################################



timetonumeric <- function(time)   as.numeric(substr(time,1,2)) + (as.numeric(substr(time,4,5))/60)

tripinfo$time_sea <- timetonumeric(tripinfo$arrival) - timetonumeric(tripinfo$departure)
tripinfo$time_sea[which(tripinfo$time_sea < 0)] <- 24 + tripinfo$time_sea[which(tripinfo$time_sea < 0)]

effort$time_fishing <- timetonumeric(effort$end) - timetonumeric(effort$start)
effort$time_fishing[which(effort$time_fishing < 0)] <- 24 + effort$time_fishing[which(effort$time_fishing < 0)]

for(i in 1:5)
    write.csv(list(catch, effort, ntrips, species, tripinfo)[[i]],
              file.path("data_analysis","data_with_covariates",
                        c("catch.csv", "effort.csv", "ntrips.csv", "species.csv", "tripinfo.csv")[[i]]))
