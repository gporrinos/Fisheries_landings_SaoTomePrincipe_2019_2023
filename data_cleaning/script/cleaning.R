
################################################################################
################################################################################
###-------------------------- CLEANING LANDING DATA -------------------------###
################################################################################
################################################################################








# Author: Guillermo Prieto Porrinos
# Database: LANDING SURVEYS
# Collected by: Fauna & Flora International, Fundacao Príncipe, Oikos Coop. & Desenv., MARAPA
# Funded by   : Blue Action Fund, Arcadia Fund









    #########################################################################
    ###              LOAD DATA AND INSTALL PCKGS (IF NEEDED)              ###
    #########################################################################





if(!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")



# Set directories
outfiles <- file.path(getwd(),"data")
rawfiles  <- file.path(getwd(),"data_cleaning","anonymised_raw_data")

# Load databases
catch   <-  read.table(file.path(rawfiles,"catch.csv"),header=TRUE,sep=",")
effort  <-  read.table(file.path(rawfiles,"effort.csv"),header=TRUE,sep=",")
ntrips  <-  read.table(file.path(rawfiles,"ntrips.csv"),header=TRUE,sep=",")
tripinfo<-  read.table(file.path(rawfiles,"tripinfo.csv"),header=TRUE,sep=",")
species <-  read.table(file.path(rawfiles,"species.csv"),header=TRUE,sep=",")









    #########################################################################
    ###                          GENERAL CLEANING                         ###
    #########################################################################





                #--------------------------------------------#
                #              Remove duplicates             # 
                #--------------------------------------------#

## Fix error from previous cleaning
tripinfo$meta_instance_ID <- gsub("Sessenta e dois","62",tripinfo$meta_instance_ID)

## ODK creates duplicates. This function removes the duplicate
delete <- unlist(lapply(1:nrow(tripinfo),function(i){trip = tripinfo$meta_instance_ID[i]
                                                     if(!is.na(trip)) {length(which(tripinfo$meta_instance_ID[1:i]==trip))} else {1}}))

remove = tripinfo$trip[-which(delete==1)]
tripinfo <- tripinfo[-which(tripinfo$trip %in% remove),]
catch    <- catch[-which(catch$trip %in% remove),]
effort   <- effort[-which(effort$trip %in% remove),]

                #--------------------------------------------#
                #           Create individual codes          #
                #--------------------------------------------#


# New code, based on unique code (uuid) created by ODK
tripinfo$trip_id  <- substr(tripinfo$meta_instance_ID,6,41)
catch             <- catch    %>%   inner_join(tripinfo[c("trip","trip_id")],by=c("trip"))
effort            <- effort   %>%   inner_join(tripinfo[c("trip","trip_id")],by=c("trip"))

# Function to count how many gears per trip, in the order that they appear
createcode <- function(dat){
           code = as.character(unlist(lapply(c(1:nrow(dat)),function(i){if(i==1){1} else {
                                   length(which(dat$trip_id[1:i]==dat$trip_id[i]))}})))
           code[which(as.numeric(code)<9.9)] = paste0(0,code[which(as.numeric(code)<9.9)])
           return(code)}

# Function to create unique catch and effort IDs, based on trip_id
catch$catch_id <- paste(catch$trip_id,"ca",createcode(catch),sep="_")
effort$effort_id <- paste(effort$trip_id,"ef",createcode(effort),sep="_")

# Substitute by new trip_id variable
tripinfo$trip <- tripinfo$trip_id
catch$trip <- catch$trip_id
effort$trip <- effort$trip_id




                #--------------------------------------------#
                #           Remove test questionnaires          #
                #--------------------------------------------#

removetrips <- c("27b1309d-ea42-4382-b20d-1f1ccd5bbcf5",
                 "1d1c3292-4e63-400f-a3f8-b5394e3be177",
                 "1b95265b-b86b-4871-a605-d8525d85271e",
                 "17834a6c-f236-47e9-9371-06b8ba8d9186",
                 "07df81c7-9f7b-4fc4-b300-c8183d1f8e10",
                 "50eba8bb-9f5f-46ea-bdc0-491d9b8d14e7")

tripinfo = tripinfo[-which(tripinfo$trip %in% removetrips),]
effort   = effort[-which(effort$trip %in% removetrips),]
catch    = catch[-which(catch$trip %in% removetrips),]



rm(removetrips)


                #--------------------------------------------#
                #              General functions             #
                #--------------------------------------------#



# Function to separate multiple-response variables into a list
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
                )
      })

# Function to merge multiple-response variables back into a single string
totext <- function(splist){
    dat = unlist(
      lapply(splist, function(x){
          for(i in 1:length(x)){
              if(i == 1) {temp = paste(x[i],", ",sep="")} else {
                temp = paste(temp,x[i],", ",sep="")}
           }
           if(temp == "NA, , ") {temp = ""}
           return(temp)
       }))
     return(dat)
     }


# Function to add a "0" if numbers are < 10 (so they have the same length)
add0              <- function(y) {unlist(lapply(y,function(x){if(nchar(x)==1) {paste0(0,x)} else {x}}))}











    #########################################################################
    ###                  FIX ERRORS IN DATES AND TIMES                    ###
    #########################################################################




### STEP 1: STANDARDISE DATES
tripinfo$date <- unlist(lapply(tripinfo$date,function(date){
  if(grepl("/",date)){
    paste(substr(date,7,10),substr(date,4,5),substr(date,1,2),sep="-")} else {date}}))
tripinfo$date <- as.POSIXct(tripinfo$date,format = c("%Y-%m-%d"),tz="UTC")

ntrips$date <- unlist(lapply(ntrips$date,function(date){
  if(grepl("/",date)){
    paste(substr(date,7,10),substr(date,4,5),substr(date,1,2),sep="-")} else {date}}))
ntrips$date <- as.POSIXct(ntrips$date,format = c("%Y-%m-%d"),tz="UTC")



### STEP 2: FIX ERRORS IN FISHING TIMES
errors <- which(!is.na(effort$start) & !grepl(":",effort$start))
effort$start[errors] <- paste(add0(floor(as.numeric(effort$start[errors])*24)),
                              add0(floor(((as.numeric(effort$start[errors])*24) - 
                                            floor(as.numeric(effort$start[errors])*24))*60)),
                              "00",sep=":")
errors <- which(!is.na(effort$end) & !grepl(":",effort$end))
effort$end[errors] <- paste(add0(floor(as.numeric(effort$end[errors])*24)),
                              add0(floor(((as.numeric(effort$end[errors])*24) - 
                                            floor(as.numeric(effort$end[errors])*24))*60)),
                              "00",sep=":")




rm(delete,errors,remove, createcode)









    #########################################################################
    ###                    CLEANING OF NTRIPS DATABASE                    ###
    #########################################################################




ntrips$landing_site[which(ntrips$landing_site == "Novo")] <- "novo"
ntrips$site[which(ntrips$landing_site == "novo")]        <- "NO"

ntrips <- ntrips[-which(is.na(ntrips$worked_y.n) & is.na(ntrips$meta.instanceID) & is.na(ntrips$surface_gillnet) ),
                 -which(colnames(ntrips) %in% c("duplicates","database","meta.instanceName"))]
ntrips <- ntrips %>%
  group_by(site_date) %>% #group by site
  summarise_all(max) %>% # keep the max value of each column when different
  ungroup
ntrips <- as.data.frame(ntrips)


### IDENTIFY DAYS THAT THEY DID NOT WORK FOR PERSONAL REASONS AND SUBSTITUTE TRIPS FOR NA
total_trips <- unlist(lapply(1:nrow(ntrips),function(i){sum(ntrips[i,c(12:16,18)])}))
ntrips[which(total_trips == 0 & ntrips$worked_y.n =="pessoal"),c(12:16,18)] <- NA

ntrips$ntrips_id <- ntrips$meta.instanceID

ntrips$hook_line     <- abs(ceiling(ntrips$hook_line))
ntrips$surface_gillnet_seine[which(ntrips$island=="PC")] <- 0
ntrips$circular_net  <- ntrips$panha

gears <- c("surface_gillnet", "circular_net", "purse_seine",
                   "surface_gillnet_seine", "demersal_gillnet","spear_fishing","hook_line","hook_line_shore")

for(i in 1:length(gears))
        ntrips[which(ntrips[,which(colnames(ntrips) == gears[i])] > 40),
               which(colnames(ntrips) == gears[i])]                        <- NA
for(i in 1:length(gears))
        ntrips[,  which(colnames(ntrips) == gears[i])]  <- as.integer(round(ntrips[,  which(colnames(ntrips) == gears[i])], 0))


ntrips <- ntrips[c("ntrips_id", "island", "site", "landing_site", "ext_worker", "date", "worked_y.n", 
                   "reason_not_fishing", "other_reason", "surface_gillnet", "circular_net", "purse_seine",
                   "surface_gillnet_seine", "demersal_gillnet","spear_fishing","hook_line","hook_line_shore")]

ntrips$demersal_gillnet[which(ntrips$demersal_gillnet > 5)] <- 0





write.csv(ntrips, file.path(outfiles, "ntrips.csv"), row.names=FALSE)

rm(ntrips,gears,i,total_trips)









    #########################################################################
    ###                   CLEANING OF TRIPINFO DATABASE                   ###
    #########################################################################






# TAG INVALID INSTANCES
tripinfo$valid    <- TRUE

tripinfo$valid[which(tripinfo$ext_worker == "acacio")] <- FALSE
tripinfo$valid[which(tripinfo$ext_worker == "gaspar")] <- FALSE
tripinfo$valid[which(tripinfo$trip == "dcee46cf-4764-4476-bcac-fa18974f732e")] <- FALSE




# CHANGE "ENGINE" VARIABLE
tripinfo$engine[which(tripinfo$vessel %in% 
           c("prao-remo", "canoa-remo"))] <- "remo-vela"
tripinfo$engine[which(tripinfo$vessel %in% 
           c("submarino-a-caminhar", "sem-embarcacao"))] <- "sem-embarcacao"



# CREATE "PROPULSION" VARIABLE
tripinfo$propulsion <- as.character(NA)
tripinfo$propulsion[which(tripinfo$engine == "sem-embarcacao")] <- "sem-embarcacao"   # No vessel
tripinfo$propulsion[which(tripinfo$engine == "remo-vela")]      <- "remo-vela"        # Rowing / sailing
tripinfo$propulsion[which(tripinfo$engine %in% c("15cv", "25cv", "40cv",  
             "4cv", "6cv", "8cv", "9cv") | tripinfo$fuel > 0)]  <- "motorisada"       # Motorised



# SIMPLIFY VESSEL TYPE VARIABLE
tripinfo$vessel_detail <- tripinfo$vessel
tripinfo$vessel[grepl("bote",tripinfo$vessel)]       <- "bote"           # Fiber-glass boats
tripinfo$vessel[grepl("canoa",tripinfo$vessel)]      <- "canoa"          # Dugouts
tripinfo$vessel[grepl("prao",tripinfo$vessel)]       <- "prao"           # Single-outrigger canoe
tripinfo$vessel[grepl("a-caminhar",tripinfo$vessel)] <- "sem-embarcacao" # No vessel



# FUEL
tripinfo$fuel[which(tripinfo$propulsion %in% c("sem-embarcacao","remo-vela"))] <- 0



# FUEL PRICE
tripinfo$price_fuel[which(tripinfo$price_fuel > 200)] <- tripinfo$price_fuel[which(tripinfo$price_fuel > 200)]/100
tripinfo$price_fuel[which(tripinfo$propulsion %in% c("sem-embarcacao","remo-vela"))] <- as.numeric(NA)



# N FISHERS
recordednames <- count.fields(textConnection(paste0(tripinfo$name_fishers, ",")), sep = "," ) -2
tripinfo$n_fishers[which(tripinfo$n_fishers == 0 & !recordednames == 0)] <- 
  recordednames[which(tripinfo$n_fishers == 0 & !recordednames == 0)]
tripinfo$n_fishers[which(tripinfo$n_fishers ==0)] <- NA

tripinfo$n_fishers[which(tripinfo$n_fishers == 30)] <- 3



# FISHERS' CODES
comms <- c("ABA",  "AN1",  "ANA",  "BMB",  "BUR",  "CAM",  "CMP",  "CO2",  "CON",  "CPT",  "IOL",  "JQM",  "LAP", "PMI",  "RI1",  "RIZ", 
           "SAN",  "SBR",  "SC2",  "SC5",  "SCR",  "SEC",  "SJ1",  "SJ2",  "SJ3",  "SJO",  "SPR",  "TV1",  "TV2",  "UN1", "UN2", "UNI")
for(i in 1:length(comms)){
  tripinfo$name_fishers <- gsub(comms[i],"",tripinfo$name_fishers)}

tripinfo$name_fishers[which(tripinfo$island == "PC" & !tripinfo$name_fishers == "")] <-
  totext(lapply(
      tolist(tripinfo$name_fishers[which(tripinfo$island == "PC" & !tripinfo$name_fishers == "")]), 
      function(fisherlist) paste0("PC",fisherlist)))

rm(comms,i,recordednames)







    #########################################################################
    ###                    CLEANING OF CATCH DATABASE                     ###
    #########################################################################




### STEP 1: SINGLE ERRORS (WEIGHT TOO HIGH)
# POLVO "weight = 1510, nfish = NA" ------> "weight = 15, nfish = 10"
instance = which(catch$trip == "89a00dd2-2241-4cb7-8aa2-fe35d6ac7e61" & catch$species == "POLVO")
catch$weight[instance] <- 15
catch$number[instance] <- 10

# SABONETE VERMELHO "weight = 3003, nfish = NA" ------> "weight = 0.3, nfish = 3"
instance = which(catch$trip == "85813d21-47e3-450d-b666-49de69bd6c62" & catch$species == "SABONETE-VERMELHO")
catch$weight[instance] <- 0.3
catch$number[instance] <- 3

# ASNO-LACETA "weight = 9000, nfish = 1" ------> "weight = 0.900, nfish = 1"
instance = which(catch$trip == "9337de42-28b9-4260-bd4f-b5dfab427ee5" & catch$species == "ASNO-LACETA")
catch$weight[instance] <- 0.9

# CHOCO "weight = 8000, nfish = 1" ------> "weight = 0.800"
instance = which(catch$trip == "9337de42-28b9-4260-bd4f-b5dfab427ee5" & catch$species == "CHOCO")
catch$weight[instance] <- 0.8

# COCOVADO "weight = 100" unlikely to weight 100g (from picture). Rounded to 0.5
instance = which(catch$trip == "dae1614a-62dc-47f6-96db-5e9d8a35535d" & catch$species == "COCOVADO")
catch$weight[instance] <- 0.4

# BARRACUDA "weight = 55, nfish = 1" ------> "weight = 5.5"
instance = which(catch$trip == "82951bc7-5a20-4780-99b0-32ed7d9d53aa" & catch$species == "BARRACUDA")
catch$weight[instance] <- 5.5





### STEP 2: GRAMS INSTEAD OF KG (WEIGHT TOO HIGH)
catch$average <- catch$weight/catch$number
biggerthan50  <- levels(as.factor(catch[which(catch$average > 50 & !is.na(catch$average) & !catch$number == 0),c(2,3,5,6,7,8,9,13)]$species))
bigspecies    <- c("TUBARAO-AREIA", "TUBARAO-BRANCO", "TUBARAO-LAGAIA", "TUBARAO-MARTELO", 
                   "TUBARAO-PEIXE", "UZUA-MANTA", "RAIA2", "ESTROMBA", "PEIXE-ANDALA")
biggerthan50 <- biggerthan50[-which(biggerthan50 %in% bigspecies)]
instance <- which(catch$species %in% biggerthan50 & catch$average > 50 &
             !is.na(catch$average) & !catch$number == 0)
catch$weight[instance] <- catch$weight[instance]/1000
catch$average <- catch$weight/catch$number
catch$weight[which(catch$catch_id == "649d1070-4372-432a-8089-2c16e83064cd_ca_01")] <- 
  catch$weight[which(catch$catch_id == "649d1070-4372-432a-8089-2c16e83064cd_ca_01")]/1000






    #########################################################################
    ###                     STANDARDISE SPECIES CODES                     ###
    #########################################################################




## Fix error from previous cleaning
instances <- which(catch$species %in% c("Bonga","Lagosta","Blonge","Broze","Bronze","Mamaninha",
                          "Malagueta","Panpole","Peixe cabra e broze","Mamaninha e bonga"))
catch$other_species[instances] <- catch$species[instances]
catch$species[instances]    <- "OUTRO-PEIXE"
effort$catch_composition[effort$trip %in% catch$trip[instances]] <- 
            paste0(effort$catch_composition[effort$trip %in% catch$trip[instances]],", OUTRO-PEIXE")

effort$catch_composition[which(effort$trip == "00000000-0000-0000-0000-000000000174")] <- gsub("BULHAO-BICA", "BULHAO-PAPAGAIO2", effort$catch_composition[which(effort$trip == "00000000-0000-0000-0000-000000000174")] )
effort$catch_composition[which(effort$trip == "0fdf6db6-5823-4ce7-8bfa-746c43690ee2")] <- gsub("CONCOM", "UNKNOWN", effort$catch_composition[which(effort$trip == "0fdf6db6-5823-4ce7-8bfa-746c43690ee2")])
effort$catch_composition[which(effort$trip == "e4d4463e-e52e-4375-8607-e542b9ed4fa4")] <- gsub("THINTHE", "TCHINTCHIN", effort$catch_composition[which(effort$trip == "e4d4463e-e52e-4375-8607-e542b9ed4fa4")] )
trip = "621130b4-17ec-4aff-acfb-ac915284e0d9"
effort$catch_composition[which(effort$trip == trip)] <- gsub("CORVINA-VERMELHA", "CORVINA-CACADEIRA", effort$catch_composition[which(effort$trip == trip)])
trip = "6b29d516-dcc6-4bda-b586-54f72cdf15d3"
effort$catch_composition[which(effort$trip == trip)] <- gsub("CORVINA-VERMELHA", "CORVINA-CACADEIRA", effort$catch_composition[which(effort$trip == trip)])
trip = "c426114b-0b18-473b-81f0-af6594eb4ed6"
effort$catch_composition[which(effort$trip == trip)] <- gsub("CORVINA-VERMELHA", "CORVINA-CACADEIRA", effort$catch_composition[which(effort$trip == trip)])
trip = "00000000-0000-0000-0000-000000000216"
effort$catch_composition[which(effort$trip == trip)] <- gsub("CORVINA-VERMELHA", "CORVINA-PRETA", effort$catch_composition[which(effort$trip == trip)])
trip = "fb244e60-ab66-454c-a8af-371e6ac55ec6"
effort$catch_composition[which(effort$trip == trip)] <- gsub("CORVINA-DE-CACA", "CORVINA-PRETA", effort$catch_composition[which(effort$trip == trip)])
trip = "5e4eaff5-28e5-48d2-a216-555024c5f3ab"
effort$catch_composition[which(effort$trip == trip)] <- gsub("FULU-FULU-BATIDO", "FULU-FULU", effort$catch_composition[which(effort$trip == trip)])
trip = "00000000-0000-0000-0000-000000000211"
effort$catch_composition[which(effort$trip == trip)] <- gsub("FULU-FULU-REBOLA", "FULU-FULU-BATIDO", effort$catch_composition[which(effort$trip == trip)])
trip = "9a77b5de-bec5-441d-a4d2-d4aad075f147"
effort$catch_composition[which(effort$trip == trip)] <- gsub("PATA-PATA", "PATA-PATA3", effort$catch_composition[which(effort$trip == trip)])



## Function to change species codes in "catch" and "effort" databases
## (!!!) species codes need to be changed in effort$catch variable for them to match.

changespecies <- function(trip,old_species,new_species){
         catch <- catch
         catch$species[which(catch$species == old_species & catch$trip == trip)] <<- new_species
         instances = which(grepl(old_species,effort$catch) & effort$trip == trip)
         for(x in instances) {x <- x
                              effort <- effort
                              temp = tolist(paste0(effort$catch_composition[x], ", "))
                              temp[[1]][which(temp[[1]] == old_species)] = new_species
                              effort$catch_composition[x] <<- substr(totext(temp), 1, nchar(totext(temp))-2)}
              }


## Load database with IDs from pictures and change codes
newspeciesnames <- read.table(file.path(rawfiles, "newspeciesnames.csv"),header=TRUE,sep=",")
newspeciesnames <- newspeciesnames[!newspeciesnames$new_name == "NO_IDEA" & !newspeciesnames$new_name == "",c("trip_id","new_name","species")]
for(i in 1:nrow(newspeciesnames)){changespecies(newspeciesnames$trip_id[i],newspeciesnames$species[i],newspeciesnames$new_name[i])}

## Standardise names of species recorded as "Other"
for(i in which(catch$species == "VOADOR")){changespecies(catch$trip[i],"VOADOR","PEIXE-VOADOR")}

for(i in which(catch$species == "AGULHA-BUZINHA")){changespecies(catch$trip[i],"AGULHA-BUZINHA","AGULHA-BUZINA")}

for(i in which(catch$other_species %in% c("Blonge","Broze","Bronze"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","BLONZE")}
catch$other_species[which(catch$other_species %in% c("Blonge","Broze","Bronze"))] <- NA


for(i in which(catch$other_species %in% c("Malagueta"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","MALAGUETA")}
catch$other_species[which(catch$other_species %in% c("Malagueta"))] <- NA


changespecies("0fdf6db6-5823-4ce7-8bfa-746c43690ee2","UNKNOWN","CONCOM-DE-FUNDO")

changespecies("00000000-0000-0000-0000-000000000456","OUTRO-PEIXE","BUBU")

for(i in which(catch$other_species %in% c("Mamaninha","Mamaninha e","Mmaninha"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","MAMANINHA")}
catch$other_species[which(catch$other_species %in% c("Mamaninha","Mamaninha e","Mmaninha"))] <- NA

for(i in which(catch$other_species %in% c("Bonga"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","BONGA")}
catch$other_species[which(catch$other_species %in% c("Bonga"))] <- NA

for(i in which(catch$other_species %in% c("Asno pego"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","ASNO-PEGO")}
catch$other_species[which(catch$other_species %in% c("Asno pego"))] <- NA

for(i in which(catch$other_species %in% c("Tchin tchin","Tchintchi","Tchintchim","Tchintchin"))) {
           changespecies(catch$trip[i],"OUTRO-PEIXE","TCHINTCHIN")}
catch$other_species[which(catch$other_species %in% c("Tchin tchin","Tchintchi","Tchintchim","Tchintchin"))] <- NA


temp  <- catch$trip[which(catch$other_species %in% c("Asno pego  e tchintche pego"))]
catch <- rbind(catch,catch[which(catch$other_species %in% c("Asno pego  e tchintche pego")),])
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego"))[1],which(colnames(catch)=="species")]  <- "TCHINTCHIN-DE-FUNDO"
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego"))[1],which(colnames(catch)=="weight")]   <- 0.6
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego"))[1],which(colnames(catch)=="number")]   <- 1
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego"))[1],which(colnames(catch)=="other_species")]  <- NA
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego")),which(colnames(catch)=="species")]  <- "ASNO-PEGO"
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego")),which(colnames(catch)=="weight")]   <- 0.9
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego")),which(colnames(catch)=="number")]   <- 1
catch[which(catch$other_species %in% c("Asno pego  e tchintche pego")),which(colnames(catch)=="other_species")] <- NA
effort$catch_composition[which(effort$trip == temp)] <- gsub("OUTRO-PEIXE","TCHINTCHIN-DE-FUNDO, ASNO-PEGO",effort$catch_composition[which(effort$trip == temp)])








   #########################################################################
   ###        CALCULATE WEIGHTS OF SPECIES SOLD BY NUMBER OF FISH        ###
   #########################################################################





# Flying fish are often caught in large quantities and it is therefore difficult to
# weigh them. This code detects instances were fish was counted but weight was 
# recorded as zero and substitutes it by "NA".

# Fix errors
catch$weight[which(catch$species == "PEIXE-VOADOR" &
            catch$trip == "479e6cf1-133e-4906-bf2d-651f31bbd961")] <- NA
catch$weight[which(catch$species == "PEIXE-VOADOR" &
            catch$trip == "880ec81d-8705-47dc-b4ad-71100be27d46")] <- NA
catch$number[which(catch$species == "PEIXE-VOADOR" &
            catch$trip == "880ec81d-8705-47dc-b4ad-71100be27d46")] <- 150

changed <- which(catch$species == "PEIXE-VOADOR" & catch$weight > catch$number)
num     <- catch$number[changed]
catch$number[changed] <- catch$weight[changed]
catch$weight[changed] <- num


# Substitute 0s by AVERAGE WEIGHT
sp = "PEIXE-VOADOR"
   datavailable    = !catch$average == 0 & !is.na(catch$average) & !catch$number == 0 & !catch$average == Inf
   zeroNAs         = catch$weight == 0 | is.na(catch$weight)
   instances       = which(catch$species == sp & zeroNAs & !catch$number == 0)
   catch$weight[instances]  = NA
   average = mean(na.omit(catch$weight[which(catch$species == sp)] / catch$number[which(catch$species == sp)]))
   catch$weight[which(catch$species == sp & is.na(catch$weight))] = 
                  catch$number[which(catch$species == sp & is.na(catch$weight))] * average



# Substitute 0s by AVERAGE WEIGHT
sp = "MAXIPOMBO"
   datavailable    = !catch$average == 0 & !is.na(catch$average) & !catch$number == 0 & !catch$average == Inf
   zeroNAs         = catch$weight == 0 | is.na(catch$weight)
   instances       = which(catch$species == sp & zeroNAs & !catch$number == 0)
   catch$weight[instances]  = NA
   average = mean(na.omit(catch$weight[which(catch$species == sp)] / catch$number[which(catch$species == sp)]))
   catch$weight[which(catch$species == sp & is.na(catch$weight))] = 
                  catch$number[which(catch$species == sp & is.na(catch$weight))] * average


   
   
   
   
   
   
   
   #########################################################################
   ###               MORE STANDARDISATION OF SPECIES CODES               ###
   #########################################################################
   
   
   
   
catch <- catch[c("catch_id", "trip", "species", "weight", "number", 
                 "use", "price", "unit", "picture", "link")]

catch <- rbind(catch, data.frame(
                   catch_id = c("48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8_ca_01", "48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8_ca_02", "48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8_ca_03"),
                   trip = c("48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8", "48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8", "48de56b2-15e1-4b3d-a7cf-ee6c6354c4d8"), 
                   species       = c("CONCOM", "CAVALA", "FULU-FULU-BATIDO"),   weight = c(NA, NA, NA),    number  = c(NA, NA, NA),  use = c(NA, NA, NA),
                   price = c(NA, NA, NA),    unit  = c(NA, NA, NA),    picture  = c(NA, NA, NA), link = c(NA, NA, NA)))
catch <- rbind(catch, data.frame(catch_id = c("00000000-0000-0000-0000-000000000249_ca_03"),
                   trip = c("00000000-0000-0000-0000-000000000249"), 
                   species       = c("VERMELHO-TERRA"),   weight = NA,    number  = NA,  use = NA,
                   price = NA,     unit  = NA,     picture  = NA,  link = NA))
catch <- rbind(catch, data.frame(catch_id = c("10ec01fa-fe3b-43f5-a631-f5e346435972_ca_04"),
                   trip = c("10ec01fa-fe3b-43f5-a631-f5e346435972"), 
                   species       = c("FULU-FULU"),   weight = NA,    number  = NA,  use = NA,
                   price = NA,     unit  = NA,     picture  = NA,  link = NA))





   #########################################################################
   ###               CHANGE SPECIES CODES BY SPECIES NAMES               ###
   #########################################################################



spec <- data.frame(code    = c(species$CODE.ST,species$CODE.PC),
                   species = c(species$species,species$species))



for(j in 1:nrow(spec)){
       trips = which(catch$species == spec[j,1])
       if(length(trips) > 0) {for(i in trips){changespecies(catch$trip[i],spec[j,1],spec[j,2])}}
         }


spec2 <- c("BACALHAU", "BESUGO", "BICA", "BOBO-QUEMA", "BONITO","CANGA", "CORVINA-VERMELHA", "PARGO", "PEIXE-AZEITE", "PEIXE-SERRA", "VERMELHO-FUNDO", "VERMELHO-TERRA")
for(i in 1:length(spec2)){
      catch$species[which(catch$species == spec2[i])] <- spec$species[which(spec$code == spec2[i])][1]
}










   #########################################################################
   ###                        FILL SPECIES WEIGHT                        ###
   #########################################################################






averageweight <- catch$weight / catch$number
temp <- lapply(levels(as.factor(catch$species)), function(sp){
  av = averageweight[which(catch$species == sp & !is.na(averageweight) & is.finite(averageweight))]
  data.frame(sp = sp, n = length(av), 
             median = median(av))
  })
for(i in 1:length(temp))   if(i == 1) averageweight = temp[[1]] else averageweight <- rbind(averageweight, temp[[i]])

positions <- which(is.na(catch$weight) & !is.na(catch$number) & !catch$number == 0)
estimateweights <- unlist(lapply(positions,function(i) {
  catch$number[i] *  averageweight$median[averageweight$sp == catch$species[i]]
       }))

catch$weight[positions] <- round(estimateweights,1)



catch$weight[which(catch$catch_id == "b3c624da-c4d5-48c8-a7b1-53808d464bbc_ca_03")] <- 0.5
catch$weight[which(catch$catch_id == "b3c624da-c4d5-48c8-a7b1-53808d464bbc_ca_04")] <- 0.6
catch$weight[which(catch$catch_id == "9eaca1c2-30d4-414c-969f-8696615fe693_ca_01")] <- 0.2
catch$weight[which(catch$catch_id == "eb3f6ba7-63cf-47d1-9f75-b112ebee38aa_ca_02")] <- 0.1


tripinfo$vessel[which(tripinfo$trip == "8dd3dc5a-0862-45f2-a6c1-a7807b95f566")] <- "canoa"
tripinfo$propulsion[which(tripinfo$trip == "8dd3dc5a-0862-45f2-a6c1-a7807b95f566")] <- "motorisada"
tripinfo$engine[which(tripinfo$trip == "8dd3dc5a-0862-45f2-a6c1-a7807b95f566")] <- NA



   #########################################################################
   ###                        FILL SPECIES NUMBER                        ###
   #########################################################################





positions <- which(is.na(catch$number) & !is.na(catch$weight))

# Weight and number in the same cell
instance = "745b5232-183f-4627-8388-1a9f690f3421_ca_01"
catch$weight[which(catch$catch_id == instance)] <- 10
catch$number[which(catch$catch_id == instance)] <- 1

# Wrong kg
catch$weight[which(catch$catch_id == "949d6a10-4777-4d96-a0d3-22b0d0eec52d_ca_03")] <- 7
catch$number[which(catch$catch_id == "949d6a10-4777-4d96-a0d3-22b0d0eec52d_ca_03")] <- 1

# Estimated from picture
catch$weight[which(catch$catch_id == "aa212729-4949-43b0-89fc-5ef2cfe3e901_ca_01")] <- 1
catch$number[which(catch$catch_id == "aa212729-4949-43b0-89fc-5ef2cfe3e901_ca_01")] <- 2

# Counted from picture
catch$number[which(catch$catch_id == "00000000-0000-0000-0000-000000000110_ca_01")] <- 4

## Kg instead of g
catch$weight[which(catch$catch_id == "1ba7f838-603c-4bbb-9f5c-080b0ac4ad46_ca_01")] <- 0.1
catch$number[which(catch$catch_id == "1ba7f838-603c-4bbb-9f5c-080b0ac4ad46_ca_01")] <- 1


## Kg instead of g
catch$weight[which(catch$catch_id == "538975ea-bb26-4206-80d2-946c90d03ef8_ca_02")] <- 0.9
catch$number[which(catch$catch_id == "538975ea-bb26-4206-80d2-946c90d03ef8_ca_02")] <- 1


# Weight and number in the same cell
instance = "92ca0969-c5ef-43d8-913d-ea6b05bf417e_ca_01"
catch$weight[which(catch$catch_id == instance)] <- 2
catch$number[which(catch$catch_id == instance)] <- 10


# Weight and number in the same cell
instance = "5150c592-c9a3-463e-a162-64b6422c149f_ca_04"
catch$weight[which(catch$catch_id == instance)] <- 3
catch$number[which(catch$catch_id == instance)] <- 5


# Weight and number in the same cell
instance = "660d57a9-65a7-4467-8e65-99e1214c3366_ca_01"
catch$weight[which(catch$catch_id == instance)] <- 6
catch$number[which(catch$catch_id == instance)] <- 4


# Weight and number in the same cell
instance = "b5b4c465-8613-4631-bc61-5fc60fdd9897_ca_03"
catch$weight[which(catch$catch_id == instance)] <- 5
catch$number[which(catch$catch_id == instance)] <- 2

### ESTIMATE MISSING NUMBER OF FISH FROM WEIGHTS AND MEDIAN WEIGHT
positions <- which(is.na(catch$number) & !is.na(catch$weight))
estimatenumber <- unlist(lapply(positions,function(i) {
  catch$weight[i] /  averageweight$median[averageweight$sp == catch$species[i]]
}))
catch$number[positions] <- ceiling(estimatenumber)







   #########################################################################
   ###                          SAVE CATCH DATA                          ###
   #########################################################################


## REMOVE OBJECTS

rm(instance,instances,biggerthan50, bigspecies,changed,datavailable,estimatenumber,
   estimateweights,i,j, num,sp,spec,spec2,temp,trip,trips,zeroNAs, newspeciesnames,
   average,changespecies, averageweight,positions)


## SAVE CATCH DATA
catch <- catch[c("catch_id", "trip", "species", "weight", "number", 
                 "use", "price", "unit", "picture", "link")]
write.csv(catch, file.path(outfiles,"catch.csv"), row.names=FALSE)












    #########################################################################
    ###                    SIMPLIFY "species" DATABASE                    ###
    #########################################################################



## Remove duplicates
keep = which(unlist(lapply(1:nrow(species),function(i){length(which(species$species[1:i]==species$species[i]))}))==1)
species <- species[keep,-c(1,2)][c("species", "species_comp", "type", "species_comp_source", "local_name_ST", "local_name_PC")]



## Save "species" database
write.csv(species, file.path(outfiles,"species.csv"), row.names=FALSE)



## Remove objects from global environment
rm(species,  keep)




    #########################################################################
    ###                         RECLASSIFY GEARS                          ###
    #########################################################################




effort$gear[which(effort$trip == "20de15c6-0ca2-49ed-bd59-c67d28288fea")] <- "RABO"
effort$hooks_width[which(effort$trip == "20de15c6-0ca2-49ed-bd59-c67d28288fea")] <- NA


effort$gear[which(effort$gear %in% c("XITO","VEGA"))] <- "COSTUMADO"

### 
effort$gear_type    <- as.character(NA)
effort$gear_subtype <- as.character(NA)

effort$gear_type[which(effort$gear %in% c("SUBMARINO"))] <- "spear_fishing"

effort$gear_type[which(effort$gear %in% c("REDE-VOADOR"))]                     <- "surface_gillnet"
effort$gear_type[which(effort$gear %in% c("REDE-FEIJAO","REDE-MALHADEIRA"))]   <- "demersal_gillnet"

effort$gear_type[which(effort$gear %in% c("REDE-BARBUDO"))]                   <- "beach_seine"
effort$gear_type[which(effort$gear %in% c("REDE-BRISA"))]                     <- "seine_net"
effort$gear_type[which(effort$gear %in% c("REDE-CERCO"))]                     <- "seine_net"
effort$gear_subtype[which(effort$gear %in% c("REDE-BRISA"))]                  <- "surface_seine_gillnet"
effort$gear_subtype[which(effort$gear %in% c("REDE-CERCO"))]                  <- "purse_seine"


effort$gear_type[which(effort$gear %in% c("VOADOR-PANHA"))] <- "circular_net"

effort$gear_type[which(effort$gear %in% c("PALANQUE", "PALIM", "RABO"))] <- "demersal_line"
effort$gear_subtype[which(effort$gear %in% c("PALANQUE", "PALIM"))]      <- "set"
effort$gear_subtype[which(effort$gear %in% c("RABO"))]                   <- "pivoting"

effort$gear_type[which(effort$gear %in% c("CORICO-DE-FIO-GROSSO", "CORICO-DE-FULU-FULU","SAMBA"))]    <- "surface_troll"
effort$gear_type[which(effort$gear %in% c("ARRASTAO"))]                                               <- "demersal_troll"


effort$gear_type[which(effort$gear %in% c("COSTUMADO","FIO-DE-FUNDO","FIO-JOGADO","FIO-NA-PEDRA","FUNDO","SONDA","TOCA"))]   <- "handline"



line <- c("demersal_line", "surface_troll", "demersal_troll", "handline")
net  <- c("surface_gillnet", "demersal_gillnet", "beach_seine", "seine_net")

# get column with nº lines for 'linha' 
effort$n_lines <- as.integer(NA)
effort$n_lines[which(effort$gear_type %in% line)] <- as.integer(effort$lines_length[which(effort$gear_type %in% line)])
effort$n_hooks <- as.integer(NA)
effort$n_hooks[which(effort$gear_type %in% line)] <- as.integer(effort$hooks_width[which(effort$gear_type %in% line)])
effort$hook_size <- as.character(NA)
effort$hook_size[which(effort$gear_type %in% line)] <- effort$size[which(effort$gear_type %in% line)]

 
# get column with length of net for 'rede'
effort$net_length <- as.integer(NA)
effort$net_length[which(effort$gear_type %in% net)] <- as.integer(effort$lines_length[which(effort$gear_type %in% net)])
effort$net_depth <- as.integer(NA)
effort$net_depth[which(effort$gear_type %in% net)] <- as.integer(effort$hooks_width[which(effort$gear_type %in% net)])
effort$mesh_size <- as.integer(NA)
effort$mesh_size[which(effort$gear_type %in% net)] <- as.integer(effort$size[which(effort$gear_type %in% net)])




    #########################################################################
    ###                             BAIT TYPE                             ###
    #########################################################################




bait   <-  read.table(file.path(rawfiles,"bait.csv"),header=TRUE,sep=",")
for(i in 1:nrow(bait)){effort$bait[effort$bait==bait[i,1]   &  !is.na(effort$bait)] <- bait[i,2]}
effort$bait[which(effort$bait == "Fu\nFulofulo")] <- "FULU-FULU"
effort$bait[grepl("Fulofulo",effort$bait) & is.na(effort$bait)] <- "FULU-FULU"



rm(bait,i,line,net)



    #########################################################################
    ###                  Create "gear general" variable                   ###
    #########################################################################



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
    ###                         EFFORT INDICATORS                         ###
    #########################################################################




effort = effort %>% inner_join(tripinfo[c("trip","n_fishers","island")], by = "trip")

# Fix errors in "number of lines" variable
selection <- which(effort$gear_type == "handline"    &    effort$n_lines > effort$n_fishers + 1  & !effort$n_fishers == 0)
effort$n_lines[selection] <- effort$n_fishers[selection]
selection <- which(effort$gear_type == "handline"    &    effort$n_lines > effort$n_fishers + 1 & effort$n_lines > 4)
effort$n_lines[selection] <- NA

selection <- which((effort$gear_type == "surface_troll"   |  
                    effort$gear_type == "handline"        |
                    effort$gear_type == "demersal_line"   |
                    effort$gear_type == "demersal_troll") &    
                   effort$n_lines > 5)
effort$n_lines[selection] <- NA

# Fix errors in "number of hooks"
selection <- which(effort$n_hooks > 1000)
effort$n_hooks[selection] <- NA

# Fix errors in "net length"
selection <- which(effort$net_length > 3000 |  effort$net_length < 50)
effort$net_length[selection] <- NA

# Fix erros in "net depth"
selection <- which(effort$net_depth > 2000 |  effort$net_depth < 5)
effort$net_depth[selection] <- NA

# Fix errors in units in "mesh size"
selection <- which(effort$mesh_size > 100 |  effort$mesh_size < 0.1)
effort$mesh_size[selection] <- NA

selection <- which(effort$mesh_size > 13)
effort$mesh_size[selection] <- effort$mesh_size[selection]/10
effort$mesh_size <- effort$mesh_size*10




# Fix errors in number of hooks
positions <- which(effort$gear_type == "handline" & effort$n_hooks > 50)
effort$n_hooks[positions] <-  floor(effort$n_hooks[positions]/10)

positions <- which(effort$gear_type == "surface_troll" & effort$n_hooks > 101)
effort$n_hooks[positions] <-  floor(effort$n_hooks[positions]/10)






trips <- c("5452ee2f-9734-4b77-8993-d90b3a1fda5a",
           "8ec39ead-711e-454b-a34b-9bbca4280993",
           "d943eec4-3d8c-4169-b778-731d257f5e49",
           "35e9fbc0-b76a-4c22-936d-65ae97d8744f")
tripinfo$n_fishers[which(tripinfo$trip %in% trips)] <- 1

trips <- tripinfo$trip[which(tripinfo$n_fishers > 4 & 
                               tripinfo$gear_general == "hook_line" & tripinfo$valid)]
tripinfo$n_fishers[which(tripinfo$trip %in% trips)] <- NA
tripinfo$name_fishers[which(tripinfo$trip %in% trips)] <- NA







## Remove objects from global environment
rm(selection)








   #########################################################################
   ###              FIX TRIPS IN WHICH VESSEL TYPE IS WRONG              ###
   #########################################################################




### TRIPS WRONGLY CLASSIFIED AS "NO VESSEL" (GEARS DO NOT MATCH)
gear_general <- as.data.frame(tripinfo[,c("trip","gear_general")] %>% inner_join(data.frame(trip = effort$trip), by = "trip"))$gear_general
trips = effort$trip[which(gear_general == "hook_line_shore" & 
                             !effort$gear_type == "handline")]
tripinfo$vessel[which(tripinfo$trip %in% trips)] <- "canoa"
tripinfo$engine[which(tripinfo$trip %in% trips)] <- NA
tripinfo$fuel[which(tripinfo$trip %in% trips)] <- NA

tripinfo$vessel[which(tripinfo$trip %in% "a40ddaef-544f-48b4-a1ad-be9e6ef1058a")] <- "canoa"
tripinfo$engine[which(tripinfo$trip %in% "a40ddaef-544f-48b4-a1ad-be9e6ef1058a")] <- NA
tripinfo$fuel[which(tripinfo$trip   %in% "a40ddaef-544f-48b4-a1ad-be9e6ef1058a")] <- NA

tripinfo$arrival[tripinfo$trip == "d5dfc9d7-97ae-44f7-874d-27d40c2a280e"] <- "23:50:00"

tripinfo$propulsion[which(tripinfo$trip == "dc0b23fb-1941-44f0-9e20-1742522f9335")] <- "remo-vela"


### CHANGE LOCAL NAME TO "FIO NA PEDRA" FOR CONSISTENCY
vessel <- as.data.frame(tripinfo[,c("trip","vessel")] %>% inner_join(data.frame(trip = effort$trip), by = "trip"))$vessel
positions = which(gear_general == "hook_line_shore" & 
                             !vessel == "canoa" &
                             !effort$gear == "FIO-NA-PEDRA")
if(length(positions) > 0) effort$gear[positions] <- "FIO-NA-PEDRA"

plasticbait <- c("BRINDADO", "BRINDADO, CEBOLA", "PALHA", "PALHA, BRINDADO", 
                 "PLASTICO" , "PLASTICO, BRINDADO", "PLASTICO, PALHA", 
                 "PLASTICO, PALHA, BRINDADO")






### CHANGE LOCAL NAME TO "COSTUMADO" AND "TOCA" FOR CONSISTENCY
positions = which(!gear_general == "hook_line_shore" & 
                            vessel == "canoa" &
                            effort$gear == "FIO-NA-PEDRA" &
                            !effort$bait %in% plasticbait)
if(length(positions) > 0) effort$gear[positions] <- "COSTUMADO"


positions = which(!gear_general == "hook_line_shore" & 
                    vessel == "canoa" &
                    effort$gear == "FIO-NA-PEDRA" &
                    effort$bait %in% plasticbait)
if(length(positions) > 0) effort$gear[positions] <- "TOCA"




### FIX TWO TRIPS THAT WERE USING BEACH SEINE
trips = "7a307914-c961-40d9-bc93-4ccf34f83d29"
effort$gear[which(effort$trip %in% trips)] <- "REDE-BARBUDO"
trips = "6e067e77-47e4-4ea9-8ae1-9e45ae008dcc"
effort$gear[which(effort$trip %in% trips)] <- "REDE-BARBUDO"
trips = "94ba7499-5c45-4bd3-8acc-53c7033cc8f4"
effort$gear[which(effort$trip %in% trips)] <- "REDE-BARBUDO"
tripinfo$vessel[which(tripinfo$gear_general == "beach_seine")] = "sem-embarcacao"
tripinfo$propulsion[which(tripinfo$gear_general == "beach_seine")] = "sem-embarcacao"
tripinfo$fuel[which(tripinfo$gear_general == "beach_seine")] = 0




trips = effort$trip[which(vessel == "sem-embarcacao" & 
                            !effort$gear == "FIO-NA-PEDRA"  & 
                            !effort$gear == "SUBMARINO" &
                            !effort$gear == "REDE-BARBUDO")]

if(length(trips) > 0){
  tripinfo$vessel[which(tripinfo$trip %in% trips)] <- "canoa"
  tripinfo$engine[which(tripinfo$trip %in% trips)] <- NA
  tripinfo$fuel[which(tripinfo$trip %in% trips)] <- NA
  }



### WRONG PROPULSION
propulsion <- as.data.frame(tripinfo[,c("trip","propulsion")] %>% inner_join(data.frame(trip = effort$trip), by = "trip"))$propulsion
vessel <- as.data.frame(tripinfo[,c("trip","vessel")] %>% inner_join(data.frame(trip = effort$trip), by = "trip"))$vessel

positions <- which(propulsion == "sem-embarcacao" & !vessel == "sem-embarcacao")
trips<- effort$trip[positions]
tripinfo$propulsion[which(tripinfo$trip %in% trips)] <- "remo-vela"




rm(positions,vessel,plasticbait,gear_general, propulsion,trips)









    #########################################################################
    ###                          TIME AT THE SEA                          ###
    #########################################################################





                #---------------------------------------------#
                #                EFFORT TIMES                 #
                #---------------------------------------------#


### Change effort times in which the times were switched
switched.times <- rbind(
  c("288b86fa-85db-4058-8162-15d4c5f8fcf4","FIO-NA-PEDRA"),
  c("29a04e30-6711-430d-85a9-6069b8ef7ff4","RABO"),
  c("dd9d8915-5d03-4029-97a2-e36e6937d868","REDE-VOADOR"),
  c("26bfba67-290b-4e52-b14d-0601ada3c4f0","FIO-JOGADO"),
  c("721c893b-9e79-45f0-b37d-6a6a60844689","TOCA"),
  c("51506339-0505-418b-a4ec-e94a8886e634","FUNDO"),
  c("036b5073-c297-43be-89d1-d69334e7dba2","FIO-JOGADO"),
  c("036b5073-c297-43be-89d1-d69334e7dba2","CORICO-DE-FIO-GROSSO"),
  c("ce3a5de1-07c5-43cf-a08d-c5353897619a","CORICO-DE-FULU-FULU"),
  c("2eec133e-47d6-49f0-a45b-8e8f02ff4842","CORICO-DE-FIO-GROSSO"),
  c("49551906-43bb-4184-9049-d48866e68763", "COSTUMADO")
)
for(i in 1:nrow(switched.times)){
  trip = switched.times[i,1]
  gear = switched.times[i,2]
  start = effort$end[which(effort$trip == trip & effort$gear == gear)]
  end   = effort$start[which(effort$trip == trip & effort$gear == gear)]
  effort$end[which(effort$trip == trip & effort$gear == gear)] = end
  effort$start[which(effort$trip == trip & effort$gear == gear)] = start}

rm(trip,end, start, switched.times,gear, i, tolist,
   totext, rawfiles)




                #---------------------------------------------#
                #            FISHING TIMES FUNCTION           #
                #---------------------------------------------#




totime <- function(time) {
  as.integer(substr(time,1,2)) + 
    (as.integer(substr(time,4,5))/60)}




                #---------------------------------------------#
                #          DETECT TEST QUESTIONNAIRES         #
                #---------------------------------------------#




### Check whether there are test questionnaires in the data
temp <- data.frame(start = totime(effort$start),
                   end   = totime(effort$end),
                   trip = effort$trip)
temp <- rbind(temp,
              data.frame(
                start = NA,
                end = NA,
                trip = tripinfo$trip[which(!tripinfo$trip %in% effort$trip)]
              ))
temp = as.data.frame(data.frame(trip = tripinfo$trip,
                  departure = totime(tripinfo$departure),
                  arrival = totime(tripinfo$arrival)) %>%
  inner_join(temp, by = c("trip")))


temp <- as.data.frame(
  temp  %>% rowwise() %>% mutate(max = max(start,end,arrival,departure))
   )
temp <- as.data.frame(
  temp  %>% rowwise() %>% mutate(min = min(start,end,arrival,departure))
   )
temp$dif <- abs(temp$max - temp$min)
test <- temp$trip[which(temp$dif< 1)]
rm(temp,test)







                #---------------------------------------------#
                #                FISHING TIMES                #
                #---------------------------------------------#






### RETRIEVE MAXIMUM AND MINIMUM FISHING TIMES OF EACH FISHING TRIP
efforttimes <- function(maxmin){
  unlist(lapply(levels(as.factor(effort$trip)), function(trip) {
    maxminfunc = get(maxmin)
    if(maxmin == "max") times1 = effort$end[which(effort$trip == trip)]
    if(maxmin == "min") times1 = effort$start[which(effort$trip == trip)]
    times2 = as.POSIXct(times1, format = c("%H:%M:%S"), tz = "GMT")
    times1[which(times2 == maxminfunc(times2))][1]
  }
  ))}
efforttimes <- data.frame(trip = levels(as.factor(effort$trip)),
                   max  = efforttimes("max"),
                   min  = efforttimes("min"))
efforttimes <- rbind(efforttimes,
              data.frame(trip = tripinfo$trip[which(!tripinfo$trip %in% efforttimes$trip)],
                         max  = NA,
                         min  = NA))
efforttimes <- data.frame(trip = tripinfo$trip)    %>%   inner_join(efforttimes,by=c("trip"))

timefishing <- totime(efforttimes$max) - totime(efforttimes$min)
departuredate <- tripinfo$date
departuredate[which(timefishing<0)] <- tripinfo$date[which(timefishing<0)]-(24*60*60)

# Maximum and minimum fishing times
tripinfo$maxf   <- as.POSIXct(paste(tripinfo$date, efforttimes$max), format = c("%Y-%m-%d %H:%M:%S"), tz = "GMT")
tripinfo$minf   <- as.POSIXct(paste(departuredate, efforttimes$min), format = c("%Y-%m-%d %H:%M:%S"), tz = "GMT")
timefishing     <- as.numeric(tripinfo$maxf - tripinfo$minf)/3600
rm(efforttimes, departuredate)





                #---------------------------------------------#
                #               TIME AT THE SEA               #
                #---------------------------------------------#





### CALCULATE FISHING TIMES AS POSIX.CT
timeseatrip <- totime(tripinfo$arrival) - totime(tripinfo$departure)
departuredate <- tripinfo$date
departuredate[which(timeseatrip<0)] <- tripinfo$date[which(timeseatrip<0)]-(24*60*60)
tripinfo$arrival   <- as.POSIXct(paste(tripinfo$date, tripinfo$arrival), format = c("%Y-%m-%d %H:%M:%S"), tz = "GMT")
tripinfo$departure <- as.POSIXct(paste(departuredate, tripinfo$departure), format = c("%Y-%m-%d %H:%M:%S"), tz = "GMT")
time_sea <- function() as.numeric(tripinfo$arrival-tripinfo$departure)/3600
rm(departuredate, timeseatrip)


# Arrival time minus time they stopped fishing
timetoarrival      <- function() as.numeric((tripinfo$arrival-tripinfo$maxf)/3600)

# Time they started fishing minus time they departed
timesincedeparture <- function() as.numeric((tripinfo$minf-tripinfo$departure)/3600)


# How long since they finished fishing until they returned?
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "surface_gillnet")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "spear_fishing")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "hook_line")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "hook_line_shore")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "seine_net")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "circular_net")]))
median(na.omit(timetoarrival()[which(tripinfo$gear_general == "demersal_gillnet")]))





              #---------------------------------------------#
              #            FISHING TIMES SUMMARY            #
              #---------------------------------------------#


timeastext <- function(time) substr(time,12,21)
  
# Table to view fishing times and departure and landing times simultaneously
fishing_times <- function()
  data.frame(gear_general = tripinfo$gear_general,
             arrival = timeastext(tripinfo$arrival), 
             departure = timeastext(tripinfo$departure),
             timeseatrip = time_sea(),
             maxf = timeastext(tripinfo$maxf),
             minf = timeastext(tripinfo$minf),
             timefishing = timefishing,
             ground = tripinfo$ground,
             site = tripinfo$site,
             trip = tripinfo$trip)

fishing_times()







              #---------------------------------------------#
              #            FIX VERY SHORT TRIPS             #
              #---------------------------------------------#



# Fix trips in which the time they landed was earlier than the time they finished
# fishing
gears = c("surface_gillnet", "hook_line", "hook_line_shore",
         "seine_net","circular_net","spear_fishing", "demersal_gillnet")
positions <- which(
  time_sea() < 2 &
    tripinfo$valid  &
    timetoarrival()<0 &
    time_sea() < timefishing &
    timefishing < 10
    ) 

plot(time_sea(),timefishing)
if(length(positions) > 0)    for(i in positions) {
  # Change landing times by summing the median time to arrival to the maximum
  # fishing time. If needed, subtract median time since departure to minimum
  # fishing time.
     gear = tripinfo$gear_general[i]
     median.timetoarrival        = median(na.omit(
       timetoarrival()[which(tripinfo$gear_general == gear)]
               ))
     if(timesincedeparture()[i] < median.timetoarrival & timesincedeparture()[i] > 0)
       timedif = timesincedeparture()[i]*(60*60) else timedif = median.timetoarrival*(60*60)
     tripinfo$arrival[i] = tripinfo$maxf[i] + timedif
     if(timesincedeparture()[i] < 0)   {
     median.timesincedeparture <- median(na.omit(
       timesincedeparture()[which(tripinfo$gear_general == gear)]
        )) * 3600
     tripinfo$departure[i] <- tripinfo$minf[i] - median.timesincedeparture
      }
    }
rm(gear,median.timesincedeparture,median.timetoarrival,i, timedif)
plot(time_sea(),timefishing)





# Fix trips with wrong departure times but correct landing times
positions <- which(
  time_sea() < 1 &
    timefishing < 10 &
    tripinfo$valid  &
    timetoarrival()<3 &
    timetoarrival()>0 &
    timesincedeparture() < 0 &
    time_sea() < timefishing)
if(length(positions > 0))
  tripinfo$departure[positions] <- (tripinfo$minf - 
                                      (tripinfo$arrival -tripinfo$maxf))[positions]
plot(time_sea(),timefishing)





# Fix trips with same arrival and departure time
positions <- which(
  time_sea() < 1 &
    timefishing < 10 &
    tripinfo$valid  &
    timesincedeparture() < 0 &
    totime(timeastext(tripinfo$minf)) - totime(timeastext(tripinfo$departure)) < 0 &
    timetoarrival() == 0 &
    time_sea() < timefishing)
if(length(positions) > 0)    for(i in positions) {
  gear = tripinfo$gear_general[i]
  median.timesincedeparture <- median(na.omit(
      timesincedeparture()[which(tripinfo$gear_general == gear)]
    )) * 3600
  tripinfo$departure[i] <- tripinfo$minf[i] - median.timesincedeparture
}
plot(time_sea(),timefishing)
rm(gear,median.timesincedeparture,positions,i)








# Wrong landing time, correct departure time but wrong departure date
positions <- which(
  time_sea() < 1 &
    timefishing < 10 &
    tripinfo$valid  &
    timesincedeparture() < (-20) &
    time_sea() < timefishing)

if(length(positions) > 0)    for(i in positions) {
  tripinfo$departure[i] <- tripinfo$departure[i] -(3600*24)
  tripinfo$arrival[i] <- tripinfo$maxf[i] + (tripinfo$minf[i] - tripinfo$departure[i])
}
plot(time_sea(),timefishing)






# Everything wrong, fix using fishing times as reference
positions <- which(
  time_sea() < 1 &
    timefishing < 10 &
    tripinfo$valid  &
    time_sea() < timefishing)

if(length(positions) > 0)    for(i in positions) {
  # Change landing times by summing the median time to arrival to the maximum
  # fishing time. If needed, subtract median time since departure to minimum
  # fishing time.
  gear = tripinfo$gear_general[i]
  median.timetoarrival        = median(na.omit(
    timetoarrival()[which(tripinfo$gear_general == gear)]
  ))*3600
  tripinfo$arrival[i] = tripinfo$maxf[i] + median.timetoarrival
  median.timesincedeparture <- median(na.omit(
    timesincedeparture()[which(tripinfo$gear_general == gear)]
    )) * 3600
  tripinfo$departure[i] <- tripinfo$minf[i] - median.timesincedeparture
  }

plot(time_sea(),timefishing)
rm(gear,median.timesincedeparture,median.timetoarrival,i)







             #---------------------------------------------#
             #             FIX VERY LONG TRIPS             #
             #---------------------------------------------#



### INDIVIDUAL ERRORS VERY LONG TRIPS
# Fix one trip in Praia Burras (sum 5 min to maximum fishing time, trip looks
# like a vessel that went out fishing opportunistically and in front of the beach
#  because there was a shoal)
temp = which(tripinfo$trip == "cce06405-86fd-4d43-8e22-cb2cf6d49536")
fishing_times()[temp,]
tripinfo$arrival[temp] <- 
  tripinfo$maxf[temp] + (5*60) 
tripinfo$departure[temp] <- 
  tripinfo$departure[temp]  + (24*60*60)
fishing_times()[temp,]


# Fix one trip in Malanza (looks like only departure time is wrong)
temp = which(tripinfo$trip == "00000000-0000-0000-0000-000000000505")
fishing_times()[temp,]
tripinfo$departure[temp] <- 
  tripinfo$minf[temp] - (tripinfo$arrival[temp]-tripinfo$maxf[temp])
fishing_times()[temp,]


# Fix one trip in P. Seca in which it seems they changed landing and depart. times
switched.times <- c("49a5435f-95ba-467a-9342-648d469f5cfc","a2efd97c-5162-4fcd-8b3c-04628364b104")
temp = which(tripinfo$trip %in% switched.times)
fishing_times()[temp,]
tripinfo$departure[temp] <- tripinfo$arrival[temp]
tripinfo$arrival[temp] <- tripinfo$maxf[temp] + (tripinfo$minf[temp] - tripinfo$departure[temp])
fishing_times()[temp,]




# Confounded 8 with 20h
temp = which(tripinfo$trip == "759d5c24-084b-4bf8-960d-dd5b682ea4f5")
fishing_times()[temp,]
tripinfo$departure[temp] <- tripinfo$departure[temp] + (12*60*60)
fishing_times()[temp,]


# Confounded midnight with midday
temp = which(tripinfo$trip == "f3aef8d0-bc0e-4c2f-ad25-e4856365f864")
fishing_times()[temp,]
tripinfo$departure[temp] <- tripinfo$departure[temp] + (12*60*60)
fishing_times()[temp,]


# Times make no sense, substitute with NA
temp = which(tripinfo$trip == "117f53bf-95f8-4fa4-aa32-87f0a429db5f")
fishing_times()[temp,]
tripinfo$departure[temp] = NA
tripinfo$arrival[temp] = NA
fishing_times()[temp,]






#### DEPARTURE TIME IS WRONG
positions <- which(
  time_sea() > 22 &
    timefishing < 10 &
    timetoarrival() < 3.1 &
    timetoarrival() > 0 &
    tripinfo$valid
     )

if(length(positions) > 0) {
    tripinfo$departure[positions] = (tripinfo$minf - (tripinfo$arrival - tripinfo$maxf))[positions]
    }
plot(time_sea(),timefishing)








### ARRIVAL TIME IS WRONG
positions <- which(
  time_sea() > 22 &
    timefishing < 10 &
    timesincedeparture() < 2 &
    timesincedeparture() > 0 &
    tripinfo$valid
)
if(length(positions) > 0) {
  tripinfo$arrival[positions] = (tripinfo$maxf + (tripinfo$minf - tripinfo$departure))[positions]
}
plot(time_sea(),timefishing)







### ERRORS IN WHICH ARRIVAL TIME AND IS WRONG AND DEPARTURE DATE IS WRONG
positions <- which(
  time_sea() > 22 &
    timefishing < 10 &
    totime(fishing_times()$minf) - totime(fishing_times()$departure) < 2 &
    totime(fishing_times()$minf) - totime(fishing_times()$departure) > 0 &
    tripinfo$valid  &
    !tripinfo$trip %in% c("7a307914-c961-40d9-bc93-4ccf34f83d29")
)
if(length(positions) > 0) {
  temp <- (totime(fishing_times()$minf) - totime(fishing_times()$departure))[positions] * 3600
  tripinfo$arrival[positions]    =   tripinfo$maxf[positions] + temp
  tripinfo$departure[positions]  = tripinfo$departure[positions] + (3600*24)
}
plot(time_sea(),timefishing)











### ERRORS IN WHICH ARRIVAL TIME IS WRONG
positions = which(
  time_sea() > 22 &
    timefishing < 10 &
    totime(fishing_times()$minf) - totime(fishing_times()$departure) < 3.1 &
    totime(fishing_times()$minf) - totime(fishing_times()$departure) > 0 &
    tripinfo$valid &
    !tripinfo$trip %in% c("7a307914-c961-40d9-bc93-4ccf34f83d29",
                          "5139f224-e2af-4e6d-bade-1dba5473855b")
)
if(length(positions) > 0) {
  tripinfo$arrival[positions]    =   
    tripinfo$maxf[positions] + 
    (totime(fishing_times()$minf) - 
       totime(fishing_times()$departure))[positions] * 3600
  wrongdeparturedate = positions[(tripinfo$minf - tripinfo$departure)[positions]/(3600)>24]
  tripinfo$departure[wrongdeparturedate] = tripinfo$departure[wrongdeparturedate] + (3600*24)
  rm(wrongdeparturedate)
}
plot(time_sea(),timefishing)







### ERRORS IN WHICH EVERYTHING TIME IS WRONG
positions <-   which(
  time_sea() > 22 &
    timefishing < 10 &
    tripinfo$valid  &
    tripinfo$gear_general %in% gears[!gears == "demersal_gillnet"]
)
patata = positions
for(i in which(!gears == "demersal_gillnet")) {
  gear = gears[i]
  # Locate entries that need changing
  positions = which(
    time_sea() > 22 &
      timefishing < 10 &
      tripinfo$valid  &
      tripinfo$gear_general %in% gear
  )
  # Change landing times by summing the median time to arrival to the maximum
  # fishing time. If needed, subtract median time since departure to minimum
  # fishing time.
  if(length(positions) > 0) {
    median.timetoarrival        = median(na.omit((tripinfo$arrival-tripinfo$maxf)[which(tripinfo$gear_general == gear)]))
    tripinfo$arrival[positions] = tripinfo$maxf[positions] + median.timetoarrival
    median.timesincedeparture = median(na.omit((tripinfo$minf-tripinfo$departure)[which(tripinfo$gear_general == gear)]))
    tripinfo$departure[positions] = tripinfo$minf[positions] - median.timesincedeparture
    rm(median.timesincedeparture, median.timetoarrival)
  }
}
plot(time_sea(),timefishing)
rm(patata, temp, positions, i, gear)









### FIX WRONG DATES OF MAXIMUM AND MINIMUM FISHING TIMES
wrongdates <- which(timesincedeparture()>23.999999,
                    timetoarrival() < 0)
if(length(wrongdates > 0)) {
  tripinfo$maxf[wrongdates] <- tripinfo$maxf[wrongdates] - (24*60*60)
  tripinfo$minf[wrongdates] <- tripinfo$minf[wrongdates] - (24*60*60)
}
rm(wrongdates, switched.times)





### SPECIFIC ERRORS
position <- which(tripinfo$trip == "f110a40c-5798-407e-842d-2bdf0ffeca79")
tripinfo$arrival[position] <- tripinfo$arrival[position] - (12*3600)
tripinfo$departure[position] <- tripinfo$departure[position] - (24*3600)


position <- which(tripinfo$trip == "e2a6b9f8-f654-492f-81c6-97c1b0901809")
tripinfo$departure[position] <- tripinfo$arrival[position] - (4*3600)




   #########################################################################
   ###                           FISHING TIMES                           ###
   #########################################################################



#effortoriginal <- effort
#effort <- effortoriginal
substracttime <- function(time)   ((as.integer(time)/ (24*60*60)) - floor(as.integer(time)/ (24*60*60))) * 24


               #---------------------------------------------#
               #                EFFORT TIMES                 #
               #---------------------------------------------#

#effort <- effort[,!colnames(effort) %in% c("arrival.x", "departure.x")]
#colnames(effort)[c(2,3,4)] <- c("date","arrival", "departure")


effort <-as.data.frame(tripinfo[,c("trip","site", "gear_general","date","arrival","departure")]    %>%   inner_join(effort,by=c("trip")))
effort$ngears <- unlist(lapply(effort$trip, function(trip) length(which(effort$trip == trip)))) 



### ADD DATES
effort$startdate <- ""
positions <- which(totime(effort$start) < totime(effort$end))
effort$startdate[positions] <- substr(effort$date[positions],1,10)
positions <- which(totime(effort$start) > totime(effort$end))
effort$startdate[positions] <- substr(effort$date[positions] - (24*3600),1,10)
positions <- which(totime(effort$start) == totime(effort$end))
effort$startdate[positions] <- substr(effort$date[positions],1,10)
positions <- which(totime(effort$start) > substracttime(effort$arrival) &
                     totime(effort$end) > substracttime(effort$arrival) &
                     totime(effort$start) < totime(effort$end))
effort$startdate[positions] <- substr(effort$date[positions] - (24*3600),1,10)
effort$startdate[effort$startdate == "" & !is.na(effort$start)] <- effort$date[effort$startdate == "" & !is.na(effort$start)]


effort$enddate <- substr(effort$date, 1,10)
positions <- which(totime(effort$start) > substracttime(effort$arrival) &
                   totime(effort$end) > substracttime(effort$arrival) &
                   totime(effort$start) < totime(effort$end))
effort$enddate[positions] <- substr(effort$date[positions] - (24*3600),1,10)
effort$enddate[which(is.na(effort$enddate))] <- ""



positions <- which(!effort$startdate == "")
effort$start <- as.POSIXct(paste(effort$startdate, effort$start),format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

positions <- which(!effort$enddate == "")
effort$end <- as.POSIXct(paste(effort$enddate, effort$end),format = "%Y-%m-%d %H:%M:%S", tz = "GMT")


### MAX AND MIN TIMES
ismaxtime <- unlist(lapply(c(1:nrow(effort)), function(i) 
                  effort$end[i] == max(effort$end[which(effort$trip == effort$trip[i])])
                           ))
ismintime <- unlist(lapply(c(1:nrow(effort)), function(i) 
  effort$start[i] == min(effort$start[which(effort$trip == effort$trip[i])])
))



### TIME AT THE SEA AND TIME FISHING
effort$time_sea <- as.numeric(effort$arrival - effort$departure)/(60*60)
time_fishing <- function() as.numeric(effort$end - effort$start) / 3600



### DYNAMIC TABLE TO VIEW FISHING TIMES AND TIME AT THE SEA SIMULTANEOUSLY
fishing_times <- function()
  data.frame(gear_type    = effort$gear_type,
             gear_general = effort$gear_general,
             arrival = timeastext(effort$arrival), 
             departure = timeastext(effort$departure),
             timeseatrip = effort$time_sea,
             start = timeastext(effort$start),
             end = timeastext(effort$end),
             timefishing = time_fishing(),
             ngears      = effort$ngears,
             site = effort$site,
             trip = effort$trip)



### UPDATE MAXIMUM AND MINIMUM FISHING TIMES IN TRIPINFO DATABASE
temp <- lapply(tripinfo$trip, function(trip) {
  temp = effort$end[which(effort$trip == trip & ismaxtime)][1]
  if(length(temp) == 0) temp = as.POSIXct(NA, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  return(temp)})

for(i in 1:nrow(tripinfo)) {
  tripinfo$maxf[i] <- temp[[i]]
}


### UPDATE MAXIMUM AND MINIMUM FISHING TIMES IN TRIPINFO DATABASE
temp <- lapply(tripinfo$trip, function(trip) {
  temp = effort$start[which(effort$trip == trip & ismintime)][1]
  if(length(temp) == 0) temp = as.POSIXct(NA, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  return(temp)})

for(i in 1:nrow(tripinfo)) {
  tripinfo$minf[i] <- temp[[i]]
}





positions <- which(time_fishing() > effort$time_sea & 
                     effort$departure < effort$start & 
                     time_fishing() < 10)

temp <- positions[effort$trip[positions] %in% effort$trip[which(ismaxtime)] & 
                  !is.na(timesincedeparture()[positions]) & 
                  timesincedeparture()[positions] > 0 &
                  timesincedeparture()[positions] < 3.5]
for(i in temp)  tripinfo$arrival[tripinfo$trip == effort$trip[i]] = effort$end[i] + (timesincedeparture()[i] * 60 * 60)
plot(time_sea(),timefishing)
updatetimes <-   tripinfo[,c("trip","arrival","departure")]    %>%   inner_join(data.frame(trip = effort$trip),by=c("trip"))
effort$arrival <- updatetimes$arrival
effort$time_sea <- as.numeric(effort$arrival - effort$departure)/(60*60)



positions <- which(effort$time_fishing > effort$time_sea + 5 & effort$time_fishing > 10)
effort$gear_type[positions]
effort$time_fishing[positions] <- NA
effort$start[positions] <- NA
effort$end[positions] <- NA



positions <- which(effort$time_fishing > 15)
effort$time_fishing[positions] <- NA
effort$start[positions] <- NA
effort$end[positions] <- NA



positions <- which(effort$time_fishing > 12 & effort$gear_subtype == "demersal_troll")
if(length(positions) > 0){
  effort$time_fishing[positions] <- NA
  effort$start[positions] <- NA
  effort$end[positions] <- NA  
}




















    #########################################################################
    ###########------------------ WRITE DATABASE ------------------###########
    #########################################################################






tripinfo <- tripinfo[c("trip","island", "site", "landing_site","ext_worker", "date","arrival",
                       "departure", "vessel","propulsion","engine","n_fishers",
                       "name_fishers","participation","catch", "consumption", "sao_pedro", "ground", 
                       "weather","current", "material", "fuel", "price_fuel", "problems","valid")]

tripinfo$departure <- as.character(substr(tripinfo$departure,12,16))
tripinfo$arrival <- as.character(substr(tripinfo$arrival,12,16))

write.csv(tripinfo, paste0(getwd(),"/data/tripinfo.csv"), row.names=FALSE)



effort$start <- as.character(substr(effort$start,12,16))
effort$end   <- as.character(substr(effort$end,12,16))
keep <- c("effort_id","trip","gear","gear_type","gear_subtype","nsets","start","end","light","bait","n_lines", 
            "n_hooks", "hook_size","net_length","net_depth","mesh_size","catch_composition")


write.csv(effort[keep], paste0(getwd(),"/data/effort.csv"), row.names=FALSE)


