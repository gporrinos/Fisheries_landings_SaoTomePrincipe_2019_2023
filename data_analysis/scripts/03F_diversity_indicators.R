





#########################################################################
#########################################################################
#                   ANALYSIS OF DIVERSITY INDICATORS                    #
#########################################################################
#########################################################################



# This script calculates diversity indicators and adds them to "tripinfo" and 
# "effort" databases






#########################################################################
#                             DIRECTORIES                               #
#########################################################################


## Directories exist? If not, create them
figures    <- file.path(getwd(), "data_analysis", "outputs", "figures")
if(!file.exists(figures)) dir.create(figures)

tables     <- file.path(getwd(), "data_analysis", "outputs", "tables")
if(!file.exists(tables)) dir.create(tables)

r_objects  <- file.path(getwd(), "data_analysis", "outputs", "r_objects")
if(!file.exists(r_objects)) dir.create(r_objects)

modelobjects  <- file.path(getwd(), "data_analysis", "outputs", "modelobjects")
if(!file.exists(modelobjects)) dir.create(modelobjects)







#########################################################################
#                               LOAD DATA                               #
#########################################################################





### RUN SCRIPTS 1, 2A AND 2B
# 1  -> LOADS & CLEANS DATA
# 2A -> ADDS COVARIATES TO CATCH AND EFFORT
# 2B -> CALCULATES DIVERSITY INDICATORS
# source(file.path("data_analysis","scripts","02B_variable_def_diversity_indicators.R"))



startyear              <- c("2019-01-01","2020-01-01","2021-01-01","2022-01-01","2023-01-01")
startyear              <- as.integer(as.POSIXct(startyear,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)


ntrips   <- read.csv(file.path(getwd(),"data_analysis", "data_with_covariates","ntrips.csv"))
catch    <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","catch.csv"),    header=TRUE, sep = ",")
effort   <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","effort.csv"),   header=TRUE, sep = ",")
tripinfo <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","tripinfo.csv"), header=TRUE, sep = ",")
species  <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","species.csv"),  header=TRUE, sep = ",")





### REMOVE NON-VALID OBSERVATIONS
remove <- tripinfo$trip[which(!tripinfo$valid | tripinfo$participation == "nao")]
effort <- effort[-which(effort$trip %in% remove),]
catch  <- catch[-which(catch$trip %in% remove),]
tripinfo    <- tripinfo[-which(tripinfo$trip %in% remove),]

### RE-LABEL ISLAND CATEGORIES
gear       = c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net",
               "hook_line",   "hook_line_shore",    "spear_fishing", "demersal_line",
               "demersal_troll", "handline", "surface_troll")
gear_label = c("Circular net", "Surface gillnet", "Demersal gillnet", "Seine net",
               "Hook & line", "Hook & line (shore)","Spear fishing", "Demersal line",
               "Demersal troll", "Handline", "Surface troll")







#########################################################################
###                           LOAD PACKAGES                           ###
#########################################################################


# Data management
if(!require("dplyr"))   install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)

# Run GAMs
if(!require("mgcv"))     install.packages("mgcv", repos = "https://cloud.r-project.org")
library(dplyr)
if(!require("performance"))     install.packages("performance", repos = "https://cloud.r-project.org")
library(performance)

# Model selection
if(!require("MuMIn"))    install.packages("MuMIn", repos = "https://cloud.r-project.org")
library(MuMIn)

# Plot things
if(!require("ggplot2"))  install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require("cowplot"))  install.packages("cowplot", repos = "https://cloud.r-project.org")
library(cowplot)
if(!require("gratia"))   install.packages("gratia", repos = "https://cloud.r-project.org")
library(gratia)







#########################################################################
###              SUMMARY OF OBSERVED DIVERSITY INDICATORS             ###
#########################################################################



### FUNCTION THAT RETURNS VALUES AS TEXT, ROUNDED TO ONE DECIMAL PLACE (EVEN IF ENDED IN 0)
roundtodecimal <- function(dat)     
  if(nchar(as.character(round(round(dat,1)-round(dat),1)))<3) 
    paste0(round(dat,1),".0") else round(dat,1)



### FUNCTION TO SUMMARISE DIVERSITY INDICATOR OF EACH GEAR CATEGORY
create.table <- function(dat, gear_category,gearlist){
    island   = c("Príncipe","São Tomé")
    dat$gear = dat[,which(colnames(dat) == gear_category)]
    divindicatorsummary   = as.data.frame(expand.grid(island = island,gear = gearlist))[,c(2,1)]
    divindicatorsummary$n = unlist(
       lapply(1:nrow(divindicatorsummary),
           function(i)    
              length(which(dat$gear == divindicatorsummary$gear[i] & 
                          dat$island == divindicatorsummary$island[i]))
      ))
    for(j in 1:4) { 
       diversityindicator    = c("weight", "richness", "evenness", "MTL")[j]
       nam                   = colnames(divindicatorsummary)
       divindicatorsummary$a = unlist(lapply(1:nrow(divindicatorsummary),function(i) {   
         output = dat[,which(colnames(dat)  == diversityindicator)]
         output = na.omit(output[which(dat$gear == divindicatorsummary$gear[i] & 
                                 dat$island       == divindicatorsummary$island[i])])
         if(length(output)==0)     "0.0 (0.0)"     else         paste0(roundtodecimal(mean(output)), " (", roundtodecimal(sd(output)), ")")
        }))
       colnames(divindicatorsummary) = c(nam,diversityindicator)
    }
    return(divindicatorsummary)}
    


### Save table summary of diversity indicators per main gear type
create.table(tripinfo,
             "gear_general",
             c("hook_line","hook_line_shore","surface_gillnet","seine_net",
               "demersal_gillnet","circular_net","spear_fishing"))

create.table(effort,
             "gear_type",
             c("demersal_line","demersal_troll","surface_troll","handline"))





#########################################################################
###                            FIT MODELS                             ###
#########################################################################





### STEP 1: FUNCTION FOR AIC MODEL SELECTION 
# Sourced from script below
source(file.path("data_analysis","scripts","00_all_models_FUNCTION.R"))
all_models









divmods <- function(effectlist,diversityindicator,gear,dat, deltaAIC = 6, freecores = 3, startfromend = FALSE){
    directory = paste(gear,diversityindicator,sep="_")
    modeloutputs = file.path(modelobjects,paste0(directory,"_",deltaAIC,".RDS")) 
  if(file.exists(modeloutputs)){
      mod = readRDS(modeloutputs)
   } else {
      f = as.formula(paste(diversityindicator, "~", effectlist))
      if(diversityindicator %in% c("weight","MTL"))
        mod = all_models(formula = f, dat = dat, family = gaussian, directory = directory, freecores = freecores)
      if(diversityindicator == "richness"){
        fam = "TRY"
        if(file.exists(file.path(r_objects,directory))) {
          files = list.files(file.path(r_objects,directory),full.names = TRUE)
          if(!length(files) == 0) fam = family(readRDS(files[1]))$family}
        if(fam == "TRY")
          if(check_overdispersion(mgcv::gam(f,family = poisson, data = dat, method = "REML")
                                  )$p_value < 0.05) fam = "nb" else fam = "poisson"
        if(fam == "poisson"){
          mod = all_models(formula = f, dat = dat, family =  poisson, directory = directory, freecores = freecores)
        } else {
          mod = all_models(formula = f, dat = dat, family =  mgcv::nb, directory = directory, freecores = freecores)
        }
                                  }
      if(diversityindicator == "evenness")
        mod = all_models(formula = f, dat = dat, family =  mgcv::betar, directory = directory, freecores = freecores)
      globalmodel = unlist(lapply(mod,function(mod) length(labels(terms(mod$formula)))))
      globalmodel = which(globalmodel == max(globalmodel))
      bestmodels  = as.numeric(rownames(
              subset(subset(MuMIn::model.sel(mod),!nested(.)),delta<deltaAIC)
                                 ))
      if(!globalmodel %in% bestmodels) mods = c(globalmodel,bestmodels) else mods = bestmodels
      mod = mod[mods]
      saveRDS(mod,modeloutputs)}
      return(mod)}



tripinfo$island <- as.factor(tripinfo$island)
effort$island <- as.factor(effort$island)




#--------------------------------------------#
###               Hook and line             #####
#--------------------------------------------#




#### 1. SELECT DATA
dat <- tripinfo[which(tripinfo$gear_general %in% c("hook_line","hook_line_shore")),]


### 2. EFFECTS                                      
effectlist <-   "s(date)  +
                 s(ordinaldate, bs = 'cc')  +
                 island  +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc')  +
                 propulsion  +
                 n_fishers   +
                 s(time_sea)   +
                 ngears" 

### 3. FIT MODELS
hook_line_weight     <- divmods(effectlist,"weight","hook_line",dat)
hook_line_richness   <- divmods(effectlist,"richness","hook_line",dat)






#--------------------------------------------#
###               DEMERSAL LINES             #####
#--------------------------------------------#



dat <- effort[which(effort$gear_type == "demersal_line"),]
  
### STEP 1: Effects
effectlist <-   "s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(hook_size) +
                 s(n_hooks) +
                 s(time_fishing) +
                 gear_subtype + 
                 propulsion"



### 3. FIT MODELS
demersal_line_weight   <- divmods(effectlist,"weight","demersal_line",dat)
demersal_line_richness   <- divmods(effectlist,"richness","demersal_line",dat)







#------------------------------------#
#--------- Jigging handline ---------#
#------------------------------------#



dat <- effort[which(effort$gear_type == "handline"),]

### STEP 1: Effects                                
effectlist <- "s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(hook_size) +
                 s(n_hooks) +
                 n_lines +
                 bait_type +
                 s(time_fishing) +
                 island:bait_type +
                 propulsion"    



### 3. FIT MODELS
handline_weight   <- divmods(effectlist,"weight","handline",dat)
handline_richness   <- divmods(effectlist,"richness","handline",dat)







#---------------------------------#
#--------- Surface troll ---------#
#---------------------------------#

dat <- effort[which(effort$gear_type == "surface_troll"),]


### STEP 1: Effects
effectlist <-   "s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(hook_size) +
                 s(n_hooks) +
                 n_lines +
                 bait_type +
                 s(time_fishing) +
                 island:bait_type +
                 propulsion"                  


### STEP 3: Fit models
surface_troll_weight    <- divmods(effectlist,"weight","surface_troll",dat)
surface_troll_richness   <- divmods(effectlist,"richness","surface_troll",dat)




#---------------------------------#
#--------- Demersal troll --------#
#---------------------------------#


### STEP 1: Data
dat <- effort[which(effort$gear_type == "demersal_troll"),]
dat <- dat[-which(dat$island == "ST"),]

### STEP 2: Effects
effectlist <- c("s(date) +
                 s(ordinaldate, bs = 'cc') +
                 s(hook_size) +
                 s(n_hooks) +
                 s(time_fishing) +
                 propulsion")


### STEP 3: Fit models
demersal_troll_weight    <- divmods(effectlist,"weight","demersal_troll",dat)
demersal_troll_richness   <- divmods(effectlist,"richness","demersal_troll",dat)





#-----------------------------------#
#--------- Surface gillnet ---------#
#-----------------------------------#

### STEP 1: Define effects
dat <- effort[which(effort$gear_type == "surface_gillnet"),]

### STEP 2: Define effects
effectlist <- c("s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(mesh_size) +
                 s(net_depth) +
                 s(net_length) +
                 s(time_fishing)")


### STEP 3: Fit models
surface_gillnet_weight    <- divmods(effectlist,"weight","surface_gillnet",dat)
surface_gillnet_richness   <- divmods(effectlist,"richness","surface_gillnet",dat)



#-----------------------------------#
#--------- Demersal gillnet ---------#
#-----------------------------------#


### STEP 1: Data
dat <- effort[which(effort$gear_type == "demersal_gillnet"),]

### STEP 2: Define effects
effectlist <- c("s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(mesh_size) +
                 s(net_depth) +
                 s(net_length) +
                 s(time_fishing)")


### STEP 3: Fit models
demersal_gillnet_weight    <- divmods(effectlist,"weight","demersal_gillnet",dat)
demersal_gillnet_richness   <- divmods(effectlist,"richness","demersal_gillnet",dat)






#-----------------------------------#
#--------- Seine nets ---------#
#-----------------------------------#


### STEP 1: Data
dat <- effort[which(effort$gear_type == "seine_net"),]


### STEP 2: Effects
effectlist <- c("s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(mesh_size) +
                 s(net_depth) +
                 s(net_length) +
                 s(time_fishing) +
                 nsets")


### STEP 3: Fit models
seine_net_weight    <- divmods(effectlist,"weight","seine_net",dat)
seine_net_richness   <- divmods(effectlist,"richness","seine_net",dat)



#-----------------------------------#
#--------- Spear fishing ---------#
#-----------------------------------#


### STEP 1: Data
dat <- effort[which(effort$gear_type == "spear_fishing"),]


### STEP 2: Define effects
effectlist <- c("s(date) +
                 s(ordinaldate, bs = 'cc') +
                 island +
                 s(date, by = island) +
                 s(ordinaldate, by = island, bs = 'cc') +
                 s(time_fishing) +
                 n_fishers +
                 propulsion")

### STEP 3: Fit models
spear_fishing_weight    <- divmods(effectlist,"weight","spear_fishing",dat)
spear_fishing_richness   <- divmods(effectlist,"richness","spear_fishing",dat)









#########################################################################
###                          GET PREDICTIONS                          ###
#########################################################################




### STEP 1: FUNCTION TO CALCULATE PREDICTIONS
predictions <- function(gear, diversityindicator){
  
  # Get models
  mods        = get(paste(gear, diversityindicator, sep="_"))
  bestmodels  = get.models(MuMIn::model.sel(mods), subset = delta < 6)
  globalmodel = unlist(lapply(mods,function(mod) length(labels(terms(mod$formula)))))
  globalmodel = mods[[which(globalmodel == max(globalmodel))]]
  
  
  # Extract variable names and data
  vars = all.vars(formula(globalmodel))
  vars = vars[!vars %in% diversityindicator]
  dat  = globalmodel$model  
  if(!"island" %in% vars) dat$island = "PC"
  
  
  # Obtain estimates of partial smooth effects from gratia::smooth_estimates
  globalestimates = as.data.frame(gratia::smooth_estimates(globalmodel))
  
  for(i in 1:length(bestmodels)){
    if(length(bestmodels) > 1){
      weight = model.sel(bestmodels)$weight
    } else {weight = 1}
    # Calculate estimates of parameters present in model "i"
    if(length(which(grepl(")",formula(bestmodels[[i]])))) > 0){
      effectlist = levels(as.factor(as.data.frame(gratia::smooth_estimates(bestmodels[[i]]))$'.smooth'))
      for(j in 1:length(effectlist)){
        temp = as.data.frame(gratia::smooth_estimates(bestmodels[[i]], select = effectlist[j]))
        temp$'.estimate' = temp$'.estimate' * weight[i]
        temp$'.se' = temp$'.se' * weight[i]
        temp$variable <- colnames(temp)[6]
        colnames(temp)[6] <- "x"
        if(!"island" %in% colnames(temp)) temp$island = "NA"
        if(j == 1 & i == 1)  estimates = temp else estimates = rbind(estimates,temp)
      }
    }
    # Substitute estimates and standard error of parameters not present in model "i" by zero
    notinmodel = levels(as.factor(globalestimates$'.smooth'))[!levels(as.factor(globalestimates$'.smooth')) %in% levels(as.factor(temp$'.smooth'))]
    for(j in 1:length(notinmodel)){
      temp = as.data.frame(gratia::smooth_estimates(globalmodel, select = notinmodel[j]))
      temp$variable <- colnames(temp)[6]
      colnames(temp)[6] <- "x"
      if(!"island" %in% colnames(temp)) temp$island = "NA"
      temp$'.estimate' <- 0
      temp$'.se' <- 0
      estimates = rbind(estimates,temp)
    } 
  }
  estimates = estimates[,c(".by",".estimate",".se","x","island","variable")]
  colnames(estimates) <- c("by", "estimate", "se","x", "island","variable")
  sm = levels(as.factor(estimates$variable))
  
  # Create matrix with all variables for predicting non smooth variables
  notsm  = vars[!vars %in% colnames(globalestimates)]
  notsm  = c(notsm,"island")
  newdat = lapply(notsm,function(var) {
    output = levels(as.factor(dat[,which(colnames(dat) == var)]))
    if(var %in% c("island", "bait_type", "propulsion", "gear_subtype")) output else as.numeric(output)
    })
  names(newdat) = notsm
  
  allterms <- labels(terms(globalmodel$formula))
  allterms <- allterms[!grepl(")",allterms)]
  
  if(length(bestmodels) > 1) mod = model.avg(bestmodels) else mod = bestmodels[[1]]
  categorical <- allterms[which(allterms %in% c("island", "bait_type", "propulsion", "gear_subtype","island:bait_type"))]
  getcoef <- function(effect,statistic) if(length(which(effect %in% rownames(coefs)))>0) coefs[rownames(coefs) %in% effect,statistic] else 0
  for(i in 1:length(categorical)){
    for(j in 1:length(bestmodels)){
      coefs = coefTable(bestmodels[[j]])
      if(categorical[i] == "island")
        estimates = rbind(estimates,
                          data.frame(by = NA,estimate = getcoef("islandST",1),    se = getcoef("islandST",2),    x="ST", island = NA, variable = "island"),
                          data.frame(by = NA,
                                     estimate = if(getcoef("islandST",1) == 0) 0 else getcoef("(Intercept)",1), 
                                     se = if(getcoef("islandST",1) == 0) 0 else getcoef("(Intercept)",2), 
                                     x="PC", island = NA, variable = "island"))
      if(categorical[i] == "bait_type")
        estimates = rbind(estimates,
                          data.frame(by = NA,estimate = getcoef("bait_typeLURE",1), se = getcoef("bait_typeLURE",2), x="LURE", island = NA, variable = "bait_type"),
                          data.frame(by = NA,
                                     estimate = if(getcoef("bait_typeLURE",1) == 0) 0 else getcoef("(Intercept)",1),   
                                     se = if(getcoef("bait_typeLURE",1) == 0) 0 else getcoef("(Intercept)",2),   
                                     x="PC",   island = NA, variable = "bait_type"))
      if(categorical[i] == "island:bait_type"){
        temp = list(a = c("bait_typeLURE:islandST","islandST:bait_typeLURE"),
                    b = c("bait_typeLURE:islandPC","islandPC:bait_typeLURE"), 
                    c = c("bait_typeBAIT:islandST","islandST:bait_typeBAIT"), 
                    d = c("bait_typeBAIT:islandPC","islandPC:bait_typeBAIT"))
        estimates = rbind(estimates,
                          data.frame(by = "island",estimate = getcoef(temp[[1]],1), se = getcoef(temp[[1]],2), x="LURE", island = "ST", variable = "bait_type"),
                          data.frame(by = "island",estimate = getcoef(temp[[2]],1), se = getcoef(temp[[2]],2), x="LURE", island = "PC", variable = "bait_type"),
                          data.frame(by = "island",estimate = getcoef(temp[[3]],1), se = getcoef(temp[[3]],2), x="BAIT", island = "ST", variable = "bait_type"),
                          data.frame(by = "island",estimate = getcoef(temp[[4]],1), se = getcoef(temp[[4]],2), x="BAIT", island = "PC", variable = "bait_type"))
      }
      if(categorical[i] == "gear_subtype")
        estimates = rbind(estimates,
                          data.frame(by = NA,estimate = getcoef("gear_subtypeset",1), se = getcoef("gear_subtypeset",2), x="set",      island = NA, variable = "gear_subtype"),
                          data.frame(by = NA,
                                     estimate = if(getcoef("gear_subtypeset",1) == 0) 0 else getcoef("(Intercept)",1),     
                                     se = if(getcoef("gear_subtypeset",1) == 0) 0 else getcoef("(Intercept)",2),     
                                     x="pivoting", island = NA, variable = "gear_subtype"))
      if(categorical[i] == "propulsion"){
        estimates = rbind(estimates,
                          data.frame(by = NA,estimate = getcoef("propulsionremo-vela",1), se = getcoef("propulsionremo-vela",2), x="remo-vela",      island = NA, variable = "propulsion"),
                          data.frame(by = NA,
                                     estimate = if(getcoef("propulsionremo-vela",1) == 0) 0 else getcoef("(Intercept)",1),         
                                     se = if(getcoef("propulsionremo-vela",1) == 0) 0 else getcoef("(Intercept)",2),
                                     x="motorisada",     island = NA, variable = "propulsion"))
        if(gear %in% c("handline","hook_line","spear_fishing"))
          estimates = rbind(estimates,
                            data.frame(by = NA,estimate = getcoef("propulsionsem-embarcacao",1), se = getcoef("propulsionsem-embarcacao",2), x="sem-embarcacao",      island = NA, variable = "propulsion"))
      }
    }
  }
  
  for(i in 1:length(sm)){
    temp = list(a = median(dat[,sm[i]]))
    names(temp) = c(sm)[i]
    newdat = c(newdat,temp)
  }
  newdat <- expand.grid(newdat)
  
  # Predict non-smooth variables on model average
  notsm  = notsm[!notsm %in% categorical]
  if(length(notsm) >0){
    pred   = predict(mod, newdat, se.fit = TRUE)
    for(i in 1:length(notsm)){
      estimates = rbind(estimates,
                        data.frame(by = NA,    estimate = pred$fit, se = pred$se.fit,
                                   x  = newdat[,which(colnames(newdat) == notsm[i])],
                                   island = NA, variable = notsm[i]))
    }
  }
  estimates$effect = estimates$variable
  estimates$effect[which(!is.na(estimates$by))]  = paste0(estimates$variable[which(!is.na(estimates$by))],
                                                          ":",
                                                          estimates$by[which(!is.na(estimates$by))])
  suppressMessages({
    estimates = as.data.frame(estimates %>% group_by(effect,variable,by,x,island) %>% summarise(estimate = mean(estimate), se = mean(se)))
  })
  
  for(i in 1:2){
    islands = levels(as.factor(dat$island))
    for(j in 1:length(islands)){
      positions = which(estimates$variable == c("date","ordinaldate") [i] &
                          estimates$island == islands[j])
      island = estimates$estimate[which(estimates$variable == "island" & estimates$x == islands[j])]
      if(!sum(estimates$estimate[positions]) == 0)
            estimates$estimate[positions] = estimates$estimate[positions]+island
      }}
  
  # CENTRE AROUND ZERO
  effect = levels(as.factor(estimates$effect))
  for(i in 1:length(effect)){
    positions = which(estimates$effect == effect[i])
    temp      = estimates$estimate[positions]
    estimates$estimate[positions] = estimates$estimate[positions] - (max(temp) + min(temp))/2
  }
  estimates$diversityindicator = diversityindicator
  estimates$gear = gear
  return(estimates)
}






#########################################################################
###                        GET SUMMED WEIGHTS                         ###
#########################################################################





getsw <- function(gear,diversityindicator){
  mods        = get(paste(gear,diversityindicator,sep="_"))
  bestmodels  = get.models(MuMIn::model.sel(mods), subset = delta < 6)
  globalmodel = unlist(lapply(mods,function(mod) length(labels(terms(mod$formula)))))
  globalmodel = mods[[which(globalmodel == max(globalmodel))]]
  
  effects = labels(terms(formula(globalmodel)))
  effects[effects == "island:bait_type"] = "bait_type:island"
  if(length(bestmodels) > 1)   {
    sweights = sw(bestmodels)
    names(sweights)[names(sweights) == "island:bait_type"] = "bait_type:island"
  } else {
    labs = labels(terms(formula(bestmodels[[1]])))
    labs[labs == "island:bait_type"] = "bait_type:island"
    sweights = rep(1,length.out = length(labs))
    names(sweights) = labs
  } 
  temp     = rep(0,length(effects[!effects %in% names(sweights)]))
  names(temp) = effects[!effects %in% names(sweights)]
  sweights = c(sweights,temp)
  return(sweights)}




allsw <- function(diversityindicator){
  gears <- c("surface_gillnet", "hook_line", "seine_net","demersal_gillnet",
             "spear_fishing", "demersal_line", "handline",
             "demersal_troll","surface_troll")
  for(i in 1:length(gears)){
    sweights = getsw(gears[i], diversityindicator)
    sweights = as.data.frame(t(as.data.frame(sweights)))
    sweights$gear = gears[i]
    sweights$diversityindicator = diversityindicator
    if(i == 1) {
      output = sweights
      rownames(output) = gears[i]
    } else {
      nam = rownames(output)
      output = bind_rows(output, sweights)
      rownames(output) = c(nam,gears[i])
    }
  }
  effects = c("gear", "diversityindicator", "s(date)", "s(ordinaldate, by = island, bs = \"cc\")", "s(date, by = island)",
              "s(net_depth)", "s(net_length)", "s(ordinaldate, bs = \"cc\")", "island", 
              "s(mesh_size)", "s(time_fishing)", "n_fishers",
              "ngears", "propulsion", "s(time_sea)", "nsets", "gear_subtype", "s(hook_size)",
              "s(n_hooks)", "n_lines", "bait_type:island", "bait_type")
  for(i in 1:length(effects)){
    colnames(output)[which(colnames(output) == effects[i])] =
      c("gear", "diversityindicator", "date", "ordinaldate:island", "date:island", "net_depth", "net_length",
        "ordinaldate", "island", "mesh_size", "time_fishing", "n_fishers",
        "ngears", "propulsion", "time_sea", "nsets", "gear_subtype", "hook_size",
        "n_hooks", "n_lines", "bait_type:island", "bait_type")[i]
  }
  return(output)}

allweights <- bind_rows(allsw("weight"), allsw("richness"))
write.csv(allweights, file = file.path(tables,"TableS7_SW_diversity_indicators.csv"))






#########################################################################
###                         PLOT PREDICTIONS                          ###
#########################################################################






### FUNCTION 1: SET Y LIMITS ACROSS FACETS
set_ylims = function(dat){
  dat$ymax = dat$estimate + dat$se
  dat$ymin = dat$estimate - dat$se
  temp = lapply(levels(as.factor(dat$gear)), function(gear){
    temp = dat[which(dat$gear == gear),][1,]
    temp[1,] = NA
    temp$gear = gear
    ymax = temp
    ymax$estimate = max(dat$ymax[which(dat$gear == gear)])
    ymin = temp
    ymin$estimate = min(dat$ymin[which(dat$gear == gear)])
    return(rbind(ymax,ymin))})
  for(i in 1:length(temp)) if(i == 1) output = temp[[i]] else output = rbind(output,temp[[i]])
  return(output)
}




### FUNCTION 2: ROUND TO DECIMAL
roundtodecimal <- function(dat, digits = 2) unlist(lapply(
  as.character(round(dat, digits = digits)), 
  function(x) 
    if(nchar(x) == 1) x = paste0(x, ".", paste(rep(0,digits),collapse = "")) else 
      paste0(x,paste(rep(0,digits-(nchar(x)-2))), collapse = "")
))



### FUNCTION 3: PLOT ALL EFFECT TYPES
ploteffects <- function(pred, 
                        effect,
                        nostriptext = TRUE, 
                        noyaxis = TRUE, 
                        legend.position = "none",
                        legend.position.inside = c(0, 0),
                        legend.justification.inside = c(0,0)){

  
  # Categorical effects
  categorical <- c("island", "bait_type", "bait_type:island", "gear_subtype", "propulsion")
  
  ## PREPARE DATABASES
  # Data frame of PREDICTIONS
  dat = pred[which(pred$effect %in% effect),]
  
  # Data frame of Y LIMITS
  ylims = set_ylims(pred)
  ylims$x = dat$x[1]
  if(effect %in% categorical) {
    ylims$x = 1
    d = length(levels(as.factor(paste(dat$x,dat$island)))) - 1
    ylims$x[c(1,2)] = c(1-(0.2*d),d+1+(0.2*d))
  }
  if(effect == "bait_type:island") {
    ylims$x = 1
    ylims$x[c(1,2)] = c(0.5,3.5)
  }
  ylims$estimate = ylims$estimate * 1.2
  if(!effect %in% categorical) ylims$x = as.numeric(ylims$x)
  
  # Data frame of SUM OF WEIGHTS
  SW = allweights
  
  # Add empty data point for non-existent gear/effect combinations
  gears = levels(as.factor(pred$gear))
  notinmodel = gears[!gears %in% dat$gear]
  if(length(notinmodel) > 0) 
    for(i in 1:length(notinmodel)){
      temp = dat[1,]
      temp[1,] = NA
      temp$gear = notinmodel[i]
      temp$effect = effect
      dat = rbind(dat,temp)
    }
  
  
  ## RE-LABEL EFFECTS AND GEARS
  effectlabel =   data.frame(effects = c("date", "ordinaldate:island", "date:island", "net_depth", "net_length",
                                         "ordinaldate", "island", "mesh_size", "time_fishing", "n_fishers",
                                         "ngears", "propulsion", "time_sea", "nsets", "gear_subtype", "hook_size",
                                         "n_hooks", "n_lines", "bait_type:island", "bait_type"),
                             labels = c("Date", "Ordinal date : Island", "Date : Island", "Net depth", "Net length",
                                        "Ordinal date", "Island", "Mesh size", "Time fishing", "N. of fishers",
                                        "N. of gears", "Propulsion", "Time at the sea", "N. of sets", "Gear subtype", "Hook size",
                                        "N. of hooks", "N. of lines", "Bait type : Island", "Bait type"))
  
  effectlabel  = effectlabel$labels[which(effectlabel$effects == effect)]
  dat$effect   = effectlabel
  ylims$effect = effectlabel
  colnames(SW)[colnames(SW) == effect] = effectlabel
  
  gearlabel = list(surface_gillnet = "Surf. gillnet", seine_net = "Seine net",
                    spear_fishing = "Spear fish.",     hook_line = "Hook & line",
                    demersal_line = "Dem. line*",   demersal_troll = "Dem. troll*",
                    handline = "Handline*",             surface_troll = "Surf. troll*")
  
  dat$gear   = factor(dat$gear, levels = names(gearlabel), labels = unlist(gearlabel)) 
  ylims$gear = factor(ylims$gear, levels = names(gearlabel), labels = unlist(gearlabel))
  SW$gear    = factor(SW$gear, levels = names(gearlabel), labels = unlist(gearlabel))
  
  
  
  ## PLOT CONTINUOUS VARIABLES
  if(!effect %in% categorical) {
    if(grepl("island", effect)){
      dat = dat[!is.na(dat$island),]
      output = 
        ggplot(data = dat,
               aes(x = as.numeric(x), y = estimate, ymax = estimate + se, ymin = estimate - se, fill = island)) +
        geom_ribbon(alpha = 0.4) + 
        geom_line(data = dat, aes(x=as.numeric(x), y = estimate, colour = island)) +
        scale_colour_manual(breaks = c("PC", "ST"), values = c("firebrick","#00BFC4")) +
        scale_fill_manual(breaks = c("PC", "ST"), values = c("firebrick","#00BFC4")) 
    } else {
      output = 
        ggplot(data = dat,
               aes(x = as.numeric(x), y = estimate, ymax = estimate + se, ymin = estimate - se)) +
        geom_ribbon(alpha = 0.4) + 
        geom_line(data = dat, aes(x=as.numeric(x), y = estimate), colour = "grey25") 
    }
  }
  
  ## PLOT CATEGORICAL EFFECTS
  if(effect %in% categorical) {
    if(effect == "bait_type:island"){
      dat$x[which(dat$x == "BAIT" & dat$island == "PC")] = 1
      dat$x[which(dat$x == "BAIT" & dat$island == "ST")] = 1.5
      dat$x[which(dat$x == "LURE" & dat$island == "PC")] = 2.5
      dat$x[which(dat$x == "LURE" & dat$island == "ST")] = 3
      dat$x = as.numeric(dat$x)
          } else {
      lab = levels(as.factor(dat$x))
      for(i in 1:length(lab)) dat$x[dat$x == lab[i]] = i
      dat$x = as.numeric(dat$x)
    }
    output = 
      ggplot(data = dat,
             aes(x = as.numeric(x), y = estimate, ymax = estimate + se, ymin = estimate - se, colour = island)) +
      geom_errorbar(width = 0.2) + 
      geom_point(data = dat, aes(x= x, y = estimate, fill = island)) +
      scale_colour_manual(breaks = c("PC", "ST"), values = c("firebrick","#00BFC4")) +
      scale_fill_manual(breaks = c("PC", "ST"), values = c("firebrick","#00BFC4")) 
    if(effect == "bait_type:island"){
      output = output + xlim(0,4) +
        scale_x_continuous(breaks=c(1.25,2.75), labels = c("Bait", "Lures"))
    } else {
      output = output + xlim(0,4) +
        scale_x_continuous(breaks=c(1:length(lab)), labels = lab)
    }
  }
  
  ## ADD FACETS TO OUTPUT PLOT
  output = output + 
      geom_point(data = ylims, aes(x= x, y = estimate), colour = NA, fill = NA) + 
    facet_grid(gear ~ effect, scales = "free_y") +
    ylab(paste0("Effect on ",pred$diversityindicator[1])) +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          plot.margin = unit(c(0.1, 0.05, 0.1, 0.05), "cm"),
          axis.title.x = element_blank(),
          legend.position = legend.position)
  
  
  ## FORMAT X AXIS OF DATE
  if(grepl("date", paste0(effect, " "))){
    output = output + scale_x_continuous(
      breaks = as.integer(as.POSIXct(c("2020-07-02", "2021-07-02","2022-07-02", "2023-07-02"),
                                     format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60),
      minor_breaks = startyear,  labels = c("2020", "2021", "2022", "2023")) 
  }
  
  ## FORMAT X AXIS OF ORDINAL DATE
  if(grepl("ordinaldate", paste0(effect, " "))){
    output = output + scale_x_continuous(
      breaks = c(15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350), 
      minor_breaks = c(1,60,122,183,243,305,365),
      labels = c("J",  "F",  "M",  "A",  "M",   "J",   "J",   "A",   "S",   "O",   "N", "D")) 
  }

  ## ADD SW
  SW = SW[which(SW$gear %in% dat$gear & allweights$diversityindicator %in% dat$diversityindicator[1]),
                  which(colnames(allweights) %in% c("gear",effect))]
  colnames(SW)[which(colnames(SW) == effectlabel)] = "label"
  SW$label <- unlist(lapply(SW$label,function(label) if(is.na(label)) NA else 
    paste0("SW = ", roundtodecimal(label))))
  if(effect %in% categorical) 
    x = max(na.omit(as.numeric(ylims$x))) else 
    x = max(na.omit(as.numeric(dat$x)))
  SW = data.frame(SW, effect = effectlabel, by = NA, diversityindicator = NA,  
                  ymax=NA,  ymin = NA, se = NA, island = NA,
                  x= x,
                  estimate = unlist(lapply(SW$gear, function(gear) 
                    max(ylims$estimate[ylims$gear == gear])-(max(ylims$estimate[ylims$gear == gear])/10))))
  output = output + 
    geom_text(data = SW,aes(x=x,y=estimate, label = label), hjust = 1, size =3, colour = "black")  

  
  ## OTHER FORMATTING OPTIONS
  if(nostriptext) output = output + theme(strip.text.y = element_blank())
  if(noyaxis) output = output + theme(axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.line.y=element_blank())
  if(legend.position == "inside")     output = 
             output +
    scale_color_manual(values= c(PC = "firebrick", ST = "#00BFC4"),
                      labels= c("Príncipe", "São Tomé"),
                                 breaks= c("PC", "ST")) +
    
    scale_fill_manual(breaks= c("PC", "ST"),
                      labels= c("Príncipe", "São Tomé"),
                      values= c(PC = "firebrick", ST = "#00BFC4", "NA" = "grey")) +
              
              theme(legend.position = legend.position.inside,
                                                          legend.background = element_blank(),
                                                          legend.title = element_blank(),
                                                          legend.justification.inside = legend.justification.inside,
                                                          legend.text = element_text(size = 7),
                                                          legend.key.height = unit(0.35, "cm"))
  return(output)}






### FUNCTION 4: COMPOSE PLOTS
plotalleffects = function(diversityindicator, fig1, fig2, m1 =1, m2=0, m3=3){
  pred = rbind(
    predictions("surface_gillnet",diversityindicator),
    predictions("seine_net",diversityindicator),
    predictions("spear_fishing",diversityindicator),
    predictions("hook_line",diversityindicator),
    predictions("demersal_line",diversityindicator),
    predictions("handline",diversityindicator),
    predictions("surface_troll",diversityindicator))
  pred$x[which(pred$x == "motorisada")] = "Mo"
  pred$x[which(pred$x == "remo-vela")] = "N-Mo"
  pred$x[which(pred$x == "sem-embarcacao")] = "NV"
  pred$x[which(pred$variable == "bait_type" & 
                 pred$x == "PC")] = "Bait"
  pred$x[which(pred$effect == "bait_type" & 
                 pred$x == "LURE")] = "Lures"
  pred = pred[-which(pred$effect == "n_lines" & 
                 pred$x %in% c(0,5)),] 
  
  
  
  temp = pred[which(pred$effect %in% c("date","date:island","ordinaldate:island","ordinaldate","island")),]
  p = plot_grid(
    ploteffects(temp,"island", noyaxis = FALSE),
    ploteffects(temp,"date"),
    ploteffects(temp,"date:island",legend.position = "inside"),
    ploteffects(temp,"ordinaldate"),
    ploteffects(temp,"ordinaldate:island",legend.position = "inside",
                nostriptext = FALSE),
    ncol=5, align = "h",axis = "lr",
    rel_widths = c(1.25,1,1,1,1.1))
  svg(filename = file.path(figures, paste0(fig1, ".svg")), height = 6.5, width = 8.3)
  plot(p)
  dev.off()
  pdf(file.path(figures, paste0(fig1, ".pdf")), height = 6.5, width = 8.3)
  plot(p)
  dev.off()
  
  
  leftaxiswdth = 1.5
  rightaxiswdth = 1.2
  temp = pred[which(pred$effect %in% c("bait_type","bait_type:island","time_fishing", "hook_size","n_hooks","n_lines", 
                                       "propulsion")),]
  temp = temp[which(temp$gear %in% c("demersal_line","handline","surface_troll")),]
  p1 = plot_grid(
    ploteffects(temp,"time_fishing", noyaxis = FALSE)+ 
      theme(axis.title.y = element_text(margin = margin(r = m1, "cm"))),
    ploteffects(temp,"hook_size"),
    ploteffects(temp,"n_hooks"),
    ploteffects(temp,"n_lines"),
    ploteffects(temp,"bait_type"),
    ploteffects(temp,"bait_type:island", legend.position = "inside",
                legend.position.inside = c(0,0.83)),
    ploteffects(temp,"propulsion",nostriptext = FALSE),
    ncol=7, align = "h",axis = "lr",
    rel_widths = c(leftaxiswdth,1,1,1,1,1,rightaxiswdth))
  

  
  temp = pred[which(pred$effect %in% c("time_fishing", "mesh_size","net_length","net_depth", 
                                       "nsets")),]
  temp = temp[which(temp$gear %in% c("surface_gillnet","seine_net")),]
  p2 = plot_grid(
    ploteffects(temp,"time_fishing", noyaxis = FALSE)+ 
      theme(axis.title.y = element_text(margin = margin(r = m2, "cm"))),
    ploteffects(temp,"mesh_size"),
    ploteffects(temp,"net_length"),
    ploteffects(temp,"net_depth"),
    ploteffects(temp,"nsets",nostriptext = FALSE),
    ncol=7, align = "h",axis = "lr",
    rel_widths = c(leftaxiswdth,1,1,1,rightaxiswdth,1,1))
  
  
  
  
  temp = pred[which(pred$effect %in% c("time_sea","time_fishing","n_fishers", "ngears", 
                                       "propulsion")),]
  temp$effect[which(temp$effect %in% c("time_sea"))] = "time_fishing"
  temp = temp[which(temp$gear %in% c("spear_fishing","hook_line")),]
  p3 = plot_grid(
    ploteffects(temp,"time_fishing", noyaxis = FALSE) + 
      theme(axis.title.y = element_text(margin = margin(r = m3, "cm"))),
    ploteffects(temp,"n_fishers"),
    ploteffects(temp,"ngears"),
    ploteffects(temp,"propulsion",nostriptext = FALSE),
    ncol=7, align = "h",axis = "lr",
    rel_widths = c(leftaxiswdth,1,1,rightaxiswdth,1,1,1))
  empty = ggplot(data = data.frame(x = 0, y = 0),
                 aes(x = x, y = y)) + theme_void()
  p = plot_grid(p1,empty,p2, empty,p3,nrow = 5,
            rel_heights = c(1.5,0.1,1,0.1,1), 
            labels = c("A","","B","","C")) 
  hgth = 7
  wdth = 9
  svg(filename = file.path(figures, paste0(fig2, ".svg")), 
      height = hgth, width = wdth)
  plot(p)
  dev.off()
  
  pdf(file.path(figures, paste0(fig2,".pdf")), 
      height = hgth, width = wdth)
  plot(p)
  dev.off()
}





### USE PREVIOUS FUNCTIONS TO COMPOSE PLOTS
plotalleffects(diversityindicator = "weight", 
               fig1 = "FigS13_season_biomass",
               fig2 = "FigS14_other_effects_biomass",
               m1 =0, m2=0, m3=0)

plotalleffects(diversityindicator = "richness", 
               fig1 = "FigS15_season_richness",
               fig2 = "FigS16_other_effects_richness",
               m1 =4, m2=-0.3, m3=10)
  
  
  


