






#########################################################################
#########################################################################
###########---------------- SAMPLING SUMMARY ----------------############
#########################################################################
#########################################################################












#########################################################################
#---------------------------  DIRECTORIES  -----------------------------#
#########################################################################


## Directories exist? If not, create them
tables     <- file.path(getwd(), "data_analysis", "outputs", "tables")
if(!file.exists(tables)) dir.create(tables)





#########################################################################
#                      LOAD DATA (FROM SCRIPT  1B)                      #
#########################################################################



### STEP 1: LOAD AND CLEAN DATABASES AND ADDS VARIABLES TO EFFORT DATA
source(file.path("data_analysis","scripts","02A_variable_definition.R"))

# "tripinfo" and "effort" databases (loaded in script above)
str(tripinfo)
str(effort)

# Function to convert multiple-response variables into a list (loaded in script above)
tolist



#--------------------------------------------#
######## Number of individual fishers ########
#--------------------------------------------#

fishers <- unlist(tolist(tripinfo$name_fishers[which(tripinfo$valid & tripinfo$participation == "sim")]))
fishers <- fishers[-which(fishers=="")]
fishers <- levels(as.factor(fishers))
length(fishers[which(grepl("ST", fishers))])
length(fishers[which(grepl("PC", fishers))])



#--------------------------------------------#
######## Summarise sampling ########
#--------------------------------------------#

summarise.sampling = function(island){
  dat    = tripinfo[which(tripinfo$island == island),]
  eff    = effort[which(effort$island == island),]
  output = data.frame(
    n.sampling.days = length(levels(as.factor(dat$date[which(dat$island == island)]))),
    start           = dat$dateoriginal[which(dat$date == min(na.omit(dat$date)))][1],
    end             = dat$dateoriginal[which(dat$date == max(na.omit(dat$date)))][1],
    num.ext.workers = length(levels(as.factor(dat$ext_worker))),
    ntrips          = nrow(dat),
    participated    = length(which(dat$participation == 'sim')), 
    resp.rate       = length(which(dat$participation == 'sim'))/nrow(dat),
    excluded        = length(which(dat$participation == 'sim' & !dat$valid)),
    retained        = length(which(dat$participation == 'sim' & dat$valid)),
    num.fishing.comm= length(levels(as.factor(dat$site[which(!dat$site == "NO")]))),
    num.fishers     = length(fishers[which(grepl(island, fishers))])
  )
  dat  = dat[which(dat$participation == 'sim'),]
  eff  = eff[-which(eff$trip %in% dat$trip[which(!dat$valid)]),]
  dat  = dat[which(dat$valid),]
  temp = data.frame(
    foot              = length(which(dat$vessel == 'sem-embarcacao')),
    vessel            = length(which(!dat$vessel == 'sem-embarcacao')),
    hook_line         = length(which(dat$gear_general == 'hook_line')),
    hook_line.motor   = length(which(dat$gear_general == 'hook_line' & dat$propulsion == "motorisada")),
    hook_line.rowing  = length(which(dat$gear_general == 'hook_line' & dat$propulsion == "remo-vela")),
    hook_line_ngears  = paste0(round(mean(dat$ngears[which(dat$gear_general == 'hook_line')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'hook_line')]),2), ")"),
    hook_line_jigg    = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "hook_line" & eff$gear_type == "handline")]))),
    hook_line_demline = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "hook_line" & eff$gear_type == "demersal_line")]))),
    hook_line_demtrol = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "hook_line" & eff$gear_type == "demersal_troll")]))),
    hook_line_surtrol = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "hook_line" & eff$gear_type == "surface_troll")]))),
    
    sur_gillnet       = length(which(dat$gear_general == 'surface_gillnet')),
    sur_gillnet.motor = length(which(dat$gear_general == 'surface_gillnet' & dat$propulsion == "motorisada")),
    sur_gillnet.rowing= length(which(dat$gear_general == 'surface_gillnet' & dat$propulsion == "remo-vela")),
    sur_gill_ngear    = paste0(round(mean(dat$ngears[which(dat$gear_general == 'surface_gillnet')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'surface_gillnet')]),2), ")"),
    sur_gill_oth_gear = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "surface_gillnet" & !eff$gear_type == "surface_gillnet")]))),
    
    seine_nets        = length(which(dat$gear_general == 'seine_net')),
    seine_netsmotor   = length(which(dat$gear_general == 'seine_net' & dat$propulsion == "motorisada")),
    seine_netsrowing  = length(which(dat$gear_general == 'seine_net' & dat$propulsion == "remo-vela")),
    seine_net_ngear   = paste0(round(mean(dat$ngears[which(dat$gear_general == 'seine_net')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'seine_net')]),2), ")"),
    seine_net_oth_gear= length(levels(as.factor(eff$trip[
      which(eff$gear_general == "seine_net" & !eff$gear_type == "seine_net")]))),
    
    dem_gillnet       = length(which(dat$gear_general == 'demersal_gillnet')),
    dem_gillnet.motor = length(which(dat$gear_general == 'demersal_gillnet' & dat$propulsion == "motorisada")),
    dem_gillnet.rowing= length(which(dat$gear_general == 'demersal_gillnet' & dat$propulsion == "remo-vela")),
    dem_gill_ngear    = paste0(round(mean(dat$ngears[which(dat$gear_general == 'demersal_gillnet')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'demersal_gillnet')]),2), ")"),
    dem_gill_oth_gear = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "demersal_gillnet" & !eff$gear_type == "demersal_gillnet")]))),
    beach_seine       = length(which(dat$gear_general == 'beach_seine')),
    
    spear_fishing     = length(which(dat$gear_general == 'spear_fishing')),
    spear_fishing.motor = length(which(dat$gear_general == 'spear_fishing' & dat$propulsion == "motorisada")),
    spear_fishing.rowing= length(which(dat$gear_general == 'spear_fishing' & dat$propulsion == "remo-vela")),
    spear_fishing.novessel= length(which(dat$gear_general == 'spear_fishing' & dat$propulsion == "sem-embarcacao")),
    spear_fishing_ngea= paste0(round(mean(dat$ngears[which(dat$gear_general == 'spear_fishing')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'spear_fishing')]),2), ")"),
    spear_fishing_othg= length(levels(as.factor(eff$trip[
      which(eff$gear_general == "spear_fishing" & !eff$gear_type == "spear_fishing")]))),
    circ_net          = length(which(dat$gear_general == 'circular_net')),
    circ_net.motor    = length(which(dat$gear_general == 'circular_net' & dat$propulsion == "motorisada")),
    circ_net.rowing   = length(which(dat$gear_general == 'circular_net' & dat$propulsion == "remo-vela")),
    circ_net_ngea     = paste0(round(mean(dat$ngears[which(dat$gear_general == 'circular_net')]),2),   " (",  
                               round(sd(dat$ngears[which(dat$gear_general == 'circular_net')]),2), ")"),
    circ_net_othg     = length(levels(as.factor(eff$trip[
      which(eff$gear_general == "circular_net" & !eff$gear_type == "circular_net")]))),
    
    hook_line_shore   = length(which(dat$gear_general == 'hook_line_shore'))
  )
  output = t(cbind(output,temp))
  colnames(output) = island
  return(output)}

tripinfo$n_gears
(summary.sampling <- cbind(summarise.sampling("PC"),summarise.sampling("ST")))
write.csv(summary.sampling,paste0(tables,"/TableS3_S4_Sampling_summary.csv"))



