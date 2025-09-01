

#########################################################################
#########################################################################
#                            OBSERVED SPECIES                           #
#########################################################################
#########################################################################







## Directories exist? If not, create them
tables     <- file.path(getwd(), "data_analysis", "outputs", "tables")
if(!file.exists(tables)) dir.create(tables)


## Directories exist? If not, create them
figures    <- file.path(getwd(), "data_analysis", "outputs", "figures")
if(!file.exists(figures)) dir.create(figures)



#########################################################################
####                     LOAD DATA AND PACKAGES                      ####
#########################################################################



if(!require("ggplot2"))    install.packages("ggplot2",    repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require("ggpattern"))    install.packages("ggpattern",    repos = "https://cloud.r-project.org")
library(ggpattern)
if(!require("cowplot"))    install.packages("cowplot",    repos = "https://cloud.r-project.org")
library(cowplot)
if(!require("dplyr"))      install.packages("dplyr",    repos = "https://cloud.r-project.org")
library(dplyr)
if(!require("ggtext"))    install.packages("ggtext",    repos = "https://cloud.r-project.org")
library(ggtext)
if(!require("gridExtra"))    install.packages("gridExtra",    repos = "https://cloud.r-project.org")
library(gridExtra)





   
### RUN SCRIPTS 1 AND 2A
# (1) LOADS & CLEANS DATA; (2a) ADDS COVARIATES TO CATCH AND EFFORT
   #source(file.path("data_analysis","scripts","02A_variable_definition.R"))
   
   
   
   ntrips <- read.csv(file.path(getwd(),"data_analysis", "data_with_covariates","ntrips.csv"))
   catch    <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","catch.csv"),    header=TRUE, sep = ",")
   effort   <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","effort.csv"),   header=TRUE, sep = ",")
   tripinfo <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","tripinfo.csv"), header=TRUE, sep = ",")
   species  <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","species.csv"),  header=TRUE, sep = ",")
   
   
   
   tripinfo$gear_general[which(tripinfo$gear_general == "hook_line_shore")] <- "hook_line"
   
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


### REMOVE NON-VALID OBSERVATIONS
remove <- tripinfo$trip[which(!tripinfo$valid | tripinfo$participation == "nao")]
effort <- effort[-which(effort$trip %in% remove),]
catch  <- catch[-which(catch$trip %in% remove),]
tripinfo    <- tripinfo[-which(tripinfo$trip %in% remove),]
rm(remove)





   #########################################################################
   ####                     CREATE DATA MATRICES                        ####
   #########################################################################


### STEP 1: FUNCTION THAT CREATES gear / trip VS species MATRIX
sptrip_matrix <- function(rw,variable){
  specs     = levels(as.factor(catch$species))
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



### STEP 2: CREATE gear / trip VS species MATRIX OF WEIGHT
trip_weight <- sptrip_matrix(rw="trip",variable="weight")
gear_weight <- sptrip_matrix(rw="effort_id",variable="weight")



### STEP 3: CREATE gear / trip VS species MATRIX OF NUMBER OF FISH
trip_number <- sptrip_matrix(rw="trip",variable="number")
gear_number <- sptrip_matrix(rw="effort_id",variable="number")




### STEP 3: REMOVE OBJECTS
rm(sptrip_matrix)






   #########################################################################
   ####                         PREPARE DATA                            ####
   #########################################################################


               #----------------------------------------------#
               #      AGGREGATE SPECIES OF DIFFICULT ID       #
               #----------------------------------------------#


### STEP 1: FUNCTION TO AGGREGATE SPECIES
collapse_species <- function(dat,specs,newname){
  specs = colnames(dat)[which(colnames(dat) %in% specs)]
  if(length(specs) > 1.1) {dat$newsp = dat[,which(colnames(dat) %in% specs)] %>% rowSums()
  dat       = dat[,-which(colnames(dat) %in% specs)]
  colnames(dat)[which(colnames(dat) == "newsp")] = newname}
  return(dat)}

### STEP 2: AGGREGATE SPECIES
temp <-  list(specs   =   list(     species$species[which(grepl("Lutjanus",species$species) & !grepl("fulgens",species$species))],
                                    c("Euthynnus alletteratus", "Auxis thazard", "Auxis thazard + Euthynnus alletteratus"),
                                    species$species[which(   grepl("Pagrus",species$species)  )]     ),
              newname = c("Lutjanus sp.", 
                          "Auxis thazard + Euthynnus alletteratus", 
                          "Pagrus sp.")
              )

for(i in 1:length(temp$newname)){
  gear_weight <- collapse_species(dat = gear_weight, specs = temp$specs[[i]], newname =  temp$newname[i])
   }

for(i in 1:length(temp$newname)){
  trip_weight <- collapse_species(dat = trip_weight, specs = temp$specs[[i]], newname =  temp$newname[i])
   }




### STEP 3: RECLASSIFY FAMILIES
species$Family[which(species$species == "Selachimorpha")] <- "Selachimorpha"
species$Family[which(species$species == "Crustacean")] <- "Decapoda"
species$Family[which(species$species == "Gastropoda")] <- "Gastropoda"
species$type[which(species$type == "Mobulidae")] <- "Batoidea"
species$type[which(species$type == "Myliobatiformes")] <- "Batoidea"
species$type[which(species$type == "Crustacean")] <- "Decapoda"

rm(temp,i,collapse_species)




### STEP 4: CREATE "ALL" CATEGORY
species$aggregated <- "all"






### STEP 5: CREATE LABELS
gear       = c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net",
               "hook_line",   "hook_line_shore",    "spear_fishing", "demersal_line",
               "demersal_troll", "handline", "surface_troll")
gear_label = c("Circular net", "Surface gillnet", "Demersal gillnet", "Seine net",
               "Hook & line", "Hook & line (shore)","Spear fishing", "Demersal line",
               "Demersal troll", "Handline", "Surface troll")


gear_general <- c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net",
                  "hook_line",    "spear_fishing")
gear_general_label = c("Scoop net", "Surface gillnet", "Demersal gillnet", "Seine net",
                       "Hook & line", "Spear fishing")
hook_line    <- c("demersal_line","demersal_troll", "handline", "surface_troll")












   #########################################################################
   ####                    FUNCTION TO SUMMARISE DATA                   ####
   #########################################################################



summary_catch <- function(gear = gear_general,
                          island = c("PC","ST"), 
                          threshold = NA, 
                          taxonomical_level = "species",
                          taxonomical_group = "all",
                          type = "trip",
                          gear_category = "gear_type"){
  transformdatabases <- function(variable){
    # Get relevant data
    if(type == "trip") {
      temp = tripinfo[,which(colnames(tripinfo) %in% c("trip","island","gear_general","n_fishers"))]
      dat = get(paste0("trip_",variable)) %>% inner_join(temp,   by=c("trip"))
      dat$n_fishers[dat$n_fishers == 0] <- 1
      dat = dat[which(dat$gear_general %in% gear & dat$island %in% island),]
      }
    if(type == "gear") {
      temp = effort[,which(colnames(effort) %in% c("effort_id","island",gear_category,"n_fishers"))]
      dat = get(paste0("gear_",variable)) %>% inner_join(temp,   by=c("effort_id"))
      dat$n_fishers[dat$n_fishers == 0] <- 1
      colnames(dat)[which(colnames(dat)==gear_category)] = "gear_category"
      dat = dat[which(dat$gear_category %in% gear & dat$island %in% island),]
    }
  
  
    # Remove NAs
    dat    = na.omit(dat)
    if(nrow(dat)<30){dat = dat[1,][-1,]}
    sp   = colnames(trip_weight)[-1]
    if(length(gear) == 1) if(gear == "spear_fishing"){dat[,which(colnames(dat)%in%sp)] = dat[,which(colnames(dat)%in%sp)]/dat$n_fishers}
    
  
    # Keep only species from specific group
    if(!taxonomical_group == "all") 
      dat <- 
        dat[,!colnames(dat) %in% 
            species$species[which(!species$type == taxonomical_group)]]
     

    # Transform to family (if family = TRUE)
    if(!taxonomical_level == "species"){
      taxalist = species[,which(colnames(species) == taxonomical_level)]
      sp       = levels(as.factor(taxalist[which(species$species %in% colnames(dat))]))
      for(i in 1:length(sp)){
          spec = species$species[which(taxalist == sp[i])]
          spec = spec[which(spec %in% colnames(dat))]
          if(length(spec) > 0.1) {
            if(length(spec) == 1)  {
              dat$temp = unlist(dat[,which(colnames(dat) == spec)])
              dat = dat[,-which(colnames(dat) == spec)]
              } else {
                dat$temp = dat[,which(colnames(dat) %in% spec)] %>% rowSums(.)
                dat = dat[,-which(colnames(dat) %in% spec)]
                }
            colnames(dat)[length(dat)] = sp[i]
          }
      }} else {
        sp = species[,which(colnames(species) == taxonomical_level)]
        sp = sp[sp %in% colnames(dat)]
          }
       return(list(dat = dat, sp = sp))}  
    
  # Create databases
  temp = transformdatabases("weight")
  dat  = temp$dat
  sp   = temp$sp
  num  = transformdatabases("number")$dat
  
  # Keep most abundant species, aggregate the rest under "Other" label
  if(!is.na(threshold)) {
    temp = data.frame(sp = sp)
    temp$weight = unlist(lapply(sp,function(x){
      sum(na.omit(dat[, which(colnames(dat) == x)]))
    }))
    temp = dplyr::arrange(temp,-weight)
    remove = sp[-which(sp %in% temp$sp[c(1:threshold)])]
    sp   = c(temp$sp[c(1:threshold)],"Other")
    dat$Other = dat[,which(colnames(dat)%in% remove)] %>% rowSums(.)
    colnames(dat)[ncol(dat)] <- "Other"
  }
  # Create data frame
  output = data.frame(sp = sp)
  output$nobs = nrow(dat)
  output$occurrences = unlist(lapply(c(1:length(sp)),function(i){
    length(which(!dat[,which(colnames(dat) == output$sp[i])]==0))
  }))
  output$occurrences_perc <- output$occurrences*100 / nrow(dat)
  output$weight = unlist(lapply(c(1:nrow(output)),function(i){
    sum(na.omit(dat[, which(colnames(dat) == output$sp[i])]))
  }))
  output$number = unlist(lapply(c(1:nrow(output)),function(i){
    sum(na.omit(num[, which(colnames(num) == output$sp[i])]))
  }))
  output$weight_perc = output$weight / sum(output$weight)
  output$mean_weight = output$weight / nrow(dat)
  output$se_weight <- unlist(lapply(c(1:nrow(output)),function(i){
    sd(na.omit(dat[, which(colnames(dat) == output$sp[i])]))/(nrow(dat)^0.5)
  }))
  output$sd_weight <- unlist(lapply(c(1:nrow(output)),function(i){
    sd(na.omit(dat[, which(colnames(dat) == output$sp[i])]))
  }))
  if(length(island) == 1){
    if(island == "PC") {output$island = "Príncipe"} else {output$island = "São Tomé"}
    output$island = factor(output$island, levels = c("Príncipe","São Tomé"))
    } else {
      output$island = "all"
    }
  output$sp= as.factor(output$sp)
  output$order  = c(1:nrow(output))
  if(length(gear)==1) output$gear   = gear else output$gear == "all"
  return(output)}












#############################################################################
####           CATCH OF MAIN TAXONOMIC GROUPS BY MÉTIER TYPE             ####
#############################################################################




### PLOT CATCH WEIGHT BY GEAR TYPE AND ISLAND
for(i in 1:length(gear_general)) {
  for(j in 1:2){
    temp = summary_catch(
      gear   = gear_general[i],
      island = c("PC","ST")[j],
      taxonomical_level = "aggregated",
      type = "trip")
    if(i == 1) dat = temp else dat = rbind(dat, temp)
  }}
dat$gear <- factor(dat$gear,levels=gear_general,labels = gear_general_label)



lwdth = 0.4
a<- ggplot(data = dat, aes(x = gear, y = mean_weight, ymin = mean_weight, ymax = mean_weight + se_weight, fill = island)) +
  geom_bar(stat = "identity", position = "stack",fill = "white", colour = "black",  size = lwdth) + 
  geom_bar(stat = "identity", position = "stack", colour = "black", alpha = 0.65,  size = lwdth) + 
  geom_errorbar(linewidth = lwdth, width = 0.3)+
  facet_grid(.~island) +  
  scale_fill_manual(values = c("firebrick","#00BFC4") ) +
  ylab ("Mean weight (kg per trip)") + labs(fill = "Island")




### PLOT PROPORTION OF MAIN TAXONOMIC GROUPS ON EACH GEAR TYPE AND ISLAND
for(i in 1:length(gear_general)) {
  for(j in 1:2){
    temp = summary_catch(
      gear   = gear_general[i],
      island = c("PC","ST")[j],
      taxonomical_level = "type",
      type = "trip")
    if(i == 1) dat = temp else dat = rbind(dat, temp)
  }}
dat$gear <- factor(dat$gear,levels=gear_general,labels = gear_general_label)
View(dat)
write.csv(dat,file.path(tables,"maintypes.csv"))


b <- ggplot(data = dat, aes(x = gear, y = weight_perc, fill = sp, pattern = sp)) + 
  geom_bar(stat = 'identity', position = "stack", fill = 'white', colour = "white", 
           size = lwdth) +
  geom_bar(stat = 'identity', position = "stack", colour = "black", 
           alpha = 0.7,  size = lwdth) +
  geom_bar_pattern(position="stack", stat="identity", pattern_fill = "black", 
                   pattern_colour = "black",fill = NA,
                   colour = "black", pattern_spacing = 0.04,
                   pattern_frequency = 0.1,  size = lwdth,
                   pattern_density = 0.05,
                   pattern_size = 0.02,
                   pattern_key_scale_factor=0.5) +
  facet_grid(.~island)+
  scale_pattern_manual(values=c('circle','stripe','none','stripe', 'crosshatch','circle'))   +
  scale_fill_manual(values = c("yellow","red","green", "blue", 
                               "dodgerblue", "grey50") ) +
  ylab ("% of catch weight") + labs(fill = "Group", pattern = "Group")




### COMPOSE AND EXPORT PLOT
ggtheme <- theme_bw() +
  theme(axis.text.x = element_text(hjust=1,angle=20, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.justification = "left",
        panel.grid =  element_blank(),
        axis.title.x = element_blank())

# Príncipe pdf figure
pdf(file.path(figures,"Fig4_catch_composition.pdf"), width = 7, height = 6)
plot_grid(a + ggtheme,
          ggplot() + xlim(0, 10) + ylim(0, 5) + theme_void(),
          b + ggtheme,
          ncol=1,align = "v", axis = "lr", labels = c("A","","B"),
          rel_heights = c(1,0.1,1))
dev.off()

# Print svg figure
svg(file.path(figures,"Fig4_catch_composition.svg"), width = 7, height = 6)
plot_grid(a + ggtheme,
          ggplot() + xlim(0, 10) + ylim(0, 5) + theme_void(),
          b + ggtheme,
          ncol=1,
          align = "v", axis = "lr", labels = c("A","","B"),
          rel_heights = c(1,0.1,1))
dev.off()











#########################################################################
####          SPECIES AND FAMILY COMPOSITION ACROSS MÉTIERS          ####
#########################################################################




### CREATE FUNCTION THAT CREATES A PLOT OF SPECIES AND FAMILY COMPOSITION OF CATCH 
family_composition <- function(taxonomical_level = "Family", taxonomical_group = "Teleost",
                               ncol = 7, textarea = 0.15, top = 0.95, textsize = 4,
                               wdth = 9, spacing = 0.3, hgth = 4.5, rel_widths = c(0.995,1.4), 
                               correction = 0.05,
                               figname){
  
  ## Summarise data per gear and taxa
  for(i in 1:length(gear_general)) {
    for(j in 1:2){
      temp = summary_catch(
        island = c("PC","ST")[j],
        gear   = gear_general[i],
        taxonomical_level = taxonomical_level,
        taxonomical_group = taxonomical_group,
        type = "trip")
      temp$gear = gear_general[i]
      temp$weight = temp$weight /c(7081, 5161)[j]
      temp$se_weight = temp$se_weight /c(7081, 5161)[j]
      if(i == 1 & j == 1) 
        dat = temp else 
          dat = bind_rows(dat, temp)
    }}
  dat$gear <- factor(dat$gear,levels=gear_general,labels = gear_general_label)
  fulldat = dat
  temp = fulldat[c("sp","island","weight")] %>% group_by(sp,island) %>% summarise(weight = sum(weight))
  temp$spisland = paste(temp$sp,temp$island)
  fulldat = fulldat[!paste(fulldat$sp,fulldat$island) %in% temp$spisland[temp$weight== 0],]
  fulldat$sp = as.character(fulldat$sp)
  fulldat$sp[fulldat$sp == "Auxis thazard + Euthynnus alletteratus"] = "A. thazard + E. alletteratus"
  if(length(which(grepl("Selachimorpha",fulldat$sp)))>0){
    fulldat = rbind(
      fulldat[!fulldat$sp == "Selachimorpha",],
      fulldat[fulldat$sp == "Selachimorpha",]
      )
    fulldat$sp[fulldat$sp == "Selachimorpha"] = "Not possible to ID"
  }
  
  
  ## Summarise data per taxa and island
  for(j in 1:2){
    temp = summary_catch(
      island = c("PC","ST")[j],
      taxonomical_level = taxonomical_level,
      taxonomical_group = taxonomical_group, 
      threshold = 10)
    if(j == 1) agg = temp else agg = rbind(agg, temp)}
  agg$gear = "All"
  agg = agg[!is.na(agg$sp),]
  agg = agg[!agg$weight == 0,]
  agg$sp = as.character(agg$sp)
  if(length(which(grepl("Selachimorpha",agg$sp)))>0){
    agg = rbind(
      agg[!agg$sp == "Selachimorpha",],
      agg[agg$sp == "Selachimorpha",]
    )
    agg$sp[agg$sp == "Selachimorpha"] = "Not possible to ID"
  }
  sp = agg$sp
  sp[sp == "Auxis thazard + Euthynnus alletteratus"] = "A. thazard + E. alletteratus"
  sporiginal = sp
  sp[grepl(" ", sp) & !sp == "Not possible to ID"] = paste0("*",sp[grepl(" ", sp) & !sp == "Not possible to ID"], "*")

  agg$sp = as.character((10+nrow(agg)):(10+1))
  agg$lab = "Mean landing weight (kg)"
  
  
  ## Plot of mean weight per gear per island
  mean_weight <-
    ggplot(data = agg, aes(x = sp, y = mean_weight, 
                           ymax = mean_weight + se_weight, 
                           ymin = mean_weight )) +
    geom_bar(stat = "identity") + 
    geom_errorbar() + 
    coord_flip() + 
    scale_x_discrete(breaks = agg$sp, labels = sp) +
    facet_grid(island~lab, scales = "free") +
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_markdown())
  
  
  
  ## Calculate contribution of each métier to biomass of each taxa
  agg$sp = sporiginal
  agg$spmarkdown = sp
  
  for(j in 1:2){
    island = c("Príncipe", "São Tomé")[j]
    dat = fulldat[fulldat$island == island,]
    dat$sp = as.character(dat$sp)
    dat$sp[!dat$sp %in% agg$sp[agg$island == island]] = "Other"
    dat=as.data.frame(dat %>% group_by(sp,gear, island) %>% summarise(number = sum(number), mean_weight = sum(weight), se_weight = mean(se_weight)))
    if(j == 1) bygear = dat else bygear = rbind(bygear, dat)
  }
  
  agg$spisland = paste(agg$sp,agg$island)
  bygear$spisland = paste(bygear$sp,bygear$island)
  for(i in 1:nrow(agg)) bygear$spisland[which(bygear$spisland == agg$spisland[i])] = i
  bygear$spisland = factor(bygear$spisland , levels =as.character(c(nrow(agg):1)))
  agg$spisland = 1:nrow(agg)
  agg$spisland = factor(agg$spisland , levels =as.character(c(nrow(agg):1)))
  
  bygear$mean_weight = unlist(lapply( 1:nrow(bygear), function(i) 
    bygear$mean_weight[i] / sum(bygear$mean_weight[which(bygear$spisland == bygear$spisland[i])])
        ))
  bygear$mean_weight = bygear$mean_weight*100
  bygear$lab = "% of biomass caught with each métier"
  
  
  
  ## Plot contribution of each métier to biomass of each taxa
  bygear = ggplot(data = bygear, aes(x = spisland, y = mean_weight, fill = gear, pattern = gear)) + 
    geom_bar(position = "stack", stat = "identity", colour = "black") + 
    geom_bar_pattern(aes(pattern_angle = gear),position = "stack", stat = "identity", 
                     colour = "black",
                     fill = NA,
                     pattern_colour = "black", 
                     pattern_fill = "black",
                     pattern_spacing = 0.03,
                     pattern_frequency = 0.2,
                     pattern_density = 0.05,
                     pattern_size = 0.01,
                     pattern_key_scale_factor=0.5) +
    coord_flip() +
    theme_bw() + 
    scale_pattern_manual(values=c('stripe','stripe','crosshatch',
                                  'crosshatch','circle', 'none'),
                         breaks = c("Scoop net","Surface gillnet","Demersal gillnet",
                                    "Seine net","Hook & line", "Spear fishing")) +
    scale_pattern_angle_manual(values = c(0,45,0,45,45,0),
                               breaks = c("Scoop net","Surface gillnet","Demersal gillnet",
                                          "Seine net","Hook & line", "Spear fishing")) +
    scale_x_discrete(breaks = as.character(c(1:nrow(agg))), 
                     labels = as.character(agg$spmarkdown[1:nrow(agg)])) + 
    facet_grid(island~lab,scales = "free") +  
    labs(fill = "Métiers", pattern = "Métiers", pattern_angle = "Métiers")  + 
    theme(axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_markdown())
  
  
  ## Plot fish outlines
  fish = levels(as.factor(agg$sp))
  fish = fish[!fish == "Other"]
  

  fishoutlines <- lapply(1:length(fish), function(i) {
    path =  file.path(getwd(),"fish_pictures","fishoutlines", paste0(fish[i],".JPG"))
    path2 =  file.path(getwd(),"fish_pictures","fishoutlines", paste0(fish[i],".jpg"))
    blank = file.path(getwd(),"fish_pictures","fishoutlines", "blank.JPG")
    if(file.exists(path)) f = jpeg::readJPEG(source = path,native=TRUE) else 
      if(file.exists(path2)) f = jpeg::readJPEG(source = path2,native=TRUE) else 
      f = jpeg::readJPEG(source = blank,native=TRUE) 
    
    
    p = ggplot(data.frame(x=c(0,1), y = c(0,1)), aes(x=x,y=y)) + theme_void() + 
      annotation_raster(f, xmin = 0, xmax = 1, ymin = textarea, ymax = top)
    p+geom_text(x=0.5,y=0.1,label=fish[i] , size = textsize, 
                fontface = if(grepl(" ", fish[i])) "italic" else "plain") + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    })
  
  f = jpeg::readJPEG(source = file.path(getwd(),"fish_pictures","fishoutlines", "blank.JPG"),
                     native=TRUE)
  
  ph <- ((dim(f)[1]/(1-textarea-(1-top)))/dim(f)[2])*(wdth/ncol)
  ph = ph + correction
  hgth = hgth + 
    ((ceiling(length(fish)/ncol) * ph))
  p=plot_grid(
    plot_grid(mean_weight,bygear,ncol = 2, rel_widths=rel_widths,
              labels = c("A","B")),
    ggplot(data.frame(x=c(0,1), y = c(0,1)), aes(x=x,y=y)) + theme_void(),
    plot_grid(plotlist = fishoutlines,ncol = ncol), 
    labels = c("","", "C"),
    ncol = 1, rel_heights = c(hgth,spacing,
                              (ceiling(length(fish)/ncol) * ph))
      )
  
  # Save to PDF
  pdf(paste0(figures,"/",figname,".pdf"), width = wdth, height =hgth)
  plot(p)
  dev.off()
  
  # Save to svg
  svg(paste0(figures,"/",figname,".svg"), width = wdth, height =hgth)
  plot(p)
  dev.off()
  }





## CREATE FIGURES OF EACH TAXA TYPE USING FUNCTION ABOVE
family_composition(ncol = 7, textarea = 0.15, top = 0.95, textsize = 3.5,
                   wdth = 8.5, spacing = 0.3, hgth = 4, rel_widths = c(0.995,1.4),
                   correction = 0.15,
                   figname = "Fig5_Catch_composition")


family_composition(taxonomical_level = "species", ncol = 6, textarea = 0.15, 
                   top = 0.95, textsize = 4,wdth = 12, spacing = 0.3, hgth = 2.5, 
                   correction = 0.6,rel_widths = c(1,1.3),
                   figname = "FigS7_teleost_catch_composition")


family_composition(taxonomical_level = "species", 
                   taxonomical_group = "Selachimorpha",
                   ncol = 6, textarea = 0.15, 
                   top = 0.95, textsize = 4,wdth = 12, spacing = 0.3, hgth = 2.5, 
                   correction = 0.6,rel_widths = c(1,1.3),
                   figname = "FigS11_shark_catch_composition")



family_composition(taxonomical_level = "species", 
                   taxonomical_group = "Batoidea",
                   ncol = 6, textarea = 0.15, 
                   top = 0.95, textsize = 4,wdth = 12, spacing = 0.3, hgth = 2.5, 
                   correction = 0.6,rel_widths = c(1,1.3),
                   figname = "FigS12_ray_catch_composition")










#########################################################################
####            SPECIES AND FAMILY COMPOSITION BY MÉTIERS            ####
#########################################################################





### STEP 1: CREATE FUNCTION THAT SUMMARISES CATCH OF THE TWO ISLANDS
summarise.species <- function(gearlist,threshold,type,taxonomical_level, gear_category = "gear_type"){
  summarise.species = function(island){
    temp = lapply(gearlist,function(gear) 
      summary_catch(gear = gear, island = island,
                    threshold=threshold, type = type, 
                    taxonomical_level = taxonomical_level, 
                    gear_category = gear_category,
                    taxonomical_group = "Teleost")  )
    for(i in 1:length(temp)) if(i==1) {dat = temp[[i]]} else {dat = rbind(dat, temp[[i]])}
    return(dat) }
  rbind(summarise.species("PC"),summarise.species("ST"))}





### STEP 2: FUNCTIONS TO PLOT CATCH COMPOSITION
plotlist <-function(gearlist,type,threshold,taxonomical_level="Family",gear_category="gear_type", leftmargin = 0)lapply(gearlist,function(gear){
  dat = summarise.species(gear,threshold = threshold,   type = type,   
                          taxonomical_level = taxonomical_level, 
                          gear_category = gear_category)
  categories = c("demersal_line","handline","surface_troll","demersal_troll")
  labs       = c("Demersal line","Handline","Surface troll","Demersal troll")
  for(i in 1:length(categories))    dat$gear[which(dat$gear == categories[i])] = labs[i]
  categories = c("hook_line",    "surface_gillnet",  "seine_net", "demersal_gillnet",    "circular_net",  "spear_fishing")
  labs       = c("Hook & line",  "Surface gillnet",  "Seine net", "Demersal gillnet",    "Scoop net",  "Spear fishing")
  for(i in 1:length(categories))    dat$gear[which(dat$gear == categories[i])] = labs[i]
  categories = c("demersal_lineBAIT", "demersal_trollLURE", "handlineBAIT", "handlineLURE","surface_trollBAIT", "surface_trollLURE")
  labs       = c("Dem. line (B)", "Dem. troll (L)",  "Handline (B)",  "Handline (L)", "Surf. troll (B)",    "Surf. troll (L)")
  for(i in 1:length(categories))    dat$gear[which(dat$gear == categories[i])] = labs[i]
  
  dat$sp = as.character(dat$sp)
  dat$sp[which(dat$mean_weight == 0 | is.na(dat$mean_weight))] = ""
  dat$sp[which(dat$sp == "Auxis thazard + Euthynnus alletteratus")] = "A. thazard & E. alletteratus"
  
  dat$sp = as.factor(dat$sp)
  dat$categories = paste0(dat$order,dat$gear,dat$island)
  breaks = dat$categories
  labs   = as.character(dat$sp)
  labs[which(grepl(" ",labs))]   = paste0("*",labs[which(grepl(" ",labs))],"*")
  
  ggplot(dat,aes(x=categories,y=mean_weight,fill=island)) + 
    geom_bar(stat='identity', alpha=0.8,colour="black", fill = "white") + 
    geom_bar(stat='identity', alpha=0.8,colour="black") + 
    geom_errorbar(aes(ymin=mean_weight, ymax=mean_weight+se_weight), width=.2) +
    facet_grid(gear~island, scales = "free_x") +
    ylab("Kg per trip") +
    scale_x_discrete(breaks=breaks,labels = labs) +
    xlab(NULL) +  
    scale_fill_manual(values = c("firebrick","#00BFC4") )+ 
    theme_bw() +
    theme(axis.text.x = element_markdown(angle=20, hjust=1, colour="black"),
          axis.text.y = element_text(colour="black"),
          legend.position = 'none',
          panel.grid.major.x = element_blank(),   panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),    panel.grid.minor.y = element_blank(),
          strip.text.x = element_blank(), 
          plot.margin = unit(c(3, 12, 0, 1+leftmargin), "point"))
})



catchcompositionplots <- function(gearlist, type, threshold = 5, taxonomical_level = "Family", 
                                  figname, wdth, hgth,relativesize, imageposition, size, 
                                  gear_category = "gear_type", leftmargin = 0,
                                  legend.alignment.x = 0,
                                  legend.alignment.y = 0){
  gearplots = plotlist(gearlist,type=type,threshold = threshold, 
                       taxonomical_level = taxonomical_level, gear_category = gear_category,
                       leftmargin = leftmargin)
  gearplots[[1]] = gearplots[[1]] + theme(legend.position = "inside",
                                          legend.position.inside = c(0.885 + legend.alignment.x,
                                                                     0.83 + legend.alignment.y),
                                          legend.text = element_text(size = 7),
                                          legend.key.height = unit(0.35, "cm"),
                                          legend.title = element_blank())
  p1        =  plot_grid(plotlist = gearplots,ncol=1,align="hv")
  # Create species key
  p2        = ggplot(data.frame(x=c(0,1),y=c(0,1)),aes(x=x,y=y)) + 
    geom_blank() + theme_void() + scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "point"))
  temp      = gearplots[[1]]$data
  for(i in 2:length(gearplots) ) temp = rbind(temp,gearplots[[i]]$data)
  sp        = levels(as.factor(as.character(temp$sp)))
  sp        = sp[!sp %in% c("Other","")]
  splabel   = sp 
  splabel[which(grepl(" ",splabel))]   = paste0("*",splabel[which(grepl(" ",splabel))],"*")
  yposition = seq(from = 1-(1/(2*length(sp))),to = 0, by=-(1/length(sp)))
  p2        = p2 + annotate("richtext", ,
                            fill = NA,
                            label.color = NA,
                            x=rep(imageposition,length(sp)), 
                            y = yposition, label = splabel, hjust = 1, size = size)
  for(i in 1:length(sp)){
    if(paste0(sp[i],".JPG") %in% list.files(file.path(getwd(),"fish_pictures","fishoutlines"))) {
      fish = jpeg::readJPEG(source = file.path(getwd(),"fish_pictures","fishoutlines",paste0(sp[i],".JPG")),native=TRUE)
    } else {
      if(paste0(sp[i],".jpg") %in% list.files(file.path(getwd(),"fish_pictures","fishoutlines"))) {
        fish = jpeg::readJPEG(source = file.path(getwd(),"fish_pictures","fishoutlines",paste0(sp[i],".jpg")),native=TRUE)
      } else {
        fish = jpeg::readJPEG(source = file.path(getwd(),"fish_pictures","fishoutlines",paste0(sp[i],".JPG")),native=TRUE)
      }}
    p2 = p2 + annotation_raster(fish, xmin = imageposition + 0.02, 
                                xmax = imageposition + 0.02 + (((1/length(sp))*2.9)/((wdth/relativesize)/hgth)), 
                                ymin = 1-((1/length(sp))*i), 
                                ymax = 1-((1/length(sp))*(i-1))) 
  }
  # Save to PDF
  pdf(paste0(figures,"/",figname,".pdf"), width = wdth, height =hgth)
  grid.arrange(p1, p2, ncol=2,widths=c(1-(1/relativesize),1/relativesize))
  dev.off()
  
  # Save to svg
  svg(paste0(figures,"/",figname,".svg"), width = wdth, height =hgth)
  grid.arrange(p1, p2, ncol=2,widths=c(1-(1/relativesize),1/relativesize))
  dev.off()
}






# STEP 3: CREATE PLOTS
gearlist     <- c("hook_line","surface_gillnet","seine_net","demersal_gillnet","circular_net","spear_fishing")
catchcompositionplots(gearlist = gearlist, type = "trip", threshold = 5, taxonomical_level = "Family", 
                      figname  = "FigS8_familycomposition",      wdth = 8,     hgth  = 10.4,     relativesize = 2.8, imageposition = 0.4, size = 4, 
                      leftmargin = 12,
                      legend.alignment.x = 0,
                      legend.alignment.y = -0.02)
catchcompositionplots(gearlist = gearlist, type = "trip", threshold = 5, taxonomical_level = "species", 
                      figname  = "FigS9_speciescomposition",      wdth = 10,     hgth  = 14,     relativesize = 3.2, imageposition = 0.6, size = 3.5, 
                      leftmargin = 23,
                      legend.alignment.x = 0.03,
                      legend.alignment.y = 0.02)


effort$gear_bait = paste0(effort$gear_type,effort$bait_type)
effort$gear_bait[which(effort$propulsion == "sem-embarcacao")] <- paste0(effort$gear_bait[which(effort$propulsion == "sem-embarcacao")], "_shore")
gearlist = levels(as.factor(effort$gear_bait))
gearlist = gearlist[!grepl("NA",gearlist) & !grepl("circular", gearlist) & !grepl("shore", gearlist)]

catchcompositionplots(gearlist = gearlist, type = "gear", threshold = 5, taxonomical_level =  "species", 
                      figname  = "FigS10_speciescomposition",      wdth = 8,     hgth  = 10,     
                      relativesize = 3, imageposition = 0.6, gear_category = "gear_bait", size =3.2, 
                      leftmargin = 40,
                      legend.alignment.x = -0.01,
                      legend.alignment.y = -0.08)











#########################################################################
####       TABLES OF SPECIES AND FAMILY COMPOSITION BY MÉTIERS       ####
#########################################################################




### PART 1: OVERALL SUMMARIES OF CATCH WEIGHT
# Average catch
summary_catch(taxonomical_level = "aggregated")


# Average catch by main taxonomic group
summary_catch(taxonomical_level = "type")


# Species composition per island
PCcatch <- summary_catch(island = "PC", taxonomical_group = "Teleost")
View(PCcatch)
STcatch <- summary_catch(island = "ST", taxonomical_group = "Teleost")
View(STcatch)
rm(PCcatch,STcatch)





### PART 2: CATCH PER MÉTIER TYPE
# Catch per métier type
for(i in 1:length(gear_general)) {
  temp = summary_catch(
    gear   = gear_general[i],
    taxonomical_level = "aggregated",
    type = "trip")
  if(i == 1) dat = temp else dat = rbind(dat, temp)
}
dat$gear <- factor(dat$gear,levels=gear_general,labels = gear_general_label)
dat


## Catch composition of demersal lines
summarise.species("demersal_line",5,"gear","Family")


## Catch composition of spear fishing
summary_catch(gear   = "spear_fishing",
  taxonomical_level = "type", type = "trip")
spearfishing <- summary_catch(gear   = "spear_fishing",
                              type = "trip")
View(spearfishing)
rm(spearfishing)

# Teleost composition of hook-and-line landings
hookline <- summary_catch(taxonomical_level = "Family",
                          taxonomical_group = "Teleost",
                          gear = "hook_line")
View(hookline)
rm(hookline)



# Main taxonomical groups per métier
for(i in 1:length(gear_general)) {
  temp = summary_catch(
    gear   = gear_general[i],
    taxonomical_level = "type",
    type = "trip")
  if(i == 1) dat = temp else dat = rbind(dat, temp)
}
dat$gear <- factor(dat$gear,levels=gear_general,labels = gear_general_label)
View(dat)
write.csv(dat,file.path(tables,"maintypes.csv"))






### PART 3: BIOMASS OF SPECIFIC TAXA ACROSS MÉTIERS
## Total weight and number of sharks across métiers
for(i in 1:length(gear_general)){
  temp = summary_catch(taxonomical_level = "type", 
                taxonomical_group = "Selachimorpha",
                gear = gear_general[i])
  temp = data.frame(gear = gear_general[i],
                    nobs = temp$nobs, 
                    weight = temp$weight,
                    number = temp$number)
  if(i ==1) sharks = temp else sharks = rbind(sharks, temp)
}
sharks
summary_catch(taxonomical_group = "Selachimorpha", taxonomical_level = "Family")
summary_catch(taxonomical_group = "Selachimorpha")
rm(sharks)




## Total weight and number of RAYS across métiers
for(i in 1:length(gear_general)) {
  temp = summary_catch(
    gear   = gear_general[i],
    taxonomical_level = "type",
    taxonomical_group = "Batoidea",
    type = "trip")
  if(i == 1) rays = temp else rays = rbind(rays, temp)
}
rays$gear <- factor(rays$gear,levels=gear_general,labels = gear_general_label)
rays
summary_catch(taxonomical_group = "Batoidea", taxonomical_level = "Family")
summary_catch(taxonomical_group = "Batoidea")
rm(rays)




## Biomass of gastropoda across métier types
for(i in 1:length(gear_general)) {
  temp = summary_catch(
    gear   = gear_general[i],
    taxonomical_level = "type",
    taxonomical_group = "Gastropoda",
    type = "trip")
  if(i == 1) gastropoda = temp else gastropoda = rbind(gastropoda, temp)
}
gastropoda$gear <- factor(gastropoda$gear,levels=gear_general,labels = gear_general_label)
gastropoda
summary_catch(taxonomical_group = "Gastropoda", taxonomical_level = "Family")
rm(gastropoda)




## Biomass of cephalopoda across métier types
for(i in 1:length(gear_general)) {
  temp = summary_catch(
    gear   = gear_general[i],
    taxonomical_level = "type",
    taxonomical_group = "Cephalopoda",
    type = "trip")
  if(i == 1) cephalopoda = temp else cephalopoda = rbind(cephalopoda, temp)
}
cephalopoda$gear <- factor(cephalopoda$gear,levels=gear_general,labels = gear_general_label)
cephalopoda
summary_catch(taxonomical_group = "Cephalopoda", taxonomical_level = "Family")
rm(cephalopoda)




## Teleost catch composition across métiers
for(i in 1:length(gear_general)){
  temp = summary_catch(taxonomical_level = "Family",
                       taxonomical_group = "Teleost",
                       gear = gear_general[i])
  temp = data.frame(gear = temp$gear,
                    species = temp$sp,
                    a = temp$mean_weight,
                    b = temp$sd_weight)
  colnames(temp) = c("gear", "species", paste(temp$gear[1], "mean"), paste(temp$gear[1], "sd"))
  if(i == 1) teleost = temp[,-1] else teleost = cbind(teleost,temp[,-c(1,2)])
}





