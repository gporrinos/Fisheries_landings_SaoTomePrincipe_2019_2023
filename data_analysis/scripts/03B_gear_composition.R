

#########################################################################
#########################################################################
#                            GEAR COMPOSITION                           #
#########################################################################
#########################################################################









#########################################################################
#---------------------------  DIRECTORIES  -----------------------------#
#########################################################################


## Directories exist? If not, create them
figures    <- file.path(getwd(), "data_analysis", "outputs", "figures")
if(!file.exists(figures)) dir.create(figures)

tables     <- file.path(getwd(), "data_analysis", "outputs", "tables")
if(!file.exists(tables)) dir.create(tables)

r_objects  <- file.path(getwd(), "data_analysis", "outputs", "r_objects")
if(!file.exists(r_objects)) dir.create(r_objects)



#########################################################################
#-----------------------------  LOAD DATA  -----------------------------#
#########################################################################





### RUN SCRIPTS 1A AND 1B
# 1A -> LOADS & CLEANS DATA
# 1B -> ADDS COVARIATES TO CATCH AND EFFORT
#source(file.path("data_analysis","scripts","02A_variable_definition.R"))



ntrips <- read.csv(file.path(getwd(),"data_analysis", "data_with_covariates","ntrips.csv"))
catch    <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","catch.csv"),    header=TRUE, sep = ",")
effort   <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","effort.csv"),   header=TRUE, sep = ",")
tripinfo <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","tripinfo.csv"), header=TRUE, sep = ",")
species  <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","species.csv"),  header=TRUE, sep = ",")



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




tripinfo$gear_general[tripinfo$gear_general == "hook_line_shore"] = "hook_line"



#########################################################################
#                     LOAD (AND INSTALL) PACKAGES                       #
#########################################################################



# Data manipulation
if(!require("dplyr"))   install.packages("dplyr",   repos = "https://cloud.r-project.org")
library(dplyr)
# Plots
if(!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require("ggtext")) install.packages("ggtext", repos = "https://cloud.r-project.org")
library(ggtext)
if(!require("cowplot")) install.packages("cowplot", repos = "https://cloud.r-project.org")
library(cowplot)
if(!require("ggpattern")) install.packages("ggpattern", repos = "https://cloud.r-project.org")
library(ggpattern)





#########################################################################
#                            PLOT GEAR TYPES                            #
#########################################################################



### STEP 1: CREATE THEME AND GEOM OBJECTS FOR ALL PLOTS
ggtheme = theme_bw() + 
  theme(plot.margin = margin(t = 4, r = 6, b = 16, l = 6, unit = "pt"),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.x   = element_blank(),
        legend.justification = "left",
        axis.text.x          = element_text(size = 9, angle = 20, hjust = 1), 
        axis.title=element_text(size=10),
        strip.text = element_text(size = 10))

geomemptybarplot         <- geom_bar(stat = 'identity', fill = 'white', colour = "white", alpha = 0.7)
geombarplot              <- geom_bar(stat = 'identity', colour = "black", alpha = 0.7)
geompatternbarplot       <- geom_bar_pattern(position="stack", stat="identity", pattern_fill = "black", fill = NA,
                                             colour = "black", pattern_spacing = 0.04,
                                             pattern_frequency = 0.5, 
                                             pattern_size = 0.1,
                                             pattern_density = 0.2,
                                             pattern_colour = "black",
                                             pattern_angle = 45, linewidth = 0.1,
                                             pattern_key_scale_factor=0.5) 






### STEP 2: CREATE VECTORS WITH LABELS
gear_general       = c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net", "hook_line",    "hook_line_shore",    "spear_fishing")
gear_general_label = c("Scoop net", "Surface gillnet", "Demersal gillnet", "Seine net", "Hook & line",  "Hook & line (shore)","Spear fishing")
hook_line          = c("demersal_line", "demersal_troll", "handline", "surface_troll")
hook_line_label    = c("Demersal line", "Demersal troll", "Handline", "Surface troll")
island             = c("PC","ST")
island_label       = c("Príncipe", "São Tomé")
propulsion         = c("sem-embarcacao","remo-vela", "motorisada")
propulsion_label   = c("No vessel","Rowing or sailing", "Motorised")






### STEP 3: PLOT OF MOTORISATION
# Create data frame
gear <- as.data.frame(expand.grid(propulsion = propulsion,
                                  gear_general = gear_general,
                                  island    = c("PC","ST")))

# Count observations
gear$n <- unlist(lapply(1:nrow(gear), function(i)
  length(which(tripinfo$island     == gear$island[i]     &     
                 tripinfo$gear_general  == gear$gear_general[i] &
                 tripinfo$propulsion  == gear$propulsion[i]))))
gear = gear[!gear$gear_general == "hook_line_shore",]

# Calculate % of observations over the total for island
gear$perc <- as.numeric(0)
for(i in 1:2) gear$perc[which(gear$island == c("PC","ST")[i])] <- 
  gear$n[which(gear$island == c("PC","ST")[i])] /
  length(which(tripinfo$island == c("PC","ST")[i]))
gear$perc <- gear$perc * 100

# Labels
gear$propulsion <- factor(gear$propulsion, levels = propulsion, labels = propulsion_label)
gear$island     <- factor(gear$island, levels = island, labels = island_label)
gear$gear_general  <- unlist(lapply(gear$gear_general, function(gear) gear_general_label[which(gear_general == gear)]))
gear$gear_general  <- factor(gear$gear_general,levels=gear_general_label)

# Create plot
a <-     ggplot(gear,aes(x = gear_general, y = perc, fill = island, pattern = propulsion)) + 
  geomemptybarplot + geombarplot + geompatternbarplot +
  scale_pattern_manual(values=c('none','circle','stripe'), labels = propulsion_label)  +
  scale_fill_manual(values = c("firebrick","#00BFC4") ) +
  labs(pattern = "Motorisation") + xlab("Main gear types") + ylab("% of observations")    + 
  facet_grid(.~island) +  guides(fill = FALSE) +
  ggtheme





### STEP 4: PLOT OF GEARS & BAIT
# Create data frame
gear <- as.data.frame(expand.grid(bait_type = c("LURE","BAIT"),
                                  gear_type = hook_line,
                                  island    = island))

# Count observations
gear$n <- unlist(lapply(1:nrow(gear), function(i)
  length(which(effort$island     == gear$island[i]     &
                 effort$gear_type  == gear$gear_type[i] &
                 effort$bait_type  == gear$bait_type[i]))))

# Calculate % of observations over the total for island
gear$perc <- as.numeric(0)
for(i in 1:2) 
  gear$perc[which(gear$island == island[i])] <- 
  gear$n[which(gear$island == island[i])] /
  length(which(tripinfo$gear_general == "hook_line" & tripinfo$island == island[i]))
gear$perc <- gear$perc * 100

# Labels
gear$bait_type <- factor(gear$bait_type, levels = c("LURE","BAIT"), labels = c("Plastic lures","Bait"))
gear$island    <- factor(gear$island, levels = island, labels = island_label)
gear$gear_type <- unlist(lapply(gear$gear_type, function(gear) hook_line_label[which(hook_line == gear)]))

# Create plot
b <-     ggplot(gear,aes(x = gear_type, y = perc, fill = island, pattern = bait_type))+ 
  geomemptybarplot + geombarplot + geompatternbarplot +
  scale_pattern_manual(values=c('circle','stripe'))  + guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick","#00BFC4") ) +
  labs(pattern = "Bait type") + xlab("Hook and line gears") + ylab("% of observations")    +  
  facet_grid(.~island) + 
  ggtheme 





### STEP 5: PLOT OF AVERAGE NUMBER OF TRIPS
ntrips$seine_net = ntrips$purse_seine + ntrips$surface_gillnet_seine
gear <- as.data.frame(expand.grid(gear_general = gear_general,
                                  island    = island))[,c(2,1)]
gear$mean <- unlist(lapply(1:nrow(gear), function(i)
  mean(na.omit(ntrips[ntrips$island==gear$island[i],which(colnames(ntrips) == gear$gear_general[i])]))))
gear$sd  <- unlist(lapply(1:nrow(gear), function(i)
  sd(na.omit(ntrips[ntrips$island==gear$island[i],which(colnames(ntrips) == gear$gear_general[i])])) / (length(which(ntrips$island==gear$island[i]))^0.5)
))
gear$sd <- gear$sd

# Labels
gear$island     <- factor(gear$island, levels = island, labels = island_label)
gear$gear_general  <- unlist(lapply(gear$gear_general, function(gear) gear_general_label[which(gear_general == gear)]))
gear$gear_general  <- factor(gear$gear_general,levels=gear_general_label)

# Create plot
c <-  ggplot(gear,aes(x = gear_general, y = mean, fill = island, ymin=mean, ymax=mean+sd))+ 
  geomemptybarplot + geombarplot +  geom_errorbar(width = 0.2) +
  xlab("Main gear types") + ylab("Fishing trip counts")+ 
  labs(fill="Island") + facet_grid(.~island) + 
  scale_fill_manual(values = c("firebrick","#00BFC4") ) +
  ggtheme +   
  theme(plot.margin = margin(t = 4, r = 6, b = 10, l = 6, unit = "pt"))






### STEP 6: CREATE FIGURE
svg(file.path(figures, "Fig2.gear_composition.svg"), width = 8.5, height =8)
cowplot::plot_grid(a,b,c,ncol=1,align = "v", labels = c("A", "B", "C"))
dev.off()

pdf(file.path(figures, "Fig2.gear_composition.pdf"), width = 8.5, height =8)
cowplot::plot_grid(a,b,c,ncol=1,align = "v", labels = c("A", "B", "C"))
dev.off()

rm(a,b,c,geombarplot,geomemptybarplot,geompatternbarplot,ggtheme,gear,i,
   propulsion,propulsion_label,remove)











#########################################################################
#       Function to create histogram with relative frequencies          #
#########################################################################


hist_dat <- function(x,
                     bins, 
                     dat, 
                     transform = NA, 
                     from = NA, 
                     to = NA) {
  
  dat = data.frame(x = as.numeric(dat[,c(x)]), Island = dat$island, Gear = dat$gear_type)
  if(class(transform) == "function") suppressWarnings({dat$x = transform(dat$x)})
  temp = na.omit(dat[,c("x")])
  temp = temp[is.finite(temp)]
  if(is.na(from)) from = min(temp)
  if(is.na(to))   to   = max(temp)
  output = as.data.frame(expand.grid(
    x = seq(from = from, to = to, length.out = bins),
    Island = levels(as.factor(dat$Island)),
    Gear   = levels(as.factor(dat$Gear))
  ))
  output$count = as.integer(NA)
  for(i in 1:nrow(output)) {
    bin1 = output$x[i]
    bin0 = as.numeric(levels(as.factor(output$x)))
    bin0 = bin0[which(bin0<bin1)]
    if(length(bin0) == 0) {
      output$count[i] = length(which(
        dat$Island == output$Island[i] &
          dat$Gear   == output$Gear[i]   &
          dat$x <= bin1
      ))
    }
    if(length(bin0) > 0) {
      bin0 = bin0[length(bin0)]
      output$count[i] = length(which(
        dat$Island   == output$Island[i] &
          dat$Gear   == output$Gear[i]   &
          dat$x <= bin1 & dat$x > bin0
      ))
    }
  }
  output$y = as.numeric(output$count)
  for(i in 1:nrow(output)) 
    output$y[i] = 100 * (output$count[i] / 
                           sum(output$count[which(output$Gear == output$Gear[i] &
                                                    output$Island == output$Island[i])]))
  return(output)}




plothistograms <- function(dat,vline = FALSE,intercept = NULL){
  outplot = ggplot(dat = dat, 
                   aes(x=x, y = y, fill = Island) )
  if(vline) outplot = outplot + 
      geom_vline(xintercept = intercept, 
                 color="grey70", 
                 linetype = "dashed")
  outplot = 
    outplot +
    geom_bar(colour = "white", stat = "identity", fill = "white") + 
    geom_bar(colour = "black", stat = "identity", alpha = 0.7) + 
    facet_grid(Gear ~Island, scales = "free_y") + guides(fill = FALSE) + 
    scale_fill_manual(values = c("firebrick","#00BFC4") ) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt"))  
  return(outplot)}






#########################################################################
#                PLOT EFFORT INDICATORS OF H&L FISHING                  #
#########################################################################




linegears <- effort[which(effort$gear_type %in% hook_line),]
for(i in 1:4) linegears$gear_type[which(linegears$gear_type == hook_line[i])] <- hook_line_label[i]
linegears$island <- as.character(linegears$island)
for(i in 1:2) linegears$island[which(linegears$island == island[i])] <- island_label[i]





dat = hist_dat(x="n_hooks",bins = 25, dat = linegears, transform = log10)
brks      <- c(1,2,5,10,20,50,100,200,500,1000)
nhooks   <- plothistograms(dat = dat,      vline = TRUE,
                           intercept = log10(c(1,10,100,1000))) +
            scale_x_continuous(breaks = log10(brks), labels = brks)       + 
            xlab("Number of hooks") + ylab("% of observations") +
            theme(plot.margin = margin(t = 4, r = 10, b = 4, l = 4, unit = "pt"),
                  axis.text.x = element_text(angle = 35, hjust = 1))



dat = hist_dat(x="n_lines",bins = 5, dat = linegears, from = 1, to = 5)
nlines   <- plothistograms(dat = dat) + 
            ylab("% of observations")  + 
            xlab("Number of lines") +
            theme(plot.margin = margin(t = 4, r = 8, b = 4, l = 8, unit = "pt"))




dat = hist_dat(x="hook_size",bins = 20, dat = linegears, from = 1)
hooksize <- plothistograms(dat = dat) +
            ylab("Number of observations")  + 
            xlab("Hook size (largest = 1, smallest = 20)")+
            theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 16, unit = "pt"))
  



### CREATE FIGURE
# Save to svg
svg(file.path(figures,"FigS3_hook_line_effort.svg"), width = 11, height =5)
cowplot::plot_grid(nhooks,nlines,hooksize,ncol=3,align = "h", labels = c("A", "B","C"))
dev.off()

# Save to svg
pdf(file.path(figures,"FigS3_hook_line_effort.pdf"), width = 11, height =5)
cowplot::plot_grid(nhooks,nlines,hooksize,ncol=3,align = "h", labels = c("A", "B","C"))
dev.off()


rm(hooksize,nlines, nhooks, dat, linegears)








#########################################################################
#                PLOT EFFORT INDICATORS OF NET FISHING                  #
#########################################################################





netgears <- effort[which(effort$gear_type %in% c("surface_gillnet","seine_net","demersal_gillnet")),]
for(i in 1:4) netgears$gear_type[netgears$gear_type == gear_general[i]] <- gear_general_label[i]
netgears$island <- as.character(netgears$island)
for(i in 1:2) netgears$island[which(netgears$island == island[i])] <- island_label[i]





dat = hist_dat(x="net_length",bins = 30, dat = netgears)
net_length   <- plothistograms(dat = dat)       + 
  xlab("Net length (m)") + ylab("% of observations") +
  theme(plot.margin = margin(t = 4, r = 16, b = 4, l = 4, unit = "pt"),
        panel.grid.minor = element_line())



dat = hist_dat(x="net_depth",bins = 20, dat = netgears, from = log10(50), transform = log10)
brks      <- c(20,50,100,200,500,1000)
net_depth   <- plothistograms(dat = dat,      vline = TRUE,
                               intercept = log10(c(100,1000))) + 
  scale_x_continuous(breaks = log10(brks), labels = brks)       + 
  ylab("% of observations")  + 
  xlab("Net depth (number of rows)") +
  theme(plot.margin = margin(t = 4, r = 8, b = 4, l = 8, unit = "pt"))





dat = hist_dat(x="mesh_size",bins = 14, dat = netgears, from = 10, to = 150)
mesh_size   <- plothistograms(dat = dat)       + 
  xlab("Mesh size (mm)") + ylab("% of observations") +
  theme(plot.margin = margin(t = 4, r = 4, b = 4, l = 16, unit = "pt"))



# Save to svg
svg(file.path(figures,"FigS4_net_gears_effort.svg"), width = 11, height =4)
cowplot::plot_grid(net_length,net_depth,mesh_size,ncol=3,align = "h", labels = c("A", "B","C"))
dev.off()


pdf(file.path(figures,"FigS4_net_gears_effort.pdf"), width = 11, height =4)
cowplot::plot_grid(net_length,net_depth,mesh_size,ncol=3,align = "h", labels = c("A", "B","C"))
dev.off()


rm(mesh_size,net_depth,net_length,netgears)







  #########################################################################
  #                             BAIT TYPES                                #
  #########################################################################


bait <- read.table(paste0(getwd(),"/data/bait.csv"), sep=",",header=TRUE)


baitsummary <- function(island,gear,threshold){
  if(gear == "hook_line"  |  gear == "hook_line_shore") gear_category = "gear_general" else gear_category = "gear_type"
  dat = paste0(effort$bait, ", ")
  if(gear_category == "gear_general")
    dat = dat[which(effort$island == island & effort[,which(colnames(effort) == gear_category)] == gear & effort$bait_type == "BAIT")]
  if(gear_category == "gear_type")
    dat = dat[which(effort$island == island & 
                      effort[,which(colnames(effort) == gear_category)] == gear & 
                      effort$bait_type == "BAIT"   & 
                      effort$gear_general == "hook_line")]
  dat = unlist(tolist(dat))
  for(i in 1:nrow(bait)) dat[which(dat == bait$Bait[i])] = bait$Reclass[i]
  bait_types = levels(as.factor(dat))
  bait_types = data.frame(bait = bait_types, n= unlist(lapply(bait_types,function(bait) length(which(dat == bait))  )))
  bait_types = arrange(bait_types,-n)
  bait_types = rbind(bait_types[c(1:threshold),], data.frame(bait = "Other", n = sum(bait_types$n[(threshold+1):nrow(bait_types)])))
  bait_types$fontface = as.character(NA)
  bait_types$fontface = unlist(lapply(bait_types$bait,function(bt) bait$fontface[which(bait$Reclass == bt)][1]))
  bait_types$bait[which(bait_types$bait == "Peixinho")] <- "'Peixinho'"
  return(bait_types)}


baitbarplot <- function(island,threshold, type = "plot"){
  for(i in 1:4){
    gear = c("hook_line","demersal_line","handline","surface_troll")[i]
    temp      = baitsummary(island,gear, threshold)
    temp$gear = 
         paste0(
           c("All sub-métiers", "Demersal line",
                 "Handline", "Surface troll")[i], " (n=", sum(temp$n),")")
    temp$perc = 100*temp$n/sum(temp$n)
    if(i == 1) dat = temp else dat = rbind(dat,temp)
  }
  labsnomarkdown     = dat$bait
  dat$bait = 1:nrow(dat)
  dat$island = c("Príncipe","São Tomé")[which(c("PC","ST") == island)]
  labs = labsnomarkdown
  labs[which(dat$fontface == "italic")] <- 
    paste0("*", labs[which(dat$fontface == "italic")], "*")
  if(island == "PC") fill= "firebrick" else fill = "#00BFC4"
  output = ggplot(data = dat, 
         aes(x = reorder(bait, -bait), y = perc)) +
    geom_bar(stat = "identity", fill = "white") + 
    geom_bar(stat = "identity", fill = fill, colour = "black", alpha = 0.7) + 
    scale_x_discrete(breaks = dat$bait, label = labs)+
    xlab("") + ylab("% of observations") +
    coord_flip() + facet_grid(gear ~ island, scales = "free") +
    theme_bw() + theme(panel.grid = element_blank(),
                       panel.grid.major.x = element_line(colour = "grey85"),
                       axis.text.y = element_markdown())
  dat$bait = labsnomarkdown
  return(if(type == "plot") output else dat)
}



wdth = 12
hgth = 7
relativesize = 0.3
thr = 5


p2 <- ggplot(data.frame(x=c(0,1),y=c(0,1)),aes(x=x,y=y)) + 
  geom_blank() + theme_void() + scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "point"))
temp      = rbind(baitbarplot("PC",thr, type = "data"),
                  baitbarplot("ST",thr, type = "data"))
sp        = levels(as.factor(temp$bait))
sp        = sp[!sp %in% c("Other","")]
fontface  = unlist(lapply(sp,function(bt) temp$fontface[which(temp$bait == bt)][1]))
yposition = seq(from = 1-(1/(2*length(sp))),to = 0, by=-(1/length(sp)))
imageposition = 0.5
size = 3.5
p2        = p2 + annotate("text",        x=rep(imageposition,length(sp)), 
                          y = yposition, label = sp, hjust = 1, size = size,
                          fontface = fontface)
sp[which(sp == "Crab / Hermit crab")] <- "Hermit crab"
fishpictures <- file.path(getwd(),"fish_pictures","bait")
for(i in 1:length(sp)){
  if(paste0(sp[i],".JPG") %in% list.files(fishpictures)) {
    fish = jpeg::readJPEG(source = file.path(fishpictures,paste0(sp[i],".JPG")),native=TRUE)
  } else {
    if(paste0(sp[i],".jpg") %in% list.files(fishpictures)) {
      fish = jpeg::readJPEG(source = file.path(fishpictures,paste0(sp[i],".jpg")),native=TRUE)
    } else {
    fish = jpeg::readJPEG(source = file.path(fishpictures,"blank.JPG"),native=TRUE)
  }}
  p2 = p2 + annotation_raster(fish, xmin = imageposition + 0.02, 
                              xmax = imageposition + 0.02 + (((1/length(sp))*2.9)/((wdth*relativesize)/hgth)), 
                              ymin = 1-((1/length(sp))*i), 
                              ymax = 1-((1/length(sp))*(i-1))) 
}


figname = "FigS5_bait_types"
# Save to PDF
pdf(paste0(figures,"/",figname,".pdf"), width = wdth, height =hgth)
cowplot::plot_grid(baitbarplot("PC",thr),
                        baitbarplot("ST",thr),
                        p2, ncol=3, nrow = 1, labels = c("A", "B","C"),
                        rel_widths=c(relativesize,relativesize,relativesize))
dev.off()

# Save to svg
svg(paste0(figures,"/",figname,".svg"), width = wdth, height =hgth)
cowplot::plot_grid(baitbarplot("PC",thr),
                   baitbarplot("ST",thr),
                   p2, ncol=3, nrow = 1, labels = c("A", "B","C"),
                   rel_widths=c(relativesize,relativesize,relativesize))
dev.off()

