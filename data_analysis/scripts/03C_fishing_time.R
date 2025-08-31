


#########################################################################
#########################################################################
#                              FISHING TIMES                            #
#########################################################################
#########################################################################









#########################################################################
#---------------------------  DIRECTORIES  -----------------------------#
#########################################################################


## Directories exist? If not, create them
figures    <- file.path(getwd(), "data_analysis", "outputs", "figures")
if(!file.exists(figures)) dir.create(figures)



# Data management
if(!require("dplyr"))   install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)


# Plots
if(!require("ggplot2"))   install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require("cowplot"))   install.packages("cowplot", repos = "https://cloud.r-project.org")
library(cowplot)


#########################################################################
#-----------------------------  LOAD DATA  -----------------------------#
#########################################################################





### RUN SCRIPTS 1A AND 1B
# 1A -> LOADS & CLEANS DATA
# 1B -> ADDS COVARIATES TO CATCH AND EFFORT
#source(file.path("data_analysis","scripts","02A_variable_definition.R"))



tripinfo <-  read.table(file.path(getwd(),"data_analysis", "data_with_covariates","tripinfo.csv"), header=TRUE, sep = ",")









dat  = tripinfo
strt = tripinfo$departure
ends = tripinfo$arrival
dat$dep <- as.numeric(substr(strt,1,2))+(as.numeric(substr(strt,4,5))/60)
dat$lan <- as.numeric(substr(ends,1,2))+(as.numeric(substr(ends,4,5))/60)
dat$gear = dat[,which(colnames(dat) == "gear_general")]
timehistogram <- data.frame(Island = dat$island, Gear = dat$gear, n=1)
n=1.5
for(i in c(0:(24*n))/n) timehistogram = data.frame(timehistogram, n = i)
for(i in 1:((24*n) +1)){
  ones <- dat$dep > dat$lan   &   timehistogram[,i+3] < dat$lan | 
    dat$dep > dat$lan   &   timehistogram[,i+3] > dat$dep |
    dat$dep < dat$lan   &   timehistogram[,i+3] < dat$lan  & timehistogram[,i+3] > dat$dep
  timehistogram[which(ones),i+3] <- 1
  timehistogram[which(!ones),i+3] <- 0
}
timehistogram <- timehistogram[-which(is.na(dat$dep) | is.na(dat$lan)),]
for(i in 1:((24*n)+1)){
  temp = data.frame(timehistogram[,c(1,2)] , time = c(c(0:(24*n))/n)[i], active = timehistogram[,i+3])
  if(i == 1) activevessels = temp else activevessels = rbind(activevessels, temp)
}
activevessels$Island = as.factor(activevessels$Island)
activevessels$Gear   = as.factor(activevessels$Gear)

activevessels_all = activevessels[,c("Island","time", "active")]
activevessels <- as.data.frame(activevessels %>% group_by(Island, Gear, time) %>% summarise(sum = sum(active))  )
activevessels$active <- activevessels$sum / unlist(lapply(1:nrow(activevessels), function(i) 
  length(which(dat$island == activevessels$Island[i] & 
                 dat$gear == activevessels$Gear[i] &
                 !is.na(dat$time_sea)))))
activevessels = activevessels[-which(activevessels$Gear == "beach_seine"),]
activevessels$active <- activevessels$active * 100


activevessels_all <- as.data.frame(activevessels_all %>% group_by(Island, time) %>% summarise(sum = sum(active))  )
activevessels_all$active <- activevessels_all$sum / unlist(lapply(1:nrow(activevessels_all), function(i) 
  length(which(dat$island == activevessels_all$Island[i] &
                 !is.na(dat$time_sea)))))  
activevessels_all$active <- activevessels_all$active * 100
activevessels_all$Gear = "All métiers"


sc = scale_x_continuous(breaks = c(0,6,12,18,24)) 
ggtheme <- theme_bw() +
  theme(panel.grid = element_blank())
nightcolour = "grey90"





gear_general <- c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net",
                  "hook_line",    "spear_fishing", "hook_line_shore")
gear_general_label = c("Scoop net", "Surface gillnet", "Demersal gillnet", "Seine net",
                       "Hook & line", "Spear fishing", "Hook & line (sh.)")
activevessels$Gear <- as.character(activevessels$Gear)
for(i in 1:length(gear_general))  activevessels$Gear[activevessels$Gear == gear_general[i]] <- gear_general_label[i]
activevessels$Island <- factor(as.character(activevessels$Island), levels = c("PC","ST"), labels = c("Príncipe", "São Tomé"))

activevessels_all$Island <- factor(as.character(activevessels_all$Island), levels = c("PC","ST"), labels = c("Príncipe", "São Tomé"))

axistitlesize = 9.5
a <- ggplot(activevessels, aes(x=time,y = active, colour = Island)) + 
  geom_rect(aes(xmin = 0, xmax = 6, ymin = 0, ymax = 100), colour = NA, fill = nightcolour) + 
  geom_rect(aes(xmin = 18, xmax = 24, ymin = 0, ymax = 100), colour = NA, fill = nightcolour) + 
  geom_line(linewidth = 0.6) + facet_grid(Gear~.) + sc + ggtheme +
  scale_colour_manual(values = c("firebrick","#00BFC4") )+ 
  xlab("Time of day") + ylab("% of active vessels")+
  theme(axis.title = element_text(size = axistitlesize))
b <- ggplot(activevessels_all, aes(x=time,y = active, colour = Island)) + 
  geom_rect(aes(xmin = 0, xmax = 6, ymin = 0, ymax = 100), colour = NA, fill = nightcolour) + 
  geom_rect(aes(xmin = 18, xmax = 24, ymin = 0, ymax = 100), colour = NA, fill = nightcolour) + 
  geom_line(linewidth = 0.6)  + facet_grid(Gear~.) + sc + ggtheme +
  scale_colour_manual(values = c("firebrick","#00BFC4") ) + 
  xlab("Time of day") + ylab("% of active vessels") +
  theme(axis.title = element_text(size = axistitlesize))

svg(filename = file.path(figures,"FigS6_fishing_times.svg"),height = 9.6,width = 7)
plot_grid(b,a,ncol = 1, align = "hv", axis = "lr", rel_heights = c(1.6,6), 
          labels = c("A","B"))
dev.off()


pdf(file = file.path(figures,"FigS6_fishing_times.pdf"),height = 9.6,width = 7)
plot_grid(b,a,ncol = 1, align = "hv", axis = "lr", rel_heights = c(1.6,6), 
          labels = c("A","B"))
dev.off()


  
  
  
  