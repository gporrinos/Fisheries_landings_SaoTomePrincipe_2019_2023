




#########################################################################
#########################################################################
#               ANALYSIS OF THE NUMBER OF TRIPS DATABASE                #
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
#                     LOAD (AND INSTALL) PACKAGES                       #
#########################################################################


# GAMs
if(!require("mgcv"))    install.packages("mgcv",    repos = "https://cloud.r-project.org")
library(mgcv)

# Data manipulation
if(!require("dplyr"))   install.packages("dplyr",   repos = "https://cloud.r-project.org")
library(dplyr)


# Parallel
if(!require("parallel"))   install.packages("parallel",   repos = "https://cloud.r-project.org")
library(parallel)

# MuMIn
if(!require("MuMIn"))   install.packages("MuMIn",   repos = "https://cloud.r-project.org")
library(MuMIn)

# Plot things
if(!require("ggplot2"))   install.packages("ggplot2",   repos = "https://cloud.r-project.org")
library(ggplot2)
if(!require("gratia"))   install.packages("gratia",   repos = "https://cloud.r-project.org")
library(gratia)
if(!require("cowplot"))   install.packages("cowplot",   repos = "https://cloud.r-project.org")
library(cowplot)

#########################################################################
#                   PREPARE NTRIPS DATA FOR ANALYSIS                    #
#########################################################################


ntrips <- read.csv(file.path(getwd(),"data","ntrips.csv"))




### STEP 1: Date as number
startyear               <- c("2019-01-01","2020-01-01","2021-01-01","2022-01-01","2023-01-01")
startyear               <- as.integer(as.POSIXct(startyear,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)
ntrips$date_astext      <- ntrips$date
ntrips$date             <- as.integer(as.POSIXct(ntrips$date,format = c("%Y-%m-%d"),tz="UTC"))/(24*60*60)
ntrips$ordinaldate      <- unlist(lapply(ntrips$date,function(x){x-max(startyear[!startyear>x])}))


###  STEP 2: Remove sites with highly incomplete time series and NAs
retain_sites <- c("AB", "AN", "BU",  "CA",  "CO",  "LA",  "RI",  "SA",  "UN",  "AT",  "IG",  "IR", "MA",  "PA",  "RA",  "RP")
ntrips <- ntrips[-which(is.na(ntrips$date) | 
                          !ntrips$site %in% retain_sites |  
                          as.character(ntrips$island) == ""),]





### STEP 3: Merge the two types of seine nets, since they could not be accurately discriminated in the field
ntrips$seine_net     <- ntrips$purse_seine + ntrips$surface_gillnet_seine


### STEP 4: Transform database
gears             <- c("circular_net", "surface_gillnet", "demersal_gillnet", "seine_net",
                       "hook_line", "hook_line_shore", "spear_fishing" )
for(i in 1:length(gears)){
  temp = data.frame(site   = ntrips$landing_site,
                    code   = ntrips$site,
                    island = ntrips$island, 
                    date   = ntrips$date,
                    ordinaldate = ntrips$ordinaldate,
                    ntrips = ntrips[,which(colnames(ntrips) == gears[i])],
                    gear   = gears[i])
  if(i == 1) dat.ntrips = temp else dat.ntrips = rbind(temp,dat.ntrips)
}
dat.ntrips$island <- as.factor(dat.ntrips$island)
dat.ntrips$site   <- as.factor(dat.ntrips$site)
dat.ntrips$gear   <- as.factor(dat.ntrips$gear)




#########################################################################
#                           RUN HURDLE MODEL                            #
#########################################################################




# Run hurdle model
poissonparameter <-   as.formula(
  ntrips ~ 
    island +
    s(ordinaldate, bs = 'cc', m = 1) +
    s(ordinaldate, by = gear, bs = 'cc', m = 1) +  
    s(ordinaldate, by = island, bs = 'cc', m = 1) +   
    s(date, m = 1) +
    s(date, by = gear, m = 1) +
    s(date, by = island, m = 1) +
    gear +
    gear:island )


probabilityofpresence <- as.formula(~gear:site)






# Extract variable names and remove NAs
dat             <- dat.ntrips
variablenames   <- levels(as.factor(c(all.vars(poissonparameter), all.vars(probabilityofpresence))))
poisson.eff     <- labels(terms(poissonparameter))
response        <- as.character(poissonparameter[2])
dat             <- na.omit(dat[,c(variablenames)])

# Create all possible combinations of parameters
id   = unlist(
  lapply(1:length(poisson.eff), 
         function(i)
           combn(1:length(poisson.eff),i,simplify=FALSE)
  ),recursive=FALSE)
id <- c(id,"a")
poisson.form = sapply(id,
                      function(i)
                        paste(response,"~",paste(poisson.eff[i],collapse="+"))
)
poisson.form = c(poisson.form,paste(response, "~ 1"))
filenames    = paste0(
  unlist(lapply(id,function(x)  paste(x,collapse="_"))), 
  ".rds")
presence = c(rep("~gear:site",length(filenames)), 
             rep("~1",length(filenames)))
filenames = c(filenames,
              paste0("n",filenames))
poisson.form = c(poisson.form,poisson.form)


# Create output directory, if necessary
directory = response
directory = file.path(r_objects,directory)
if(!file.exists(directory))       dir.create(directory)


# Remove models already existing in "directory" (from previous run if aborted)
exists       <- list.files(directory)
presence     <- presence[!filenames %in% exists]
filenames    <- filenames[!filenames %in% exists]
poisson.form <- poisson.form[!filenames %in% exists]

direction = "random"
if(direction == "start")  runmodels <- 1:length(filenames)
if(direction == "end")    runmodels <- length(filenames):1
if(direction == "random") runmodels <- sample(c(1:length(filenames)))


# Run all models already existing in "directory" (from previous run if aborted)
ncores = 4
if(length(filenames)>0) {
  cl <- makeCluster(ncores)
  clusterExport(cl = cl, c("dat","directory","presence","filenames","poisson.form"))
  parLapply(cl, runmodels, function(i){
    #    if(!file.exists(file.path(directory,filenames[i]))){
    mod     = mgcv::gam(list(as.formula(poisson.form[i]), 
                             as.formula(presence[i])),
                        data = dat,
                        family = mgcv::ziplss(),
                        method = "REML")
    saveRDS(mod,file.path(directory,filenames[i]))
    #    }
  })
  stopCluster(cl)
}



temp <- lapply(list.files(directory, full.names = TRUE), function(x) readRDS(x))

output = data.frame(a = labels(terms(poissonparameter)))
output = data.frame(t(output))
output[1,] = 1
colnames(output) = labels(terms(poissonparameter))
output$'gear:site' = 1
output$AIC = 1
output = output[-1,]


modelselection <- lapply(temp, function(mod) {
  temp = c(
    unlist(lapply(labels(terms(poissonparameter)), function(x){
      if(x %in% labels(terms(mod$formula[[1]]))) 1 else 0
    })),
    if("gear:site" %in% labels(terms(mod$formula[[2]])) | 
       "site:gear" %in% labels(terms(mod$formula[[2]]))) 1 else 0,
    AICc(mod))
  temp = data.frame(a = temp)
  temp = as.data.frame(t(temp))
  colnames(temp) = colnames(output)
  return(temp)
})

for(i in 1:length(modelselection)){
  output = rbind(output, modelselection[[i]])
}
colnames(output) = c("island","ordinaldate", "ordinaldate:gear","ordinaldate:island", "date", 
                     "date:gear", "date:island","gear","island:gear","gear:site","AIC")
output$model = 1:nrow(output)
output <- arrange(output,AIC)
output$delta <- unlist(lapply(output$AIC,function(aic) aic - min(output$AIC)))
View(output)

# Model 1 was removed as output showed separation.
# After removing Model 1, Model 2 was the only one with delta AIC <6




estimates <- gratia::smooth_estimates(temp[[output$model[2]]])
estimates$smooth = "date"
estimates$smooth[which(grepl("ordinaldate",estimates$'.smooth'))] <- "ordinaldate"
positions <- which(!is.na(estimates$island))
estimates$smooth[positions] <- paste0(estimates$smooth[positions], ":island")
positions <- which(!is.na(estimates$gear))
estimates$smooth[positions] <- paste0(estimates$smooth[positions], ":gear")
estimates <- estimates[-which(estimates$date<18300),]
estimates$smooth = factor(estimates$smooth, 
                          levels = c("ordinaldate", "ordinaldate:island", 
                                     "ordinaldate:gear", "date:gear"),
                          labels = c("Ordinal date", "Ordinal date : Island", 
                                     "Ordinal date : Gear", "Date : Gear"))

dat = estimates[which(estimates$smooth %in% c("Ordinal date : Island", "Ordinal date")),]
marg =0.3
long = 2.8
a <- ggplot(dat = dat,
            aes(x = ordinaldate, y = .estimate, ymin = .estimate - .se, ymax = .estimate + .se, fill = island)) +
  geom_ribbon(alpha = 0.5) + 
  geom_line(dat = dat, aes(x = ordinaldate, y = .estimate, colour = island)) + 
  facet_grid(.~smooth,scales = "free") +
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),
                     breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)+15) +
  
  scale_color_manual(values= c(PC = "firebrick", ST = "#00BFC4", "NA" = "grey"),
                     labels= c("Príncipe", "São Tomé"),
                     breaks= c("PC", "ST")) +
  
  scale_fill_manual(breaks= c("PC", "ST"),
                    labels= c("Príncipe", "São Tomé"),
                    values= c(PC = "firebrick", ST = "#00BFC4", "NA" = "grey")) +
  labs(y = "Estimate", x= NULL, fill = "Island", colour = "Island") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position = "inside",
                     legend.position.inside = c(1.15,0.5),
                     plot.margin = unit(c(marg,long,marg,marg),"cm"))




estimates$x = estimates$date
estimates$x[is.na(estimates$x)] = estimates$ordinaldate[is.na(estimates$x)]
dat = estimates[which(!is.na(estimates$gear)),]
dat$gear <- factor(as.character(dat$gear),
                         levels = c("circular_net", "demersal_gillnet", "hook_line",
                                    "hook_line_shore", "seine_net", "spear_fishing",
                                    "surface_gillnet"),
                         labels = c("Scoop net", "Dem. gillnet", "Hook & line",
                                    "H. & line (shor.)", "Seine net", "Spear fish.",
                                    "Surf. gillnet"))
b <- ggplot(dat = dat,
            aes(x = x, y = .estimate, ymin = .estimate - .se, ymax = .estimate + .se)) +
  geom_ribbon(alpha = 0.6) + geom_line() + facet_grid(gear~smooth, scales = "free") +
  labs(y = "Estimate", x= NULL) +
  ggh4x::facetted_pos_scales(
    x = list(
      smooth == "Ordinal date : Gear" ~ scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),
                                                      breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)+15),
      smooth == "Date : Gear" ~ scale_x_continuous(labels = c("","2020","2021","2022","2023"),
                                              breaks = startyear+(365/2))) 
  )+
  theme_bw() + theme(panel.grid = element_blank(),
                     plot.margin = unit(c(marg,long,marg,marg),"cm"))

svg(file.path(figures,"Fig3.Ntrips_effects.svg"), height = 9, width = 7)
cowplot::plot_grid(a,b,ncol = 1,
                   align = "v",
                   axis = "rl",
                   rel_heights = c(0.25,1),
                   labels = c("A","B"))
dev.off()

pdf(file.path(figures,"Fig3.Ntrips_effects.pdf"), height = 9, width = 7)
cowplot::plot_grid(a,b,nrow = 2,
                   align = "v",
                   axis = "rl",
                   rel_heights = c(0.25,1),
                   labels = c("A","B"))
dev.off()







