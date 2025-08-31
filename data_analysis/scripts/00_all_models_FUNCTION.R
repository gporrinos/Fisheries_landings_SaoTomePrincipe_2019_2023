



# I did not use MuMIn::dredge because takes too long to run. Instead, I wrote the
# formula "all_models" (available in script below). It first creates all possible 
# combinations of models, and uses different computer cores to run each model via
# the "parallel" package, saving each model in a directory specified by the user. 
# In this case, directory is: 
#           "data_analysis/outputs/r-objects/nameofresponsevariable"
# If the function was run previously, it will retrieve a list of models from saved
# RDS files (each of them containing a model). If the function was curtailed in
# a previous run, it will pick up in the last model and keep running from there.










if(!require("parallel")) install.packages("parallel", repos = "https://cloud.r-project.org")


#########-------------------------------------------------------#########
#                   MODEL SELECTION                    #


### Function that runs all models in different 
all_models <- function(formula, 
                       dat,   
                       family, 
                       directory = NA,
                       freecores = 1,
                       startfromend = FALSE){
  
 # Extract variable names and remove NAs
      variablenames <- all.vars(formula)
      effects       <- labels(terms(formula))
      response     <- as.character(formula[2])
      dat = na.omit(dat[,c(variablenames)])
  
 # Create all possible combinations of parameters
      id   = unlist(
                 lapply(1:length(effects), 
                           function(i)
                             combn(1:length(effects),i,simplify=FALSE)
                       ),recursive=FALSE)
      formulas = sapply(id,
                        function(i)
                           paste(response,"~",paste(effects[i],collapse="+"))
                       )
      formulas = c(paste(response, "~ 1"),formulas)
  
 # Create output directory, if necessary
      if(is.na(directory)) directory = response
      directory = file.path(r_objects,directory)
      if(!file.exists(directory))       dir.create(directory)
  
  # Check if models exist from a previous model run
      exists     = list.files(directory)
      
  # Remove models already existing in "directory" (from previous run if aborted)
      modlist    = paste0(
                     c(unlist(lapply(id,function(x)  paste(x,collapse="_"))),"a"), 
                     ".rds")
      formulas   = formulas[which(!modlist %in% exists)]
      modlist    = modlist[which(!modlist %in% exists)]
      
  # Run all models already existing in "directory" (from previous run if aborted)
      if(length(formulas)>0){
         cl <- makeCluster(detectCores()-freecores)
         if(startfromend) countmods = length(formulas):1   else    countmods = 1:length(formulas)
         fitallmodels <- parLapply(cl, countmods, function(i){
             outfile = file.path(directory,modlist[i])
             if(!file.exists(outfile)){
               mod     = mgcv::gam(as.formula(formulas[i]), 
                                 data = dat,
                                 family = family(),
                                 method = "REML")
               saveRDS(mod,outfile)
             }
           })
         stopCluster(cl)
      }
  # Run all models already existing in "directory" (from previous run if aborted)
        allmodelfiles = list.files(directory, include.dirs = F, full.names = T, recursive = T)
        allmodels = lapply(allmodelfiles,function(x) readRDS(x))

  return(allmodels)}




