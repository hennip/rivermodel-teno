
source("00-Functions/packages-and-paths.r")

#######################
# Load jags simulations 

load(str_c(pathOut,"rivermodel_baltic_run.RData")) 
#run<-run1
chains<-as.mcmc.list(run)
chains<-window(chains,start=1)
