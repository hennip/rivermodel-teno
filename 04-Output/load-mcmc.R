
#source("00-Functions/packages-and-paths.r")

#######################
# Load jags simulations 
#RiverNames1=c("TenoMS", "Pulmanki", "Utsjoki", "Tsars", "Kevo", "Inari", "Torne")
#load(str_c(pathOut,"rivermodelTeno_Teno6&Torne.RData")) ; RiverNames<-RiverNames1


RiverNames2=c("TenoMS", "Pulmanki", "Utsjoki", "Tsars", "Kevo", "Inari")
load(str_c(pathOut,"rivermodelTeno_Teno6.RData")) ; RiverNames<-RiverNames2


#run<-run1
chains<-as.mcmc.list(run)
#chains<-window(chains,start=1)
