

#source("00-Functions/packages-and-paths.r")

df.bugs<-read.table("C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/SmoltW.txt", header=T)

colnames(df.bugs) = c("M1", "T1", "M2", "T2", "M3", "T3", "M4", "T4", "M5", "T5", 
                      "M6", "T6","M7", "T7", "M8", "T8", "M9", "T9", "M10", "T10", 
                      "M11", "T11", "M12", "T12", "M13", "T13","M14", "T14", 
                      "M15", "T15", "M16", "T16")

#M<-df.bugs[,1]
#Tau<-df.bugs[,2]
#SD<-1/sqrt(Tau)




M1<-"
model{
for(y in 1:Nyears){
  x[y]~dlnorm(M[y],Tau[y])
}

}"

modelName<-"test"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

data<-list(
  Nyears=length(as.matrix(df.bugs[,1])),
  M=df.bugs[,1],
  Tau=df.bugs[,2])



var_names<-c(
  "x")


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

run1 <- run.jags(M1, 
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =10000, adapt = 100, 
                 progress.bar=TRUE)

summary(run1)
