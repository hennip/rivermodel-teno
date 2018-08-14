
# runjags diagnostics (object "run")

summary(run, var="S")
summary(run, var="S[40,1]")
summary(run, var="S[41,1]")

summary(run, var="S[1,14]")
summary(run, var="S[2,14]")
summary(run, var="S[3,14]")
summary(run, var="S[4,14]")
summary(run, var="S[5,14]")
summary(run, var="S[6,14]")
summary(run, var="S[7,14]")
summary(run, var="S[8,14]")
summary(run, var="S[9,14]")
summary(run, var="S[10,14]")
summary(run, var="S[11,14]")
summary(run, var="S[12,14]")
summary(run, var="S[13,14]")
summary(run, var="S[14,14]")
summary(run, var="S[15,14]")
summary(run, var="S[16,14]")
summary(run, var="S[17,14]")
summary(run, var="S[18,14]")
summary(run, var="S[19,14]")
summary(run, var="S[20,14]")
summary(run, var="S[21,14]")
summary(run, var="S[22,14]")
summary(run, var="S[23,14]")
summary(run, var="S[24,14]")
summary(run, var="S[25,14]")
summary(run, var="S[26,14]")
summary(run, var="S[27,14]")
summary(run, var="S[28,14]")

#Utsjoki
summary(run, var="S[31,14]") #2010
summary(run, var="S[35,14]") #2014

# Torne
summary(run, var="S[31,1]") #2010
summary(run, var="S[35,1]") #2014


summary(run, var="S[29,1]")
summary(run, var="S[29,14]")



summary(run, var="Total")
summary(run, var="phi")
summary(run, var="TS")

summary(run, var="S[1")
plot(run, var="TS")
plot(run, var="phi")
plot(run, var="gamma")



chains<-as.mcmc.list(run)
chains<-window(chains,thin=100)
chains<-window(chains,start=20000)
#save(chains, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_fixedObsProp_0714_chains.RData")


gelman.diag(chains[,"S[40,1]"])
gelman.diag(chains[,"S[41,1]"])
gelman.diag(chains[,"TS[10]"])
gelman.diag(chains[,"TS[20]"])
gelman.diag(chains[,"TS[30]"])
gelman.diag(chains[,"Ntot[6]"])
gelman.diag(chains[,"TS[41]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])
gelman.diag(chains[,"Ntot[6]"])

gelman.diag(chains[,"aP"])
gelman.diag(chains[,"bP"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"cvmuD"])

summary(chains[,"sums1[48]"])
#summary(chains[,"sums[48]"])


# Traces
par(mfrow=c(3,3),mar=c(2.5,4,4,1))
traceplot(chains[,"TS[41]"],main="TS1", cex.main=1.5)
traceplot(chains[,"S[41,1]"],main="s41", cex.main=1.5)
traceplot(chains[,"Ntot[3]"],main="Ntot3", cex.main=1.5)
traceplot(chains[,"Ntot[4]"],main="Ntot4", cex.main=1.5)
traceplot(chains[,"Ntot[5]"],main="Ntot5", cex.main=1.5)
traceplot(chains[,"Ntot[6]"],main="Ntot6", cex.main=1.5)
traceplot(chains[,"Ntot[7]"],main="Ntot7", cex.main=1.5)
traceplot(chains[,"Ntot[8]"],main="Ntot8", cex.main=1.5)
traceplot(chains[,"Ntot[9]"],main="Ntot9", cex.main=1.5)

# compare with bugs


