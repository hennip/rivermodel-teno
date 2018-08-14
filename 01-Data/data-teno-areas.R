
# combine priors for the size of production areas at Ustjoki, Tsars & Kevo

# point estimates for the size of prod areas (in hectares) based on NINA report
mu_U<-98
mu_T<-83
mu_K<-69

cv<-0.5
Tau<-1/log(cv*cv+1)

M_U<-log(mu_U)-0.5/Tau
M_T<-log(mu_T)-0.5/Tau
M_K<-log(mu_K)-0.5/Tau

M_U;M_T;M_K;Tau


M_areas<-"
model{

A[1]~dlnorm(M_U,Tau) # Utsjoki
A[2]~dlnorm(M_T,Tau) # Tsars
A[3]~dlnorm(M_K,Tau) # Kevo

Atot[1]<-sum(A[1:3])

# check: Torne:

A[4]<-exp(AL)
AL~dnorm(8.595,Atau)
Atau<-1/pow(0.1417,2)

# check:Utsjoki total

Atot[2]<-exp(AL_U)
AL_U~dnorm(5.4816,1/pow(0.2823,2))



}"

modelName<-"areas"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M_areas,file=Mname)

data<-list(
M_U=M_U, M_T=M_T, M_K=M_K, Tau=Tau
)



var_names<-c("A","Atot")

#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

t1<-Sys.time();t1
runX <- run.jags(M_areas, 
                 data=data,monitor=var_names, inits=NA,
                 n.chains = 2, method = 'parallel', thin=1, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 1000, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)

summary(runX)

#> summary(run0)
#      Lower95    Median Upper95      Mean       SD Mode     MCerr MC%ofSD SSeff        AC.10      psrf
#A[1]  26.3093  86.96880 191.892  97.84075 48.63198   NA 1.0145092     2.1  2298 -0.012874084 1.0005372
#A[2]  25.3272  73.85335 165.104  82.27916 41.03057   NA 0.9174714     2.2  2000 -0.017428864 0.9996242
#A[3]  18.0691  62.20365 137.455  69.95214 34.91631   NA 0.7807524     2.2  2000  0.007702853 1.0003009
#Atot 121.9020 240.32750 390.882 250.07203 72.27970   NA 1.6162233     2.2  2000 -0.015312119 1.0008461


# => EA:
log(250)-0.5*log(0.288*0.288+1)
#5.4816

# SA: 
sqrt(log(0.288*0.288+1))
#0.2822

#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)


