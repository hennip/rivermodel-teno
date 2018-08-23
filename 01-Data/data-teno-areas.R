
# combine priors for the size of production areas at Ustjoki, Tsars & Kevo

# point estimates for the size of prod areas (in hectares) based on NINA report
mu_U<-98
mu_T<-83
mu_K<-69

mu_Inari<-564.2

cv<-0.5
Tau<-1/log(cv*cv+1)

M_U<-log(mu_U)-0.5/Tau
M_T<-log(mu_T)-0.5/Tau
M_K<-log(mu_K)-0.5/Tau
M_Inari<-log(mu_Inari)-0.5/Tau

M_U;M_T;M_K;Tau
M_Inari

M_areas<-"
model{

A[1]~dlnorm(M_U,Tau) # Utsjoki
A[2]~dlnorm(M_T,Tau) # Tsars
A[3]~dlnorm(M_K,Tau) # Kevo
A_Inari[1]~dlnorm(M_Inari,Tau) # Inarijoki

Atot_Utsjoki[1]<-sum(A[1:3])

# check: Torne:

A_Torne<-exp(AL)
AL~dnorm(8.595,Atau)
Atau<-1/pow(0.1417,2)

# check:Utsjoki total

Atot_Utsjoki[2]<-exp(AL_U)
AL_U~dnorm(5.4816,1/pow(0.2823,2))

# check: Inarijoki

A_Inari[2]<-exp(AL_I)
AL_I~dnorm(6.230,1/pow(0.484,2))


}"

modelName<-"areas"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M_areas,file=Mname)

data<-list(
M_U=M_U, M_T=M_T, M_K=M_K, Tau=Tau,
M_Inari=M_Inari
)



var_names<-c("A","Atot_Utsjoki", "A_Inari", "A_Torne")

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

# > summary(runX)
#                 Lower95     Median  Upper95       Mean        SD Mode      MCerr MC%ofSD SSeff
# A[1]              27.5852   88.64395  191.786   97.61487  47.48326   NA  1.0617579     2.2  2000
# A[2]              20.0549   74.42120  165.327   83.13576  41.00655   NA  0.9435958     2.3  1889
# A[3]              19.4857   61.17695  144.338   69.96165  36.05425   NA  0.8061975     2.2  2000
# Atot_Utsjoki[1]  131.9840  240.35100  395.326  250.71228  71.32517   NA  1.5948793     2.2  2000
# Atot_Utsjoki[2]  126.2700  240.80700  400.724  252.08090  73.09079   NA  1.6343597     2.2  2000
# A_Inari          133.2270  500.37800 1095.740  556.91499 273.04181   NA  6.1054004     2.2  2000
# A_Torne         3888.7700 5400.88000 6983.570 5434.23459 791.30886   NA 16.9629061     2.1  2176
#                     AC.10      psrf
# A[1]             0.007606038 1.0035862
# A[2]             0.004350305 0.9997386
# A[3]             0.028862869 0.9998445
# Atot_Utsjoki[1]  0.007161290 0.9998555
# Atot_Utsjoki[2] -0.004951689 1.0042170
# A_Inari         -0.019451782 1.0026112
# A_Torne          0.037665442 0.9997480


# Utsjoki
# => EA:
log(250)-0.5*log(0.288*0.288+1)
#5.4816

# SA: 
sqrt(log(0.288*0.288+1))
#0.2822

#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)

# Inari
# => EA:
log(564)-0.5*log(0.484*0.484+1)
#6.230

# SA: 
sqrt(log(0.484*0.484+1))
#0.459

#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)


