
# combine priors for the size of production areas at Ustjoki, Tsars & Kevo

# point estimates for the size of prod areas (in hectares) based on NINA report
mu_TMS<-2052.5 # Teno main stem

mu_U<-98
mu_Tsars<-83
mu_Kevo<-69

mu_Inari<-564.2
mu_Karigas<-29.9
mu_Iskoras<-21.3
mu_Goss<-260.3
mu_Kietsi<-19.9
mu_Inari+mu_Karigas+mu_Iskoras+mu_Goss+mu_Kietsi
#895.6

cv<-0.5
Tau<-1/log(cv*cv+1)

M_TMS<-log(mu_TMS)-0.5/Tau
M_U<-log(mu_U)-0.5/Tau
M_Tsars<-log(mu_Tsars)-0.5/Tau
M_Kevo<-log(mu_Kevo)-0.5/Tau
M_Inari<-log(mu_Inari)-0.5/Tau
M_Karigas<-log(mu_Karigas)-0.5/Tau
M_Iskoras<-log(mu_Iskoras)-0.5/Tau
M_Goss<-log(mu_Goss)-0.5/Tau
M_Kietsi<-log(mu_Kietsi)-0.5/Tau

M_TMS;M_U;M_Tsars;M_Kevo;Tau
M_Inari;M_Karigas;M_Iskoras;M_Goss;M_Kietsi

M_areas<-"
model{

#Teno main stem
A_TMS~dlnorm(M_TMS,Tau)

# Utsjoki main stem and tributaries
A_U[1]~dlnorm(M_U,Tau) # Utsjoki main stem
A_U[2]~dlnorm(M_Tsars,Tau) # Tsars
A_U[3]~dlnorm(M_Kevo,Tau) # Kevo
#A[3]~dlnorm(M_Kevo,Tau) # Kevo

# Inarijoki main stem and tributaries
A_I[1]~dlnorm(M_Inari,Tau) # Inarijoki main stem
A_I[2]~dlnorm(M_Karigas,Tau) # Karigasjoki
A_I[3]~dlnorm(M_Iskoras,Tau) # Iskorasjohka
A_I[4]~dlnorm(M_Goss,Tau) # Gossjohka
A_I[5]~dlnorm(M_Kietsi,Tau) # Kietsimajoki

Atot_Utsjoki[1]<-sum(A_U[])
Atot_Inari[1]<-sum(A_I[])

# check: Torne:

A_Torne<-exp(AL)
AL~dnorm(8.595,Atau)
Atau<-1/pow(0.1417,2)

# check:Utsjoki total

Atot_Utsjoki[2]<-exp(AL_U)
AL_U~dnorm(5.4816,1/pow(0.2823,2))

# check: Inarijoki

Atot_Inari[2]<-exp(AL_I)
AL_I~dnorm(6.7516,1/pow(0.3518,2))

}"

modelName<-"areas"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M_areas,file=Mname)

data<-list(
M_TMS=M_TMS,
M_U=M_U, M_Tsars=M_Tsars, M_Kevo=M_Kevo, Tau=Tau,
M_Inari=M_Inari,
M_Kietsi=M_Kietsi,
M_Goss=M_Goss,
M_Iskoras=M_Iskoras,
M_Karigas=M_Karigas

)



var_names<-c("A_TMS","Atot_Utsjoki", "Atot_Inari", "A_Torne")

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

#                 Lower95    Median  Upper95      Mean        SD Mode     MCerr MC%ofSD SSeff       AC.10      psrf
# Atot_Utsjoki[1]  123.637  240.1480  391.589  249.2395  71.68366   NA  1.602895     2.2  2000  0.02933796 1.0019669
# Atot_Utsjoki[2]  129.564  240.1500  389.080  249.8511  70.65360   NA  1.620566     2.3  1901  0.00759521 0.9995255
# Atot_Inari[1]    383.965  838.2285 1499.590  894.1385 305.71817   NA  7.249700     2.4  1778  0.05813882 1.0069359
# Atot_Inari[2]    391.621  851.8540 1598.890  910.7112 320.89441   NA  6.595485     2.1  2367  0.03081335 1.0040560
# A_Torne         4004.840 5427.4100 7035.900 5459.9509 780.35004   NA 17.449157     2.2  2000 -0.02501379 0.9998307


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

# Inari main stem
# => EA:
log(564)-0.5*log(0.484*0.484+1)
#6.230

# SA: 
sqrt(log(0.484*0.484+1))
#0.459

# Inari tot
# => EA:
log(910)-0.5*log(0.363*0.363+1)
#6.752

# SA: 
sqrt(log(0.363*0.363+1))
#0.352



#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)


