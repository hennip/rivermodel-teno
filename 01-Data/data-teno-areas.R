
# combine priors for the size of production areas at Ustjoki, Tsars & Kevo

# point estimates for the size of prod areas (in hectares) based on NINA report
mu_TMS<-2052.5 # Teno main stem

mu_Pul<-55.7 # Pulmanki

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
M_Pul<-log(mu_Pul)-0.5/Tau
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
A_TMS[1]~dlnorm(M_TMS,Tau)

#Pulmanki
A_Pul~dlnorm(M_Pul,Tau) 

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

# check: Teno main stem
A_TMS[2]<-exp(AL_TMS)
AL_TMS~dnorm(7.5337,1/pow(0.4812,2))


}"

modelName<-"areas"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M_areas,file=Mname)

data<-list(
M_TMS=M_TMS,
M_Pul=M_Pul,
M_U=M_U, M_Tsars=M_Tsars, M_Kevo=M_Kevo, Tau=Tau,
M_Inari=M_Inari,
M_Kietsi=M_Kietsi,
M_Goss=M_Goss,
M_Iskoras=M_Iskoras,
M_Karigas=M_Karigas

)



var_names<-c("A_TMS","A_Pul","Atot_Utsjoki", "Atot_Inari", "A_Torne", "A_U")

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

#                 Lower95    Median  Upper95      Mean         SD Mode     MCerr MC%ofSD SSeff        AC.10      psrf
# A_TMS            451.997 1867.3800 4114.570 2100.7681 1074.35889   NA 24.023395     2.2  2000  0.001280858 1.0014864
# Atot_Utsjoki[1]  124.850  240.1980  396.742  252.6231   74.22807   NA  1.582502     2.1  2200 -0.024994764 1.0013226
# Atot_Utsjoki[2]  135.478  242.0925  406.181  251.8768   70.85560   NA  1.627823     2.3  1895  0.008611197 1.0002530
# Atot_Inari[1]    409.652  834.4235 1477.740  888.7370  305.13818   NA  7.316300     2.4  1739 -0.028386006 0.9995101
# Atot_Inari[2]    379.159  855.5125 1516.370  903.8401  323.88509   NA  7.068289     2.2  2100  0.030278963 0.9995434
# A_Torne         4027.890 5420.6500 6972.940 5474.4015  774.25011   NA 17.312759     2.2  2000 -0.004964185 0.9997133


# Pulmanki
# EA; SA
log(55.7)-0.5*log(0.4946^2+1); sqrt(log(0.4946^2+1))
#3.9106 
#0.4678



# Utsjoki main
# EA; SA
log(98)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
#4.4694 
#0.4808

# Tsars
log(83)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
#4.3032
#0.4808

# Kevo
log(69)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
#4.1185
#0.4808


# Utsjoki tot
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

# Teno main stem
# => EA:
log(2100)-0.5*log(0.511*0.511+1)
#7.533

# SA: 
sqrt(log(0.511*0.511+1))
#0.482


#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)


