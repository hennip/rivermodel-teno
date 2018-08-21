
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
#Lower95     Median  Upper95       Mean        SD Mode      MCerr MC%ofSD SSeff
#A[1]      25.9828   89.24145  197.955  100.04295  50.54878   NA  1.0386544     2.1  2369
#A[2]      19.0822   73.78625  164.377   83.11716  42.42440   NA  0.8678067     2.0  2390
#A[3]      19.3486   61.21320  134.092   67.76992  34.18238   NA  0.7643412     2.2  2000
#A[4]    3996.9200 5414.79000 7030.170 5471.31143 788.99345   NA 18.1718233     2.3  1885
#Atot[1]  122.4230  240.91050  395.126  250.93001  73.35104   NA  1.5344139     2.1  2285
#Atot[2]  140.9390  240.68700  401.373  252.26354  72.29295   NA  1.6451307     2.3  1931
#          AC.10      psrf
#A[1]     0.005339159 1.0032051
#A[2]    -0.030719803 1.0008106
#A[3]    -0.013301120 1.0036442
#A[4]     0.041423772 1.0005562
#Atot[1] -0.030514486 0.9997203
#Atot[2] -0.006504740 1.0035074


# => EA:
log(250)-0.5*log(0.288*0.288+1)
#5.4816

# SA: 
sqrt(log(0.288*0.288+1))
#0.2822

#A[r]<-exp(AL[r])
#AL[r]~dnorm(EA[r],Atau[r])
#Atau[r]<-1/pow(SA[r],2)


