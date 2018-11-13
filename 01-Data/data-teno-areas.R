
# Estimates for production areas. Method acknowledges differences in the habitat quality.


df<-read_xlsx(str_c(pathIn, "orig/Kutukantatavoitteet popmalliin JE&PO.xlsx"), sheet=1, range=c("B4:S27"),na=c("", "?"),
          col_names = c("river", "L0", "L1", "tmp", "L4", "empty","L1.8", "L2", "L2.2", "empty2",
                        "min0","max0","min1", "max1", "min2", "max2", "min4", "max4"))%>%
  select(-tmp, -empty, -empty2)

div<-2 # scaling factor, habitat type 2 as level of reference (Htype0 = *0.1, Htype1 = *0.5, Htype4=*2)
df0<-df%>%mutate( A=100*ifelse(is.na(L0)==F, L0, NA),
                    meanH=ifelse(is.na(L0)==F, 0.1/div, NA),
                    minH=ifelse(is.na(L0)==F, min0/div, NA),
                    maxH=ifelse(is.na(L0)==F, max0/div, NA),
                    Htype=0,
                   mean=A*meanH, min=A*minH, max=A*maxH)%>%
  select(river,Htype, A, mean, min, max)

df1<-df%>%mutate( A=100*ifelse(is.na(L1)==F, L1, NA),
                   meanH=ifelse(is.na(L1)==F, 1/div, NA),
                   minH=ifelse(is.na(L1)==F, min1/div, NA),
                   maxH=ifelse(is.na(L1)==F, max1/div, NA),
                   Htype=1,
                   mean=A*meanH, min=A*minH, max=A*maxH)%>%
  select(river, Htype, A, mean, min, max)


df2<-df%>%mutate( A=100*ifelse(is.na(L1.8)==F, L1.8, 
                            ifelse(is.na(L2)==F,L2, 
                                   ifelse(is.na(L2.2)==F,L2.2,NA))),
                   meanH=ifelse(is.na(L1.8)==F, 1.8/div, 
                                 ifelse(is.na(L2)==F,2/div, 
                                        ifelse(is.na(L2.2)==F,2.2/div,NA))),
                   minH=ifelse(is.na(L1.8)==F | is.na(L2)==F| is.na(L2.2)==F, min2/div, NA),
                   maxH=ifelse(is.na(L1.8)==F | is.na(L2)==F| is.na(L2.2)==F, max2/div, NA),
                   Htype=2,
                   mean=A*meanH, min=A*minH, max=A*maxH)%>%
  select(river, Htype, A, mean, min, max)

df4<-df%>%mutate( A=100*ifelse(is.na(L4)==F, L4, NA),
                   meanH=ifelse(is.na(L4)==F, 4/div, NA),
                   minH=ifelse(is.na(L4)==F, min4/div, NA),
                   maxH=ifelse(is.na(L4)==F, max4/div, NA),
                   Htype=4,
                   mean=A*meanH, min=A*minH, max=A*maxH)%>%
  select(river, Htype, A, mean, min, max)

df<-full_join(df0, df1)%>%
  full_join(df2)%>%
  full_join(df4)%>%
  filter(is.na(A)==F)%>%
  mutate(river=parse_factor(river, levels=NULL))%>%
  mutate(mu=(mean+min+max)/3,
         s2= (mean^2+min^2+max^2-mean*min-mean*max-min*max)/18,
         cv=sqrt(s2)/mu)%>%
  arrange(river)%>%
  mutate(stock=fct_recode(river, 
                            "1"="Maskejoki",  "2"="Pulmanki",
                          "3"="Valjohka",     "4"="Karas ala",
                          "5"="Geaimme",      "6"="Inarijoki",
                          "7"="Kietsimä",     "8"="Laksjohka",
                          "9"="Vetsijoki",    "10"="Utsjoki",
                          "11"="Kevojoki",    "12"="Kuoppilasjoki",
                          "13"="Levajoki",    "14"="Nilijoki",
                          "15"="Iesjoki",     "16"="Iskorasjohka",
                          "17"="Teno pääuoma","18"="Tsarsjoki",
                          "19"="Baisjoki",    "20"="Akujoki",
                          "21"="Karas ylä",   "22"="Bavtta",
                          "23"="Karigasjoki", "24"="Gossjohka"
  ))%>%
  filter(is.na(mu)==F) # Removes Inarijoki 0 for now!

dat<-df%>%select(stock,mu, cv)

M_areas<-"
model{
for( i in 1:N){
  T[i]<-1/log(cv[i]*cv[i]+1)
  A[i]~dlnorm(log(mu[i])-0.5/T[i],T[i])
}

Atot[1]<-sum(A[1:4])
Atot[2]<-sum(A[5:6])
Atot[3]<-sum(A[7:9])
Atot[4]<-sum(A[10:11]) # Karasjoki ala only
Atot[5]<-sum(A[12:13])
Atot[6]<-sum(A[14])
Atot[7]<-sum(A[15])
Atot[8]<-sum(A[16:17])
Atot[9]<-sum(A[18:19])
Atot[10]<-sum(A[20:22])
Atot[11]<-sum(A[23:24])
Atot[12]<-sum(A[25:26])
Atot[13]<-A[27]
Atot[14]<-sum(A[28:29])
Atot[15]<-sum(A[30:31])
Atot[16]<-A[32]
Atot[17]<-A[33]
Atot[18]<-A[34]
Atot[19]<-A[35]
Atot[20]<-A[36]
Atot[21]<-Atot[4]+A[37] # Karasjoki ala + yla
Atot[22]<-A[38]
Atot[23]<-A[39]
Atot[24]<-A[40]

}"

data<-list(
  #stock=dat$stock,
  N=dim(dat)[1],
  mu=dat$mu, cv=dat$cv
)


var_names<-c("A", "Atot")

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

# sum_run<-summary(runX)[1:40,4:5] # mean,SD
# 
# df2<-df%>%
#   mutate(mu=round(mu,2),sd=round(sqrt(s2),2))%>%
#   select(river, Htype, A, mu, sd)
# 
# tmp<-cbind(df2, round(sum_run,2))
# (tmp$mu-tmp$Mean)/tmp$mu
# (tmp$sd-tmp$SD)/tmp$sd

sum_run<-summary(runX)[41:(40+24),4:5] # mean,SD

tmp1<-df%>%select(river, stock)

tmp2<-as.tibble(sum_run)%>%
  mutate(stock=parse_factor(1:24, levels=NULL))



areas<-right_join(tmp1, tmp2)%>%
  distinct()%>%
  mutate(cv=SD/Mean,
         SA=sqrt(log(cv^2+1)),
         EA=log(Mean)-0.5*SA^2)%>%
  filter(river=="Teno pääuoma"| river=="Pulmanki"| river=="Vetsijoki" | river=="Utsjoki"| 
           river=="Tsarsjoki"| river=="Kevojoki"|
           river=="Kuoppilasjoki"| river=="Nilijoki"|
           river=="Akujoki"|
           river=="Inarijoki")%>%
  select(river, EA, SA)

#View(areas)         

# 
# 
# 7.571789, #1: TMS
# 4.384182, # 2: Pulmanki
# 4.849535, # 3: Vetsijoki
# 4.832325, # 4: Utsjoki main
# 4.314214, # 5: Tsars
# 4.442797, # 6: Kevo
# 3.447606, # 7: Kuoppilasjoki
# 3.276931, # 8: Nilijoki
# 2.374227, # 9: Akujoki
# 6.329898 # 10: Inari
# 
# 0.2113711, #1: TMS
# 0.1389517, # 2: Pulmanki
# 0.1241878, # 3: Vetsijoki
# 0.07614008, # 4: Utsjoki main
# 0.14195152, # 5: Tsars
# 0.11507365, # 6: Kevo
# 0.15462176, # 7: Kuoppilasjoki
# 0.12147614, # 8: Nilijoki
# 0.14841259, # 9: Akujoki
# 0.21966877 # 10: Inari
# 
# 
# 







# 
# 
# 
# 
# 
# # point estimates for the size of prod areas (in hectares) based on NINA report
# mu_TMS<-2052.5 # Teno main stem
# 
# mu_Pul<-55.7 # Pulmanki
# 
# mu_U<-98
# mu_Tsars<-83
# mu_Kevo<-69
# 
# mu_Inari<-564.2
# mu_Karigas<-29.9
# mu_Iskoras<-21.3
# mu_Goss<-260.3
# mu_Kietsi<-19.9
# mu_Inari+mu_Karigas+mu_Iskoras+mu_Goss+mu_Kietsi
# #895.6
# 
# cv<-0.5
# Tau<-1/log(cv*cv+1)
# 
# M_TMS<-log(mu_TMS)-0.5/Tau
# M_Pul<-log(mu_Pul)-0.5/Tau
# M_U<-log(mu_U)-0.5/Tau
# M_Tsars<-log(mu_Tsars)-0.5/Tau
# M_Kevo<-log(mu_Kevo)-0.5/Tau
# M_Inari<-log(mu_Inari)-0.5/Tau
# M_Karigas<-log(mu_Karigas)-0.5/Tau
# M_Iskoras<-log(mu_Iskoras)-0.5/Tau
# M_Goss<-log(mu_Goss)-0.5/Tau
# M_Kietsi<-log(mu_Kietsi)-0.5/Tau
# 
# M_TMS;M_U;M_Tsars;M_Kevo;Tau
# M_Inari;M_Karigas;M_Iskoras;M_Goss;M_Kietsi
# 
# M_areas<-"
# model{
# 
# #Teno main stem
# A_TMS[1]~dlnorm(M_TMS,Tau)
# 
# #Pulmanki
# A_Pul~dlnorm(M_Pul,Tau) 
# 
# # Utsjoki main stem and tributaries
# A_U[1]~dlnorm(M_U,Tau) # Utsjoki main stem
# A_U[2]~dlnorm(M_Tsars,Tau) # Tsars
# A_U[3]~dlnorm(M_Kevo,Tau) # Kevo
# #A[3]~dlnorm(M_Kevo,Tau) # Kevo
# 
# # Inarijoki main stem and tributaries
# A_I[1]~dlnorm(M_Inari,Tau) # Inarijoki main stem
# A_I[2]~dlnorm(M_Karigas,Tau) # Karigasjoki
# A_I[3]~dlnorm(M_Iskoras,Tau) # Iskorasjohka
# A_I[4]~dlnorm(M_Goss,Tau) # Gossjohka
# A_I[5]~dlnorm(M_Kietsi,Tau) # Kietsimajoki
# 
# Atot_Utsjoki[1]<-sum(A_U[])
# Atot_Inari[1]<-sum(A_I[])
# 
# # check: Torne:
# 
# A_Torne<-exp(AL)
# AL~dnorm(8.595,Atau)
# Atau<-1/pow(0.1417,2)
# 
# # check:Utsjoki total
# 
# Atot_Utsjoki[2]<-exp(AL_U)
# AL_U~dnorm(5.4816,1/pow(0.2823,2))
# 
# # check: Inarijoki
# 
# Atot_Inari[2]<-exp(AL_I)
# AL_I~dnorm(6.7516,1/pow(0.3518,2))
# 
# # check: Teno main stem
# A_TMS[2]<-exp(AL_TMS)
# AL_TMS~dnorm(7.5337,1/pow(0.4812,2))
# 
# 
# }"
# 
# modelName<-"areas"
# 
# Mname<-str_c("03-Model/",modelName, ".txt")
# cat(M_areas,file=Mname)
# 
# data<-list(
# M_TMS=M_TMS,
# M_Pul=M_Pul,
# M_U=M_U, M_Tsars=M_Tsars, M_Kevo=M_Kevo, Tau=Tau,
# M_Inari=M_Inari,
# M_Kietsi=M_Kietsi,
# M_Goss=M_Goss,
# M_Iskoras=M_Iskoras,
# M_Karigas=M_Karigas
# 
# )
# 
# 
# 
# var_names<-c("A_TMS","A_Pul","Atot_Utsjoki", "Atot_Inari", "A_Torne", "A_U")
# 
# #nb of samples = samples * thin, burnin doesn't take into account thin
# # sample on tässä lopullinen sample, toisin kuin rjagsissa!!!
# 
# t1<-Sys.time();t1
# runX <- run.jags(M_areas, 
#                  data=data,monitor=var_names, inits=NA,
#                  n.chains = 2, method = 'parallel', thin=1, burnin =0, 
#                  modules = "mix",keep.jags.files=T,sample =1000, adapt = 1000, 
#                  progress.bar=TRUE)
# t2<-Sys.time()
# difftime(t2,t1)
# 
# summary(runX)
# 
# #                 Lower95    Median  Upper95      Mean         SD Mode     MCerr MC%ofSD SSeff        AC.10      psrf
# # A_TMS            451.997 1867.3800 4114.570 2100.7681 1074.35889   NA 24.023395     2.2  2000  0.001280858 1.0014864
# # Atot_Utsjoki[1]  124.850  240.1980  396.742  252.6231   74.22807   NA  1.582502     2.1  2200 -0.024994764 1.0013226
# # Atot_Utsjoki[2]  135.478  242.0925  406.181  251.8768   70.85560   NA  1.627823     2.3  1895  0.008611197 1.0002530
# # Atot_Inari[1]    409.652  834.4235 1477.740  888.7370  305.13818   NA  7.316300     2.4  1739 -0.028386006 0.9995101
# # Atot_Inari[2]    379.159  855.5125 1516.370  903.8401  323.88509   NA  7.068289     2.2  2100  0.030278963 0.9995434
# # A_Torne         4027.890 5420.6500 6972.940 5474.4015  774.25011   NA 17.312759     2.2  2000 -0.004964185 0.9997133
# 
# 
# # Pulmanki
# # EA; SA
# log(55.7)-0.5*log(0.4946^2+1); sqrt(log(0.4946^2+1))
# #3.9106 
# #0.4678
# 
# 
# 
# # Utsjoki main
# # EA; SA
# log(98)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
# #4.4694 
# #0.4808
# 
# # Tsars
# log(83)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
# #4.3032
# #0.4808
# 
# # Kevo
# log(69)-0.5*log(0.51^2+1); sqrt(log(0.51^2+1))
# #4.1185
# #0.4808
# 
# 
# # Utsjoki tot
# # => EA:
# log(250)-0.5*log(0.288*0.288+1)
# #5.4816
# # SA: 
# sqrt(log(0.288*0.288+1))
# #0.2822
# 
# #A[r]<-exp(AL[r])
# #AL[r]~dnorm(EA[r],Atau[r])
# #Atau[r]<-1/pow(SA[r],2)
# 
# # Inari main stem
# # => EA:
# log(564)-0.5*log(0.484*0.484+1)
# #6.230
# 
# # SA: 
# sqrt(log(0.484*0.484+1))
# #0.459
# 
# # Inari tot
# # => EA:
# log(910)-0.5*log(0.363*0.363+1)
# #6.752
# 
# # SA: 
# sqrt(log(0.363*0.363+1))
# #0.352
# 
# # Teno main stem
# # => EA:
# log(2100)-0.5*log(0.511*0.511+1)
# #7.533
# 
# # SA: 
# sqrt(log(0.511*0.511+1))
# #0.482
# 
# 
# #A[r]<-exp(AL[r])
# #AL[r]~dnorm(EA[r],Atau[r])
# #Atau[r]<-1/pow(SA[r],2)
# 
# 
