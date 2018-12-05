# Tributary electrofishing data before 2000
###########################################

File1<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/SahkoDataLaaja sivujoet ennen v.2000.xlsx"
# Original location: G:\ex-RKTL\0_HOT\Kalavarat\Tenojoki\TENO-NÄÄTÄMÖ_SÄHKÖDATA\Sivujoet

df_old<-read_xlsx(File1, na=c("","."), sheet="SahkoDataLaaja",
              range="B1:N54456",col_types=c(rep("guess",2), "date", rep("guess",10)))

df_old<-df_old%>%select(river, place, chdate, ala, maxk, k, species, age)%>%
  mutate(year=year(chdate),month=month(chdate),day=day(chdate))%>%
  filter(species==1)


File2<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Sivujoki 2000-luku originaalidata 250315.xlsx"
# Original location: G:\ex-RKTL\0_HOT\Kalavarat\Tenojoki\TENO-NÄÄTÄMÖ_SÄHKÖDATA\Sivujoet

df_new<-read_xlsx(File2, na="", 
              range="A1:R26892",col_types=c(rep("guess",3), "date", rep("numeric",7), rep("guess",7)))

df_new<-df_new%>%select(river, place, chdate, ala, maxk, k, species, age)%>%
  mutate(year=year(chdate),month=month(chdate),day=day(chdate))%>%
  filter(species==1)

df<-full_join(df_old,df_new)


df2<-df%>%
  #mutate(river=parse_factor(river, levels=NULL))%>%
  # small streams without significant spawning -> later ignore 0+ densities on these
  mutate(IP0NA=if_else((river=="01.01.03"|
                          river=="01.01.04"| river== "01.01.05"| river=="01.01.06"| 
                          river== "01.01.07"| river=="01.01.09"|
                         river=="01.01.04.04"| river=="01.01.05.05"| river== "01.01.06.06"| 
                         river=="01.02.06"|river=="01.02.09"|river=="01.02.10"| 
                          river=="01.03.03"| river=="01.03.04"| river=="01.03.05"| 
                         river=="01.03.06"| river=="01.03.07"| river=="01.03.08"| river=="01.03.13"| 
                         river=="01.03.14"| river=="01.03.15"| river=="01.03.17"| river=="01.03.19"|
                          
                         river=="01.06.01"| river=="01.06.02"| river=="01.06.03"|river=="01.06.05"|
                         
                         river=="01.08"| river=="01.09"| 
                         river=="01.10"| river=="01.100"| river=="01.103"| river=="01.104"| river=="01.105"| 
                         river=="01.106"| river=="01.107"|river=="01.108"| river=="01.109"| river=="01.11"| 
                         river=="01.110"| river=="01.112"| river=="01.117"| 
                         river=="01.118.02.01" |  river=="01.119"| 
                         
                         river=="01.13"| river=="01.14"| river=="01.15"| river=="01.16"| 
                         river=="01.17"| river=="01.19"| river=="01.20"| river=="01.21"| 
                         river=="01.22"| river=="01.23"| river=="01.24"| river=="01.25"| 
                         river=="01.26"| river=="01.27"| river=="01.28"| river=="01.29"| 
                         river=="01.30"| river=="01.31"| river=="01.32"| river=="01.33"| 
                         river=="01.35"| river=="01.36"| river=="01.39"| 
                          
                         river=="02.01.02"| river=="02.01.03"| river=="02.01.04"| river=="02.01.05"| 
                         river=="02.02"| river=="02.03"|river=="02.04"| river=="02.05"| river=="02.06"| 
                         river=="02.07.01" | river=="02.07.02"|
                          
                         river=="03.01.03"|river=="03.01.07"|river=="03.03"|
                          
                         river=="02.09"| river=="02.10"| river=="02.100"| river=="02.101"| river=="02.102"|
                         river=="02.11"| river=="02.12"| 
                         river=="02.11.03"| river=="02.11"| river=="02.11.03"| river=="100.01"| 
                         river=="100.02"|river=="100.03"| river=="100.05"| river=="100.06"| 
                         river=="100.07"|river=="100.10"), 0, 1))%>% # dummy to select IP0's as missing
  mutate(river2=fct_recode(river,
                           "01.01.01"="01.01.01.01", # Lovttajoki -> Kalddas
                           "01.01"="01.01.02", # Luossajoki -> Pulmanki
                           "01.01"="01.01.02.01", # Skiihpa -> Pulmanki
                           "01.01"="01.01.03", 
                           "01.01"="01.01.04", # Viertsa -> Pulmanki
                           "01.01"="01.01.04.04", 
                           "01.01"="01.01.05", # Leaibe -> Pulmanki
                           "01.01"="01.01.05.05", 
                           "01.01"="01.01.06", # ?
                           "01.01"="01.01.06.06", 
                           "01.01"="01.01.07", # ?
                           "01.01"="01.01.08", # Moresvei -> Pulmanki
                           "01.01"="01.01.09",
                           
                           "01.02"="01.02.01",
                           "01.02"="01.02.02", # Vaisjoki -> Vetsijoki
                           "01.02"="01.02.06", 
                           "01.02"="01.02.09", 
                           "01.02"="01.02.10", # Parse -> Vetsijoki
                           
                           "01.03"="01.03.03", # -> Utsjoki
                           "01.03"="01.03.04", 
                           "01.03"="01.03.05", 
                           "01.03"="01.03.06",
                           "01.03"="01.03.06",
                           "01.03"="01.03.06",
                           "01.03"="01.03.07",
                           "01.03"="01.03.08",
                           "01.03"="01.03.13",
                           "01.03"="01.03.14",
                           "01.03"="01.03.15",
                           "01.03"="01.03.17",
                           "01.03"="01.03.19",
                           
                           "01.04"="01.04.01", # Pirke -> Kuoppilasjoki
                           "01"="01.05", # Nuvvus -> TMS
                           "01.06"="01.06.01", # Manna-Avdsi -> Nilijoki
                           "01.06"="01.06.02", # Kartejoki -> Nilijoki
                           "01.06"="01.06.03", 
                           "01.06"="01.06.05",
                           
                           "01"="01.08", # -> TMS
                           "01"="01.09", # Peänsjeara -> TMS
                           "01"="01.10", # -> TMS
                           "01"="01.100", 
                           "01"="01.103",
                           "01"="01.104",
                           "01"="01.105",
                           "01"="01.106",
                           "01"="01.107",
                           "01"="01.108",
                           "01"="01.109",
                           "01"="01.11","01"="01.110",
                           
                           "01.111"="01.111.01", # -> Levajoki
                           "01.111"="01.111.02","01.111"="01.111.03",
                           
                           "01"="01.112","01"="01.117","01"="01.119",
                           
                           "01.118.02"="01.118.02.01", # -> Gurtejohka
                           
                           "01.12"="01.120","01.12"="01.120.02", # -> Valjoki
                           
                           "01"="01.13","01"="01.14","01"="01.15", # ->TMS
                           "01"="01.16","01"="01.17","01"="01.19",
                           "01"="01.20","01"="01.21","01"="01.22",
                           "01"="01.23","01"="01.24","01"="01.25",
                           "01"="01.26","01"="01.27",
                           "01"="01.28", "01"="01.29", "01"="01.30",
                           "01"="01.31", "01"="01.32","01"="01.33",
                           "01"="01.35", "01"="01.36", "01"="01.39",
                           
                           "02.01"="02.01.02", # ->Karigasjoki
                           "02.01"="02.01.03",
                           "02.01"="02.01.04",
                           "02.01"="02.01.05",
                           
                           "02.07"="02.07.01", # -> Vuomajoki
                           "02.07"="02.07.02",
                           
                           "02"="02.02","02"="02.03","02"="02.04", # ->Inarijoki
                           "02"="02.05","02"="02.06",
                           
                           "02"="02.09","02"="02.10", "02"="02.100",
                           "02"="02.101","02"="02.102","02"="02.11","02"="02.11.03",
                           
                           "03.01"="03.01.03", #-> Iesjoki
                           "03.01"="03.01.07",
                           "03"="03.03", # ->Karasjoki
                           
                           "02"="02.11","02"="02.12","02"="02.11.03",
                           "01"="100",
                           "01"="100.01",
                           "01"="100.02",
                           "01"="100.03",
                           "01"="100.05",
                           "01"="100.06",
                           "01"="100.07",
                           "01"="100.10",
                           
                           "02"="200"))
  
# Datasta kaksi versiota kahteen niin että toisesta pudotetaan pois sivupurot -> tästä datasta nollikastiheydet
# Siitä versiosta jossa kaikki purot mukana lasketaan 1+ ja >=2 tiheydet

df2_IP0<-filter(df2, IP0NA==1)
df2

df3_IP0<-df2_IP0%>%filter(is.na(age)==F)%>%
  group_by(river2, year, place)%>%
  summarise(`0+`=sum(age==0),
            acre=mean(ala)/100)%>% # areas often missing
  mutate(IP0=`0+`/acre)%>%
  ungroup()%>%
  group_by(river2, year)%>%
  summarise(IP0=mean(IP0))


df3<-df2%>%filter(is.na(age)==F)%>%
  group_by(river2, year, place)%>%
  summarise(`1+`=sum(age==1),
            `1++`=sum(age>0),`2++`=sum(age>1), # age=10 means >=2+
            acre=mean(ala)/100)%>% # areas often missing
  mutate(IP1=`1+`/acre, IOP1=`1++`/acre, IP2=`2++`/acre)%>%
  ungroup()%>%
  group_by(river2, year)%>%
  summarise(IP1=mean(IP1), IOP1=mean(IOP1), IP2=mean(IP2), area=mean(acre), n=n())

df4<-full_join(df3, df3_IP0)%>%
  select(river2, year, IP0, everything())%>%
  filter(is.na(area)==F)

#View(df4)

# Name 'em
df5<-df4%>%
  ungroup()%>%
  mutate(rivername=fct_recode(river2,
                              "TenoMS"="01",
                              "Pulmanki"="01.01", "Kalddas"="01.01.01", 
                              "Vetsijoki"="01.02","Utsjoki"="01.03", 
                              "Tsars"="01.03.01", "Kevo" ="01.03.02",
                              "Kuoppilas"="01.04", "Nilijoki" ="01.06",
                              "Akujoki"="01.07","Levajoki"="01.111",
                              "Baisjoki"="01.115", "Laksejoki"="01.118",
                              "Deavkke"="01.118.01","Gurte"="01.118.02",
                              "Valjoki"="01.12",
                              "Inari"="02",
                              "Karigas"="02.01", "Vuomajoki"="02.07",
                              "Kietsima"="02.08","Gossjohka"="02.103",
                              "Karasjoki"="03",
                              "Iesjoki"="03.01","Geaime"="03.01.01",
                              "Bavtta"="03.02","Maske"="100.08",
                              "Ciiko"="100.08.01"))%>%
  filter(is.na(area)==F)%>%
  select(river2, rivername, year, everything())
# View(df5)
# yht 27 streams


df_trib<-df5%>%
  select(-area, -IOP1)


# Smolt trap data 1989-1994: Pulmanki, Tsars, Kevo (Karigas, Kuoppilas, Kalddas not included yet)
###########################################

File<-"H:/Projects/ISAMA/data/der/Teno_smolts_89-94.xlsx"

df<-read_xlsx(File, na="", range="J4:P29")%>%
  mutate(mu=round((p_min+p_mean+p_max)/3,2), # transform from triangular into lognormal
         var= round((p_min^2+p_mean^2+p_max^2-p_min*p_max-p_min*p_mean-p_mean*p_max)/18,4),
         cv=round(sqrt(var)/mu,2))

# JAGS model for trap data (total N = N observed / expert evaluated prop observed)
# 
# M_traps<-"model{
# 
# for(i in 1:25){
#   smoltEst[i]<-n[i]/p[i]
#   tau[i]<-1/log(cv[i]*cv[i]+1)
#   p[i]~dlnorm(log(mu[i])-0.5/tau[i], tau[i])
# }
# 
# }"
# 
# data<-list(n=df$n, mu=df$mu, cv=df$cv)
# 
# var_names<-c("smoltEst", "p")
# 
# run <- run.jags(M_traps,
#                  data=data,monitor=var_names, inits=NA,
#                  n.chains = 2, method = 'parallel', thin=1, burnin =100,
#                  modules = "mix",keep.jags.files=T,sample =10000, adapt = 1000,
#                  progress.bar=TRUE)
# 
# #plot(run)
# 
# traps<-summary(run)
# save(traps, file="01-Data/summary_traps.RData")

load("01-Data/summary_traps.RData")

tmp<-as.tibble(traps[1:25,4:5])%>%
  mutate(year=c(1989:1994, 1989:1994, 1989:1991,1993:1994,1990,1990:1994,1991:1992),
                 river=c(rep("Pulmanki", 6), rep("Tsars",6), rep("Kevo",5),
                         "Kalddas",rep("Karigas", 5),rep("Kuoppilas", 2)))

smolts_trib<-full_join(df,tmp)%>%
  select(year, river, n, Mean, SD)%>%
  mutate(CIS=round(SD/Mean,2),
    IS=log(Mean/1000*(CIS*CIS+1)))%>%
  mutate(rivername=parse_factor(river, levels=NULL))%>%
  select(year, rivername, IS, CIS)%>%
  mutate(river2=ifelse(rivername=="Pulmanki", "01.01", 
                      ifelse(rivername=="Tsars", "01.03.01", 
                             ifelse(rivername=="Kevo", "01.03.02", 
                                    ifelse(rivername=="Kalddas", "01.01.01",
                                           ifelse(rivername=="Karigas", "02.01",
                                                  ifelse(rivername=="Kuoppilas", "01.04",NA)))))))

df_trib<-full_join(df_trib,smolts_trib)
#View(tmpX)

#View(df_trib%>%arrange(rivername))
