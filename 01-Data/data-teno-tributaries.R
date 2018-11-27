# Tributary electrofishing data before 2000
###########################################

File1<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/SahkoDataLaaja sivujoet ennen v.2000.xlsx"
# Original location: G:\ex-RKTL\0_HOT\Kalavarat\Tenojoki\TENO-NÄÄTÄMÖ_SÄHKÖDATA\Sivujoet

df<-read_xlsx(File1, na=c("","."), sheet="SahkoDataLaaja",
              range="B1:N54456",col_types=c(rep("guess",2), "date", rep("guess",10)))

df<-df%>%select(river, place, chdate, ala, maxk, k, species, age)%>%
  mutate(year=year(chdate),month=month(chdate),day=day(chdate))%>%
  filter(species==1)

#View(filter(df, river=="01.01", year==1985))
#filter(df, is.na(ala)==T)

df2<-df%>%filter(is.na(age)==F)%>%
  group_by(river, year, place)%>%
  summarise(`0+`=sum(age==0),`1+`=sum(age==1),
            `1++`=sum(age>0),`2++`=sum(age>1), # age=10 means >=2+
            acre=mean(ala)/100)%>% # areas often missing
  mutate(IP0=`0+`/acre, IP1=`1+`/acre,
         IOP1=`1++`/acre, IP2=`2++`/acre)

df2<-df2%>%select(river,year, place, acre, IP0, IP1, IP2, IOP1)%>%
  group_by(river,year)%>%
  summarise(IP0=mean(IP0), IP1=mean(IP1), IOP1=mean(IOP1), IP2=mean(IP2), area=mean(acre), n=n())%>%
  filter(n>1)

#tmp<-filter(df2, is.na(area)==T, n>3)
#View(tmp)

df3<-df2%>%filter(river=="01.01" |river=="01.02" | river=="01.03.01" | river=="01.03.02" | 
                    river=="01.04"|river=="01.06"|river=="01.07")
#df3<-df2

# Name 'em
df4<-df3%>%
  ungroup()%>%
  mutate(river=parse_factor(river, levels=NULL))%>%
  mutate(rivername=fct_recode(river,
                              "Pulmanki"="01.01", "Vetsijoki" ="01.02",
                              "Tsars"="01.03.01", "Kevo" ="01.03.02",
                              "Kuoppilas"="01.04", "Nilijoki" ="01.06",
                              "Akujoki"="01.07"))%>%
  filter(is.na(area)==F)

df_trib_old<-df4%>%select(-IOP1)%>%
  select(-river)


# Tributary electrofishing data from 2000 and onwards
###########################################

File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Sivujoki 2000-luku originaalidata 250315.xlsx"
# Original location: G:\ex-RKTL\0_HOT\Kalavarat\Tenojoki\TENO-NÄÄTÄMÖ_SÄHKÖDATA\Sivujoet

df<-read_xlsx(File, na="", 
              range="A1:R26892",col_types=c(rep("guess",3), "date", rep("numeric",7), rep("guess",7)))

df<-df%>%select(river, place, chdate, ala, maxk, k, species, age)%>%
  mutate(year=year(chdate),month=month(chdate),day=day(chdate))%>%
  filter(species==1)

df2<-df%>%group_by(river, year, place)%>%
  summarise(`0+`=sum(age==0),`1+`=sum(age==1),
            `1++`=sum(age>0),`2++`=sum(age>1), # age=10 means >=2+
            acre=mean(ala)/100)%>%
  mutate(IP0=`0+`/acre, IP1=`1+`/acre,
         IOP1=`1++`/acre, IP2=`2++`/acre)

df2<-df2%>%select(river,year, place, acre, IP0, IP1, IP2, IOP1)%>%
  group_by(river,year)%>%
  summarise(IP0=mean(IP0), IP1=mean(IP1), IOP1=mean(IOP1), IP2=mean(IP2), area=mean(acre), n=n())%>%
  filter(n>1)

#View(df2)

# Pulmanki, Vetsijoki, Tsars, Kevo, 
# Kuoppilasjoki, Nilijoki, Akujoki#, Karasjoki, Iesjoki
df3<-df2%>%filter(river=="01.01" |river=="01.02" | river=="01.03.01" | river=="01.03.02" | 
                    river=="01.04"|river=="01.06"|river=="01.07")#| river=="03" | river=="03.01")
#df3<-df2
# Name 'em
df4<-df3%>%#filter(river=="01.01" | river=="01.03.01" | river=="01.03.02")%>%
  ungroup()%>%
  mutate(river=parse_factor(river, levels=NULL))%>%
  mutate(rivername=fct_recode(river,
                              "Pulmanki"="01.01", "Vetsijoki" ="01.02",
                              "Tsars"="01.03.01", "Kevo" ="01.03.02",
                              "Kuoppilas"="01.04", "Nilijoki" ="01.06",
                              "Akujoki"="01.07"))
  

df_trib_new<-df4%>%select(-IOP1)%>%
  select(-river)


# Smolt trap data 1989-1994: Pulmanki, Tsars, Kevo (Karigas, Kuoppilas, Kalddas not included yet)
###########################################

File<-"H:/Projects/ISAMA/data/der/Teno_smolts_89-94.xlsx"

df<-read_xlsx(File, na="", range="J4:P21")%>%
  mutate(mu=round((p_min+p_mean+p_max)/3,2), # transform from triangular into lognormal
         var= round((p_min^2+p_mean^2+p_max^2-p_min*p_max-p_min*p_mean-p_mean*p_max)/18,4),
         cv=round(sqrt(var)/mu,2))

# JAGS model for trap data (total N = N observed / expert evaluated prop observed)

# M_traps<-"model{
# 
# for(i in 1:17){
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

tmp<-as.tibble(traps[1:17,4:5])%>%
  mutate(year=c(1989:1994, 1989:1994, 1989:1991,1993,1994),
                 river=c(rep("Pulmanki", 6), rep("Tsars",6), rep("Kevo",5)))

tmp2<-full_join(df,tmp)%>%
  select(year, river, n, Mean, SD)%>%
  mutate(CIS=round(SD/Mean,2),
    IS=log(Mean/1000*(CIS*CIS+1)))%>%
  mutate(rivername=parse_factor(river, levels=NULL))%>%
  select(year, rivername, IS, CIS)

df_trib<-full_join(df_trib_new,df_trib_old)%>%
  full_join(tmp2)
#View(df_trib)

#View(df_trib%>%arrange(rivername))
