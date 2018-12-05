
df<-full_join(df_MS,dfU, by=NULL)%>%
  full_join(df_Inari, by=NULL)%>%
  full_join(df_trib, by=NULL)%>%
  mutate(n=ifelse(n==0, 1,n))%>%
  filter(year>=1980)%>%
  mutate(rcode=ifelse(rivername=="TenoMS", "01", 
                      ifelse(rivername=="Inari","02", 
                             ifelse(rivername=="Utsjoki", "01.03",as.character(river2)))))

smolts<-df%>%
  select(rivername, rcode, year, CIS, IS)%>%
  filter(is.na(IS)==F)

dfT<-df%>%
  group_by(rcode, rivername, year)%>%
  summarise(IP0=mean(IP0, na.rm=T),IP1=mean(IP1, na.rm=T),IP2=mean(IP2, na.rm=T), n=sum(n))%>%
  ungroup()%>%
  mutate(IP0=ifelse(is.na(IP0)==T,NA_real_,IP0))%>%
  mutate(IP1=ifelse(is.na(IP1)==T,NA_real_,IP1))%>%
  mutate(IP2=ifelse(is.na(IP2)==T,NA_real_,IP2))


dfT2<-full_join(dfT, smolts)%>%
  mutate(n=ifelse(is.na(n)==T,1,n))
  

df<-dfT2%>%
  select(rcode,rivername, year, IS, CIS, n, IP0, IP1, IP2)%>%
  mutate(stock=as.factor(rivername))%>%
  mutate(stock=fct_recode(stock, "1"="TenoMS","2"="Pulmanki", 
                          "3"="Kalddas","4"="Vetsijoki", 
                          "5"="Utsjoki","6"="Tsars", "7"="Kevo", 
                          "8"="Kuoppilas","9"="Nilijoki", "10"="Akujoki", 
                          
                          "11"="Levajoki","12"="Baisjoki", "13"="Laksejoki", 
                          "14"="Deavkke","15"="Gurte", "16"="Valjoki", 
                          "17"="Inari","18"="Karigas", "19"="Vuomajoki", 
                          
                          "20"="Kietsima","21"="Gossjohka", "22"="Karasjoki", 
                          "23"="Iesjoki","24"="Geaime", "25"="Bavtta", 
                          "26"="Maske","27"="Ciiko"))%>%
  mutate(stock=as.numeric(as.character(stock)))




#df<-df%>%ungroup

n<-as.matrix(df%>%select(n, stock, year)%>%
  spread(key=stock, value=n)%>%
    select(-year)%>%
    replace(is.na(.),1))

CIS<-as.matrix(df%>%select(CIS, stock, year)%>%
  spread(key=stock, value=CIS)%>%
    select(`2`,`3`,`5`,`6`,`7`,`8`,`18`,-year)%>%
    replace(is.na(.),0.2))

IS<-as.matrix(df%>%select(IS, stock, year)%>%
  spread(key=stock, value=IS)%>%
    select(`2`,`3`,`5`,`6`,`7`,`8`,`18`,-year))
    
IP1<-round(as.matrix(df%>%select(IP1, stock, year)%>%
  spread(key=stock, value=IP1)%>%
    select(-year)))

IP2<-round(as.matrix(df%>%select(IP2, stock, year)%>%
  spread(key=stock, value=IP2)%>%
    select(-year)))

IP0<-round(as.matrix(df%>%select(IP0, stock, year)%>%
  spread(key=stock, value=IP0)%>%
    select(-year)))

#IOP1<-round(as.matrix(df%>%select(IOP1, stock, year)%>%
#  spread(key=stock, value=IOP1)%>%
#    select(-year)))

