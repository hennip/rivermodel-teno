
dfT<-full_join(df_MS,dfU, by=NULL)%>%
  full_join(df_Inari, by=NULL)%>%
  full_join(df_trib, by=NULL)%>%
  mutate(n=ifelse(n==0, 1,n))%>%
  filter(year>=1980)

filter(dfT, is.na(rivername==T))

RiverNames=c("TenoMS", "Pulmanki", "Vetsijoki", "Utsjoki", "Tsars", "Kevo",
             "Kuoppilas", "Nilijoki", "Akujoki", "Inari")#, "Torne")

# If Tornionjoki needs to be included
# source("01-Data/data-baltic-north.r")
#dfB2<-filter(dfB, stock==1) # Only Tornionjoki
#dfB2<-dfB2%>%
  #mutate(stock=7)
#  mutate(rivername="Torne")
df<-dfT%>%#full_join(dfT,dfB2, by=NULL)%>%
  select(year, rivername, IS, CIS, n, IP0, IP1, IP2)%>%
  mutate(stock=parse_factor(rivername, levels=RiverNames))%>%
  mutate(stock=fct_recode(stock, "1"="TenoMS","2"="Pulmanki", "3"="Vetsijoki", 
                          "4"="Utsjoki","5"="Tsars", "6"="Kevo", 
                          "7"="Kuoppilas","8"="Nilijoki", "9"="Akujoki", 
                          "10"="Inari"))#, "7"="Torne"))

#View(df_MS)


df<-df%>%ungroup

n<-as.matrix(df%>%select(n, stock, year)%>%
  spread(key=stock, value=n)%>%
    select(-year)%>%
    replace(is.na(.),1))

CIS<-as.matrix(df%>%select(CIS, stock, year)%>%
  spread(key=stock, value=CIS)%>%
    select(`2`,`4`,`5`,`6`,-year)%>%
    replace(is.na(.),0.2))

IS<-as.matrix(df%>%select(IS, stock, year)%>%
  spread(key=stock, value=IS)%>%
    select(`2`,`4`,`5`,`6`,-year))
    
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

