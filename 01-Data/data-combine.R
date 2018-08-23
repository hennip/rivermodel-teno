

dfU<-filter(dfU, year>=1980)
df_Inari<-filter(df_Inari, year>=1980)

df<-full_join(dfB,dfU, by=NULL)%>%
  full_join(df_Inari, by=NULL)%>%
  mutate(n=ifelse(n==0, 1,n))

#df<-dfB
#View(df)

n<-as.matrix(df%>%select(n, stock, year)%>%
  spread(key=stock, value=n)%>%
    mutate(`14`=ifelse(is.na(`14`)==T, 1,`14`))%>%
    mutate(`15`=ifelse(is.na(`15`)==T, 1,`15`))%>%
  select(-year))

CIS<-as.matrix(df%>%select(CIS, stock, year)%>%
  spread(key=stock, value=CIS)%>%
    mutate(`14`=ifelse(is.na(`14`)==T, 0.2,`14`))%>%
    mutate(`15`=ifelse(is.na(`15`)==T, 0.2,`15`))%>%
    select(-year))

IS<-as.matrix(df%>%select(IS, stock, year)%>%
  spread(key=stock, value=IS)%>%
  select(-year))

IP1<-round(as.matrix(df%>%select(IP1, stock, year)%>%
  spread(key=stock, value=IP1)%>%
  select(-year)))

IP2<-round(as.matrix(df%>%select(IP2, stock, year)%>%
  spread(key=stock, value=IP2)%>%
  select(-year)))

IP0<-round(as.matrix(df%>%select(IP0, stock, year)%>%
  spread(key=stock, value=IP0)%>%
  select(-year)))

IOP1<-round(as.matrix(df%>%select(IOP1, stock, year)%>%
  spread(key=stock, value=IOP1)%>%
  select(-year)))


