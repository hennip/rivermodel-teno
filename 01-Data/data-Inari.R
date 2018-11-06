
File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Teno-Naatamo_vakioalueiden keski- ja koealuekohtaiset tiheyskuvat_CNT1 ver 13-12-2017.xlsx"
Sheet<-"Inarijoki vuosittain, CNT1"

Range<-c("C6:F44","G6:J44","K6:N44","O6:R44","S6:V44",
         "W6:Z44","AA6:AD44","AE6:AH44","AI6:AL44","AM6:AP44")

dfX<-list()
for(i in 1:10){
  dfX[[i]]<-read_xlsx(File, sheet=Sheet, na=c("", "-"), range=Range[i],col_names=c("IP0","IP1","IOP1","IP2"))%>% 
    mutate(year=c(1979:2017),area=i)%>%
    gather("IP0","IP1","IOP1","IP2", key="age", value="density")
  #gather(`0+`,`1+`,`1++`,`2++`, key="age", value="density")
}


dfX2<-full_join(dfX[[1]], dfX[[2]], by=NULL)%>%
  full_join(dfX[[3]], by=NULL)%>%
  full_join(dfX[[4]], by=NULL)%>%
  full_join(dfX[[5]], by=NULL)%>%
  full_join(dfX[[6]], by=NULL)%>%
  full_join(dfX[[7]], by=NULL)%>%
  full_join(dfX[[8]], by=NULL)%>%
  full_join(dfX[[9]], by=NULL)%>%
  full_join(dfX[[10]], by=NULL)

dens<-dfX2%>%group_by(year, age)%>%
  summarize(ave_density=mean(density, na.rm=T), n=sum(!is.na(density)))%>%
  mutate(ave_density=ifelse(ave_density=="NaN", NA, ave_density))%>%
  mutate(n_juveniles= ave_density*n*5) # total number of juveniles across all study areas

dens2<-select(dens, -n_juveniles)%>%
  spread(key=age, value=ave_density)
#View(dens2)

df_Inari<-select(dens, -ave_density)%>%
  spread(key=age, value=n_juveniles)%>%
  #mutate(stock=6)%>%
  mutate(rivername="Inari")%>%
  select(-IOP1) # leave IOP1 out so the same data is not added twice


