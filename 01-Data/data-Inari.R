
File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Teno-Naatamo_vakioalueiden keski- ja koealuekohtaiset tiheydet_CNT1 ver 04-06-2015.xlsx"
Sheet<-"Inarijoki vuosittain, CNT1"

Range<-c("C6:F41","G6:J41","K6:N41","O6:R41","S6:V41",
         "W6:Z41","AA6:AD41","AE6:AH41","AI6:AL41","AM6:AP41")

dfX<-list()
for(i in 1:10){
  dfX[[i]]<-read_xlsx(File, sheet=Sheet, na="", range=Range[i],col_names=c("IP0","IP1","IOP1","IP2"))%>% 
    mutate(year=c(1979:2014),area=i)%>%
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
  mutate(stock=15)%>%
  select(-IOP1) # leave IOP1 out so the same data is not added twice


