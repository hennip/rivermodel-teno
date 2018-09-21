
File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Teno-Naatamo_vakioalueiden keski- ja koealuekohtaiset tiheyskuvat_CNT1 ver 13-12-2017.xlsx"
Sheet<-"Teno paauoma vuosittain, CNT1"

Range<-c("C6:F44","G6:J44","K6:N44","O6:R44","S6:V44",
         "W6:Z44","AA6:AD44","AE6:AH44","AI6:AL44","AM6:AP44",
         "AQ6:AT44","AU6:AX44","AY6:BB44","BC6:BF44","BG6:BJ44",
         "BK6:BN44","BO6:BR44","BS6:BV44","BW6:BZ44","CA6:CD44",
         "CE6:CH44","CI6:CL44","CM6:CP44","CQ6:CT44","CU6:CX44",
         "CY6:DB44","DC6:DF44","DG6:DJ44","DK6:DN44","DO6:DR44",
         "DS6:DV44","DW6:DZ44")

dfX<-list()
for(i in 1:32){
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
  full_join(dfX[[10]], by=NULL)%>%
  full_join(dfX[[11]], by=NULL)%>%
  full_join(dfX[[12]], by=NULL)%>%
  full_join(dfX[[13]], by=NULL)%>%
  full_join(dfX[[14]], by=NULL)%>%
  full_join(dfX[[15]], by=NULL)%>%
  full_join(dfX[[16]], by=NULL)%>%
  full_join(dfX[[17]], by=NULL)%>%
  full_join(dfX[[18]], by=NULL)%>%
  full_join(dfX[[19]], by=NULL)%>%
  full_join(dfX[[20]], by=NULL)%>%
  full_join(dfX[[21]], by=NULL)%>%
  full_join(dfX[[22]], by=NULL)%>%  
  full_join(dfX[[23]], by=NULL)%>%
  full_join(dfX[[24]], by=NULL)%>%
  full_join(dfX[[25]], by=NULL)%>%
  full_join(dfX[[26]], by=NULL)%>%
  full_join(dfX[[27]], by=NULL)%>%
  full_join(dfX[[28]], by=NULL)%>%
  full_join(dfX[[29]], by=NULL)%>%
  full_join(dfX[[30]], by=NULL)%>%
  full_join(dfX[[31]], by=NULL)%>%
  full_join(dfX[[32]], by=NULL)

dens<-dfX2%>%group_by(year, age)%>%
  summarize(ave_density=mean(density, na.rm=T), n=sum(!is.na(density)))%>%
  mutate(ave_density=ifelse(ave_density=="NaN", NA, ave_density))%>%
  mutate(n_juveniles= ave_density*n*5) # total number of juveniles across all study areas

dens2<-select(dens, -n_juveniles)%>%
  spread(key=age, value=ave_density)
#View(dens2)

df_MS<-select(dens, -ave_density)%>%
  spread(key=age, value=n_juveniles)%>%
  mutate(stock=1)%>%
  select(-IOP1) # leave IOP1 out so the same data is not added twice


