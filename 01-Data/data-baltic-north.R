
# Years 1980-2020
# IP2: 2+ & older
# IP1: 1+
# IOP1: 1+ & older
# IP0: 0+

File<-"O:/Jokimallipaketti/WGDATA_river model input -2017.xlsx"
Sheet<-"Calculation template"
Range<-c("B52:H92","I52:O92","P52:V92","W52:AC92","AD52:AJ92",
         "AK52:AQ92","AR52:AX92","AY52:BE92","BF52:BL92","BM52:BS92",
         "BT52:BZ92","CA52:CG92","CH52:CN92")
 
dfB<-list()
for(i in 1:13){
 # i<-2
  dfB[[i]]<-read_xlsx(File, sheet=Sheet,na="NA", range=Range[i], 
                      col_names = c("IS", "CIS", "IP2", "IP1", "IOP1", "IP0", "n"),
                      col_types=rep("numeric",7))%>%
    mutate(stock=i)%>%
    mutate(year=1980:2020)
}
dfB[[2]]

full_join(dfB[[1]], dfB[[2]], by=NULL)

dfX<-full_join(dfB[[1]], dfB[[2]], by=NULL)%>%
  full_join(dfB[[3]], by=NULL)%>%
  full_join(dfB[[4]], by=NULL)%>%
  full_join(dfB[[5]], by=NULL)%>%
  full_join(dfB[[6]], by=NULL)%>%
  full_join(dfB[[7]], by=NULL)%>%
  full_join(dfB[[8]], by=NULL)%>%
  full_join(dfB[[9]], by=NULL)%>%
  full_join(dfB[[10]], by=NULL)%>%
  full_join(dfB[[11]], by=NULL)%>%
  full_join(dfB[[12]], by=NULL)%>%
  full_join(dfB[[13]], by=NULL)

#View(df) 

dfB<-dfX



