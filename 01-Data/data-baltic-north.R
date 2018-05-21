

df<-read_xlsx("O:/Jokimallipaketti/WGDATA_river model input -2017.xlsx",
              sheet="Calculation template", na="NA",
              #range="B5:CN45", 
              range="B52:CN92",
              col_names=c("IS_1", "CIS_1", "IP2_1", "IP1_1","IOP1_1", "IP0_1", "n_1",
                          "IS_2", "CIS_2", "IP2_2", "IP1_2","IOP1_2", "IP0_2", "n_2",
                          "IS_3", "CIS_3", "IP2_3", "IP1_3","IOP1_3", "IP0_3", "n_3",
                          "IS_4", "CIS_4", "IP2_4", "IP1_4","IOP1_4", "IP0_4", "n_4",
                          "IS_5", "CIS_5", "IP2_5", "IP1_5","IOP1_5", "IP0_5", "n_5",
                          "IS_6", "CIS_6", "IP2_6", "IP1_6","IOP1_6", "IP0_6", "n_6",
                          "IS_7", "CIS_7", "IP2_7", "IP1_7","IOP1_7", "IP0_7", "n_7",
                          "IS_8", "CIS_8", "IP2_8", "IP1_8","IOP1_8", "IP0_8", "n_8",
                          "IS_9", "CIS_9", "IP2_9", "IP1_9","IOP1_9", "IP0_9", "n_9",
                          "IS_10", "CIS_10", "IP2_10", "IP1_10","IOP1_10", "IP0_10", "n_10",
                          "IS_11", "CIS_11", "IP2_11", "IP1_11","IOP1_11", "IP0_11", "n_11",
                          "IS_12", "CIS_12", "IP2_12", "IP1_12","IOP1_12", "IP0_12", "n_12",
                          "IS_13", "CIS_13", "IP2_13", "IP1_13","IOP1_13", "IP0_13", "n_13"))

n=cbind(df$n_1, df$n_2,df$n_3,df$n_4,df$n_5,df$n_6,df$n_7,df$n_8,
        df$n_9,df$n_10,df$n_11,df$n_12,df$n_13)
CIS=cbind(df$CIS_1, df$CIS_2,df$CIS_3,df$CIS_4,df$CIS_5,df$CIS_6,df$CIS_7,df$CIS_8,
          df$CIS_9,df$CIS_10,df$CIS_11,df$CIS_12,df$CIS_13)
IS=cbind(df$IS_1, df$IS_2,df$IS_3,df$IS_4,df$IS_5,df$IS_6,df$IS_7,df$IS_8,
          df$IS_9,df$IS_10,df$IS_11,df$IS_12,df$IS_13)
IP1=round(cbind(df$IP1_1, df$IP1_2,df$IP1_3,df$IP1_4,df$IP1_5,df$IP1_6,df$IP1_7,df$IP1_8,
         df$IP1_9,df$IP1_10,df$IP1_11,df$IP1_12,df$IP1_13),0)
IP2=round(cbind(df$IP2_1, df$IP2_2,df$IP2_3,df$IP2_4,df$IP2_5,df$IP2_6,df$IP2_7,df$IP2_8,
          df$IP2_9,df$IP2_10,df$IP2_11,df$IP2_12,df$IP2_13),0)
IP0=round(cbind(df$IP0_1, df$IP0_2,df$IP0_3,df$IP0_4,df$IP0_5,df$IP0_6,df$IP0_7,df$IP0_8,
          df$IP0_9,df$IP0_10,df$IP0_11,df$IP0_12,df$IP0_13),0)
IOP1=round(cbind(df$IOP1_1, df$IOP1_2,df$IOP1_3,df$IOP1_4,df$IOP1_5,df$IOP1_6,df$IOP1_7,df$IOP1_8,
          df$IOP1_9,df$IOP1_10,df$IOP1_11,df$IOP1_12,df$IOP1_13),0)


df.IS<-df%>%gather(IS_1, IS_2, IS_3, IS_4, IS_5, IS_6, IS_7, 
            IS_8, IS_9, IS_10, IS_11, IS_12, IS_13, 
            key="river", value="IS")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                              "1"="IS_1","2"="IS_2","3"="IS_3","4"="IS_4","5"="IS_5",
         "6"="IS_6","7"="IS_7","8"="IS_8","9"="IS_9","10"="IS_10","11"="IS_11",
         "12"="IS_12","13"="IS_13"))%>%
select(IS, River)

df.CIS<-df%>%gather(CIS_1, CIS_2, CIS_3, CIS_4, CIS_5, CIS_6, CIS_7, 
                   CIS_8, CIS_9, CIS_10, CIS_11, CIS_12, CIS_13, 
                   key="river", value="CIS")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="CIS_1","2"="CIS_2","3"="CIS_3","4"="CIS_4","5"="CIS_5",
                          "6"="CIS_6","7"="CIS_7","8"="CIS_8","9"="CIS_9","10"="CIS_10","11"="CIS_11",
                          "12"="CIS_12","13"="CIS_13"))%>%
  select(CIS, River)
  
df.IP2<-df%>%gather(IP2_1, IP2_2, IP2_3, IP2_4, IP2_5, IP2_6, IP2_7, 
                    IP2_8, IP2_9, IP2_10, IP2_11, IP2_12, IP2_13, 
                    key="river", value="IP2")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="IP2_1","2"="IP2_2","3"="IP2_3","4"="IP2_4","5"="IP2_5",
                          "6"="IP2_6","7"="IP2_7","8"="IP2_8","9"="IP2_9","10"="IP2_10","11"="IP2_11",
                          "12"="IP2_12","13"="IP2_13"))%>%
  select(IP2, River)

df.IP1<-df%>%gather(IP1_1, IP1_2, IP1_3, IP1_4, IP1_5, IP1_6, IP1_7, 
                    IP1_8, IP1_9, IP1_10, IP1_11, IP1_12, IP1_13, 
                    key="river", value="IP1")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="IP1_1","2"="IP1_2","3"="IP1_3","4"="IP1_4","5"="IP1_5",
                          "6"="IP1_6","7"="IP1_7","8"="IP1_8","9"="IP1_9","10"="IP1_10","11"="IP1_11",
                          "12"="IP1_12","13"="IP1_13"))%>%
  select(IP1, River)

df.IOP1<-df%>%gather(IOP1_1, IOP1_2, IOP1_3, IOP1_4, IOP1_5, IOP1_6, IOP1_7, 
                    IOP1_8, IOP1_9, IOP1_10, IOP1_11, IOP1_12, IOP1_13, 
                    key="river", value="IOP1")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="IOP1_1","2"="IOP1_2","3"="IOP1_3","4"="IOP1_4","5"="IOP1_5",
                          "6"="IOP1_6","7"="IOP1_7","8"="IOP1_8","9"="IOP1_9","10"="IOP1_10","11"="IOP1_11",
                          "12"="IOP1_12","13"="IOP1_13"))%>%
  select(IOP1, River)


df.n<-df%>%gather(n_1, n_2, n_3, n_4, n_5, n_6, n_7, 
                     n_8, n_9, n_10, n_11, n_12, n_13, 
                     key="river", value="n")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="n_1","2"="n_2","3"="n_3","4"="n_4","5"="n_5",
                          "6"="n_6","7"="n_7","8"="n_8","9"="n_9","10"="n_10","11"="n_11",
                          "12"="n_12","13"="n_13"))%>%
  select(n, River)


df.IP0<-df%>%gather(IP0_1, IP0_2, IP0_3, IP0_4, IP0_5, IP0_6, IP0_7, 
                    IP0_8, IP0_9, IP0_10, IP0_11, IP0_12, IP0_13, 
                    key="river", value="IP0")%>%
  mutate(River=factor(river))%>%
  mutate(River=fct_recode(River,
                          "1"="IP0_1","2"="IP0_2","3"="IP0_3","4"="IP0_4","5"="IP0_5",
                          "6"="IP0_6","7"="IP0_7","8"="IP0_8","9"="IP0_9","10"="IP0_10","11"="IP0_11",
                          "12"="IP0_12","13"="IP0_13"))%>%
  select(IP0, River)




