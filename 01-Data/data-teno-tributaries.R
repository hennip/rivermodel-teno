#File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/sahkot ennen v2000 tarkistamatta.xlsx"

#df<-read_xlsx(File, na="", sheet="sahkot ennen v2000 tarkistamatt",
#              range="B1:R26892",col_types=c(rep("guess",3), "date", rep("numeric",7), rep("guess",7)))






# Tributary data from 2000 and onwards

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

# Pulmanki, Tsars, Kevo, Akujoki, Karasjoki, Iesjoki
df3<-df2%>%filter(river=="01.01" | river=="01.03.01" | river=="01.03.02" | river=="01.07"
                  | river=="03" | river=="03.01")

# Pulmanki, Tsars, Kevo
df3<-df2%>%filter(river=="01.01" | river=="01.03.01" | river=="01.03.02")%>%
  ungroup()%>%
  mutate(stock=ifelse(river=="01.01", 2, ifelse(river=="01.03.01", 4, ifelse(river=="01.03.02", 5,river))))

df_trib<-df3%>%select(-IOP1)%>%
  select(-river)%>%
  mutate(stock=parse_double(stock))
