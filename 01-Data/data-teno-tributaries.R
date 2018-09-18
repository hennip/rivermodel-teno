#File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/sahkot ennen v2000 tarkistamatta.xlsx"

#df<-read_xlsx(File, na="", sheet="sahkot ennen v2000 tarkistamatt",
#              range="B1:R26892",col_types=c(rep("guess",3), "date", rep("numeric",7), rep("guess",7)))







File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Sivujoki 2000-luku originaalidata 250315.xlsx"
# Original location: G:\ex-RKTL\0_HOT\Kalavarat\Tenojoki\TENO-NÄÄTÄMÖ_SÄHKÖDATA\Sivujoet

df<-read_xlsx(File, na="", 
              range="A1:R26892",col_types=c(rep("guess",3), "date", rep("numeric",7), rep("guess",7)))

df<-df%>%select(river, place, chdate, ala, maxk, k, species, age)%>%
  mutate(year=year(chdate),month=month(chdate),day=day(chdate))%>%
  filter(species==1)

# Cannot recode P1 etc. areas since are at the same rivers as the ones given with numeric values 
#df%>%mutate(alue=factor(place))%>%
#  mutate(alue2=fct_recode(alue,"1"="P1"))

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


View(df2)


# Tsarsjoki

tsars<-df%>%filter(river=="01.03.01", species==1)#%>%
#  mutate(place=parse_integer(place))

# Obs! individuals without age identification are not summed (there are some that do have length identification but not age)
tsars<-tsars%>%group_by(year, place)%>%
  summarise(`0+`=sum(age==0, na.rm=T),`1+`=sum(age==1, na.rm=T),
            `1++`=sum(age>0, na.rm=T),`2++`=sum(age>1, na.rm=T),
            acre=mean(ala)/100)%>%
  mutate(IP0=`0+`/acre, IP1=`1+`/acre,
         IOP1=`1++`/acre, IP2=`2++`/acre)

tsars%>%select(year, place, acre, IP0, IP1, IP2, IOP1)%>%
  group_by(year)%>%
  summarise(IP0=mean(IP0), IP1=mean(IP1), IOP1=mean(IOP1), IP2=mean(IP2), area=mean(acre), n=n())

View(tsars)

# Kevojoki

kevo<-df%>%filter(river=="01.03.02", species==1)%>%
  mutate(place=parse_integer(place))

kevo<-kevo%>%group_by(year, place)%>%
  summarise(`0+`=sum(age==0),`1+`=sum(age==1),
            `1++`=sum(age>0),`2++`=sum(age>1),
            acre=mean(ala)/100)%>%
  mutate(IP0=`0+`/acre, IP1=`1+`/acre,
         IOP1=`1++`/acre, IP2=`2++`/acre)

kevo%>%select(year, place, acre, IP0, IP1, IP2, IOP1)%>%
  group_by(year)%>%
  summarise(IP0=mean(IP0), IP1=mean(IP1), IOP1=mean(IOP1), IP2=mean(IP2), area=mean(acre), n=n())

