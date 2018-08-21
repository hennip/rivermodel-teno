
File<-"H:/Projects/ISAMA/data/orig/TENO SAHKODATA/Teno-Naatamo_vakioalueiden keski- ja koealuekohtaiset tiheydet_CNT1 ver 04-06-2015.xlsx"
Sheet<-"Utsjoki vuosittain, CNT1"

Range<-c("C6:F41","G6:J41","K6:N41","O6:R41","S6:V41",
         "W6:Z41","AA6:AD41","AE6:AH41","AI6:AL41","AM6:AP41",
         "AQ6:AT41","AU6:AX41")

dfX<-list()
for(i in 1:12){
    dfX[[i]]<-read_xlsx(File, sheet=Sheet, na="", range=Range[i],col_names=c("IP0","IP1","IOP1","IP2"))%>% 
    mutate(year=c(1979:2014),area=i)%>%
    gather("IP0","IP1","IOP1","IP2", key="age", value="density")
    #gather(`0+`,`1+`,`1++`,`2++`, key="age", value="density")
    
}


dfU<-full_join(dfX[[1]], dfX[[2]], by=NULL)%>%
  full_join(dfX[[3]], by=NULL)%>%
  full_join(dfX[[4]], by=NULL)%>%
  full_join(dfX[[5]], by=NULL)%>%
  full_join(dfX[[6]], by=NULL)%>%
  full_join(dfX[[7]], by=NULL)%>%
  full_join(dfX[[8]], by=NULL)%>%
  full_join(dfX[[9]], by=NULL)%>%
  full_join(dfX[[10]], by=NULL)%>%
  full_join(dfX[[11]], by=NULL)%>%
  full_join(dfX[[12]], by=NULL)

dens<-dfU%>%group_by(year, age)%>%
summarize(ave_density=mean(density, na.rm=T), n=sum(!is.na(density)))%>%
  mutate(ave_density=ifelse(ave_density=="NaN", NA, ave_density))%>%
  mutate(n_juveniles= ave_density*n*5) # total number of juveniles across all study areas

dens2<-select(dens, -n_juveniles)%>%
  spread(key=age, value=ave_density)
#View(dens2)

dfU<-select(dens, -ave_density)%>%
  spread(key=age, value=n_juveniles)%>%
  mutate(stock=14)%>%
  select(-IOP1) # leave IOP1 out so the same data is not added twice


# smolts 2002:2014
smolts<-read.table("H:/Projects/ISAMA/prg/output/Utsjoki-smolts/stats_Smolts_etaB_covs_all.csv",
             header=T, sep=",")[136:148,]

dfs<-as.tibble(list(mu=smolts$mean/1000, CIS=smolts$cv))%>%
  mutate(year=c(2002:2014))%>%
  mutate(IS=log(mu*(CIS^2+1)))%>%
  select(-mu)

dfU<-full_join(dfU, dfs, by=NULL)
#View(tmp)

# next: laske koealojen lkm (n) per vuosi: done
# tee tarvittavat muunnokset Utsjoen datalle -> keskitiheydestä per aari poikasten lkm:ään kaikilla koealoilla yhteensä!
# liimaa data yhteen balticin kanssa
# sitten smoltti-input; IS & CIS
# vielä tarvitaan EA & SA (tee oma sormiharjoitus ensin, miten Utsjoki, Kevo, Tsars arviot yhdistetään. 
# Tai vedä vaan epävarmuus hatusta tässä vaiheessa. EA=250HA)
# and we are good to go... nevermind the combatibility issues.
# - mitkä joet itämereltä voidaan laittaa yhteen Tenon kantojen kanssa (mielellään ei ollenkaan)
# - miten pinta-alamäärittelyn yhtenäistämiseen voidaan vaikuttaa (ei ehkä pysty)
# - smoltti-ikä? (selvitä tämä!)
# - koealueiden pinta-alat, vaihtelu historiassa? Nyt oletetaan vakio 5 aaria. Jaska: 1 aari vastaa paremmin todellisuutt
# Tsars & Kevo: 
# - hajanaista sähkötysdataa on olemassa. Kuitenkin suurin osa matskusta Utsjoen pääuomasta. Kuinka klaarataan tämä?
# - Ilmeisesti myös Tsarsilta ja Kevolta on smolttiestimaatteja 80-luvulta?
# joen koko "kovariaattina". Tämä erityisen tärkeää kun lähdetään yhdistelemään erikokoisia jokia. 



