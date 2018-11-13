
#summary(run, var="S")
#summary(run, var="38]")
#plot(run, var="S[20,1]")
#summary(as.mcmc(chains[,"S[2,7]"][[1]]))


years<-c(1980:2019) 
nyears<-length(years)
rivernames<-c(RiverNames, "Utsjoki_tot", "Teno tot")
nstocks<-length(rivernames)


# Estimated annual smolt abundance
for(i in 1:nstocks){
  #i<-7
  df<-boxplot.jags.df2(chains, "S[",str_c(i,"]"),1:nyears)%>%
    mutate(stock=rivernames[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}

df2<-setNames(df2,c("year","q5","q25","q50","q75","q95","stock"))

df<-as.tibble(df2)%>%
  mutate(Year=year+1979)%>%
  filter(Year>1986)%>%
  mutate(stock=parse_factor(stock, levels=rivernames))



ggplot(df, aes(Year))+
  geom_line(aes(Year,q50))+
 geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  facet_wrap(~stock, scales="free")+
  labs(x="Year", y="Smolts (1000's)")+
  theme_bw()
