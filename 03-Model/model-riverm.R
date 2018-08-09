

#source("00-Functions/packages-and-paths.r")





M1<-"

model{
  for(y in 5:years){
    for(r in 1:rivers){
    
      #smolt measurements on log scale
      IS[y,r]~dnorm(LS[y,r],Tau[y,r])		
      LS[y,r]<-log(S[y,r])
      Tau[y,r]<-1/log(pow(CIS[y,r],2)+1)							#Distribution of smolt measurement
      
      #smolt abundance
      #S[y,r]~sm.dlnorm(ES[y,r],mu.gammas)	
      S[y,r]~dlnorm(MS[y,r],tauS[y,r])	
      MS[y,r]<-log(ES[y,r])-0.5/tauS[y,r]
      tauS[y,r]<-1/log(mu.gammas/ES[y,r]+1) # mu.gammas/ES[y,r] =CV^2 <=> mu.gammas = var(x)/ES
      bs[y,r]<-pow(gammas[r],2)*ES[y,r]							#Distribution of log-smolts
      
      # ES: expected num of smolts on real scale
      ES[y,r]<-betas[r]*(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r])*A[r]/100   
      
      # 2+ parr abundance
      #P2[y,r]~sm.dlnorm(EP2[y,r],mu.gammap2)
      P2[y,r]~dlnorm(MP2[y,r],tauP2[y,r])
      MP2[y,r]<-log(EP2[y,r])-0.5/tauP2[y,r]
      tauP2[y,r]<-1/log(mu.gammap2/EP2[y,r]+1)
      
      bp2[y,r]<-pow(gammap2[r],2)*EP2[y,r]
      EP2[y,r]<-betap[r]*(q[1,r]*P1[y-1,r]+q[2,r]*P1[y-2,r]+q[3,r]*P1[y-3,r])
      
      # 2+ parr measurement (number of parr over all study areas)
      #x~sm.dnegbin(mu,k) <=> x~dnegbin(1/k+1, mu/k)
      #IP2[y,r]~sm.dnegbin(EIP2[y,r],bip2[y,r])
      IP2[y,r]~dnegbin(1/(bip2[y,r]+1),EIP2[y,r]/bip2[y,r]) 
      EIP2[y,r]<-n[y,r]*5*P2[y,r]
      bip2[y,r]<-EIP2[y,r]*pow(CV[r],2)/n[y,r]
      CIP[y,r]<-1/sqrt(n[y,r])
      
      # 1+ parr abundance
      #P1[y,r]~sm.dlnorm(EP1[y,r],mu.gammap)
      P1[y,r]~dlnorm(MP1[y,r],tauP1[y,r])
      MP1[y,r]<-log(EP1[y,r])-0.5/tauP1[y,r]
      tauP1[y,r]<-1/log(mu.gammap/EP1[y,r]+1)
      
      EP1[y,r]<-P0[y-1,r]*alpha[r]
      bp1[y,r]<-pow(gammap[r],2)*EP1[y,r]
      
      # 1+ parr measurement
      #IP1[y,r]~sm.dnegbin(EIP1[y,r],bip1[y,r])
      IP1[y,r]~dnegbin(1/(bip1[y,r]+1),EIP1[y,r]/bip1[y,r]) 
      EIP1[y,r]<-n[y,r]*5*P1[y,r]
      bip1[y,r]<-EIP1[y,r]/n[y,r]
      
      # >1+ parr abundance
      OP1[y,r]<-P1[y,r]+P2[y,r]
      
      # >1+ parr measurement
      #IOP1[y,r]~sm.dnegbin(EIOP1[y,r],biop1[y,r])
      IOP1[y,r]~dnegbin(1/(biop1[y,r]+1),EIOP1[y,r]/biop1[y,r]) 
      
      EIOP1[y,r]<-n[y,r]*5*OP1[y,r]
      biop1[y,r]<-EIOP1[y,r]*pow(CV[r],2)/n[y,r]
      
      # 0+ parr abundance
      #P0[y,r]~sm.dlnorm(10,100)
      P0[y,r]~dlnorm(1.1,0.42)
      
      # 0+ parr measurement
      #IP0[y,r]~sm.dnegbin(EIP0[y,r],bip0[y,r])
      IP0[y,r]~dnegbin(1/(bip0[y,r]+1),EIP0[y,r]/bip0[y,r]) 
      EIP0[y,r]<-n[y,r]*5*P0[y,r]
      bip0[y,r]<-EIP0[y,r]*pow(CV[r],2)/n[y,r]
    }
    
    #Groups: if rivers are added, sum those into correct groups!
    TotalS[y,1]<-sum(S[y,1:4])
    TotalS[y,2]<-sum(S[y,5:11])
    TotalS[y,3]<-S[y,12]
  }
  
  for(y in 1:4){
  for( r in 1:rivers){
  P2[y,r]~dlnorm(1.1,0.42)
  P1[y,r]~dlnorm(1.1,0.42)
  P0[y,r]~dlnorm(1.1,0.42)
  S[y,r]~dlnorm(1.1,0.42)
  
  #P2[y,r]~sm.dlnorm(10,100)
  #P1[y,r]~sm.dlnorm(10,100)
  #P0[y,r]~sm.dlnorm(10,100)
  #S[y,r]~sm.dlnorm(10,100)
  
  }
  }
  
  for( r in 1:rivers){
  
  A[r]<-exp(AL[r])
  AL[r]~dnorm(EA[r],Atau[r])
  Atau[r]<-1/pow(SA[r],2)
  
  c1.beta[r]<-pow(c.beta,2)*mu.alpha[r]
  #mu.alpha[r]<-exp(a.alpha+b.alpha*sm.std(AL[r],AL[]))
  mu.alpha[r]<-exp(a.alpha+b.alpha*std.AL[r])
  
  std.AL[r]<-(AL[r]-mean.AL)/sqrt((1/(rivers-1)*sumx2.AL))
  x2.AL[r]<-pow(mean.AL-AL[r],2)
  
  #alpha[r]~sm.dlnorm(mu.alpha[r],c1.beta[r])
  #betas[r]~sm.dlnorm(mu.betas[r],c3.beta[r])
  #betap[r]~sm.dlnorm(mu.betap,c2.beta)
  
  alpha[r]~dlnorm(M.alpha[r],T.alpha[r])
  M.alpha[r]<-log(mu.alpha[r])-0.5/T.alpha[r]
  T.alpha[r]<-1/log(c1.beta[r]/mu.alpha[r]+1)
  
  betas[r]~dlnorm(M.betas[r],T.betas[r])
  M.betas[r]<-log(mu.betas[r])-0.5/T.betas[r]
  T.betas[r]<-1/log(c3.beta[r]/mu.betas[r]+1)
  
  betap[r]~dlnorm(M.betap,T.betap)
  
  #CV[r]~sm.dlnorm(mu.CV,C.CV)
  CV[r]~dlnorm(M.CV,T.CV)
  
  #log(mu.betas[r])<-a.betas+b.betas*sm.std(AL[r],AL[])
  log(mu.betas[r])<-a.betas+b.betas*std.AL[r]
  c3.beta[r]<-pow(c.beta,2)*mu.betas[r]
  
  #gammap[r]~sm.dlnorm(mu.gammap,c1.gamma)
  #gammap2[r]~sm.dlnorm(mu.gammap2,c2.gamma)
  #gammas[r]~sm.dlnorm(mu.gammas,c3.gamma)
  
  gammap[r]~dlnorm(M.gammap,T.gammap)
  gammap2[r]~dlnorm(M.gammap2,T.gammap2)
  gammas[r]~dlnorm(M.gammas,T.gammas)
  
  #KP2[r]<-100/betas[r]
  #KP1[r]<-100/(betas[r]*betap[r])	
  MP0[r]~dnorm(0,0.001)
  TP0[r]~dgamma(0.1,0.1)
  
  for(i in 1:3){
  qr[i,r]~dgamma(aq[i],1)T(0.001,)
  q[i,r]<-qr[i,r]/sum(qr[1:3,r])
  }
  for(i in 1:2){
  pt[i,r]~dgamma(ap[i],1)T(0.001,)
  p[i,r]<-pt[i,r]/sum(pt[1:2,r])
  }
  }
  for(i in 1:2){
  ap[i]~dgamma(5,2)
  }
  for(i in 1:3){
  aq[i]~dgamma(1,3)
  }
  
  mean.AL<-1/rivers*sum(AL[])
  sumx2.AL<-sum(x2.AL[])
  
  
  #mu.betas~sm.dbetas(1,1,100)
  #mu.betap~sm.dbetas(1,1,100)
    mu.betap~dunif(0,100)
  
  M.betap<-log(mu.betap)-0.5/T.betap
  
  mu.gammas~dunif(0.01,10)
  mu.gammap~dunif(0.01,10)
  mu.gammap2~dunif(0.01,10)
  
  M.gammap<-log(mu.gammap)-0.5/T.gammap
  M.gammap2<-log(mu.gammap2)-0.5/T.gammap2
  M.gammas<-log(mu.gammas)-0.5/T.gammas
  T.gammap<-1/log(c1.gamma/mu.gammap+1)
  T.gammap2<-1/log(c2.gamma/mu.gammap2+1)
  T.gammas<-1/log(c3.gamma/mu.gammas+1)
  
  
  c.beta~dunif(0.01,2)
  
  #mu.CV~sm.dlnorm(1,100)
  mu.CV~dlnorm(-2.3,0.22) # sm.dlnorm(1,100)
  
  C.CV~dlnorm(0,0.001)I(0.01,10) # C.CV =var/mu
  M.CV<-log(mu.CV)-0.5/T.CV
  T.CV<-1/log(C.CV/mu.CV+1)
  
  
  c2.beta<-pow(c.beta,2)*mu.betap
  T.betap<-1/log(c2.beta/mu.betap+1)
  
  
  c1.gamma<-pow(c.gamma,2)*mu.gammap
  c2.gamma<-pow(c.gamma,2)*mu.gammap2
  c3.gamma<-pow(c.gamma,2)*mu.gammas
  c.gamma~dunif(0.01,0.1)
  for(y in 5:years){
  TS[y]<-sum(S[y,])
  RTS100[y]<-100*round(TS[y]/100)
  
  for(r in 1:rivers){
  pr[y,r]<-S[y,r]/TS[y]
  
  }
  }
  a.alpha~dnorm(0,0.0001)
  b.alpha~dnorm(0,0.0001)
  a.betas~dnorm(0,0.0001)
  b.betas~dnorm(0,0.0001)T(,0)
  corr<-0
  
  
  
}"

modelName<-"rivermodel"
dataName<-"baltic"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

data<-list(
  n=n, CIS=CIS, IS=IS, IP1=IP1, IP2=IP2, IP0=IP0, 
  IOP1=IOP1,
  years=41,
  rivers=13,
  EA=c(
    8.595,7.865,5.528,5.956,4.448,
    6.335,7.480,3.435,3.098,5.5013, # Ume & Rickle updated, Öre & Lögde updated
    5.3799,3.003,4.567), # Kåge added
  SA=c(
    0.1417,0.1252,0.07537,0.0917,0.1214,
    0.0941,0.1443,0.22,0.2904,0.12,
    0.122,0.3414,0.22)
)



inits.fn<-function() {
  list(a.alpha=rnorm(1,0,30),b.alpha=rnorm(1,0,30),a.betas=rnorm(1,0,30),b.betas=rlnorm(1,1.2,1),
       mu.CV=rlnorm(1,-2.3,1))  
}

initsall<-list(inits.fn(),inits.fn())


var_names<-c(
  "CV",  "mu.CV",
  "C.CV",  "a.alpha",  "b.alpha",
  "betas",  "c.beta",
  "betap",  "mu.betap", #"phi.betap",
  "a.betas",  "b.betas",
  "alpha",  "mu.alpha", #"phi.alpha",
  "c.gamma",
  "gammas",  "mu.gammas",  "c3.gamma",
  "gammap",  "mu.gammap", "c1.gamma",
  "gammap2",  "mu.gammap2", "c2.gamma",
  "S",  
  #"P2","P1",  "P0",
  "TS",  
  #"pr","q",
  "qr",  "aq",
  "p",  "ap",
  "A",
  "TotalS"
  
)


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

t1<-Sys.time();t1
run0 <- run.jags(M1, 
                 data=data,monitor=var_names, inits=NA,
                 n.chains = 2, method = 'parallel', thin=1, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 1000, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)

t1<-Sys.time();t1
run1 <- extend.jags(run0, combine=F, sample=1000, thin=10, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)

run<-run1
