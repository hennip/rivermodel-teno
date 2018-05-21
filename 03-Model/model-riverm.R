

#source("00-Functions/packages-and-paths.r")





M1<-"
model{
  for(y in 5:years){
    for(r in 1:rivers){
    
      #smolt measurements on log scale
      IS[y,r]~dnorm(log(S[y,r]),1/log(pow(CIS[y,r],2)+1))

      #smolt abundance
      S[y,r]~dlnorm(log(ES[y,r])-0.5/tauS[y,r],tauS[y,r])
      tauS[y,r]<-1/log(phi.S/ES[y,r]+1)                 
      bs[y,r]<-pow(gammas[r],2)*ES[y,r]							#Distribution of log-smolts
      
      # ES: expected num of smolts on real scale
      ES[y,r]<-betas[r]*(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r])*A[r]/100   
      
      # 2+ parr abundance
      P2[y,r]~dlnorm(log(EP2[y,r])-0.5/tauP2[y,r],tauP2[y,r])
      tauP2[y,r]<-1/log(phi.P2/EP2[y,r]+1)
      bp2[y,r]<-pow(gammap2[r],2)*EP2[y,r]
      EP2[y,r]<-betap[r]*(q[1,r]*P1[y-1,r]+q[2,r]*P1[y-2,r]+q[3,r]*P1[y-3,r])
      
      # 2+ parr measurement (number of parr over all study areas)
      #x~sm.dnegbin(mu,k) <=> x~dnegbin(1/k+1, mu/k)
      IP2[y,r]~dnegbin(1/(bip2[y,r]+1),EIP2[y,r]/bip2[y,r]) 
      EIP2[y,r]<-n[y,r]*5*P2[y,r]
      bip2[y,r]<-EIP2[y,r]*pow(CV[r],2)/n[y,r]
      CIP[y,r]<-1/sqrt(n[y,r])

      # 1+ parr abundance
      P1[y,r]~dlnorm(log(EP1[y,r])-0.5/tauP1[y,r],tauP1[y,r])
      tauP1[y,r]<-1/log(phi.P1/EP1[y,r]+1)
      EP1[y,r]<-P0[y-1,r]*alpha[r]
      bp1[y,r]<-pow(gammap1[r],2)*EP1[y,r]
      
      # 1+ parr measurement
      IP1[y,r]~dnegbin(1/(bip1[y,r]+1),EIP1[y,r]/bip1[y,r])
      EIP1[y,r]<-n[y,r]*5*P1[y,r]
      bip1[y,r]<-EIP1[y,r]/n[y,r]
      
      # >1+ parr abundance
      OP1[y,r]<-P1[y,r]+P2[y,r]
      
      # >1+ parr measurement
      IOP1[y,r]~dnegbin(1/(biop1[y,r]+1),EIOP1[y,r]/biop1[y,r])
      EIOP1[y,r]<-n[y,r]*5*OP1[y,r]
      biop1[y,r]<-EIOP1[y,r]*pow(CV[r],2)/n[y,r]
      
      # 0+ parr abundance
      P0[y,r]~dlnorm(1.1,0.42) # tämä ja ao. ovat liki samat
      #P0[y,r]~sm.dlnorm(10,100)
      
      # 0+ parr measurement
      IP0[y,r]~dnegbin(1/(bip0[y,r]+1),EIP0[y,r]/bip0[y,r])
      EIP0[y,r]<-n[y,r]*5*P0[y,r]
      bip0[y,r]<-EIP0[y,r]*pow(CV[r],2)/n[y,r]
    }
      
    #Groups: if rivers are added, sum those into correct groups!
    TotalS[y,1]<-sum(S[y,1:4])
    TotalS[y,2]<-sum(S[y,5:11])+S[y,13]
    TotalS[y,3]<-S[y,12]
  }
  
  for(y in 1:4){
    for(r in 1:rivers){
      P2[y,r]~dlnorm(1.1,0.42)
      P1[y,r]~dlnorm(1.1,0.42)
      P0[y,r]~dlnorm(1.1,0.42)
      S[y,r]~dlnorm(1.1,0.42)
    }
  }
  
  for(r in 1:rivers){
    A[r]<-exp(AL[r])
    AL[r]~dnorm(EA[r],Atau[r])
    Atau[r]<-1/pow(SA[r],2)
    
    phi.alpha[r]<-pow(c.beta,2)*mu.alpha[r]
    mu.alpha[r]<-exp(a.alpha+b.alpha*std.AL[r])
    std.AL[r]<-(AL[r]-mean.AL)/sqrt((1/(rivers-1)*sumx2.AL))
    x2.AL[r]<-pow(mean.AL-AL[r],2)

    alpha[r]~dlnorm(log(mu.alpha[r])-0.5/T.alpha[r],T.alpha[r])
    T.alpha[r]<-1/log(phi.alpha[r]/mu.alpha[r]+1)

    betas[r]~dlnorm(log(mu.betas[r])-0.5/T.betas[r],T.betas[r])
    T.betas[r]<-1/log(phi.betas[r]/mu.betas[r]+1)

    betap[r]~dlnorm(log(mu.betap)-0.5/T.betap,T.betap)
    
    CV[r]~dlnorm(log(mu.CV)-0.5/T.CV,T.CV)

    log(mu.betas[r])<-a.betas+b.betas*std.AL[r]
    phi.betas[r]<-pow(c.beta,2)*mu.betas[r]
    
    gammap1[r]~dlnorm(log(phi.P1)-0.5/T.gammap1,T.gammap1)
    gammap2[r]~dlnorm(log(phi.P2)-0.5/T.gammap2,T.gammap2)
    gammas[r]~dlnorm(log(phi.S)-0.5/T.gammas,T.gammas)

    #KP2[r]<-100/betas[r]
    #KP1[r]<-100/(betas[r]*betap[r])	
    MP0[r]~dnorm(0,0.001)
    TP0[r]~dgamma(0.1,0.1)
    
    for(i in 1:3){
      qr[i,r]~dgamma(aq[i],1)
      q[i,r]<-qr[i,r]/sum(qr[1:3,r])
    }
    for(i in 1:2){
      pt[i,r]~dgamma(ap[i],1)
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

  mu.betap~dunif(0,100)
  T.betap<-1/log(phi.betap/mu.betap+1)
  phi.betap<-pow(c.beta,2)*mu.betap*100
  c.beta~dunif(0.01,2)

  phi.S~dunif(0.01,10)
  phi.P1~dunif(0.01,10)
  phi.P2~dunif(0.01,10)
  T.gammap1<-1/log(c1.gamma/phi.P1+1)
  T.gammap2<-1/log(c2.gamma/phi.P2+1)
  T.gammas<-1/log(c3.gamma/phi.S+1)
  c1.gamma<-pow(c.gamma,2)*phi.P1
  c2.gamma<-pow(c.gamma,2)*phi.P2
  c3.gamma<-pow(c.gamma,2)*phi.S
  c.gamma~dunif(0.01,0.1)

  mu.CV~dlnorm(-2.3,0.22) # sm.dlnorm(1,100)
  T.CV<-1/log(C.CV/mu.CV+1)
  C.CV~dlnorm(0,0.001)T(0.01,10)

  for(y in 5:years){
    TS[y]<-sum(S[y,])
    #RTS100[y]<-100*round(TS[y]/100)
    
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
n=n, CIS=CIS, IS=IS, IP1=IP1, IP2=IP2, #IP0=IP0, 
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
  "betap",  "mu.betap", "phi.betap",
  "a.betas",  "b.betas",
  "alpha",  "mu.alpha", "phi.alpha",
  "c.gamma",
  #"gammas",  
  "phi.S", "c3.gamma",
  #"gammap1",  
  "phi.P1", "c1.gamma",
  #"gammap2",  
  "phi.P2", "c2.gamma",
  "S",  "P2",
  "P1",  "P0",
  "TS",  
  "pr","q",
  "qr",  "aq",
  "p",  "ap",
  "A",
  "TotalS"
  
)



#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!


t1<-Sys.time();t1
run1 <- run.jags(M1, 
                 data=data,monitor=var_names, inits=NA,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 1000, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 17h

run<-run1
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=T, sample=10000, thin=10, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?

run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=F, sample=10000, thin=10, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?

run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

