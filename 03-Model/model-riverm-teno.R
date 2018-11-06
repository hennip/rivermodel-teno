

#source("00-Functions/packages-and-paths.r")



stocksys<-c(1,2,3,3,3,6,7,3)

M1<-"

model{
  for(y in 5:years){

    # ES: expected num of smolts on real scale in all stock systems
    for(r in 1:rivers){
      ES[y,r]<-betas[r]*(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r])*A[r]/100
    }
#    ES[y,1]<-betas[1]*(p[1,1]*P2[y-1,1]+p[2,1]*P1[y-1,1])*A[1]/100  #Teno MS
#    ES[y,2]<-betas[2]*(p[1,2]*P2[y-1,2]+p[2,2]*P1[y-1,2])*A[2]/100  #Pulmanki
#    ES[y,3]<-betas[3]*(p[1,3]*P2[y-1,3]+p[2,3]*P1[y-1,3])*A[3]/100  #Utsjoki main
#    ES[y,4]<-betas[4]*(p[1,4]*P2[y-1,4]+p[2,4]*P1[y-1,4])*A[4]/100  #Tsars
#    ES[y,5]<-betas[5]*(p[1,5]*P2[y-1,5]+p[2,5]*P1[y-1,5])*A[5]/100  #Kevo
#    ES[y,6]<-betas[6]*(p[1,6]*P2[y-1,6]+p[2,6]*P1[y-1,6])*A[6]/100  #Inari
#    ES[y,7]<-betas[7]*(p[1,7]*P2[y-1,7]+p[2,7]*P1[y-1,7])*A[7]/100  #Torne
    ES[y,8]<-sum(ES[y,3:5])                                         # Utsjoki tot
            

    for(r in 1:(rivers+1)){ # Individual stocks & stock systems

      #smolt measurements on log scale
      IS[y,r]~dnorm(LS[y,r],Tau[y,r])		
      LS[y,r]<-log(S[y,r])
      Tau[y,r]<-1/log(pow(CIS[y,r],2)+1)							#Distribution of smolt measurement
      
      #smolt abundance
      S[y,r]~dlnorm(MS[y,r],tauS[y,r])	
      MS[y,r]<-log(ES[y,r])-0.5/tauS[y,r]
      tauS[y,r]<-1/log(mu.gammas/ES[y,r]+1+0.001) # mu.gammas/ES[y,r] =CV^2 <=> mu.gammas = var(x)/ES
    }

    for(r in 1:rivers){ # sub-populations separately (2: Utsjoki main!)
      
      # >=2+ parr abundance  relative density!
      P2[y,r]~dlnorm(MP2[y,r],tauP2[y,r])
      MP2[y,r]<-log(EP2[y,r])-0.5/tauP2[y,r]
      tauP2[y,r]<-1/log(mu.gammap2/EP2[y,r]+1)
      
      bp2[y,r]<-pow(gammap2[r],2)*EP2[y,r]
      EP2[y,r]<-betap[r]*(q[1,r]*P1[y-1,r]+q[2,r]*P1[y-2,r]+q[3,r]*P1[y-3,r])
      
      # >=2+ parr measurement (number of parr over all study areas)
      IP2[y,r]~dnegbin(1/(bip2[y,r]+1),EIP2[y,r]/bip2[y,r]) 
      EIP2[y,r]<-n[y,r]*1*P2[y,r] # EIP2: expected total number
      bip2[y,r]<-EIP2[y,r]*pow(CV[r],2)/n[y,r]
      CIP[y,r]<-1/sqrt(n[y,r])
      
      # 1+ parr abundance
      P1[y,r]~dlnorm(MP1[y,r],tauP1[y,r])
      MP1[y,r]<-log(EP1[y,r])-0.5/tauP1[y,r]
      tauP1[y,r]<-1/log(mu.gammap/EP1[y,r]+1)
      
      EP1[y,r]<-P0[y-1,r]*alpha[r]
      bp1[y,r]<-pow(gammap[r],2)*EP1[y,r]
      
      # 1+ parr measurement
      IP1[y,r]~dnegbin(1/(bip1[y,r]+1),EIP1[y,r]/bip1[y,r]) 
      EIP1[y,r]<-n[y,r]*1*P1[y,r]
      bip1[y,r]<-EIP1[y,r]/n[y,r]
      
      # >1+ parr abundance
      OP1[y,r]<-P1[y,r]+P2[y,r]
      
      # >1+ parr measurement
  #    IOP1[y,r]~dnegbin(1/(biop1[y,r]+1),EIOP1[y,r]/biop1[y,r]) 
  #    EIOP1[y,r]<-n[y,r]*1*OP1[y,r]
  #    biop1[y,r]<-EIOP1[y,r]*pow(CV[r],2)/n[y,r]
      
      # 0+ parr abundance
      P0[y,r]~dlnorm(1.1,0.42)
      
      # 0+ parr measurement
      IP0[y,r]~dnegbin(1/(bip0[y,r]+1),EIP0[y,r]/bip0[y,r]) 
      EIP0[y,r]<-n[y,r]*1*P0[y,r]
      bip0[y,r]<-EIP0[y,r]*pow(CV[r],2)/n[y,r]
    }
    
  }
  
  for(y in 1:4){
    for( r in 1:rivers){
      P2[y,r]~dlnorm(1.1,0.42)
      P1[y,r]~dlnorm(1.1,0.42)
      P0[y,r]~dlnorm(1.1,0.42)
      S[y,r]~dlnorm(1.1,0.42)
    }
  }
  
  for( r in 1:rivers){
    
    A[r]<-exp(AL[r])
    AL[r]~dnorm(EA[r],Atau[r])
    Atau[r]<-1/pow(SA[r],2)
    
    # alpha: survival from 0+ to 1+
    alpha[r]~dlnorm(M.alpha[r],T.alpha)
    #M.alpha[r]<-a.alpha+b.alpha*(AL[r]-mean(AL[]))/sd(AL[])-0.5/T.alpha
    M.alpha[r]<-a.alpha+b.alpha[streamcat[r]]-0.5/T.alpha
  # a.alpha: basic level, small tributaries
  # b.alpha[1]: =0 for small tribs
  # b.alpha[2]: ~D() for large tribs
  # b.alpha[3]: ~D() for Teno main stem


    # betas: survival to smolt stage
    betas[r]~dlnorm(M.betas[r],T.alpha)
    M.betas[r]<-a.betas+b.betas*(AL[r]-mean(AL[]))/sd(AL[])-0.5/T.alpha

    # betap: survival of >1+ parr
    betap[r]~dlnorm(log(mu.betap)-0.5/T.betap,T.betap)
    
    CV[r]~dlnorm(M.CV,T.CV)
   
    gammap[r]~dlnorm(log(mu.gammap)-0.5/T.gammap,T.gammap)
    gammap2[r]~dlnorm(log(mu.gammap2)-0.5/T.gammap2,T.gammap2)
    gammas[r]~dlnorm(log(mu.gammas)-0.5/T.gammas,T.gammas)
    
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

  mu.betap~dunif(0,100)

  mu.gammas~dunif(0.01,10)
  mu.gammap~dunif(0.01,10)
  mu.gammap2~dunif(0.01,10)
  
  T.gammap<-1/log(c1.gamma/mu.gammap+1)
  T.gammap2<-1/log(c2.gamma/mu.gammap2+1)
  T.gammas<-1/log(c3.gamma/mu.gammas+1)
  
  T.alpha<-1/log(c.beta*c.beta+1)
  c.beta~dunif(0.01,2) 
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
    #RTS100[y]<-100*round(TS[y]/100)
  
    for(r in 1:rivers){
      pr[y,r]<-S[y,r]/TS[y]
    }
  }
  a.alpha~dnorm(0,0.0001)
  b.alpha[1]<-0
  b.alpha[2]~dnorm(0,0.0001)
  a.betas~dnorm(0,0.0001)
  b.betas~dnorm(0,0.0001)T(,0)
  corr<-0
  
}"

modelName<-"rivermodelTeno"

Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

# Choose stocks to be included
#stocks<-c(1:4);dataName<-"Teno4"
stocks<-c(1:7);dataName<-"Teno6&Torne"

#stocks:
# 1: Teno main stem
# 2: Pulmanki
# 3: Utsjoki main (pinta-ala, parrit) / tot (smoltit)
# 4: Tsars
# 5: Kevo
# 6: Inarijoki (vakiot)
# (7: Tornionjoki)

streamcat<-c(2,1,1,1,1,1,1)

# Params for reproduction areas
EA<-c(
  7.533, #1: TMS
  3.9106, # 2: Pulmanki
  4.4694, # 3: Utsjoki main
  4.3032, # Tsars
  4.1185, # Kevo
 # 5.4816, #Utsjoki tot
  6.7516, # Inari tot
  8.595# Torne
) 

SA<-c(
  0.482, #TMS
  0.4678,# Pulmanki
  0.4808, # Utsjoki main
  0.4808, # Tsars
  0.4808, # Kevo
  #0.2822, #Utsjoki tot
  0.3518, # Inari tot
  0.1417 # Torne

)


data<-list(
  n=n[,stocks], CIS=CIS[,stocks], IS=IS[,stocks], 
  IP1=IP1[,stocks], IP2=IP2[,stocks], IP0=IP0[,stocks], #IOP1=IOP1[,stocks],
  streamcat=streamcat,
  years=ifelse(length(stocks)==1,1,dim(n[,stocks])[1]),
  rivers=length(stocks),
  EA=EA[stocks],SA=SA[stocks]
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
  "betap",  "mu.betap", "c2.beta",
  "a.betas",  "b.betas",
  "alpha", # "mu.alpha",
  "c.gamma",
  "gammas",  "mu.gammas",  "c3.gamma",
  "gammap",  "mu.gammap", "c1.gamma",
  "gammap2",  "mu.gammap2", "c2.gamma",
  "S",  
  #"P2","P1",  "P0",
  "TS",  
  #"pr","q",
  #"qr",  "aq",
  "p",  #"ap",
  #"TotalS",
  "A"
  
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


t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=3000, thin=30, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1) #10min

run<-run2


t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=F, sample=3000, thin=400, keep.jags.files=T)
t2<-Sys.time();t2
difftime(t2,t1) #2.3h

run<-run3


t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=F, sample=3000, thin=1000, keep.jags.files=T)
t2<-Sys.time();t2
difftime(t2,t1) #5.7h

run<-run4


save(run, file=str_c(pathOut,modelName,"_",dataName,".RData"))

plot(run, var="S[24,2]")
summary(run, var="S[24,2]")
plot(run, var="beta")
summary(run, var="beta")

chains<-as.mcmc.list(run)

source("04-Output/save-stats.r")

