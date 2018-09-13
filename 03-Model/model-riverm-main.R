require(rjags)
bx<-function(v,first,last,nt,b,name1,name2,...){
beg<-1+b
end<-(last-first+1)*nt+b
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end
bxp(z,ylim=c(0,1.2*max(z$stats)),axes=FALSE,...)
axis(2)
axis(1,at=seq(from=beg-b,to=end-b,by=nt),lab=first:last)
box()
points(z$stats[3,],type="l")
}

model<-"
model{
	for(y in 5:years){
		for(r in 1:rivers){

		#smolt measurements
		IS[y,r]~dnorm(LS[y,r],Tau[y,r])
  	Tau[y,r]<-1/log(pow(CIS[y,r],2)+1)							#Distribution of smolt measurement

    # 2+ parr measurement
		IP2[y,r]~dpois(lambdaIP2[y,r])
		lambdaIP2[y,r]~dgamma(aip2[y,r],bip2[y,r])
		aip2[y,r]<-n[y,r]/pow(CV[r],2)
  	bip2[y,r]<-1/(5*P2[y,r]*pow(CV[r],2))

    # 1+ parr measurement
		IP1[y,r]~dpois(lambdaIP1[y,r])
		lambdaIP1[y,r]~dgamma(aip1[y,r],bip1[y,r])
		aip1[y,r]<-n[y,r]/pow(CV[r],2)
  	bip1[y,r]<-1/(5*P1[y,r]*pow(CV[r],2))

  	# >1+ parr measurement
		IOP1[y,r]~dpois(lambdaIOP1[y,r])
		lambdaIOP1[y,r]~dgamma(aiop1[y,r],biop1[y,r])
		aiop1[y,r]<-n[y,r]/pow(CV[r],2)
  	biop1[y,r]<-1/(5*OP1[y,r]*pow(CV[r],2))

		# 0+ parr measurement
		IP0[y,r]~dpois(lambdaIP0[y,r])
		lambdaIP0[y,r]~dgamma(aip0[y,r],bip0[y,r])
		aip0[y,r]<-n[y,r]/pow(CV[r],2)
  	bip0[y,r]<-1/(5*P0[y,r]*pow(CV[r],2))

		#smolt abundance
		S[y,r]<-exp(LS[y,r])
		LS[y,r]~dnorm(MS[y,r],TS[y,r])
		MS[y,r]<-log(ES[y,r])-0.5/TS[y,r]
		TS[y,r]<-1/log(mugammas/ES[y,r]+1)
 		ES[y,r]<-betas[r]*(p[1,r]*P2[y-1,r]+p[2,r]*P1[y-1,r])*A[r]/100

		# 2+ parr abundance
    P2[y,r]~dlnorm(MP2[y,r],TP2[y,r])
    MP2[y,r]<-log(EP2[y,r])-0.5/TP2[y,r]
  	EP2[y,r]<-betap[r]*(q[1,r]*P1[y-1,r]+q[2,r]*P1[y-2,r]+q[3,r]*P1[y-3,r])
  	TP2[y,r]<-1/log(mugammap2/EP2[y,r]+1)

		# 1+ parr abundance
    P1[y,r]~dlnorm(MP1[y,r],TP1[y,r])
    MP1[y,r]<-log(EP1[y,r])-0.5/TP1[y,r]
    TP1[y,r]<-1/log(mugammap/EP1[y,r]+1)
		EP1[y,r]<-P0[y-1,r]*alpha[r]

		# >1+ parr abundance
		OP1[y,r]<-P1[y,r]+P2[y,r]

		# 0+ parr abundance

		P0[y,r]~dlnorm(MP0[y,r],TP0[y,r])
		MP0[y,r]<-log(10)-0.5/TP0[y,r]
		TP0[y,r]<-1/log(100/10+1)


		}
	}


# Priors for the first 4 years

for(y in 1:4){
for( r in 1:rivers){

 	P0[y,r]~dlnorm(MP0[y,r],TP0[y,r])
	MP0[y,r]<-log(10)-0.5/TP0[y,r]
	TP0[y,r]<-1/log(100/10+1)

	P1[y,r]~dlnorm(MP1[y,r],TP1[y,r])
	MP1[y,r]<-log(10)-0.5/TP1[y,r]
	TP1[y,r]<-1/log(100/10+1)

	P2[y,r]~dlnorm(MP2[y,r],TP2[y,r])
	MP2[y,r]<-log(10)-0.5/TP2[y,r]
	TP2[y,r]<-1/log(100/10+1)

  S[y,r]<-exp(logS[y,r])
	logS[y,r]~dnorm(MS[y,r],TS[y,r])
	MS[y,r]<-log(10)-0.5/TS[y,r]
	TS[y,r]<-1/log(100/10+1)
  }
}

# Priors for river specific paramaters


for( r in 1:rivers){

# Production area
A[r]<-exp(AL[r])
AL[r]~dnorm(EA[r],Atau[r])
Atau[r]<-1/pow(SA[r],2)

# alpha: survival from 0+ to 1+

alpha[r]~dlnorm(Malpha[r],Talpha)
Malpha[r]<-aalpha+balpha*(AL[r]-mean(AL[]))/sd(AL[])-0.5/Talpha

# betas: survival to smolt stage
betas[r]~dlnorm(Mbetas[r],Talpha)
Mbetas[r]<-abetas+bbetas*(AL[r]-mean(AL[]))/sd(AL[])-0.5/Talpha

# betap: survival of >1+ parr
betap[r]~dlnorm(Mbetap,Tbetap)

CV[r]~dlnorm(Mcv,Tcv)


for(i in 1:3){
qr[i,r]~dgamma(aq[i],1)
q[i,r]<-qr[i,r]/sum(qr[1:3,r])
}
for(i in 1:2){
pt[i,r]~dgamma(ap[i],1)
p[i,r]<-pt[i,r]/sum(pt[1:2,r])
}
}

# Priors for hyperparameters

for(i in 1:2){
ap[i]~dgamma(5,2)
}
for(i in 1:3){
aq[i]~dgamma(1,3)
}

Talpha<-1/log(cbeta*cbeta+1)
Mbetap<-log(mubetap)-0.5/Tbetap
Tbetap<-1/log(c2beta*c2beta+1)
Mcv<-log(muCV)-0.5/Tcv
Tcv<-1/log(CCV*CCV+1)
mubetap~dunif(0,100)

mugammas~dunif(0.01,10)
mugammap~dunif(0.01,10)
mugammap2~dunif(0.01,10)

cbeta~dunif(0.01,2)

muCV~dgamma(1,1)
CCV~dlnorm(0,0.001)T(0.01,10)
c2beta~dunif(0.01,2)


for(y in 5:years){
TotS[y]<-sum(S[y,])

#Groups
TotalS[y,1]<-sum(S[y,1:3])
TotalS[y,2]<-sum(S[y,4:11])
TotalS[y,3]<-S[y,12]

for(r in 1:rivers){
pr[y,r]<-S[y,r]/TotS[y]
}
}
aalpha~dnorm(0,0.0001)
balpha~dnorm(0,0.0001)
abetas~dnorm(0,0.0001)
bbetas~dnorm(0,0.0001)T(,0)

}
"

setwd("f:/models/wgbast10/rivermodel/jags")
cat(model,file="model.txt")
list(EA=c(8.595,7.865,5.528,5.956,4.448,6.335,7.163,2.822,3.098,4.673,4.666,3.003),
SA=c(0.1417,0.1252,0.07537,0.0917,0.1214,0.0941,0.1678,0.2906,0.2904,0.1203,0.1297,0.3414))
list(years=29,rivers=12)
dat<-read.table("riverdata.txt")


IS<-dat[1:30,seq(1,78,by=7)]
CIS<-dat[1:30,seq(2,79,by=7)]
IP2<-dat[1:30,seq(3,80,by=7)]
IP1<-dat[1:30,seq(4,81,by=7)]
IOP1<-dat[1:30,seq(5,82,by=7)]
IP0<-dat[1:30,seq(6,83,by=7)]
n<-dat[1:30,seq(7,84,by=7)]

data<-list(
years=29,rivers=12,
EA=c(8.595,7.865,5.528,5.956,4.448,6.335,7.163,2.822,3.098,4.673,4.666,3.003),
SA=c(0.1417,0.1252,0.07537,0.0917,0.1214,0.0941,0.1678,0.2906,0.2904,0.1203,0.1297,0.3414),
IS=as.matrix(IS),
CIS=as.matrix(CIS),
IP2=as.matrix(IP2),
IOP1=as.matrix(IOP1),
IP0=as.matrix(IP0),
n=as.matrix(n))

inits<-list(aalpha=0,balpha=0,abetas=0,bbetas=-0.1,cbeta=0.1,c2beta=0.1,mugammas=1,mugammap=1,mugammap2=1,mubetap=1,P0=matrix(50,nrow=29,ncol=12)

)

jm<-jags.model("model.txt",data=data,n.chain=2,inits=inits)
mon=c(
"alpha",
"betas",
"betap",
"aalpha",
"balpha",
"abetas",
"bbetas",
"c2beta",
"muCV",
"CCV",
"mugammap",
"mugammas",
"mugammap2",
"mubetap",
"S",
"P0",
"P1",
"P2",
"ap",
"aq",
"p",
"q"
)
system.time(update(jm,n.iter=10000))
system.time(chain<-coda.samples(jm,mon,n.iter=10000,thin=10))
plot(chain[,'ap'])
plot(chain[,'aq'])
v<-as.matrix(chain)
bx(v,1987,(1987+20),1,8,"S[",",1]")
bx(v,1987,(1987+20),1,8,"S[",",2]")
bx(v,1987,(1987+20),1,8,"S[",",3]")
bx(v,1987,(1987+20),1,8,"S[",",4]")
bx(v,1987,(1987+20),1,8,"S[",",5]")