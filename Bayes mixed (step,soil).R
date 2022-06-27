# step-factor optim and Bayes Emulation

durham=read.csv("Data_PhD/AF_WE/SBAR6-yld.csv")
attach(durham)
x1=as.factor(durham$St)
x2=as.factor(durham$So)
x3=as.factor(durham$WYear)
x4=as.factor(durham$SYear)
Data=data.frame(x1,x2,x3,x4)
a <- as.factor(paste(Data$x1,Data$x2,Data$x3,Data$x4,sep='-'))
Data <- cbind(Data,Unique=a,GYLD,N,P)
my.levels <- levels(a)
d41=subset(Data,Data$Unique=="5-6-1-12")
d42=subset(Data,Data$Unique=="6-6-1-12")
d43=subset(Data,Data$Unique=="7-6-1-12")
d44=subset(Data,Data$Unique=="8-6-1-12")
dfmb1<-d41[d41$N>0 & d41$P>0 ,]
dfmb2<-d42[d42$N>0 & d42$P>0 ,]
dfmb3<-d43[d43$N>0 & d43$P>0 ,]
dfmb4<-d44[d44$N>0 & d44$P>0 ,]

res1=split(dfmb1, sample(rep(1:16,times=c(9))))
df1=rbind(res1$'2',res1$'3',res1$'4',res1$'5',res1$'6',res1$'7',res1$'8',res1$'9',res1$'10',res1$'11',res1$'12',res1$'13',res1$'14',res1$'15',res1$'16')
ynt1=df1$GYLD
xnt1=df1$N
zpt1=df1$P
x1t1=res1$`1`[,7]
z1t1=res1$`1`[,8]
y1t1=res1$`1`[,6]
dst1=df1$x1
d1t1=res1$`1`[,1]
u1t1=df1$x2
dtr1=res1$`1`[,2]

res2=split(dfmb2, sample(rep(1:16,times=9)))
df2=rbind(res2$'2',res2$'3',res2$'4',res2$'5',res2$'6',res2$'7',res2$'8',res2$'9',res2$'10',res2$'11',res2$'12',res2$'13',res2$'14',res2$'15',res2$'16')
ynt2=df2$GYLD
xnt2=df2$N
zpt2=df2$P
x2t2=res2$`1`[,7]
z2t2=res2$`1`[,8]
y2t2=res2$`1`[,6]
dst2=df2$x1
d2t2=res2$`1`[,1]
u1t2=df2$x2
dtr2=res2$`1`[,2]

res3=split(dfmb3, sample(rep(1:16,times=9)))
df3=rbind(res3$'2',res3$'3',res3$'4',res3$'5',res3$'6',res3$'7',res3$'8',res3$'9',res3$'10',res3$'11',res3$'12',res3$'13',res3$'14',res3$'15',res3$'16')
ynt3=df3$GYLD
xnt3=df3$N
zpt3=df3$P
x3t3=res3$`1`[,7]
z3t3=res3$`1`[,8]
y3t3=res3$`1`[,6]
dst3=df3$x1
d3t3=res3$`1`[,1]
u1t3=df3$x2
dtr3=res3$`1`[,2]

res4=split(dfmb4, sample(rep(1:16,times=c(9))))
df4=rbind(res4$'2',res4$'3',res4$'4',res4$'5',res4$'6',res4$'7',res4$'8',res4$'9',res4$'10',res4$'11',res4$'12',res4$'13',res4$'14',res4$'15',res4$'16')
ynt4=df4$GYLD
xnt4=df4$N
zpt4=df4$P
x4t4=res4$`1`[,7]
z4t4=res4$`1`[,8]
y4t4=res4$`1`[,6]
dst4=df4$x1
d4t4=res4$`1`[,1]
dtr4=res4$`1`[,2]
u1t4=df4$x2

xnst=c(xnt1,xnt2,xnt3,xnt4)
zpst=c(zpt1,zpt2,zpt3,zpt4)
yst=c(ynt1,ynt2,ynt3,ynt4)
stf=as.factor(c(dst1,dst2,dst3,dst4))
utr=as.factor(c(u1t1,u1t2,u1t3,u1t4))
#levels(utr) <- c("10","10","10")
#Soil
durham=read.csv("Data_PhD/AF_WE/SBAR6-yld.csv")
attach(durham)
x1=as.factor(durham$St)
x2=as.factor(durham$So)
x3=as.factor(durham$WYear)
x4=as.factor(durham$SYear)
Data=data.frame(x1,x2,x3,x4)
a <- as.factor(paste(Data$x1,Data$x2,Data$x3,Data$x4,sep='-'))
Data <- cbind(Data,Unique=a,GYLD,N,P)
my.levels <- levels(a)
d41=subset(Data,Data$Unique=="5-6-1-12")
d42=subset(Data,Data$Unique=="5-7-1-12")
d43=subset(Data,Data$Unique=="5-8-1-12")
dfmb1<-d41[d41$N>0 & d41$P>0 ,]
dfmb2<-d42[d42$N>0 & d42$P>0 ,]
dfmb3<-d43[d43$N>0 & d43$P>0 ,]
res1=split(dfmb1, sample(rep(1:16,times=c(9))))
df1=rbind(res1$'2',res1$'3',res1$'4',res1$'5',res1$'6',res1$'7',res1$'8',res1$'9',res1$'10',res1$'11',res1$'12',res1$'13',res1$'14',res1$'15',res1$'16')
ynt1=df1$GYLD
xnt1=df1$N
zpt1=df1$P
xt1=res1$`1`[,7]
zt1=res1$`1`[,8]
yt1=res1$`1`[,6]
d1=df1$x2
ds1=df1$x1
dt1=res1$`1`[,2]
da1=res1$`1`[,1]

res2=split(dfmb2, sample(rep(1:16,times=9)))
df2=rbind(res2$'2',res2$'3',res2$'4',res2$'5',res2$'6',res2$'7',res2$'8',res2$'9',res2$'10',res2$'11',res2$'12',res2$'13',res2$'14',res2$'15',res2$'16')
ynt2=df2$GYLD
xnt2=df2$N
zpt2=df2$P
xt2=res2$`1`[,7]
zt2=res2$`1`[,8]
yt2=res2$`1`[,6]
d2=df2$x2
dt2=res2$`1`[,2]
ds2=df2$x1
da2=res2$`1`[,1]

res3=split(dfmb3, sample(rep(1:16,times=9)))
df3=rbind(res3$'2',res3$'3',res3$'4',res3$'5',res3$'6',res3$'7',res3$'8',res3$'9',res3$'10',res3$'11',res3$'12',res3$'13',res3$'14',res3$'15',res3$'16')
ynt3=df3$GYLD
xnt3=df3$N
zpt3=df3$P
xt3=res3$`1`[,7]
zt3=res3$`1`[,8]
yt3=res3$`1`[,6]
d3=df3$x2
dt3=res3$`1`[,2]
ds3=df3$x1
da3=res3$`1`[,1]

xnso=c(xnt1,xnt2,xnt3)
zpso=c(zpt1,zpt2,zpt3)
yso=c(ynt1,ynt2,ynt3)

xn=c(xnso,xnst)
zp=c(zpso,zpst)
y2=c(yso,yst)

sof=as.factor(c(d1,d2,d3))
dsf=as.factor(c(ds1,ds2,ds3))
#levels(dsf) <- c("9", "9", "9","9")

#levels(sof) <- c("11", "12", "13")
stf=as.factor(c(dst1,dst2,dst3,dst4))
wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)

# var-cov beta
nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  nu<-par[3]
  so1<-par[4]
  so2<-par[5]
  so3<-par[6]
  st1<-par[7]
  st2<-par[8]
  st3<-par[9]
  st4<-par[10]
  na <- length(Y)
  R=(1-nu)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(nu,na)
  block<-function (kp)
    {
    m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
    m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
    m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
    m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
    m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
    m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
    m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
    ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
    mb=matrix(1,nrow=(1/7)%*%nrow(R),ncol = (1/7)%*%nrow(R))
    kp=kronecker(ma,mb)
  }
  
  q=block(kp)
  K=as.numeric(q)*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}

#matrix soil and steepness
library(MASS)
beta1=seq(-5,0,by=1)
beta2=seq(-5,0,by=1)
cr1=6
cr2=7
cr3=8
cr4=5
cr5=6
cr6=7
cr7=8
ng=seq(0,0.0002, by=0.00005)
ng=seq(0,1,by=0.2)
ng=seq(0.01,0.1,by=0.02)
ng=0.0
ma=c()
ma=as.matrix(expand.grid(beta1,beta2,ng,cr1,cr2,cr3,cr4,cr5,cr6,cr7))
for (i in 1:nrow(ma)){
  ma[i,] <- nl(c(ma[i,]),Y=y2)
}
L=array(ma[,1],dim=c(length(beta1),length(beta2),length(ng),length(cr1),length(cr2),length(cr3)))
z = matrix(unlist(L[,,1,1,1,1]), nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2"#, #plot.axes={points(log10(0.004667595),log10(0.047178060),col="2")
               #points(log10(0.002820409),log10(0.018523329),col="4")
               #points(-1.30,-1.34,col="3")
               #axis(1)
               #axis(2)}
) 
ma=as.matrix(expand.grid(beta1,beta2,ng,cr1,cr2,cr3,cr4,cr5,cr6,cr7))
m=apply(ma, nl, MARGIN=1, Y=y2)
m1=m[1:121]
m2=m[122:242]
m3=m[243:363]
m4=m[364:484]
m5=m[485:605]
m6=m[606:726]

m1=m[1,][1:121]
m2=m[1,][122:242]
m3=m[1,][243:363]
m4=m[1,][364:484]
m5=m[1,][485:605]
m6=m[1,][606:726]

m1=m[1:441]
m2=m[442:882]
m3=m[883:1323]
m4=m[1324:1764]
m5=m[1765:2205]
m6=m[2206:2646]
z = matrix(unlist(m1), nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2"#, #plot.axes={points(log10(0.004667595),log10(0.047178060),col="2")
               #points(log10(0.002820409),log10(0.018523329),col="4")
               #points(-1.30,-1.34,col="3")
               #axis(1)
               #axis(2)}
)
par(mar = c(5, 5, 5, 5)) 
d=c(d6[1:263],1870.744,unlist(d61[,1]))
# Bayes Emulation
# var-cov beta
nl <- function(par, Y) 
{{
  b1 <-par[1]
  b2<-par[2]
  nu<-par[3]
  so1<-par[4]
  so2<-par[5]
  so3<-par[6]
  st1<-par[7]
  st2<-par[8]
  st3<-par[9]
  st4<-par[10]
  na <- length(Y)
  R=(1-nu)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(nu,na)
  block<-function (kp)
    
  {
    m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
    m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
    m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
    m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
    m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
    m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
    m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
    ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
    mb=matrix(1,nrow=(1/7)%*%nrow(R),ncol = (1/7)%*%nrow(R))
    kp=kronecker(ma,mb)
  }
  
  q=block(kp)
  K=as.numeric(q)*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  #ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
}
  return(list(B=B,si=si,vb=vb))
}
ng=0.06
b1=10^-1.5
b2=10^-1.5
so1=2
so2=2
so3=2
st1=2
st2=2
st3=2
st4=2
est=nl(c(b1,b2,ng,so1,so2,so3,st1,st2,st3,st4),Y=y2)
rs=c(est$si[,1])
v=est$vb
pt=est$B
ED=pt[1,]+pt[2,]*xn+pt[3,]*zp+pt[4,]*wa[,2]+pt[5,]*wa[,3]+pt[6,]*wa[,4]+pt[7,]*wa[,5]+pt[8,]*wa[,6]

#B POINTS
soft=as.factor(c(dt1,dt2,dt3))
dtr=as.factor(c(dtr1,dtr2,dtr3,dtr4))
#levels(soft) <- c("11", "12", "13")
stft=as.factor(c(d1t1,d2t2,d3t3,d4t4))
dat=as.factor(c(da1,da2,da3))
wb1=c(soft,dtr)
wb2=c(dat,stft)
wb=model.matrix(~wb1+wb2)
xt=c(xt1,xt2,xt3,x1t1,x2t2,x3t3,x4t4)
zt=c(zt1,zt2,zt3,z1t1,z2t2,z3t3,z4t4)
yt=c(yt1,yt2,yt3,y1t1,y2t2,y3t3,y4t4)

EB=pt[1,]+pt[2,]*xt+pt[3,]*zt+pt[4,]*wb[,2]+pt[5,]*wb[,3]+pt[6,]*wb[,4]+pt[7,]*wb[,5]+pt[8,]*wb[,6]

w1=data.frame(wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
va=matrix(v, nrow = 8, ncol =8)
k1=data.frame(1,xn,zp,w1[,1],w1[,2],w1[,3],w1[,4],w1[,5])
pa=as.matrix(k1,nrow=length(xn),ncol=8)
v_sigma=pa%*%va%*%t(pa)

w2=data.frame(wb[,2],wb[,3],wb[,4],wb[,5],wb[,6])
k2=data.frame(1,xt,zt,w2[,1],w2[,2],w2[,3],w2[,4],w2[,5])
pb=as.matrix(k2,nrow=length(xt),ncol=8)
v_sigmat=pb%*%va%*%t(pb)

v_sigmanew=pb%*%va%*%t(pa)

sigmal<-function (vb)
  
{ a1=rs*(1-ng)*exp(-b1 * outer(xn, xn, '-')^2 )*exp(-b2*outer(zp, zp, '-')^2)+diag(ng,length(xn))
m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
mb=matrix(1,nrow=(1/7)%*%nrow(a1),ncol = (1/7)%*%nrow(a1))
kp=kronecker(ma,mb)
vb=(kp)%*%a1
}
sigma=v_sigma+sigmal(vb)

uig=ginv(sigmal(vb))%*%pa
bg=ginv((ginv(va)+t(pa)%*%ginv(sigmal(vb))%*%pa))
tg=t(pa)%*%ginv(sigmal(vb))
VD=ginv(sigmal(vb))-uig%*%bg%*%tg

sigmatl<-function (vd)
  
{ a2=rs*(1-ng)*exp(-b1 * outer(xt, xt, '-')^2 )*exp(-b2*outer(zt, zt, '-')^2)+diag(ng,length(xt))
m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
mb=matrix(1,nrow=(1/7)%*%nrow(a2),ncol = (1/7)%*%nrow(a2))
kp=kronecker(ma,mb)
vd=(kp)%*%a2
}
sigmat=v_sigmat+sigmatl(vd)


sigmanewl<-function (cvbd)
{ a3=rs*(1-ng)*exp(-b1 * outer(xt, xn, '-')^2 )*exp(-b2*outer(zt, zp, '-')^2)+diag(ng,length(xt),length(xn))
m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
mb=matrix(1,nrow=(1/7)%*%nrow(a3),ncol = (1/7)%*%nrow(a3))
kp=kronecker(ma,mb)
cvbd=kp%*%a3
}
sigma_new=v_sigmanew+sigmanewl(cvbd)

##BAYES UPDATE
library(MASS)
D=c(y2)
ED_B  <- EB + sigma_new %*% ginv(sigma) %*% (D - ED) 
VarD_B <- sigmat - sigma_new %*% ginv(sigma) %*% t(sigma_new)

ED_B  <- EB + sigma_new %*% VD %*% (D - ED) 
VarD_B <- sigmat - sigma_new %*% VD %*% t(sigma_new)
obs=1:63
plot(obs,yt,ylim=c(0,7))
lines(obs,ED_B,col=2)
lines(obs,ED_B+3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B-3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B+2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B-2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B+1*sqrt(diag(VarD_B)),col=5)
lines(obs,ED_B-1*sqrt(diag(VarD_B)),col=5)
legend(x="top", bty="n",lwd=2, lty=c(NA,1,1,1, 1),
       legend=c("Data", "mean New","90% cred. interval",
                "95%  cred. interval","99% cred. interval"),
       col=c(1,2,3,4,5), ncol=3, pch=c(1,NA,NA,NA,NA))
plot(obs,diag(VarD_B))
abline(h=0)

de=data.frame(obs,expectation=ED_B,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(de, aes(x=obs, y=expectation, shape=step,color=step)) +
  geom_point()

dvar=data.frame(obs,variance=diag(VarD_B),step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(dvar, aes(x=obs, y=variance, shape=step,color=step)) +
  geom_point()


spe<-(yt-ED_B)/sqrt(diag(VarD_B))
spe=c(spe)
plot(obs,spe,ylim=c(-3,3))
res<-1-(diag(VarD_B)/diag(sigmat))
res=c(res)
spe=data.frame(obs,Standardprederror=spe,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(spe, aes(x=obs, y=Standardprederror, shape=step,color=step)) +
  geom_point()

res=data.frame(obs,Resolution=res,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(res, aes(x=obs, y=Resolution, shape=step,color=step)) +
  geom_point()


dis=((yt)-(ED_B))%*%(ginv(diag(VarD_B))%*%(as.matrix(yt)-ED_B))
dis=((yt)-(ED_B))%*%(ginv(diag(VDM))%*%(as.matrix(yt)-ED_B))
rkd=rank(diag(VarD_B))
drd=dis/rkd
plot(obs,c(drd))
VarD_B=sigmat - sigma_new %*% VD %*% t(sigma_new)
uigm=ginv(sigmat)%*%sigma_new
bgm=ginv((ginv(ginv(sigma))+t(sigma_new)%*%ginv(sigmat)%*%sigma_new))
tgm=t(sigma_new)%*%ginv(sigmat)
VDM=ginv(sigmat)-uigm%*%bgm%*%tgm

tg=t(pa)%*%ginv(sigmal(vb))
#optim
nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  #nu<-par[3]
  so1<-par[3]
  so2<-par[4]
  so3<-par[5]
  st1<-par[6]
  st2<-par[7]
  st3<-par[8]
  st4<-par[9]
  na <- length(Y)
  R=(1-0.05)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(.05,na)
  block<-function (kp)
    
  {
    m1=c(1,exp(-so1-so2),exp(-so1-so3),exp(-so1-st1),exp(-so1-st2),exp(-so1-st3),exp(-so1-st4))
    m2=c(exp(-so2-so1),1,exp(-so2-so3),exp(-so2-st1),exp(-so2-st2),exp(-so2-st3),exp(-so2-st4))
    m3=c(exp(-so3-so1),exp(-so3-so2),1,exp(-so3-st1),exp(-so3-st2),exp(-so3-st3),exp(-so3-st4))
    m4=c(exp(-st1-so1),exp(-st1-so2),exp(-st1-so3),1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
    m5=c(exp(-st2-so1),exp(-st2-so2),exp(-st2-so3),exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
    m6=c(exp(-st3-so1),exp(-st3-so2),exp(-st3-so3),exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
    m7=c(exp(-st4-so1),exp(-st4-so2),exp(-st4-so3),exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
    ma=matrix(c(m1,m2,m3,m4,m5,m6,m7),nrow=7,ncol=7)
    mb=matrix(1,nrow=(1/7)%*%nrow(R),ncol = (1/7)%*%nrow(R))
    kp=kronecker(ma,mb)
  }
  
  q=block(kp)
  K=as.numeric(q)*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}
beta1=seq(-2,2.5,by=.5)
beta2=seq(-2,2.5,by=.5)
cr1=6
cr2=7
cr3=8
cr4=5
cr5=6
cr6=7
cr7=8
ng=seq(0,0.0002, by=0.00005)
ng=seq(0,1,by=0.2)
ng=seq(0,0.1,by=0.02)
ng=0.05
ma=c()
df=as.matrix(expand.grid(beta1,beta2,cr1,cr2,cr3,cr4,cr5,cr6,cr7))
df=as.matrix(expand.grid(cr1,cr2,cr3,cr4,cr5,cr6,cr7))
df1=df[1:2,]
df1=df[3:4,]
df3=df[5:6,]
df4=df[7:8,]
df1=df[11:12,]
for (i in 1:nrow(df1))
{
  df1[i,]<-optim(c(df1[i,]), nl, Y=y2, method = "L-BFGS-B", lower = c(-5,-5,.01,1,1,1,1,1,1), upper = c(5,5,.2,2,2,2,2,2,2))$par
}


# New Bayes Emulation
wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
nl <- function(par,Y) 
{
  b1 <-par[1]
  b2<-par[2]
  nu<-par[3]
  so1<-par[4]
  so2<-par[5]
  so3<-par[6]
  st1<-par[7]
  st2<-par[8]
  st3<-par[9]
  st4<-par[10]
 na <- length(Y)
 R=(1-nu)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(nu,na)
 
 sofac = numeric(length(wx))
 sofac[wx==6] <- so1
 sofac[wx==7]<-so2
 sofac[wx==8]<-so3
 soil=exp(-outer(sofac, sofac, "-")^2)

 stfac = numeric(length(wy))
 stfac[wy==5] <- st1
 stfac[wy==6] <- st2
 stfac[wy==7]<-st3
 stfac[wy==8]<-st4
 step=exp(-outer(stfac, stfac, "-")^2)
  K= soil*step*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  #return(list(B=B,si=si,vb=vb))
  return(ll)
}
#plot
library(MASS)
beta1=seq(-5,5,by=1)
beta2=seq(-5,5,by=1)
beta1=seq(-5,5,by=.5)
beta2=seq(-5,5,by=.5)
cr1=5
cr2=6
cr3=7
cr4=5
cr5=6
cr6=7
cr7=8
cr1=1
cr2=1
cr3=1
cr4=1
cr5=1
cr6=1
cr7=1
ng=seq(0,0.0002, by=0.00005)
ng=seq(0,1,by=0.2)
ng=seq(0.01,0.1,by=0.02)
ng=0.05
ma=as.matrix(expand.grid(beta1,beta2,ng,cr1,cr2,cr3,cr4,cr5,cr6,cr7))
for (i in 1:nrow(ma)){
  ma[i,] <- nl(c(ma[i,]),Y=y2)
}
m=apply(ma, nl, MARGIN=1, Y=y2)
m1=m[1:121]
m2=m[122:242]
m3=m[243:363]
m4=m[364:484]
m5=m[485:605]
m6=m[606:726]

m1=m[1,][1:121]
m2=m[1,][122:242]
m3=m[1,][243:363]
m4=m[1,][364:484]
m5=m[1,][485:605]
m6=m[1,][606:726]

m1=m[1,][1:441]
m2=m[442:882]
m3=m[883:1323]
m4=m[1324:1764]
m5=m[1765:2205]
m6=m[2206:2646]
z = matrix(unlist(m1), nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2"#, #plot.axes={points(log10(0.004667595),log10(0.047178060),col="2")
               #points(log10(0.002820409),log10(0.018523329),col="4")
               #points(-1.30,-1.34,col="3")
               #axis(1)
               #axis(2)}
               )

ng=0.01
b1=10^-1.5
b2=10^-1.5
so1=1
so2=1
so3=1
st1=1
st2=1
st3=1
st4=1
est=nl(c(b1,b2,ng,so1,so2,so3,st1,st2,st3,st4),Y=y2)
rs=c(est$si[,1])
v=est$vb
pt=est$B

# D Points
ED=pt[1,]+pt[2,]*xn+pt[3,]*zp+pt[4,]*wa[,2]+pt[5,]*wa[,3]+pt[6,]*wa[,4]+pt[7,]*wa[,5]+pt[8,]*wa[,6]

#B POINTS
soft=as.factor(c(dt1,dt2,dt3))
dtr=as.factor(c(dtr1,dtr2,dtr3,dtr4))
stft=as.factor(c(d1t1,d2t2,d3t3,d4t4))
dat=as.factor(c(da1,da2,da3))
wb1=c(soft,dtr)
wb2=c(dat,stft)
wb=model.matrix(~wb1+wb2)
xt=c(xt1,xt2,xt3,x1t1,x2t2,x3t3,x4t4)
zt=c(zt1,zt2,zt3,z1t1,z2t2,z3t3,z4t4)
yt=c(yt1,yt2,yt3,y1t1,y2t2,y3t3,y4t4)

EB=pt[1,]+pt[2,]*xt+pt[3,]*zt+pt[4,]*wb[,2]+pt[5,]*wb[,3]+pt[6,]*wb[,4]+pt[7,]*wb[,5]+pt[8,]*wb[,6]
# Variance Part
w1=data.frame(wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
va=matrix(v, nrow = nrow(v), ncol =ncol(v))
k1=data.frame(1,xn,zp,w1[,1],w1[,2],w1[,3],w1[,4],w1[,5])
pa=as.matrix(k1,nrow=length(xn),ncol=ncol(k1))
v_sigma=pa%*%va%*%t(pa)

w2=data.frame(wb[,2],wb[,3],wb[,4],wb[,5],wb[,6])
k2=data.frame(1,xt,zt,w2[,1],w2[,2],w2[,3],w2[,4],w2[,5])
pb=as.matrix(k2,nrow=length(xt),ncol=nrow(v))
v_sigmat=pb%*%va%*%t(pb)

v_sigmanew=pb%*%va%*%t(pa)
#v_sigmanew[v_sigmanew<0]<-0
# Gaussian Corr part
sigmal<-function (vb)
  
{ a1=rs*(1-ng)*exp(-b1 * outer(xn, xn, '-')^2 )*exp(-b2*outer(zp, zp, '-')^2)+diag(ng,length(xn))
sofac = numeric(length(wx))
sofac[wx==6] <- so1
sofac[wx==7]<-so2
sofac[wx==8]<-so3
#soil=exp(-outer`(sofac, sofac, "-")^2)
soil=exp(-outer(sofac, sofac, "+"))
diag(soil)=1
stfac = numeric(length(wy))
stfac[wy==5] <- st1
stfac[wy==6] <- st2
stfac[wy==7]<-st3
stfac[wy==8]<-st4
#step=exp(-outer(stfac, stfac, "-")^2)
step=exp(-outer(stfac, stfac, "+"))
diag(step)=1
 vb=soil*step*a1
}
sigma=v_sigma+sigmal(vb)

uig=ginv(sigmal(vb))%*%pa
bg=ginv((ginv(va)+t(pa)%*%ginv(sigmal(vb))%*%pa))
tg=t(pa)%*%ginv(sigmal(vb))
VD=ginv(sigmal(vb))-uig%*%bg%*%tg

sigmatl<-function (vd)
  
{ a2=rs*(1-ng)*exp(-b1 * outer(xt, xt, '-')^2 )*exp(-b2*outer(zt, zt, '-')^2)+diag(ng,length(xt))
sofact = numeric(length(wb1))
sofact[wb1==6] <- so1
sofact[wb1==7]<-so2
sofact[wb1==8]<-so3
#soil=exp(-outer(sofact, sofact, "-")^2)
soil=exp(-outer(sofac, sofac, "+"))
diag(soil)=1
stfact = numeric(length(wb2))
stfact[wb2==5] <- st1
stfact[wb2==6] <- st2
stfact[wb2==7]<-st3
stfact[wb2==8]<-st4
#step=exp(-outer(stfact, stfact, "-")^2)
step=exp(-outer(stfact, stfact, "+"))
diag(step)=1
vd=soil*step*a2
}
sigmat=v_sigmat+sigmatl(vd)


sigmanewl<-function (cvbd)
{ a3=rs*(1-ng)*exp(-b1 * outer(xt, xn, '-')^2 )*exp(-b2*outer(zt, zp, '-')^2)+diag(ng,length(xt),length(xn))
sofac = numeric(length(wx))
sofac[wx==6] <- so1
sofac[wx==7]<-so2
sofac[wx==8]<-so3

stfac = numeric(length(wy))
stfac[wy==5] <- st1
stfac[wy==6] <- st2
stfac[wy==7]<-st3
stfac[wy==8]<-st4

sofact = numeric(length(wb1))
sofact[wb1==6] <- so1
sofact[wb1==7]<-so2
sofact[wb1==8]<-so3

stfact = numeric(length(wb2))
stfact[wb2==5] <- st1
stfact[wb2==6] <- st2
stfact[wb2==7]<-st3
stfact[wb2==8]<-st4
#soiln=exp(-outer(sofact, sofac, "-")^2)
soiln=exp(-outer(sofact, sofac, "+"))
diag(soiln)=1
#stepn=exp(-outer(stfact, stfac, "-")^2)
stepn=exp(-outer(stfact, stfac, "+"))
diag(stepn)=1
cvbd=soiln*stepn*a3
}
sigma_new=v_sigmanew+sigmanewl(cvbd)
##BAYES UPDATE
library(MASS)
D=c(y2)
ED_B  <- EB + sigma_new %*% ginv(sigma) %*% (D - ED) 
VarD_B <- sigmat - sigma_new %*% ginv(sigma) %*% t(sigma_new)

ED_B  <- EB + sigma_new %*% VD %*% (D - ED) 
VarD_B <- sigmat - sigma_new %*% VD %*% t(sigma_new)

obs=1:63
plot(obs,yt,ylim=c(0,7))
lines(obs,ED_B,col=2)
lines(obs,ED_B+3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B-3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B+2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B-2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B+1*sqrt(diag(VarD_B)),col=5)
lines(obs,ED_B-1*sqrt(diag(VarD_B)),col=5)
legend(x="top", bty="n",lwd=2, lty=c(NA,1,1,1, 1),
       legend=c("Data", "mean New","90% cred.",
                "95%  cred.","99% cred."),
       col=c(1,2,3,4,5), ncol=3, pch=c(1,NA,NA,NA,NA))
plot(obs,diag(VarD_B))
abline(h=0)

de=data.frame(obs,expectation=ED_B,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(de, aes(x=obs, y=expectation, shape=step,color=step)) +
  geom_point()

dvar=data.frame(obs,variance=diag(VarD_B),step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(dvar, aes(x=obs, y=variance, shape=step,color=step)) +
  geom_point()


spe<-(yt-ED_B)/sqrt(diag(VarD_B))
spe=c(spe)
plot(obs,spe)
abline(h=2,col=2)
abline(h=-2,col=2)
res<-1-(diag(VarD_B)/diag(sigmat))
res=c(res)
plot(obs,res)

spe=data.frame(obs,Standardprederror=spe,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(spe, aes(x=obs, y=Standardprederror, shape=step,color=step)) +
  geom_point()

res=data.frame(obs,Resolution=res,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(res, aes(x=obs, y=Resolution, shape=step,color=step)) +
  geom_point()


dis=((yt)-(ED_B))%*%(ginv(diag(VarD_B))%*%(as.matrix(yt)-ED_B))
plot(obs,dis)
rkd=rank(diag(VarD_B))
drd=dis/rkd
plot(obs,c(drd))

w1=c(6,6,7,7,8,8,6)
sofact = numeric(length(w1))
sofact[w1==6] <- so1
sofact[w1==7]<-so2
sofact[w1==8]<-so3
s=exp(-outer(sofact, sofact, "+"))
diag(s)=1
#McMillan Way

wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
nl <- function(par,Y) 
{
  b1 <-par[1]
  b2<-par[2]
  nu<-par[3]
  so1<-par[4]
  so2<-par[5]
  so3<-par[6]
  st1<-par[7]
  st2<-par[8]
  st3<-par[9]
  st4<-par[10]
  na <- length(Y)
  R=(1-nu)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(nu,na)
  
  sofac = numeric(length(wx))
  sofac[wx==6] <- so1
  sofac[wx==7]<-so2
  sofac[wx==8]<-so3
  soil=exp(-outer(sofac, sofac, "+"))
  diag(soil)=1
  stfac = numeric(length(wy))
  stfac[wy==5] <- st1
  stfac[wy==6] <- st2
  stfac[wy==7]<-st3
  stfac[wy==8]<-st4
  step=exp(-outer(stfac, stfac, "+"))
  diag(step)=1
  K= soil*step*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  #return(list(B=B,si=si,vb=vb))
  return(ll)
}


#Zaou Way

wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
nl <- function(par,Y) 
{
  b1 <-par[1]
  b2<-par[2]
  nu<-par[3]
  so1<-par[4]
  so2<-par[5]
  so3<-par[6]
  st1<-par[7]
  st2<-par[8]
  st3<-par[9]
  st4<-par[10]
  na <- length(Y)
  R=(1-nu)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(nu,na)
  
  sofac = numeric(length(wx))
  sofac[wx==6] <- so1
  sofac[wx==7]<-so2
  sofac[wx==8]<-so3
  soil=exp(-outer(sofac, sofac, "+"))
  diag(soil)=1
  stfac = numeric(length(wy))
  stfac[wy==5] <- st1
  stfac[wy==6] <- st2
  stfac[wy==7]<-st3
  stfac[wy==8]<-st4
  step=exp(-outer(stfac, stfac, "+"))
  diag(step)=1
  K= soil*step*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(list(B=B,si=si,vb=vb))
  #return(si)
}

nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  #nu<-par[3]
  c12<-par[3]
  c13<-par[4]
  c23<-par[5]
  na <- length(Y)
  R=(1-0.0)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(0.0,na)
  block<-function (kp)
    
  {
    m1=c(1,(cos(c12 * pi / 180)),cos(c13 * pi / 180))
    m2=c(cos(c12 * pi / 180),1,(cos(c12 * pi / 180)*cos(c13 * pi / 180)+sin(c12 * pi / 180)*sin(c13 * pi / 180)*cos(c23 * pi / 180)))
    m3=c(cos(c13 * pi / 180),(cos(c12 * pi / 180)*cos(c13 * pi / 180)+sin(c12 * pi / 180)*sin(c13 * pi / 180)*cos(c23 * pi / 180)),1)
    ma=matrix(c(m1,m2,m3),nrow=3,ncol=3)
    mb=matrix(1,nrow=(1/3)%*%nrow(R),ncol = (1/3)%*%nrow(R))
    kp=kronecker(ma,mb)
  }
  q=block(kp)
  K=as.numeric(q)*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}
library(MASS)
b1=seq(-5,5,by=1)
b2=seq(-5,5,by=1)
c12=60
c13=60
c23=60
c12=seq(0,180,by=60)
c13=seq(0,180,by=60)
c23=seq(0,180,by=60)
ma=as.matrix(expand.grid(b1,b2,c12,c13,c23))
for (i in 1:nrow(ma)){
  ma[[i]] <- nl(c(ma[i,]),Y=y2)
}

L=array(ma[,1],dim=c(length(beta1),length(beta2),length(c12),length(c13),length(c23)))
z = matrix(unlist(L[,,1,1,1]), nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2"#, #plot.axes={points(log10(0.004667595),log10(0.047178060),col="2")
               #points(log10(0.002820409),log10(0.018523329),col="4")
               #points(-1.30,-1.34,col="3")
               #axis(1)
               #axis(2)}
) 
ma=apply(ma, nl, MARGIN=1, Y=y2)
beta1=-1
beta2=-3
c12=seq(60,180,by=30)
c13=seq(60,180,by=30)
c23=seq(60,180,by=30)
df=as.matrix(expand.grid(beta1,beta2,c12,c13,c23))
df1=df[1:7,]
df1=df[3:4,]
df3=df[5:6,]
df4=df[7:8,]
df1=df[19:21,]
for (i in 1:nrow(df1))
{
  df1[i,]<-optim(c(df1[i,]), nl, Y=y2, method = "L-BFGS-B", lower = c(-5,-5,0,0,0), upper = c(5,5,180,180,180))$par
}
library(optimx)
par1=as.vector(c(-1.5,-1.5,90,90,90))
optimx(par1, nl, gr=Null, Hess=Null, lower=c(-5,-5,0,0,0),
       upper=c(5,5,180,180,180), method="L-BFGS-B", itnmax=Null)
c12 =59.97
c13=29.91
c23=29.80
#c14=50
#c24=70
#c34=90
(cos(c12 * pi / 180)*cos(c13 * pi / 180)+sin(c12 * pi / 180)*sin(c13 * pi / 180)*cos(c23 * pi / 180))

L3=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)

#ZHOU FINAL

wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
hdata=data.frame(xn,zp,wx,wy)

nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  ng<-par[3]
  c12<-par[4]
  c13<-par[5]
  c23<-par[6]
  c14<-par[7]
  c24<-par[8]
  c34<-par[9]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
  T_so=L3_so%*%t(L3_so)
H_so<- matrix(nrow = length(xn), ncol = length(zp))
      for(i in 1:nrow(H_so)){
      for(j in 1:ncol(H_so)){
        H_so[i,j]=T_so[hdata$wx[i],hdata$wx[j]]
        H_so[j,i]=H_so[i,j]
    }
      }

l1=c(1,0,0,0)
l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
T_st=L4_st%*%t(L4_st)

H_st<- matrix(nrow = length(xn), ncol = length(zp))
for(i in 1:nrow(H_st)){
  for(j in 1:ncol(H_st)){
    H_st[i,j]=T_st[hdata$wy[i],hdata$wy[j]]
    H_st[j,i]=H_st[i,j]
  }
}
  K=H_so*H_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}

library(MASS)
c12=seq(0,180,by=60)
c13=seq(0,180,by=60)
c23=seq(0,180,by=60)

beta1=seq(-5,5,by=1)
beta2=seq(-5,5,by=1)
c12=90
c13=90
c23=90
c14=c12
c24=c12
c34=c12
ng=0.01
ng=c(0.01,0.05,0.1)
md=as.matrix(expand.grid(beta1,beta2,ng,c12,c13,c23,c14,c24,c34))
ma=as.matrix(expand.grid(beta1,beta2,ng,c12,c13,c23,c14,c24,c34))
for (i in 1:nrow(ma)){
  ma[[i]] <- nl(c(ma[i,]),Y=y2)}
L=array(ma[,1],dim=c(length(beta1),length(beta2),length(c12),length(c13),length(c23),length(c14),length(c24),length(c34)))
z = matrix(unlist(L[,,1,1,1,1,1,1]), nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2")
m=apply(ma, nl, MARGIN=1, Y=y2)
m1=ma[,1][1:121]
z = matrix(m1, nrow =length(beta1), ncol = length(beta2))
filled.contour(x = c(beta1), y = c(beta2), z=z, xlab="beta1",ylab="beta2")

# Optimization
nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  ng<-par[3]
  c12<-par[4]
  c13<-par[5]
  c23<-par[6]
  c14=par[7]
  c24=par[8]
  c34=par[9]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
  T_so=L3_so%*%t(L3_so)
  H_so<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(H_so)){
    for(j in 1:ncol(H_so)){
      H_so[i,j]=T_so[hdata$wx[i],hdata$wx[j]]
      H_so[j,i]=H_so[i,j]
    }
  }
  l1=c(1,0,0,0)
  l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
  l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
  l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
  L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
  T_st=L4_st%*%t(L4_st)
  
  a1_st<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(a1_st)){
    for(j in 1:ncol(a1_st)){
      a1_st[i,j]=T_st[hadata$wy[i],hadata$wy[j]]
    }
  }
  K=H_so*a1_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}
# only soil

nl <- function(par, Y) 
{
  b1 <--1.5
  b2<--1.5
  ng<-0.01
  c12<-par[1]
  c13<-par[2]
  c23<-par[3]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
  T_so=L3_so%*%t(L3_so)
  H_so<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(H_so)){
    for(j in i:ncol(H_so)){
      H_so[i,j]=T_so[hdata$wx[i],hdata$wx[j]]
      H_so[j,i]=H_so[i,j]
    }
  }
 
  K=H_so*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}


# only steepness
nl <- function(par, Y) 
{
  b1 <--1.5
  b2<--1.5
  ng<-0.01
  c12<-par[1]
  c13<-par[2]
  c23<-par[3]
  c14=par[4]
  c24=par[5]
  c34=par[6]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  l1=c(1,0,0,0)
  l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
  l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
  l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
  L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
  T_st=L4_st%*%t(L4_st)
  
  a1_st<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(a1_st)){
    for(j in 1:ncol(a1_st)){
      a1_st[i,j]=T_st[hadata$wy[i],hadata$wy[j]]
    }
  }
  K=a1_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}

l1=c(1,0,0,0)
l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
T_st=L4_st%*%t(L4_st)

a1_st<- matrix(nrow = length(xn), ncol = length(zp))
for(i in 1:nrow(a1_st)){
  for(j in 1:ncol(a1_st)){
    a1_st[i,j]=T_st[hadata$wy[i],hadata$wy[j]]
  }
}

b1=-1.35
b2=-1.45
c12=70
c13=70
c23=70
c14=c12
c24=c12
c34=c12
ng=0.01
ng=c(0.01,0.05,0.1)
df=as.matrix(expand.grid(beta1,beta2,ng,c12,c13,c23,c14,c24,c34))
df=as.matrix(expand.grid(beta1,beta2,ng,c12,c13,c23,c14,c24,c34))
c12=seq(10,180,by=30)
c13=seq(10,180,by=30)
c23=seq(10,180,by=30)
c14=c12
c24=c12
c34=c12
df=as.matrix(expand.grid(c12,c13,c23))
df=as.matrix(expand.grid(c12,c13,c23,c14,c24,c34))
df1=df[1:3,]
for (i in 1:nrow(df1))
{
  df1[i,]<-optim(c(df1[i,]), nl, Y=y2, method = "L-BFGS-B", lower = c(-2,-5,0.01,0,0,0,0,0,0), upper = c(5,3,0.1,180,180,180,180,180,180))$par
}
optim(c(df[1,]), nl, Y=y2, method = "L-BFGS-B", lower = c(10,10,10), upper = c(180,180,180))$par
df=as.matrix(expand.grid(beta1,beta2,ng,c12,c13,c23))
optim(c(df[1,]), nl, Y=y2, method = "L-BFGS-B", lower = c(-2,-5,0.01,0,0,0), upper = c(5,3,0.1,180,180,180))$par

# Bayes linear Emulation Zhou Way
wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
hdata=data.frame(xn,zp,wx,wy)

nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  ng<-par[3]
  c12<-par[4]
  c13<-par[5]
  c23<-par[6]
  c14=par[7]
  c24=par[8]
  c34=par[9]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
  T_so=L3_so%*%t(L3_so)
  H_so<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(H_so)){
    for(j in 1:ncol(H_so)){
      H_so[i,j]=T_so[hdata$wx[i],hdata$wx[j]]
      H_so[j,i]=H_so[i,j]
    }
  }
  l1=c(1,0,0,0)
  l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
  l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
  l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
  L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
  T_st=L4_st%*%t(L4_st)
  
  a2_st<- matrix(nrow = length(xn), ncol = length(zp))
  for(i in 1:nrow(a2_st)){
    for(j in 1:ncol(a2_st)){
      a2_st[i,j]=T_st[hdata$wy[i],hdata$wy[j]]
      a2_st[j,i]=a2_st[i,j]
    }
  }
  K=H_so*a2_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  return(list(B=B,si=si,vb=vb))
}
b1=10^-1.5
b2=10^-1.5
 c12=30
 c13=30
 c23=30
 c14=30
 c24=30
 c34=30
 #ng=c(0.01,0.05,0.1)
ng=0.01
#est=nl(c(b1,b2,ng,c12,c13,c23,c12a,c13a,c23a,c14,c24,c34),Y=y2)
est=nl(c(b1,b2,ng,c12,c13,c23,c14,c24,c34),Y=y2)
rs=c(est$si[,1])
v=est$vb
pt=est$B

# D Points
ED=pt[1,]+pt[2,]*xn+pt[3,]*zp+pt[4,]*wa[,2]+pt[5,]*wa[,3]+pt[6,]*wa[,4]+pt[7,]*wa[,5]+pt[8,]*wa[,6]

#B POINTS
soft=as.factor(c(dt1,dt2,dt3))
dtr=as.factor(c(dtr1,dtr2,dtr3,dtr4))
stft=as.factor(c(d1t1,d2t2,d3t3,d4t4))
dat=as.factor(c(da1,da2,da3))
wb1=c(soft,dtr)
wb2=c(dat,stft)
wb=model.matrix(~wb1+wb2)
xt=c(xt1,xt2,xt3,x1t1,x2t2,x3t3,x4t4)
zt=c(zt1,zt2,zt3,z1t1,z2t2,z3t3,z4t4)
yt=c(yt1,yt2,yt3,y1t1,y2t2,y3t3,y4t4)
EB=pt[1,]+pt[2,]*xt+pt[3,]*zt+pt[4,]*wb[,2]+pt[5,]*wb[,3]+pt[6,]*wb[,4]+pt[7,]*wb[,5]+pt[8,]*wb[,6]

# Variance Part
w1=data.frame(wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
va=matrix(v, nrow = nrow(v), ncol =ncol(v))
k1=data.frame(1,xn,zp,w1[,1],w1[,2],w1[,3],w1[,4],w1[,5])
pa=as.matrix(k1,nrow=length(xn),ncol=ncol(v))
v_sigma=pa%*%va%*%t(pa)

w2=data.frame(wb[,2],wb[,3],wb[,4],wb[,5],wb[,6])
k2=data.frame(1,xt,zt,w2[,1],w2[,2],w2[,3],w2[,4],w2[,5])
pb=as.matrix(k2,nrow=length(xt),ncol=ncol(v))
v_sigmat=pb%*%va%*%t(pb)

v_sigmanew=pb%*%va%*%t(pa)

# Gaussian Corr part D points

hadata=data.frame(xn,zp,wx,wy)
sigmal<-function (vb)
  
{ a1=rs*(1-ng)*exp(-b1 * outer(xn, xn, '-')^2 )*exp(-b2*outer(zp, zp, '-')^2)+diag(ng,length(xn))
L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
T_so=L3_so%*%t(L3_so)
a1_so<- matrix(nrow = length(xn), ncol = length(zp))
for(i in 1:nrow(a1_so)){
  for(j in 1:ncol(a1_so)){
    a1_so[i,j]=T_so[hadata$wx[i],hadata$wx[j]]
  }
}

l1=c(1,0,0,0)
l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
T_st=L4_st%*%t(L4_st)

a1_st<- matrix(nrow = length(xn), ncol = length(zp))
for(i in 1:nrow(a1_st)){
  for(j in 1:ncol(a1_st)){
    a1_st[i,j]=T_st[hadata$wy[i],hadata$wy[j]]
  }
}
vb=a1_so*a1_st*a1
}
sigma=v_sigma+sigmal(vb)

#uig=ginv(sigmal(vb))%*%pa
#bg=ginv((ginv(va)+t(pa)%*%ginv(sigmal(vb))%*%pa))
#tg=t(pa)%*%ginv(sigmal(vb))
#VD=ginv(sigmal(vb))-uig%*%bg%*%tg

# Gauissan Part B points
hbdata=data.frame(xt,zt,wb1,wb2)
sigmatl<-function (vd)
  { a2=rs*(1-ng)*exp(-b1 * outer(xt, xt, '-')^2 )*exp(-b2*outer(zt, zt, '-')^2)+diag(ng,length(xt))
L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
T2_so=L3_so%*%t(L3_so)
a2_so<- matrix(nrow = length(xt), ncol = length(zt))
for(i in 1:nrow(a2_so)){
  for(j in 1:ncol(a2_so)){
    a2_so[i,j]=T2_so[hbdata$wb1[i],hbdata$wb1[j]]
    a2_so[j,i]=a2_so[i,j]
  }
}

l1=c(1,0,0,0)
l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
T_st=L4_st%*%t(L4_st)

a2_st<- matrix(nrow = length(xt), ncol = length(zt))
for(i in 1:nrow(a2_st)){
  for(j in 1:ncol(a2_st)){
    a2_st[i,j]=T_st[hbdata$wb2[i],hbdata$wb2[j]]
    a2_st[j,i]=a2_st[i,j]
  }
}
vb=a2_so*a2_st*a2
}
sigmat=v_sigmat+sigmatl(vd)

# Cov Part of BD

sigmanewl<-function (cvbd)
{ a3=rs*(1-ng)*exp(-b1 * outer(xt, xn, '-')^2 )*exp(-b2*outer(zt, zp, '-')^2)+diag(ng,length(xt),length(xn))
L3_so=matrix(c(1,cos(c12* pi / 180),cos(c13 * pi / 180),0,sin(c12* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),0,0,sin(c13* pi / 180)*sin(c23* pi / 180)),nrow=3,ncol=3)
T_so=L3_so%*%t(L3_so)
a3_so<- matrix(nrow = length(xt), ncol = length(xn))
for(i in 1:nrow(a3_so)){
  for(j in 1:ncol(a3_so)){
    a3_so[i,j]=T_so[hbdata$wb1[i],hadata$wx[j]]
  }
}


l1=c(1,0,0,0)
l2=c(cos(c12* pi / 180),sin(c12* pi / 180),0,0)
l3=c(cos(c13* pi / 180),sin(c13* pi / 180)*cos(c23* pi / 180),sin(c13* pi / 180)*sin(c23* pi / 180),0)
l4=c(cos(c14* pi / 180),sin(c14* pi / 180)*cos(c24* pi / 180),sin(c14* pi / 180)*sin(c24* pi / 180)*cos(c34*pi/180),sin(c14* pi / 180)*sin(c24* pi / 180)*sin(c34*pi/180))
L4_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
T_st=L4_st%*%t(L4_st)

a3_st<- matrix(nrow = length(zt), ncol = length(zp))
for(i in 1:nrow(a3_st)){
  for(j in 1:ncol(a3_st)){
    a3_st[i,j]=T_st[hbdata$wb2[i],hadata$wy[j]]
  }
}
cvbd=a3_so*a3_st*a3
}
sigma_new=v_sigmanew+sigmanewl(cvbd)
##BAYES UPDATE
library(MASS)
D=c(y2)
ED_B  <- EB + sigma_new %*% ginv(sigma) %*% (D - ED) 
VarD_B <- sigmat - sigma_new %*% ginv(sigma) %*% t(sigma_new)

#ED_B  <- EB + sigma_new %*% VD %*% (D - ED) 
#VarD_B <- sigmat - sigma_new %*% VD %*% t(sigma_new)

obs=1:63
plot(obs,yt,ylim=c(0,7))
lines(obs,ED_B,col=2)
lines(obs,ED_B+3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B-3*sqrt(diag(VarD_B)),col=3)
lines(obs,ED_B+2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B-2*sqrt(diag(VarD_B)),col=4)
lines(obs,ED_B+1*sqrt(diag(VarD_B)),col=5)
lines(obs,ED_B-1*sqrt(diag(VarD_B)),col=5)
legend(x="top", bty="n",lwd=2, lty=c(NA,1,1,1, 1),
       legend=c("Data", "mean New","90% cred.",
                "95%  cred.","99% cred."),
       col=c(1,2,3,4,5), ncol=3, pch=c(1,NA,NA,NA,NA))

# Trend plot for B
plot(obs,yt,ylim=c(-2,7),main = "Trend plot for B")
lines(obs,EB,col=2)
lines(obs,EB+3*sqrt(diag(v_sigmat)),col=3)
lines(obs,EB-3*sqrt(diag(v_sigmat)),col=3)
lines(obs,EB+2*sqrt(diag(v_sigmat)),col=4)
lines(obs,EB-2*sqrt(diag(v_sigmat)),col=4)
lines(obs,EB+1*sqrt(diag(v_sigmat)),col=5)
lines(obs,EB-1*sqrt(diag(v_sigmat)),col=5)
legend(x="top", bty="n",lwd=2, lty=c(NA,1,1,1, 1),
       legend=c("TESTData", "B points","90% cred.",
                "95%  cred.","99% cred."),
       col=c(1,2,3,4,5), ncol=3, pch=c(1,NA,NA,NA,NA))
# Trend plot for RESIDUAL
r=sqrt(diag(VarD_B))-sqrt(diag(v_sigmat))
plot(obs,yt-EB,ylim=c(-2,2),main="Trend plot for RESIDUAL")
lines(obs,ED_B-EB,col=2)
lines(obs,(ED_B-EB)+3*r,col=3)
lines(obs,(ED_B-EB)-3*r,col=3)
lines(obs,(ED_B-EB)+2*r,col=4)
lines(obs,(ED_B-EB)-2*r,col=4)
lines(obs,(ED_B-EB)+1*r,col=5)
lines(obs,(ED_B-EB)-1*r,col=5)
legend(x="top", bty="n",lwd=2, lty=c(NA,1,1,1, 1),
       legend=c("Test-B points", "ED[B]-B points","90% cred.",
                "95%  cred.","99% cred."),
       col=c(1,2,3,4,5), ncol=3, pch=c(1,NA,NA,NA,NA))


spe<-(yt-ED_B)/sqrt(diag(VarD_B))
spe=c(spe)
plot(obs,spe)
abline(h=2,col=2)
abline(h=-2,col=2)

res<-1-(diag(VarD_B)/diag(sigmat))
resolution=c(res)
plot(obs,resolution)



plot(obs,diag(VarD_B))
abline(h=0)

de=data.frame(obs,expectation=ED_B,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(de, aes(x=obs, y=expectation, shape=step,color=step)) +
  geom_point()

dvar=data.frame(obs,variance=diag(VarD_B),step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(dvar, aes(x=obs, y=variance, shape=step,color=step)) +
  geom_point()




spe=data.frame(obs,Standardprederror=spe,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(spe, aes(x=obs, y=Standardprederror, shape=step,color=step)) +
  geom_point()

res=data.frame(obs,Resolution=res,step=as.factor(c(rep(0, times=9),rep(1,times=9),rep(2,times=9))))
library(ggplot2)
ggplot(res, aes(x=obs, y=Resolution, shape=step,color=step)) +
  geom_point()


dis=((yt)-(ED_B))%*%(ginv(diag(VarD_B))%*%(as.matrix(yt)-ED_B))
plot(obs,dis)
rkd=rank(diag(VarD_B))
drd=dis/rkd
plot(obs,c(drd))


# Exchangeable Method
wx=c(sof,utr)
wy=c(dsf,stf)
wa=model.matrix(~wx+wy)
G=data.frame(1,xn,zp,wa[,2],wa[,3],wa[,4],wa[,5],wa[,6])
G=as.matrix(G)
hdata=data.frame(xn,zp,wx,wy)
hadata=data.frame(xn,zp,wx,wy)
nl <- function(par, Y) 
{
  b1 <-par[1]
  b2<-par[2]
  ng<-par[3]
  c12<-par[4]
  c13<-par[5]
  c23<-par[6]
  c14=par[7]
  c24=par[8]
  c34=par[9]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
   m1=c(1,10^c12,10^c13)
    m2=c(10^c12,1,10^c23)
    m3=c(10^c13,10^c23,1)
    T_so=t(matrix(c(m1,m2,m3),nrow=3,ncol=3))  
  H_so=T_so[hdata$wx,hdata$wx]
  l1=c(1,10^c12,10^c13,10^c14)
  l2=c(10^c12,1,10^c23,10^c24)
  l3=c(10^c13,10^c23,1,10^c34)
  l4=c(10^c14,10^c24,10^c34,1)
  T_st=t(matrix(c(l1,l2,l3,l4),nrow=4,ncol=4))
  a1_st=T_st[hdata$wy,hdata$wy]
  K=H_so*a1_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}
b1=seq(-5,5,by=1)
b2=seq(-5,5,by=1)
ng=0.05
c12=-.5
c13=-.5
c23=-.5
c14=c12
c24=c12
c34=c12
ma=as.matrix(expand.grid(b1,b2,ng,c12,c13,c23,c14,c24,c34))
for (i in 1:nrow(ma)){
  ma[[i]] <- nl(c(ma[i,]),Y=y2)
}
L=array(ma[,1],dim=c(length(b1),length(b2),length(c12),length(c13),length(c23),length(c14),length(c24),length(c34)))
z = matrix(unlist(L[,,1,1,1,1,1,1]), nrow =length(b1), ncol = length(b2))
filled.contour(x = c(b1), y = c(b2), z=z, xlab="beta1",ylab="beta2")
# optimization
beta1=-1.5
beta2=-2.5
ng=0.01
c12=seq(0.01,.49,by=.1)
c13=seq(0.01,.49,by=.1)
c23=seq(0.01,.49,by=.1)
c14=c12
c24=c12
c34=c12
df=as.matrix(expand.grid(c12,c13,c23,c14,c24,c34))
df1=df[1:7,]
df1=df[3:4,]
df3=df[5:6,]
df4=df[7:8,]
df1=df[19:21,]
for (i in 1:nrow(df1))
{
  df1[i,]<-optim(c(df1[i,]), nl, Y=y2, method = "L-BFGS-B", lower = c(-5,-5,0,0,0), upper = c(5,5,180,180,180))$par
}
optim(c(df[1,]), nl, Y=y2, method = "L-BFGS-B", lower = c(0,0,0,0,0,0), upper = c(1,1,1,1,1,1))$par
opts <- list( "algorithm" = "NLOPT_LN_NELDERMEAD", 
              "xtol_rel" = 1.0e-5, 
              "maxeval" = 200)
param.opt <- nloptr(c(-1.5,-1.5,0.03,-1,-1,-1), nl, Y=y2,  eval_grad_f = NULL
                    ,lb = c(-5,-3,0.01,-2,-2,-2), ub = c(5,-1,.1,-.1,-.1,-.1), 
                    opts=opts)

param.opt <- nloptr(c(-1.5,-2.5,0.03,-0.5,-0.5,-0.5), nl, Y=y2,  eval_grad_f = NULL
                                        ,lb = c(-5,-3,0.01,-2,-2,-2), ub = c(5,-1,.1,-.1,-.1,-.1), 
                                     opts=opts)

 param.opt <- nloptr(c(-1,-3,0.01,-1,-1,-1,-1,-1,-1), nl, Y=y2,  eval_grad_f = NULL
                                           ,lb = c(-5,-3,0.01,-2,-2,-2,-2,-2,-2), ub = c(5,-1,.1,-.1,-.1,-.1,-.1,-.1,-.1), 
                                         opts=opts)
# optim Mcmillan
nl <- function(par, Y) 
{
  b1 <--1.5
  b2<--1.5
  ng<-0.01
  so1<-par[1]
  so2<-par[2]
  so3<-par[3]
  st1<-par[4]
  st2<-par[5]
  st3<-par[6]
  st4<-par[7]
  na <- length(Y)
  R=(1-ng)*exp(-(10^as.numeric(b1))*outer(xn, xn,'-')^2 )*exp(-(10^as.numeric(b2))*outer(zp, zp,'-')^2)+diag(ng,na)
  
    m1=c(1,exp(-so1-so2),exp(-so1-so3))
    m2=c(exp(-so2-so1),1,exp(-so2-so3))
    m3=c(exp(-so3-so1),exp(-so3-so2),1)
    ma=matrix(c(m1,m2,m3),nrow=3,ncol=3)
    
    H_so<- matrix(nrow = length(xn), ncol = length(zp))
    
    for(i in 1:nrow(H_so)){
      for(j in i:ncol(H_so)){
        H_so[i,j]=ma[hdata$wx[i],hdata$wx[j]]
        H_so[j,i]=H_so[i,j]
      }
    }
    m4=c(1,exp(-st1-st2),exp(-st1-st3),exp(-st1-st4))
    m5=c(exp(-st2-st1),1,exp(-st2-st3),exp(-st2-st4))
    m6=c(exp(-st3-st1),exp(-st3-st2),1,exp(-st3-st4))
    m7=c(exp(-st4-st1),exp(-st4-st2),exp(-st4-st3),1)
    mb=matrix(c(m4,m5,m6,m7),nrow=4,ncol=4)
    H_st<- matrix(nrow = length(xn), ncol = length(zp))
    for(i in 1:nrow(H_st)){
      for(j in i:ncol(H_st)){
        H_st[i,j]=mb[hdata$wy[i],hdata$wy[j]]
        H_st[j,i]=H_st[i,j]
      }
    }
  K=H_so*H_st*R
  Ki <-ginv(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  B<-ginv(t(G)%*%Ki%*%G)%*%(t(G)%*%Ki%*%Y)
  si<-(1/na)%*%t(Y-G%*%B)%*%Ki%*%(Y-G%*%B)
  vb<-as.numeric(si)*(solve(t(G)%*%Ki%*%G))
  ll <- - (na/2)%*%log(si) - (1/2)%*%ldetK
  return(ll)
}

optim(c(6,7,8,5,6,7,8), nl, Y=y2, method = "L-BFGS-B", lower = c(1,1,1,1,1,01,01), upper = c(2,2,2,2,2,2,2))$par