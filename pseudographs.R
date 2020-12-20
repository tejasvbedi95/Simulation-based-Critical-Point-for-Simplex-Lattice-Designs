library(mixexp)
x1<-c(1,0,0,0.5,0.5,0)
x2<-c(0,1,0,0.5,0,0.5)
x3<-c(0,0,1,0,0.5,0.5)
des.points<-data.frame(x1, x2, x3)
x1<-c(1,0,0,0.5,0.5,0,1/3)
x2<-c(0,1,0,0.5,0,0.5,1/3)
x3<-c(0,0,1,0,0.5,0.5,1/3)

#MixturePlot(x3,x2,x1, lims =c(rep(0.2,6)),contrs=F,w=c(11,12.4,15,14.8,16.1,8.8), mod=2, corner.labs = c("x3", "x2", "x1"))
DesignPoints(des.points)
#MixturePlot(x3,x2,x1, lims =c(rep(0.2,6)),contrs=F,w=c(11,12.4,15,14.8,16.1,8.8), mod=2, corner.labs = c("x3", "x2", "x1"), x3lab="x3=0.5", x2lab="x2=0.5", x1lab="x1=0.5")
DesignPoints(des.points)

x.mat.f2<-matrix(c(rep(c(1,0,0,0),2),rep(c(1/2,1/2,0,0),3),rep(c(0,1,0,0),2),rep(c(0,1/2,1/2,0),3),rep(c(0,0,1,0),2),rep(c(1/2,0,1/2,0),3),rep(c(0,0,0,1),2),rep(c(1/2,0,0,1/2),3),rep(c(0,1/2,0,1/2),3),rep(c(0,0,1/2,1/2),3)), nrow=26,ncol=4,byrow=T)
y.mat<-matrix(c(11,12.4,15,14.8,16.1,8.8,10,10,9.7,11.8,16.8,16,17.7,16.4,16.6,12,13,10,11,12,9,10,11,7,8,9),nrow=26,ncol=1,byrow=T)

des<-SLD(3,2)

Xvert(nfac=3, lc=c(0.2,0.2,0.4), uc=c(1,1,1), ndm =0, pseudo=F, cornerlabs =c(expression("x"[3]),expression("x"[2]),expression("x"[1])), axislabs=c("x1","x2","x3") )
par(mfrow=c(1,2))
Xvert(nfac=3, uc=c(0.4,0.3,0.6), lc=c(0,0,0), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3") )
Xvert(nfac=3, uc=c(0.4,0.7,0.6), lc=c(0,0,0), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3") )

Xvert(nfac=3, lc=c(0.5,0,0.15), uc=c(1,1,1), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3"))
Xvert(nfac=3, lc=c(0,0,0), uc=c(0.85,0.25,0.3), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3"))
Xvert(nfac=3, lc=c(0.5,0,0.15), uc=c(0.85,0.25,0.3), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3") )

#Xvert(nfac=3, lc=c(0,0,0), uc=c(1,1,1), ndm =0, pseudo=F,  cornerlabs =c("x3","x2","x1"), axislabs=c("x1","x2","x3") )
