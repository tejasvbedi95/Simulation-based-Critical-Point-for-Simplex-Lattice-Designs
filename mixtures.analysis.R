##########################################################################
# rel.eff() computes the relative efficiency of using the simulation based strategy over the Scheffe Adaptation
# sch = scheffe critical point
# w = simulation - based critical point



rel.eff<-function(sch,w){
  ef<-(((sch^2)/(w^2))-1)*100
  return(ef)
}


#k=3 ,r=2
repli<-2
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
#4.823055

n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.909191
re32<-rel.eff(5.909191,4.823055)

#k=3 ,r=3
repli<-3
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
#3.915832
n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.729667
re33<-rel.eff(4.729667,3.915832)

#k=3 ,r=4
repli<-4
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
# 3.631931
n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.396077
re34<-rel.eff(4.396077,3.631931)

#k=3 ,r=5
repli<-5
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
# 3.523028
n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.238811
re35<-rel.eff(4.238811,3.523028)

#k=3 ,r=7
repli<-7
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
#3.41062
n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.087457
re37<-rel.eff(4.087457,3.41062)

#k=3 ,r=20
repli<-20
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
#3.24549
n<-nrow(full.mat(x.mat.q3))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#3.889349
re3inf<-rel.eff(3.889349,3.24549)

#k=4 ,r=2
repli<-2
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#4.559861
n<-nrow(full.mat(x.mat.q4))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#6.09655
re42<-rel.eff(6.09655,4.559861)

#k=4 ,r=3
repli<-3
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#3.976215
n<-nrow(full.mat(x.mat.q4))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.266566
re43<-rel.eff(5.266566,3.976215)

#k=4 ,r=4
repli<-4
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#3.81117
n<-nrow(full.mat(x.mat.q4))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.011179
re44<-rel.eff(5.011179,3.81117)


#k=4 ,r=5
repli<-5
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#3.712745
n<-nrow(full.mat(x.mat.q4))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.886882
re45<-rel.eff(4.886882,3.712745)

#k=4 ,r=7
repli<-7
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#3.643889
n<-nrow(x.mat.q4)
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.76466
re47<-rel.eff(4.76466,3.643889)


#k=4 ,r=20
repli<-20
p<-4+choose(4,2)
x.mat.q4<-matrix(c(rep(c(1,0,0,0),repli),rep(c(1/2,1/2,0,0),repli),rep(c(0,0,1/2,1/2),repli),rep(c(1/2,0,0,1/2),repli),rep(c(0,1,0,0),repli),rep(c(0,1/2,1/2,0),repli),rep(c(0,0,1,0),repli),rep(c(1/2,0,1/2,0),repli),rep(c(0,1/2,0,1/2),repli),rep(c(0,0,0,1),repli)), nrow=p*repli,ncol=4,byrow=T)
w.two.sim(x.mat.q4,B=80000,k=4,alpha=0.05,l=10,f=10)
#3.533874
n<-nrow(full.mat(x.mat.q4))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.600498
re4inf<-rel.eff(4.600498,3.533874)

#k=5 ,r=2
repli<-2
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=1000,k=5,alpha=0.05,l=7,f=7)
#4.540327
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#6.552205
re52<-rel.eff(6.552205,4.540327)

#k=5 ,r=3
repli<-3
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=80000,k=5,alpha=0.05,l=7,f=7)
#4.186878
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.882798
re53<-rel.eff(5.882798,4.186878)

#k=5 ,r=4
repli<-4
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=80000,k=5,alpha=0.05,l=7,f=7)
#4.050028
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.667283
re54<-rel.eff(5.667283,4.050028)

#k=5 ,r=5
repli<-5
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=80000,k=5,alpha=0.05,l=7,f=7)
#3.993118
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.560542
re55<-rel.eff(5.560542,3.993118)

#k=5 ,r=7
repli<-7
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=80000,k=5,alpha=0.05,l=7,f=7)
#3.915451
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.454328
re57<-rel.eff(5.454328,3.915451)

#k=5 ,r=20
repli<-20
p<-5+choose(5,2)
x.mat.f2.q5.repli<-matrix(c(rep(c(1,0,0,0,0),repli),rep(c(1/2,1/2,0,0,0),repli),rep(c(0,1,0,0,0),repli),rep(c(0,1/2,1/2,0,0),repli),rep(c(0,0,1,0,0),repli),rep(c(1/2,0,1/2,0,0),repli),rep(c(0,0,0,1,0),repli),rep(c(1/2,0,0,1/2,0),repli),rep(c(0,1/2,0,1/2,0),repli),rep(c(0,0,1/2,1/2,0),repli), rep(c(0,0,0,0,1),repli), rep(c(1/2,0,0,0,1/2),repli), rep(c(0,1/2,0,0,1/2),repli), rep(c(0,0,1/2,0,1/2),repli), rep(c(0,0,0,1/2,1/2),repli)), nrow=p*repli,ncol=5,byrow=T)
w.two.sim(x.mat.f2.q5.repli,B=80000,k=5,alpha=0.05,l=7,f=7)
#3.828918
n<-nrow(full.mat(x.mat.f2.q5.repli))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#5.309594
re5inf<-rel.eff(5.309594,3.828918)


w.dfr<-data.frame(re3=c(re32,re33,re34,re35,re37,re3inf),re4=c(re42,re43,re44,re45,re47,re4inf),re5=c(re52,re53,re54,re55,re57,re5inf))
colnames(w.dfr)<-c("q=3","q=4")
rownames(w.dfr)<-c("r=2","r=3","r=4","r=5","r=7","r=inf")
round(w.dfr,3)


#Artificial Sweetner
p<-6
artif<-matrix(c(rep(c(1,0,0),2),rep(c(1/2,1/2,0),3),rep(c(0,1,0),2),rep(c(0,1/2,1/2),3),rep(c(0,0,1),2),rep(c(1/2,0,1/2),3)), nrow=15,ncol=3,byrow=T)
w.arti<-w.one.sim(artif,B=100000,k=3,alpha=0.05,l=15,f=15)
#3.244005
3.244^2
n<-nrow(full.mat(artif))
df<-n-p
scheffe<-sqrt(p*qf(0.975,p,df)) 
#4.499169
4.499169^2
rel.eff(scheffe,w.arti$sim.percentile)
#92.35%

#Fruit Data
#getwd()
#setwd("/Users/apple/desktop")
p<-4+choose(4,2)
fruit.full<-read.csv("fruit.csv",header=T)
fruit<-fruit.full
fruit.x<-fruit[,1:4]
n<-nrow(fruit.x)
fruit.y<-fruit[,5]
w.one.sim(as.matrix(fruit.x),B=100000,k=4)
#3.309117
3.309117^2
df<-n-p
# Our case
scheffe<-sqrt(p*qf(0.95,p,df)) 
5.266566^2
rel.eff(scheffe,3.309117 )
#153.2974


