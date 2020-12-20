###########################################################################################################
## Function to generate all possible points inside the simplex region (will be called inside the main algo)
ridge.mix<-function(k=3,l=10){
  
  theta<-seq(0,0.5,length=l)
  
  #browser()
  x<-expand.grid(rep(list(theta),(k-2)))
  x<-cbind((1-(apply(x,1,sum))),x)
  x.mid<-x
  for(ii in 2:(k-1)){
    x.mat<-x
    x.mat[,1]<-x[,ii]
    x.mat[,ii]<-x[,1]
    x.mid<-rbind(x.mid,x.mat)
  }
  
  x.mid<-unique(x.mid)
  
  x.mid<-cbind(matrix(0,nrow(x.mid),1),x.mid)
  
  #browser()
  
  x.final<-x.mid
  
  for(ii in 2:(k)){
    x.mat1<-x.mid
    x.mat1[,1]<-x.mid[,ii]
    x.mat1[,ii]<-x.mid[,1]
    x.final<-rbind(x.final,x.mat1)
  }
  
  #browser()	
  return(unique(x.final))
  
}


mix.plot<-function(x,y,critical1,l=100){
  
  
  ###############################################
  #This function creates a ridge path for mixture designs assuming that the 
  #components are not restricted
  #x and y are the components and response respectively
  #l is the number of angles to use
  #f is the number of triangles to create
  #4/2011 RP: Courtesy of Dr. Robert Parody
  ###############################################
  
  k<-ncol(x)
  ref<-rep(1/k,3)
  N<-nrow(x)
  
  
  
  
  for(j in 1:k){
    for(m in 1:k){
      if(m>j){
        ll<-x[,j]*x[,m]
        x<-cbind(x,ll)
        
      }
    }
  }
  x<-as.matrix(x)
  xtx<-t(x)%*%x
  xtx.inv<-solve(t(x)%*%x)
  xty<-t(x)%*%y
  betahat<-xtx.inv%*%xty
  p<-length(betahat)
  H<-x%*%xtx.inv%*%t(x)
  I<-diag(1,N,N)
  SSE<-as.numeric(t(y)%*%(I-H)%*%y)
  dfe<-N-p
  MSE<-as.numeric(SSE/dfe)
  varbeta<-MSE*xtx.inv
  
  
  ref.mat<-t(as.matrix(ref))
  for(j in 1:k){
    for(m in 1:k){
      if(m>j){
        ll<-ref.mat[,j]*ref.mat[,m]
        ref.mat<-cbind(ref.mat,ll)
        
      }
    }
  }
  
  ref.mat<-cbind(ref.mat)
  #browser()
  
  yhat.ref<-(ref.mat)%*%betahat	
  
  
  
  
  
  trimage <- function(f){
    
    #x = y = seq(1,0,length=l) 
    x1 = seq(max(x[,1]),min(x[,1]),length=l)
    y = seq(max(x[,2]),min(x[,2]),length=l)
    t1 = length(x1) 
    im = aux = numeric(0) 
    for( i in seq( 1, t1, by = 2 ) ){
      
      
      idx = seq( i*t1, t1**2, by = t1 ) - (i-1)
      im = c(im, aux, idx, aux )
      aux = c(aux, NA)
    }
    
    z = outer(X=x1, Y=y, FUN=f) 
    return( matrix(z[im],nr=t1) ) 
  }
  
  
  
  
  f = function(x1,x2){
    
    
    x3<-1-x1-x2
    
    diff.x1<-x1-ref.mat[1]
    diff.x2<-x2-ref.mat[2]
    diff.x3<-x3-ref.mat[3]
    diff.x1x2<-x1*x2-ref.mat[4]
    diff.x1x3<-x1*x3-ref.mat[5]
    diff.x2x3<-x2*x3-ref.mat[6]
    
    x.mat<-cbind(diff.x1,diff.x2,diff.x3,diff.x1x2,diff.x1x3,diff.x2x3)
    #browser()
    yhat<-x.mat%*%betahat     
    #var.delta<-diag(diff.mat%*%varbeta%*%t(diff.mat))
    #out.mat<-cbind(delta,var.delta)
    
    return( yhat ) 
  }
  
  f1 = function(x1,x2){
    
    
    x3<-1-x1-x2
    diff.x1<-x1-ref.mat[1]
    diff.x2<-x2-ref.mat[2]
    diff.x3<-x3-ref.mat[3]
    diff.x1x2<-x1*x2-ref.mat[4]
    diff.x1x3<-x1*x3-ref.mat[5]
    diff.x2x3<-x2*x3-ref.mat[6]
    
    x.mat<-cbind(diff.x1,diff.x2,diff.x3,diff.x1x2,diff.x1x3,diff.x2x3)
    
    
    var.x<-diag(x.mat%*%varbeta%*%t(x.mat))
    
    
    return( var.x ) 
  }
  
  
  f2 = function(x1,x2){
    
    
    x3<-1-x1-x2
    diff.x1<-x1-ref.mat[1]
    diff.x2<-x2-ref.mat[2]
    diff.x3<-x3-ref.mat[3]
    diff.x1x2<-x1*x2-ref.mat[4]
    diff.x1x3<-x1*x3-ref.mat[5]
    diff.x2x3<-x2*x3-ref.mat[6]
    
    x.mat<-cbind(diff.x1,diff.x2,diff.x3,diff.x1x2,diff.x1x3,diff.x2x3)
    
    
    var.prof<-diag(x.mat%*%xtx.inv%*%t(x.mat))
    
    
    return( var.prof ) 
  }
  
  
  
  
  #browser()
  y.tri = trimage( f=f )
  var.yhat = trimage(f=f1)
  var.profile<-trimage(f=f2)
  critical<-critical1
  y.lower<-y.tri-sqrt(critical*var.yhat)
  y.upper<-y.tri+sqrt(critical*var.yhat)
  eps <- 0.05
  
  #browser()
  par(mfrow=c(1,2))
 
  plot(c(0-2*eps,1+2*eps), c(0-2*eps,1+2*eps), type="n", axes=F,xlab="", ylab="")
  polygon(c(0.5, 0, 1), c(1, 0, 0), density=0)
  text(c(0.5,0-eps,1+eps),c(1+2*eps,0.05,0.05),c(expression(x[1]),expression(x[2]),expression(x[3])),cex=1.5)
  text(c(0.5,0-eps,1+eps),c(1+eps,0,0),c(1,1,1),cex=1.25)
  contour(y.tri,nlevels=30,labcex=1,add=T)
  title("(a). Estimated Maximum Improvement")
  
  #browser()
  plot(c(0-2*eps,1+2*eps), c(0-2*eps,1+2*eps), type="n", axes=F,xlab="", ylab="")
  polygon(c(0.5, 0, 1), c(1, 0, 0), density=0)
  text(c(0.5,0-eps,1+eps),c(1+2*eps,0.05,0.05),c(expression(x[1]),expression(x[2]),expression(x[3])),cex=1.5)
  text(c(0.5,0-eps,1+eps),c(1+eps,0,0),c(1,1,1),cex=1.25)
  contour(y.upper,nlevels=30,labcex=1,add=T)
  title("(b). 95% Upper Confidence Bounds")
 
  
  
}

mix.dlcomp<-function(x,y,critical1,l=10,f=10){
  
  
  ###############################################
  #This function creates a ridge path for mixture designs assuming that the
  #components are not restricted
  #x and y are the components and response respectively
  #l is the number of angles to use
  #f is the number of triangles to create
  #4/2011 RP
  ##########################
  
  
  
  
  
  
  L.vec<-apply(x,2,min)
  c.vec<-apply(x,2,mean)
  N<-nrow(x)
  k<-ncol(x)          
  
  #pseudo.mat<-NULL
  #ref.mat<-NULL
  #for(i in 1:k){
  #            pseudo.x<-(x[,i]-L.vec[i])/(1-sum(L.vec))
  #            pseudo.ref<-(ref[i]-L.vec[i])/(1-sum(L.vec))
  #            pseudo.mat<-cbind(pseudo.mat,pseudo.x)
  #            ref.mat<-cbind(ref.mat,pseudo.ref)
  #}
  
  
  
  
  #browser()        
  
  ref.mat<-t(as.matrix(c.vec))
  for(j in 1:k){
    for(m in 1:k){
      if(m>j){
        ll<-ref.mat[,j]*ref.mat[,m]
        ref.mat<-cbind(ref.mat,ll)
        
      }
    }
  }
  
  #ref.mat<-cbind(ref.mat,ref.mat[,1]*ref.mat[,2]*ref.mat[,3])
  
  #browser()
  
  
  #x<-pseudo.mat
  
  #browser()        
  
  
  for(j in 1:k){
    for(m in 1:k){
      if(m>j){
        ll<-x[,j]*x[,m]
        x<-cbind(x,ll)
        
      }
    }
  }
  
  
  #x<-cbind(x,x[,1]*x[,2]*x[,3])
  
  #browser()
  x<-as.matrix(x)
  xtx<-t(x)%*%x
  xtx.inv<-solve(t(x)%*%x)
  #browser()
  xty<-t(x)%*%y
  betahat<-xtx.inv%*%xty
  p<-length(betahat)
  H<-x%*%xtx.inv%*%t(x)
  I<-diag(1,N,N)
  SSE<-as.numeric(t(y)%*%(I-H)%*%y)
  dfe<-N-p
  MSE<-as.numeric(SSE/dfe)
  varbeta<-MSE*xtx.inv
  
  
  #browser()
  
  
  
  
  
  fit.ref<-ref.mat%*%betahat
  
  
  
  
  
  
  L.mat<-NULL
  
  
  L1<-matrix(0,k,1)
  x.tri<-ridge.mix(k=k,l=l)
  xmax.mat<-NULL
  deltamax.mat<-NULL    
  ymax.mat<-NULL
  for(i in 1:k){
    
    
    L2<-as.matrix(seq(L.vec[i],c.vec[i],length=f))
    L.mat<-cbind(L.mat,L2)
    #browser()        
  }
  out.mat<-NULL
  for(ii in 1:f){
    x.mat<-NULL
    for(i in 1:k){
      
      x.int<-x.tri[,i]*(1-sum(L.mat[ii,]))+L.mat[ii,i]
      x.mat<-cbind(x.mat,x.int)                         
      
      
      #browser()
    }
    
    x.mat<-unique(x.mat)                 
    
    for(j in 1:k){
      for(m in 1:k){
        if(m>j){
          ll<-x.mat[,j]*x.mat[,m]
          x.mat<-cbind(x.mat,ll)
          
        }
      }
    }
    
    
    #x.mat<-cbind(x.mat,x.mat[,1]*x.mat[,2]*x.mat[,3])
    
    #browser()
    
    yhat<-(as.matrix(x.mat)%*%betahat)
    #browser()
    fit.mat<-matrix(fit.ref,nrow(yhat),1)
    deltahat<-yhat-fit.mat
    
    delta.max<-max(deltahat)
    y.max<-max(yhat)
    y.min<-min(yhat)
    select.est<-deltahat==max(deltahat)
    x.max<-x.mat[select.est,(1:k)]
    #browser()
    #}
    
    
    #browser()
    #colnames(x.max)<-paste("x",as.character(1:k),sep="")   
    
    out.mat<-rbind(out.mat,cbind(sum(L.mat[ii,]),y.max,x.max))
    
    deltamax.mat<-rbind(deltamax.mat,cbind(sum(L.mat[ii,]),delta.max))
    ymax.mat<-rbind(ymax.mat,cbind(sum(L.mat[ii,]),y.max))
    xmax.mat<-rbind(xmax.mat,x.max)                      
    
    
    
  }
  
  max.coded<-NULL
  for(j in 1:k){
    coded.q<-(xmax.mat[,j]-L.vec[j])/(1-sum(L.vec))
    max.coded<-cbind(max.coded,coded.q)
  }
  
  #browser()
  
  for(j in 1:k){
    for(m in 1:k){
      if(m>j){
        ll<-xmax.mat[,j]*xmax.mat[,m]
        xmax.mat<-cbind(xmax.mat,ll)
        
      }
    }
  }
  
  #xmax.mat<-cbind(xmax.mat,xmax.mat[,1]*xmax.mat[,2]*xmax.mat[,3])
  
  diff.mat<-NULL
  
  #browser()
  for(i in 1:p){
    
    test<-matrix(ref.mat[,i],nrow(xmax.mat),1)
    diff.x<-xmax.mat[,i]-test
    diff.mat<-cbind(diff.mat,diff.x)
  }
  xmax.mat<-as.matrix(xmax.mat)
  
  
  
  
  
  
  browser()        
  
  var.diff<-diff.mat%*%varbeta%*%t(diff.mat)
  var.y<-xmax.mat%*%varbeta%*%t(xmax.mat)
  #delta.lower<-deltamax.mat[,2]-sqrt(p*qf(0.95,df1=p,df2=dfe)*as.matrix(diag(var.diff)))
  #delta.upper<-deltamax.mat[,2]+sqrt(p*qf(0.95,df1=p,df2=dfe)*as.matrix(diag(var.diff)))
  critical<-critical1
  y.lower<-ymax.mat[,2]-critical*sqrt(as.matrix(diag(var.y)))
  y.upper<-ymax.mat[,2]+critical*sqrt(as.matrix(diag(var.y)))
  
  delta.lower<-deltamax.mat[,2]-critical*sqrt(as.matrix(diag(var.diff)))
  delta.upper<-deltamax.mat[,2]+critical*sqrt(as.matrix(diag(var.diff)))
  
  #max.y.lower<-max(y.lower)
  #max.delta.lower<-max(delta.lower)
  min.y.upper<-min(y.upper)
  min.delta.upper<-min(delta.upper)
  #browser()
  
  #colnames(deltamax.mat)<-c('L','Max Delta')
  #colnames(xmax.mat)<-paste("x",as.character(1:k),sep="")
  par(mfrow=c(1,2))
  
  matplot(1-deltamax.mat[,1],cbind(delta.lower,deltamax.mat[,2],delta.upper),type='l',xlab=expression(r[L]),ylab='Max Improvement',lwd=3,lty=c(1,2,1),col=1)
  abline(h=0)
  title('(a). Estimated Improvement and 95% Bounds')
  #browser()
  
  matplot(1-deltamax.mat[,1],xmax.mat[,1:k],xlab=expression(r[L]),ylab='Component Values',type='l',lwd=3,lty=c(1:k),col=c(2,3,4,5))
  leg<-c(expression(x[1]),expression(x[2]),expression(x[3]),expression(x[4]))
  legend(locator(1),leg,lty=c(1:k),lwd=3,col=c(2,3,4,5),xjust=.5,yjust=.5,cex=0.75)
  title('(b). Component Values Vs. Range')
  
  select.max<-deltamax.mat[,2]==max(deltamax.mat[,2])
  max.L<-out.mat[select.est,1]
  max.resp<-ymax.mat[select.est,2]
  max.x<-out.mat[select.est,]
  max.vec<-out.mat[select.est,]
  
  #browser() 
  
  return(list(max.L=max.L,max.resp=max.resp,max.x=max.x,max.vec=max.vec,min.y.upper=min.y.upper,min.delta.upper=min.delta.upper, y.min=y.min,yhat=yhat))
}


getwd()
setwd("/Volumes/Tj-files/Mixtures Thesis")
crit<-3.244005
artif<-read.csv("ArtificialSweetner.csv", header=T)
artif<-artif[-c(16:19),]
#matrix(c(rep(c(1,0,0),2),rep(c(1/2,1/2,0),3),rep(c(0,1,0),2),rep(c(0,1/2,1/2),3),rep(c(0,0,1),2),rep(c(1/2,0,1/2),3)), nrow=15,ncol=3,byrow=T)

# Visualization of Confidence bounds as contour plots superimposed over a simplex lattice
mix.plot(artif[,1:3],artif[,4],crit)


# Visualization of Estimated Improvement bounds
mix.dlcomp(artif[,1:3],artif[,4],critical1=crit,l=100,f=10)

# To obtain the confidence bounds of estimated improvement, run the following commands after reaching the browser()

########################################################################################################################
var.diff<-diff.mat%*%varbeta%*%t(diff.mat)
var.y<-xmax.mat%*%varbeta%*%t(xmax.mat)
#delta.lower<-deltamax.mat[,2]-sqrt(p*qf(0.95,df1=p,df2=dfe)*as.matrix(diag(var.diff)))
#delta.upper<-deltamax.mat[,2]+sqrt(p*qf(0.95,df1=p,df2=dfe)*as.matrix(diag(var.diff)))
critical<-critical1
y.lower<-ymax.mat[,2]-critical*sqrt(as.matrix(diag(var.y)))
y.upper<-ymax.mat[,2]+critical*sqrt(as.matrix(diag(var.y)))

delta.lower<-deltamax.mat[,2]-critical*sqrt(as.matrix(diag(var.diff)))
delta.upper<-deltamax.mat[,2]+critical*sqrt(as.matrix(diag(var.diff)))

#max.y.lower<-max(y.lower)
#max.delta.lower<-max(delta.lower)
min.y.upper<-min(y.upper)
min.delta.upper<-min(delta.upper)
#browser()

#colnames(deltamax.mat)<-c('L','Max Delta')
#colnames(xmax.mat)<-paste("x",as.character(1:k),sep="")
par(mfrow=c(1,2))

matplot(1-deltamax.mat[,1],cbind(delta.lower,deltamax.mat[,2],delta.upper),type='l',xlab=expression(r[L]),ylab='Max Improvement',lwd=3,lty=c(1,2,1),col=1)
abline(h=0)
title('(a). Estimated Improvement and 95% Bounds')
#browser()

matplot(1-deltamax.mat[,1],xmax.mat[,1:k],xlab=expression(r[L]),ylab='Component Values',type='l',lwd=3,lty=c(1:k),col=c(2,3,4,5))
leg<-c(expression(x[1]),expression(x[2]),expression(x[3]),expression(x[4]))
legend(locator(1),leg,lty=c(1:k),lwd=3,col=c(2,3,4,5),xjust=.5,yjust=.5,cex=0.75)
title('(b). Component Values Vs. Range')

select.max<-deltamax.mat[,2]==max(deltamax.mat[,2])
max.L<-out.mat[select.est,1]
max.resp<-ymax.mat[select.est,2]
max.x<-out.mat[select.est,]
max.vec<-out.mat[select.est,]
##################################################################################################################

#INSECT MSE ESTIMATION
View(insect)
attach(insect)
b1<-y[1]
b2<-y[2]
b3<-y[3]
b4<-y[4]
b34<-4*y[5]-2*(b3+b4)
b24<-4*y[6]-2*(b2+b4)
b23<-4*y[7]-2*(b2+b3)
b14<-4*y[8]-2*(b1+b4)
b13<-4*y[9]-2*(b1+b3)
b12<-4*y[10]-2*(b1+b2)

b<-c(b1,b2,b3,b4,b12,b13,b14,b23,b24,b34)

full.mat<-function(x){              
  x.binary<-NULL
  for(i in 1:ncol(x)){
    for(j in 1:ncol(x)){
      if(i<j){
        x.binary<-cbind(x.binary,x[,i]*x[,j])
      }
    }
  }
  X<-cbind(x,x.binary)
  return(X)
}

hat.y11<-x11%*%t(t(b))
hat.y12<-x12%*%t(t(b))
hat.y13<-x13%*%t(t(b))
hat.y14<-x14%*%t(t(b))
hat.y15<-x15%*%t(t(b))

hat.y<-c(hat.y11,hat.y12,hat.y13,hat.y14,hat.y15)
y<-c(45,6.2,5.8,23.2,2.6)

sse<-NULL
for(i in 1:length(y)){
  sse<-sum(sse,(as.vector(y[i])-as.vector(hat.y[i]))^2)
}
mse<-sse/4




getwd()
setwd("/Users/apple/desktop")
crit<-3.244005
artif<-read.csv("ArtificialSweetner.csv", header=T)
artif<-artif[-c(16:19),]

par(mfrow=c(1,1))
mix.plot(artif[,1:3],artif[,4],crit)

crit<-4.454334

mix.plot(artif[,1:3],artif[,4],crit)

#Fruit example
crit<-3.309117
fruit<-read.csv("fruit.csv", header=T)

mix.dlcomp(fruit[,1:4],fruit[,5],critical1=crit,l=100,f=10)

plot.new()
polygon(c(0.5, 0, 1), c(1, 0, 0), density=0)



