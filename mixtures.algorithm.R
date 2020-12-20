################################################################################################################
## Function to generate the complete design matrix using the design points (will be called inside the main algo)
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

#######################################################################################
## THE MAIN ALGORITHM (2-tailed)(used for confidence interval)
w.two.sim<-function(x.design,B,k=3,alpha=0.05,l=10,f=10,seed=101){ # k=q= no. of dimensions21
  ####Intialization
  #x.design = design matrix without cross products
  #B= no. of simulations
  #k=q= defines a (q-1) dimensional simplex
  #l = index of no. of points on the simplex 
  #f= index of no. of pseudocomponents inside the original simplex
  ####
  start<-proc.time()
  set.seed(seed)
  ####### Computing the complete design matrix, variance-covariance matrix and Cholesky
  X<-full.mat(x.design)
  rows<-nrow(X)
  cols<-ncol(X)
  df = nrow(X)-ncol(X)
  XX<-t(X)%*%X
  invXX<-solve(XX, diag(x=1,nrow = cols,ncol = cols))
  g.mat<-t(chol(invXX))
  ###################################################
  ################Initializing the matrices of random numbers (Z, chi-sq)
  L<-as.matrix(seq(0,(1/k),length=f))
  ran.mat.1<-matrix(rnorm(cols*B),nrow = cols,ncol = B)
  ran.mat.2<-matrix(rchisq(B,df)/df,nrow = B, ncol=1)
  x.tri<-as.matrix(ridge.mix(k=k,l=l))
  sim.max<-NULL
  ###################################################
  #####The simulation loop (B=no. of simulations)
  for(j in 1:B){
    crit.temp<-0
    max.pivot.mat<-NULL
    ##### The loop that applies transformation to the points on the full simplex, to generate smaller and smaller simplexes inside the original simplex
    for(z in 1:f){
      x.mat<-NULL
      pivot.mat<-NULL
      x.mat<-as.matrix((x.tri*(1-(k*L[z])))+L[z]) # The Transformation
      x.mat<-full.mat(x.mat)
      r<-nrow(x.mat)
      ####### The loop to compute the pivotal quantity for each row of the design space
      for(i in 1:r){
        num<-abs(as.matrix(t(x.mat[i,]))%*%g.mat%*%ran.mat.1[,j])
        den<-sqrt(ran.mat.2[j,]*(as.matrix(t(x.mat[i,]))%*%invXX%*%as.matrix(x.mat[i,])))
        crit<-num/den
        ####### The IF condition to carry out the double maximizations 
        if(crit>crit.temp){
          crit.temp<-crit
        }
        else{
          next
        }
      }
    }
    #############################################
    sim.max<-rbind(sim.max,crit.temp)
  }
  max.sim.max<-max(sim.max)
  #########Sorting and Pulling off the alpha-percentile from the simulations
  max.sim.sort<-as.matrix(sort(sim.max))
  sim.percentile<-max.sim.sort[(1-(alpha/2))*B,1]
  #############################################3#
  elapsed<-proc.time()-start
  return(list(max = max.sim.max, sim.percentile =sim.percentile,time=elapsed))
}
################################################################################
###### The gist of the optimization is :-
##### (1) To find the max pivotal quantity from the design points of one simplex.
#### (2) Similarly, compute the max for all possible simplexes inside the original simplex and take the max out of all the maximums.
### (3) Repeat the above 2 steps B (no. of simulations) times and store the maximax quatities in a matrix
## (4) Sort and Pull of the alpha-percentile critical point from the matrix
# The IF Condition takes care of Step (1) and (2)


################################################################################
### The same algorithm but for one-sided confidence bounds (Insignificant changes)
w.one.sim<-function(x.design,B,k=3,alpha=0.05,l=10,f=10,seed=101){ # k=q= no. of dimensions21
  set.seed(seed)
  X<-full.mat(x.design)
  rows<-nrow(X)
  cols<-ncol(X)
  df = nrow(X)-ncol(X)
  XX<-t(X)%*%X
  invXX<-solve(XX, diag(x=1,nrow = cols,ncol = cols))
  g.mat<-t(chol(invXX))
  L<-as.matrix(seq(0,(1/k),length=f))
  ran.mat.1<-matrix(rnorm(cols*B),nrow = cols,ncol = B)
  ran.mat.2<-matrix(rchisq(B,df)/df,nrow = B, ncol=1)
  #browser()
  x.tri<-as.matrix(ridge.mix(k=k,l=l))
  sim.max<-NULL
  #browser()
  for(j in 1:B){
    crit.temp<--10
    max.pivot.mat<-NULL
    for(z in 1:f){
      x.mat<-NULL
      pivot.mat<-NULL
      x.mat<-as.matrix((x.tri*(1-(k*L[z])))+L[z])
      x.mat<-full.mat(x.mat)
      r<-nrow(x.mat)
      for(i in 1:r){
        num<-as.matrix(t(x.mat[i,]))%*%g.mat%*%ran.mat.1[,j]
        den<-sqrt(ran.mat.2[j,]*(as.matrix(t(x.mat[i,]))%*%invXX%*%as.matrix(x.mat[i,])))
        crit<-num/den
        if(crit>crit.temp){
          crit.temp<-crit
        }
        else{
          next
        }
      }
    }
    sim.max<-rbind(sim.max,crit.temp)
  }
  max.sim.max<-max(sim.max)
  max.sim.sort<-as.matrix(sort(sim.max))
  sim.percentile<-max.sim.sort[(1-alpha)*B,1]
  return(list(max = max.sim.max, sim.percentile =sim.percentile))
}

###############################################################################
  
ridge.mix(k=3,l=3)
w.two.sim(x.mat.f2.q3,B=500,k=3,alpha=0.05,l=10,f=10)

x.mat.f2.q3<-matrix(c(rep(c(1,0,0),2),rep(c(1/2,1/2,0),3),rep(c(0,1,0),2),rep(c(0,1/2,1/2),3),rep(c(0,0,1),2),rep(c(1/2,0,1/2),3)), nrow=15,ncol=3,byrow=T)
x.mat.f2.q5<-matrix(c(rep(c(1,0,0,0,0),2),rep(c(1/2,1/2,0,0,0),3),rep(c(0,1,0,0,0),2),rep(c(0,1/2,1/2,0,0),3),rep(c(0,0,1,0,0),2),rep(c(1/2,0,1/2,0,0),3),rep(c(0,0,0,1,0),2),rep(c(1/2,0,0,1/2,0),3),rep(c(0,1/2,0,1/2,0),3),rep(c(0,0,1/2,1/2,0),3), rep(c(0,0,0,0,1),2), rep(c(1/2,0,0,0,1/2),3), rep(c(0,1/2,0,0,1/2),3), rep(c(0,0,1/2,0,1/2),3), rep(c(0,0,0,1/2,1/2),3)), nrow=40,ncol=5,byrow=T)

x.mat.f2.q3.rep<-matrix(c(rep(c(1,0,0),5),rep(c(1/2,1/2,0),5),rep(c(0,1,0),5),rep(c(0,1/2,1/2),5),rep(c(0,0,1),5),rep(c(1/2,0,1/2),5)), nrow=30,ncol=3,byrow=T)

n<-nrow(full.mat(x.mat.f2.q3.rep))
p<-ncol(full.mat(x.mat.f2.q3.rep))
df<-n-p
# Our case
scheffe<-sqrt(p*qf(0.95,p,df)) 

repli<-2
p<-6
x.mat.q3<-matrix(c(rep(c(1,0,0),repli),rep(c(1/2,1/2,0),repli),rep(c(0,1,0),repli),rep(c(0,1/2,1/2),repli),rep(c(0,0,1),repli),rep(c(1/2,0,1/2),repli)), nrow=p*repli,ncol=3,byrow=T)
w.two.sim(x.mat.q3,B=80000,k=3,alpha=0.05,l=10,f=10)
#4.823055
