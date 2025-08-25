expit<-function(x){ 1/(1+exp(-x)) }

retb<-function(eta,cov.eta,rp,nrp,B){
  
  rb<-array(NA,c(nrp,B))
  
  etab<-rmvn(B,eta,cov.eta)
  
  lob<-etab[,1]
  scb<-exp(etab[,2])
  shb<-etab[,3]
  
  for(i in 1:B){
    rb[,i]<-rlevd(rp,lob[i],scb[i],shb[i],type="GEV")
  }
  
  rb
  
}

###

qq<-function(n){
  
  require(evd)
  
  pord<-function(q,p,n,j){
    p-porder(q,distn="norm",mlen=n,mean=0,sd=1,j=j,largest=F)
  }
  
  # calculate interval in uniroot based on quantiles
  # for largest order statistic for n N(0,1) rv's
  # symmetry of N(0,1) means those for smallest order
  # statistic are mirror image of those for largest
  
  inc<-1.5
  p<-c(0.025,0.975)
  
  int<-inc*max(abs(qnorm(p^(1/n))))*c(-1,1)
  
  med<-vector()
  low<-vector()
  upp<-vector()
  
  for(j in 1:n){
    
    if(j%%10^2==0){ print(j) }
    
    med[j]<-uniroot(pord,int,p=0.5,n=n,j=j)$root
    low[j]<-uniroot(pord,int,p=0.025,n=n,j=j)$root
    upp[j]<-uniroot(pord,int,p=0.975,n=n,j=j)$root
    
  }
  
  list(med=med,low=low,upp=upp)
  
}

###

qq.unif<-function(n,nsim=10^4){
  
  u<-array(runif(nsim*n),c(nsim,n))
  
  u<-t(apply(u,1,sort))
  
  med<-apply(u,2,quantile,probs=0.5)
  low<-apply(u,2,quantile,probs=0.025)
  upp<-apply(u,2,quantile,probs=0.975)
  
  list(med=med,low=low,upp=upp)
  
}

###

acf.ci<-function(n,lag.max){
  
  # see "Testing using Student's t-distribution"
  # en.wikipedia.org/wiki/Pearson_correlation_coefficient
  # and p564 Volume 2A Kendall's Theory of Advanced Statistics
  
  ns<-n-(0:lag.max)
  df<-ns-2
  
  t.low<-qt(0.025,df)
  t.upp<-qt(0.975,df)
  
  low<-t.low/sqrt(df+t.low^2)
  upp<-t.upp/sqrt(df+t.upp^2)
  
  list(low=low,upp=upp)
  
}

###

pitres.gev<-function(m,y){
  
  betas<-fitted(m)
  n<-dim(betas)[1]
  
  res<-vector()
  
  for(i in 1:n){
    
    loc<-betas[i,1]
    scale<-exp(betas[i,2])
    shape<-betas[i,3]
    
    res[i]<-qnorm(pevd(y[i],loc,scale,shape,type="GEV"))
    
  }
  
  res
  
}

###

pred<-function(m,n){
  
  fit<-fitted(m)
  est<-low<-upp<-vector()
  
  for(i in 1:n){
    
    lo<-fit[i,1]
    sc<-exp(fit[i,2])
    sh<-fit[i,3]
    
    est[i]<-qevd(0.5,lo,sc,sh,type="GEV")
    low[i]<-qevd(0.025,lo,sc,sh,type="GEV")
    upp[i]<-qevd(0.975,lo,sc,sh,type="GEV")
    
  }
  
  list(est=est,low=low,upp=upp)
  
}

perform<-function(y,p,p.thr){
  
  n<-length(p.thr)
  tpr<-tnr<-vector()
  
  for(i in 1:n){
    
    pp<-as.numeric(p>=p.thr[i])
    
    tpr[i]<-mean(pp[y==1]==1)
    tnr[i]<-mean(pp[y==0]==0)
    
  }
  
  data.frame(p.thr,tpr,tnr)
  
}

find.p<-function(pz,qz,z.thr){
  
  ind<-z.thr<=qz
  
  ind1<-max(ind)==0
  ind2<-min(ind)==1
  
  if(ind1){ p<-0 }
  if(ind2){ p<-1 }
  
  if((!ind1)&(!ind2)){
    ind<-which(diff(ind)==1)
    p<-1-mean(pz[ind:(ind+1)])  
  }
  
  p
  
}

# qq.old<-function(res){
#   
#   pord<-function(q,p,n,j){
#     p-porder(q,distn="norm",mlen=n,mean=0,sd=1,j=j,largest=F)
#   }
#   
#   n<-length(res)
#   
#   med<-vector()
#   low<-vector()
#   upp<-vector()
#   
#   mult<-10
#   
#   for(j in 1:n){
#     
#     if(j%%10^2==0){ print(j) }
#     
#     int.med<-10*c(-1,1)
#     int.low<-10*c(-1,0)
#     int.upp<-10*c(0,1)
#     
#     check<-0
#     while(check==0){
#       tryCatch(        {
#         med[j]<-uniroot(pord,int.med,p=0.5,n=n,j=j)$root
#         check<-1
#       },
#       error = function(e) { int.med<-int.med*mult }
#       )
#     }
#     
#     check<-0
#     while(check==0){
#       tryCatch(        {
#         low[j]<-uniroot(pord,int.med,p=0.025,n=n,j=j)$root
#         check<-1
#       },
#       error = function(e) { int.med<-int.med*mult }
#       )
#     }
#     
#     check<-0
#     while(check==0){
#       tryCatch(        {
#         upp[j]<-uniroot(pord,int.med,p=0.975,n=n,j=j)$root
#         check<-1
#       },
#       error = function(e) { int.med<-int.med*mult }
#       )
#     }
#     
#   }
#   
#   list(med=med,low=low,upp=upp)
#   
# }