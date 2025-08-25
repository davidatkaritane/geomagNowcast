library(dbarts)

load("../Data/ten-minutely data.RData")

nlags<-c(1) #c(1,2,3)
#r.thrs<-c(25,50,100)
r.thrs<-c(18, 42, 66, 90)

years.train<-1994:2002
years.test<-2003:2023


d$sy.min<-d$sy.min/10
d$sy.max<-d$sy.max/10
d$sm.min<-d$sm.min/10^2
d$sm.max<-d$sm.max/10^2

d.train<-d[d$year %in% years.train,]
d.test<-d[d$year %in% years.test,]

n.train<-dim(d.train)[1]
n.test<-dim(d.test)[1]


s1.bart=Sys.time()
system.time({
  
  for(nlag in nlags){
    
    for(r.thr in r.thrs){
      
      print(r.thr)
      
      indy<-(nlag+1):n.train
      
      y.train<-as.factor(as.numeric(d.train$r[indy]>r.thr))
      
      X.train<-data.frame(array(NA,c(n.train-nlag,0)))
      
      for(i in 1:nlag){
        
        indx<-indy-i
        
        Xadd<-cbind(
          
          d.train$r[indx],
          
          d.train$sy.min[indx],
          d.train$sm.min[indx],
          
          d.train$sy.max[indx],
          d.train$sm.max[indx]
          
        )
        
        colnames(Xadd)<-c(
          
          paste0("r.",i),
          
          paste0("x1.",i),
          paste0("x2.",i),
          paste0("x3.",i),
          paste0("x4.",i)
          
        )
        
        X.train<-cbind(X.train,Xadd)
        
      }
      
      indy<-(nlag+1):n.test
      
      y.test<-as.factor(as.numeric(d.test$r[indy]>r.thr))
      
      X.test<-data.frame(array(NA,c(n.test-nlag,0)))
      
      for(i in 1:nlag){
        
        indx<-indy-i
        
        Xadd<-cbind(
          
          d.test$r[indx],
          
          d.test$sy.min[indx],
          d.test$sm.min[indx],
          
          d.test$sy.max[indx],
          d.test$sm.max[indx]
          
        )
        
        colnames(Xadd)<-c(
          
          paste0("r.",i),
          
          paste0("x1.",i),
          paste0("x2.",i),
          paste0("x3.",i),
          paste0("x4.",i)
          
        )
        
        X.test<-cbind(X.test,Xadd)
        
      }
      
      m<-bart2(X.train,y.train,X.test,
               n.trees=50,
               n.samples=1000,
               n.burn=1000,
               n.chains=2, 
               n.threads=parallel::detectCores()-1,
               verbose=T)
      
      p.test<-fitted(m,sample="test")
      
      save.image(paste0("results ten-minute bart 2003-2023 nlag ",nlag," thr ",r.thr,".RData"))

    }
    rm(m)
    rm(p.test)
  }
  
})

s2.bart=Sys.time()
s2.bart-s1.bart

#Time difference of 3.697368 hours


