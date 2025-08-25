source("functions.R")
load("../Data/ten-minutely data.RData")


nlags<-c(1) #2,3
r.thrs<-c(18, 42, 66, 90)


years.train<-1994:2002
years.test<-2003:2023

d$sy.min<-d$sy.min/10
d$sy.max<-d$sy.max/10

d$sm.min<-d$sm.min/10^2
d$sm.max<-d$sm.max/10^2

d.train<-d[d$year %in% years.train,]
d.test<-d[d$year %in% years.test,]

rm(d)

n.train<-dim(d.train)[1]
n.test<-dim(d.test)[1]



s1.glm=Sys.time()
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
      
      m<-glm(y.train~.,data=X.train,
             maxit=1000,trace=T,
             family=binomial())
      
      p.test<-expit(predict(m,as.data.frame(X.test)))
      
#      save.image(paste0("results ten-minute glm test ",min(years.test)," nlag ",nlag," thr ",r.thr,".RData"))
      save.image(paste0("results ten-minute glm 2003-2023 nlag ",nlag," thr ",r.thr,".RData"))
      
    }
    
  }
  
})


s2.glm=Sys.time()
s2.glm-s1.glm

#Time difference of 7.627317 mins



