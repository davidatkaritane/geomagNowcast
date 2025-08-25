library(ROCR)

postscript("ROC_1hour_2003-2023_3models.eps",paper="special",width=7.5*1.15/1.2/3*4,
           height=2.5*1.15/1.2*3,
           onefile = TRUE, horizontal = FALSE)

par(mfrow=c(3,4),mar=c(4.5,4.5,1.5,1))



setwd("GLM")

workspaces<-c(
"results 1 hour glm 2003-2023 nlag 1 thr 18.RData",
"results 1 hour glm 2003-2023 nlag 1 thr 42.RData",
"results 1 hour glm 2003-2023 nlag 1 thr 66.RData",
"results 1 hour glm 2003-2023 nlag 1 thr 90.RData")


p.thr<-seq(0,1,length.out=10^4+1)

legd13 <- c("(a)","(b)","(c)","(d)")
j=0
for(k in 1:length(workspaces)){
  
  load(workspaces[k])
  
  pred<-prediction(as.numeric(p.test),y.test)
  auc<-round(as.numeric(performance(pred,"auc")@y.values),3)
  
  per<-performance(pred,"tpr","fpr")
  plot(per,
       main=paste0("max(R1) > ",r.thr,
                   " (AUC = ",auc,")"),
       xlab="probability of a false positive",
       ylab="probability of a true positive",
       lwd=2,col="blue",cex.main=0.9)
  j=j+1
  legend("bottomright",legd13[j], bty = "n")
  grid()
  
}






setwd("../BART")

works<-c(
  "results 1 hour bart 2003-2023 nlag 1 thr 18.RData",
  "results 1 hour bart 2003-2023 nlag 1 thr 42.RData",
  "results 1 hour bart 2003-2023 nlag 1 thr 66.RData",
  "results 1 hour bart 2003-2023 nlag 1 thr 90.RData")

r.thrs<-c(18, 42, 66, 90)
p.thr<-seq(0,1,length.out=10^4+1)
legd46 <- c("(e)","(f)","(g)","(h)")

j=0
for(k in 1:length(works)){
  
  load(works[k])
  
  np<-sum(y.test==1)
  
  pred<-prediction(as.numeric(p.test),y.test)
  auc<-round(as.numeric(performance(pred,"auc")@y.values),3)
  
  per<-performance(pred,"tpr","fpr")
  plot(per,
       main=paste0("max(R1) > ",r.thr,
                   " (np = ",np,", AUC = ",auc,")"),
       xlab="probability of false positive",
       ylab="probability of true positive",
       lwd=2,col="blue",cex.main=0.9)
  j=j+1
  legend("bottomright",legd46[j], bty = "n")
  grid()
  
}





setwd("../GEV")


library(extRemes)

load("results 1 hour gev 2003-2023 lag1.RData")


r.ind.test<-(1+nlag):n.test
r <- d.test$r[r.ind.test]
  

r.thrs<-c(18, 42, 66, 90)
n.thrs<-length(r.thrs)

nxs<-10^4+1
xs<-seq(0,1,length.out=nxs)

p<-vector()
legd79 <- c("(i)","(j)","(k)","(l)")

for(k in 1:(n.thrs)){
  
  r.thr<-r.thrs[k]
  
  rp<-as.numeric(r>=r.thr)
  np<-sum(rp)
  
  for(h in 1:length(r)){
    p[h]<-1-pevd(r.thr,type="GEV",
                 loc.test[h],sca.test[h],sha.test[h])
  }
  
  pred<-prediction(p,rp)
  auc<-round(as.numeric(performance(pred,"auc")@y.values),3)
  
  per<-performance(pred,"tpr","fpr")
  plot(per,
       main=paste0("max(R1) > ",r.thr,
                   " (np = ",np,", AUC = ",auc,")"),
       xlab="probability of false positive",
       ylab="probability of true positive",
       lwd=2,col="blue",cex.main=0.9)
  legend("bottomright",legd79[k], bty = "n")
  grid()
  
}

dev.off()






