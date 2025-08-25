library(extRemes)

load("../Data/hourly data.RData")

nlag<-1

d$sy.min<-d$sy.min/10
d$sy.max<-d$sy.max/10
d$sm.min<-d$sm.min/10^2
d$sm.max<-d$sm.max/10^2

#d.train<-d[d$year %in% 1994:2023,]
#d.test<-d[d$year %in% 2024,]


d.train<-d[d$year %in% 1994:2002,]
d.test<-d[d$year %in% 2003:2023,]

n.train<-dim(d.train)[1]
n.test<-dim(d.test)[1]

r.ind.train<-(1+nlag):n.train
x.ind1.train<-r.ind.train-1

df.train<-data.frame(
  
  r=d.train$r[r.ind.train],
  
  x1=d.train$r[x.ind1.train],
  x2=d.train$sy.min[x.ind1.train],
  x3=d.train$sm.min[x.ind1.train],
  x4=d.train$sy.max[x.ind1.train],
  x5=d.train$sm.max[x.ind1.train]
  
)

mod1<-formula("~x1+x2+x3+x4+x5")
mod2<-formula("~x1+x2+x3+x4+x5")
mod3<-formula("~x1+x2+x3+x4+x5")



s1.gev=Sys.time()

m.train<-fevd(r,type="GEV",use.phi=T,
              verbose=T,data=df.train,
              location.fun=mod1,
              scale.fun=mod2,
              shape.fun=mod3,
              optim.args=list(control=list(maxit=3000)))

par<-m.train$results$par

np<-names(par)

par.loc<-par[grep("mu",np)]
par.log.sca<-par[grep("phi",np)]
par.sha<-par[grep("xi",np)]

r.ind.test<-(1+nlag):n.test
x.ind1.test<-r.ind.test-1

df.test<-as.matrix(data.frame(
  
  ones=rep(1,length(r.ind.test)),
  
  x1=d.test$r[x.ind1.test],
  x2=d.test$sy.min[x.ind1.test],
  x3=d.test$sm.min[x.ind1.test],
  x4=d.test$sy.max[x.ind1.test],
  x5=d.test$sm.max[x.ind1.test]
  
))

loc.test<-df.test %*% par.loc
sca.test<-exp(df.test %*% par.log.sca)
sha.test<-df.test %*% par.sha

save.image("results 1 hour gev 2003-2023 lag1.RData")


s2.gev=Sys.time()
s2.gev-s1.gev

#Time difference of 5.520393 mins



