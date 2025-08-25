source("functions.R")
workspaces<-c(
  "results ten-minute glm 2024 nlag 1 thr 18.RData",
  "results ten-minute glm 2024 nlag 1 thr 42.RData",
  "results ten-minute glm 2024 nlag 1 thr 66.RData",
  "results ten-minute glm 2024 nlag 1 thr 90.RData",
  "results ten-minute glm 2024 nlag 1 thr 150.RData")

param.ci <- matrix(NA,nrow=5*5,ncol=4)
for(k in 1:length(workspaces)){
  load(workspaces[k])
  temp <- confint.default(m)
  param.ci[((k-1)*5+1):(k*5),] <- cbind(coef(m)[2:6],exp(coef(m)[2:6]),exp(temp[2:6,]))
}

options("scipen"=100, "digits"=3)  
param.ci







