##### Hypergeometric distribution  ####
iter<-10000
List_defectItems<-c()
for (i in 1:iter){
  DefectItem<-sum(sample(rbinom(n=1000,size = 1,prob = 0.02),size = 100),replace=FALSE)
  List_defectItems<-c(List_defectItems,DefectItem)  
}
Total_defect<-data.frame(table(List_defectItems))
Total_defect$prob<-Total_defect$Freq/iter
Total_defect$List_defectItems<-as.character(Total_defect$List_defectItems)
#Prob(d>=4)
sum(Total_defect[which(Total_defect$List_defectItems >= 4),"prob"])

1-phyper(q=3, m=20, n=980, k=100, lower.tail = TRUE, log.p = FALSE)

##### Describing distribution ####  mix disrtribution
library(e1071)
set.seed(128437923)
meanList<-c()
varianceList<-c()
skewnessList<-c()
kurtosisList<-c()
propList<-c()
for(i in 1:10000){
  n=1000
  X1<-runif(n,min=1,max=8)
  X2<-runif(n,min=0,max=10)
  X<-c(sample(x = X1,size = 0.5*n,replace = FALSE),sample(x = X2,size = 0.5*n,replace = FALSE))
  #Lager6<-length(which(X>6))/n
  meanList<-c(meanList,mean(X))
  varianceList<-c(varianceList,var(X))
  skewnessList<-c(skewnessList,skewness(X))
  kurtosisList<-c(kurtosisList,kurtosis(X))
  propList<-c(propList,length(which(X>6))/n)
}

resultMomentDistribution<-data.frame("mean"=mean(meanList),
           "variacne"=mean(varianceList),
           "skew"=mean(skewnessList),
           "kurtosis"=mean(kurtosisList),
           "proportion>6"=mean(propList))
is.num <- sapply(resultMomentDistribution, is.numeric)
resultMomentDistribution[is.num] <- lapply(resultMomentDistribution[is.num], round, 4)



library(psych)
describe(meanList)
# Kurtosis 3 means normal
plot(density(meanList), main = "Distribution", xlab = "Sample mean",col="orange")


## Censored data ###
MeanSurvival<-function(n,rate){
  CensoredData<-rexp(n, rate )
  CensoredData<-ifelse(CensoredData>=3,3,CensoredData)
  return(mean(CensoredData))
}
MeanDistribution<-replicate(n=10000,MeanSurvival(n = 50,rate = 2/3))
plot(density(MeanDistribution), main = "Distribution of mean survival time", xlab = "Sample mean",col="orange")
mean(MeanDistribution)



