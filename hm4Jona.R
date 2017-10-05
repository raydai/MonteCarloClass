#install.packages("zoo")
library("zoo")
library("MASS")
# Generate dataset
CompleteData <- function(n,ObsTime=4,rho,CorStructure) {
  if (CorStructure=="auto"){  #autoregressive
    Corr<-matrix(data =c(1,rho,rho^2,rho^3,rho,1,rho,rho^2,rho^2,rho,1,rho,rho^3,rho^2,rho,1),nrow = ObsTime)}  
  if ((CorStructure=="exch")){ #exchangable
    Corr<-matrix(data =c(1,rho,rho,rho,rho,1,rho,rho,rho,rho,1,rho,rho,rho,rho,1),nrow = ObsTime)}
  mean=rep(0,ObsTime)
  CompleteData<-mvrnorm(n,mean,Sigma = Corr,empirical=FALSE)
  #print(CompleteData)
  return(CompleteData)
  }
# Geberate Missing Data set
RemoveDataPoint <- function(Data,ObsTime=4,n,MissingProp,MARRate,MissType) {
  X<-Data
  if (MissType=="MCAR"){
    X[sample(x=seq(from=n+1,to = n*ObsTime),size = n*ObsTime*MissingProp,replace = FALSE)]<-NA
  }
  if (MissType=="MAR"){
    # TotalMissing<-n*ObsTime*MissingProp
    SampleList<-c(sample(x=seq(from=n+1,to = 2*n),size = ceiling(n*MARRate[1]) ,replace = FALSE),
                  sample(x=seq(from=2*n+1,to = 3*n),size = ceiling(n*MARRate[2]),replace = FALSE),
                  sample(x=seq(from=3*n+1,to = ObsTime*n),size = ceiling(n*MARRate[3]),replace = FALSE)) 
    X[SampleList]<-NA}
  return(X)
}
# LOCF imputation
LOCF<-function(X) return(t(na.locf(t(X))))

##############################################################################################################

full_p <- 0
for(i in 1:5000){
FullData_Auto0.5<-CompleteData(n=50,ObsTime=4,rho=0.5,CorStructure="auto")
#Auto 0.5 MCAR 5%
Auto0.5_MCAR5<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MCAR",MissingProp=0.05)
Auto0.5_MCAR10<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MCAR",MissingProp=0.1)
Auto0.5_MCAR30<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MCAR",MissingProp=0.3)

Auto0.5_MCAR5_LOCF<-LOCF(Auto0.5_MCAR5)
Auto0.5_MCAR10_LOCF<-LOCF(Auto0.5_MCAR10)
Auto0.5_MCAR30_LOCF<-LOCF(Auto0.5_MCAR30)


Auto0.5_MAR5<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MAR",MARRate = c(0.01,0.01,0.03))
Auto0.5_MAR5_LOCF<-LOCF(Auto0.5_MAR5)

Auto0.5_MAR10<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MAR",MARRate = c(0.01,0.03,0.06))
Auto0.5_MAR10_LOCF<-LOCF(Auto0.5_MAR10)


Auto0.5_MAR30<-RemoveDataPoint(Data =FullData_Auto0.5,n=50,MissType = "MAR",MARRate = c(0.05,0.1,0.15))
Auto0.5_MAR30_LOCF<-LOCF(Auto0.5_MAR30)

FullData_Exch0.2<-CompleteData(n=100,ObsTime=4,rho=0.2,CorStructure="exch")
# Exch 0.5 MCAR 5%
Exch0.2_MCAR5<-RemoveDataPoint(Data =FullData_Exch0.2,n=100,MissType = "MCAR",MissingProp=0.05)
Exch0.2_MCAR5_LOCF<-LOCF(Exch0.2_MCAR5)

Exch0.2_MCAR10<-RemoveDataPoint(Data =FullData_Exch0.2,n=100,MissType = "MCAR",MissingProp=0.1)
Exch0.2_MCAR10_LOCF<-LOCF(Exch0.2_MCAR10)



Exch0.2_MCAR30<-RemoveDataPoint(Data =FullData_Exch0.2,n=100,MissType = "MCAR",MissingProp=0.3)
Exch0.2_MCAR30_LOCF<-LOCF(Exch0.2_MCAR30)

trt <- c(rep(0,100),rep(1,100))

id <- c(1:50)

id <- as.factor(id)


#####Change to be dataset of choice: ex/Auto0.5_MAR30_LOCF, Exch0.2_MCAR30, etc
CompleteAuto0.5MCAR <- cbind(factor(id), #####Auto0.5_MAR5_LOCF#####,trt)

CompleteAuto0.5MCAR <- as.data.frame(CompleteAuto0.5MCAR)
CompleteAuto0.5MCAR$V1 <- as.factor(CompleteAuto0.5MCAR$V1)

colnames(CompleteAuto0.5MCAR) <- c("id","time1","time2","time3","time4","trt")

CompleteAuto0.5MCARLong <- reshape(CompleteAuto0.5MCAR,varying=c("time1","time2","time3","time4"),direction="long",idvar="V1",sep="")
CompleteAuto0.5MCARLong <- data.frame(CompleteAuto0.5MCARLong$V1, CompleteAuto0.5MCARLong$time, CompleteAuto0.5MCARLong$V)
CompleteAuto0.5MCARLong <- CompleteAuto0.5MCARLong[order(CompleteAuto0.5MCARLong$CompleteAuto0.5MCARLong.V1),]
CompleteAuto0.5MCARLong <- data.frame(CompleteAuto0.5MCARLong$CompleteAuto0.5MCARLong.V1, CompleteAuto0.5MCARLong$CompleteAuto0.5MCARLong.time)
CompleteAuto0.5MCARLong$trt <- trt
time <- rep(seq(1:4),rep(50))
CompleteAuto0.5MCARLong$time <- time
colnames(CompleteAuto0.5MCARLong) <- c("id","outcome","trt","time")
CompleteAuto0.5MCARLong <- within(CompleteAuto0.5MCARLong,{
	trt <- factor(trt)
	time <- factor(time)
	id <- factor(id)
})


true.aov <- aov(outcome~trt*time+Error(id),data= CompleteAuto0.5MCARLong)

summary(true.aov)

true_p <- as.numeric(unlist(summary(true.aov))["Error: Within.Pr(>F)2"])

full_p[i] <- true_p
}

length(which(full_p < 0.05))







################################################################
################################################################








#m <- Auto0.5_MCAR5

#a <- which(is.na(m),arr.ind=TRUE)
#m[a] <- rowMeans(m,na.rm=TRUE)[a[,1]]

