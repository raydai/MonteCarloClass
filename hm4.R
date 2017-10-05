#install.packages("zoo")
library("zoo")
library("MASS")
library("tidyr")
library("dplyr")
# Generate dataset
CompleteData <- function(n,ObsTime=4,rho,CorStructure) {
  if (CorStructure=="auto"){  #autoregressive
    Corr<-matrix(data =c(1,rho,rho^2,rho^3,rho,1,rho,rho^2,rho^2,rho,1,rho,rho^3,rho^2,rho,1),nrow = ObsTime)}  
  if ((CorStructure=="exch")){ #exchangable
    Corr<-matrix(data =c(1,rho,rho,rho,rho,1,rho,rho,rho,rho,1,rho,rho,rho,rho,1),nrow = ObsTime)}
  mean=rep(0,ObsTime)
  CompleteData<-mvrnorm(n,mean,Sigma = Corr,empirical=FALSE)
  return(CompleteData)
  }
# Generate Missing Data set
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

# Trnasform to long format
LongFormat <- function(Data) {
  Data<-as.data.frame(Data)
  Data<-transform(Data,"Group"=rep(c("Case","Control"),c(dim(Data)[[1]]/2,dim(Data)[[1]]/2)),"ID"=rownames(Data))
  Data<-gather(data= Data ,key = "Time",value = "score",1:4)
  Data<-within(Data,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  return(Data)
}

# RMANOVA
RMANOVA <- function(Data) {
  aovfit<-aov(formula = score~Time*Group+Error(ID),data = Data)
  result<-unlist(summary(aovfit))
  return(result)
}

##############################################################################################################

######################## RN ANOVA ########################
# convert to long format and add group
SigInteraction <- function(Data,n,MissingProp,MARRate) {
  # Data=CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto")
  # n=100
  # MissingProp=0.05
  # MARRate=c(0.01,0.01,0.03)
  Data_remove_MCAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MCAR",MissingProp=MissingProp)
  Data_remove_MAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MAR",MARRate=MARRate)
  
  Data_LOCF_MCAR<-LOCF(Data_remove_MCAR)
  Data_LOCF_MAR<-LOCF(Data_remove_MAR)
  
  Data<-as.data.frame(Data)
  
  Data_remove_MCAR<-as.data.frame(Data_remove_MCAR)
  Data_LOCF_MCAR<-as.data.frame(Data_LOCF_MCAR)
  
  Data_remove_MAR<-as.data.frame(Data_remove_MAR)
  Data_LOCF_MAR<-as.data.frame(Data_LOCF_MAR)
  
  Data<-transform(Data,"Group"=rep(c("Case","Control"),c(dim(Data)[[1]]/2,dim(Data)[[1]]/2)),"ID"=rep(1:(dim(Data)[[1]]/2),2))
  Data_remove_MCAR<-transform(Data_remove_MCAR,"Group"=rep(c("Case","Control"),c(dim(Data_remove_MCAR)[[1]]/2,dim(Data_remove_MCAR)[[1]]/2)),"ID"=rep(1:(dim(Data_remove_MCAR)[[1]]/2),2))
  Data_LOCF_MCAR<-transform(Data_LOCF_MCAR,"Group"=rep(c("Case","Control"),c(dim(Data_LOCF_MCAR)[[1]]/2,dim(Data_LOCF_MCAR)[[1]]/2)),"ID"=rep(1:(dim(Data_LOCF_MCAR)[[1]]/2),2))
  
  Data_remove_MAR<-transform(Data_remove_MAR,"Group"=rep(c("Case","Control"),c(dim(Data_remove_MAR)[[1]]/2,dim(Data_remove_MAR)[[1]]/2)),"ID"=rep(1:(dim(Data_remove_MAR)[[1]]/2),2))
  Data_LOCF_MAR<-transform(Data_LOCF_MAR,"Group"=rep(c("Case","Control"),c(dim(Data_LOCF_MAR)[[1]]/2,dim(Data_LOCF_MAR)[[1]]/2)),"ID"=rep(1:(dim(Data_LOCF_MAR)[[1]]/2),2))
  
  Data<-gather(data= Data ,key = "Time",value = "score",1:4)
  Data_remove_MCAR<-gather(data= Data_remove_MCAR ,key = "Time",value = "score",1:4)
  Data_LOCF_MCAR<-gather(data= Data_LOCF_MCAR ,key = "Time",value = "score",1:4)
  Data_remove_MAR<-gather(data= Data_remove_MAR ,key = "Time",value = "score",1:4)
  Data_LOCF_MAR<-gather(data= Data_LOCF_MAR ,key = "Time",value = "score",1:4)
  
  Data<-within(Data,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  Data_remove_MCAR<-within(Data_remove_MCAR,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  Data_LOCF_MCAR<-within(Data_LOCF_MCAR,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  Data_remove_MAR<-within(Data_remove_MAR,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  Data_LOCF_MAR<-within(Data_LOCF_MAR,{
    Group<-factor(Group)
    Time<-factor(Time)
    ID<-factor(ID)})
  aov<-aov(formula = score~Time*Group+Error(ID),data = Data)
  aov_remove_MCAR<-aov(formula = score~Time*Group+Error(ID),data = Data_remove_MCAR)
  aov_MCAR_LOCF<-aov(formula = score~Time*Group+Error(ID),data = Data_LOCF_MCAR)
  aov_remove_MAR<-aov(formula = score~Time*Group+Error(ID),data = Data_remove_MAR)
  aov_MAR_LOCF<-aov(formula = score~Time*Group+Error(ID),data = Data_LOCF_MAR)
  result<-unlist(summary(aov))
  result_MCAR_remove<-unlist(summary(aov_remove_MCAR))
  result_MCAR_LOCF<-unlist(summary(aov_MCAR_LOCF))
  result_MAR_remove<-unlist(summary(aov_remove_MAR))
  result_MAR_LOCF<-unlist(summary(aov_MAR_LOCF))
  #names(result)
  sig<-c(ifelse(result["Error: Within.Pr(>F)2"]<0.05,1,0),ifelse(result_MCAR_remove["Error: Within.Pr(>F)2"]<0.05,1,0),ifelse(result_MCAR_LOCF["Error: Within.Pr(>F)2"]<0.05,1,0),
         ifelse(result_MAR_remove["Error: Within.Pr(>F)2"]<0.05,1,0),ifelse(result_MAR_LOCF["Error: Within.Pr(>F)2"]<0.05,1,0))
  names(sig)<-c("TRUE","Miss_MCAR","MCAR_LOCF","Miss_MAR","MAR_LOCF")
  
  return(sig)
}
set.seed(0546504)
"auto0.2_MCAR0.05_MAR(0.01,0.01,0.03)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03)))%>%rowMeans()
"auto0.2_MCAR0.1_MAR(0.01,0.03,0.06)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06)))%>%rowMeans()
"auto0.2_MCAR0.3_MAR(0.05,0.1,0.15)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15)))%>%rowMeans()

"auto0.5_MCAR0.05_MAR(0.01,0.01,0.03)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03)))%>%rowMeans()
"auto0.5_MCAR0.1_MAR(0.01,0.03,0.06)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06)))%>%rowMeans()
"auto0.5_MCAR0.3_MAR(0.05,0.1,0.15)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15)))%>%rowMeans()


