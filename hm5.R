#install.packages("zoo")
#install.packages("data.table")
library("zoo")
library("MASS")
library("tidyr")
library("dplyr")
library("foreach")
# Generate dataset
CompleteData <- function(n,ObsTime=4,rho,CorStructure,d) {
  if (CorStructure=="auto"){  #autoregressive
    Corr<-matrix(data =c(1,rho,rho^2,rho^3,rho,1,rho,rho^2,rho^2,rho,1,rho,rho^3,rho^2,rho,1),nrow = ObsTime)}  
  if ((CorStructure=="exch")){ #exchangable
    Corr<-matrix(data =c(1,rho,rho,rho,rho,1,rho,rho,rho,rho,1,rho,rho,rho,rho,1),nrow = ObsTime)}
  mean=rep(0,ObsTime)
  CompleteData<-mvrnorm(n,mean,Sigma = Corr,empirical=FALSE)
  #print(CompleteData)
  effect<-c(0*d,1/3*d,2/3*d,d)
  CompleteData[(dim(CompleteData)[[1]]/2+1):dim(CompleteData)[[1]],]<-t(t(CompleteData[(dim(CompleteData)[[1]]/2+1):dim(CompleteData)[[1]],])+effect)
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
    X[SampleList]<-NA
  }
  X<-as.data.frame(X)
  return(X)}

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
  return(Data)}

# RMANOVA
RMANOVA <- function(Data) {
  Data<-na.omit(Data)
  aovfit<-aov(formula = score~Time*Group+Error(ID),data = Data)
  result<-unlist(summary(aovfit))
  return(result)}

######################## RN ANOVA ########################

# Test the significant of the interaction term
SigInteraction <- function(Data,n,MissingProp,MARRate,term) {
  Data_remove_MCAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MCAR",MissingProp=MissingProp)
  Data_remove_MAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MAR",MARRate=MARRate)
  Data_LOCF_MCAR<-LOCF(Data_remove_MCAR)
  Data_LOCF_MAR<-LOCF(Data_remove_MAR)
  Data<-LongFormat(Data)
  Data_remove_MCAR<-LongFormat(Data_remove_MCAR)
  Data_LOCF_MCAR<-LongFormat(Data_LOCF_MCAR)
  Data_remove_MAR<-LongFormat(Data_remove_MAR)
  Data_LOCF_MAR<-LongFormat(Data_LOCF_MAR)
  
  result<-RMANOVA(Data)
  result_MCAR_remove<-RMANOVA(Data_remove_MCAR)
  result_MCAR_LOCF<-RMANOVA(Data_LOCF_MCAR)
  result_MAR_remove<-RMANOVA(Data_remove_MAR)
  result_MAR_LOCF<-RMANOVA(Data_LOCF_MAR)
  #names(result)
  trt<-ifelse (term=="interaction","Error: Within.Pr(>F)2","Error: ID.Pr(>F)1")
  trt_remove<-ifelse (term=="interaction","Error: Within.Pr(>F)2","Error: ID.Pr(>F)2")
  sig<-c(ifelse(result[trt]<0.05,1,0),ifelse(result_MCAR_remove[trt_remove]<0.05,1,0),ifelse(result_MCAR_LOCF[trt]<0.05,1,0),
         ifelse(result_MAR_remove[trt_remove]<0.05,1,0),ifelse(result_MAR_LOCF[trt]<0.05,1,0))
  names(sig)<-c("TRUE","Miss_MCAR","MCAR_LOCF","Miss_MAR","MAR_LOCF")
  sig
  return(sig)
}
set.seed(0561323)
timeStart<-timestamp()
d=c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2)
auto0.2_MCAR0.05_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto",d=d),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03),term="interaction"))%>%rowMeans())
auto0.2_MCAR0.1_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto",d=d),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06),term="interaction"))%>%rowMeans())
auto0.2_MCAR0.3_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto",d=d),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15),term="interaction"))%>%rowMeans())

auto0.5_MCAR0.05_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto",d=d),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03),term="interaction"))%>%rowMeans())
auto0.5_MCAR0.1_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto",d=d),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06),term="interaction"))%>%rowMeans())
auto0.5_MCAR0.3_MAR<-foreach(d=d, .combine = 'rbind') %do% (replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto",d=d),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15),term="interaction"))%>%rowMeans())
timeend<-timestamp()
d=c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2)
rownames(auto0.2_MCAR0.05_MAR)<-d
rownames(auto0.2_MCAR0.1_MAR)<-d
rownames(auto0.2_MCAR0.3_MAR)<-d
rownames(auto0.5_MCAR0.05_MAR)<-d
rownames(auto0.5_MCAR0.1_MAR)<-d
rownames(auto0.5_MCAR0.3_MAR)<-d
write.csv(x = auto0.2_MCAR0.05_MAR,file = "auto0.2_MCAR0.05_MAR.csv",row.names = TRUE)
write.csv(x = auto0.2_MCAR0.1_MAR,file = "auto0.2_MCAR0.1_MAR.csv",row.names = TRUE)
write.csv(x = auto0.2_MCAR0.3_MAR,file = "auto0.2_MCAR0.3_MAR.csv",row.names = TRUE)
write.csv(x = auto0.5_MCAR0.05_MAR,file = "auto0.5_MCAR0.05_MAR.csv",row.names = TRUE)
write.csv(x = auto0.5_MCAR0.1_MAR,file = "auto0.5_MCAR0.1_MAR.csv",row.names = TRUE)
write.csv(x = auto0.5_MCAR0.3_MAR,file = "auto0.5_MCAR0.3_MAR.csv",row.names = TRUE)
# Test interaction
# "auto0.2_MCAR0.05_MAR(0.01,0.01,0.03)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03)))%>%rowMeans()
# "auto0.2_MCAR0.1_MAR(0.01,0.03,0.06)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06)))%>%rowMeans()
# "auto0.2_MCAR0.3_MAR(0.05,0.1,0.15)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto"),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15)))%>%rowMeans()
# 
# "auto0.5_MCAR0.05_MAR(0.01,0.01,0.03)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03)))%>%rowMeans()
# "auto0.5_MCAR0.1_MAR(0.01,0.03,0.06)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.1,MARRate = c(0.01,0.03,0.06)))%>%rowMeans()
# "auto0.5_MCAR0.3_MAR(0.05,0.1,0.15)"<-replicate(n=5000,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.5,CorStructure="auto"),n=100,MissingProp = 0.3,MARRate = c(0.05,0.1,0.15)))%>%rowMeans()
# # summary table for interaction
# finalresult<-rbind(get("auto0.2_MCAR0.05_MAR(0.01,0.01,0.03)"),get("auto0.2_MCAR0.1_MAR(0.01,0.03,0.06)"),get("auto0.2_MCAR0.3_MAR(0.05,0.1,0.15)"),
#                    get("auto0.5_MCAR0.05_MAR(0.01,0.01,0.03)"),get("auto0.5_MCAR0.1_MAR(0.01,0.03,0.06)"),get("auto0.5_MCAR0.3_MAR(0.05,0.1,0.15)"))
# rownames(finalresult)<-c("auto0.2_MCAR0.05_MAR(0.01,0.01,0.03)","auto0.2_MCAR0.1_MAR(0.01,0.03,0.06)","auto0.2_MCAR0.3_MAR(0.05,0.1,0.15)",
#                          "auto0.5_MCAR0.05_MAR(0.01,0.01,0.03)","auto0.5_MCAR0.1_MAR(0.01,0.03,0.06)","auto0.5_MCAR0.3_MAR(0.05,0.1,0.15)")
# write.csv(x = finalresult,file = "LOCF5000.csv",row.names = TRUE)



