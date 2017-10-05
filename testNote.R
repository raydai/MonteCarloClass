Data<-CompleteData(n=50,ObsTime=4,rho=.2,CorStructure="auto",d=1)
n=50
MissingProp = 0.05
MARRate = c(0.01,0.01,0.03)
term="trt"

Data_remove_MCAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MCAR",MissingProp=MissingProp)
Data_remove_MAR<-RemoveDataPoint(Data=Data,n=n,MissType = "MAR",MARRate=MARRate)
Data_LOCF_MCAR<-LOCF(Data_remove_MCAR)
Data_LOCF_MAR<-LOCF(Data_remove_MAR)

Data<-LongFormat(Data)
Data_remove_MCAR<-LongFormat(Data_remove_MCAR)
Data_LOCF_MCAR<-LongFormat(Data_LOCF_MCAR)
Data_remove_MAR<-LongFormat(Data_remove_MAR)
Data_LOCF_MAR<-LongFormat(Data_LOCF_MAR)

aovfit<-aov(formula = score~Time*Group+Error(ID),data = Data_remove_MCAR)
summary(aovfit)
result<-unlist(summary(aovfit))

result<-RMANOVA(Data)
result_MCAR_remove<-RMANOVA(Data_remove_MCAR)
result_MCAR_LOCF<-RMANOVA(Data_LOCF_MCAR)
result_MAR_remove<-RMANOVA(Data_remove_MAR)
result_MAR_LOCF<-RMANOVA(Data_LOCF_MAR)
#names(result)
summary(result)
trt<-ifelse (term=="interaction","Error: Within.Pr(>F)2","Error: ID.Pr(>F)1")
ifelse(result[trt]<0.05,1,0)
sig<-c(ifelse(result[trt]<0.05,1,0),ifelse(result_MCAR_remove[trt]<0.05,1,0),ifelse(result_MCAR_LOCF[trt]<0.05,1,0),
       ifelse(result_MAR_remove[trt]<0.05,1,0),ifelse(result_MAR_LOCF[trt]<0.05,1,0))
names(sig)<-c("TRUE","Miss_MCAR","MCAR_LOCF","Miss_MAR","MAR_LOCF")
sig
return(sig)
}
set.seed(0561323)
"testResult"<-replicate(n=500,SigInteraction(Data =CompleteData(n=100,ObsTime=4,rho=.2,CorStructure="auto",d=1),n=100,MissingProp = 0.05,MARRate = c(0.01,0.01,0.03),term="trt"))%>%rowMeans()