#install.packages("ICC")
library("tidyverse")
library("foreach")
library("ICC")
# library("lme4")
# library("lmerTest")
library("nlme")
# Generate sample size list (sample size for each group)
rand_vect <- function(subgroup, min=8,max=22,total) {
  vec=runif(n=subgroup, min = min, max = max)
  (vec <- round(vec / sum(vec) * total))
  deviation <- total - sum(vec)
  # if the the sum of the total number is large 150, we randomly remove the extra samples in the groups
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(subgroup, 1)] + sign(deviation)
  }
  # check the all the number within the range we specifed.
  while (min(vec)<min){
    N<-length(which(vec==min(vec)))
    vec[which(vec==min(vec))] <- vec[which(vec==min(vec))] + 1
    vec[i] <- vec[i <- sample(which(vec!=min(vec)), N)] - 1
  }
  while (max(vec)>max){
    N<-length(which(vec==max(vec)))
    vec
    (vec[which(vec==max(vec))] <- vec[which(vec==max(vec))] - 1)
    vec[i] <- vec[i <- sample(which(vec!=max(vec)), N)] + 1
    vec
  }
  return(vec)
  #ref:https://stackoverflow.com/questions/24845909/generate-n-random-integers-that-sum-to-m-in-r
}

#(listx<-c(rand_vect(subgroup = 10,min = 8,max = 22,total = 150)))
#paste(min(listx),max(listx),sum(listx),sep=" ")

########### ICC DATA ############
Group.data <- function(ICC,subgroup.size) {
  #ICC=0.2
  var_tau<-ICC
  var_e<-1-var_tau
  #subgroup.data
  temp.data<-foreach(a=subgroup.size,
                     b=seq(1:length(subgroup.size)),
                     tau_j=rnorm(n=length(subgroup.size), mean = 0, sd = sqrt(var_tau)),.combine='rbind') %do% {
    mu=0
    beta=0  # group effect
    g=0     # group label
    epsilon_ij = rnorm(n = a, mean = 0, sd = sqrt(var_e)) # individual deviance
    y<-mu+beta*g+tau_j+epsilon_ij
    sub.temp.data<-data.frame("subgroup"=LETTERS[b],"tau_j"=tau_j,"epsilon_ij"=epsilon_ij,"Y"=y)}
  # caculate the ICC for our simulated dataset
  result<-ICCbare(y = Y, x = subgroup, data = temp.data)
  # make sure the sythetic overall ICC is closed to the assigned ICC.
  while (abs(result-ICC)>0.02){
    temp.data<-foreach(a=subgroup.size,
                       b=seq(1:length(subgroup.size)),
                       tau_j=rnorm(n=length(subgroup.size), mean = 0, sd = sqrt(var_tau)),.combine='rbind') %do% {
                         mu=0
                         beta=0  # group effect
                         g=0     # group label
                         epsilon_ij = rnorm(n = a, mean = 0, sd = sqrt(var_e)) # individual deviance
                         y<-mu+beta*g+tau_j+epsilon_ij
                         sub.temp.data<-data.frame("subgroup"=LETTERS[b],"tau_j"=tau_j,"epsilon_ij"=epsilon_ij,"Y"=y)
                       }
    result<-ICCbare(y = Y, x = subgroup, data = temp.data)     # test the ICC
  }
  temp.data<-temp.data%>%mutate("ID"=c(1:sum(subgroup.size)),"group"=rep(c("A","B"),c(sum(subgroup.size)/2,sum(subgroup.size)/2)))
  temp.data<-temp.data%>%mutate("ID"=as.factor(ID),"group"=as.factor(group))
  return(temp.data)
}

# (ICCresult<-replicate(n = 10,Group.data(ICC=0.2,subgroup.size = rand_vect(group = 10,min = 8,max = 22,total = 150))))
# mean(ICCresult)

# generate the group label and combind two group dataset
set.seed(-2123070653)

#subgroup.size<-c(rand_vect(subgroup = 10,min = 8,max = 22,total = 150),rand_vect(subgroup = 10,min = 8,max = 22,total = 150))
#Mydata<-Group.data(ICC = 0.2,subgroup.size =subgroup.size)
#ICCnumber<-ICCbare(y = Y, x = subgroup, data = Mydata)

ModelTest <- function(Mydata) {
  # ignore subcluster and test the type I error by using lm function.
  fit<-aov(formula =Y~group,data = Mydata)
  result<-unlist(summary(fit))
  # I.result.type.I.error<-ifelse(result["Pr(>F)1"]<0.05,1,0)
  
  # summary over cluster
  Summary.Data<-as.data.frame(Mydata%>%group_by(subgroup)%>%summarise("Y_bar"=mean(Y))%>%mutate("group"=rep(c("A","B"),c(10,10))))
  fit.bar<-aov(formula =Y_bar~group,data = Summary.Data)
  result.bar<-unlist(summary(fit.bar))
  # I.result.type.I.error.bar<-ifelse(result.bar["Pr(>F)1"]<0.05,1,0)
  
  #nlme pacakge mixed effect
  nlme.fit.mixed.fixed<-lme(Y~group,random = ~1|subgroup,data = Mydata)
  result.mixed<-unlist(anova.lme(nlme.fit.mixed.fixed))

  # mixed models_fixed
  # fit.mixed.fixed<-aov(formula = Y~group/subgroup,data = Mydata)
  # result.mixed.fixed<-unlist(summary(fit.mixed.fixed))
  # #mixed models_random
  # fit.mixed.random<-lmer(formula = Y~group*subgroup+(1|subgroup),data = Mydata)
  # result.mixed.random<-unlist(summary(fit.mixed.random))
  # #mixed models_nested
  # fit.mixed.nest<-lmer(formula = Y~group*subgroup+(1|group/subgroup),data = Mydata)
  # result.mixed.nest<-unlist(summary(fit.mixed.nest))
  # I.result.type.I.error.mixed<-ifelse(result.mixed["Pr(>F)1"]<0.05,1,0)
  #names(result)
  sig<-c(ifelse(result["Pr(>F)1"]<0.05,1,0),ifelse(result.bar["Pr(>F)1"]<0.05,1,0),ifelse(result.mixed["p-value2"]<0.05,1,0))
  names(sig)<-c("Ignore_cluster","Sum_over_cluster","MixedModel")
  return(sig)
}
ICC_0.1<-replicate(n = 5000,ModelTest(Mydata = Group.data(ICC = 0.1,subgroup.size =c(rand_vect(subgroup = 10,min = 8,max = 22,total = 150),rand_vect(subgroup = 10,min = 8,max = 22,total = 150)))))
ICC_0.1.result<-rowMeans(ICC_0.1)
ICC_0.5<-replicate(n = 5000,ModelTest(Mydata = Group.data(ICC = 0.5,subgroup.size =c(rand_vect(subgroup = 10,min = 8,max = 22,total = 150),rand_vect(subgroup = 10,min = 8,max = 22,total = 150)))))
ICC_0.5.result<-rowMeans(ICC_0.5)
ICC_0.8<-replicate(n = 5000,ModelTest(Mydata = Group.data(ICC = 0.8,subgroup.size =c(rand_vect(subgroup = 10,min = 8,max = 22,total = 150),rand_vect(subgroup = 10,min = 8,max = 22,total = 150)))))
ICC_0.8.result<-rowMeans(ICC_0.8)
ICC.result<-rbind(ICC_0.1.result,ICC_0.5.result,ICC_0.8.result)
write.csv(ICC.result,file = "ICC_result_nlme.csv",row.names = TRUE)
