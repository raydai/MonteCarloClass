set.seed(201713)
(X<-rpois(n=720,lambda = 15/720))
timeline<-seq(1:length(X))

# Random staying time fucntion
staying_time<-function(n=1,mean=34,sd=6) rnorm(n, mean, sd)

CreatBed<-function(beds=2){
  TableSchedule<-data.frame(matrix(rep(x = 0,beds*720),nrow = beds))
  rownames(TableSchedule)<-paste("Bed",1:beds,sep = "")
  colnames(TableSchedule)<-paste("T",1:720,sep = "")
  return(TableSchedule)
}
BedsSchedule<-CreatBed(beds = 2)

# Random assign
BedsSchedule[c(1,2),1]<-1
BedsSchedule[1,2]<-1

## check Full beds, if full, then B3 +1
ifelse(all(BedsSchedule[,i]),Bed3[i]<-1,Bed3[i]<-0)



## if X > 0 then generate the time
##  update the Bedtime schedule
## if X > 3 then stop!!!!



for (i in 1:720){
  CheckIn_people<-X[i]
  if(CheckIn_people!=0){
    # Assigne staying time
    ind_people<-paste(colnames(BedsSchedule)[1],"_",c(1:CheckIn_people),sep = "")
    for(j in 1:CheckIn_people){
      assign(ind_people[j],staying_time())
    }
    rm(ind_people)
    
    # update bed schedule
    if (!all(BedsSchedule[,i])){
      empty_room<-grep(BedsSchedule[,i],pattern = 0)
      
      BedsSchedule[empty_room,i+ceiling(staying_time())]<-1
    }
    
    
    } else{
      
    }
}





