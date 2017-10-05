
staying_time<-function(n=1,mean=34,sd=6) rnorm(n, mean, sd)


EventA<-function(ave_births = 15){
  timebed1 = 0; 
  timebed2 = 0;
  need3_list<-c()
  x_list<-c()
  for (i in 1:720){
    x <-rpois(n=1,lambda = 15/720)
    x_list<-c(x_list,x)
    print(x)
    if (x>=1){
      if (i > timebed1) bed1=0
      if (i > timebed2) bed2=0
      ifelse(bed1==0|bed2==0,need3<-0,need3<-1)
      if (bed1==0) {
        timebed1=i+staying_time()
        print(paste("t1",timebed1))
      } else {
        if (bed2==0){
          timebed2=i+staying_time()
          print(paste("t2",timebed1))
        }
      }
    }
    need3_list<-c(need3_list,need3)
  }
  Total_need3<-length(which(need3_list>0))
#return(Total_need3)  
}

TTTT<-replicate(n = 10000,EventA())




