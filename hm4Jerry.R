library(MASS)

#generate the fully observed data (true dataset)
n=50
mean=c(0, 0, 0, 0)
excor=0.2
cov=matrix(c(1, excor, excor, excor, excor, 1, excor, excor, excor, excor, 1, excor, excor, excor, excor, excor), nrow=4, ncol=4)#exchangeable
d=mvrnorm(n=50, mean, cov, empirical=FALSE)

#throw out percent missing (cc dataset)
#MCAR
percentmiss=0.05
nummiss=ceiling(n*4*0.75*0.05)

for(i in 1:nummiss){
	
	repeat{	
		randomcol=floor(runif(1)*3)+2
		randomrow=floor(runif(1)*n)+1
		
		if(is.na(d[randomrow, randomcol])){}
		else{
			d[randomrow, randomcol]<-NA
			break
		}
	}#end repeat
}

#MAR
#0.05=0.01, 0.01, 0.03
#0.1=0.01, 0.03, 0.06
#0.3=0.05, 0.1, 0.15
t2pmiss=0.01
t3pmiss=0.01
t4pmiss=0.03
t2nummiss=ceiling(n*4*0.75*t2pmiss)
t3nummiss=ceiling(n*4*0.75*t3pmiss)
t4nummiss=ceiling(n*4*0.75*t4pmiss)

for(i in 1:t2nummiss){
	
	repeat{	
		randomcol=2
		randomrow=floor(runif(1)*n)+1
		
		if(is.na(d[randomrow, randomcol])){}
		else{
			d[randomrow, randomcol]<-NA
			break
		}
	}#end repeat
}

for(i in 1:t3nummiss){
	
	repeat{	
		randomcol=3
		randomrow=floor(runif(1)*n)+1
		
		if(is.na(d[randomrow, randomcol])){}
		else{
			d[randomrow, randomcol]<-NA
			break
		}
	}#end repeat
}

for(i in 1:t4nummiss){
	
	repeat{	
		randomcol=4
		randomrow=floor(runif(1)*n)+1
		
		if(is.na(d[randomrow, randomcol])){}
		else{
			d[randomrow, randomcol]<-NA
			break
		}
	}#end repeat
}

#replace data with LOCF (LOCF dataset)
for(i in 1:n){
	for(j in 1:4){
		if(is.na(d[i, j])){
			d[i, j]=d[i, j-1]
		}
	}
}

