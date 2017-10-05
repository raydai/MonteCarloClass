library(tidyr)
library(dplyr)
library(readxl)
# all power value
LOCF_All<-read_excel(path = "All_locf.xlsx",sheet = 1) 
PowerTable<-LOCF_All%>%gather(key = "d",value = "Power",5:31)
PowerTable<-separate(PowerTable,d,sep = "_",into=c("Type1","d"))
PowerTable<-PowerTable%>%filter(percent!=10,d<=1.25)
PowerTable$Type<-paste(PowerTable$mech,PowerTable$Type1,PowerTable$percent,sep = "_")
PowerTable<-PowerTable[,-c(3,4,5)]
dput(names(PowerTable))



# Spread function
PowerTable<-spread(data=PowerTable,key =c("Type"),value = "Power")
PowerTable<-PowerTable[,c("n", "corr", "d", "mcar_TRUE_5", "mcar_CC_5","mcar_LOCF_5"
              ,"mcar_TRUE_30","mcar_CC_30", "mcar_LOCF_30",
              "mar_TRUE_5","mar_CC_5","mar_LOCF_5",
              "mar_TRUE_30","mar_CC_30", "mar_LOCF_30")]

write.csv(PowerTable,file = "allSpreadno10.csv",row.names = FALSE)

# n
Table_n50<-PowerTable%>%filter(n==50)
Table_n100<-PowerTable%>%filter(n==100)

# auto
