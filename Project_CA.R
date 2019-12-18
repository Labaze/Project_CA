library(Amelia)
library(magrittr)
library(dplyr)

customers<-read.csv("Ressources/customerdata.csv")
summerplay<-read.csv("Ressources/summersesstrx.csv")
summerfinTRX<-read.csv("summerfintrx.csv")
customers

active.customers <- left_join(summerplay, customers, by = "CustomerID")
group<- active.customers %>% group_by(CustomerID) %>% summarise(fallbonus=sum(fallbonus))%>%
  ungroup()
group

RFM.database<-left_join(summerplay,summerfinTRX,by="CustomerID",copy=FALSE)
RFM.database <- data.frame(RFM.database[,3],RFM.database[,2],RFM.database[,14])

names <- c("ID","Date","Amount")
names(RFM.database) <- names
RFM.database
RFM.database[,2] <- as.Date(RFM.database[,2]) 
RFM.database

#
startDate_history <- as.Date("20180501","%Y%m%d")
endDate_history <- as.Date("20180831","%Y%m%d")

#Replacing NAs by 0 
RFM.database[is.na(RFM.database)] <- 0

#Adapting the price
RFM.database$Amount[RFM.database$Amount==0]<-0
RFM.database$Amount[RFM.database$Amount==1]<-2.99
RFM.database$Amount[RFM.database$Amount==2]<-4.99
RFM.database$Amount[RFM.database$Amount==3]<-9.99
RFM.database$Amount[RFM.database$Amount==4]<-25
RFM.database$Amount[RFM.database$Amount==5]<-99


createbasetable <- function(df,startDate,endDate,ID="ID",Date="Date",Amount="Amount"){
  #pay attention the names of the columns have changed in the line above
  #remove the observations before the start data and after the end Date
  newdf<-df%>%
    group_by(ID)%>%
    summarise(Frequency=n(),#create frequency and recency for them
              Recency=as.numeric(endDate-max(Date)),
              Monetaryvalue=sum(Amount)) %>%
    ungroup()#ungroup back to store as dataframe
  return(newdf)
} # end of function getDataFrame

#Run RFM
RFM_table=createbasetable(RFM.database,startDate_history,endDate_history)
RFM_table


