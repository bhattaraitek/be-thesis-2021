
#program to fill missing data(discharge)
#Missing data should be labeled as DNA or T
#Missing data will be filled by long term average value
#created on 2021-Aug-18
#by Tek Narayan Bhattarai

rm(list= ls())
library("tidyverse") #to access charting and data manipulation tool


#set your own working directory here
setwd( "D:/#3_Software Practice/R/DataAnalysis")
#set your data filename
filename<-"Prefilled-Discharge_Data"
#set file path
filepath<- paste0("D:/#3_Software Practice/R/DataAnalysis/Data/",filename,".csv")
raw_data<-data.frame(read.csv(file = filepath,header = TRUE,sep = ',',fill = TRUE,
                              na.strings = "" ))


filleddata_final<-raw_data[,c("Month","Year","Day","Date")]
filleddata_final$Month<-match(filleddata_final$Month,month.abb)
filleddata_final$Date <- as.Date(paste(filleddata_final$Year,
                                       filleddata_final$Month,
                                     filleddata_final$Day,sep = "-"))

stn_names<-seq(5,ncol(raw_data),1)
colnam<-colnames(raw_data)[5:ncol(raw_data)] #vector of stations name
attach(raw_data)

#function to fill NA values with LT average

 filling<-function(prefilleddata,ltav.discharge){
   prefilleddata$Discharge[is.na(prefilleddata$Discharge)]<-ltav.discharge
   return(prefilleddata)
 }
 
stationwise_filled_final<-NULL
for (i in stn_names){
  
  naremove.df<- NULL
  dataextract.df<-NULL
  
  dataextract.df<-raw_data[,c("Month","Year","Day")]
  dataextract.df$Month<-match(dataextract.df$Month,month.abb)
  dataextract.df$Date <- as.Date(paste(dataextract.df$Year,
                                       dataextract.df$Month,
                                       dataextract.df$Day,sep = "-"))
  
  dataextract.df$Discharge<-raw_data[[i]]
  stn<-colnam[i-4]
  filtered.df<-na.omit(dataextract.df)  #removing blank cells
  naremove.df$Discharge<-filtered.df[[5]]

  naremove.df<-data.frame(naremove.df)
  naremove.df[naremove.df=="DNA"]<-NA
  filtered.df[[5]]<-as.numeric(naremove.df$Discharge)
  
  summary_monthly<-filtered.df%>%
    group_by(Month)%>%
    summarize(AvgDischarge=mean(Discharge,na.rm = TRUE))
  
  summary_monthly$Month<-month.abb[summary_monthly$Month]
  ltavg.discharge<-as.vector(summary_monthly$AvgDischarge)
 
  
  stationwise_filled<-NULL
  
  for(month_num in 1:12){
    monthwise=NULL
    monthwise<-filtered.df[filtered.df[,1]==month_num,]
    monthwise_filled<-filling(monthwise,ltavg.discharge[month_num] )
    stationwise_filled<-rbind(stationwise_filled,monthwise_filled)
    stationwise_filled_final<-stationwise_filled[,c("Date","Discharge")]
  }
  colnames(stationwise_filled_final)[2]<-stn
 
 
  filleddata_final<-merge(filleddata_final,stationwise_filled_final,by = c("Date"),all = TRUE)
}
write.csv(filleddata_final,"Filled-Discharge_Data.csv",row.names = FALSE,na = "")



        
        
        
        


        