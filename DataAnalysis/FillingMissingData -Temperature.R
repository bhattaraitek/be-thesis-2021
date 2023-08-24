
#program to fill missing data(temperature)
#Missing data should be labeled as DNA or T
#Missing data will be filled by long term average value
#created on 2021-Aug-13

rm(list= ls())
library("tidyverse") #to access charting and data manipulation tool
library("scales") # to access breaks/formatting functions

#set your own working directory here
setwd( "D:/#3_Software Practice/R/DataAnalysis")
#set your data filename
filename<-"Prefilled-Temperature_Data"
#set file path
filepath<- paste0("D:/#3_Software Practice/R/DataAnalysis/Data/",filename,".csv")
raw_data<-data.frame(read.csv(file = filepath,header = TRUE,sep = ',',fill = TRUE,
                              na.strings = "" ))



filleddata_final<-raw_data[,c("Month","Year","Day","Date")]
filleddata_final$Month<-match(filleddata_final$Month,month.abb)
filleddata_final$Date <- as.Date(paste(filleddata_final$Year,
                                       filleddata_final$Month,
                                     filleddata_final$Day,sep = "-"))

stn_names<-seq(5,ncol(raw_data),2)
colnam<-colnames(raw_data)[5:ncol(raw_data)] #vector of stations name
attach(raw_data)

 filling<-function(prefilleddata,ltav.tmax,ltav.tmin){
   prefilleddata$Tmax[is.na(prefilleddata$Tmax)]<-ltav.tmax
   prefilleddata$Tmin[is.na(prefilleddata$Tmin)]<-ltav.tmin
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
  
  dataextract.df$Tmax<-raw_data[[i]]
  dataextract.df$Tmin<-raw_data[[i+1]]
  stationname1<-colnam[i-4]
  stationname2<-colnam[i-3]
  stn<-substr(colnam[i-4],1,4)
  filtered.df<-na.omit(dataextract.df)  #removing blank cells
  naremove.df$Tmax<-filtered.df[[5]]
  naremove.df$Tmin<-filtered.df[[6]]
  naremove.df<-data.frame(naremove.df)
  naremove.df[naremove.df=="DNA"]<-NA
  filtered.df[[5]]<-as.numeric(naremove.df$Tmax)
  filtered.df[[6]]<-as.numeric(naremove.df$Tmin)
  
  summary_monthly<-filtered.df%>%
    group_by(Month)%>%
    summarize(AvgTmax=mean(Tmax,na.rm = TRUE),AvgTmin=mean(Tmin,na.rm = TRUE))
  
  summary_monthly$Month<-month.abb[summary_monthly$Month]
  ltavg.tmax<-as.vector(summary_monthly$AvgTmax)
  ltavg.tmin<-as.vector(summary_monthly$AvgTmin)
  
  stationwise_filled<-NULL
  
  for(month_num in 1:12){
    monthwise=NULL
    monthwise<-filtered.df[filtered.df[,1]==month_num,]
    monthwise_filled<-filling(monthwise,ltavg.tmax[month_num],
                              ltavg.tmin[month_num] )
    stationwise_filled<-rbind(stationwise_filled,monthwise_filled)
    stationwise_filled_final<-stationwise_filled[,c("Date","Tmax","Tmin")]
  }
  colnames(stationwise_filled_final)[2]<-stationname1
  colnames(stationwise_filled_final)[3]<-stationname2
 
  filleddata_final<-merge(filleddata_final,stationwise_filled_final,by = c("Date"),all = TRUE)
}
write.csv(filleddata_final,"Filled-Temperature_Data.csv",row.names = FALSE,na = "")



        
        
        
        


        