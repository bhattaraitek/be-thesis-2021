#--------------------------------------------------------------------------------
# Missing Data Summary(Temperature)
# Missing data should be labeled as DNA 
# created on 2021-Aug-13
# By Tek Narayan Bhattarai
#---------------------------------------------------------------------------------
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


stn_names<-seq(5,ncol(raw_data),2)
colnam<-colnames(raw_data)[5:ncol(raw_data)] #vector of stations name
missingdata<-data.frame(Station=NA,TDays=NA,TMDays_tmax=NA,TMDays_tmin=NA,
                        PMDays_tmax=NA,PMDays_tmin=NA)
attach(raw_data)

for (i in stn_names){
  cumvar=NULL
  sum_monthly=NULL
  sum_yearly=NULL
  naremove.df<- NULL
  dataextract.df<-NULL
  
  dataextract.df<-raw_data[,c("Month","Year","Day")]
  dataextract.df$Month<-match(dataextract.df$Month,month.abb)
  # Change Date to date format
  dataextract.df$Date <- as.Date(paste(dataextract.df$Year,dataextract.df$Month,
                                       dataextract.df$Day,sep = "-"))
  
  dataextract.df$Tmax<-raw_data[[i]]
  dataextract.df$Tmin<-raw_data[[i+1]]
  stn<-substr(colnam[i-4],1,4)
  filtered.df<-na.omit(dataextract.df)  #removing blank cells
  naremove.df$Tmax<-filtered.df[[5]]
  naremove.df$Tmin<-filtered.df[[6]]
  naremove.df<-data.frame(naremove.df)
  naremove.df[naremove.df=="DNA"]<-NA
  filtered.df[[5]]<-as.numeric(naremove.df$Tmax)
  filtered.df[[6]]<-as.numeric(naremove.df$Tmin)
  # filtered.df<-filtered.df[,c(-Year,-Month,-Day)]
  
 
  rowval=NULL
  TDDays=0 
  TMDays_tmax=0
  TMDays_tmin=0

  
  TDays<-nrow(filtered.df)-1
  TMDays_tmax<-sum(is.na(filtered.df$Tmax))
  TMDays_tmin<-sum(is.na(filtered.df$Tmin))
  PMDays_tmax<-as.numeric((TMDays_tmax/TDays) *100)
  PMDays_tmin<-as.numeric((TMDays_tmin/TDays) *100)
  rowval<-c(stn,TDays,TMDays_tmax,TMDays_tmin,PMDays_tmax,PMDays_tmin)
  missingdata<-rbind (missingdata,rowval)
}
write.csv(missingdata,"MissingDataSummary_Temperature.csv",row.names = FALSE)



        
        
        
        


        