
#program to extract missing data summary from rawdata
#Missing data should be labeled as DNA or T
#created in 2021-Aug-9

rm(list= ls())
library("tidyverse") #to access charting and data manipulation tool
library("scales") # to access breaks/formatting functions
library("ggpmisc") # to access function to create regression model

#set your own working directory here
setwd( "D:/#3_Software Practice/R/DataAnalysis")
#set your data filename
filename<-"Prefilled-RainfallData_Raw"
#set file path
filepath<- paste0("D:/#3_Software Practice/R/DataAnalysis/Data/",filename,".csv")

raw_data<- data.frame (read.csv(file = filepath,header = TRUE,sep = ',',fill = TRUE,
               na.strings = "" ))

raw_data_dmy<-raw_data[,c("Month","Year","Day")]
raw_data_dmy$Month<-match(raw_data_dmy$Month,month.abb)
# Change Date to date format
raw_data_dmy$Date <- as.Date(paste(raw_data_dmy$Year,raw_data_dmy$Month,raw_data_dmy$Day,sep = "-"))

stn_names<-names(raw_data)[5:ncol(raw_data)] #vector of stations name
attach(raw_data)
missingdata<-data.frame(Station=NA,TDays=NA,TMDays=NA,
                        PMDays=NA,TMDRainy=NA,PMDRainy=NA)

for (i in stn_names){
  rowval=NULL
  TDRainy=NULL
  TDDays=0 
  TMDays=0
  TMDRainy=0
  clean.df=NULL
  temp.df<- NULL
  
  spstn.df<-within(data = raw_data_dmy,{assign(paste(i),get(i))})
  stn<-paste(i)
  clean.df<-na.omit(spstn.df)  #removing blank cells

  temp.df$value<-clean.df[[i]]
  temp.df<-data.frame(temp.df)
  temp.df$value[temp.df$value=="DNA"]<-NA
  clean.df[[i]]<-temp.df$value
  
  TDays<-nrow(clean.df)-1
  TMDays<-sum(is.na(clean.df[[i]]))
  PMDays<-as.numeric((TMDays/TDays) *100)
 
  TMDRainy<-nrow(subset(clean.df,
                        (clean.df$Month==6|clean.df$Month==7|clean.df$Month==8|clean.df$Month==9)
                        & is.na(clean.df[[i]])))
 
  PMDRainy<-as.numeric((TMDRainy/TDays)*100)
  rowval<-c(stn,TDays,TMDays,PMDays,TMDRainy,PMDRainy)
  missingdata<-rbind (missingdata,rowval)
}
write.csv(missingdata,"MissingDataSummary_Discharge.csv",row.names = FALSE)
        
        


        