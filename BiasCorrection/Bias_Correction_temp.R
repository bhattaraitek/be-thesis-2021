rm(list=ls())
setwd("D:\\#2_Project Works\\Climate Projection\\Bias Correction")

library("qmap")
library("tidyverse")

models<-data.frame(read.csv(file = "models.csv",header = TRUE,sep = ",",nrows = 5))

type<-"tasmax"

#reading observed rainfall data
daterange_data<-"1990-2014"
obs_filename<-paste0("Data_",type,"\\Observed\\Filled-Temperature_Data_",type,"_",daterange_data,".csv")
observed_temp<-data.frame(read.csv(file = obs_filename,header = TRUE,sep = ","))
observed_temp$Date<-as.Date(observed_temp$Date,format = "%m/%d/%Y")
observed_temp$Month<-lubridate::month(observed_temp$Date)
row.names(observed_temp)<-observed_temp$Date
observed_temp$Date<-NULL
 
for(i in (c(1:5))){
        if (!dir.exists(paste0(type,"_",models$Model[i]))) {dir.create(paste0(type,"_",models$Model[i]))}
        dirname<-paste0(type,"_",models$Model[i])
        
        ########### model historical data
        hist_filename<-paste0("Data_",type,"\\historical\\",type,"_historical_",models$Model[i],"_",daterange_data,".csv")
        model_histemp <-data.frame(read.csv(file = hist_filename,header = TRUE,sep = ",")) 
        model_histemp$Date<-as.Date(model_histemp$Date,format = "%m/%d/%Y")
        model_histemp$Month<-lubridate::month(model_histemp$Date)
        row.names(model_histemp) <- model_histemp$Date     ## change row name as date
        model_histemp$Date = NULL
        
        ########### ssp245 data ############
        ssp245_filename<-paste0("Data_",type,"\\ssp245\\",type,"_ssp245_",models$Model[i],"_",daterange_data,".csv")
        scenario_ssp245 <-data.frame(read.csv(file = ssp245_filename,header = TRUE,sep = ","))
        scenario_ssp245$Date<-as.Date(scenario_ssp245$Date,format = "%m/%d/%Y")
        scenario_ssp245$Month<-lubridate::month(scenario_ssp245$Date)
        row.names(scenario_ssp245) <- scenario_ssp245$Date     ## change row name as date
        scenario_ssp245$Date = NULL
        
        ########### ssp585 data ##########
        ssp585_filename<-paste0("Data_",type,"\\ssp585\\",type,"_ssp585_",models$Model[i],"_",daterange_data,".csv")
        scenario_ssp585 <-data.frame(read.csv(file = ssp585_filename,header = TRUE,sep = ","))
        scenario_ssp585$Date<-as.Date(scenario_ssp585$Date,format = "%m/%d/%Y")
        scenario_ssp585$Month<-lubridate::month(scenario_ssp585$Date)
        row.names(scenario_ssp585) <- scenario_ssp585$Date     ## change row name as date
        scenario_ssp585$Date = NULL
        
        #creating empty dataframes
        qmlinearhist<-NULL
        qmlinear245<-NULL
        qmlinear585<-NULL
        
        for (j in (1:12)){
                #sub-setting data month wise "Monthly data pooling"
                observed_temp_monthly<-subset(observed_temp,Month == j)
                model_histemp_monthly<-subset(model_histemp,Month == j)
                scenario_ssp245_monthly<-subset(scenario_ssp245,Month == j)
                scenario_ssp585_monthly<-subset(scenario_ssp585,Month == j)
                
                #removing "Month" column
                observed_temp_monthly<-observed_temp_monthly[-ncol(observed_temp_monthly)]
                model_histemp_monthly<-model_histemp_monthly[-ncol(model_histemp_monthly)]
                scenario_ssp245_monthly<-scenario_ssp245_monthly[-ncol(scenario_ssp245_monthly)]
                scenario_ssp585_monthly<-scenario_ssp585_monthly[-ncol(scenario_ssp585_monthly)]
                
                #call to fitting and applying Parametric transfer function
                qm.linear.fit.monthly<-fitQmapPTF(observed_temp_monthly,model_histemp_monthly,transfun = "linear",
                                          cost="RSS",wet.day = FALSE ,qstep = 0.01)
                qmlinearhist.monthly<-doQmapPTF(model_histemp_monthly,qm.linear.fit.monthly,type ="linear")
                qmlinear245.monthly<-doQmapPTF(scenario_ssp245_monthly,qm.linear.fit.monthly,type = "linear")
                qmlinear585.monthly<-doQmapPTF(scenario_ssp585_monthly,qm.linear.fit.monthly,type = "linear")
                
                #accumulating all month data in one "Dataframe"
                qmlinearhist<-rbind(qmlinearhist,qmlinearhist.monthly)
                qmlinear245<-rbind(qmlinear245,qmlinear245.monthly)
                qmlinear585<-rbind(qmlinear585,qmlinear585.monthly)
                
               
                
                
        }
        print(models$Model[i])
        #saving merged dataframe into csv files
        write.csv(qmlinearhist,paste0(dirname,"\\bias_corrected_historical_tasmax_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)   
        write.csv(qmlinear245,paste0(dirname,"\\bias_corrected_ssp245_tasmax_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)  
        write.csv(qmlinear585,paste0(dirname,"\\bias_corrected_ssp585_tasmax_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)
        

}

# write.csv(qmrquantssp585,paste0("bias_corrected_ssp585_",models$Model[1],"_",daterange_data,".csv"), row.names = TRUE)

## utility function.
## plots are easier to investigate if
## precipitation data are sqrt transformed