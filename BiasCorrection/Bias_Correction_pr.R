rm(list=ls())
setwd("D:\\#2_Project Works\\Climate Projection\\Bias Correction")

library("qmap")
library("tidyverse")


models<-data.frame(read.csv(file = "models.csv",header = TRUE,sep = ",",nrows = 5))
add<-"Ensemble_pr"
models<-rbind(models,add)

#reading observed rainfall data
daterange_data<-"1990-2014"
obs_filename<-paste0("Data_pr\\Observed\\Observed_RainfallData_",daterange_data,".csv")
observed_ppt<-data.frame(read.csv(file = obs_filename,header = TRUE,sep = ","))
observed_ppt$Date<-as.Date(observed_ppt$Date,format = "%m/%d/%Y")
observed_ppt$Month<-lubridate::month(observed_ppt$Date)
row.names(observed_ppt)<-observed_ppt$Date
observed_ppt$Date<-NULL
 
for(i in (6:6)){
        if (!dir.exists(paste0("BiasCorrected_",models$Model[i]))) {dir.create(paste0("BiasCorrected_",models$Model[i]))}
        dirname<-paste0("BiasCorrected_",models$Model[i])
        
        ########### model historical data
        model_histppt<-NULL
        hist_filename<-paste0("Data_pr\\historical\\prc_historical_",models$Model[i],"_",daterange_data,".csv")
        model_histppt <-data.frame(read.csv(file = hist_filename,header = TRUE,sep = ",")) 
        model_histppt$Date<-as.Date(model_histppt$Date,format = "%m/%d/%Y")
        model_histppt$Month<-lubridate::month(model_histppt$Date)
        row.names(model_histppt) <- model_histppt$Date     ## change row name as date
        model_histppt$Date = NULL
        
        ########### ssp245 data ############
        scenario_ssp245<-NULL
        ssp245_filename<-paste0("Data_pr\\ssp245\\prc_ssp245_",models$Model[i],"_",daterange_data,".csv")
        scenario_ssp245 <-data.frame(read.csv(file = ssp245_filename,header = TRUE,sep = ",",fill = TRUE))
        scenario_ssp245$Date<-as.Date(scenario_ssp245$Date,format = "%m/%d/%Y")
        scenario_ssp245$Month<-lubridate::month(scenario_ssp245$Date)
        row.names(scenario_ssp245) <- scenario_ssp245$Date     ## change row name as date
        scenario_ssp245$Date = NULL
        
        ########### ssp585 data ##########
        scenario_ssp585<-NULL
        ssp585_filename<-paste0("Data_pr\\ssp585\\prc_ssp585_",models$Model[i],"_",daterange_data,".csv")
        scenario_ssp585 <-data.frame(read.csv(file = ssp585_filename,header = TRUE,sep = ","))
        scenario_ssp585$Date<-as.Date(scenario_ssp585$Date,format = "%m/%d/%Y")
        scenario_ssp585$Month<-lubridate::month(scenario_ssp585$Date)
        row.names(scenario_ssp585) <- scenario_ssp585$Date     ## change row name as date
        scenario_ssp585$Date = NULL
        
        # assuming empty dataframes to store all data
        qmrquanthist<-NULL
        qmrquantssp245<-NULL
        qmrquantssp585<-NULL
        
        #Month == month_num[j] |Month ==j |Month == month_num[j+2]
        
        
        month_num<-c(12,1:12,1)
        for(j in (1:12)){
                
                # pooling daily time series data in the window of three month
                observed_ppt_monthly<-subset(observed_ppt,Month == j)
                model_histppt_monthly<-subset(model_histppt,Month == j)
                scenario_ssp245_monthly<-subset(scenario_ssp245,Month == j)
                scenario_ssp585_monthly<-subset(scenario_ssp585,Month == j)
                
                
                
                observed_ppt_monthly<-observed_ppt_monthly[-ncol(observed_ppt_monthly)]
                model_histppt_monthly<-model_histppt_monthly[-ncol(model_histppt_monthly)]
                scenario_ssp245_monthly<-scenario_ssp245_monthly[-ncol(scenario_ssp245_monthly)]
                scenario_ssp585_monthly<-scenario_ssp585_monthly[-ncol(scenario_ssp585_monthly)]

                
                # applying robust quantile mapping technique
                qm.fit.monthly <- fitQmapRQUANT(observed_ppt_monthly,model_histppt_monthly,
                                        qstep=0.01,nboot=10,wet.day = 1, type = "linear")
                qmrquanthist.monthly <- doQmapRQUANT(model_histppt_monthly,qm.fit.monthly,type="linear",wet.day = 1)
                qmrquantssp245.monthly <- doQmapRQUANT(scenario_ssp245_monthly,qm.fit.monthly,type="linear",wet.day = 1)
                qmrquantssp585.monthly <- doQmapRQUANT(scenario_ssp585_monthly,qm.fit.monthly,type="linear",wet.day = 1)

                qmrquanthist<-rbind(qmrquanthist,qmrquanthist.monthly)
                qmrquantssp245<-rbind(qmrquantssp245,qmrquantssp245.monthly)
                qmrquantssp585<-rbind(qmrquantssp585,qmrquantssp585.monthly)
                  
                
                
                
                #delta qualtilemapping
                # qm.fit.monthly<-fitQmapQUANT(observed_ppt_monthly,model_histppt_monthly,qstep = 0.01,
                #                              nboot =1 ,wet.day=TRUE,type = "tricub")
                # qmrquanthist.monthly <- doQmapQUANT(model_histppt_monthly,qm.fit.monthly,type="tricub")
                # qmrquantssp245.monthly <- doQmapQUANT(scenario_ssp245_monthly,qm.fit.monthly,type="tricub")
                # qmrquantssp585.monthly <- doQmapQUANT(scenario_ssp585_monthly,qm.fit.monthly,type="tricub")
                # 
                # qmrquanthist<-rbind(qmrquanthist,qmrquanthist.monthly)
                # qmrquantssp245<-rbind(qmrquantssp245,qmrquantssp245.monthly)
                # qmrquantssp585<-rbind(qmrquantssp585,qmrquantssp585.monthly)
        }
        write.csv(qmrquanthist,paste0(dirname,"\\feb14bias_corrected_historical_pr_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)   ###########################write to csv
        write.csv(qmrquantssp245,paste0(dirname,"\\feb14bias_corrected_ssp245_pr_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)   ###########################write to csv
        write.csv(qmrquantssp585,paste0(dirname,"\\feb14bias_corrected_ssp585_pr_",models$Model[i],"_",daterange_data,".csv"), row.names = TRUE)

        print(models$Model[i])
      
}

# write.csv(qmrquantssp585,paste0("bias_corrected_ssp585_",models$Model[1],"_",daterange_data,".csv"), row.names = TRUE)

## utility function.
## plots are easier to investigate if
## precipitation data are sqrt transformed