rm(list = ls())
library("tidyverse")
library("Metrics")
library("hydroGOF")

summary.performance<- NULL
summary.performance.all<-NULL
summary.performance.int<-NULL
models<-data.frame(read.csv(file = "models.csv",header = TRUE,sep = ",",nrows = 5))
add<-"Ensemble_pr"
models<-rbind(models,add)
for (k in (1:2)){
    
    for(j in (1:6)){
        
        if(k==1){
            daterange<-"1980-2014"
        }else{
            daterange<-"1990-2014"
        }
        
        fn.hist.model.bc<-paste0("BiasCorrected_",models$Model[j],"\\bias_corrected_historical_pr_",models$Model[j],"_",daterange,".csv")
        fn.hist.model.nc<-paste0("Data_pr\\historical\\prc_historical_",models$Model[j],"_",daterange,".csv")
        fn.historical.observed<-paste0("Data_pr\\Observed\\Observed_RainfallData_",daterange,".csv")
        
        historical.model.bc<-data.frame(read.csv(file = fn.hist.model.bc,header = TRUE,sep = ","))
        historical.model.bc$Date<-as.Date(historical.model.bc[[1]],format = "%Y-%m-%d")
        historical.model.bc<-historical.model.bc[order(historical.model.bc[[1]]),]
        historical.model.bc<-historical.model.bc[-ncol(historical.model.bc)]
        historical.model.nc<-data.frame(read.csv(file = fn.hist.model.nc,header = TRUE,sep = ","))
        historical.observed<-data.frame(read.csv(file = fn.historical.observed,header = TRUE,sep = ","))
        
        n<-ncol(historical.model.bc)-1
        
        stn_names<-as.vector(names(historical.model.bc)[2:(n+1)])
        stn_index<-1:n
        
        
        
        for(i in (stn_index)){
            print(stn_names[i])
            st.historical<-historical.model.bc[stn_names[i]]
            
            Model.Value.nc<-historical.model.nc[stn_names[i]]
            st.historical$Model.Value.nc<-Model.Value.nc[[1]]
            
            Observed.Value<-historical.observed[stn_names[i]]
            st.historical$Observed.Value<-Observed.Value[[1]]
            
            st.historical$Date<-historical.observed$Date
            colnames(st.historical)<-c("Model.Value.bc","Model.Value.nc","Observed.Value","Date")
            st.historical$Date<-as.Date(st.historical$Date,format = "%m/%d/%Y")
            # st.historical$Month<-lubridate::month(st.historical$Date)
            st.historical$YearMonth<-format(as.Date(st.historical$Date), "%m/%Y")
            
            monthwise<-st.historical%>%
                group_by(YearMonth)%>%
                summarize(MonthlyObserved=sum(Observed.Value,na.rm = TRUE),
                          MonthlyModel.bc = sum(Model.Value.bc,na.rm = TRUE),
                          MonthlyModel.nc =  sum(Model.Value.nc,na.rm = TRUE))
            monthwise$tempcol<-monthwise$YearMonth
            monthwise<-monthwise%>%separate(tempcol,into = c("Month","Year"),sep = -4,convert = TRUE)
            monthwise<-monthwise[-5]
            monthwise<-monthwise[order(monthwise$Year),]
     
            #Calculate Coefficient of Determination R2
            reg.bc <- lm(monthwise$MonthlyModel.bc~monthwise$MonthlyObserved)
            r2.bc <- summary(reg.bc)$r.squared

            reg.nc <- lm(monthwise$MonthlyModel.nc~monthwise$MonthlyObserved)
            r2.nc <- summary(reg.nc)$r.squared

            ##Calculate Root Mean Square Error RSME
            RMSE.bc <- Metrics::rmse(monthwise$MonthlyObserved,monthwise$MonthlyModel.bc)   ##available in hydroGOF package as well

            RMSE.nc <- Metrics::rmse(monthwise$MonthlyObserved,monthwise$MonthlyModel.nc)

            ##Calculate RMSE standard Deviation Ratio RSR
            RSR.bc <- hydroGOF::rsr(monthwise$MonthlyModel.bc,monthwise$MonthlyObserved)

            RSR.nc <- hydroGOF::rsr(monthwise$MonthlyModel.nc,monthwise$MonthlyObserved)

            ##Calculate Percentage of Bias PBIAS
            PBIAS.bc <- hydroGOF::pbias(monthwise$MonthlyModel.bc,monthwise$MonthlyObserved)

            PBIAS.nc <- hydroGOF::pbias(monthwise$MonthlyModel.nc,monthwise$MonthlyObserved)

            ##Calculate Nash SutCliffe Efficiency NSE
            nse.bc <- hydroGOF::NSE(monthwise$MonthlyModel.bc,monthwise$MonthlyObserved)

            nse.nc <- hydroGOF::NSE(monthwise$MonthlyModel.nc,monthwise$MonthlyObserved)

            smry.vec<-data.frame(models$Model[j],stn_names[i],r2.nc,r2.bc,RMSE.nc,RMSE.bc,RSR.nc,RSR.bc,PBIAS.nc,PBIAS.bc,nse.nc,nse.bc)
            summary.performance<-rbind(summary.performance,smry.vec)

         }
         summary.performance.int<-rbind(summary.performance.all,summary.performance)
         print(models$Model[j])
        
    }
    summary.performance.all<-rbind(summary.performance.all,summary.performance.int)
}
 colnames(summary.performance.all)<-c("Model","Station","R2Before","R2After","RMSEBefore","RMSEAfter", "RSRBefore","RSRAfter","PBIASBefore","PBIASAfter","NSEBefore","NSEAfter")

 average.summary<-summary.performance.all%>%
     group_by(Model)%>%
     summarise("R2 before"=mean(R2Before),
               "R2 after"= mean(R2After),
               "PBIAS before"=mean(PBIASBefore),
               "PBIAS after"=mean(PBIASAfter),
               "RMSE before"=mean(RMSEBefore),
               "RMSE after"= mean(RMSEAfter))
 # 
  write.csv(average.summary,"Model Summary.csv",row.names = FALSE)
  write.csv(summary.performance.all,"Summary of all station.csv",row.names = FALSE)
 # 
 # 
 # View(average.summary)
 # View(summary.performance.all)
 
 
 
 
 
 
 
 
 
 
