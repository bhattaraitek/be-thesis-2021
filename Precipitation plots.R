
#program to plot graphs for exploratory data analysis
#Missing data should be labeled as DNA
#created on 2021-Aug-8
# By Tek Narayan Bhattarai

rm(list= ls())
library("tidyverse") #to access charting and data manipulation tool
library("scales") # to access breaks/formatting functions
library("ggpmisc") # to access function to create regression model

#set your own working directory here
setwd( "E:/#3_Software Practice/R/DataAnalysis")
#set your data filename
filename<-"Prefilled-RainfallData_Raw"
#set file path
filepath<- paste0("E:/#3_Software Practice/R/DataAnalysis/Data/",filename,".csv")
raw_data<-data.frame(read.csv(file = filepath,header = TRUE,sep = ',',fill = TRUE,
               na.strings = "" ))

if(!dir.exists(paste0(filename))){dir.create(paste0(filename))}
sfname<-filename

stn_names<-as.vector(names(raw_data)[5:ncol(raw_data)]) #vector of stations name
attach(raw_data)

for (i in stn_names){
        cumvar=NULL
        sum_monthly=NULL
        sum_yearly=NULL
        tempdf_stnwise<- NULL
        naremoved_stnwise<-NULL
        stationwise<-NULL
        
        stationwise<-raw_data[,c("Month","Year","Day")]
        stationwise$Month<-match(stationwise$Month,month.abb)
        stationwise$Date <- as.Date(paste(stationwise$Year,stationwise$Month,
                                          stationwise$Day,sep = "-"))
        
        stationwise<-within(data = stationwise,{assign(paste(i),get(i))})
        stn<-paste(i)
        naremoved_stnwise<-na.omit(stationwise)  #removing blank cells
        tempdf_stnwise$value<-naremoved_stnwise[[i]]
        tempdf_stnwise<-data.frame(tempdf_stnwise)
        tempdf_stnwise[tempdf_stnwise=="DNA"]<-NA
        naremoved_stnwise[[i]]<-tempdf_stnwise$value
        
        cumvar<-naremoved_stnwise[[i]]   #temporary dataframe for cumulative calculation
        cumvar[is.na(cumvar)]<-0
        naremoved_stnwise$Cumulative<- cumsum(cumvar)
        naremoved_stnwise[[i]]=as.numeric(naremoved_stnwise[[i]])
        naremoved_stnwise$Cumulative<-as.numeric(naremoved_stnwise$Cumulative)
        
# ----------------------------------------------------------------------------
#                    Daily Rainfall Hyetograph
# ----------------------------------------------------------------------------
        ggplot(data = naremoved_stnwise)+
                aes(Date,get(stn))+
                geom_bar( stat = "identity",width = 15,fill= "blue")+
                xlab("Date")+ ylab("Precipitation (mm)")+
                theme(axis.title = element_text(face = "bold",size = 5))+
                labs(caption="(a)")+
                theme(plot.caption = element_text(hjust = 0.5))+
                
                scale_y_continuous(limits = c(0,max(naremoved_stnwise[[i]],na.rm = TRUE)+25),
                                   breaks = seq(0, max(naremoved_stnwise[[i]],na.rm = TRUE),50),
                                   expand = c(0,0)) + 
                scale_x_date(date_breaks = "3 years" , date_labels = "%Y-%m-%d")+
                theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
                theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
                theme(axis.ticks.length = unit(0.75,"mm"),
                      axis.ticks = element_line(size = 0.1))+
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black",size = 0.25))
        figname<-paste0(sfname,"/",i,"-(a)-DailyHyetograph.png")
        
        ggsave(filename = figname,plot = last_plot(),dpi = 300,
               width = 3.1,height = 2.1,units = "in")
        while (!is.null(dev.list()))  dev.off()
        
# ---------------------------------------------------------------------------
#                    Single Mass Curve
# ----------------------------------------------------------------------------

        ggplot(data = naremoved_stnwise)+
                aes(Date,Cumulative)+
                geom_line(stat = "identity",size=0.25, color ="blue")+
                xlab("Date")+ ylab("Cumulative precipitation (mm)")+
                theme(axis.title = element_text(face = "bold",size = 5))+
                labs(caption="(b)")+
                theme(plot.caption = element_text(hjust = 0.5))+
                scale_y_continuous(limits=c(0,NA),
                                   breaks = seq(0,max(naremoved_stnwise$Cumulative)+10000,10000),
                                   expand = c(0,0))+
                scale_x_date(date_breaks = "3 years",date_labels = "%Y-%m-%d")+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))+
                theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
                theme(axis.ticks.length = unit(0.75,"mm"),
                      axis.ticks = element_line(size = 0.1))+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line("black",size = 0.25))+
                geom_smooth(method = lm,formula = y~x,color="red",
                            linetype="dashed",size= 0.25)+
                stat_poly_eq(formula = y~x,
                             aes(label = paste(..rr.label.., sep = "~~~~")),
                             small.r = TRUE,
                             rr.digits = 4,
                             parse = TRUE)
        figname<-paste0(sfname,"/",i,"-(b)-SingleMassCurve.png")
        ggsave(filename = figname,plot = last_plot(),dpi = 300,
               width = 3.1,height = 2.1,units = "in")
        while (!is.null(dev.list()))  dev.off()


# ----------------------------------------------------------------------------
#                SUmmarizing Data Yearly
# ----------------------------------------------------------------------------

        sum_yearly<-naremoved_stnwise%>%
                group_by(Year)%>%
                summarize(TotalRainfall=sum(get(stn),na.rm= TRUE))
        sum_yearly$Year<-as.numeric(sum_yearly$Year)
        sum_yearly$TotalRainfall[sum_yearly$TotalRainfall==0]<-NA
        NoY<-nrow(sum_yearly)

        ggplot(data = sum_yearly)+
                geom_line(mapping= aes(x =Year,y = TotalRainfall,group=1),
                          stat = "identity",size= 0.5,color="blue")+
                xlab("Year")+ ylab("Total precipitation (mm)")+
                theme(axis.title = element_text(face = "bold",size = 5))+
                labs(caption="(d)")+
                theme(plot.caption = element_text(hjust = 0.5))+
                scale_y_continuous(limits= c(0,NA),
                                   breaks  = seq(0,max(sum_yearly$TotalRainfall,
                                                       na.rm = TRUE)+600,300),
                                   expand = c(0,0))+
                scale_x_continuous(breaks= seq(min(sum_yearly$Year)-2,
                                               max(sum_yearly$Year)+2,3))+
                theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
                theme(axis.ticks.length = unit(0.75,"mm"),
                      axis.ticks = element_line(size = 0.1))+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                                 hjust = 1,colour = "black"))+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black",size =0.25 ))

        figname<-paste0(sfname,"/",i,"-(d)-YearlyCumulative.png")

        ggsave(filename = figname,plot = last_plot(),dpi = 300,
               width = 3.1,height = 2.1,units = "in")
        while (!is.null(dev.list()))  dev.off()




# ----------------------------------------------------------------------------
#                    Summarizing Data Monthly
# ----------------------------------------------------------------------------

        sum_monthly<-naremoved_stnwise%>%
                group_by(Month)%>%
                summarize(TotalRainfall=sum(get(stn),na.rm = TRUE))
        sum_monthly$AvgRainfall<-sum_monthly$TotalRainfall/NoY
        sum_monthly$Month<-month.abb[sum_monthly$Month]

        ggplot(data = sum_monthly)+
                aes(Month,AvgRainfall)+
                geom_bar( stat = "identity",width = 0.5,
                          position = position_dodge(width =5),
                          fill= "blue")+
                xlab("Month")+ ylab("Average precipitation (mm)")+
                theme(axis.title = element_text(face = "bold",size = 5))+
                labs(caption="(c)")+
                theme(plot.caption = element_text(hjust = 0.5))+
                scale_y_continuous(breaks = seq(0,max(sum_monthly$AvgRainfall)+50,50),
                                   expand = c(0,0)) +
                scale_x_discrete(limits = month.abb)+
                theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
                theme(axis.ticks.length = unit(0.75,"mm"),
                      axis.ticks = element_line(size = 0.1))+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black",size = 0.25))

        figname<-paste0(sfname,"/",i,"-(c)-MonthlyCumulative.png")

        ggsave(filename = figname,plot = last_plot(),dpi = 300,
               width = 3.1,height = 2.1,units = "in")
        while (!is.null(dev.list()))  dev.off()

}








