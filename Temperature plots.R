#--------------------------------------------------------------------------------
#Program to plot graphs for exploratory data analysis(Temperature)
#Missing data should be labeled as DNA 
#created on 2021-Aug-13
#by Tek Narayan Bhattarai
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

rawdf<-data.frame(read.csv(file = filepath,header = TRUE,sep = ',',fill = TRUE,
               na.strings = "" ))


stn_names<-seq(5,ncol(rawdf),2)
colnam<-colnames(rawdf)[5:ncol(rawdf)] #vector of stations name
attach(rawdf)

for (i in stn_names){
  cumvar=NULL
  sum_monthly=NULL
  sum_yearly=NULL
  naremove.df<- NULL
  dataextract.df<-NULL
  filtered.df<-NULL
  
  dataextract.df<-rawdf[,c("Month","Year","Day")]
  dataextract.df$Month<-match(dataextract.df$Month,month.abb)
  dataextract.df$Date <- as.Date(paste(dataextract.df$Year,dataextract.df$Month,
                                       dataextract.df$Day,sep = "-"))
  
  dataextract.df$Tmax<-rawdf[[i]]
  dataextract.df$Tmin<-rawdf[[i+1]]
  stn<-substr(colnam[i-4],1,4)
  filtered.df<-na.omit(dataextract.df)  #removing blank cells
  naremove.df$Tmax<-filtered.df[[5]]
  naremove.df$Tmin<-filtered.df[[6]]
  naremove.df<-data.frame(naremove.df)
  naremove.df[naremove.df=="DNA"]<-NA
  
  filtered.df[[5]]<-as.numeric(naremove.df$Tmax)
  filtered.df[[6]]<-as.numeric(naremove.df$Tmin)
  filtered.df$Tmax[filtered.df$Tmax>60]<-NA
  
  
 
  #------------------------------------------------------------
  #             Daily temperature variations
  #------------------------------------------------------------
  colors <- c( Tmax = "red", Tmin = "blue")
    ggplot(data = filtered.df,aes(x = Date))+
    geom_line(mapping = aes(y = Tmax,color = "Tmax"),stat = "identity",
              size=0.2)+
    geom_line(mapping = aes(y = Tmin,color= "Tmin"),stat = "identity",
               size= 0.2)+
    labs(x="Date",y="Temperature ('C)",color="Legend",caption ="(a)")+
    scale_color_manual(values = colors)+
    theme(axis.title = element_text(face = "bold",size =5 ))+
    theme(plot.caption = element_text(hjust = 0.5))+
    scale_y_continuous(limits = c(-10,max(filtered.df$Tmax,na.rm = TRUE)+5),
                       breaks = seq(-100,max(filtered.df$Tmax,
                                             na.rm = TRUE)+5,5),
                       expand= c(0,0))+
 
    theme(legend.position =c(0.5,1),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.justification = "center",
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(-7,-7,-7,-7),
          legend.text = element_text(face = "bold",size = 4),
          legend.key.size = unit(0.3,"cm"),
          legend.key = element_blank())+
    scale_x_date(date_breaks = "3 years" , date_labels = "%Y-%m-%d")+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
    theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
    theme(axis.ticks.length = unit(0.75,"mm"),
         axis.ticks = element_line(size = 0.1))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black",size =0.25))
  figname<-paste0(sfname,"/",stn,"-(a)-DailyTemperatureVariations.png")
  ggsave(filename = figname,plot = last_plot(),dpi = 300,
         width = 3.1,height = 2.1,units = "in")
  while (!is.null(dev.list()))  dev.off()

  
  #----------------------------------------------------------
  #          Monthly average Tmax and Tmin
  #----------------------------------------------------------

  summary_monthly<-filtered.df%>%
    group_by(Month)%>%
    summarize(AvgTmax=mean(Tmax,na.rm = TRUE),AvgTmin=mean(Tmin,na.rm = TRUE))
  summary_monthly$Month<-month.abb[summary_monthly$Month]
  nar_sumry_monthly<-gather(data = summary_monthly,key = Type,value = Temperature,-Month)
  
  ggplot(data = nar_sumry_monthly,aes(x = Month))+
    geom_bar( aes(y = Temperature,fill= factor(Type)),
              stat = "identity",width = 0.5,
              position = "dodge2")+
    
    labs(x="Month",y="Temperature ('C)",color="Legend",caption ="(b)")+
    theme(axis.title = element_text(face = "bold",size = 5))+
    theme(plot.caption = element_text(hjust = 0.5))+
    scale_y_continuous(limits=c(0,max(summary_monthly$AvgTmax)+5), 
                       breaks = seq(0,max(summary_monthly$AvgTmax)+5,5),
                       expand = c(0,0)) +
    scale_x_discrete(limits = month.abb)+
    theme(legend.position =c(0.5,1),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.justification = "center",
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(-7,-7,-7,-7),
          legend.text = element_text(face = "bold",size = 4),
          legend.key.size = unit(0.3,"cm"),
          legend.key = element_blank())+    

    theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
    theme(axis.ticks.length = unit(0.75,"mm"),
        axis.ticks = element_line(size = 0.1))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black",size = 0.25))+
    scale_fill_manual(values = c('red','blue'))
  
  figname<-paste0(sfname,"/",stn,"-(b)-MonthlyAvgTemp.png")
  ggsave(filename = figname,plot = last_plot(),dpi = 300,
          width = 3.1,height = 2.1,units = "in")
  while (!is.null(dev.list()))  dev.off()
  
  
  #----------------------------------------------------------
  #                Yearly Tmax and Tmin
  #----------------------------------------------------------
  
  summary_yearly<-filtered.df%>%
    group_by(Year)%>%
    summarize(TempMax=max(Tmax,na.rm= TRUE),TempMin=min(Tmin,na.rm = TRUE))
  sum_yearly$Year<-as.numeric(sum_yearly$Year)
  summary_yearly[sapply(summary_yearly,is.infinite)]<-NA
  
  ggplot(data = summary_yearly,aes(x = Year))+
    geom_line(mapping= aes(y = TempMax,color="Tmax"),
              stat = "identity",size= 0.3)+
    geom_line(mapping = aes(y = TempMin,color="Tmin"),
              stat = "identity",size=0.3)+
    labs(x="Year",y="Temperature ('C)",color="Legend",caption = "(c)")+
    scale_color_manual(values = colors)+
    theme(legend.position =c(0.5,1),
                legend.direction = "horizontal",
                legend.title = element_blank(),
                legend.justification = "center",
                legend.margin = margin(0,0,0,0),
                legend.box.margin = margin(-7,-7,-7,-7),
                legend.text = element_text(face = "bold",size = 4),
                legend.key.size = unit(0.3,"cm"),
                legend.key = element_blank())+
          
    theme(axis.text = element_text(size = 4,color = "black",face = "bold"))+
    theme(axis.ticks.length = unit(0.75,"mm"),
          axis.ticks = element_line(size = 0.1))+
    
    theme(axis.title = element_text(face = "bold",size = 5),
          plot.caption = element_text(hjust = 0.5))+
    scale_y_continuous(limits= c(-10,max(summary_yearly$TempMax,na.rm = TRUE)+5),
                       breaks  = seq(-10, max(summary_yearly$TempMax,na.rm = TRUE)+5,5),
                       expand = c(0,0))+
    scale_x_continuous(breaks= seq(min(summary_yearly$Year)-2,
                                   max(summary_yearly$Year)+2,3))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black",size = 0.25))
  
  figname<-paste0(sfname,"/",stn,"-(c)-YearlyMax-MinTemp.png")
  
  ggsave(filename = figname,plot = last_plot(),dpi = 300,
         width = 3.1,height = 2.1,units = "in")
  while (!is.null(dev.list()))  dev.off()
  
}



        
        
        
        


        