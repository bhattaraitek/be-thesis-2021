library("tidyverse")
setwd("D:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

scenarios<-c("ssp245","ssp585")  
variable <-"Tmin" #rainfall Tmax Tmin
annual_average.all<-NULL
for (scenario in scenarios){
        
        annual_average.one<-as.data.frame(read.csv(paste0("YearlyChanges/",variable,"/","Change_",
                                                      scenario,"_",variable,".csv"),header = T))
        colnames(annual_average.one)[1]<-"IndexNo"
        annual_average.one$Scenario<-if(scenario == "ssp245"){"SSP245"}else{"SSP585"}
        annual_average.all<-rbind(annual_average.one,annual_average.all)
        
}
nar.ann.avg<-gather(annual_average.all,key = Period,value = Change,-IndexNo,-Scenario)
if (variable == "Tmax"){
        ylabs<-"Deviation (°C)"
        breaks<-seq(-10,10,2.5)
} else if (variable == "Tmin"){
        ylabs<-"Deviation (°C)"
        breaks<-seq(-10,10,2.5)
}else {
        ylabs <- "Deviation (%)"
        breaks<-seq(-40,100,20)
        }
order<-c("NF","MF","FF")

bplot<-ggplot(nar.ann.avg,aes(x = factor(Period,levels = order),y = Change))+
        geom_boxplot(aes(color = Scenario),outlier.colour = "black",lwd = 0.25, outlier.size = 0.6,outlier.shape = 4)+
        scale_color_manual(values = c("blue","red"))+
        theme_bw()+
        xlab("Period")+ylab(ylabs)+
        theme(legend.title = element_blank(),legend.background = element_blank(), legend.position = c(0.175,0.9),legend.text = element_text(size = 7.5),
              legend.key.size = unit(0.4,"cm"),panel.grid = element_blank())+
        theme(axis.title = element_text(face = "bold",size = 9),axis.text = element_text(face = "bold",size = 8,colour = "black"))+
        scale_y_continuous(breaks = breaks)
print(bplot)

ggsave(filename = paste0("boxplot_",variable,"-1",".png"),plot = last_plot(),width = 2.6667,height = 2,units = "in",dpi = 500)



