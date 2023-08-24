#created by @Tek 2022 Feb 13
library("rgdal")
library("sf")
library("tidyverse")
library("ggrepel")
library("ggforce")
library("RColorBrewer")
library("ggsn")
library("readxl")
library("ggvoronoi")
#library("cowplot")
library("egg")
library("viridis")

setwd("E:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

variable<-"tasmax" #rainfall tasmax tasmin

scenarios<-c("SSP245", "SSP585")
periods<-c("NF","MF","FF")

met_stn<-readOGR(dsn = "spatial_data","<filename>")
basins<-readOGR (dsn = "spatial_data","<filename>")



seasonal.scenario2<-NULL
for (scenario in scenarios){
  seasonal.period3<-NULL
  for (period in periods){
    seasonal.period1<-as.data.frame(read.csv(paste0("SeasonalValues/PerChangeSeasonal/","Change_Seasonal_",scenario,"_",period,"_",variable,".csv"),header = T))
    seasonal.period1$Period<-period
    seasonal.period3<-rbind(seasonal.period3,seasonal.period1)
  }
  seasonal.period3$Scenario<-scenario
  seasonal.scenario2<-rbind(seasonal.scenario2,seasonal.period3)
}
if (variable == "rainfall"){
  colnames(seasonal.scenario2)<-c("IndexNo","Pre-Monsoon","Monsoon","Post-Monsoon","Winter","Period","Scenario")
}else{
  colnames(seasonal.scenario2)<-c("IndexNo","Pre-Monsoon","Monsoon","Post-Monsoon","Winter","Period","Scenario")
}


# colnames(seasonal.ppt)[1]<-"Station"


stations.df<-as(met_stn,"data.frame")
stations.df[,4:11]<-NULL
# points.df[,1:2]<-NULL
colnames(stations.df)<-c("ID","Name","Index", "x","y","z")
stationsWithValues<-merge(x = stations.df,y= seasonal.scenario2,by.x = "Index",by.y="IndexNo")
stationsWithValues[,2:3]<-NULL


sf_basin<-st_as_sf(basins)
basins.df<-as_Spatial(sf_basin)

narrow.df<-gather(stationsWithValues,key = "Season",value = "PerDeviation",-Index,-x,-y,-z,-Scenario,-Period)

spatial_plot<-function(basins = sf_basin,stations = narrow.df,scenario){
  basins.df<-as_Spatial(basins)
  selected.scenario.values<-stations[stations$Scenario == scenario,]
  #View(selected.scenario.values)
  colnames(selected.scenario.values)<-c("IndexNo","x","y","z","period","scenario","season","perdeviation")
  if (variable == "rainfall"){
     selected.scenario.values$season<-factor(selected.scenario.values$season,levels = c("Pre-Monsoon","Monsoon","Post-Monsoon","Winter"))
     lw<-"#FF0000"
       md<-"#E0FFFF"
       hgh<-"#00008B"
       lmts<-c(-60,60)
       brks<-seq(-60,60,15)
       nm<-"Deviation(%)"
  }else{
    selected.scenario.values$season<-factor(selected.scenario.values$season,levels = c("Pre-Monsoon","Monsoon","Post-Monsoon","Winter"))
    hgh<-"#FF0000"
    md<- "#E0FFFF"
    lw<-"#00008B"
    lmts<-c(-5,10)
    brks<-c(0,1,2,4,6)
    nm<-"Deviation(°C)"
  }

  #View(selected.scenario.values)

  print(scenario)

  clrs<-c("#D8BFD8","royalblue4","springgreen2","khaki","yellow","orange", "#FA8072","red2","#800000")

  
    sp<-ggplot(selected.scenario.values)+
    
    geom_voronoi(mapping = aes(x = x, y = y,fill = perdeviation),outline = basins.df,show.legend = if(scenario == "SSP585"){TRUE}else{T})+
    geom_sf(data = sf_basin,color = "black",alpha = 0,lwd = 0.15)+
    
    # scale_fill_stepsn(colours= clrs,
    #                   limits = c(-30,60),
    #                   breaks = seq(-30,60,10),name = "% Deviation") +
    scale_fill_steps2(low = lw ,mid = md , high = hgh,midpoint = -2,limits = lmts,
                     breaks = brks, name = nm)+
    
    theme_bw()+
    facet_wrap(~scenario)+
    facet_grid(fct_rev(period)~season)+
    theme(panel.background = element_blank(),legend.background = element_blank(),
          legend.box.background = element_blank(),panel.grid = element_blank())+
    
    theme(legend.position = "bottom", legend.key.width = unit(0.5,"in"),legend.key.height = unit(0.1,"in"),
          legend.text = element_text(size = 9))+
    
    theme(axis.text.y = element_text(face = "bold",colour = "black",size = 8,angle = 90,hjust = 0.5,vjust = 0.5))+
    theme(axis.text.x = element_text(face = "bold",colour = "black",size = 8))+
    
    theme(plot.margin = unit(c(0, 0, 0, 0), "in"))+
    theme(strip.text = element_text(face = "bold",colour = "black",size = 11))+
    scale_x_continuous(breaks = c(80.5,82,83.5))+scale_y_continuous(breaks = c(28,29))+
    north(sf_basin,scale = 0.3,symbol = 3)+
    scalebar(sf_basin, dist = 50, dist_unit = "km",transform = F, location = "bottomleft",
             st.size = 2,height = 0.07,border.size = 0.15,st.dist = 0.08,st.bottom = F)+
    xlab(NULL)+ylab(NULL)+
    coord_sf()
    print(paste("Plotting ",scenario))
    return(sp)
    
  
}

# ssp245<-spatial_plot(scenario = "ssp245")
# ggsave(filename = paste0("SSP245_SEASONAL_SPATIAL_CHANGE_",variable,".png"),plot = last_plot(),
#        width = 7,height = 4 ,units = "in",dpi = 300)
# while (!is.null(dev.list()))  dev.off()
# print("Plotted SSP245")
# ssp585<-spatial_plot(scenario = "ssp585")
# ggsave(filename = paste0("SSP585_SEASONAL_SPATIAL_CHANGE",".png"),plot = last_plot(),
#        width = 7,height = 4,units = "in",dpi = 300)
# while (!is.null(dev.list()))  dev.off()
# print("Plotted SSP585")





for( sn in scenarios){
  ggsave(filename = paste0(sn,"June30_SEASONAL_SPATIAL_CHANGE_",variable,".png"),plot = spatial_plot(scenario = sn),
         width = 7,height = 3.5 ,units = "in",dpi = 500)
  while (!is.null(dev.list()))  dev.off()
}
























