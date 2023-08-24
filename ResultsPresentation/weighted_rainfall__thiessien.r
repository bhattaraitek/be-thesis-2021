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
library("cowplot")
library("egg")
library("viridis")

setwd("D:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

scenarios<-c("ssp245","ssp585")
periods<-c("NF","MF","FF")

met_stn<-readOGR(dsn = "spatial_data","geda")
basins<-readOGR (dsn = "spatial_data","gedabasin")



seasonal.scenario2<-NULL
for (scenario in scenarios){
  seasonal.period3<-NULL
  for (period in periods){
    seasonal.period1<-as.data.frame(read.csv(paste0("SeasonalValues/PerChangeSeasonal/","Change_Seasonal_",scenario,"_",period,"_rainfall.csv"),header = T))
    seasonal.period1$Period<-period
    seasonal.period3<-rbind(seasonal.period3,seasonal.period1)
  }
  seasonal.period3$Scenario<-scenario
  seasonal.scenario2<-rbind(seasonal.scenario2,seasonal.period3)
}

colnames(seasonal.scenario2)<-c("IndexNo","Pre-Monsoon","Monsoon","Post-Monsoon","Winter","Period","Scenario")
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
  selected.season<-selected.scenario.values[selected.scenario.values$Season == "Monsoon",]
  selected.period<-selected.season[selected.season$Period == "NF",]
  View(selected.period)
  
  colnames(selected.scenario.values)<-c("IndexNo","x","y","z","period","scenario","season","perdeviation")
  #View(selected.scenario.values)

  print(scenario)

  clrs<-c("#D8BFD8","royalblue4","springgreen2","khaki","yellow","orange", "#FA8072","red2","#800000")

  data<-voronoi_polygon(data = selected.period,x =  "x" , y = "y", outline = basins.df)
  gata<-fortify_voronoi(data)
  
  # 
  # ggplot(selected.scenario.values)+
  #   
  #   geom_voronoi(mapping = aes(x = x, y = y,fill = perdeviation),outline = basins.df,show.legend = T)+
  #   geom_sf(data = sf_basin,color = "black",alpha = 0,lwd = 0.15)+
  #   
  #   scale_fill_stepsn(colours= clrs,
  #                     limits = c(-30,60),
  #                     breaks = seq(-30,60,10),name = "% Deviation") +
  #   theme_bw()+
  #   facet_wrap(~scenario)+
  #   facet_grid(fct_rev(period)~season)+
  #   theme(panel.background = element_blank(),legend.background = element_blank(),
  #         legend.box.background = element_blank())+
  #   theme(legend.position = "right", legend.key.width = unit(0.15,"in"),legend.key.height = unit(0.75,"in"),
  #         legend.text = element_text(size = 9))+
  #   
  #   theme(axis.text = element_text(face = "bold",colour = "black",size = 7,angle = 45,hjust = 1,vjust = 1))+
  #   theme(plot.margin = unit(c(0, 0, 0, 0), "in"))+
  #   theme(strip.text = element_text(face = "bold",colour = "black",size = 9))+
  #   north(sf_basin,scale = 0.275,symbol = 3)+
  #   scalebar(sf_basin, dist = 40, dist_unit = "km",transform = F, location = "bottomleft",
  #            st.size = 2.25,height = 0.045,border.size = 0.15,st.dist = 0.07)+
  #   xlab(NULL)+ylab(NULL)+
  #   coord_sf()
  #   print(paste("Plotting ",scenario))
  # 
}

 ssp245<-spatial_plot(scenario = "ssp245")
# ggsave(filename = paste0("SSP245_SEASONAL_SPATIAL_CHANGE",".png"),plot = last_plot(),
#        width = 11,height = 7 ,units = "in",dpi = 200)
# while (!is.null(dev.list()))  dev.off()
# print("Plotted SSP245")
# ssp585<-spatial_plot(scenario = "ssp585")
# ggsave(filename = paste0("SSP585_SEASONAL_SPATIAL_CHANGE",".png"),plot = last_plot(),
#        width = 11,height = 7 ,units = "in",dpi = 200)
# while (!is.null(dev.list()))  dev.off()
# print("Plotted SSP585")






























