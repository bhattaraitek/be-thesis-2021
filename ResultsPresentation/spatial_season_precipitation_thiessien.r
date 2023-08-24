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

seasonal.historical<-as.data.frame(read_excel("SeasonalValues/Seasonal_historical_rainfall.xlsx"))
seasonal.historical$Period<-"Observed"
seasonal.historical$Scenario<-"Observed"

seasonal.scenario2<-NULL
for (scenario in scenarios){
  seasonal.period3<-NULL
  for (period in periods){
    seasonal.period1<-as.data.frame(read_excel(paste0("SeasonalValues/","Seasonal_",scenario,"_",period ,"_rainfall.xlsx")))
    seasonal.period1$Period<-period
    seasonal.period3<-rbind(seasonal.period3,seasonal.period1)
  }
  seasonal.period3$Scenario<-scenario
  seasonal.scenario2<-rbind(seasonal.scenario2,seasonal.period3)
}
seasonal.scenario2<-rbind(seasonal.historical,seasonal.scenario2)
colnames(seasonal.scenario2)<-c("IndexNo","Pre-Monsoon","Monsoon","Post-Monsoon","Winter","Period","Scenario")
# colnames(seasonal.ppt)[1]<-"Station"


stations.df<-as(met_stn,"data.frame")
stations.df[,4:11]<-NULL
# points.df[,1:2]<-NULL
colnames(stations.df)<-c("ID","Name","Index", "x","y","z")
stationsWithValues<-merge(x = stations.df,y= seasonal.scenario2,by.x = "Index",by.y="IndexNo")
stationsWithValues[,2:3]<-NULL


# sf_metstn<-st_as_sf(met_stn)
sf_basin<-st_as_sf(basins)
basins.df<-as_Spatial(sf_basin)
# sf_metstn[,4:11]<-NULL
# 
# sf_metstn<-merge(x = sf_metstn,y= seasonal.ppt, by.x = "Station",by.y="IndexNo")
narrow.df<-gather(stationsWithValues,key = "Season",value = "Precipitation",-Index,-x,-y,-z,-Scenario,-Period)

narrow.df.hist<-narrow.df[narrow.df$Scenario == "Observed",]
narrow.df.ssp245<-narrow.df[narrow.df$Scenario == "ssp245",]
narrow.df.ssp585<-narrow.df[narrow.df$Scenario == "ssp585",]

colnames(narrow.df.hist)<-c("IndexNo","x","y","z","period1","scenario1","season1", "Precipitation")
colnames(narrow.df.ssp245)<-c("IndexNo","x","y","z","period2","scenario2","season2", "Precipitation")
colnames(narrow.df.ssp585)<-c("IndexNo","x","y","z","period3","scenario3","season3", "Precipitation")

lw<-"#FF0000"
md<-"#E0FFFF"
hgh<-"#00008B"

historical<- ggplot(narrow.df.hist)+
  
  geom_voronoi(mapping = aes(x = x, y = y,fill = Precipitation),outline = basins.df,show.legend = T)+
  geom_sf(data = sf_basin,color = "black",alpha = 0)+
  geom_point(data = stationsWithValues, aes(x = x, y = y),pch = 17, size = 0.5)+
  scale_fill_steps2(low = lw,high = hgh,mid = md,midpoint = 0,
                    breaks = c(0,100,200,500,800,1200,1400,1600,1800), name = NULL)+
  # scale_fill_steps2(low = "blue", mid = "green",high = "red", guide = "colorbar")+
  
  # scale_fill_stepsn(colours = c("#87CEFA","#0000FF","#98FB98","#7CFC00","#006400","#EEE8AA","#B8860B","#FF8C00","#FA8072","#FF0000","#800000"),
  #                     # c("dodgerblue2", "#E31A1C", # red
  #                     #           "green4",
  #                     #           "#6A3D9A", # purple
  #                     #           "#FF7F00", # orange
  #                     #           "black", "gold1",
  #                     #           "skyblue2", "#FB9A99", # lt pink
  #                     #           "palegreen2",
  #                     #           "#CAB2D6", # lt purple
  #                     #           "#FDBF6F", # lt orange
  #                     #           "gray70", "khaki2",
  #                     #           "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  #                     #           "darkturquoise", "green1", "yellow4", "yellow3",
  #                     #           "darkorange4", "brown"),
  #                   # c( "dodgerblue2","lightblue","blue","green","green4","#FDBF6F","orange","red","red2"),
  #                   limits = c(0,1800),
  #                   breaks =c(0,50,100,150, seq(200,1000,300),seq(1100,2000,200)) ,name = "Pr (mm)")+
  # 
  facet_grid(period1~season1)+
  theme_bw()+
  #ggtitle("historical")+
  
  theme(legend.position = "bottom", legend.key.width = unit(1,"in"),legend.key.height = unit(0.1,"in"),
        legend.text = element_text(size = 9),panel.grid = element_blank())+
  
  theme(axis.text.y = element_text(face = "bold",colour = "black",size = 8,angle = 90,hjust = 0.5,vjust = 0.5))+
  theme(axis.text.x = element_text(face = "bold",colour = "black",size = 8))+
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "in"))+
  theme(strip.text = element_text(face = "bold",colour = "black",size = 11))+
  scale_x_continuous(breaks = c(80.5,82,83.5))+scale_y_continuous(breaks = c(28,29))+
  north(sf_basin,scale = 0.3,symbol = 3)+
  scalebar(sf_basin, dist = 45, dist_unit = "km",transform = F, location = "bottomleft",
           st.size = 2.15,height = 0.07,border.size = 0.15,st.dist = 0.08,st.bottom = F)+
  xlab(NULL)+ylab(NULL)+
  coord_sf()





# ssp245<- ggplot(narrow.df.ssp245)+
#   
#   geom_voronoi(mapping = aes(x = x, y = y,fill = Precipitation),outline = basins.df,show.legend = F)+
#   geom_sf(data = sf_basin,color = "black",alpha = 0)+
#   geom_point(data = stationsWithValues, aes(x = x, y = y),pch = 17, size = 0.5)+
#   scale_fill_steps(low = "yellow",high = "blue", name = NULL)+
#   
#   # scale_fill_stepsn(colours = c("#87CEFA","#0000FF","#98FB98","#7CFC00","#006400","#EEE8AA","#B8860B","#FF8C00","#FA8072","#FF0000","#800000"),
#   #                     #c("skyblue3","royalblue4","springgreen2","orange","gold1","yellow","purple", "hotpink2","red2"),
#   #                     # viridis(18),
#   #   #                   c("khaki1","wheat1","skyblue3","slateblue2","royalblue4", "purple3","magenta4","mediumseagreen",
#   #   #                             "springgreen2","olivedrab2","plum1","lightsalmon2", "indianred2","hotpink2","orangered2","red2","red4"),
#   #   # # "gray80","wheat2","rosybrown2","honeydew3","olivedrab2",
#   #   # #                             "palegreen3","yellow3","orange3",""
#   #   # "dodgerblue2",
#   #   # "dodgerblue2", "#E31A1C", # red
#   #   #                             "green4",
#   #   #                             "#6A3D9A", # purple
#   #   #                             "#FF7F00", # orange
#   #   #                             "black", "gold1",
#   #   #                             "skyblue2", "#FB9A99", # lt pink
#   #   #                             "palegreen2",
#   #   #                             "#CAB2D6", # lt purple
#   #   #                             "#FDBF6F", # lt orange
#   #   #                             "gray70", "khaki2",
#   #   #                             "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
#   #   #                             "darkturquoise", "green1", "yellow4", "yellow3",
#   #   #                             "darkorange4", "brown"),
#   #                   # "lightblue","blue","cyan","orange","green","purple","pink","tomato3","red"),
#   #                   limits = c(0,1800),
#   #                   breaks =c(0,50,100,150, seq(200,1000,300),seq(1100,2000,200)) ,name = "Pr (mm)")+
#   
#   facet_grid(fct_rev(period2)~season2)+
#   theme_bw()+
#   
#   theme(panel.background = element_blank())+
#   ggtitle("ssp245")+
#   theme(panel.background = element_blank())+
#   theme(legend.position = "bottom", legend.key.width = unit(1.25,"in"),legend.key.height = unit(0.2,"in"))+
#   
#   theme(axis.text = element_text(face = "bold",colour = "black",size = 7,angle = 45,hjust = 1,vjust = 1))+
#   theme(strip.text = element_text(face = "bold",colour = "black",size = 8))+
#   north(sf_basin,scale = 0.2,symbol = 3)+
#   scalebar(sf_basin, dist = 40, dist_unit = "km",transform = F, location = "bottomleft",
#            st.size = 2,height = 0.045,border.size = 0.15,st.dist = 0.07)+
#   xlab(NULL)+ylab(NULL)+
#   
#   
#   coord_sf()
# 
# 
# 
# ssp585<- ggplot(narrow.df.ssp585)+
#   
#   geom_voronoi(mapping = aes(x = x, y = y,fill = Precipitation),outline = basins.df,show.legend = T)+
#   geom_sf(data = sf_basin,color = "black",alpha = 0)+
#   geom_point(data = stationsWithValues, aes(x = x, y = y),pch = 17,size = 0.5)+
#   # scale_fill_steps2(low = "blue", mid = "green",high = "red", guide = "colorbar")+
#   scale_fill_steps2(low = "#708090",mid = "#D2691E", high = "blue",breaks =c(0,250, seq(300,1000,200),seq(1100,2500,400)), midpoint = 1000, name = NULL)+
#   
#   # scale_fill_stepsn(colours = c("#87CEFA","#0000FF","#98FB98","#7CFC00","#006400","#EEE8AA","#B8860B","#FF8C00","#FA8072","#FF0000","#800000"),
#   #                     # c("black","blue1","palegreen2","skyblue2","gold1",
#   #                     #           "#FDBF6F","#E31A1C","green4","dodgerblue2","orchid1",
#   #                     #           "khaki2","maroon","#FF7F00","#6A3D9A","gray70","#FB9A99","#CAB2D6","deeppink1"),
#   #                               
#   #   # "dodgerblue2", "#E31A1C", # red
#   #   #                             "green4",
#   #   #                             "#6A3D9A", # purple
#   #   #                             "#FF7F00", # orange
#   #   #                             "black", "gold1",
#   #   #                             "skyblue2", "#FB9A99", # lt pink
#   #   #                             "palegreen2",
#   #   #                             "#CAB2D6", # lt purple
#   #   #                             "#FDBF6F", # lt orange
#   #   #                             "gray70", "khaki2",
#   #   #                             "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
#   #   #                             "darkturquoise", "green1", "yellow4", "yellow3",
#   #   #                             "darkorange4", "brown"),
#   #                   # "lightblue","blue","cyan","orange","green","purple","pink","tomato3","red"),
#   #                   limits = c(0,1800),
#   #                    breaks =c(0,50,100,150, seq(200,1000,300),seq(1100,2000,200)) ,name = "Pr (mm)")+
#   # 
#   facet_grid(fct_rev(period3)~season3)+
#   theme_bw()+
#   ggtitle("ssp585")+
#   theme(panel.background = element_blank(),title = element_text(hjust = 0.5))+
#   
#   theme(legend.position = "bottom", legend.key.width = unit(1.25,"in"),legend.key.height = unit(0.2,"in"))+
#   
#   theme(axis.text = element_text(face = "bold",colour = "black",size = 7,angle = 45,hjust = 1,vjust = 1))+
#   theme(strip.text = element_text(face = "bold",colour = "black",size = 8))+
#   north(sf_basin,scale = 0.2,symbol = 3)+
#   scalebar(sf_basin, dist = 40, dist_unit = "km",transform = F, location = "bottomleft",
#            st.size = 2,height = 0.045,border.size = 0.15,st.dist = 0.07)+
#   xlab(NULL)+ylab(NULL)+
#   coord_sf()
# 
#  p<-ggarrange(historical,ssp245,ssp585,nrow = 3,ncol = 1 )

# breaks = c(0,100,200,400,600,900,1200,1500,1800,2100,2400,2700)
ggsave(filename = paste0("1SSP585_SEASONAL_SPATIAL(WITHOUT LEGEND)",".png"),plot = historical,
       width = 7,height = 3.5 ,units = "in",dpi = 300)
while (!is.null(dev.list()))  dev.off()
































# plot1<-ggplot()+
#         geom_sf(data = sf_basin)+
#         geom_sf(data = narrow,
#                 mapping = aes(color = Precipitation_mm),size = 2,fill = 10)+
#         scale_color_manual(values = c("wheat4","cornflowerblue","firebrick4","darkblue","tomato3",
#                                       "seagreen","yellowgreen","magenta","orange","red2"))+
#         # scale_fill_manual(values = c(        "fff100",
#         #                                      "ec008c",
#         #                                      "e81123",
#         #                                      "ec008c",
#         #                                      "68217a",
#         #                                      "00188f",
#         #                                      "00bcf2",	
#         #                                      "00b294",	
#         #                                      "009e49","bad80a"
#         # ))+
#         #scale_color_brewer (palette = "glasbey")+
# 
#         # scale_colour_gradientn(colours = rev(rainbow(7)),
#         #                        breaks = seq(0,2500,by = 100),
#         #                        # limits = c(0, 30),
#         #                        labels = as.character(seq(0,2500,by = 100))) +
#         #scale_color_distiller(palette = "Paired", breaks = c(-6,-3,-1,0,1,3,6,9,12))+
#         # scale_fill_distiller(palette = "Spectral")+
#         # scale_colour_continuous(type = "viridis")+
#         coord_sf()+
#         theme_bw()+
#         theme(axis.text.y = element_text(angle = 90,hjust = 0.5,vjust = 0.5,))+
#         facet_wrap(~Season)+
#         theme(legend.position = c(0.5,-0.1),legend.direction = "horizontal", legend.key = element_rect(linetype = 2),
#               legend.title = element_text(size = 8,face = "bold"),
#               legend.text = element_text(size= 8))+
#         theme(axis.text = element_text(face = "bold",colour = "black",size = 7))+
#         theme(strip.text = element_text(face = "bold",colour = "black"))+
#         north(sf_basin,scale = 0.2,symbol = 3)+
#         scalebar(sf_basin, dist = 20, dist_unit = "km",transform = F, location = "bottomleft",
#                   st.size = 2,height = 0.04,border.size = 0.25)+
#         xlab(NULL)+ylab(NULL)


# facet_grid(.~Indices,cols = 3,rows = vars(), as.table = TRUE)

# ggsave(filename = paste0("binnedspatial_dist_seasonal_ppt_hist",".png"),plot = last_plot(),
#        width = 9,height = 7,units = "in")
# while (!is.null(dev.list()))  dev.off()

