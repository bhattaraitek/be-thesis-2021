#created by @Tek 2022 Feb 13
library("rgdal")
library("sf")
library("tidyverse")
library("ggrepel")
library("ggforce")
library("RColorBrewer")
library("gridExtra")
library("ggsn")
library("cowplot")
library("egg") 
library("readxl")
library("ggvoronoi")
# library("legendMap")



setwd("D:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

variable<-"rainfall"
scenarios<-c("ssp245","ssp585")
periods<-c("NF","MF","FF")
slopevalue.2s<-NULL
for(scenario in scenarios){ 
        slopevalue.3p<-NULL
        for(period in periods){
                slopevalue.1p<-read_excel(paste0("TrendAverages/Slope/",variable,"_Indices_",scenario,"_",period,".xlsx"))
                slopevalue.1p$Scenario<-scenario
                slopevalue.1p$Period<-period
                slopevalue.3p<-rbind(slopevalue.3p,slopevalue.1p)
        }
slopevalue.2s<-rbind(slopevalue.2s,slopevalue.3p)
}
slopevalue.2s[is.na(slopevalue.2s)]<-"Not Significant"
slopevalue.2s[slopevalue.2s == "S"] <-"95% Significant"


met_stn<-readOGR(dsn = "spatial_data","geda")
basins<-readOGR (dsn = "spatial_data","gedabasin")


stations.df<-as(met_stn,"data.frame")
stations.df[,4:11]<-NULL
# points.df[,1:2]<-NULL
colnames(stations.df)<-c("ID","Name","Index", "x","y","z")
stationsWithValues<-merge(x = stations.df,y= slopevalue.2s,by.x = "Index",by.y="IndexNo") #narrow dataframe with all information
stationsWithValues[,2:3]<-NULL


sf_basin<-st_as_sf(basins)
# 
# narrow.df<-gather(stationsWithValues,key = "Indices",value = "IndicesValue",-Index,-x,-y,-z)


spatial_plot<-function(basins = sf_basin,stations = stationsWithValues,scenario){
                basins.df<-as_Spatial(basins)
                selected.scenario.values<-stations[stations$Scenario == scenario,]
                #View(selected.scenario.values)
                colnames(selected.scenario.values)<-c("IndexNo","x","y","z","index","slope","significance","scenario","period")
                
                if(variable == "temperature"){
                        selected.scenario.values$index<-factor(selected.scenario.values$index,
                                                               levels = c("txx","tnn","txn","tnx","tx90p","tn90p","su","wsdi"))
                        levels(selected.scenario.values$index)<-c("txx(°C)","tnn(°C)","txn(°C)","tnx(°C)","tx90p(%)","tn90p(%)","su(days)","wsdi(days)")
                }else{
                        
                        selected.scenario.values$index<-factor(selected.scenario.values$index,
                                                               levels = c("rx1day","rx5day","prcptot","r95ptot","r99ptot","cdd","cwd","r20mm"))
                        levels(selected.scenario.values$index)<-c("rx1day(mm)","rx5day(mm)","prcptot(mm)","r95ptot(%)","r99ptot(%)","cdd(days)","cwd(days)","r20mm(days)")
                }
                
                #View(selected.scenario.values)
                significant.stations<-selected.scenario.values[selected.scenario.values$significance == "95% Significant",]
                # minvalue<- round(as.numeric(min(selected.scenario.values$slope,na.rm = T)),2)
                # maxvalue<-round(as.numeric(max(selected.scenario.values$slope,na.rm = T)),2)
                # print(minvalue)
                # print(maxvalue)
                # step<-  round(0.1*(maxvalue-minvalue),2)
                print(scenario)
                # print(step)
                # 
                # if(minvalue >= -5){
                #         brks<- c(seq(round(minvalue,0),0,2.5),0,2.5,5)
                #         clrs<-c("skyblue3","springgreen2","orange","yellow","purple", "hotpink2","red2")
                # }else if( minvalue < -5){
                #         brks<- c(seq(round(minvalue,0),-5,5),seq(-5,5,2.5))
                #         clrs<-c("skyblue3","royalblue4","springgreen2","orange","gold1","yellow","purple", "hotpink2","red2")
                # }
                # if(maxvalue<= 15 & maxvalue > 5){
                #         brks2<-c(10,15)
                #         clrs<-c("skyblue3","royalblue4","springgreen2", "hotpink2","red2")
                # }else if(maxvalue >15){
                #        brks2<- seq(15,maxvalue,10)
                # }else{
                #         brks2<-NULL
                #         clrs<-c("skyblue3","springgreen2", "hotpink2","red2")
                # }
                
                if (variable == "temperature"){
                        lmts<-c(-1.5,1.5)
                        brks<-seq(-1.5,1.5,0.5) #low = "#FF0000",mid = "#E0FFFF", high = "#00008B"
                        lw<-"#00008B"  #00008B
                        md<-"#E0FFFF"
                        hg<-"#FF0000"
                }else{
                        lmts<-c(-1.5,1.5)
                        brks<-c(seq(-1.5,1.5,0.5))
                        hg<-"#00008B"  #00008B #blue
                        md<-"#E0FFFF"  #cyan
                        lw<-"#FF0000" #red
                }
            
                ggplot(selected.scenario.values)+
                
                geom_voronoi(mapping = aes(x = x, y = y,fill = slope),outline = basins.df,show.legend = T)+
                geom_sf(data = sf_basin,color = "black",alpha = 0,lwd = 0.15)+
                geom_point(data = significant.stations, aes(x = x, y = y), size = 1.25,pch = 19,show.legend = F)+
                       # scale_shape_manual(values = c(3),name = NULL)+
                 scale_fill_steps2(low = lw,mid = md, high = hg , midpoint = 0,
                                   limits = lmts,
                                   breaks = brks, name = "Slope(units/year)")+
                        # scale_fill_brewer(palette = "RdBu",limits = lmts, breaks = brks)+
                
                        
                # scale_fill_stepsn(colours= "RdBu",
                #                   limits = lmts,
                #                   breaks = brks,name = "Slope") +
                theme_bw()+
                facet_wrap(~scenario)+
                        facet_grid(fct_rev(period)~index)+
                theme(panel.background = element_blank(),legend.background = element_blank(),
                      legend.box.background = element_blank(),panel.grid.major = element_blank())+
                theme(legend.position = "bottom", legend.key.width = unit(1,"in"),legend.key.height = unit(0.15,"in"),
                      legend.text = element_text(size = 13),legend.title = element_text(size = 13))+
                
                theme(axis.text.y = element_text(face = "bold",colour = "black",size = 10.5,angle = 90,hjust = 0.5,vjust = 0.5))+
                theme(axis.text.x = element_text(face = "bold",colour = "black",size = 10.5))+
                theme(plot.margin = unit(c(0, 0, 0, 0), "in"))+
                theme(strip.text = element_text(face = "bold",colour = "black",size = 13))+
                scale_x_continuous(breaks = c(80.5,82,83.5))+scale_y_continuous(breaks = c(28,29))+
                # scale_bar(lon = 80.5, lat = 27.5,
                #                            distance_lon = 40, distance_lat = 40,
                #                            distance_legend = 40, dist_unit = "km")+
                        
                        
                north(sf_basin,scale = 0.3,symbol = 3)+
                scalebar(sf_basin, dist = 75, dist_unit = "km",transform = F, location = "bottomleft",
                        st.size = 3,height = 0.07,border.size = 0.15,st.dist = 0.095,st.bottom = F)+
                xlab(NULL)+ylab(NULL)+
                coord_sf()
                
                print("plotting...")
                
                
}

if (variable == "rainfall"){
        ssp245<-spatial_plot(scenario = "ssp245")
        ggsave(filename = paste0("SPATIAL_TREND_SSP245-RAINFALL",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
        print("plotted...")
        
        ssp585<-spatial_plot(scenario = "ssp585")
        #combined.rainfall<-ggarrange(cdd,cwd,r20mm,rx1day,rx5day,prcptot,r95ptot,r99ptot,cdd,ncol = 3,nrow = 3,draw = F)
        ggsave(filename = paste0("SPATIAL_TREND_SSP585-RAINFALL",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
        print("plotted...")
}else{
        ssp245<-spatial_plot(scenario = "ssp245")
        ggsave(filename = paste0("SPATIAL-TREND_SSP245-TEMPERATURE",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()

        ssp585<-spatial_plot(scenario = "ssp585")
 
        ggsave(filename = paste0("SPATIAL-TREND_SSP585-TEMPERATURE",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
}















        







