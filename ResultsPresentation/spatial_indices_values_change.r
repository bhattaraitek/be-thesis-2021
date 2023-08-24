#created by @Tek 2022 Feb 13
library("rgdal")
library("sf")
library("tidyverse")
library("ggrepel")
library("ggforce")
library("RColorBrewer")
library("gridExtra")
library("ggsn")
#library("cowplot")
library("egg") 
library("readxl")
library("ggvoronoi")
# library("legendMap")



setwd("E:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

variable<-"TemperatureIndices"  #TemperatureIndices
scenarios<-c("ssp245","ssp585")
periods<-c("NF","MF","FF")
slopevalue.2s<-NULL
for(scenario in scenarios){ 
        slopevalue.3p<-NULL
        for(period in periods){
                slopevalue.1p<-read.csv(paste0("TrendAverages/Ensemble/","Ensemble_Change_",variable,"_",scenario,"_",period,".csv"),header = T)
                colnames(slopevalue.1p)[1] <- "IndexNo"
                slopevalue.1p$IndexNo <- substr(slopevalue.1p$IndexNo,2,4)
                slopevalue.1p$Scenario<-scenario
                slopevalue.1p$Period<-period
                slopevalue.3p<-rbind(slopevalue.3p,slopevalue.1p)
        }
slopevalue.2s<-rbind(slopevalue.2s,slopevalue.3p)
}

if (variable == "RainfallIndices"){
        colnames(slopevalue.2s)[2:9]<-c("rx1day","rx5day","prcptot","r95ptot","r99ptot","cdd","cwd","r20mm")
}else{
        colnames(slopevalue.2s)[2:9]<-c("txx","tnn","txn","tnx","tx90p","tn90p","su","wsdi")
}

met_stn<-readOGR(dsn = "spatial_data","geda")
basins<-readOGR (dsn = "spatial_data","gedabasin")


stations.df<-as(met_stn,"data.frame")
stations.df[,4:11]<-NULL
# points.df[,1:2]<-NULL
colnames(stations.df)<-c("ID","Name","Index", "x","y","z")
stationsWithValues<-merge(x = stations.df,y= slopevalue.2s,by.x = "Index",by.y="IndexNo") #narrow dataframe with all information
stationsWithValues[,2:3]<-NULL

stationsWithValues<-gather(stationsWithValues,key = Indices,value = PerChange,-Index,-x,-y,-z,-Period,-Scenario)


sf_basin<-st_as_sf(basins)
# 
# narrow.df<-gather(stationsWithValues,key = "Indices",value = "IndicesValue",-Index,-x,-y,-z)


spatial_plot<-function(basins = sf_basin,stations = stationsWithValues,scenario){
                basins.df<-as_Spatial(basins)
                selected.scenario.values<-stations[stations$Scenario == scenario,]
                # View(selected.scenario.values)
                colnames(selected.scenario.values)<-c("IndexNo","x","y","z","scenario","period","indices","perchange")
                #View(selected.scenario.values)
                if(variable == "TemperatureIndices"){
                        selected.scenario.values$indices<-factor(selected.scenario.values$indices,
                                                               levels = c("txx","tnn","txn","tnx","tx90p","tn90p","su","wsdi"))
                        levels(selected.scenario.values$indices)<-c("txx(°C)","tnn(°C)","txn(°C)","tnx(°C)","tx90p(%)","tn90p(%)","su(days)","wsdi(days)")
                }else{

                        selected.scenario.values$indices<-factor(selected.scenario.values$indices,
                                                               levels = c("rx1day","rx5day","prcptot","r95ptot","r99ptot","cdd","cwd","r20mm"))
                        levels(selected.scenario.values$indices)<-c("rx1day(mm)","rx5day(mm)","prcptot(mm)","r95ptot(%)","r99ptot(%)","cdd(days)","cwd(days)","r20mm(days)")
                }
                
      
                print(scenario)
     
                
                if (variable == "TemperatureIndices"){
                        lmts<-c(-60,60)
                        brks<-seq(-45,45,15) #low = "#FF0000",mid = "#E0FFFF", high = "#00008B"
                        lw<-"#00008B"  #00008B
                        md<-"#E0FFFF"
                        hg<-"#FF0000"
                }else{
                        lmts<-c(-20,60)
                        brks<-c(seq(-15,60,15))
                        hg<-"#00008B"  #00008B #blue
                        md<-"#E0FFFF"  #cyan
                        lw<-"#FF0000" #red
                }
            
                ggplot(selected.scenario.values)+
                
                geom_voronoi(mapping = aes(x = x, y = y,fill = perchange),outline = basins.df,show.legend = T)+
                geom_sf(data = sf_basin,color = "black",alpha = 0,lwd = 0.15)+
                #geom_point(data = significant.stations, aes(x = x, y = y), size = 1.25,pch = 19,show.legend = F)+
                       # scale_shape_manual(values = c(3),name = NULL)+
                 scale_fill_steps2(low = lw,mid = md, high = hg , midpoint = 0,
                                   limits = lmts,
                                   breaks = brks, name = "Deviation(%)")+
                        # scale_fill_brewer(palette = "RdBu",limits = lmts, breaks = brks)+
                
                        
                # scale_fill_stepsn(colours= "RdBu",
                #                   limits = lmts,
                #                   breaks = brks,name = "Slope") +
                theme_bw()+
                facet_wrap(~scenario)+
                        facet_grid(fct_rev(period)~indices)+
                theme(panel.background = element_blank(),legend.background = element_blank(),
                      legend.box.background = element_blank(),panel.grid.major = element_blank())+
                theme(legend.position = "bottom", legend.key.width = unit(1,"in"),legend.key.height = unit(0.15,"in"),
                      legend.text = element_text(size = 11))+
                
                theme(axis.text.y = element_text(face = "bold",colour = "black",size = 10.5,angle = 90,hjust = 0.5,vjust = 0.5))+
                theme(axis.text.x = element_text(face = "bold",colour = "black",size = 10.5))+
                theme(plot.margin = unit(c(0, 0, 0, 0), "in"))+
                theme(strip.text = element_text(face = "bold",colour = "black",size = 13))+
                scale_x_continuous(breaks = c(80.5,82,83.5))+scale_y_continuous(breaks = c(28,29))+
                north(sf_basin,scale = 0.3,symbol = 3)+
                scalebar(sf_basin, dist = 75, dist_unit = "km",transform = F, location = "bottomleft",
                        st.size = 3,height = 0.07,border.size = 0.15,st.dist = 0.095,st.bottom = F)+
                xlab(NULL)+ylab(NULL)+
                coord_sf()
                
                print("plotting...")
                
                
}

if (variable == "RainfallIndices"){
        ssp245<-spatial_plot(scenario = "ssp245")
        ggsave(filename = paste0("Aug-245SPATIAL_PerChangeIndicesValue_SSP245-RAINFALL",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
        print("plotted...")
        
        ssp585<-spatial_plot(scenario = "ssp585")
        ggsave(filename = paste0("Aug-585SPATIAL_PerChangeIndicesValue_SSP585-RAINFALL",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
        print("plotted...")
}else{
        ssp245<-spatial_plot(scenario = "ssp245")
        ggsave(filename = paste0("Aug-245SPATIAL-PerChangeIndicesValue_SSP245-TEMPERATURE",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()

        ssp585<-spatial_plot(scenario = "ssp585")
 
        ggsave(filename = paste0("Aug-585SPATIAL-PerChangeIndicesValue_SSP585-TEMPERATURE",".png"),plot = last_plot(),
               width = 15,height = 4,units = "in",dpi = 500)
        while (!is.null(dev.list()))  dev.off()
}















        







