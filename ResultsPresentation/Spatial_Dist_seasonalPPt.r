#created by @Tek 2022 Feb 13
library("rgdal")
library("sf")
library("tidyverse")
library("ggrepel")
library("ggforce")
library("RColorBrewer")
library("gridExtra")
library("ggsn")
library("tmap")
library("readxl")
library("pals")
setwd("D:/#2_Project Works/Climate Projection/Projected_Change_Analysis")

scenario<-"ssp245"
period<-"historical"

met_stn<-readOGR(dsn = "spatial_data","geda")
basins<-readOGR (dsn = "spatial_data","gedabasin")
seasonal.ppt<-as.data.frame(read_excel("SeasonalValues/Seasonal_historical_rainfall.xlsx"))
# colnames(seasonal.ppt)[1]<-"Station"

sf_metstn<-st_as_sf(met_stn)
sf_basin<-st_as_sf(basins)
sf_metstn[,4:11]<-NULL

sf_metstn<-merge(x = sf_metstn,y= seasonal.ppt, by.x = "Station",by.y="IndexNo")
narrow<-gather(sf_metstn,key = "Season",value = "Precipitation", -geometry,-OBJECTID,-Hydro_Mete,-Station)

narrow$Precipitation_mm<-factor(cut(x = narrow$Precipitation,breaks = c(0,50,100,150,250,500,750,1000,1250,
                                                       1500,1750,2000,2250,2500,max(narrow$Precipitation))))
levels(narrow$Precipitation_mm)<-c("0-50","51-100","101-150","151-250","251-500","501-750","751-1000",
                                   "1001-1250","1251-1500","1501-1750","1751-2000","2001-2250","2251-2500")


plot1<-ggplot()+
        geom_sf(data = sf_basin)+
        geom_sf(data = narrow,
                mapping = aes(color = Precipitation_mm),size = 2,fill = 10)+
        scale_color_manual(values = c("wheat4","cornflowerblue","firebrick4","darkblue","tomato3",
                                      "seagreen","yellowgreen","magenta","orange","red2"))+
        # scale_fill_manual(values = c(        "fff100",
        #                                      "ec008c",
        #                                      "e81123",
        #                                      "ec008c",
        #                                      "68217a",
        #                                      "00188f",
        #                                      "00bcf2",	
        #                                      "00b294",	
        #                                      "009e49","bad80a"
        # ))+
        #scale_color_brewer (palette = "glasbey")+

        # scale_colour_gradientn(colours = rev(rainbow(7)),
        #                        breaks = seq(0,2500,by = 100),
        #                        # limits = c(0, 30),
        #                        labels = as.character(seq(0,2500,by = 100))) +
        #scale_color_distiller(palette = "Paired", breaks = c(-6,-3,-1,0,1,3,6,9,12))+
        # scale_fill_distiller(palette = "Spectral")+
        # scale_colour_continuous(type = "viridis")+
        coord_sf()+
        theme_bw()+
        theme(axis.text.y = element_text(angle = 90,hjust = 0.5,vjust = 0.5,))+
        facet_wrap(~Season)+
        theme(legend.position = c(0.5,-0.1),legend.direction = "horizontal", legend.key = element_rect(linetype = 2),
              legend.title = element_text(size = 8,face = "bold"),
              legend.text = element_text(size= 8))+
        theme(axis.text = element_text(face = "bold",colour = "black",size = 7))+
        theme(strip.text = element_text(face = "bold",colour = "black"))+
        north(sf_basin,scale = 0.2,symbol = 3)+
        scalebar(sf_basin, dist = 20, dist_unit = "km",transform = F, location = "bottomleft",
                  st.size = 2,height = 0.04,border.size = 0.25)+
        xlab(NULL)+ylab(NULL)
        
                      
        # facet_grid(.~Indices,cols = 3,rows = vars(), as.table = TRUE)

ggsave(filename = paste0("binnedspatial_dist_seasonal_ppt_hist",".png"),plot = last_plot(),
       width = 9,height = 7,units = "in")
while (!is.null(dev.list()))  dev.off()

