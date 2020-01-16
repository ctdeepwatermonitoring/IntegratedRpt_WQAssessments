setwd("P:/Projects/GitHub_Prj/IntegratedRpt_WQAssessments")

library(dataRetrieval)
library(leaflet)

pcd<-"50468"
state<-"CT"
start.date <-"2017-01-01"
end.date <- "2018-12-31"
stype <- "ST"

#Get all stream sites that have data with param cd and within dates
bactiCTsites<- readNWISdata(stateCd=state,parameterCd=pcd,startDate=start.date,endDate=end.date,
                            siteType=stype,service="site")
#Map of sites
leaflet(data=bactiCTsites) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(~dec_long_va,~dec_lat_va,color = "red", radius=5, stroke=FALSE,
                   fillOpacity = 0.8, opacity = 0.8, popup=~station_nm)

#Get site no to get data
sites<-bactiCTsites$site_no

#PUll data for sites and dates
bactiCT<- readNWISqw(siteNumbers=sites,parameterCd=pcd,startDate=start.date,endDate=end.date)
write.csv(bactiCT,"CT_USGS_BacteriaDataPcd50468_2017_2018.csv")