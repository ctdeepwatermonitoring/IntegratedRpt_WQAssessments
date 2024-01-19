setwd("P:/R/IR") #change as needed

#needed packages
library(dplyr)
library(leaflet)
library(sf)
library(sp)
library(mapview)

################################################################################
#####querying the WQP directly for E coli stations##############################

results_list <- list() #empty list to populate with E coli results
stations_list <- list() #empty list to populate with E coli stations
orgs <- c("USGS-CT", "CTVOLMON", "FRWA", "TLGVWQMPROGRAM") #add orgs as we identify them

#can change elements of this URL
base_URL <- "https://www.waterqualitydata.us/data/requesttype/search?statecode=US%3A09&organization=orgs&characteristicName=Escherichia%20coli&startDateLo=startdate&startDateHi=enddate&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET"

#date portion
startdate <- "01-01-2021" #change if needed
enddate <- "12-31-2023" #change if needed
base_URL <- gsub("startdate", startdate, base_URL) 
base_URL <- gsub("enddate", enddate, base_URL)

#type of table we query from WQP
station_URL <- base_URL
station_URL <- gsub("requesttype", "Station", base_URL)

#looping over orgs list into station URl request, each iteration is stored in list
for (i in orgs) {
  request_station_URL <- gsub("orgs", i, station_URL)
  stations <- read.csv(request_station_URL)
  stations_list[[i]] <- stations
}

merged_stations <- do.call(rbind, stations_list) #merging each loop iteration
write.csv(merged_stations, "merged_stations.csv", row.names = FALSE) #save to reference if needed

################################################################################
#######code for snapping WQX sites to DEEP segments#############################

#shapefiles for snapped map - QA use to check how points changed
rivers <- read_sf(dsn = ".", layer = "CT_305b_Assessed_River_2022")
basins <- read_sf(dsn = ".", layer = "Drainage_Basin_Polygon")
basins <- st_make_valid(basins) #Loop 0 is not valid: Edge 225 has duplicate vertex with edge 267?
sites_sf <- st_as_sf(merged_stations, coords=c("LongitudeMeasure","LatitudeMeasure"), crs =4326)

#function source: https://mapping-in-r-workshop.ryanpeek.org/03_vig_snapping_points_to_line
st_snap_points <- function(x, y, namevar, max_dist = 1000) {
  
  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  # this part: 
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>% 
    mutate({{namevar}} := x[[namevar]]) %>% 
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)
  
  return(out_xy)
}

#snapping and joining
sites_sf <- st_transform(sites_sf, crs = 4326) #making same coordinate system
rivers <- st_transform(rivers, crs = 4326) #making same coordinate system
sites_snapped <- st_snap_points(sites_sf, rivers, namevar = "MonitoringLocationIdentifier", max_dist = 30) #function from above
sites_buffer <- st_buffer(sites_snapped, 1) #buffer sites to give them dimension so spatial joins work
sites_joined <- st_join(sites_buffer, rivers)
sites_joined_df <- as.data.frame(sites_joined)
sites_joined_df <- merge (merged_stations, sites_joined_df, by = "MonitoringLocationIdentifier")
colnames(sites_joined_df)
sites_joined_df <- sites_joined_df[c("X", "Y", "LongitudeMeasure", "LatitudeMeasure", "MonitoringLocationIdentifier", "ASSESSMENT", "MonitoringLocationDescriptionText")]

#final file to manually QA - check every assigned segment to online assessed rivers map
write.csv(sites_joined_df, "segmentQA.csv", row.names = FALSE) 
#will also likely need to create new segments

#messy mapview to check what the snapping code did
snapped_map <- mapview(basins, col.regions = "lightblue", alpha.regions = .5, color = "lightblue", zcol = "BASIN_NO", legend = FALSE) + 
  mapview(rivers) + mapview(sites_snapped, col.regions = "blue") + 
  mapview(sites_sf, col.regions = "red", zcol = "MonitoringLocationIdentifier", legend = FALSE) + 
  mapview(sites_buffer, col.regions = "cyan")
snapped_map

################################################################################
######bind E coli results to segmentQA file after adding and fixing segments####

#getting E coli results - duplicated code from above to readability.. may change
base_URL <- "https://www.waterqualitydata.us/data/requesttype/search?statecode=US%3A09&organization=orgs&characteristicName=Escherichia%20coli&startDateLo=startdate&startDateHi=enddate&mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET"

#date portion
startdate <- "01-01-2021" #change if needed
enddate <- "12-31-2023" #change if needed
base_URL <- gsub("startdate", startdate, base_URL) 
base_URL <- gsub("enddate", enddate, base_URL)

result_URL <- base_URL
result_URL <- gsub("requesttype", "Result", result_URL)

for (i in orgs) {
  request_result_URL <- gsub("orgs", i, result_URL)
  data <- read.csv(request_result_URL)
  results_list[[i]] <- data
}

merged_results <- do.call(rbind, results_list) #all E coli results with the request parameters

#counting number of results per site, useful for checking if a segment should be created
results_count <- merged_results %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(ResultsPerSite = n())

#reading and merging segment information back in after performing manual QA and creating segments
station_to_segment <- read.csv("station_to_segment_updated.csv") #this should be the UPDATED segment QA file
result_with_segment <- merged_results
result_with_segment <- merge(station_to_segment, result_with_segment, by = "MonitoringLocationIdentifier")
result_with_segment$ActivityConductingOrganizationText <- ifelse(is.na(result_with_segment$ActivityConductingOrganizationText), 
                                                                 result_with_segment$OrganizationIdentifier, result_with_segment$ActivityConductingOrganizationText) #create one column with org info
result_with_segment$ActivityConductingOrganizationText <- gsub("U.S. Geological Survey-Water Resources Discipline", 
                                                               "U.S. Geological Survey", result_with_segment$ActivityConductingOrganizationText) #they call themselves two different things
result_with_segment <- result_with_segment[c("ASSESSMENT", "MonitoringLocationIdentifier", "ActivityConductingOrganizationText", 
                                             "ActivityStartDate", "CharacteristicName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
                                             "ResultDetectionConditionText")] #these are the columns we need for analysis

#creating file to manually check and fix things
write.csv(result_with_segment, "IR_Ecoli_with_segment.csv", row.names = FALSE)
#NOTE: there are inconsistencies w how orgs report above/below detect limits
#changing <1 or NA values in  Excel is probably the easiest thing to do as each
#org reports it differently i.e. cannot programmatically change them all at once

################################################################################
#####performing E coli result analysis per segment##############################

result_with_segment <- read.csv("IR_Ecoli_with_segment.csv") #note: MANUALLY changed NA/null result values to either 1 or 2000 depending on detect limit comment
sapply(result_with_segment, class) #checking data types before doing maths
gmean <- function(x) exp(mean(log(x))) #define geomean function

#summarized table
segment_analysis <- result_with_segment %>%
  group_by(ASSESSMENT) %>%
  summarise(
    nSamples = n(), #how many samples contributed to assessment
    maxSample = max(ResultMeasureValue, na.rm = TRUE), #max sample in case there is something wild screwing the gmean
    minSample = min(ResultMeasureValue, na.rm = TRUE), #informational
    nOver410 = sum(ResultMeasureValue > 410), #how many samples over 410 MPN?
    nOver576 = sum(ResultMeasureValue > 576), #how many samples over 576 MPN?
    nOver1000 = sum(ResultMeasureValue > 1000), #how many samples over 1000 MPN?
    meanSamples = mean(ResultMeasureValue), #regular mean
    gmeanSamples = gmean(ResultMeasureValue), #from my handy geomean function
    percentExceed = ((nOver576/nSamples) * 100), #this is how Walter calculated it
    whichOrgs = toString(unique(ActivityConductingOrganizationText)), #which orgs contributed
    nContributingOrgs = n_distinct(ActivityConductingOrganizationText) #how many orgs contributed (QA check on whichOrgs)
  )

write.csv(segment_analysis, "segment_analysis.csv", row.names = FALSE) #to make final assessment decisions
