#-------------------------------------------------------------------------------
# R script for computing GEDI biophysical variables with the rGEDI package
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Tasks
#-------------------------------------------------------------------------------

#1 Define function to query CMR (common metadata repository) based on user-provided input (date, BB)
#2 Use GEDI finder to locate GEDI data of your AOI
#3 Download and subset respective GEDI data
#4 Compute Vegetation Biophysical Variables
#5 Compute Plant Area Index (PAI) and Plant Area Volume Density (PAVD) Profiles
#6 compute biophycial vegetation parameters
#7 compute grids from metrics
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Check for required packages, install if not previously installed
#-------------------------------------------------------------------------------


# Import Packages
library(rGEDI)
library(dplyr)
library(leaflet)
library(httr)
library(rasterVis)
library(viridis)
library(rgdal)


#-------------------------------------------------------------------------------
# Assign your working directory
#-------------------------------------------------------------------------------

myDirectory <- "D:/Mandl_L_PhD/NP_BGD/GEDI/_data"

# set your working directory
setwd(myDirectory)

# assign an output directory
outdir <- myDirectory

#-------------------------------------------------------------------------------
#Define function to Query CMR
#-------------------------------------------------------------------------------
gedi_finder <- function(product, bbox) {
  
  # Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  
  # Set up list where key is GEDI shortname + version and value is CMR Concept ID
  concept_ids <- list('GEDI01_B.002'='C1908344278-LPDAAC_ECS', 
                      'GEDI02_A.002'='C1908348134-LPDAAC_ECS', 
                      'GEDI02_B.002'='C1908350066-LPDAAC_ECS')
  
  # CMR uses pagination for queries with more features returned than the page size
  page <- 1
  bbox <- sub('','',bbox)  # Remove any white spaces
  granules <- list()          # Set up a list to store and append granule links to
  
  # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number
  cmr_response <- GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page))
  
  # Verify the request submission was successful
  if (cmr_response$status_code==200){
    
    # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as a list
    cmr_url <- sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)
    cmr_response <- content(GET(cmr_url))$feed$entry
    
    # If 2000 features are returned, move to the next page and submit another request, and append to the response
    while(length(cmr_response) %% 2000 == 0){
      page <- page + 1
      cmr_url <- sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)
      cmr_response <- c(cmr_response, content(GET(cmr_url))$feed$entry)
    }
    
    # CMR returns more info than just the Data Pool links, below use for loop to grab each DP link, and add to list
    for (i in 1:length(cmr_response)) {
      granules[[i]] <- cmr_response[[i]]$links[[1]]$href
    }
    
    # Return the list of links
    return(granules)
  } else {
    
    # If the request did not complete successfully, print out the response from CMR
    print(content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$errors)
  }
}


#-------------------------------------------------------------------------------
# User-provided inputs (UPDATE FOR YOUR DESIRED PRODUCT AND BOUNDING BOX REGION OF INTEREST)
#-------------------------------------------------------------------------------
product <- 'GEDI02_B.002'           # Options include 'GEDI01_B.002', 'GEDI02_A.002', 'GEDI02_B.002'
bbox <-'12.759659,47.458228,13.098862,47.646021'  # bounding box coords in LL Longitude, LL Latitude, UR Longitude, UR Latitude format


#-------------------------------------------------------------------------------
# Call the gedi_finder function using the user-provided inputs
#-------------------------------------------------------------------------------
granules_L2B <- gedi_finder(product, bbox)
print(sprintf("%s %s Version 2 granules found.", length(granules_L2B), product))

#-------------------------------------------------------------------------------
# convert granules list to vector
#-------------------------------------------------------------------------------

granules_L2B_v <- unlist(granules_L2B, use.names=FALSE)

#-------------------------------------------------------------------------------
# Export Results and save URLS as .txt file
#-------------------------------------------------------------------------------

# Set up output textfile name using the current datetime
outName_L2B <- sprintf("%s_GranuleList_%s.txt", sub('.002', '_002', product), format(Sys.time(), "%Y%m%d%H%M%S"))

# Save to text file in current working directory
write.table(granules_L2B, outName_L2B, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, getwd(), outName_L2B))

#-------------------------------------------------------------------------------
# Download GEDI data
#-------------------------------------------------------------------------------
gediDownload(filepath = granules_L2B_v[7], outdir = outdir)
gediDownload(filepath = granules_L2B_v[11], outdir = outdir)
gediDownload(filepath = granules_L2B_v[17], outdir = outdir)
gediDownload(filepath = granules_L2B_v[2], outdir = outdir)
gediDownload(filepath = granules_L2B_v[8], outdir = outdir)
gediDownload(filepath = granules_L2B_v[10], outdir = outdir)
gediDownload(filepath = granules_L2B_v[15], outdir = outdir)
gediDownload(filepath = granules_L2B_v[18], outdir = outdir)

#
# read h5 files
#
gedilevel2b_1 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019180075351_O03082_03_T00826_02_003_01_V002.h5"))
gedilevel2b_2 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019255220154_O04255_02_T05016_02_003_01_V002.h5"))
gedilevel2b_3 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019331160117_O05430_02_T02170_02_003_01_V002.h5"))
gedilevel2b_4 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019128004537_O02270_02_T04863_02_003_01_V002.h5"))
gedilevel2b_5 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019220155440_O03708_03_T05095_02_003_01_V002.h5"))
gedilevel2b_6 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019220155440_O03708_03_T05095_02_003_01_V002.h5"))
gedilevel2b_7 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI02_B_2019307053534_O05051_03_T03825_02_003_01_V002.h5"))
gedilevel2b_8 <- readLevel2B(level2Bpath = file.path(outdir, "GEDI01_B_2019331160117_O05430_02_T02170_02_005_01_V002.h5"))



#-------------------------------------------------------------------------------
# subset data based on BB
#-------------------------------------------------------------------------------

xmin = 12.759659
xmax = 13.098862
ymin = 47.458228
ymax = 47.646021

# subset gedi data  
level2b_subset_1 <- clipLevel2B(gedilevel2b_1, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_1.h5")
level2b_subset_2 <- clipLevel2B(gedilevel2b_2, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_2.h5")
level2b_subset_3 <- clipLevel2B(gedilevel2b_3, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_3.h5")
level2b_subset_4 <- clipLevel2B(gedilevel2b_4, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_4.h5")
level2b_subset_5 <- clipLevel2B(gedilevel2b_5, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_5.h5")
level2b_subset_6 <- clipLevel2B(gedilevel2b_6, xmin, xmax, ymin, ymax, output = "GEDI_2B_subset_6.h5")


#-------------------------------------------------------------------------------
#read subset data
#-------------------------------------------------------------------------------

gedilevel2b_subset_0808 <- readLevel2B(level2Bpath = file.path(outdir, "L2B/GEDI_2B_subset_0808.h5"))
gedilevel2b_subset_1209 <- readLevel2B(level2Bpath = file.path(outdir, "L2B/GEDI_2B_subset_1209.h5"))

#-------------------------------------------------------------------------------
# Compute biophysical vegetation variables
#-------------------------------------------------------------------------------
level2BVPM_0808<-getLevel2BVPM(gedilevel2b_subset_0808)
head(level2BVPM_0808[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])

level2BVPM_1209<-getLevel2BVPM(gedilevel2b_subset_1209)
head(level2BVPM_1209[,c("beam","shot_number","pai","fhd_normal","omega","pgap_theta","cover")])

### remove nodata values

level2BVPM_0808_cl <- subset(level2BVPM_0808, level2BVPM_0808$pai != -9999)
level2BVPM_1209_cl <- subset(level2BVPM_1209, level2BVPM_1209$pai != -9999)


### Converting shot_number as "integer64" to "character"

level2BVPM_0808_cl$shot_number<-paste0(level2BVPM_0808_cl$shot_number)
level2BVPM_1209_cl$shot_number<-paste0(level2BVPM_1209_cl$shot_number)


### Converting GEDI Vegetation Profile Biophysical Variables as data.table to 
### SpatialPointsDataFrame
level2BVPM_spdf_0808<-SpatialPointsDataFrame(cbind(level2BVPM_0808_cl$longitude_lastbin,level2BVPM_0808_cl$latitude_lastbin),data=level2BVPM_0808_cl)
level2BVPM_spdf_1209<-SpatialPointsDataFrame(cbind(level2BVPM_1209_cl$longitude_lastbin,level2BVPM_1209_cl$latitude_lastbin),data=level2BVPM_1209_cl)


### Exporting GEDI Vegetation Profile Biophysical Variables as ESRI Shapefile
raster::shapefile(level2BVPM_spdf_0808_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/GEDI_2B_subset_0808_VPM_cl"))
raster::shapefile(level2BVPM_spdf_1209_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/GEDI_2B_subset_1209_VPM_cl"))


### Get Plant Area Index (PAI) sliced in different height classes
level2BPAIProfile_0808<-getLevel2BPAIProfile(gedilevel2b_subset_0808)
head(level2BPAIProfile_0808[,c("beam","shot_number","pai_z0_5m","pai_z5_10m")])

level2BPAIProfile_1209<-getLevel2BPAIProfile(gedilevel2b_subset_1209)
head(level2BPAIProfile_1209[,c("beam","shot_number","pai_z0_5m","pai_z5_10m")])


### remove nodata values
level2BPAIProfile_0808_cl <- subset(level2BPAIProfile_0808, level2BPAIProfile_0808$height_bin0 != -9999)
level2BPAIProfile_1209_cl <- subset(level2BPAIProfile_1209, level2BPAIProfile_1209$height_bin0 != -9999)


### Get Plant Area Volume Density Profiles (PAVD)
level2BPAVDProfile_0808<-getLevel2BPAVDProfile(gedilevel2b_subset_0808)
head(level2BPAVDProfile_0808[,c("beam","shot_number","pavd_z0_5m","pavd_z5_10m")])

level2BPAVDProfile_1209<-getLevel2BPAVDProfile(gedilevel2b_subset_1209)
head(level2BPAVDProfile_1209[,c("beam","shot_number","pavd_z0_5m","pavd_z5_10m")])

### remove nodata values
level2BPAVDProfile_0808_cl <- subset(level2BPAVDProfile_0808, level2BPAVDProfile_0808$height_bin0 != -9999)
level2BPAVDProfile_1209_cl <- subset(level2BPAVDProfile_1209, level2BPAVDProfile_1209$height_bin0 != -9999)


### Converting PAI shot_number as "integer64" to "character"
level2BPAIProfile_0808_cl$shot_number<-paste0(level2BPAIProfile_0808_cl$shot_number)
level2BPAIProfile_1209_cl$shot_number<-paste0(level2BPAIProfile_1209_cl$shot_number)


### Converting PAVD shot_number as "integer64" to "character"
level2BPAVDProfile_0808_cl$shot_number<-paste0(level2BPAVDProfile_0808_cl$shot_number)
level2BPAVDProfile_1209_cl$shot_number<-paste0(level2BPAVDProfile_1209_cl$shot_number)


### Converting PAI as data.table to SpatialPointsDataFrame
level2BPAIProfile_spdf_0808_cl<-SpatialPointsDataFrame(cbind(level2BPAIProfile_0808_cl$lon_lowestmode,level2BPAIProfile_0808_cl$lat_lowestmode),
                                               data=level2BPAIProfile_0808_cl)
level2BPAIProfile_spdf_1209_cl<-SpatialPointsDataFrame(cbind(level2BPAIProfile_1209_cl$lon_lowestmode,level2BPAIProfile_1209_cl$lat_lowestmode),
                                                 data=level2BPAIProfile_1209_cl)


### Converting PAVD as data.table to SpatialPointsDataFrame
level2BPAVDProfile_spdf_0808_cl<-SpatialPointsDataFrame(cbind(level2BPAVDProfile_0808_cl$lon_lowestmode,level2BPAVDProfile_0808_cl$lat_lowestmode),
                                                data=level2BPAVDProfile_0808_cl)
level2BPAVDProfile_spdf_1209_cl<-SpatialPointsDataFrame(cbind(level2BPAVDProfile_1209_cl$lon_lowestmode,level2BPAVDProfile_1209_cl$lat_lowestmode),
                                                     data=level2BPAVDProfile_1209_cl)


### Exporting PAI and PAVD Profiles as ESRI Shapefile
raster::shapefile(level2BPAIProfile_spdf_0808_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAI_0808_cl"))
raster::shapefile(level2BPAIProfile_spdf_1209_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAI_1209_cl"))


raster::shapefile(level2BPAVDProfile_spdf_0808_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAVD_0808_cl"))
raster::shapefile(level2BPAVDProfile_spdf_1209_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAVD_1209_cl"))

#-------------------------------------------------------------------------------
# plot PAI and PAVD profiles
#-------------------------------------------------------------------------------

###specify GEDI beam
beam="BEAM1011"

### Plot Level2B PAI Profile
gPAIprofile<-plotPAIProfile(level2BPAIProfile_0808, beam=beam, elev=TRUE)

### Plot Level2B PAVD Profile
gPAVDprofile<-plotPAVDProfile(level2BPAVDProfile_0808, beam=beam, elev=TRUE)


#-------------------------------------------------------------------------------
# Compute descriptive statistics of GEDI Level2A and Level2B data
#-------------------------------------------------------------------------------

### Define your own function
mySetOfMetrics = function(x)
{
  metrics = list(
    min =min(x), # Min of x
    max = max(x), # Max of x
    mean = mean(x), # Mean of x
    sd = sd(x), # Sd of x
    sum = sum(x) # sum of x
  )
  return(metrics)
}


### compute PAI min, max, mean, sd, sum

pai_stats_0808<-polyStatsLevel2BVPM(level2BVPM_0808_cl, func=mySetOfMetrics(pai), id="shot_number")
pai_stats_1209<-polyStatsLevel2BVPM(level2BVPM_1209_cl, func=mySetOfMetrics(pai), id="shot_number")

# compute FHD min, max, mean, sd, sum
FHD_stats_0808<-polyStatsLevel2BVPM(level2BVPM_0808_cl, func=mySetOfMetrics(fhd_normal), id="shot_number")
FHD_stats_1209<-polyStatsLevel2BVPM(level2BVPM_1209_cl, func=mySetOfMetrics(fhd_normal), id="shot_number")

# compute clumping index min, max, mean, sd, sum
clumping_stats_0808<-polyStatsLevel2BVPM(level2BVPM_0808_cl, func=mySetOfMetrics(), id="shot_number")
clumping_stats_1209<-polyStatsLevel2BVPM(level2BVPM_1209_cl, func=mySetOfMetrics(fhd_normal), id="shot_number")


### Computing the max/mean of the Total Plant Area Index

pai_max_0808<-polyStatsLevel2BVPM(level2BVPM_0808_cl,func=max(pai), id=NULL)
head(pai_max_0808)

pai_max_1209<-polyStatsLevel2BVPM(level2BVPM_1209_cl,func=max(pai), id=NULL)
head(pai_max_1209)


### Computing the mean of Total Plant Area Index

pai_mean_0808<-polyStatsLevel2BVPM(level2BVPM_0808_cl,func=mean(pai), id=NULL)
head(pai_mean_0808)

pai_mean_1209<-polyStatsLevel2BVPM(level2BVPM_1209_cl,func=mean(pai), id=NULL)
head(pai_mean_1209)


### remove nodata values
pai_mean_0808 <- subset(pai_mean_0808, pai_mean_0808$height_bin0 != -9999)
level2BPAVDProfile_1209_cl <- subset(level2BPAVDProfile_1209, level2BPAVDProfile_1209$height_bin0 != -9999)


### Converting PAI shot_number as "integer64" to "character"
level2BPAIProfile_0808_cl$shot_number<-paste0(level2BPAIProfile_0808_cl$shot_number)
level2BPAIProfile_1209_cl$shot_number<-paste0(level2BPAIProfile_1209_cl$shot_number)


### Converting PAVD shot_number as "integer64" to "character"
level2BPAVDProfile_0808_cl$shot_number<-paste0(level2BPAVDProfile_0808_cl$shot_number)
level2BPAVDProfile_1209_cl$shot_number<-paste0(level2BPAVDProfile_1209_cl$shot_number)


### Converting PAI as data.table to SpatialPointsDataFrame
level2BPAIProfile_spdf_0808_cl<-SpatialPointsDataFrame(cbind(level2BPAIProfile_0808_cl$lon_lowestmode,level2BPAIProfile_0808_cl$lat_lowestmode),
                                                       data=level2BPAIProfile_0808_cl)
level2BPAIProfile_spdf_1209_cl<-SpatialPointsDataFrame(cbind(level2BPAIProfile_1209_cl$lon_lowestmode,level2BPAIProfile_1209_cl$lat_lowestmode),
                                                       data=level2BPAIProfile_1209_cl)


### Converting PAVD as data.table to SpatialPointsDataFrame
level2BPAVDProfile_spdf_0808_cl<-SpatialPointsDataFrame(cbind(level2BPAVDProfile_0808_cl$lon_lowestmode,level2BPAVDProfile_0808_cl$lat_lowestmode),
                                                        data=level2BPAVDProfile_0808_cl)
level2BPAVDProfile_spdf_1209_cl<-SpatialPointsDataFrame(cbind(level2BPAVDProfile_1209_cl$lon_lowestmode,level2BPAVDProfile_1209_cl$lat_lowestmode),
                                                        data=level2BPAVDProfile_1209_cl)


### Exporting PAI and PAVD Profiles as ESRI Shapefile
raster::shapefile(level2BPAIProfile_spdf_0808_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAI_0808_cl"))
raster::shapefile(level2BPAIProfile_spdf_1209_cl,paste0("D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2B/L2B_PAI_1209_cl"))



#-------------------------------------------------------------------------------
#start here for stratification by polygon (eg for specific altitudinal level)
#-------------------------------------------------------------------------------

### Computing a serie of statistics of Canopy Cover stratified by polygon
cover_metrics_st<-polyStatsLevel2BVPM(level2BVPM_0808_cl,func=mySetOfMetrics(cover),
                                      id="poly_id")
head(cover_metrics_st)


### Compute Grids with descriptive statistics of GEDI-derived Canopy Cover and 
### Vertical Profile Metrics (Level2B)


### Computing a series of statistics of Total Plant Area Index
pai_metrics_0808<-gridStatsLevel2BVPM(level2BVPM = level2BVPM_0808_cl, func=mySetOfMetrics(pai), res=0.005)
pai_metrics_1209<-gridStatsLevel2BVPM(level2BVPM = level2BVPM_1209_cl, func=mySetOfMetrics(pai), res=0.005)

### View maps

pai_maps_0808<-levelplot(pai_metrics_0808,
                    layout=c(1, 4),
                    margin=FALSE,
                    xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                    colorkey=list(
                      space='right',
                      labels=list(at=seq(0, 1.5, 0.2), font=4),
                      axis.line=list(col='black'),
                      width=1),
                    par.settings=list(
                      strip.border=list(col='gray'),
                      strip.background=list(col='gray'),
                      axis.line=list(col='gray')
                    ),
                    scales=list(draw=TRUE),
                    col.regions=viridis,
                    at=seq(0, 1.5, len=101),
                    names.attr=c("PAI min","PAI max","PAI mean", "PAI sd"))

pai_maps_1209<-levelplot(pai_metrics_1209,
                         layout=c(1, 4),
                         margin=FALSE,
                         xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                         colorkey=list(
                           space='right',
                           labels=list(at=seq(0, 1.5, 0.2), font=4),
                           axis.line=list(col='black'),
                           width=1),
                         par.settings=list(
                           strip.border=list(col='gray'),
                           strip.background=list(col='gray'),
                           axis.line=list(col='gray')
                         ),
                         scales=list(draw=TRUE),
                         col.regions=viridis,
                         at=seq(0, 1.5, len=101),
                         names.attr=c("PAI min","PAI max","PAI mean", "PAI sd"))

### Export maps 
png("fig0808.png", width = 6, height = 8, units = 'in', res = 300)
pai_maps_0808
dev.off()

png("fig1209.png", width = 6, height = 8, units = 'in', res = 300)
pai_maps_1209
dev.off()


#-------------------------------------------------------------------------------
# compute height grids with descriptive statistics of GEDI-derived elevation
#-------------------------------------------------------------------------------

### Computing a serie of statistics of GEDI RH100 metric
rh100metrics<-gridStatsLevel2AM(level2AM = , func=mySetOfMetrics(rh100), res=0.005)


rh100maps<-levelplot(rh100metrics,
                     layout=c(1, 4),
                     margin=FALSE,
                     xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                     colorkey=list(
                       space='right',
                       labels=list(at=seq(0, 18, 2), font=4),
                       axis.line=list(col='black'),
                       width=1),
                     par.settings=list(
                       strip.border=list(col='gray'),
                       strip.background=list(col='gray'),
                       axis.line=list(col='gray')
                     ),
                     scales=list(draw=TRUE),
                     col.regions=viridis,
                     at=seq(0, 18, len=101),
                     names.attr=c("rh100 min","rh100 max","rh100 mean", "rh100 sd"))

### Exporting maps 
png("RH_0808.png", width = 6, height = 8, units = 'in', res = 300)
rh100maps
dev.off()

### Computing a serie of statistics of GEDI RH100 metric
rh100metrics<-gridStatsLevel2AM(level2AM = level2AM_1209_cl, func=mySetOfMetrics(rh100), res=0.005)


rh100maps<-levelplot(rh100metrics,
                     layout=c(1, 4),
                     margin=FALSE,
                     xlab = "Longitude (degree)", ylab = "Latitude (degree)",
                     colorkey=list(
                       space='right',
                       labels=list(at=seq(0, 18, 2), font=4),
                       axis.line=list(col='black'),
                       width=1),
                     par.settings=list(
                       strip.border=list(col='gray'),
                       strip.background=list(col='gray'),
                       axis.line=list(col='gray')
                     ),
                     scales=list(draw=TRUE),
                     col.regions=viridis,
                     at=seq(0, 18, len=101),
                     names.attr=c("rh100 min","rh100 max","rh100 mean", "rh100 sd"))

### Exporting maps 
png("RH_1209.png", width = 6, height = 8, units = 'in', res = 300)
rh100maps
dev.off()


