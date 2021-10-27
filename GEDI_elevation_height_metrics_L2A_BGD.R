#-------------------------------------------------------------------------------
# Tasks
#-------------------------------------------------------------------------------

#1 Define function to query CMR (common data repository) based on user-provided input (date, BB)
#2 Use GEDI finder to locate GEDI data of your AOI
#3 Download and subset respective GEDI data
#4 Compute height metrics

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Check for required packages, install if not previously installed
#-------------------------------------------------------------------------------


# Import Packages
library(rGEDI)
library(dplyr)
library(leaflet)
library(httr)


#-------------------------------------------------------------------------------
# Assign your working directory
#-------------------------------------------------------------------------------


myDirectory <- "D:/Mandl_L_PhD/NP_BGD/GEDI/_data/L2A"

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
product <- 'GEDI02_A.002'           # Options include 'GEDI01_B.002', 'GEDI02_A.002', 'GEDI02_B.002'
bbox <-'12.759659,47.458228,13.098862,47.646021'  # bounding box coords in LL Longitude, LL Latitude, UR Longitude, UR Latitude format


#-------------------------------------------------------------------------------
# Call the gedi_finder function using the user-provided inputs
#-------------------------------------------------------------------------------
granules_L2A <- gedi_finder(product, bbox)
print(sprintf("%s %s Version 2 granules found.", length(granules_L2A), product))

#-------------------------------------------------------------------------------
# convert granules list to vector
#-------------------------------------------------------------------------------

granules_L2A_v <- unlist(granules_L2A, use.names=FALSE)

#-------------------------------------------------------------------------------
# Export Results and save URLS as .txt file
#-------------------------------------------------------------------------------

# Set up output textfile name using the current datetime
outName_L2A <- sprintf("%s_GranuleList_%s.txt", sub('.002', '_002', product), format(Sys.time(), "%Y%m%d%H%M%S"))

# Save to text file in current working directory
write.table(granules_L2A, outName_L2A, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, getwd(), outName_L2A))

#-------------------------------------------------------------------------------
# Download GEDI data
#-------------------------------------------------------------------------------
gediDownload(filepath = granules_L2A_v[10], outdir = outdir)
gediDownload(filepath = granules_L2A_v[11], outdir = outdir)
gediDownload(filepath = granules_L2A_v[12], outdir = outdir)
gediDownload(filepath = granules_L2A_v[13], outdir = outdir)
gediDownload(filepath = granules_L2A_v[14], outdir = outdir)
gediDownload(filepath = granules_L2A_v[15], outdir = outdir)


#-------------------------------------------------------------------------------
# Read the h5 GEDI files (necessary at all??) - h5 file names do not match!!
#-------------------------------------------------------------------------------

gedilevel2a_0808 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019180075351_O03082_03_T00826_02_003_01_V002.h5"))
gedilevel2a_1209 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019255220154_O04255_02_T05016_02_003_01_V002.h5"))
gedilevel2a_0710 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019331160117_O05430_02_T02170_02_003_01_V002.h5"))
gedilevel2a_0910 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019128004537_O02270_02_T04863_02_003_01_V002.h5"))
gedilevel2a_0111 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019220155440_O03708_03_T05095_02_003_01_V002.h5"))
gedilevel2a_0311 <- readLevel2A(level2Apath = file.path(outdir, "GEDI02_A_2019220155440_O03708_03_T05095_02_003_01_V002.h5"))

#-------------------------------------------------------------------------------
# subset data based on BB
#-------------------------------------------------------------------------------

xmin = 12.759659
xmax = 13.098862
ymin = 47.458228
ymax = 47.646021

# subset gedi data  
# attention, subset 8 doesnt work!
level2a_subset_0808 <- clipLevel2A(gedilevel2a_0808, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_0808.h5")
level2a_subset_1209 <- clipLevel2A(gedilevel2a_1209, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_1209.h5")
level2a_subset_0710 <- clipLevel2A(gedilevel2a_0710, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_0710.h5")
level2a_subset_0910 <- clipLevel2A(gedilevel2a_0910, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_0910.h5")
level2a_subset_0111 <- clipLevel2A(gedilevel2a_0111, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_0111.h5")
level2a_subset_0311 <- clipLevel2A(gedilevel2a_0311, xmin, xmax, ymin, ymax, output = "GEDI_2A_subset_0311.h5")



#-------------------------------------------------------------------------------
#read subset data
#-------------------------------------------------------------------------------


gedilevel2a_subset_0808 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_0808.h5"))

gedilevel2a_subset_1209 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_1209.h5"))

gedilevel2a_subset_0710 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_0710.h5"))

gedilevel2a_subset_0910 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_0910.h5"))

gedilevel2a_subset_0111 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_0111.h5"))

gedilevel2a_subset_0311 <- readLevel2A(level2Apath = file.path(outdir, "L2A/GEDI_2A_subset_0311.h5"))


#-------------------------------------------------------------------------------
# Get GEDI Elevation and Height Metrics (GEDI Level2A)
#-------------------------------------------------------------------------------

level2AM_0808<-getLevel2AM(gedilevel2a_subset_0808)
head(level2AM_0808[,c("beam","shot_number","elev_highestreturn","elev_lowestmode","rh100")])

level2AM_1209<-getLevel2AM(gedilevel2a_subset_1209)
head(level2AM_1209[,c("beam","shot_number","elev_highestreturn","elev_lowestmode","rh100")])



# remove nodata values
level2AM_0808_cl <- subset(level2AM_0808, level2AM_0808$quality_flag == 1)
level2AM_1209_cl <- subset(level2AM_1209, level2AM_1209$quality_flag == 1)

#-------------------------------------------------------------------------------
#Converting shot_number as "integer64" to "character"
#-------------------------------------------------------------------------------
level2AM_0808_cl$shot_number<-paste0(level2AM_0808_cl$shot_number)

level2AM_1209_cl$shot_number<-paste0(level2AM_1209_cl$shot_number)


#-------------------------------------------------------------------------------
# Converting Elevation and Height Metrics as data.table to SpatialPointsDataFrame
#-------------------------------------------------------------------------------
level2AM_0808_spdf_cl<-SpatialPointsDataFrame(cbind(level2AM_0808_cl$lon_lowestmode,level2AM_0808_cl$lat_lowestmode),
                                      data=level2AM_0808_cl)

level2AM_1209_spdf_cl<-SpatialPointsDataFrame(cbind(level2AM_1209_cl$lon_lowestmode,level2AM_1209_cl$lat_lowestmode),
                                      data=level2AM_1209_cl)


#-------------------------------------------------------------------------------
# Exporting Elevation and Height Metrics as ESRI Shapefile
#-------------------------------------------------------------------------------
raster::shapefile(level2AM_0808_spdf_cl,paste0(outdir,"\\GEDI_2A_RH_0808"))

raster::shapefile(level2AM_1209_spdf_cl,paste0(outdir,"\\GEDI_2A_RH_1209"))


#-------------------------------------------------------------------------------
# Plot waveform with RH metrics
# enter for shot number any of the available shot numbers
#-------------------------------------------------------------------------------
shot_number = "37080000300139047"

png("wf_RH_metrics.png", width = 8, height = 6, units = 'in', res = 300)
plotWFMetrics(gedilevel1b_subset_0808, gedilevel2a_subset_0808, shot_number, rh=c(25, 50, 75, 90))
dev.off()

plotWFMetrics(gedilevel1b_subset_0808, gedilevel2a_subset_0808, shot_number, rh=c(25, 50, 75, 90))


#-------------------------------------------------------------------------------
# Compute descriptive statistics from GEDI L2A and L2B
#-------------------------------------------------------------------------------

# Define your own function
mySetOfxMetrics = function(x)
{
  metrics = list(
    min =min(x), # Min of x
    max = max(x), # Max of x
    mean = mean(x), # Mean of x
    sd = sd(x)# Sd of x
  )
  return(metrics)
}

# Computing the maximum of RH100 stratified by polygon
rh100max_st<-polyStatsLevel2AM(level2AM_clip_gb,func=max(rh100), id="poly_id")
head(rh100max_st)

# Computing a serie statistics for GEDI metrics stratified by polygon
rh100metrics_st<-polyStatsLevel2AM(level2AM_clip_gb,func=mySetOfMetrics(rh100),
                                   id="poly_id")
head(rh100metrics_st)

# Computing the max of the Total Plant Area Index
pai_max<-polyStatsLevel2BVPM(level2BVPM_clip_gb,func=max(pai), id=NULL)
pai_max

# Computing a series of statistics of Canopy Cover stratified by polygon
cover_metrics_st<-polyStatsLevel2BVPM(level2BVPM_clip_gb,func=mySetOfMetrics(cover),
                                      id="poly_id")
head(cover_metrics_st)