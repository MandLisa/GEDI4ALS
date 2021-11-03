#-------------------------------------------------------------------------------
# Tasks
#-------------------------------------------------------------------------------

#1 Define function to query CMR (common data repository) based on user-provided input (date, BB)
#2 Use GEDI finder to locate GEDI data of your AOI
#3 Download respective GEDI data
#4 Subset GEDI granules
#5 Visualise L1B geolocated pulses using Leaflet and plot pulse distribution
#6 Export GEDI L1B waveform geolocations as ESRI shapefile

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Check for required packages, install if not previously installed
#-------------------------------------------------------------------------------
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Import Packages
library(rGEDI)
library(dplyr)
library(leaflet)
library(httr)
library(rasterVis)
library(viridis)

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
# here, for National Park Berchtesgaden
#-------------------------------------------------------------------------------
product <- 'GEDI01_B.002'           # Options include 'GEDI01_B.002', 'GEDI02_A.002', 'GEDI02_B.002'
bbox <-'12.759659,47.458228,13.098862,47.646021'  # bounding box coords in LL Longitude, LL Latitude, UR Longitude, UR Latitude format


#-------------------------------------------------------------------------------
# Call the gedi_finder function using the user-provided inputs
#-------------------------------------------------------------------------------
granules_L1B <- gedi_finder(product, bbox)
print(sprintf("%s %s Version 2 granules found.", length(granules_L1B), product))

#-------------------------------------------------------------------------------
# convert granules list to vector
#-------------------------------------------------------------------------------

granules_L1B_v <- unlist(granules_L1B, use.names=FALSE)

#-------------------------------------------------------------------------------
# Export Results and save URLS as .txt file
#-------------------------------------------------------------------------------

# Set up output textfile name using the current datetime
outName <- sprintf("%s_GranuleList_%s.txt", sub('.002', '_002', product), format(Sys.time(), "%Y%m%d%H%M%S"))

# Save to text file in current working directory
write.table(granules_L1B, outName, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, getwd(), outName))


#-------------------------------------------------------------------------------
# Download GEDI data; index determines position of granule in txt file
#-------------------------------------------------------------------------------
#gediDownload(filepath = granules_v[7], outdir = outdir)
#gediDownload(filepath = granules_v[11], outdir = outdir)
#gediDownload(filepath = granules_v[17], outdir = outdir)
gediDownload(filepath = granules_L1B_v[2], outdir = outdir)
gediDownload(filepath = granules_L1B_v[8], outdir = outdir)
gediDownload(filepath = granules_L1B_v[10], outdir = outdir)
gediDownload(filepath = granules_L1B_v[15], outdir = outdir)
gediDownload(filepath = granules_L1B_v[18], outdir = outdir)

#-------------------------------------------------------------------------------
# Read the h5 GEDI files
#-------------------------------------------------------------------------------

gedilevel1b_1 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019180075351_O03082_03_T00826_02_005_01_V002.h5"))
gedilevel1b_2 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019255220154_O04255_02_T05016_02_005_01_V002.h5"))
gedilevel1b_3 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019331160117_O05430_02_T02170_02_005_01_V002.h5"))
gedilevel1b_4 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019128004537_O02270_02_T04863_02_005_01_V002.h5"))
gedilevel1b_5 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019192225830_O03278_02_T02323_02_005_01_V002.h5"))
gedilevel1b_6 <- readLevel1B(level1Bpath = file.path(outdir, "GEDI01_B_2019220155440_O03708_03_T05095_02_005_01_V002.h5"))


#-------------------------------------------------------------------------------
# subset data based on BB (NP BGD)
#-------------------------------------------------------------------------------

xmin = 12.759659
xmax = 13.098862
ymin = 47.458228
ymax = 47.646021

# subset gedi data  
level1b_subset_1 <- clipLevel1B(gedilevel1b_1, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_1.h5")
level1b_subset_2 <- clipLevel1B(gedilevel1b_2, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_2.h5")
level1b_subset_3 <- clipLevel1B(gedilevel1b_3, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_3.h5")
level1b_subset_4 <- clipLevel1B(gedilevel1b_4, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_4.h5")
level1b_subset_5 <- clipLevel1B(gedilevel1b_5, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_5.h5")
level1b_subset_6 <- clipLevel1B(gedilevel1b_6, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_6.h5")
level1b_subset_7 <- clipLevel1B(gedilevel1b_7, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_7.h5")
level1b_subset_8 <- clipLevel1B(gedilevel1b_8, xmin, xmax, ymin, ymax, output = "GEDI_1B_subset_8.h5")


#-------------------------------------------------------------------------------
#read subset data
#-------------------------------------------------------------------------------

gedilevel1b_subset_0808 <- readLevel1B(level1Bpath = file.path(outdir, "L1B/GEDI_1B_subset_0808.h5"))

gedilevel1b_subset_1209 <- readLevel1B(level1Bpath = file.path(outdir, "L1B/GEDI_1B_subset_1209.h5"))


# read and get new subsetted h5 data files for all two datasets
level1bGeo_subset_0808 <- getLevel1BGeo(level1b = gedilevel1b_subset_0808, select=c("elevation_bin0"))
head(level1bGeo_subset_0808)

level1bGeo_subset_1209 <- getLevel1BGeo(level1b = gedilevel1b_subset_1209, select=c("elevation_bin0"))
head(level1bGeo_subset_1209)


#-------------------------------------------------------------------------------
# Plotting the subsetted data
#-------------------------------------------------------------------------------

# add NP border polygon
library(sf)
NP <- st_read(
  "T:/transit/Lisa/_tausch/Grenze NP_1/grenze_np.shp")

leaflet() %>%
  addCircleMarkers(level1bGeo_subset_0808$longitude_bin0,
                    level1bGeo_subset_0808$latitude_bin0,
                    radius = 1,
                    opacity = 1,
                    color = "red")  %>%
  addCircleMarkers(level1bGeo_subset_1209$longitude_bin0,
                   level1bGeo_subset_1209$latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   color = "orange")  %>%
  #addPolygons(data = NP,
             # color = "yellow") %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(colors = c("red", "orange"), labels= c("Aug_8", "Sep_12"),title ="GEDI Level1B")
 

#-------------------------------------------------------------------------------
# Prepare geolocation data for export as .shp
#------------------------------------------------------------------------------- 

#Converting shot_number as "integer64" to "character"
level1bGeo_subset_0808$shot_number<-paste0(level1bGeo_subset_0808$shot_number)
level1bGeo_subset_1209$shot_number<-paste0(level1bGeo_subset_1209$shot_number)


#-------------------------------------------------------------------------------
# Converting level1bGeo as data.table to SpatialPointsDataFrame
#-------------------------------------------------------------------------------
library(sp)
level1bGeo_subset_0808_spdf<-SpatialPointsDataFrame(cbind(level1bGeo_subset_0808$longitude_bin0, level1bGeo_subset_0808$latitude_bin0),
                                                data=level1bGeo_subset_0808)

level1bGeo_subset_1209_spdf<-SpatialPointsDataFrame(cbind(level1bGeo_subset_1209$longitude_bin0, level1bGeo_subset_1209$latitude_bin0),
                                                 data=level1bGeo_subset_1209)


#-------------------------------------------------------------------------------
# Exporting level1bGeo as ESRI Shapefile
#-------------------------------------------------------------------------------
raster::shapefile(level1bGeo_subset_0808_spdf,paste0(outdir,"\\GEDI01_B_0808_sub"))

raster::shapefile(level1bGeo_subset_1209_spdf,paste0(outdir,"\\GEDI01_B_1209_sub"))


#-------------------------------------------------------------------------------
# Get GEDI Full-waveform (GEDI Level1B)
#-------------------------------------------------------------------------------

# Extracting GEDI full-waveform for a given shotnumber
wf_0808 <- getLevel1BWF(gedilevel1b_subset_0808, shot_number="37080000300139047")

wf_1209 <- getLevel1BWF(gedilevel1b_subset_1209, shot_number="42550000200197596")


# subset 1
par(mfrow = c(2,1), mar=c(4,4,1,1), cex.axis = 1.5)

plot(wf_0808, relative=FALSE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude", ylab="Elevation (m)")
grid()
plot(wf_0808, relative=TRUE, polygon=FALSE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude (%)", ylab="Elevation (m)")
grid()

# subset 2

par(mfrow = c(2,1), mar=c(4,4,1,1), cex.axis = 1.5)

plot(wf_1209, relative=FALSE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude", ylab="Elevation (m)")
grid()
plot(wf_1209, relative=TRUE, polygon=FALSE, type="l", lwd=2, col="forestgreen",
     xlab="Waveform Amplitude (%)", ylab="Elevation (m)")
grid()



