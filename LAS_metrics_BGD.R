#-------------------------------------------------------------------------------
# R script for computing ALS metrics with the forestR package
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Tasks
#-------------------------------------------------------------------------------

# 1
# 2
# 3

#-------------------------------------------------------------------------------
# Check for required packages, install if not previously installed
#-------------------------------------------------------------------------------

if ("forestr" %in% rownames(installed.packages()) == FALSE) { install.packages("forestr")}

# Import Packages
library(forestr)
library(lidR)
library(gstat)
library(raster)
library(ggpubr)

#-------------------------------------------------------------------------------
# Assign your working directory
#-------------------------------------------------------------------------------

myDirectory <- "D:/Mandl_L_PhD/NP_BGD/Data"

# set your working directory
setwd(myDirectory)

# assign an output directory
outdir <- myDirectory

#-------------------------------------------------------------------------------
# Load and read LAS file(s)
#-------------------------------------------------------------------------------

# create variable which stores the LAS file and read it

LASfile <- "D:/GEDI/R_ALS_scripts/ALS_processing_scripts/4569_5272_all.las"
las <- readLAS(LASfile)
plot(las)

# if you want to apply a filter to remove outliers, use:
#las <- readLAS((LASfile),  filter = "-drop_z_below 150 -drop_z_above 325")

#-------------------------------------------------------------------------------
# define CRS to las using the EPSG code
#-------------------------------------------------------------------------------

projection(las) <- 25832
sf::st_crs(las)$input
epsg(las)

#-------------------------------------------------------------------------------
# clip point cloud (if needed) - change to round plot as soon as available
#-------------------------------------------------------------------------------

# define centroid coordinates of the LAS tile
x <- 4569496 #easting 
y <- 5272503 #northing

# cut out a 200 x 200 m buffer by adding 100 m to easting and northing 
# coordinates (x,y). As GEDI footprint is 25m, adapt to GEDI (25m)
data.200m <- 
  clip_rectangle(las,
                 xleft = (x - 100), ybottom = (y - 100),
                 xright = (x + 100), ytop = (y + 100))


#-------------------------------------------------------------------------------
# Normalize tree height to ground
#-------------------------------------------------------------------------------

# Correct for elevation 
# First set the center of where you want the plot to be (note easting 
# and northing works in units of m because these data are in a UTM 
# projection as shown in the summary above).

# Correct for ground height using a kriging function to interpolate 
# elevation from ground points in the .laz file.
# If the function will not run, then you may need to check for outliers
# by adjusting the 'drop_z_' arguments when reading in the .laz files.

dtm <- grid_terrain(data.200m, 1, kriging(k = 10L))

data.200m <- normalize_height(data.200m, dtm)

#Will often give a warning if not all points could be corrected, but visually 
# check to see if it corrected for ground height 
plot(data.200m)

# apply height filter (in terms of minimum tree height limit, here 3m)
data.200m@data$Z[data.200m@data$Z <= 3] <- NA  

#visualize the clipped plot point cloud
plot(data.200m) 


#-------------------------------------------------------------------------------
# Compute some basic raster products 
#-------------------------------------------------------------------------------

# Generate DTM
LASfile_DTM <- "D:/GEDI/R_ALS_scripts/ALS_processing_scripts/4569_5272_all.las"

# visualise point cloud
las_DTM <- readLAS(LASfile_DTM, select = "xyzc")
plot(las_DTM, size = 8, bg = "white")

# compute DEM using TIN
dtm_tin <- grid_terrain(las_DTM, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")
  
# compute hillshade from DTM
dtm_tin <- crop(dtm_tin, extent(dtm_tin) - 3) # remove 3m of buffer
dtm_prod <- terrain(dtm_tin, opt = c("slope", "aspect"))
dtm_tin_hillshade <- hillshade(slope = dtm_prod$slope, apsect = dtm_prod$aspect)
plot(dtm_tin_hillshade, col = grey.colors(50, 0, 1), legend = FALSE)


# Generate DSM

#
# point-to-raster
#

# keep in mind: some pixels can be empty if grid resolution is too fine for the
# available point density ("voids")
chm <- grid_canopy(las_DTM, res = 1, algorithm = p2r())
col <- height.colors(50)
plot(chm, col = col)

# reduce number of voids in DSM
# every point in point cloud is replaced with a disk of a known radius (e.g. 15cm)
chm_voidfree <- grid_canopy(las_DTM, res = 0.5, algorithm = p2r(subcircle = 0.15))
plot(chm_voidfree, col = col)

# alternative: interpolate remaining empty pixels
chm_voidfree1 <- grid_canopy(las_DTM, res = 0.5, p2r(0.2, na.fill = tin()))
plot(chm_voidfree1, col = col)
summary(chm_voidfree1)

#
# CHM using triangulation
#advantages: it's parameter-free, does not output empty pixels
chm_tin <- grid_canopy(las_DTM, res = 0.5, algorithm = dsmtin())
plot(chm_tin, col = col)

# when there are a lot of missing points (e.g. on lakes), use the max_edge
# argument. It defines the maximum edge of a triangle allowed in the Delaunay
# triangulation. By default, it is 0, meaning no triangles are removed.
# When setting the argument to 8, that means that an edge longer than 8 will be
# discarded from the triangulation

chm_maxedge <- grid_canopy(las_DTM, res = 0.5, algorithm = dsmtin(max_edge = 8))
plot(chm_maxedge, col = col)

#
# CHM using pitfree algorithm
#

# by increasing the max_edge argument, the CHM becomes smoother, but also more
# unrealistic
chm_pitfree <- grid_canopy(las_DTM, res = 0.5, pitfree(thresholds = c(0, 10, 20),
                                                       max_edge = c(0, 1.5)))
plot(chm_pitfree, col = col)



#-------------------------------------------------------------------------------
# compute structural diversity metrics along 5 taxonomic categories
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# category 1: area & density metrics
#-------------------------------------------------------------------------------

#1: VAI (Vegetation Area Index)


#some of the next few functions won't handle NAs, so we need 
#to filter these out of a vector of Z points
Zs <- data.200m@data$Z
Zs <- Zs[!is.na(Zs)]

#leaf area density, assesses leaf area in the canopy volume 
#k = 0.5 is a standard extinction coefficient for foliage 
#dz = 1 partitions point cloud in 1 m horizontal slices 
#z0 is set to the same height as gap fraction profile above

LADen <- LAD(Zs, dz = 1, k=0.5, z0=3) 
plot(LADen, col = "#00AFBB", pch = 19)

# VAI, sum of leaf area density values for all horizontal slices assessed in 
# previous line
VAI.AOP <- sum(LADen$lad, na.rm=TRUE)
plot(VAI.AOP)
summary(VAI.AOP)
head(VAI.AOP)


#2: Maximum VAI - does not work so far
VAImean <- grid_metrics(las, ~mean(VAI.AOP), 10)
plot(VAImean, col = height.colors(50))
summary(VAImean)

#-------------------------------------------------------------------------------
# category 2: Height metrics
#-------------------------------------------------------------------------------

#3: mean height on grid level

hmean <- grid_metrics(las, ~mean(Z), 1)
plot(hmean, col = height.colors(50))


# plot 56 predefined metrics

metrics <- grid_metrics(las, .stdmetrics, 1)
plot(metrics, col = height.colors(50))


#-------------------------------------------------------------------------------
#define custom metrics
#-------------------------------------------------------------------------------

# metric_1
custom_metric <- function(z) {
  list(
    coef_var <- sd(z) / mean(z) * 100,
    iqr <- IQR(z)
  )
}


#MEAN OUTER CANOPY HEIGHT (MOCH)
#calculate MOCH, the mean CHM height value
mean.max.canopy.ht <- mean(chm_voidfree@data@values, na.rm = TRUE)
plot(mean.max.canopy.ht)
summary(mean.max.canopy.ht)

#MAX CANOPY HEIGHT
#calculate HMAX, the maximum CHM height value
max.canopy.ht <- max(chm_voidfree@data@values, na.rm=TRUE) 
summary(max.canopy.ht)

#RUMPLE
#calculate rumple, a ratio of outer canopy surface area to 
#ground surface area (1600 m^2)
rumple <- rumple_index(chm_voidfree) 
plot(rumple)
summary(rumple)

#TOP RUGOSITY
#top rugosity, the standard deviation of pixel values in chm and 
#is a measure of outer canopy roughness
top.rugosity <- sd(chm_voidfree@data@values, na.rm = TRUE) 
summary(top.rugosity)

#DEEP GAPS & DEEP GAP FRACTION
#number of cells in raster (also area in m2)
cells <- length(chm_voidfree@data@values) 
chm.0 <- chm_voidfree
chm.0[is.na(chm.0)] <- 0 #replace NAs with zeros in CHM
#create variable for the number of deep gaps, 1 m^2 canopy gaps
zeros <- which(chm.0@data@values == 0) 
deepgaps <- length(zeros) #number of deep gaps
summary(deepgaps)
#deep gap fraction, the number of deep gaps in the chm relative 
#to total number of chm pixels
deepgap.fraction <- deepgaps/cells 
summary(deepgap.fraction)

#COVER FRACTION
#cover fraction, the inverse of deep gap fraction
cover.fraction <- 1 - deepgap.fraction 
summary(cover.fraction)

#-------------------------------------------------------------------------------
# all metrics in one list
#-------------------------------------------------------------------------------

#Zip up all the code we previously used and write function to 
#run all 13 metrics in a single function. 
structural_diversity_metrics <- function(las) {
  chm <- grid_canopy(las, res = 1, dsmtin()) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  rumple <- rumple_index(chm) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  cells <- length(chm@data@values) 
  chm.0 <- chm
  chm.0[is.na(chm.0)] <- 0 
  zeros <- which(chm.0@data@values == 0) 
  deepgaps <- length(zeros) 
  deepgap.fraction <- deepgaps/cells 
  cover.fraction <- 1 - deepgap.fraction 
  vert.sd <- cloud_metrics(data.40m, sd(Z, na.rm = TRUE)) 
  sd.1m2 <- grid_metrics(data.40m, sd(Z), 1) 
  sd.sd <- sd(sd.1m2[,3], na.rm = TRUE) 
  Zs <- data.40m@data$Z
  Zs <- Zs[!is.na(Zs)]
  entro <- entropy(Zs, by = 1) 
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0=3)
  GFP.AOP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=0.5, z0=3) 
  VAI.AOP <- sum(LADen$lad, na.rm=TRUE) 
  VCI.AOP <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(
    matrix(c(x, y, mean.max.canopy.ht,max.canopy.ht, 
             rumple,deepgaps, deepgap.fraction, 
             cover.fraction, top.rugosity, vert.sd, 
             sd.sd, entro, GFP.AOP, VAI.AOP,VCI.AOP),
           ncol = 15)) 
  colnames(out.plot) <- 
    c("easting", "northing", "mean.max.canopy.ht.aop",
      "max.canopy.ht.aop", "rumple.aop", "deepgaps.aop",
      "deepgap.fraction.aop", "cover.fraction.aop",
      "top.rugosity.aop","vert.sd.aop","sd.sd.aop", 
      "entropy.aop", "GFP.AOP.aop",
      "VAI.AOP.aop", "VCI.AOP.aop") 
  print(out.plot)
}

ALS_structural_diversity <- structural_diversity_metrics(las)
