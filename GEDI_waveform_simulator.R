#-------------------------------------------------------------------------------
# Tasks
#-------------------------------------------------------------------------------

#1 
#2 
#3 
#4 

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Check for required packages, install if not previously installed
#-------------------------------------------------------------------------------

# install needed packages
install.packages("plot3D")

# Import Packages
library(rGEDI)
library(dplyr)
library(leaflet)
library(httr)


#-------------------------------------------------------------------------------
# Assign your working directory
#-------------------------------------------------------------------------------

myDirectory <- "D:/GEDI/R_GEDI_scripts"

# set your working directory
setwd(myDirectory)

# assign an output directory
outdir <- myDirectory

#-------------------------------------------------------------------------------
# apply GEDI waveform simulator to ALS LiDAR data
#-------------------------------------------------------------------------------

# Specifying the path to ALS data
lasfile_kr <- "Z:/00_basis/lidar/2017/laserdata/Schoenau_LAS/4572_5270_all.las"


# Reading and plot ALS file
library(lidR)
library(plot3D)

las_kr<-readLAS(lasfile_kr)


# Extracting plot center geolocations
xcenter_kuehroint = mean(las_kr@bbox[1,])
ycenter_kuehroint = mean(las_kr@bbox[2,])


# Simulating GEDI full-waveform
wf_kuehroint<-gediWFSimulator(input=lasfile_kr,output=paste0(getwd(),"//gediWF_kuehroint_simulation.h5"),coords = c(xcenter_kuehroint, ycenter_kuehroint))


# Plotting ALS and GEDI simulated full-waveform
png("gediWf.png", width = 8, height = 6, units = 'in', res = 300)

par(mfrow=c(2,2), mar=c(4,4,0,0), oma=c(0,0,1,1),cex.axis = 1.2)
scatter3D(las_kr@data$X,las_kr@data$Y,las_kr@data$Z,pch = 16,colkey = FALSE, main="",
          cex = 0.5,bty = "u",col.panel ="gray90",phi = 30,alpha=1,theta=45,
          col.grid = "gray50", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Elevation (m)")

plot(wf_kuehroint, relative=TRUE, polygon=TRUE, type="l", lwd=2, col="forestgreen",
     xlab="", ylab="Elevation (m)", ylim=c(1300, 1500))
grid()

dev.off()

close(wf_kuehroint)