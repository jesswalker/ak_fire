########################################################################### #
#
# process_ak_frp_data.R 
#
# Objective:  Process MxD14A1 FRP data in order to plot and analyze data.
#
# Input:     Input files of MxD14A1 data were originally produced in
#            GEE for subsets of fire complexes in Alaska.  The FRP files were
#            intersected in Arc with the vector file of fire history to produce
#            points that are tagged with FRP data, EVT data (in the encompassing
#            MODIS 1-km pixel), and fire history data. The value in each EVT
#            class column is the number of 30-m pixels in each 1-km MODIS pixel.
#
#   - mxda1_gee_<yyyy>_plus_fires_R.csv, where <yyyy> = 2003, 2004, 2005, 2009
#
#   JWalker 6 April 2018
#  
########################################################################### #

# Remove existing data and libraries from R environment
rm(list=ls())


# Load libraries
library(lubridate) # dates
library(dplyr)
library(dtplyr)
library(sp)  # for semivariograms
library(gstat)  # ""


# Set paths
path.in <- "D:/projects/ak_fire/"
path.r <- "D:/projects/ak_fire/data"
rdata <- "process_ak_frp_data.R"
path.plots <- "D:/projects/ak_fire/output/plots/frp"

source(file.path(path.in, "R", "ak_functions.R"))

filenames <- c('mxd14a1_gee_2003_plus_fires_r.csv', 'mxd14a1_gee_2004_plus_fires_r.csv',
           'mxd14a1_gee_2005_plus_fires_r.csv', 'mxd14a1_gee_2009_plus_fires_r.csv')

# Read in all files to a single file
x <- do.call(rbind, lapply(file.path(path.in, "data", filenames), read.csv, header=T))

# Retain only specified columns
  colNames <- c("FID", "FID_mxd14a", "index", "class11", "class12", "class21", "class2197", "class22", "class23",
              "class2600", "class2601", "class2602", "class2603", "class2604", "class2605", "class2606", "class2607", "class2608",
              "class2609", "class2610", "class2611", "class2612", "class2631", "class2633", "class2634", "class2635", "class2636",
              "class2740", "class2745", "class2751", "class2753", "class2757", "class2763", "class2773", "class2776", "class2777",
              "class2782", "class2786", "class2793", "class31", "MaxFRP", "latitude", "longitude", "newdate", "year", "FireYear", "FIREID",
              "DiscDate", "Region")
  x <- x[, colNames]  

# Remove all NA-only columns
  x <- Filter(function(y) !all(is.na(y)), x)

# Format date columns and remove old ones
  x$modis_date <- as.Date(x$newdate, format = "%m/%d/%Y")
  x$fire_date <- as.Date(x$DiscDate, format = "%m/%d/%Y")

  x$DiscDate <- NULL
  x$newdate <- NULL

# MODIS MaxFRP is scaled by 10
  x$MaxFRP <- 0.1 * x$MaxFRP

# Function to count the number of times each point appears: 1 = first fire, 2 = 2nd, etc.
  get_fire_order = function(df) {
      df <- df[order(df$fire_date), ]
      df$burn_num <- ave(df$FireYear, df$index, FUN = rank)
      df$reburn <- 0
      df[which(as.numeric(df$burn_num) > 1), ]$reburn <- 1
      df$year_int <- ave(df$FireYear, factor(df$index), FUN=function(t) c(0, diff(t))) #changed from NA
      df <- df[order(df$FID_mxd14a), ]
  return(df)
  }

# Calculate fire order
  x <- get_fire_order(x)

# Remove all within-season burns: entries tagged as reburns but with year_int = 0.
# FID is unique to each point
  x.1season <- subset(x, reburn == 1 & year_int == 0)
  x <- x[!(x$FID %in% x.1season$FID), ]

# Redo the rank calculate to account for changes
  x <- get_fire_order(x)

# Retain only the last (highest) number of burns for a given point;
# otherwise burns are double or triple-counted. 
# Ensure file is ordered by index (unique to sets of points that fall in
# the same pixel), then by burn_num (highest to lowest)
  x <- x[order(x$index, -x$burn_num), ]

# Remove duplicates (all but the first--highest--one)
  x <- x[!duplicated(x$index), ]

# Retrieve the column with the highest # of pixels.
# Look only at "class" columns

 x$max_class <- apply(x, 1, function(y) names(y[grepl("class", names(y))])[which.max(y[grepl("class", names(y))])[1]])
 x$max_class <- as.factor(x$max_class)

# Save data and environment settings  
 print(paste0("R data file saved to ", file.path(path.r, rdata)))
 save.image(file = file.path(path.r, rdata))

