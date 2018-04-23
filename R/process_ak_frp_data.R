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
library(tidyr)
library(sp)  # for semivariograms
library(gstat)  # ""
library(spdep)


# Set paths
path.in <- "D:/projects/ak_fire/"
path.r <- "D:/projects/ak_fire/data"
rdata <- "process_ak_frp_data.R"
path.plots <- "D:/projects/ak_fire/output/plots/frp"

source(file.path(path.in, "R", "ak_functions.R"))

filenames <- c('mxd14a1_gee_2003_plus_fires.csv', 'mxd14a1_gee_2004_plus_fires.csv',
           'mxd14a1_gee_2005_plus_fires.csv', 'mxd14a1_gee_2009_plus_fires.csv')

# Read in all files to a single file
x.all <- do.call(rbind, lapply(file.path(path.in, "data", filenames), read.csv, header=T))

# Retain only specified columns
colNames <- c("FID", "FID_mxd14a", "index", "class11", "class12", "class21", "class2197", "class22", "class23",
              "class2600", "class2601", "class2602", "class2603", "class2604", "class2605", "class2606", "class2607", "class2608",
              "class2609", "class2610", "class2611", "class2612", "class2631", "class2633", "class2634", "class2635", "class2636",
              "class2740", "class2745", "class2751", "class2753", "class2757", "class2763", "class2773", "class2776", "class2777",
              "class2782", "class2786", "class2793", "class31", "MaxFRP", "latitude", "longitude", "newdate", "year", "FireYear", "FIREID",
              "DiscDate", "Region", "Region2")
x <- x.all[, colNames]  


evt_classes <- data.frame(class =  
                c("class11","class12",  "class21"  , "class2197","class22","class23","class2600","class2601","class2602","class2603",
                "class2604","class2605","class2606","class2607","class2608","class2609","class2610",
                "class2611","class2612","class2631","class2633","class2634","class2635","class2636",
                "class2740","class2745","class2751","class2753","class2757","class2763","class2773",
                "class2776","class2777","class2782","class2786","class2793","class31"),

            class_name = c("Water","Snow-Ice", "Dev-Open", "Burned", "Dev-Low", "Dev-Med", "WS Forest", "WS Wood",
                  "Sprc-Lchn Wood", "WS-Hdwd Forest", "BS Forest","Brch Aspn", "Aspn-Steppe",
                  "Pplr-Aspn", "Avanlanche Slp", "Aldr Shrub", "Brch-Wllw Shrub", "Bluejoint",          
                  "Dry Grss", "Shrb Summit", "Herb Meadow", "Dryas Shrub", "Ericcs Shrub",   
                  "Shrb-Lchn Shrub", "Aquatic Beds", "Herb Wetlands", "Woody Wetlands", "C-D Wetlands",       
                  "Shrub Wetlands", "Floodplains", "Peatlands", "Riparian", "Swamp",
                  "Tussock Tundra", "Shrb-Tussock Tundra", "Sparse", "Barren")  ) 



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
  
# Remove all burns for which the MODIS date predates the burn data
x <- x[which(x$modis_date > x$fire_date), ]
  
# Retain only the last (highest) number of burns for a given point;
# otherwise burns are double or triple-counted. 
# Ensure file is ordered by index (unique to sets of points that fall in
# the same pixel), then by burn_num (highest to lowest)
x <- x[order(x$index, -x$burn_num), ]

# Remove duplicates (all but the first--highest--point)
x <- x[!duplicated(x$index), ]

# Retrieve the column with the highest # of pixels.
# Look only at "class" columns
x$max_class <- apply(x, 1, function(y) names(y[grepl("class", names(y))])[which.max(y[grepl("class", names(y))])[1]])

x$max_class <- as.factor(x$max_class)

# Housekeeping
rm(x.1season)
 
# STATS 

# FRP by burn count (data)
x$burn_num <- as.factor(x$burn_num)

# why is this flaky and doesn't work sometimes
x.frp <- x %>%
          group_by(as.factor(burn_num)) %>%
          summarize(avg = mean(MaxFRP),
                    med = median(MaxFRP))

# # this works!
# x.frp <- ddply(x, .(burn_num), summarize,
#                avg = mean(MaxFRP),
#                med = median(MaxFRP))
# Round decimals
means$MaxFRP <- round(means[,2], 1)

# Get highest classes
x.maxclass <- summary(x$max_class)

# Get FRP by veg class
x.frp.class <- ddply(x, .(max_class, burn_num), summarize,
               avg = mean(MaxFRP),
               med = median(MaxFRP),
               n = length(burn_num))

x.frp.class <- x %>% 
               group_by(max_class, burn_num) %>%
               summarize (avg = mean(MaxFRP), 
                          med=median(MaxFRP), 
                          n=length(burn_num))

# ID classes with higher #s of points
x.frp.class.sub <- subset(x.frp.class, n > 30)


# Only keep those classes that have at least 30 points in burn_num 2
x.frp.class.sub <- x.frp.class.sub[!(as.numeric(x.frp.class.sub$max_class) %in% which(table(x.frp.class.sub$max_class) < 2)),]
            
# Assign class name
x.frp.class.sub <- merge(x.frp.class.sub, evt_classes, by.x = "max_class", by.y = "class")

# 
# > x.frp.class.sub
#    max_class burn_num       avg    med    n          class_name
# 1  class2600        1 317.17533  93.70 3365           WS Forest
# 2  class2600        2 204.77702 104.60  457           WS Forest
# 3  class2602        1 392.85854 129.30   41      Sprc-Lchn Wood
# 4  class2603        1 202.57169  79.70  650      WS-Hdwd Forest
# 5  class2603        2 150.35935  71.40  123      WS-Hdwd Forest
# 6  class2604        1 299.72281 113.80 1657           BS Forest
# 7  class2604        2 190.11410 131.40  156           BS Forest
# 8  class2605        1 227.17803  83.80  851           Brch Aspn
# 9  class2605        2 133.49634  71.45  246           Brch Aspn
# 10 class2605        3 128.09318  79.90   44           Brch Aspn
# 11 class2610        1 170.51291  83.60  395     Brch-Wllw Shrub
# 12 class2610        2  93.93385  70.80   65     Brch-Wllw Shrub
# 13 class2612        1 424.34725 175.25  218            Dry Grss
# 14 class2634        1  81.46458  43.50   48         Dryas Shrub
# 15 class2745        1 154.59416  72.90  137       Herb Wetlands
# 16 class2745        2 275.34213 149.80  178       Herb Wetlands
# 17 class2751        1 274.31278  88.00 1612      Woody Wetlands
# 18 class2751        2 181.65263  71.10  171      Woody Wetlands
# 19 class2757        1  74.74186  49.10   43      Shrub Wetlands
# 20 class2763        1 107.49452  54.90  693         Floodplains
# 21 class2763        2 203.72286  90.00  140         Floodplains
# 22 class2782        1 339.62899  60.10   69      Tussock Tundra
# 23 class2782        3  60.11875  50.30   32      Tussock Tundra
# 24 class2786        1 189.89763  75.20  927 Shrb-Tussock Tundra
# 25 class2786        2 188.95450  84.30  444 Shrb-Tussock Tundra
# 26 class2786        3  72.54154  41.10   65 Shrb-Tussock Tundra

# Combine 
x.t <- merge(x, evt_classes, by.x = "max_class", by.y = "class")




# Save data and environment settings   
 print(paste0("R data file saved to ", file.path(path.r, rdata)))
 save.image(file = file.path(path.r, rdata))

