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
#            MODIS 1-km pixel), and fire history data.
#
#   - mxda1_gee_<yyyy>_plus_fires_R.csv, where <yyyy> = 2003, 2004, 2005, 2009
#
#   JWalker 6 April 2018
#  
########################################################################### #


year <- '2005'

# Load libraries
library(ggmap)
library(lubridate) # dates
library(dtplyr)

# Set paths
filename = paste0('mxd14a1_gee_', year, '_plus_fires_r.csv')
path.in <- "D:/projects/ak_fire"
df <- read.csv(file.path(path.in, "data", filename), header = T)

# Retain only specified columns
colNames <- c("FID", "FID_mxd14a", "index", "class11", "class12", "class21", "class2197", "class22", "class23",
              "class2600", "class2601", "class2602", "class2603", "class2604", "class2605", "class2606", "class2607", "class2608",
              "class2609", "class2610", "class2611", "class2612", "class2631", "class2633", "class2634", "class2635", "class2636",
              "class2740", "class2745", "class2751", "class2753", "class2757", "class2763", "class2773", "class2776", "class2777",
              "class2782", "class2786", "class2793", "class31", "MaxFRP", "latitude", "longitude", "newdate", "year", "FireYear", "FIREID",
              "DiscDate", "Region")
x <- df[, colNames]  

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
x <- x[!duplicated(xx$index), ]



# PLOTS

# by burn number
ggplot(x, aes(as.factor(burn_num), MaxFRP)) + geom_boxplot()  + ylim(0, 100)

# by years since prior burn
# Drop factors for which there are fewer than 10 points
keep <- levels(as.factor(x$year_int))[table(x$year_int) > 10]
x.sub.int <- x[x$year_int %in% keep, ]
ggplot(x.sub.int, aes(as.factor(year_int), MaxFRP)) + geom_boxplot() + ylim(0, 100)

# by year of fire
keep <- levels(as.factor(x$FireYear))[table(x$FireYear) > 10]
x.sub.yr <- x[x$FireYear %in% keep, ]
ggplot(x.sub.yr, aes(as.factor(FireYear), MaxFRP)) + geom_boxplot() + ylim(0,100)

# by points
x.test <- x.sub.int %>% group_by(year_int) %>% summarize(avg = mean(MaxFRP))
ggplot(x.test, aes(year_int, avg)) + geom_point()





#### Parked
#mapak <- get_map(location = c(lon = mean(x$longitude), lat = mean(x$latitude)), zoom = 4, maptype = "satellite", scale = 1)
#ggmap(mapak) + geom_point(data = x, aes(longitude, latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)


