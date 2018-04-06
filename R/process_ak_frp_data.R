
year <- '2009'

# Load libraries
library(plyr) # file manipulation
library(dplyr) # moving average, many others
library(ggmap)

# ^^ plyr and dplyr have to be loaded in this order, otherwise "mutate" functions
# will not perform correctly. 
library(lubridate) # dates
library(reshape2)  # melt

library(dtplyr) #supersedes data.table ^^
library(zoo) # for calculating moving average

# set paths
filename = paste0('mxd14a1_gee_', year, '_plus_fires_r.csv')
path.in <- "D:/projects/ak_fire"
df <- read.csv(file.path(path.in, "data", filename), header = T)

# get rid of all columns except the specified ones
colNames <- c("FID", "FID_mxd14a", "index", "class11", "class12", "class21", "class2197", "class22", "class23",
              "class2600", "class2601", "class2602", "class2603", "class2604", "class2605", "class2606", "class2607", "class2608",
              "class2609", "class2610", "class2611", "class2612", "class2631", "class2633", "class2634", "class2635", "class2636",
              "class2740", "class2745", "class2751", "class2753", "class2757", "class2763", "class2773", "class2776", "class2777",
              "class2782", "class2786", "class2793", "class31", "MaxFRP", "latitude", "longitude", "newdate", "year", "FireYear", "FIREID",
              "DiscDate", "Region")
x <- df[, colNames]  

# get rid of columns with all NAs
x <- Filter(function(y) !all(is.na(y)), x)

# format date columns
x$modis_date <- as.Date(x$newdate, format = "%m/%d/%Y")
x$fire_date <- as.Date(x$DiscDate, format = "%m/%d/%Y")

# remove old columns
x$DiscDate <- NULL
x$newdate <- NULL

# Count the number of rows in each factor level for fire rank: 1 = first fire, 2 = 2nd, etc.
x <- x[order(x$fire_date), ]
x$burn_num <- ave(x$FireYear, x$index, FUN = rank)
x$reburn <- 0
x[which(as.numeric(x$burn_num) > 1), ]$reburn <- 1
x$year_int <- ave(x$FireYear, factor(x$index), FUN=function(t) c(NA, diff(t)))
x <- x[order(x$FID_mxd14a),]

# Remove all within-season burns: entries tagged as reburns but with year_int = 0.
# Index is unique to each point
x.1season <- subset(x, reburn == 1 & year_int == 0)
x.clean <- x[!(x$index %in% x.1season$index), ]
x <- x.clean

# Redo the rank and reburn analysis to account for changes
x <- x[order(x$fire_date), ]
x$burn_num <- ave(x$FireYear, x$index, FUN = rank)
x$reburn <- 0
x[which(as.numeric(x$burn_num) > 1), ]$reburn <- 1
x$year_int <- ave(x$FireYear, factor(x$index), FUN=function(t) c(NA, diff(t)))
x <- x[order(x$FID_mxd14a),]

# We want a file that has only the last (highest) number of burns for a given
# pixel. Otherwise boxplots for burn_number count repetitiously

# Make sure file is ordered by ptid, then by burn_num (highest to lowest)
xx <- x[order(x$index, -x$burn_num), ]

# Remove duplicates (all but the first--highest--one)
x.duped <- xx[!duplicated(xx$index), ]

x <- x.duped

# drop factors for which there are fewer than 10 points
keep <- levels(as.factor(x$year_int))[table(x$year_int) > 10]
x.sub <- x[x$year_int %in% keep, ]


# PLOTS

# by burn number
ggplot(x, aes(as.factor(burn_num), MaxFRP)) + geom_boxplot() + ylim(0, 5000)

# by years since prior burn
ggplot(x.sub, aes(as.factor(year_int), MaxFRP)) + geom_boxplot() + ylim(0, 5000)
ggplot(x.sub, aes(year_int, MaxFRP)) + geom_point() + ylim(0, 5000)

# by year of fire
ggplot(x, aes(as.factor(FireYear), MaxFRP)) + geom_boxplot() + ylim(0,5000)




####
mapak <- get_map(location = c(lon = mean(x$longitude), lat = mean(x$latitude)), zoom = 4, maptype = "satellite", scale = 1)
ggmap(mapak) + geom_point(data = x, aes(longitude, latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)


