########################################################################### #
#
# process_ak_frp_data.R 
#
# Objective:  Process MxD14A1 FRP data in order to plot and analyze data.
#
# Input:     Files of MxD14A1 data originally produced in GEE
#            for fire complexes in Alaska.  FRP files must be run through 
#            format_gee_file_for_import_into_arc.R, then
#            intersected in Arc with the vector file of fire history 
#            (intersect_frp_data_with_fire_perimeters.py) to produce
#            points that are tagged with FRP data, EVT data (in the encompassing
#            MODIS 1-km pixel), and fire history data. The value in each EVT
#            class column is the number of 30-m pixels in each 1-km MODIS pixel.
#
#   - mxda1_gee_<yyyy>_plus_fire_info.csv, where <yyyy> = 2002-2016
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
path.in <- "D:/projects/ak_fire"
path.r <- "D:/projects/ak_fire/data"
rdata <- "process_ak_frp_data.RData"
path.plots <- "D:/projects/ak_fire/output/plots/frp"

source(file.path(path.in, "R", "ak_functions.R"))

filenames.in <- list.files(file.path(path.in, "data/tables"), pattern = "*plus_fire_info_bufferIn600m.csv")

# Read in all files to a single file
x.all <- do.call(rbind, lapply(file.path(path.in, "data/tables", filenames.in), read.csv, header=T))

# Column headings were screwed up...

# Switch columns from "classlas_" or "classlass" to "class"
#names.sub <- names(x.all)[which((substring(names(x.all), 1, 8) == "classlas"))]
#names(x.all)[which((substring(names(x.all), 1, 8) == "classlas"))] <- paste0("class", substring(names.sub, 2))


# Retain only specified columns
colNames <- c("index", "FID_firePe",
"class11", "class12", "class21", "class2197", "class22", "class23", "class24",     
"class2600", "class2601", "class2602", "class2603", "class2604", "class2605", "class2606", "class2607", "class2608", "class2609", 
"class2610", "class2611", "class2612",
"class2631", "class2633", "class2634", "class2635", "class2636", "class2638", "class2639", 
"class2640", "class2642", "class2643", "class2644", "class2645", "class2646", "class2648", "class2649", 
"class2651", "class2652", 
"class2671", "class2677", "class2678", "class2679", 
"class2682", "class2683", "class2684", "class2685", "class2686", "class2687", "class2688", "class2689", 
"class2690", "class2691", "class2692", "class2699", 
"class2709", "class2718", "class2719", 
"class2720", 
"class2730", 
"class2740", "class2741", "class2742", "class2743", "class2744", "class2745", "class2746", "class2747", 
"class2751", "class2753", "class2756", "class2757", "class2758",     
"class2761", "class2762", "class2763", "class2764", 
"class2771", "class2772", "class2773", "class2774", "class2776", "class2777", 
"class2781", "class2782","class2783",  "class2784", "class2785", "class2786",     
"class2791", "class2792","class2793", "class2794", 
"class31", 
"class81", "class82",
"MaxFRP", "latitude", "longitude", 
"newdate", "FireYear", "FIREID", "DiscDate")

x <- x.all[, colNames]  

# Set up EVT classes and consolidated groups
evt_classes <- data.frame(class =  
                c("class11","class12", "class21", "class2197","class22","class23", "class24",
                  "class2600","class2601","class2602","class2603", "class2604","class2605","class2606","class2607","class2608","class2609",
                  "class2610","class2611","class2612",
                  "class2631","class2633","class2634","class2635","class2636","class2638", "class2639",
                  "class2640", "class2642", "class2643", "class2644", "class2645", "class2646", "class2648", "class2649",
                  "class2651", "class2652",
                  "class2671","class2677","class2678", "class2679",
                  "class2682", "class2683", "class2684", "class2685","class2686", "class2687", "class2688", "class2689",
                  "class2690", "class2691", "class2692", "class2699",
                  "class2709", "class2718", "class2719", 
                  "class2720", 
                  "class2730",
                  "class2740","class2741", "class2742","class2743","class2744","class2745","class2746", "class2747",
                  "class2751","class2753","class2756", "class2757", "class2758",
                  "class2761", "class2762", "class2763","class2764",
                  "class2771", "class2772", "class2773", "class2774", "class2776","class2777",
                  "class2781", "class2782","class2783", "class2784", "class2785", "class2786", 
                  "class2791", "class2792", "class2793", "class2794","class31", "class81", "class82"),

class_name = c("Water","Snow", "Developed", "Burned", "Developed", "Developed", "Developed",
                   "WS", "WS", "BS", "WS", "BS", "BirchAspen", "Aspen", 
                   "BalsPopAsp", 
                   "Shrubland", "Shrubland", "Shrubland", "Grassland", "Grassland","Shrubland", 
                   "Grassland", 
                   "Shrubland", "Shrubland", "Shrubland", "Shrubland", "Shrubland", "Shrubland", 
                   "BirchAspen", "Shrubland", "Spruce", "Grassland", 
                   "Hemlock", "Hemlock", "Hemlock", 
                    "Grassland", "Shrubland", "Grassland",
                   "WS", "WS", "WS", "Shrubland", 
                   "Tundra", "Tundra", "Tundra", "Tundra", "Tundra",
                   "Shrubland", "Shrubland", "Shrubland", 
                   "Tundra", "Tundra", 
                   "Grassland", "Grassland", 
                   "Shrubland", "Shrubland", "Shrubland", "Peatland", "Marsh", "Tidal",
                   "Tidal", "Wetland", "Wetland", "Wetland", "Wetland", "Grassland", "Wetland",
                   "BS", "Shrubland", "Shrubland", "Shrubland", "Floodplain", "Floodplain", "Floodplain",
                   "Floodplain", "Peatland", "Peatland","Peatland", "Peatland", "Shrubland", "Swamp",
                   "Tundra", "Tundra","Tundra","Tundra","Tundra","Tundra","Barren", "Barren", "Barren",
                   "Barren", "Barren", "Agriculture", "Agriculture"))

# index is unique to each point but cumbersome in its current form;
# Replace with sequence of sequential numbers.
levels(x$index) <- c(seq(1:length(levels(x$index))))

# Rename index
colnames(x)[which(colnames(x) == "index")] <- "ptid"

# Set FID as unique id for each row
x$FID <- 1:nrow(x)

# Reorder s.t. FID is first
x <- x[, c(ncol(x), 1:(ncol(x)-1))]

# Remove NA-only columns
x <- Filter(function(y) !all(is.na(y)), x)

# Remove rows in which MaxFRP == NA
x <- x[!is.na(x$MaxFRP), ]

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
    df$burn_num <- ave(df$FireYear, df$ptid, FUN = rank)
    df$reburn <- 0
    df[which(as.numeric(df$burn_num) > 1), ]$reburn <- 1
    df$year_int <- ave(df$FireYear, factor(df$ptid), FUN=function(t) c(0, diff(t))) #changed from NA
 #   df <- df[order(df$FID_mxd14a), ]
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
  
# Only keep burns for which the MODIS date is after the burn date;
# No sense in getting modis data for burns that haven't happened yet.
x <- x[which(x$modis_date > x$fire_date), ]
  
# Retain only the last (highest) number of burns for a given point,
# otherwise a given FRP value can be associated with multiple burn states.
# Ensure file is ordered by ptid, then by burn_num (highest to lowest)
x <- x[order(x$ptid, -x$burn_num), ]

# Remove duplicates; i.e., all but the first point, which has the highest burn#
x <- x[!duplicated(x$ptid), ]

# Retrieve the column with the highest # of pixels.
# Look only at "class" columns
x$max_class <- apply(x, 1, function(y) names(y[grepl("class", names(y))])[which.max(y[grepl("class", names(y))])[1]])

# Remove rows in which max_class == NA
x <- x[!is.na(x$max_class), ]

# Make sure it's a factor
x$max_class <- as.factor(x$max_class)

# Housekeeping
rm(x.1season)

 
# ----------------------------------
# STATS ----
# ----------------------------------

# FRP by burn count (data)
x$burn_num <- as.factor(x$burn_num)

# why is this flaky and doesn't work sometimes
x.frp <- x %>%
          group_by(as.factor(burn_num)) %>%
          summarize(avg = mean(MaxFRP),
                    med = median(MaxFRP))

# > x.frp
# # A tibble: 4 x 3
# `as.factor(burn_num)`   avg   med
# <fct>                 <dbl> <dbl>
#   1 1                    197.  77.8
# 2 2                      176.  81.4
# 3 3                      176.  75.7
# 4 4                      132.  52.2


# # this works reliably...just less fun
 # x.frp2 <- ddply(x, .(burn_num), summarize,
 #                avg = mean(MaxFRP),
 #                med = median(MaxFRP))
 
# Round decimals
means$MaxFRP <- round(means[,2], 1)

# Get highest classes
x.maxclass <- summary(x$max_class)

# Get FRP by veg class
# x.frp.class <- ddply(x, .(max_class, burn_num), summarize,
#                avg = mean(MaxFRP),
#                med = median(MaxFRP),
#                n = length(burn_num))

x.maxclass.gp <- x %>% 
                  group_by(max_class, burn_num) %>%
                  summarize (avg = mean(MaxFRP), 
                            med=median(MaxFRP), 
                            n=length(burn_num))

# Keep only those classes that have at least 2 burn levels
x.maxclass.gp.top <- x.maxclass.gp[!(as.numeric(x.maxclass.gp$max_class) %in% which(table(x.maxclass.gp$max_class) < 2)), ]

# Keep only those classes in which at least one level has 30 points
x.maxclass.gp.top2 <- x.maxclass.gp.top[x.maxclass.gp.top$max_class %in% x.maxclass.gp.top$max_class[which(x.maxclass.gp.top$n > 10 &
                                                                                                           x.maxclass.gp.top$burn_num == 3)], ]


# Further restrict it to classes where the 3rd fire has > 10 points
x.maxclass.gp.top2 <- x.maxclass.gp.top[x.maxclass.gp.top$max_class %in% x.maxclass.gp.top$max_class[which(x.maxclass.gp.top$n[x.maxclass.gp.top$burn_num == 3] > 10) ]]
x.maxclass.gp.top2 <- k[k$max_class %in% k$max_class[which(k$n[k$burn_num == 3] > 10) ]]

# ID classes with higher #s of points
x.maxclass.gp.top <- subset(x.maxclass.gp, n >= 30)
            
# Assign class name
x.maxclass.gp.top <- merge(x.frp.class.sub, evt_classes, by.x = "max_class", by.y = "class")

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

# Calculate ANOVA

x.anova <- aov(formula = MaxFRP ~ burn_num, data = x)
summary(x.anova)
TukeyHSD(x.anova, "burn_num")


# Save data and environment settings   
 print(paste0("R data file saved to ", file.path(path.r, rdata)))
 save.image(file = file.path(path.r, rdata))

