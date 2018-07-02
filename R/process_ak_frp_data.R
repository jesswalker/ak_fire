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
#            class column is the number of 30-m pixels in each 1-km MODIS pixel
#            (maximum of 1111 30-m pixels)
#
#   files:   mxda1_gee_<yyyy>_plus_fire_info.csv, where <yyyy> = 2002-2016
#
# Output:   ak_frp_data.RData.  Contains files processed through to 
#           statistical results.
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
rdata <- "ak_frp_data.RData"
path.plots <- "D:/projects/ak_fire/output/plots/frp"

# maximum number of landsat pixels in a 1km MODIS pixel
max_landsat_pixels = 1111

source(file.path(path.in, "R", "ak_functions.R"))

# Set vars
#load(file = file.path(path.in, "data", rdata))

# if the data are already saved in an Rdata file, skip this part
#if (!file.exists(file.path(path.r, rdata))){

  # Get the relevant filenames
  filenames.in <- list.files(file.path(path.in, "data/tables"), pattern = "*plus_fire_info_bufferIn600m.csv")
  
  # Read all files to a single file. Suppress warnings b/c of spurious "incomplete last line" error
  suppressWarnings(x.all <- do.call(rbind, lapply(file.path(path.in, "data/tables", filenames.in), read.csv, header=T)))
  
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
  evt_classes <- data.frame(evt_number =  
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
                  "class2791", "class2792", "class2793", "class2794","class31", 
                  "class81", "class82"),

  evt_group = c("Water","Snow", "Developed", "Burned", "Developed", "Developed", "Developed",
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

  # Index is unique to each point but cumbersome in its current form;
  # replace with sequence of sequential numbers.
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

  # Housekeeping
  x$DiscDate <- NULL
  x$newdate <- NULL

  # MODIS MaxFRP is scaled by 10
  x$MaxFRP <- 0.1 * x$MaxFRP

  # Function to count the number of times each point appears: 1 = first fire, 2 = 2nd, etc.
  get_fire_order = function(df) {
    df <- df[order(df$fire_date), ]
    df$burn_num <- ave(df$FireYear, df$ptid, FUN = rank) #Rank the fires associated with a single point
    df$reburn <- 0 
    df[which(as.numeric(df$burn_num) > 1), ]$reburn <- 1 #Reburn: no = 0, yes = 1
    df$year_int <- ave(df$FireYear, factor(df$ptid), FUN=function(t) c(0, diff(t))) #Get years between fires
  return(df)
  }

  # Calculate fire order
  x <- get_fire_order(x)

  # Remove all within-season burns: entries tagged as reburns but with year_int = 0.
  x.1season <- subset(x, reburn == 1 & year_int == 0)
  x <- x[!(x$FID %in% x.1season$FID), ]
  
  # Redo the rank calculate to account for changes
  x <- get_fire_order(x)
  
  # Only keep burns for which the MODIS date is after the burn date;
  # no sense in getting modis data for burns that haven't happened yet.
  x <- x[which(x$modis_date > x$fire_date), ]
  
  # Get the EVT class column with the highest # of pixels
  x$max_evt <- apply(x, 1, function(y) names(y[grepl("class", names(y))])[which.max(y[grepl("class", names(y))])[1]])
  
  # ...get the column value
  x$max_evt_val <- apply(x[grepl("class", names(x))], 1, max, na.rm = TRUE)
  
  # ...calc the proportion of pixels
  x$max_evt_prop <- x$max_evt_val/max_landsat_pixels * 100

  # Remove rows in which max_class == NA; this means none of the classes had a majority
  x <- x[!is.na(x$max_evt), ]

  # Convert to factor
  x$max_evt <- as.factor(x$max_evt)

  # Merge with file of EVT classes to associate a name with each class
  x <- merge(x, evt_classes, by.x = "max_evt", by.y = "evt_number")
  
  # Keep original set of data prior to excluding duplications
  x.all <- x
  
  # Get rid of all individual classes. Keep as separate file s.t. proportions
  # can be analyzed if desired
  x <- x[, -which(names(x) %in% names(x[grepl("class", names(x))]))]

  # Retain only the highest number of burns for a given point,
  # otherwise a given FRP value will be associated with multiple burn states.
  # First ensure file is ordered by ptid, then by burn_num (highest to lowest)
  x <- x[order(x$ptid, -x$burn_num), ]
  
  # Remove duplicate points; i.e., all but the first-occurring point, which has the highest burn#
  x <- x[!duplicated(x$ptid), ]

  # Clean up
  rm(x.1season)
  

# ----------------------------------
# Calculate STATS ----
# ----------------------------------

# Convert to factor
x$burn_num <- as.factor(x$burn_num)

# FRP by burn count (data)
# (why is this flaky and doesn't work sometimes?)
x.frp.all <- x %>%
              group_by(as.factor(burn_num)) %>%
              summarize(avg = mean(MaxFRP),
                    med = median(MaxFRP),
                    n = length(burn_num))

# > x.frp
# # A tibble: 4 x 4
# `as.factor(burn_num)`   avg   med     n
# <fct>                 <dbl> <dbl> <int>
#   1 1                    197.  77.8 59532
# 2 2                      176.  81.4  9807
# 3 3                      176.  75.7   967
# 4 4                      132.  52.2    67

# # this works reliably...just less fun
 # x.frp2 <- ddply(x, .(burn_num), summarize,
 #                avg = mean(MaxFRP),
 #                med = median(MaxFRP))
 
# Round decimals
# means$MaxFRP <- round(means[,2], 1)

# Get evt classes wi
x.maxevt <- summary(x$evt_group)

# Get FRP by veg class. 
# x.frp.class <- ddply(x, .(max_class, burn_num), summarize,
#                avg = mean(MaxFRP),
#                med = median(MaxFRP),
#                n = length(burn_num))

x.maxevt.gp <- x %>% 
                  group_by(evt_group, burn_num) %>%
                  summarize (avg = mean(MaxFRP), 
                            med = median(MaxFRP), 
                            n = length(burn_num))

# Keep only those classes that have at least 2 burn levels
x.maxevt.gp.top <- x.maxevt.gp[!(as.numeric(x.maxevt.gp$evt_group) %in% which(table(x.maxevt.gp$evt_group) < 2)), ]

# Keep only those classes in which at least one level has 30 points
x.maxevt.gp.top2 <- x.maxevt.gp.top[x.maxevt.gp.top$evt_group %in% x.maxevt.gp.top$evt_group[which(x.maxevt.gp.top$n > 10 &
                                                                                                           x.maxevt.gp.top$burn_num == 3)], ]
# Further restrict it to classes where the 3rd fire has > 10 points
x.maxevt.gp.top2 <- x.maxevt.gp.top[x.evt.gp.top$evt_group %in% x.maxevt.gp.top$evt_group[which(x.maxevt.gp.top$n[x.maxevt.gp.top$burn_num == 3] > 10) ]]
x.maxevt.gp.top2 <- k[k$evt_group %in% k$evt_group[which(k$n[k$burn_num == 3] > 10) ]]

# ID classes with higher #s of points
x.maxevt.gp.top <- subset(x.maxevt.gp, n >= 20)

x.maxevt.gp.top <- data.frame(x.maxevt.gp.top)
x.maxevt.gp.top

# > x.maxclass.gp.top
# class_name burn_num       avg    med     n
# 1  Agriculture        1  61.61667  41.55    24
# 2  Agriculture        2  68.20000  68.20     1
# 3       Barren        1 156.94000  98.70    35
# 4       Barren        2  56.60000  56.60     2
# 5   BirchAspen        1 175.08680  74.20  5947
# 6   BirchAspen        2 174.45053  73.10  1508
# 7   BirchAspen        3 199.33527  70.10   241
# 8   BirchAspen        4 205.76897  30.00    29
# 9           BS        1 232.20899  90.70 10338
# 10          BS        2 209.34827  99.50  1243
# 11          BS        3 113.83810  39.90    21
# 12          BS        4  26.80000  26.80     1
# 13      Burned        1  91.93478  47.90    23
# 14      Burned        2  70.94643  39.75    28
# 15      Burned        3  37.79231  17.90    13
# 16  Floodplain        1 144.17485  65.00  2743
# 17  Floodplain        2 150.51377  79.40   530
# 18  Floodplain        3 133.24286  65.20    21
# 19   Grassland        1 238.92504  92.40   611
# 20   Grassland        2 123.14286  68.40    49
# 21   Grassland        3 355.07500 528.40     8
# 22    Peatland        1 112.45367  50.20   667
# 23    Peatland        2 114.57907  57.20    43
# 24   Shrubland        1 196.21391  73.05  4946
# 25   Shrubland        2 139.85748  71.00   936
# 26   Shrubland        3 139.45874  90.50   143
# 27   Shrubland        4  77.36000  79.40     5
# 28        Snow        1  56.46667  48.30     9
# 29        Snow        2 347.00000 171.20    10
# 30       Swamp        1 164.35455  81.50    33
# 31       Swamp        2 132.98000  70.40    15
# 32      Tundra        1 141.20876  64.10  6327
# 33      Tundra        2 144.37394  74.70  1869
# 34      Tundra        3 155.08300  58.40   253
# 35      Tundra        4  37.86000  24.40    10
# 36       Water        1 136.94788  84.70   165
# 37       Water        2  83.43571  58.35    28
# 38     Wetland        1 182.97015  75.60 14421
# 39     Wetland        2 187.67659  75.40  1803
# 40     Wetland        3 146.00741  73.90   135
# 41     Wetland        4  97.06190  78.30    21
# 42          WS        1 237.44518  85.20 13227
# 43          WS        2 207.15316  96.30  1742
# 44          WS        3 265.42879 126.40   132
# 45          WS        4  44.60000  44.60     1


# Calculate 1-way ANOVA; burn_num
x.anova <- aov(formula = MaxFRP ~ burn_num, data = x)
summary(x.anova)
TukeyHSD(x.anova, "burn_num")

# Calculate 1-way ANOVA; evt class
x.anova <- aov(formula = MaxFRP ~ evt_group, data = x)
summary(x.anova)
TukeyHSD(x.anova, "evt_group")

# Calculate 2-way ANOVA; evt group and burn num
x.anova <- aov(formula = MaxFRP ~ evt_group * burn_num, data = x)
summary(x.anova)
TukeyHSD(x.anova, which = "burn_num")

# Calculate 1-way ANOVA based on #years between fire, FRP, by evt_group
x.anova <- aov(formula = MaxFRP ~ as.factor(year_int) * evt_group, data = subset(x, reburn > 0))
summary(x.anova)
# > summary(x.anova)
# Df    Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(year_int)              62  56216262  906714  10.557  < 2e-16 ***
#   evt_group                        14   3454320  246737   2.873 0.000241 ***
#   as.factor(year_int):evt_group   329  50451828  153349   1.785 3.71e-16 ***
#  Residuals                     10435 896259713   85890
TukeyHSD(x.anova, which = "evt_group")



# Save data and environment settings   
 print(paste0("R data file saved to ", file.path(path.r, rdata)))
 save.image(file = file.path(path.r, rdata))

