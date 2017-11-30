# process_CAN_burn_data.R
#
# This script ingests the attribute table of the Canadian fire perimeters dataset.  The original file of 
# overlapping fire polygons was edited in ArcMap to create a file of non-overlapping polygons.
#
# August 2016
# JWalker


# The input file should consist of the following.
# Column names are unimportant, but the order must be as shown:
#
# ptid - id of each point associated with a discrete (non-overlapping) fire polygon. 
#        Multiple points may be associated with a single polygon due to the polygon
#        burning in different years
# polyid - unique id of each fire polygon
# ac - acreage of each individual poly
# lat - latitude of each point (poly centroid)
# long - longitude of each point 
# date - discovery date of each fire. Used here as initial fire date.
# outdate - date each fire was out
# parentid - id of parent fire (overlapping polygons)
# parent_ac - acreage of parent fire
# name - name of fire
#
# Also necessary is the script of related functions: 
# ak_functions.R
#
# -------------------------------------------------------------------
# Begin program
# -------------------------------------------------------------------

# Remove existing data and values from R environment
rm(list=ls())

# Load libraries
library(plyr)
library(dplyr)
library(lubridate)


# *****
# User input required here!

file.in <- "firePerimeters_1970_2016_dates_for_each_burn_R.csv" #"NFDB_firepolys_thru2015_dates_for_each_burn_ecoregions_20170103_edited_r.csv" #
rdata <- "AK_reburn_files.RData" # "CAN_reburn_files.RData"
path.in <- "D:/projects/Fire_AK_reburn/tables"
path.out <- "D:/projects/Fire_AK_reburn/R"

# ******



# Load functions
  source(file.path(path.out, "ak_functions.R"))

# Get input file
  x <- read.csv(file.path(path.in, file.in), header = T)

  # Rename columns
  colNames <- c("ptid", "polyid", "acres", "lat", "long", "date", "outdate", "parentid", "parentac", "name")
  colnames(x) <- colNames
  x <- x[, colNames]  # get rid of all columns except the specified ones
  x$date <- as.Date(x$date, format = "%m/%d/%Y")
  x$outdate <- as.Date(x$outdate, format = "%m/%d/%Y")

# Extract year, Julian day, and day of year (DOY) for initial fire date
  x$year <- as.numeric(format(x$date, '%Y'))
  x$julian <- julian(x$date)
  x$doy <- yday(x$date)
  
# Filter abnormal start dates: dates past DOY 335 (Dec. 1) are set to NA.
  x$doy[x$doy > 334] <- NA
  
# Extract year, Julian day, and DOY for fire out date
  x$outjulian <- julian(x$outdate)
  x$outdoy <- yday(x$outdate)
  
# Calculate fire duration for each polygon
  x$firelength <- x$outdoy - x$doy

# Get rid of super small acreage polys
  x <- subset(x, acres > 0.1)

# Get rank and time between fires. Uses polyid, date.
  x <- getReburnRanks(x)  # function in ak_functions.R script
  
#  head(x)
#    ptid polyid     acres      lat      long       date    outdate parentid parentac         name year julian doy outjulian outdoy firelength burn_num reburn year_int julian_int size_rank
#  1    0      0  1612.710 56.74181 -157.9385 2006-05-22 2006-06-13      715   1612.7 Meshik River 2006  13290 142     13312    164         22       1      0       NA         NA        11
#  2    1      1 14852.858 57.01803 -154.3084 1997-04-16 1997-05-01      557  14852.9    Moser Bay 1997   9967 106      9982    121         15       1      0       NA         NA        18
#  3    2      2  3135.107 57.54828 -154.0979 1996-10-13 1996-10-22      310   3135.1   Larsen Bay 1996   9782 287      9791    296          9       1      0       NA         NA        35
#  4    3      3  4861.982 57.59670 -152.2218 2015-08-27 2015-12-10      106   4861.9  Twin Creeks 2015  16674 239     16779    344        105       1      0       NA         NA       152
#  5    4      4  3302.031 57.58348 -157.5702 2005-05-29 2005-06-08     1349   3302.0  PILOT POINT 2005  12932 149     12942    159         10       1      0       NA         NA        96
#  6    5      5  1010.771 58.18316 -152.5350 1996-05-09 1996-05-25     1196   1010.8  Kazakof Bay 1996   9625 130      9641    146         16       1      0       NA         NA        60

# Remove all within-season burns: entries tagged as reburns but with year_int = 0.
# Point ID (rather than polyID) gives the specific date
  x.1season <- subset(x, reburn == 1 & year_int == 0)
  x.clean <- x[!(x$ptid %in% x.1season$ptid), ]
  x <- x.clean

# Redo the rank and reburn analysis to account for changes
  x <- getReburnRanks(x)
  
# Calculate top % of all fires (acreages), by year/reburn status
  # Where 1 = in top x%; 0 = not in top x%
  top5 <- ave(x$acres, x$reburn, x$year, FUN = function(y) y > quantile(y, prob = 0.95))
  top10 <- ave(x$acres, x$reburn, x$year, FUN = function(y) y > quantile(y, prob = 0.90))
  x <- cbind(x, top5, top10)  


# =========================
#       Summary tables
# =========================
  
# ------------------------------------------------------------------------------------
# General summary of fire attributes by year and fire status (all, initial, reburns):
# - acres
# - latitude
# - longitude
# - start DOY 
# - season length (start dates only for Canada; no end date data exist) 
# ------------------------------------------------------------------------------------
  
  # All burns
  x.all.sum <- ddply(x, .(year), summarize,
                         n = sum(!is.na(acres)),   # of entries
                         sum_ac = sum(acres, na.rm = T),
                         mean_ac = mean(acres, na.rm = T),
                         med_ac = median(acres, na.rm = T),
                         sd_ac = sd(acres, na.rm = T),
                         se_ac = sd_ac/sqrt(n),
                         mean_lat = mean(lat, na.rm = T),
                         med_lat = median(lat, na.rm = T),
                         sd_lat = sd(lat, na.rm = T),
                         se_lat = sd_lat/sqrt(n),
                         mean_long = mean(long, na.rm = T),
                         med_long = median(long, na.rm = T),
                         sd_long = sd(long, na.rm = T),
                         se_long = sd_long/sqrt(n),
                         mean_start = mean(doy, na.rm = T),
                         med_start = median(doy, na.rm = T),
                         sd_start = sd(doy, na.rm = T),
                         se_start = sd_start/sqrt(n),
                         length_start = max(doy) - min(doy),
                         n_length = sum(!is.na(outdoy)),   # of entries  
                         mean_length = mean(firelength, na.rm = T),
                         med_length = median(firelength, na.rm = T),
                         sd_length = sd(firelength, na.rm = T),
                         se_length = sd_length/sqrt(n_length),
                         n_int = sum(!is.na(year_int)),  # entries with year_int > 0
                         mean_int = mean(year_int, na.rm = T),
                         sd_int = sd(year_int, na.rm = T),
                         se_int = sd_int/sqrt(n_int))
  
  # Summary by year and burn type
  x.burntype.sum <- ddply(x, .(year, reburn), summarize,
                         n = sum(!is.na(acres)),
                         sum_ac = sum(acres, na.rm = T),
                         mean_ac = mean(acres, na.rm = T),
                         med_ac = median(acres, na.rm = T),
                         sd_ac = sd(acres, na.rm = T),
                         se_ac = sd_ac/sqrt(n),
                         mean_lat = mean(lat, na.rm = T),
                         med_lat = median(lat, na.rm = T),
                         sd_lat = sd(lat, na.rm = T),
                         se_lat = sd_lat/sqrt(n),
                         mean_long = mean(long, na.rm = T),
                         med_long = median(long, na.rm = T),
                         sd_long = sd(long, na.rm = T),
                         se_long = sd_long/sqrt(n),
                         mean_start = mean(doy, na.rm = T),
                         med_start = median(doy, na.rm = T),
                         sd_start = sd(doy, na.rm = T),
                         se_start = sd_start/sqrt(n),
                         length_start = max(doy) - min(doy),
                         n_length = sum(!is.na(outdoy)),   # of entries 
                         mean_length = mean(firelength, na.rm = T),
                         med_length = median(firelength, na.rm = T),
                         sd_length = sd(firelength, na.rm = T),
                         se_length = sd_length/sqrt(n_length),
                         n_int = sum(!is.na(year_int)),
                         mean_int = mean(year_int, na.rm = T),
                         sd_int = sd(year_int, na.rm = T),
                         se_int = sd_int/sqrt(n_int))

# Add cumulative acreage sums to each burn type
  x.burntype.sum = ddply(x.burntype.sum, c('reburn'), transform, cumsum_ac = cumsum(sum_ac), total_ac = sum(sum_ac))
  
  
# ------------------------------------------------------
# Reburns and their parent fires
# ------------------------------------------------------

# Areas that have reburned, AND their associated initial burns;
  # i.e., any polyid that is duplicated has been reburned at some point
  x.duplicates <- x %>% group_by(polyid) %>% filter(n() > 1)  #dplyr
  
# > x.duplicates
#  Source: local data frame [7,615 x 23]
#  Groups: polyid [3,543]
  
#      ptid polyid    acres      lat      long       date    outdate parentid parentac          name  year julian   doy outjulian outdoy firelength burn_num reburn year_int julian_int size_rank  top5 top10
#  1     38     33    2.515 60.10923 -150.8311 1994-08-30 1994-10-31     1215   3035.3   WINDY POINT  1994   9007   242      9069    304         62       1      0       NA         NA       103     0     0
#  2     37     33    2.515 60.10923 -150.8311 2005-07-11 2005-11-03      601  25532.7     Fox Creek  2005  12975   192     13090    307        115       2      1       11       3968       447     0     0
#  3     40     34    2.072 60.11062 -150.8373 1994-08-30 1994-10-31     1215   3035.3   WINDY POINT  1994   9007   242      9069    304         62       1      0       NA         NA       105     0     0
#  4     39     34    2.072 60.11062 -150.8373 2005-07-11 2005-11-03      601  25532.7     Fox Creek  2005  12975   192     13090    307        115       2      1       11       3968       452     0     0
#  5     35     35 2698.725 60.11435 -151.1677 1996-06-06 1996-10-11      350  13739.8 Crooked Creek  1996   9653   158      9780    285        127       1      0       NA         NA        37     0     0
#  6     36     35 2698.725 60.11435 -151.1677 2007-06-19 2007-10-30     1205  55437.8 Caribou Hills  2007  13683   170     13816    303        133       2      1       11       4030         3     1     1

# ------------------------------------------------------
# Proportion of original fire that ultimately reburned
# ------------------------------------------------------
  
  # Initial burns are polyids with burn_num == 1.
  # Summarize #acres in reburns (polys with burn_num > 1) by firename =>
  #   total reburned acres associated with parent fire.
  # Using parentid + name means there's no chance of fires with identical names being lumped together.
  
  x.reburn.prop <- ddply(subset(x.duplicates, burn_num > 1), .(burn_num, name, parentid, parentac, year), summarize, 
                    sum_reburn = sum(acres), # of reburn acres
                    n = length(acres), # of fires
                    mean_lat = mean(lat),  # mean latitude of reburn area per firename
                    mean_long = mean(long)) # mean longitude of reburn areas per firename
  
  #  proportion in reburn areas
  x.reburn.prop$prop_reburn <- x.reburn.prop$sum_reburn/x.reburn.prop$parentac
  
#  > head(x.reburn.prop)
#    burn_num          name parentid parentac year sum_reburn n mean_lat mean_long prop_reburn
#  1       2     Fox Creek      601  25532.7 2005   1192.726 7 60.15356 -150.9914  0.04671367
#  2       2 Caribou Hills     1205  55437.8 2007   2704.062 2 60.12311 -151.1411  0.04877650
#  3       2   Funny River     1627 196610.1 2014  16127.750 7 60.36725 -150.6112  0.08202910
#  4       2   Card Street      361   8874.6 2015    627.565 4 60.47223 -150.5732  0.07071474
#  5       2     Can Creek       82   6590.1 2013    203.666 1 61.25084 -155.1903  0.03090484
#  6       2  Timber Creek     1331   9411.1 2016   9038.461 1 61.16141 -159.1766  0.96040431
  
  # group all reburns by year AND by reburn #
  x.reburn.by.year.rank <- ddply(x.reburn.prop, .(year, burn_num), summarize, 
                              mean_prop_reburn = mean(prop_reburn),
                              med_prop_reburn = median(prop_reburn),
                              sum_reburn_ac = sum(sum_reburn),
                              mean_reburn_ac = mean(sum_reburn),
                              med_reburn_ac = median(sum_reburn),
                              mean_orig_ac = mean(parentac),
                              mean_lat = mean(mean_lat),
                              mean_long = mean(mean_long),
                              n = length(prop_reburn),
                              sddev = sd(prop_reburn),
                              se = sddev/sqrt(n))

#  > head(x.reburn.by.year.rank)
#    year burn_num mean_prop_reburn med_prop_reburn sum_reburn_ac mean_reburn_ac med_reburn_ac mean_orig_ac mean_lat mean_long n
#  1 1973       2      0.021558520     0.021558520       513.746        513.746       513.746      23830.3 65.67631 -154.9042 1
#  2 1974       2      0.008147778     0.008147778      5584.288       2792.144      2792.144     178402.9 66.88397 -159.0233 2
#  3 1975       2      0.009822002     0.009822002        71.238         71.238        71.238       7252.9 65.19862 -147.8383 1
#  4 1977       2      0.041684349     0.024045895     42523.076       4724.786      1525.488     135590.6 65.72011 -158.9372 9
#  5 1978       2      0.028700412     0.028700412        64.751         64.751        64.751       2256.1 66.71709 -155.2934 1
#  6 1979       2      0.490816726     0.491998198     23542.535       7847.512      4369.436      81684.0 65.97223 -153.0082 3
#  sddev          se
#  1        NaN         NaN
#  2 0.01089975 0.007707286
#  3        NaN         NaN
#  4 0.04826113 0.016087043
#  5        NaN         NaN
#  6 0.41830533 0.241508692
  
  
  # Turn burn_num (1st, 2nd, etc.) into a factor
  x.reburn.by.year.rank$burn_num <- as.factor(x.reburn.by.year.rank$burn_num)
  

# ----------------------------------------- 
# Acres in each reburn frequency category
# -----------------------------------------
  x.burn.ac <- ddply(x, .(burn_num), summarize, sum_acres = sum(acres))

#  > head(x.burn.ac)
# burn_num    sum_acres
#  1     1 21998167.474
#  2     2  1804906.790
#  3     3    50827.872
#  4     4     1566.422

  
# --------------------------------------------
# Acres of reburn types per year
# --------------------------------------------
  
  x.burn.ac.yr <- ddply(x, .(burn_num, year), summarize, sum_acres = sum(acres))
  
#  > head(x.reburn.ac.yr)
#    burn_num year  sum_acres
#  1       1 1926   4211.798
#  2       1 1930   1200.049
#  3       1 1932   9277.146
#  4       1 1942  16733.308
#  5       1 1943   7471.189
#  6       1 1944 150105.249 



# ---------------------------------
# Top 5 fires/reburns in each year
# ---------------------------------  

  # Get the mean size (acres) of the top 5 fires/reburns in each year
  x.top5size <- ddply(x[which(x$size_rank < 6),], .(year, reburn), summarize,
                      mean_ac = mean(acres),
                      n = length(acres))
  

# --------------------------------------
# Top 5%/10% fires/reburns in each year
# --------------------------------------  

# 5%  
  x.burn.top5 <- ddply(x, .(year, reburn, top5), summarize, 
                         sum_ac = sum(acres),
                         n = length(reburn),
                         mean_ac = mean(acres))

#  > head(x.reburn.top5)
#  year reburn top5 sum_acres   n mean_acres
#  1 1970      0    0  55088.41  20   2754.420
#  2 1970      0    1  30933.03   2  15466.516
#  3 1971      0    0 418693.12 172   2434.262
#  4 1971      0    1 534715.16  10  53471.516
#  5 1972      0    0 345394.37 252   1370.613
#  6 1972      0    1 577704.92  14  41264.637
  
# 10%
  x.burn.top10 <- ddply(x, .(year, reburn, top10), summarize, 
                          sum = sum(acres),
                          n = length(reburn),
                          mean_ac = mean(acres))


# Save data and environment settings  
save.image(file = file.path(path.out, rdata))
  