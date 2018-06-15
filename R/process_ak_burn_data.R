########################################################################### #
#
# process_burn_data.R 
#
# Objective: This script ingests the attribute table of the Alaskan fire perimeters dataset. 
# The original file of overlapping fire polygons was manipulated in ArcMap to create a 
# file of discrete, non-overlapping polygons that contain information about any successive
# burns. In addition, the respective amounts of ecoregion types 1 and 2 (described below)
# were calculated for each pixel in Arc using the Tabulate Area command and returned as m^2 values.
#
#
# Input:
#
#   - firePerimeters_1940_2016_burn_data_plus_ecoregions_w_perimeter_R.csv
#
# Column names are unimportant, but the column order must be as shown:
#
# ptid - id of each point associated with a discrete (non-overlapping) fire polygon. 
#        Multiple points may be associated with a single polygon due to the polygon
#        burning in different years
# polyid - unique id of each fire polygon
# ac - acreage of each individual poly
# perim - perimeter (m) of each individual poly
# lat - latitude of each point (poly centroid)
# lon - longitude of each point 
# date - discovery date of each fire. Used here as initial fire date.
# outdate - date each fire was out
# parentid - id of parent fire. Fires are fragmented into individual polygons by overlapping with
#            another polygon; all "pieces" of a given fire area retain the original fire ID.
# parent_ac - acreage of parent fire (ac)
# name - name of fire                          _
# tundra - amount of pixel (m^2) in tundra      |
# maritime -                                    |  Ecoregions - level 1 
# boreal -                                     _|
# bering_taiga                                  |
# aleutian_meadows                              |
# alaska_range                                  |
# arctic_tundra                                 |  
# coast_rain                                    |  Ecoregions - level 2
# pacific_mts                                   |
# intermontane_boreal                           |
# coast_mts                                     |
# bering_tundra                                _|
#
#   - ak_functions.R, file of helper functions:
# 
#  Output:
#   - Multiple summary tables of fire parameters
#   - AK_reburn_data.RData, which is necessary for plotting purposes
#
#
# August 2016 JWalker, modified extensively since 
#
########################################################################### #

# Set environment ---- 

# Remove existing data and libraries from R environment
rm(list=ls())

# Load libraries
library(plyr) # file manipulation
library(dplyr) # moving average, many others

# ^^ plyr and dplyr have to be loaded in this order, otherwise "mutate" functions
# will not perform correctly.  Note to self: get rid of all plyr commands
# if possible, since this fs up some datasets otherwise. 

library(lubridate) # dates
library(reshape2)  # melt
library(data.table) # for padding series with dates; calculating proportion
library(dtplyr) #supersedes data.table ^^
library(zoo) # for calculating moving average

#************************************ 
# User input ----

file.in <- "firePerimeters_1940_2016_burn_data_plus_ecoregions_w_perimeter_R.csv"
rdata <- "AK_reburn_data.RData"   
path.in <- "D:/projects/ak_fire"
#************************************ 


# --------------------------------------------------------------------------------------- #
                                #### PROCESSING  ####
# --------------------------------------------------------------------------------------- #

# load previously saved data file
  #load(file.path(path.in, "R", rdata))

# Square meter to acres conversion: m^2 * 0.000247105 ac/m^2
  sqm2ac <- 0.000247105

# Load functions
  source(file.path(path.in, "R", "ak_functions.R"))

# Get input file
  x <- read.csv(file.path(path.in, "data", file.in), header = T)

# Rename columns
  colNames <- c("ptid", "polyid", "acres", "perim", "lat", "lon", "date", "outdate", "parentid", "parentac", 
                "name", "tundra", "maritime", "boreal", "bering_taiga", "aleutian_meadows", "ak_range", "arctic_tun", 
                "coast_rain", "pacific_mts", "intermontane_boreal", "coast_mts", "bering_tun")
  colnames(x) <- colNames
#  colNames2 <- c("ptid", "polyid", "acres", "perim", "lat", "lon", "date", "outdate", "parentid", "parentac", 
#                "name", "tundra", "maritime", "boreal")
  x <- x[, colNames]  # get rid of all columns except the specified ones
  
  x$date <- as.Date(x$date, format = "%m/%d/%Y")
  x$outdate <- as.Date(x$outdate, format = "%m/%d/%Y")

# Extract year and day of year (DOY) for initial fire date
  x$year <- as.numeric(format(x$date, '%Y'))
  x$doy <- yday(x$date)
  
# Filter abnormal start dates: start dates past DOY 335 (Dec. 1) are set to NA.
# Except Pastoliak River, which did actually start on 12/27/00 per 
# https://www.fws.gov/fire/program_statistics/2000/2000_Activity_Report.pdf   
  x$doy[x$doy > 334 & x$name != "Pastoliak River"] <- NA
  
# Extract year and DOY for fire out date
  x$outyear <- as.numeric(format(x$outdate, '%Y'))
  x$outdoy <- yday(x$outdate)
  
# Outdates on the last day of the year are mighty suspicious  
  x$outdoy[x$outdoy > 360] <- NA
  
# Calculate fire duration for each polygon
  x$firelength <- x$outdoy - x$doy
  
# Make sure fires that span years (looking at you, Pastoliak) do not yield negative lengths
# Replacements bomb out if NAs are returned; select only non-NAs
  x$firelength[x$year < x$outyear & !is.na(x$firelength)] <- x$firelength[which(x$year < x$outyear)] + 365

# Get rid of super small acreage polys
  x <- subset(x, acres > 0.1)  # this could be higher

# Get rank and time between fires. Uses polyid, date.
  x <- getReburnRanks(x)  # function in ak_functions.R script
  
# > head(x)
#    ptid polyid     acres      lat      lon       date    outdate parentid parentac         name    tundra maritime boreal
#  1    0      0  1612.710 56.74181 -157.9385 2006-05-22 2006-06-13      715   1612.7 Meshik River        0  6525000      0
#  2    1      1 14852.858 57.01803 -154.3084 1997-04-16 1997-05-01      557  14852.9    Moser Bay        0 60078600      0
#  3    2      2  3135.107 57.54828 -154.0979 1996-10-13 1996-10-22      310   3135.1   Larsen Bay        0 12699000      0
#  4    3      3  4861.982 57.59670 -152.2218 2015-08-27 2015-12-10      106   4861.9  Twin Creeks        0 19661400      0
#  5    4      4  3302.031 57.58348 -157.5702 2005-05-29 2005-06-08     1349   3302.0  PILOT POINT 13106700        0      0
#  6    5      5  1010.771 58.18316 -152.5350 1996-05-09 1996-05-25     1196   1010.8  Kazakof Bay        0  4077900      0

  #  year doy outdoy firelength burn_num reburn year_int size_rank
#  1 2006 142    164         22        1      0       NA        11
#  2 1997 106    121         15        1      0       NA        18
#  3 1996 287    296          9        1      0       NA        35
#  4 2015 239    344        105        1      0       NA       152
#  5 2005 149    159         10        1      0       NA        96
#  6 1996 130    146         16        1      0       NA        60

# Remove all within-season burns: entries tagged as reburns but with year_int = 0.
# Point ID (rather than polyID) gives the specific date
  x.1season <- subset(x, reburn == 1 & year_int == 0)
  x.clean <- x[!(x$ptid %in% x.1season$ptid), ]
  x <- x.clean

# Redo the rank and reburn analysis to account for changes
  x <- getReburnRanks(x)
  
# Calculate the largest ecoregion type in each fire polygon
  x$ecoreg1 <- names(x[c("tundra", "maritime", "boreal")])[max.col(x[c("tundra", "maritime", "boreal")])]
  x$ecoreg2 <- c("bering_taiga", "aleutian_meadows", "ak_range", "arctic_tun", "coast_rain", "pacific_mts", "intermontane",
                 "coast_mts", "bering_tun")[max.col(x[c("bering_taiga", "aleutian_meadows", "ak_range", "arctic_tun", 
                                                        "coast_rain", "pacific_mts", "intermontane_boreal", "coast_mts", 
                                                        "bering_tun")])]
  
 
# --------------------------------------------------------------------------------------- #
                                 #### SUMMARY TABLES  ####
# --------------------------------------------------------------------------------------- #
  
# --------------------------------------------------------- #
# Fire attributes by reburn and # of times burned ----
# --------------------------------------------------------- #
  
# Summary by reburn status over entire time period
  x.ac <- ddply(x, .(reburn, burn_num), summarize, 
                    bor_ac = sum(boreal * sqm2ac),
                    mar_ac = sum(maritime * sqm2ac),
                    tun_ac = sum(tundra * sqm2ac),
                    ber_ac = sum(bering_taiga * sqm2ac),
                    ale_ac = sum(aleutian_meadows * sqm2ac),
                    akr_ac = sum(ak_range * sqm2ac),
                    arc_ac = sum(arctic_tun * sqm2ac),
                    cor_ac = sum(coast_rain * sqm2ac),
                    pac_ac = sum(pacific_mts * sqm2ac),
                    int_ac = sum(intermontane_boreal * sqm2ac),
                    com_ac = sum(coast_mts * sqm2ac),
                    bet_ac = sum(bering_tun * sqm2ac))

#  > head(x.ac)
#    reburn burn_num      bor_ac   mar_ac     tun_ac       ber_ac    ale_ac      akr_ac    arc_ac   cor_ac    pac_ac      int_ac
#  1      0        1 50251644.96 84027.31 6353839.33 3919858.9610 18200.543 1636515.948 789379.49 65826.77 135337.06 48474882.37
#  2      1        2  7689594.07 13224.91  412896.07  146223.0494   866.449   73422.655  33138.34 12358.46  18793.45  7597377.97
#  3      1        3   713183.36     0.00   25385.66     642.9425     0.000    4491.479      0.00     0.00      0.00   708691.88
#  4      1        4    70829.98     0.00   11955.48       0.0000     0.000       0.000      0.00     0.00      0.00    70829.98
#  5      1        5    19680.58     0.00       0.00       0.0000     0.000       0.000      0.00     0.00      0.00    19680.58
#  6      1        6    11709.52     0.00       0.00       0.0000     0.000       0.000      0.00     0.00      0.00    11709.52
#  com_ac     bet_ac
#  1 4909.581 1644600.88
#  2    0.000  233534.69
#  3    0.000   24742.72
#  4    0.000   11955.48
#  5    0.000       0.00
#  6    0.000       0.00


# --------------------------------------------------------- #
#  Num fires per year, no ecoregion split ----
# --------------------------------------------------------- #
  
  x.nfires.yr <- ddply(x, .(year), summarize,
                  n_fires = length(unique(parentid)))
 
  # ----- Calculate moving average --------
  # Pad the time series w/ NAs to get the full range of dates
  x.ts.nfires <- padTimeSeries(x.nfires.yr, 1940, 2016)
  
  # Order by year 
  x.ts.nfires <- x.ts.nfires[order(x.ts.nfires$year), ]
  
  # Convert NAs to 0 so that rollapply correctly calculates mean;
  # otherwise NAs are tossed out and the average is based on
  # a constrained time period
  x.ts.nfires[is.na(x.ts.nfires)] <- 0

  # Calculate moving average for all fires (no ecoregion split)
  x.nfires.moveavg <-
    x.ts.nfires %>% 
    mutate(n_10yrMove = rollapply(n_fires, 10, mean, na.rm = TRUE, align = 'right', fill = NA))
  
  
# --------------------------------------------------------- # 
# Acres per year, by ecoregions ----
# --------------------------------------------------------- #

  x.all.sum <- ddply(x, .(year), summarize,
                     bor_ac = sum(boreal * sqm2ac),    
                     mar_ac = sum(maritime * sqm2ac),
                     tun_ac = sum(tundra * sqm2ac),
                     ber_ac = sum(bering_taiga * sqm2ac),
                     ale_ac = sum(aleutian_meadows * sqm2ac),
                     akr_ac = sum(ak_range * sqm2ac),
                     arc_ac = sum(arctic_tun * sqm2ac),
                     cor_ac = sum(coast_rain * sqm2ac),
                     pac_ac = sum(pacific_mts * sqm2ac),
                     int_ac = sum(intermontane_boreal * sqm2ac),
                     com_ac = sum(coast_mts * sqm2ac),
                     bet_ac = sum(bering_tun * sqm2ac))
  
#  > head(x.all.sum)
#    year    bor_ac   mar_ac  tun_ac  ber_ac   ale_ac     akr_ac arc_ac cor_ac    pac_ac    int_ac com_ac bet_ac
#  1 1940      0.00    0.000 12865.3 12865.3    0.000      0.000      0      0     0.000      0.00      0      0
#  2 1942  27771.51    0.000     0.0     0.0    0.000  27771.513      0      0     0.000      0.00      0      0
#  3 1943 237576.48 5999.759     0.0     0.0 5999.759   5292.544      0      0     0.000 232283.94      0      0
#  4 1944  96149.15    0.000     0.0     0.0    0.000      0.000      0      0     0.000  96149.15      0      0
#  5 1946 386953.31    0.000     0.0     0.0    0.000   4005.547      0      0  4800.163 378147.60      0      0
#  6 1947 343881.50    0.000     0.0     0.0    0.000 318095.746      0      0 18780.993   7004.76      0      0
  
# Put in long format for plotting purposes
  x.ac.sum.long.eco1 <- melt(x.all.sum[, 1:4], id.vars = .(year), variable.name = 'eco1', value.name = 'eco1_ac')
  x.ac.sum.long.eco2 <- melt(x.all.sum[, c(1, 5:ncol(x.all.sum))], id.vars = .(year), variable.name = 'eco2', value.name = 'eco2_ac')
 
# Calculate cumulative sums 
  x.sum.eco1 <- ddply(x.ac.sum.long.eco1, c('eco1'), transform, cumsum_ac = cumsum(eco1_ac), total_ac = sum(eco1_ac))
  

# --------------------------------------------------------------------------- #
# Acres by year/reburn/ecoregion, with moving average, cumulative sum ----
# --------------------------------------------------------------------------- #
  
# Need summary by year/reburn first
  x.burntype.prep <- ddply(x, .(year, reburn), summarize,
                         bor_ac = sum(boreal * sqm2ac),
                         mar_ac = sum(maritime * sqm2ac),
                         tun_ac = sum(tundra * sqm2ac))

# Subset since otherwise zoo squawks about duplicate dates in subsequent steps
  x.burntype.0 <- subset(x.burntype.prep, reburn == 0)
  x.burntype.1 <- subset(x.burntype.prep, reburn == 1)

# To calculate correct moving average w zoo::rollapply, pad missing years with NAs  
  x.ts.0 <- padTimeSeries(x.burntype.0, 1940, 2016)
  x.ts.1 <- padTimeSeries(x.burntype.1, 1940, 2016)
  
# Make sure reburns are coded correctly
  x.ts.0$reburn <- 0
  x.ts.1$reburn <- 1
  
# Merge the burn/reburn dfs  
  x.ts.all <- rbind(x.ts.0, x.ts.1)
  
# Housekeeping: order and factors
  x.ts.all <- x.ts.all[order(x.ts.all$year),]
  x.ts.all$reburn <- as.factor(x.ts.all$reburn)
  
# Remove NAs to make rollapply correctly calculates mean
  x.ts.all[is.na(x.ts.all)] <- 0

# Add all acreage
  x.ts.all$all_ac <- x.ts.all$bor_ac + x.ts.all$mar_ac + x.ts.all$tun_ac
   
# Calculate moving average for each reburn status in each ecoregion
    x.burntype.sum <-
    x.ts.all %>% 
    group_by(reburn) %>% 
    mutate(bor_10yrMove = rollapply(bor_ac, 10, mean, na.rm = TRUE, align = 'right', fill = NA),
           mar_10yrMove = rollapply(mar_ac, 10, mean, na.rm = TRUE, align = 'right', fill = NA),
           tun_10yrMove = rollapply(tun_ac, 10, mean, na.rm = TRUE, align = 'right', fill = NA),
           all_10yrMove = rollapply(all_ac, 10, mean, na.rm = TRUE, align = 'right', fill = NA))

# Make sure reburn is a factor    
  x.burntype.sum$reburn <- as.factor(x.burntype.sum$reburn)
  

#> head(x.burntype.sum)
#  Source: local data frame [15 x 10]
#  Groups: reburn [2]
#  
#      year reburn    bor_ac   mar_ac  tun_ac    all_ac bor_10yrMove mar_10yrMove tun_10yrMove all_10yrMove
#  <dbl> <fctr>     <dbl>    <dbl>   <dbl>     <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#  1   1940      0      0.00    0.000 12865.3  12865.30           NA           NA           NA           NA
#  2   1940      1      0.00    0.000     0.0      0.00           NA           NA           NA           NA
#  3   1941      0      0.00    0.000     0.0      0.00           NA           NA           NA           NA
#  4   1941      1      0.00    0.000     0.0      0.00           NA           NA           NA           NA
#  5   1942      0  27771.51    0.000     0.0  27771.51           NA           NA           NA           NA
#  6   1942      1      0.00    0.000     0.0      0.00           NA           NA           NA           NA
  
  
# Get the proportion of initial/reburn for each class  
  
  x.burntype.sum$ecoreg1 <- c("boreal", "maritime", "tundra")[max.col(x.burntype.sum[c("bor_ac", "mar_ac", "tun_ac")])]
  x.burntype.sum$ecoreg1 <- as.factor(x.burntype.sum$ecoreg1)
  x.burntype.sum.split <- x.burntype.sum[, c("year", "reburn", "all_ac", "all_10yrMove", 'ecoreg1')]  
  x.burntype.sum.split <- data.frame(x.burntype.sum.split)
  setDT(x.burntype.sum.split)[, prop:= all_ac/sum(all_ac)*100, by=.(year, ecoreg1)]

#  > x.burntype.sum.split
#     year reburn     all_ac all_10yrMove      prop
#  1: 1940      0   12865.30           NA 100.00000
#  2: 1940      1       0.00           NA   0.00000
#  3: 1941      0       0.00           NA       NaN
#  4: 1941      1       0.00           NA       NaN
#  5: 1942      0   27771.51           NA 100.00000
#  ---                                              
#  150: 2014      1   41676.95     313809.6  18.41735
#  151: 2015      0 3932379.33     915623.2  76.88894
#  152: 2015      1 1181983.40     305741.4  23.11106
#  153: 2016      0  339269.93     924742.1  71.90909
#  154: 2016      1  132534.00     318331.6  28.09091

  
# Get decadal breakdown
  x.burntype.sum.split.decade <- setDecade(x.burntype.sum.split)
  x.burntype.decade <- ddply(x.burntype.sum.split.decade, .(reburn, decade), summarize, 
                           sum_ac = sum(all_ac, na.rm = T))
  x.burntype.decade.prop <- ddply(x.burntype.decade, .(decade), transform, prop = sum_ac/sum(sum_ac))
  
  x.burntype.decade.prop$prop <- format(round(x.burntype.decade.prop$prop, 4))
                           
#   > x.burntype.decade.prop
#   reburn decade     sum_ac   prop
#   1       0   1945  1145955.0 1.0000
#   2       1   1945        0.0 0.0000
#   3       0   1955 11659474.2 0.9805
#   4       1   1955   231383.2 0.0195
#   5       0   1965  5769068.1 0.9562
#   6       1   1965   264134.6 0.0438
#   7       0   1975  4942480.7 0.8813
#   8       1   1975   665780.2 0.1187
#   9       0   1985  3566190.5 0.8994
#   10      1   1985   399023.5 0.1006
#   11      0   1995  8358481.3 0.8910
#   12      1   1995  1022142.7 0.1090
#   13      0   2005 14634610.2 0.7762
#   14      1   2005  4218448.0 0.2238
#   15      0   2015  6613251.4 0.7528
#   16      1   2015  2171875.1 0.2472

# Put in long format
  x.burntype.eco1.long <- melt(x.burntype.prep[, 1:5], id.vars = .(year, reburn), variable.name = 'ecoreg1', value.name = 'ac')
  
# Calculate cumulative sums 
  x.burntype.csums <- ddply(x.burntype.eco1.long, .(ecoreg1, reburn), transform, cumsum_ac = cumsum(ac), total_ac = sum(ac))
  
#   > head(x.burntype.csums[order(x.burntype.csums$year),],10)
#       year reburn ecoreg1         ac  cumsum_ac    total_ac
#   1   1940      0  bor_ac      0.000      0.000 50251644.96
#   131 1940      0  mar_ac      0.000      0.000    84027.31
#   261 1940      0  tun_ac  12865.299  12865.299  6353839.33
#   2   1942      0  bor_ac  27771.513  27771.513 50251644.96
#   132 1942      0  mar_ac      0.000      0.000    84027.31
#   262 1942      0  tun_ac      0.000  12865.299  6353839.33
#   3   1943      0  bor_ac 237576.483 265347.996 50251644.96
#   133 1943      0  mar_ac   5999.759   5999.759    84027.31
#   263 1943      0  tun_ac      0.000  12865.299  6353839.33
#   4   1944      0  bor_ac  96149.149 361497.145 50251644.96

# --------------------------------------------------------------------------- #
# Acres per year, no ecoregion or reburn split ----
# --------------------------------------------------------------------------- #
  
  x.acres.yr <- ddply(x, .(year), summarize,
                acres = sum(acres))
  
  # Pad the time series to get the full range of dates
  x.ts.acres <- padTimeSeries(x.acres.yr, 1940, 2016)
  
  # Replace the NAs with 0 to correctly calculate moving average
  x.ts.acres$acres[is.na(x.ts.acres$acres)] <- 0
  
  # Order by year 
  x.ts.acres <- x.ts.acres[order(x.ts.acres$year), ]
  
  # Calculate moving average for all fires regardless of ecoregion
  x.acres.moveavg <-
    x.ts.acres %>% 
    mutate(ac_10yrMove = rollapply(acres, 10, mean, na.rm = TRUE, align = 'right', fill = NA))
  
#  > head(x.acres.moveavg)
#    year     acres ac_10yrMove
#  1 1940  12865.37          NA
#  2 1941      0.00          NA
#  3 1942  27779.55          NA
#  4 1943 243598.98          NA
#  5 1944  96142.13          NA
#  6 1945      0.00          NA
  
# --------------------------------------------------------------------------- #
# Acres per year/reburn, no ecoregion split ----
# --------------------------------------------------------------------------- #
#   
#   x.ac.yr.reburn <- ddply(x, .(year, reburn), summarize,
#                             sum_ac = sum(acres))
#   
#   # subset  
#   x.ac.yr.reburn.0 <- subset(x.ac.yr.reburn, reburn == 0)
#   x.ac.yr.reburn.1 <- subset(x.ac.yr.reburn, reburn == 1)
#   
#   # Pad the time series to get the full range of dates
#   x.ts.ac.reburn.0 <- padTimeSeries(x.ac.yr.reburn.0, 1940, 2016)
#   x.ts.ac.reburn.1 <- padTimeSeries(x.ac.yr.reburn.1, 1940, 2016)
#   
#   
#   # Replace the NAs with 0 to correctly calculate rolling average;
#   # otherwise rollapply ignores the NAs fields
#   x.ts.ac.reburn.0$sum_sac[is.na(x.ts.ac.reburn.0$sum_ac)] <- 0
#   x.ts.ac.reburn.1$sum_ac[is.na(x.ts.ac.reburn.1$sum_ac)] <- 0
#   
#   # also for reburn type...
#   x.ts.ac.reburn.0$reburn[is.na(x.ts.ac.reburn.0$reburn)] <- 0
#   x.ts.ac.reburn.1$reburn[is.na(x.ts.ac.reburn.1$reburn)] <- 1
#   
#   # Order by year 
#   x.ts.ac.reburn.0 <- x.ts.ac.reburn.0[order(x.ts.ac.reburn.0$year), ]
#   x.ts.ac.reburn.1 <- x.ts.ac.reburn.1[order(x.ts.ac.reburn.1$year), ]
#   
#   # combine the two files
#   x.ts.ac.reburn <- rbind(x.ts.ac.reburn.0, x.ts.ac.reburn.1)
#   x.ts.ac.reburn.wide <- cbind(x.ts.ac.reburn.0, x.ts.ac.reburn.1)
#   
#   # Calculate moving average for all fires regardless of ecoregion
#   x.ac.moveavg.reburn <-
#     x.ts.ac.reburn %>% 
#     group_by(reburn) %>%
#     mutate(ac_10yrMove = rollapply(sum_ac, 10, mean, na.rm = TRUE, align = 'right', fill = NA))
#   
  
# --------------------------------------------------------------------------- #
# Reburn acres only, by fire interval ----
# --------------------------------------------------------------------------- #
  
  # This shows how many acres of a particular fire poly were reburns, and at what time intervals.
  
  # Group REBURN ONLY polygons by parentid and year_int (years since last burn) to consolidate multiple
  # reburns associated with a single fire into different time-since-fire categories.   
  # Year, parentac, name, doy, outdoy, and firelength are the same for each parentid.
  # ParentID now summarizes the attibutes of each group of reburns associated with that parent.

  x.grouped.int <- ddply(subset(x, reburn == 1), .(year, parentid, parentac, name, year_int, doy, outdoy, firelength), summarize,
                         n = sum(!is.na(polyid)), # number of polygons split from parent.  Yes we want 'sum'.
                         sum_ac = sum(acres, na.rm = T),
                         lat = mean(lat, na.rm = T),
                         lon = mean(lon, na.rm = T),
                         bor_ac = sum(boreal * sqm2ac),
                         mar_ac = sum(maritime * sqm2ac),
                         tun_ac = sum(tundra * sqm2ac))
  
  # Assign ecoregion type to the largest proportion of acreage
  x.grouped.int$ecoreg1 <- c("boreal", "maritime", "tundra")[max.col(x.grouped.int[c("bor_ac", "mar_ac", "tun_ac")])]
  
  # Assign # of years since burn to a reburn interval
  x.grouped.int$int_gp <- cut(x.grouped.int$year_int, breaks = c(0, 10, 30, 50, 76), 
                   include.lowest = T, labels = c(5, 20, 40, 63))
  
  # Calculate proportion of burn
  x.grouped.int$prop <- x.grouped.int$sum_ac/x.grouped.int$parentac * 100

  # Break data into pre, post-2004 sets
  x.grouped.int$post2004 <- 0
  x.grouped.int[which(x.grouped.int$year >= 2004),]$post2004 <- 1
  x.grouped.int$post2004 <- as.factor(x.grouped.int$post2004)
  
# By decade
  x.grouped.int.decade <- setDecade(x.grouped.int)
  x.grouped.int.decade <- ddply(x.grouped.int.decade, .(decade, int_gp), summarize, summ_ac = sum(sum_ac))
  
# By proportion in each decade
  x.grouped.int.decade.prop <-ddply(x.grouped.int.decade, .(decade), transform, prop=summ_ac/sum(summ_ac))
  

# Time-since-burn in each ecoregion ----

  x.int.sum <- ddply(x.grouped.int, .(int_gp, ecoreg1, year), summarize, 
                     n_polys = length(unique(parentid)),
                     mean_start = mean(doy),
                     sd_start = sd(doy),
                     se_start = sd(doy)/sqrt(n_polys),
                     sum_ac = sum(sum_ac),
                     mean_lat = mean(lat),
                     mean_lon = mean(lon))
  
#  > head(x.int.eco)  added year, changed name from x.int.eco to plot data, switched year_int for int_gp
#   year_int ecoreg1 n_polys mean_start sd_start se_start     sum_ac mean_lat  mean_lon
#  1        1  boreal      48   171.4167 22.50658 3.248545  92079.762 64.84549 -149.4509
#  2        1  tundra       1   182.0000      NaN      NaN   2198.885 67.17384 -151.5474
#  3        2  boreal      41   178.2439 17.37208 2.713062  97244.268 65.30482 -149.7965
#  4        2  tundra       1   174.0000      NaN      NaN      2.083 68.06302 -160.8072
#  5        3  boreal      46   184.9348 23.98463 3.536340 130080.490 65.02357 -150.3278
#  6        3  tundra       2   164.0000  0.00000 0.000000    891.759 62.21268 -161.7032
  
  
  
# --------------------------------------------------------------------------- #
# By year/burn status/parentid - burns/reburns appear twice ----
# --------------------------------------------------------------------------- #
  
  # This groups individual polygons by parent fire, so that multiple small polygons don't disproportionately 
  # weight group statistics; the attributes of all polygons belonging to a given parent are summarized.
  # This is the precursor file for calculating the average lat, lon, season length, etc. by reburn type. 
  # Similar to x.grouped.int except that characteristics are grouped by 'reburn' type.  A given fire name/id
  # may thus appear twice: once as a burn, once as a reburn. Thus individual acreage sums are correct, but total  
  # fire counts will be off. 
  
  # The better way to do this is in Arc using 'dissolve'.
  
  # This essentially replicates the original dbase of fires, except that fires are split by reburn. 
  # Taking the mean returns the original doy, acres, etc.
  # See following table (x.grouped.burn.reburn) for the case in which the original burns have been dropped.
  
  x.grouped <- ddply(x, .(year, parentid, parentac, reburn, name, doy, outdoy, firelength), summarize,
                         n = sum(!is.na(polyid)), # number of polygons from each parent. Yes we want 'sum'
                         sum_ac = sum(acres, na.rm = T),
                         lat = mean(lat, na.rm = T),
                         lon = mean(lon, na.rm = T),
                         doy = mean(doy, na.rm = T),
                         outdoy = mean(outdoy, na.rm = T),
                         firelength = mean(firelength, na.rm = T),
                         bor_ac = sum(boreal * sqm2ac),
                         mar_ac = sum(maritime * sqm2ac),
                         tun_ac = sum(tundra * sqm2ac))
                         
  # Assign ecoregion type to the largest proportion of acreage
  x.grouped$ecoreg1 <- c("boreal", "maritime", "tundra")[max.col(x.grouped[c("bor_ac", "mar_ac", "tun_ac")])]
  #x.grouped$int_gp <- cut(x.grouped$wtd_int, breaks = c(0, 10, 30, 50, 76), 
  #              include.lowest = T, labels = c(5, 20, 40, 63))
  
  
  # -------------------------------------------------------------------- #
  # By year/burn status/parentid - hard split between burns/reburns ---
  # -------------------------------------------------------------------- #
  
  # Produce dataset of unique parent ids by removing the "original" (once-burned) portions of
  # any original/reburn paired set. This leaves a dataset that distinguishes fires that only burn across
  # unburned areas (since 1940) from those that burn across any amount of a previously burned area. 
  # 
  # ***** This is the file to use for examining burn vs. reburn characteristics, because there is a definitive
  # split between fires types.  It is not the file to use for summing individual polygon acreages, since it
  # ignores the non-reburn areas of fires that have burn/reburn proportions  *****
  
  # Order such that reburns (1) take precedence. Duplicates are then identified as reburn = 0.
  x.grouped.slim <- x.grouped[order(-x.grouped$reburn), ]
  
  # Remove the "original" parentid from any original/reburn pairs
  x.grouped.slim <- x.grouped.slim[!duplicated(x.grouped.slim$parentid), ]
  
  # Group the split burn/reburn parentid fires by year
  x.grouped.burn.reburn <- ddply(x.grouped.slim, .(year, ecoreg1, reburn), summarize,
                            n_fires = length(unique(parentid)), # how many individual parent fires
                            sum_ac = sum(sum_ac),
                            mean_ac = mean(sum_ac),
                            mean_lat = mean(lat),
                            med_lat = median(lat, na.rm = T),
                            sd_lat = sd(lat, na.rm = T),
                            se_lat = sd_lat/sqrt(n_fires),
                            mean_lon = mean(lon),
                            med_lon = median(lon, na.rm = T),
                            sd_lon = sd(lon, na.rm = T),
                            se_lon = sd_lon/sqrt(n_fires),
                            length_start = max(doy) - min(doy),
                            mean_start = mean(doy, na.rm = T),
                            med_start = median(doy, na.rm = T),
                            sd_start = sd(doy, na.rm = T),
                            se_start = sd_start/sqrt(n_fires),
                            mean_length = mean(firelength, na.rm = T),
                            sd_length = sd(firelength, na.rm = T),
                            se_length = sd_length/sqrt(n_fires))
  
   x.grouped.burn.reburn$reburn <- as.factor(x.grouped.burn.reburn$reburn)
  
   # Add cumulative acreage sums to each burn type
   x.grouped.burn.reburn = ddply(x.grouped.burn.reburn, c('ecoreg1', 'reburn'), transform, cumsum_ac = cumsum(sum_ac), total_ac = sum(sum_ac))
   
   
  # write.csv(x.grouped.burn.reburn, file = "D:/projects/ak_fire/tables/x_grouped_burn_reburn.csv", row.names = F)
  
  # ------------------------------------------------------------------- #
  # Fires by year/ecoregion ----
  # ------------------------------------------------------------------- #

  # Fires are assigned to only one ecoregion (the majority component), so total acres calculated here
  # are from a winner-takes-all perspective: even if a proportion of a fire's acreage is from a different
  # class, those acres count towards those of the dominant ecoregion
  
# Keep reburn status out of it -- get all lumped fires                        
   x.allfires.sum <- ddply(x.grouped, .(year, ecoreg1), summarize,
                         n_polys = length(unique(parentid)), # how many individual parent fires
                         sum_ac = sum(parentac),
                         mean_length = mean(firelength, na.rm = T),
                         med_length = median(firelength, na.rm = T),
                         sd_length = sd(firelength, na.rm = T),
                         se_length = sd_length/sqrt(n_polys),
                         mean_lat = mean(lat),
                         med_lat = median(lat, na.rm = T),
                         sd_lat = sd(lat, na.rm = T),
                         se_lat = sd_lat/sqrt(n_polys),
                         mean_lon = mean(lon),
                         med_lon = median(lon, na.rm = T),
                         sd_lon = sd(lon, na.rm = T),
                         se_lon = sd_lon/sqrt(n_polys),
                         length_start = max(doy) - min(doy),
                         mean_start = mean(doy, na.rm = T),
                         med_start = median(doy, na.rm = T),
                         sd_start = sd(doy, na.rm = T),
                         se_start = sd_start/sqrt(n_polys),
                         mean_length = mean(firelength, na.rm = T))
                      #   mean_int = mean(int, na.rm = T),
                      #   sd_int = sd(int, na.rm = T),
                      #   se_int = sd_int/sqrt(n_polys))
  
# Add cumulative acreage sums to each burn type
# x.burntype.sum = ddply(x.burntype.sum, c('reburn'), transform, cumsum_ac = cumsum(sum_ac), total_ac = sum(sum_ac))
#  x.ecotype.sum = ddply(x.allfires.sum, c('ecoreg1', 'reburn'), transform, cumsum_ac = cumsum(sum_ac), total_ac = sum(sum_ac))
  
  
# ------------------------------------------------------ #
# Reburns and their parent fires ----
# ------------------------------------------------------ #

# Areas that have reburned AND their associated initial burns;
  # i.e., any polyid that is duplicated has been reburned at some point
  x.duplicates <- x %>% group_by(polyid) %>% filter(n() > 1)  #dplyr
  
# > x.duplicates
#  Source: local data frame [7,615 x 23]
#  Groups: polyid [3,543]
  
#      ptid polyid    acres      lat      lon       date    outdate parentid parentac          name  year julian   doy outjulian outdoy firelength burn_num reburn year_int julian_int size_rank  top5 top10
#  1     38     33    2.515 60.10923 -150.8311 1994-08-30 1994-10-31     1215   3035.3   WINDY POINT  1994   9007   242      9069    304         62       1      0       NA         NA       103     0     0
#  2     37     33    2.515 60.10923 -150.8311 2005-07-11 2005-11-03      601  25532.7     Fox Creek  2005  12975   192     13090    307        115       2      1       11       3968       447     0     0
#  3     40     34    2.072 60.11062 -150.8373 1994-08-30 1994-10-31     1215   3035.3   WINDY POINT  1994   9007   242      9069    304         62       1      0       NA         NA       105     0     0
#  4     39     34    2.072 60.11062 -150.8373 2005-07-11 2005-11-03      601  25532.7     Fox Creek  2005  12975   192     13090    307        115       2      1       11       3968       452     0     0
#  5     35     35 2698.725 60.11435 -151.1677 1996-06-06 1996-10-11      350  13739.8 Crooked Creek  1996   9653   158      9780    285        127       1      0       NA         NA        37     0     0
#  6     36     35 2698.725 60.11435 -151.1677 2007-06-19 2007-10-30     1205  55437.8 Caribou Hills  2007  13683   170     13816    303        133       2      1       11       4030         3     1     1

# --------------------------------------------------------------------- #
# Proportion of each fire that burned over previous burn areas ----
# --------------------------------------------------------------------- #
  
  # Initial burns are polyids with burn_num == 1.
  # Summarize acres in reburns (polys with burn_num > 1) by firename => total reburned acres associated with parent fire.
  # Using parentid + name means there's no chance of fires with identical names being lumped together.
  
  x.reburn.prop <- ddply(subset(x.duplicates, burn_num > 1), .(burn_num, name, parentid, parentac, year), summarize, 
                    sum_reburn = sum(acres), # of reburn acres
                    n = length(acres), # of fires
                    mean_lat = mean(lat),  # mean latitude of reburn area per firename
                    mean_lon = mean(lon)) # mean longitude of reburn areas per firename
  
  #  proportion in reburn areas
  x.reburn.prop$prop_reburn <- (x.reburn.prop$sum_reburn/x.reburn.prop$parentac)*100
 # write.csv(x.reburn.prop, file = file.path(path.in, 'tables', 'x_reburn_prop.csv'), row.names = F)
  
#   > head(x.reburn.prop)
#     burn_num                 name parentid  parentac year sum_reburn n mean_lat  mean_lon prop_reburn
#   1        2   Little Black River     1757 1366729.1 1950  52357.146 9 66.24598 -143.2706    3.830836
#   2        2       S.W.-Ft. Yukon      756   11791.3 1950   3793.829 1 66.46671 -145.8230   32.174815
#   3        2 Christian River Fire     1555   18997.4 1951  14735.951 1 67.08585 -145.5300   77.568251
#   4        2             Sheenjek      848   68663.6 1953  22689.518 6 66.75125 -144.2416   33.044463
#   5        2        40 Mile Yukon      115  168727.7 1953  16308.459 3 66.19635 -144.1117    9.665549
#   6        2          VENETIE S15     1308   56858.0 1954  48696.800 6 66.78578 -146.3464   85.646347

  # group all reburns by year AND by reburn #
  x.reburn.by.year.rank <- ddply(x.reburn.prop, .(year, burn_num), summarize, 
                              mean_prop_reburn = mean(prop_reburn),
                              med_prop_reburn = median(prop_reburn),
                              sum_reburn_ac = sum(sum_reburn),
                              mean_reburn_ac = mean(sum_reburn),
                              med_reburn_ac = median(sum_reburn),
                              mean_orig_ac = mean(parentac),
                              mean_lat = mean(mean_lat),
                              mean_lon = mean(mean_lon),
                              n = length(prop_reburn),
                              sddev = sd(prop_reburn),
                              se = sddev/sqrt(n))

#  > head(x.reburn.by.year.rank)
#    year burn_num mean_prop_reburn med_prop_reburn sum_reburn_ac mean_reburn_ac med_reburn_ac mean_orig_ac mean_lat mean_lon n
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

  # Not too amazingly useful b/c we want the year_int in there.  Not sure why I even did this...look
  # at x.grouped.int for more complete info, including year_int. 
  
# ------------------------------------------------------------------------------- #
# Acres of ecoregion type in each original (parent fire) polygon ----
# ------------------------------------------------------------------------------- #
  
  x.parent.ac <- ddply(x, .(parentid), summarize,
                       sum_ac = sum(acres),
                       sum_bor_ac = sum(boreal *  sqm2ac),
                       sum_mar_ac = sum(maritime * sqm2ac),
                       sum_tun_ac = sum(tundra * sqm2ac))
  
  x.reburn.ac <- ddply(x, .(polyid), summarize,
                       sum_ac = sum(acres),
                       sum_bor_ac = sum(boreal * sqm2ac),
                       sum_mar_ac = sum(maritime * sqm2ac),
                       sum_tun_ac = sum(tundra * sqm2ac))
  
  # Convert raw acreages to proportion of each ecotype  
  x.parent.ac.prop <- cbind(x.parent.ac, data.frame(t(apply(x.parent.ac[, c(-1, -2)], 1, function(x) x/sum(x)))))
  colnames(x.parent.ac.prop)[6:8] <- c("bor", "mar", "tun")

  # melt to get 1 column with ecoregion type  
  x.parent.ac.prop.melt1 <- melt(x.parent.ac.prop[,c(1, 6:8)], id = ("parentid"))
  x.parent.ac.prop.melt2 <- melt(x.parent.ac.prop[,c(1, 3:5)], id = ("parentid"))
  
  # join the tables
  x.parent.ac.prop.melt <- cbind(x.parent.ac.prop.melt1, x.parent.ac.prop.melt2)
  colnames(x.parent.ac.prop.melt) <- c("parentid", "ecoreg1", "prop", "parentidX", "ecoregX", "ac")
  
  # housekeeping
  x.parent.ac.prop.melt$parentidX <- NULL
  x.parent.ac.prop.melt$ecoregX <- NULL

 # > head(x.parent.ac.prop.melt)
#    parentid ecoreg1 prop         ac
#  1        0     bor    1 154428.739
#  2        1     bor    1   5322.345
#  3        2     bor    1  20129.371
#  4        3     bor    1   8136.970
#  5        4     bor    1  25656.319
#  6        5     bor    1  17339.210
  
  # assign the proportion data to 0, < 0.5, or > 0.5
  x.parent.ac.prop.melt$interval <- cut(x.parent.ac.prop.melt$prop, breaks = c(-1, 0, 0.5, 1), include.lowest = F)
  
  # acres in each proportion
  p <- ddply(x.parent.ac.prop.melt, .(ecoreg1, interval), summarize, sum_ac = sum(ac, na.rm = T))

# this quantifies the acres that are lost when using the 'winner takes all' approach to assigning fires  
#  > p
#   ecoreg1 interval      sum_ac
# 1     bor   (-1,0]        0.00
# 2     bor  (0,0.5]   119913.56
# 3     bor  (0.5,1] 58641056.71
# 4     mar   (-1,0]        0.00
# 5     mar  (0,0.5]    13200.00
# 6     mar  (0.5,1]    84052.22
# 7     tun   (-1,0]        0.00
# 8     tun  (0,0.5]   699270.58
# 9     tun  (0.5,1]  6104805.97

  
# ---------------------------------------------------- #
# Acres in each ecoregion by year and reburn ----
# ---------------------------------------------------- #

# This entry differs from x.ecotype.sum in that the acreages are not reclassified as belonging to the dominant
# ecoregion in each fire. 
  
x.sub <- x[, c("ptid", "polyid", "year", "doy", "reburn", "burn_num", "tundra", "maritime", "boreal")]
x.ecotype <- melt(x.sub, id.vars = c("ptid", "polyid", "year", "doy", "reburn", "burn_num"))
x.ecotype.ac <- ddply(x.ecotype, .(year, reburn, variable), summarize,
                      sum_ac = sum(value * sqm2ac))

#> head(x.ecotype.ac)
#  year reburn variable   sum_ac
#1 1940      0   tundra 12865.30
#2 1940      0 maritime     0.00
#3 1940      0   boreal     0.00
#4 1942      0   tundra     0.00
#5 1942      0 maritime     0.00
#6 1942      0   boreal 27771.51

# ----------------------------------------------- # 
# Acres in each reburn frequency category ----
# ----------------------------------------------- #
  x.burn.ac <- ddply(x, .(burn_num), summarize, sum_acres = sum(acres))

#> x.burn.ac
#  burn_num    sum_acres
#1        1 56690913.379
#2        2  8115673.940
#3        3   738570.623
#4        4    82789.661
#5        5    19675.282
#6        6    11711.582
#7        7     3843.699
#8        8      486.141

  
# -------------------------------------------- #
# Acres of reburn types per year ----
# -------------------------------------------- #

  x.burn.ac.yr <- ddply(x, .(burn_num, year), summarize, sum_acres = sum(acres))
  
#> head(x.burn.ac.yr)
#  burn_num year sum_acres
#1        1 1940  12865.37
#2        1 1942  27779.55
#3        1 1943 243598.98
#4        1 1944  96142.13
#5        1 1946 386946.17
#6        1 1947 343878.28


# -------------------------------------------------- #
# Acres of reburn types per year - burn num 2 ----
# -------------------------------------------------- #

x.burn.ac.yr.net <- ddply(subset(x, burn_num < 3), .(burn_num, year, reburn), summarize, sum_acres = sum(acres))


# ------------------------------------------------------ #
# Mean start date by decade ----
# ------------------------------------------------------ #

# This essentially recreates the original database of fires rather than polygons; #rows = #fires
x.original  <- ddply(x, .(year, parentid, parentac, name), summarize,
                    n = sum(!is.na(polyid)), # number of polygons from each parent. Yes we want 'sum'
                    year_int = mean(year_int, na.rm = T),
                    sum_ac = sum(acres, na.rm = T),
                    doy = mean(doy, na.rm = T),   
                    bor_ac = sum(boreal * sqm2ac),
                    mar_ac = sum(maritime * sqm2ac),
                    tun_ac = sum(tundra * sqm2ac))

# Assign ecoregion type to the largest proportion of acreage
x.original$ecoreg1 <- c("boreal", "maritime", "tundra")[max.col(x.original[c("bor_ac", "mar_ac", "tun_ac")])]

#write.csv(x.original, file = file.path(path.in, "tables", "x_original.csv"), row.names = F)

# assign decade to each row based on year
x.startdates <- setDecade(x.original)

# get # of fires by ecoreg1
x.nfires.eco1 <- ddply(x.original, .(year, ecoreg1), summarize,
                       n_fires = length(unique(parentid)))
#write.csv(x.nfires.eco1, file = "D:/projects/ak_fire/tables/x_nfires_eco1.csv", row.names = F)

# get startdates by year
x.startdates.year <- ddply(x.startdates, .(year, ecoreg1), summarize, 
                             mean_start = mean(doy, na.rm = T),
                            # wtd_mean_start = weighted.mean(doy, sum_ac, na.rm = T),
                             sd_start = sd(doy, na.rm = T),
                             n = length(decade),
                             se_start = sd_start/sqrt(n))

# by decade
x.startdates.decade <- ddply(x.startdates, .(decade, ecoreg1), summarize, 
                             mean_start = mean(doy, na.rm = T),
                           #  wtd_mean_start = weighted.mean(doy, sum_ac, na.rm = T),
                             sd_start = sd(doy, na.rm = T),
                             n = length(decade),
                             se_start = sd_start/sqrt(n))

x.startdates.decade <- x.startdates.decade[order(x.startdates.decade$ecoreg1), ]

##x.startdates.reburns: use x.grouped.burn.reburn

# --------------------------------------- #
# Top 5 fires/reburns in each year ----
# --------------------------------------- #
# No one seems to like this data summary much

  # Get the mean size (acres) of the top 5 fires/reburns in each year
  x.top5size <- ddply(x[which(x$size_rank < 6),], .(year, reburn), summarize,
                      mean_ac = mean(acres),
                      n = length(acres))
  

# --------------------------------------------------- #
# Create Arc-ready file of areas w/ burn numbers ----
# --------------------------------------------------- #

# We want a file that has only the last (highest) number of burns for a given
# polygon, so that it can be joined to a shapefile of polygon perimeters

# Make sure file is ordered by polyid, then by burn_num (highest to lowest)
xx <- x[order(x$polyid, -x$burn_num), ]

# Remove duplicates (all but the first--highest--one)
x.duped <- xx[!duplicated(xx$polyid), ]

# output
#write.csv(x.duped, file = file.path(path.in, 'tables', "processed_x_highest_burn_num.csv"), row.names = F)


# --------------------------------------------------- #
# Areas grouped by parent fire and burn_num ----
# --------------------------------------------------- #

x.test <- ddply(x, .(parentid, name, burn_num), summarize,
                      n = sum(!is.na(polyid)), # number of polygons from each parent. Yes we want 'sum'
                      sum_ac = sum(acres, na.rm = T),
                      lat = mean(lat, na.rm = T),
                      lon = mean(lon, na.rm = T),
                      doy = mean(doy, na.rm = T))

#write.csv(x.test, file = file.path(path.in, 'tables', "processed_x_group_by_parent.csv"), row.names = F)


####################################################### #

# Save RData ----
save.image(file = file.path(path.in, "data", rdata)) 
print(paste0("R data file saved to ", file.path(path.in, "data", rdata)))  

