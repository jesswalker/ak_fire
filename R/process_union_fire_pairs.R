########################################################################### #
#
# process_union_fire_pairs.R 
#
# Objective:
# This script calculates the landscape metrics for any given pair of fires that spatially overlap.
# It produces metrics for:
#   1.  The non-reburned area of the older fire;
#   2.  The non-reburned area of the newer fire (i.e., the fire that overburned the earlier one);
#   3.  The reburned area that the two fires have in common.

# Input:
#
# - union_polygon_results.csv -- output from the python script 'union_fire_pairs.py', 
#   which completes a pairwise intersection of fire polygons. (Since rendered obsolete 
#   by ESRI's introduction of the "pairwise_analysis" function in Arc 10.4). Prior to 
#   running this R script, fields for 'acres' ("ac") and 'perimeter' ("perim_m", in meters) 
#   must be added in Arc for all polygons.  The first entry is also duplicated and should be removed. 

# - x_original.csv --"original" file of fires with the addition of ecoregion level 1 membership.
#   Used to associate ecoregion information with the file of processed polygons.
#   
# Output:
#
# - x_metrics.csv -- file of all props, shape metrics
# - shape_metrics.RData - file of R data
#
# August 2017 JWalker
#  
########################################################################### #

# ---- Set environment ----
# Remove everything
rm(list=ls())

# Load libraries
library(reshape2)  # dcast
library(plyr)  # ldply
library(dplyr) # %>%, mutate

# Set directories
path.in <- "D:/projects/Fire_AK_reburn/"
path.plots <- "D:/projects/Fire_AK_reburn/plots"

# Set input/output filenames
filename.in <- "union_polygon_results.csv"
filename.eco <- "x_original.csv"
filename.out <- "x_shape_metrics.csv"
rdata.out <- "shape_metrics.RData"

# ---- Functions ----
# - calculate indices
calc_metrics = function(df) {
  
  df$area_cir <- (df$perim_m^2)/(4*pi)
  df$area_m2 <- df$ac * 4046.86
  df$para <- df$perim_m/df$area_m2
  df$p2a <- df$perim_m^2/df$area_m2
  df$shape <- df$perim_m/(2*sqrt(pi*df$area_m2)) #Schumaker 1996
  df$shape2 <- sqrt(df$area_m2/df$area_cir)
  df$r <- sqrt(df$area_m2/pi)
  df$eac_perim <- 2*pi*df$r
  df$frac <- 2*log(df$perim_m)/log(df$area_m2) # log in R = ln; log10 in R = log
  return(df)
}

# - count # of polys within a given group
assign_count <- function(df, burntype){
  df$count <- sum(na.omit(df$type == burntype)) 
  return(df)
}

# - assign maximum year_int to others within the same group
# For some reason this doesn't work as an integrated function with AVE; all columns are 
# assigned the same value. Works nicely when broken out, though--same for count function, above.
assign_ints <- function(df){
  df$year_int <- max(df$year_int)
  return(df)
}

# - add missing burn info to completely reburned set
# to add burn1, turn off all burn2 elements, and vice versa
add_fcn_burn <- function(df, burn_num, other_burn_num){
  if (df$count[1] == 0) {
    new_row <- df[df$type == 'reburn', ]
    new_row[[paste0('name', other_burn_num)]] <- ""
    new_row[[paste0('fid', other_burn_num)]] <- -1
    new_row[[paste0('year', other_burn_num)]] <- NA
    new_row[[paste0('parentac', other_burn_num)]] <- 0.0
    new_row[[paste0('parentid', other_burn_num)]] <- 0
    new_row$type <- paste0('burn', burn_num)
    df <- rbind(df, new_row)
  }
  return(df)
}


# ---- Input ----
# Read and subset ecoregion file  
x.ecoreg <- read.csv(file.path(path.in, "tables", filename.eco), header = T)
x.ecoreg.sub <- x.ecoreg[, c("parentid", "ecoreg1")]

# Read file of intersected polys. Each pair typically has 3 associated
# entries:  1x non-reburned poly (older fire); 1x non-reburned poly (newer fire);
# and 1x reburned area. Totally reburned areas have 2 entries: 
# one for the area around but not including the reburn, and 1 for the
# reburn itself.  
x <- read.csv(file.path(path.in, "tables", filename.in), header = T) 

# Original file is messy
# 
# > head(x)
#   FID FID_prior_      FireName FireYear CalcAcres           DiscDate parentid FID_curr_f FireName_1 FireYear_1 CalcAcre_1         DiscDate_1
# 1   0          1 Olga Bay Fire     1950   33808.2 11/26/1950 0:00:00      660         -1                    NA       0.00 12/30/1899 0:00:00
# 2   1         -1                     NA       0.0 12/30/1899 0:00:00        0          1  Moser Bay       1997   14852.90  4/16/1997 0:00:00
# 3   2          1 Olga Bay Fire     1950   33808.2 11/26/1950 0:00:00      660          1  Moser Bay       1997   14852.90  4/16/1997 0:00:00
# 4   3          1  Iliamna Lake     1943    5999.1  6/19/1943 0:00:00     1715         -1                    NA       0.00 12/30/1899 0:00:00
# 5   4         -1                     NA       0.0 12/30/1899 0:00:00        0          1    ILIAMNA       2003    5460.39  5/23/2003 0:00:00
# 6   5          1  Iliamna Lake     1943    5999.1  6/19/1943 0:00:00     1715          1    ILIAMNA       2003    5460.39  5/23/2003 0:00:00
# parentid_1 pairid       ac  perim_m
# 1          0      1 21450.54 72242.81
# 2       1040      1  2495.20 29787.90
# 3       1040      1 12357.66 39645.59
# 4          0      2  5134.37 35523.82
# 5       1111      2  4595.43 22067.80
# 6       1111      2   864.68 11354.70

# Get rid of extraneous columns, rename others for consistency.
x$DiscDate <- NULL
x$DiscDate_1 <- NULL
x$Shape_Area <- NULL  # automatically added for gdb files; will not exist for shapefiles
x$Shape_Length <- NULL
colnames(x)[which(colnames(x) == "OBJECTID")] <- "id"
colnames(x)[which(colnames(x) == "FireYear")] <- "year1"
colnames(x)[which(colnames(x) == "FireYear_1")] <- "year2"
colnames(x)[which(colnames(x) == "FireName")] <- "name1"
colnames(x)[which(colnames(x) == "FireName_1")] <- "name2"
colnames(x)[which(colnames(x) == "CalcAcres")] <- "parentac1"
colnames(x)[which(colnames(x) == "CalcAcre_1")] <- "parentac2"
colnames(x)[which(colnames(x) == "parentid")] <- "parentid1"
colnames(x)[which(colnames(x) == "parentid_1")] <- "parentid2"
colnames(x)[which(colnames(x) == "parentid")] <- "parentid1"

# fid1 = earlier burn; fid2 = later
colnames(x)[which(colnames(x) == "FID_prior_")] <- "fid1"
colnames(x)[which(colnames(x) == "FID_curr_f")] <- "fid2"

# Reclassify name field to be able to compare them. Factor levels get in the way.
x$name1 <- as.character(x$name1)
x$name2 <- as.character(x$name2)

# Assign burn type to distinguish areas
x$type[x$fid1 == 1 & x$fid2 == -1] <- "burn1"
x$type[x$fid1 == -1 & x$fid2 == 1] <- "burn2"
x$type[x$fid1 == 1 & x$fid2 == 1] <- "reburn"
x$type <- as.factor(x$type)

# > head(x)
#   FID fid1         name1 year1 parentac1 parentid1 fid2     name2 year2 parentac2 parentid2 pairid       ac  perim_m   type
# 1   0    1 Olga Bay Fire  1950   33808.2       660   -1              NA      0.00         0      1 21450.54 72242.81  burn1
# 2   1   -1                  NA       0.0         0    1 Moser Bay  1997  14852.90      1040      1  2495.20 29787.90  burn2
# 3   2    1 Olga Bay Fire  1950   33808.2       660    1 Moser Bay  1997  14852.90      1040      1 12357.66 39645.59 reburn
# 4   3    1  Iliamna Lake  1943    5999.1      1715   -1              NA      0.00         0      2  5134.37 35523.82  burn1
# 5   4   -1                  NA       0.0         0    1   ILIAMNA  2003   5460.39      1111      2  4595.43 22067.80  burn2
# 6   5    1  Iliamna Lake  1943    5999.1      1715    1   ILIAMNA  2003   5460.39      1111      2   864.68 11354.70 reburn

# Remove duplicate fires. Each pair of fires appears twice--once from the perspective of each fire--due to 
# python-script flailing. The usual command of dissolve in arc doesn't work in this case b/c we want to 
# retain the pairid identifier for each set. 
x.unique <- x[!duplicated(x[, c("parentid1", "fid2", "parentid2", "ac", "type")]), ]

# Add id for complete reburns
x.unique$allburn1 <- 0
x.unique$allburn2 <- 0 

# Count the number of burn1 polys in each group.  Burn1 polys don't exist 
# for pairs in which the newer fire completely contains the older one.
x.count <- ddply(x.unique, .(pairid), function(y) m <- assign_count(y, "burn1"))

# Add burn1 entry to complete reburn pairs
x.slim <- ddply(x.count, .(pairid), function(y) m <- add_fcn_burn(y, "1", "2"))

# ID appropriately
x.slim$allburn1[x.slim$count == 0] <- 1 

# Count the number of burn2 polys in each group.  Burn2 polys don't exist
# for pairs in which the older fire completely contains the newer one.
x.count <- ddply(x.slim, .(pairid), function(y) m <- assign_count(y, "burn2"))

# Add burn2 entry to complete reburn pairs
x.slim <- ddply(x.count, .(pairid), function(y) m <- add_fcn_burn(y, "2", "1"))

# ID appropriately
x.slim$allburn2[x.slim$count == 0] <- 1

# Housekeeping; remove count
x.slim$count <- NULL

# > head(x.slim)
#   FID fid1         name1 year1 parentac1 parentid1 fid2     name2 year2 parentac2 parentid2 pairid       ac  perim_m   type allburn1 allburn2
# 1   0    1 Olga Bay Fire  1950   33808.2       660   -1              NA      0.00         0      1 21450.54 72242.81  burn1        0        0
# 2   1   -1                  NA       0.0         0    1 Moser Bay  1997  14852.90      1040      1  2495.20 29787.90  burn2        0        0
# 3   2    1 Olga Bay Fire  1950   33808.2       660    1 Moser Bay  1997  14852.90      1040      1 12357.66 39645.59 reburn        0        0
# 4   3    1  Iliamna Lake  1943    5999.1      1715   -1              NA      0.00         0      2  5134.37 35523.82  burn1        0        0
# 5   4   -1                  NA       0.0         0    1   ILIAMNA  2003   5460.39      1111      2  4595.43 22067.80  burn2        0        0
# 6   5    1  Iliamna Lake  1943    5999.1      1715    1   ILIAMNA  2003   5460.39      1111      2   864.68 11354.70 reburn        0        0

#---- Calculate fire intervals ----

# Set all intervals to 0
x.slim$year_int <- 0

# Get the fire interval from reburn areas, which have info from both parents
x.slim$year_int[x.slim$type == "reburn"] <- x.slim$year2[x.slim$type == "reburn"] - x.slim$year1[x.slim$type == "reburn"]

# Assign interval to all fires in the same pairid group
x.slim <- ave(x.slim, x.slim$pairid, FUN = function(y) assign_ints(y)) 

# > head(x.slim)
#   FID fid1         name1 year1 parentac1 parentid1 fid2     name2 year2 parentac2 parentid2 pairid       ac  perim_m   type allburn1 allburn2 year_int
# 1   0    1 Olga Bay Fire  1950   33808.2       660   -1              NA      0.00         0      1 21450.54 72242.81  burn1        0        0       47
# 2   1   -1                  NA       0.0         0    1 Moser Bay  1997  14852.90      1040      1  2495.20 29787.90  burn2        0        0       47
# 3   2    1 Olga Bay Fire  1950   33808.2       660    1 Moser Bay  1997  14852.90      1040      1 12357.66 39645.59 reburn        0        0       47
# 4   3    1  Iliamna Lake  1943    5999.1      1715   -1              NA      0.00         0      2  5134.37 35523.82  burn1        0        0       60
# 5   4   -1                  NA       0.0         0    1   ILIAMNA  2003   5460.39      1111      2  4595.43 22067.80  burn2        0        0       60
# 6   5    1  Iliamna Lake  1943    5999.1      1715    1   ILIAMNA  2003   5460.39      1111      2   864.68 11354.70 reburn        0        0       60

# Associate ecoregion info with the fires. Split df to alternately use parentid1 and 2.
x.eco1 <- merge(x.slim[x.slim$type == "burn1", ], x.ecoreg.sub, by.x = "parentid1", by.y = "parentid", all.x = T)
x.eco2 <- merge(x.slim[x.slim$type != "burn1", ], x.ecoreg.sub, by.x = "parentid2", by.y = "parentid", all.x = T)
x.eco <- rbind(x.eco1, x.eco2)

# Calculate proportions of burned area for each of the first and second fires
# 'ac' is the un-reburned area, so we need the inverse.  
x.props <- transform(x.eco, prop_reburn =
  ifelse(fid1 == 1, 
  round(100 - ac/parentac1 * 100, 3),
  round(100 - ac/parentac2 * 100, 3)) 
)

# ---- Calculate proportions ----
# Set proportion of reburn in reburn to NA
x.props$prop_reburn[x.eco$type == 'reburn'] <- NA

# Assign complete reburn proportions to 100, because otherwise they calculate as 0
x.props$prop_reburn[x.props$type == 'burn1' & x.props$allburn1 == 1] <- 100
x.props$prop_reburn[x.props$type == 'burn2' & x.props$allburn2 == 1] <- 100

# Order appropriately
x.props <- x.props[order(x.props$pairid), ]


# ---- Calculate metrics ----
x.metrics <- calc_metrics(x.props)
x.metrics$type <- as.factor(x.metrics$type)


# Calculate averages for better plotting
x.sub1 <- subset(x.sub, type == "burn1")
x.sub2 <- subset(x.sub, type == "burn2")

x.sub1.means <- ddply(x.sub1, .(year_int, ecoreg1), summarize, 
                      n = length(FID), 
                      mean = mean(prop_reburn, na.rm = T), 
                      sd = sd(prop_reburn, na.rm = T), 
                      se = sd/sqrt(n))

x.sub2.means <- ddply(x.sub2, .(year_int, ecoreg1), summarize, 
                      n = length(FID), 
                      mean = mean(prop_reburn, na.rm = T), 
                      sd = sd(prop_reburn, na.rm = T), 
                      se = sd/sqrt(n))

x.sub1.means$type <- "burn1"
x.sub2.means$type <- "burn2"

x.sub.means <- rbind(x.sub1.means, x.sub2.means)

# ---- Output ----

# File
write.csv(x.metrics, file = file.path(path.in, "tables", filename.out), row.names = F)

# RData
save.image(file = file.path(path.in, "R", rdata.out))

