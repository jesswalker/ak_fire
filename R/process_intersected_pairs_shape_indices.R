# AK reburn project 
#
#  process_intersected_pairs_shape_indices.R
#  
#  The script formerly known as 'shape_indices_burn_reburn_polys.R'.  This script was INGENIOUS, but based
#  on a python script that used intersecting to derive polygon info. This has since been supplanted with 
#  a script that relies on union_analysis.  Still some lovely scripting in there...just obsolete.
#
#  RData saved to "D:/projects/Fire_AK_reburn/R/intersected_data.RData"

# This script calculates the landscape metrics for any given fire that overburned an 
# earlier fire.  It looks at characteristics of the reburn area and the non-reburn area; i.e.,
# the area that burned once (in the newer fire), and the area that overlapped an earlier burn.

# This script processes the output from a python script that completes a pairwise intersection of fire polygons.
# (Rendered obsolete, not quickly enough, by ESRI's inclusion of a "pairwise_analysis" function in Arc 10.4)
# Prior to running this script, an "acres" field was added in Arc and polygons were dissolved to get rid of duplicates.
# The end result is a file of individual reburn polygons with acreage, perimeter (m) fields
# as well as the time-since-reburn in years. 

# Something of a misnomer in the status field: 'noburn' is actually the area that only burned once;
# 'burn' burned twice.

rm(list=ls())

library(reshape2)  # for dcast
library(plyr)  # for ldply
library(dplyr) # for %>%, mutate
path.in <- "D:/projects/Fire_AK_reburn/"


# ------------- function to calculate indices
calc_indices = function(df) {
  
  df$area_cir <- (df$perim^2)/(4*pi)
  df$area_m2 <- df$ac * 4046.86
  df$para <- df$perim/df$area_m2
  df$p2a <- df$perim^2/df$area_m2
  df$shape <- df$perim/(2*sqrt(pi*df$area_m2)) #Schumaker 1996
  df$shape2 <- sqrt(df$area_m2/df$area_cir)
  df$r <- sqrt(df$area_m2/pi)
  df$eac_perim <- 2*pi*df$r
  df$frac <- 2*log(df$perim)/log(df$area_m2) # log in R = ln
  
  return(df)
}


# file of the non-reburned polys (i.e., the non-overlapping portion from the intersection of
# 2 polys). A given fire may have multiple entries, each representing the non-reburned
# portion of an interaction it had with another fire area.
x <- read.csv(file.path(path.in, "tables", "intersect_unburned_areas.csv"), header = T)

#> head(x)
#  FID FireName FireYear CalcAcres          DiscDate parentid    acres     perim
#1   0              2001   84784.1 6/20/2001 0:00:00     1091 77936.51 228477.46
#2   1              2001   84784.1 6/20/2001 0:00:00     1091 83701.54 184681.56
#3   2   031035     1990    6137.7  7/3/1990 0:00:00     1675  5693.53  31970.32
#4   3   031039     1990    2720.7  7/5/1990 0:00:00     1485   761.46   9295.04
#5   4   031045     1990    4013.6  7/3/1990 0:00:00     1183  3798.64  27930.30
#6   5   031051     1990   18172.2 7/17/1990 0:00:00     1173 17907.91  59652.37


# file of intersection polygons (i.e., the overlapping portion from the intersection of
# 2 polys). There are always 2 entries for each unique polygon, representing the 2
# contributing 'parent' polygons.
y <- read.csv(file.path(path.in, "tables", "intersected_polys_1940_2016.csv"), header=T)

# > head(y)
#  FID FireName FireYear CalcAcres parentid         ac     perim
#1   0              2001   84784.1     1091  1082.5927  14704.63
#2   1              2001   84784.1     1091  1797.1959  58577.57
#3   2              2001   84784.1     1091  6847.6196  80196.81
#4   3              2001   84784.1     1091 40513.0513 116001.70
#5   4   031012     1990    4955.9     1674  4955.9014  39294.89
#6   5   031031     1990    2998.1      397   570.1729  15159.55


# order the fires such that each pair of fires (earlier burn and later reburn; 
# id'd by matching acres) is in order: earlier/later 
y <- y[order(y$ac, y$FireYear), ]

# create a column that will assign each fire to first burn, 2nd burn. 
y.fires <- rep(c("fire1", "fire2"), times = nrow(y)/2)

# attach it to the original fire dataset
y.new <- cbind(y, y.fires)

# > head(y.new)
#      FID       FireName FireYear CalcAcres parentid     ac    perim y.fires
#1033 1032 Delta Barley 8     1979    6218.2      424 5.8534  721.094   fire1
#1396 1395  Granite Creek     1987   45274.9     1332 5.8534  721.094   fire2
#3216 3215          WHERE     1990   20484.6     1276 5.9646 2217.560   fire1
#2029 2028           Luck     1991    9996.6     1383 5.9646 2217.560   fire2
#2935 2934    TELSITNA #2     1959    3276.6       36 6.0937  790.683   fire1
#2964 2963          Titna     2015   30678.1     1985 6.0937  790.683   fire2

# create a new df that gets the difference in years between each pair
g <- y.new %>% 
  dcast(ac ~ y.fires, value.var = "FireYear", fill = 0) %>% 
  mutate(year_int = fire2 - fire1) %>%  
  select(ac, year_int) 

#> head(g)
#      ac year_int
#1 5.8534    8
#2 5.9646    1
#3 6.0937   56
#4 6.2624    9
#5 6.3014   18
#6 6.3515    1

# merge later fires (#2) with the data from the earlier file
y.2 <- y.new[y.fires == 'fire2', ]
y.all <- merge(y.2, g, by = "ac")

# join both the unburned polygon (x) and reburn polygon (y) files

# some manipulation ---prior to grouping
# for the file of reburned polys
colnames(y.all) <- c("burn_ac", "fid", "name", "year", "parentac", "parentid", "burn_perim", "y.fires", "year_int")
y.all <- y.all[, c('fid', 'name', 'year', 'parentac', 'parentid', 'burn_ac', 'burn_perim', 'y.fires', 'year_int')]
y.all$noburn_ac <- y.all$parentac - y.all$burn_ac
y <- y.all[order(y.all$fid),]

# remove noburn acreages that are negative or very small
y <- y[y$noburn_ac > 1,]

# for the file of non-reburned polys
x$DiscDate <- NULL
colnames(x) <- c('fid', 'name', 'year', 'parentac', 'parentid', "noburn_ac", "noburn_perim")
x$burn_ac <- x$parentac - x$noburn_ac

# this function allows 2 columns to be used to merge, with both functioning as a "nearest" (i.e,
# this employs the roll='nearest' option, for 2 columns.)
# Frickin genius solution
# https://stackoverflow.com/questions/28435126/merge-data-table-by-2-nearest-variables/28435542#28435542
# do not have to be data.tables

func = function(u,v)
{
  vec = with(x, (u - x$parentid)^2 + (v - x$burn_ac)^2) 
  x[which.min(vec), ]$noburn_perim 
}

# merge to get noburn_perim attached to the file of erased polygons (i.e., non-reburn polys)
xy <- transform(y, noburn_perim = apply(y[, c('parentid', 'burn_ac')], 1, function(u) func(u[1], u[2])))

# this gives us a set of burn ac and perim and noburn ac and perim.

# > head(xy1)
#      fid   name year parentac parentid   burn_ac burn_perim y.fires year_int  noburn_ac noburn_perim
# 705    0        2001  84784.1     1091 1082.5927  14704.633   fire2       23 83701.5073    184681.56
# 1350   2        2001  84784.1     1091 6847.6196  80196.813   fire2       28 77936.4804    228477.46
# 450    7 031035 1990   6137.7     1675  444.1601  11072.856   fire2       21  5693.5399     31970.32
# 930    8 031039 1990   2720.7     1485 1959.2564  15253.517   fire2       33   761.4436      9295.04
# 298   12 031045 1990   4013.6     1183  215.0052   7598.423   fire2       13  3798.5948     27930.30
# 334   13 031051 1990  18172.2     1173  264.2508   5374.450   fire2       19 17907.9492     59652.37
#  
# # bring over the fire return interval (year_int)
# # too lazy to modify function; just define a new one
# func2 = function(u,v)
# {
#   vec = with(y, (u - y$parentid)^2 + (v - y$burn_ac)^2)
#   y[which.min(vec), ]$year_int  
# }
# 
# xy <- transform(xy1, year_int = apply(xy1[, c('parentid', 'burn_ac')], 1, function(u) func2(u[1], u[2])))

#> head(xy)
#  fid   name year parentac parentid noburn_ac noburn_perim burn_ac burn_perim year_int
#1   0        2001  84784.1     1091  77936.51    228477.46 6847.59  80196.813   28
#2   1        2001  84784.1     1091  83701.54    184681.56 1082.56  14704.633   23
#3   2 031035 1990   6137.7     1675   5693.53     31970.32  444.17  11072.856   21
#4   3 031039 1990   2720.7     1485    761.46      9295.04 1959.24  15253.517   33
#5   4 031045 1990   4013.6     1183   3798.64     27930.30  214.96   7598.423   13
#6   5 031051 1990  18172.2     1173  17907.91     59652.37  264.29   5374.450   19


# tag with pre, post-2004 for analysis split
xy$post2004 <- 0
xy$post2004[xy$year > 2003] <- 1
xy$post2004 <- as.factor(xy$post2004)

# > head(xy)
#      fid   name year parentac parentid   burn_ac burn_perim y.fires year_int  noburn_ac noburn_perim post2004
# 705    0        2001  84784.1     1091 1082.5927  14704.633   fire2       23 83701.5073    184681.56        0
# 1350   2        2001  84784.1     1091 6847.6196  80196.813   fire2       28 77936.4804    228477.46        0
# 450    7 031035 1990   6137.7     1675  444.1601  11072.856   fire2       21  5693.5399     31970.32        0
# 930    8 031039 1990   2720.7     1485 1959.2564  15253.517   fire2       33   761.4436      9295.04        0
# 298   12 031045 1990   4013.6     1183  215.0052   7598.423   fire2       13  3798.5948     27930.30        0
# 334   13 031051 1990  18172.2     1173  264.2508   5374.450   fire2       19 17907.9492     59652.37        0
 
# group data by year_int: 20 years. For some reason the last interval kept getting dropped
xy1 <- split(xy, cut(xy$year_int, seq(1, 90, by = 20), right = F, labels = F))
xy.wide <- ldply(xy1, data.frame)
colnames(xy.wide)[1] <- "int_20"
xy.wide$y.fires <- NULL


# This would go perfectly with...x.grouped.int

x.grouped.int <- read.csv("D:/projects/Fire_AK_reburn/tables/x_grouped_int.csv", header = T)

# > head(x.grouped.int)
# year parentid parentac          name year_int doy outdoy firelength n    sum_ac      lat       lon    bor_ac    mar_ac tun_ac  ecoreg1
# 1 1997     1040 14852.90     Moser Bay       47 106    121         15 1 12357.657 57.02073 -154.3265    0.0000 12358.462      0 maritime
# 2 2003     1111  5460.39       ILIAMNA       60 143    162         19 1   864.683 59.77344 -154.8644    0.0000   866.449      0 maritime
# 3 2013     1825  2446.80      Chulitna       56 170    194         24 1  1069.235 60.03323 -155.5582 1069.2728     0.000      0   boreal
# 4 2005     1033 25532.70     Fox Creek       11 192    307        115 3   352.775 60.11388 -150.8339  352.0505     0.000      0   boreal
# 5 2007     1599 55437.80 Caribou Hills       11 170    303        133 2  2704.062 60.12311 -151.1411 2708.9874     0.000      0   boreal
# 6 2005     1033 25532.70     Fox Creek        9 192    307        115 4   839.951 60.18331 -151.1096  840.6512     0.000      0   boreal
#        prop int_gp
# 1 83.200298     40
# 2 15.835553     63
# 3 43.699322     63
# 4  1.381660     20
# 5  4.877650     20
# 6  3.289707      5

# get the ecoregion info in there
xy.wide.eco <- merge(x.grouped.int[, c('name', 'parentid', 'parentac', 'year', 'year_int', 'ecoreg1')], xy.wide, by = c("name", "parentid", 'parentac', 'year', "year_int")) # do not keep all all.x = T)

# > head(xy.wide.eco[order(xy.wide.eco$parentid, xy.wide.eco$year_int),])
#                name parentid parentac year year_int ecoreg1 int_20  fid   burn_ac burn_perim  noburn_ac noburn_perim post2004
# 903    Kandik River        0 154431.6 2005        1  boreal      1 1728  400.1629  10552.692 154031.437    231550.27        1
# 904    Kandik River        0 154431.6 2005       20  boreal      1 1729 1233.4880  13633.290 153198.112    252614.57        1
# 905    Kandik River        0 154431.6 2005       55  boreal      3 1730 6368.1654  28022.171 148063.435    233324.36        1
# 971          LAWSON        3   8129.8 1969       12  boreal      1 1868  470.3644   6260.788   7659.436     26057.26        0
# 1152 MOUNTAIN CREEK        5  17336.4 1969       19  boreal      1 2199 6069.7835  37941.488  11266.617     36723.80        0
# 1708      WIEN LAKE       15  27130.2 1968       11  boreal      1 3225   99.9097   3465.926  27030.290     67407.55        0

# Note that sum_ac != bor + mar + tun_ac != burn_ac
# also prop is now meaningless
# housekeeping
xy.wide.eco$fid <- NULL

# need a unique ID for each fire
xy.wide.eco$id <- seq(1, nrow(xy.wide.eco))
  
# melt the df to get it into long format. 
  
split.1 <- melt(xy.wide.eco, id.vars = c("name", "parentid", "parentac","year", "year_int", "ecoreg1", "int_20", "post2004",
                                      'noburn_perim', 'burn_perim', 'id'), variable.name = "poly_ac", value.name = "ac") 
split.2 <- melt(xy.wide.eco, id.vars = c("name", "parentid", "parentac","year", "year_int", "ecoreg1", "int_20", "post2004",
                                      'noburn_ac', 'burn_ac', 'id'), variable.name = "poly_perim", value.name = "perim") 

# merge the splits
#xy.long <- merge(split.1, split.2)
xy.long <- cbind(split.1, split.2[, c('poly_perim', 'perim')])

# change the level labels
levels(xy.long$poly_ac)[levels(xy.long$poly_ac) == "noburn_ac"] <- "burn"
levels(xy.long$poly_ac)[levels(xy.long$poly_ac) == "burn_ac"] <- "reburn"
levels(xy.long$poly_perim)[levels(xy.long$poly_perim) == "noburn_perim"] <- "burn"
levels(xy.long$poly_perim)[levels(xy.long$poly_perim) == "burn_perim"] <- "reburn"

# # too many redundant columns--somehow that got messed up
# xy.long <- xy.long[xy.long$poly_ac == xy.long$poly_perim, ]
# xy.long$noburn_ac <- NULL
# xy.long$burn_ac <- NULL
 xy.long$noburn_perim <- NULL
 xy.long$burn_perim <- NULL
# xy.long$poly_perim <- NULL

# sum_ac refers only to burned acres and is nonsensical now
colnames(xy.long)[which(colnames(xy.long) == "poly_ac")] <- "status"
xy.long$poly_perim <- NULL
 
# > head(xy.long[order(xy.long$parentid, xy.long$year_int), ], 10)
#                name parentid parentac year year_int ecoreg1 int_20 post2004   id status          ac      perim
# 787    Kandik River        0 154431.6 2005        1  boreal      1        1  787   burn    400.1629  10552.692
# 2354   Kandik River        0 154431.6 2005        1  boreal      1        1  787 noburn 154031.4371 231550.270
# 788    Kandik River        0 154431.6 2005       20  boreal      1        1  788   burn   1233.4880  13633.290
# 2355   Kandik River        0 154431.6 2005       20  boreal      1        1  788 noburn 153198.1120 252614.570
# 789    Kandik River        0 154431.6 2005       55  boreal      3        1  789   burn   6368.1654  28022.171
# 2356   Kandik River        0 154431.6 2005       55  boreal      3        1  789 noburn 148063.4346 233324.360
# 841          LAWSON        3   8129.8 1969       12  boreal      1        0  841   burn    470.3644   6260.788
# 2408         LAWSON        3   8129.8 1969       12  boreal      1        0  841 noburn   7659.4356  26057.260
# 1014 MOUNTAIN CREEK        5  17336.4 1969       19  boreal      1        0 1014   burn   6069.7835  37941.488
# 2581 MOUNTAIN CREEK        5  17336.4 1969       19  boreal      1        0 1014 noburn  11266.6165  36723.800
# 

# add metrics
xy.long <- calc_indices(xy.long)
xy.long <- xy.long[order(xy.long$parentid, xy.long$year_int, xy.long$status), ]


save.image(file = "D:/projects/Fire_AK_reburn/R/intersected_data.RData")
write.csv(xy.long, file = "D:/projects/Fire_AK_reburn/tables/xy_long.csv", row.names = F)
write.csv(xy.wide, file = "D:/projects/Fire_AK_reburn/tables/xy_wide.csv", row.names = F)
