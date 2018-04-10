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
library(ggmap)
library(lubridate) # dates
library(dplyr)
library(dtplyr)
library(sp)  # for semivariograms
library(gstat)  # ""


# Set paths
path.in <- "D:/projects/ak_fire/"
path.plots <- "D:/projects/ak_fire/output/plots/frp"

source(file.path(path.in, "R", "ak_functions.R"))

filenames <- c('mxd14a1_gee_2003_plus_fires_r.csv', 'mxd14a1_gee_2004_plus_fires_r.csv',
           'mxd14a1_gee_2005_plus_fires_r.csv', 'mxd14a1_gee_2009_plus_fires_r.csv')

# Read in all files to a single file
x <- do.call(rbind, lapply(file.path(path.in, "data", filenames), read.csv, header=T))




# Read in files
#for (year in c('2003', '2004', '2005', '2009')) {
  
  print(paste0('Year is ', year))
#  filename = paste0('mxd14a1_gee_', year, '_plus_fires_r.csv')
#  x <- read.csv(file.path(path.in, "data", filename), header = T)

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
  
  
  
# ------------------------------------ -
# PLOTS
# ------------------------------------
  
  # Set consistent plot parameters
  dodge = position_dodge(0.16)
  theme_set(theme_bw())
  plot_opts <- theme(panel.grid.minor.x = element_blank(),
                     panel.grid.major.x = element_blank(), # hide gridlines
                     legend.key = element_blank(),  # remove boxes around legend items
                     plot.title = element_text(hjust = 0.5)) # center title
  
# ------- by burn number

  title <- paste0("FRP by burn number, ", year)
  plot.name <- paste0("AK_FRP by burn number_", year, ".png")
  
  p <- ggplot(x, aes(as.factor(burn_num), MaxFRP)) + 
        geom_boxplot() + 
        ylim(0, 1000) +
        labs(x = "Burn number", y = "FRP") +
        ggtitle(title)
  p + plot_opts

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)
  
  
# ------ by years since prior burn
# Drop factors for which there are fewer than 10 points
  keep <- levels(as.factor(x$year_int))[table(x$year_int) > 10]
  x.sub.int <- x[x$year_int %in% keep, ]
  
  title <- paste0("FRP by years since prior burn, ", year)
  plot.name <- paste0("AK_FRP by years since prior burn_", year, ".png")
  
  p <- ggplot(x.sub.int, aes(as.factor(year_int), MaxFRP)) + 
        geom_boxplot() + 
        ylim(0, 1000) + 
        labs(x = "Years since prior burn", y = "FRP") +
        ggtitle(title)
  p + plot_opts
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)
  

# ------- by year of fire
  keep <- levels(as.factor(x$FireYear))[table(x$FireYear) > 10]
  x.sub.yr <- x[x$FireYear %in% keep, ]
  
  title <- paste0("FRP by year of fire, ", year)
  plot.name <- paste0("AK_FRP by year of fire_", year, ".png")
  
  p <- ggplot(x.sub.yr, aes(as.factor(FireYear), MaxFRP)) + 
        geom_boxplot() + 
        ylim(0, 1000) +
        labs(x = "Year of fire", y = "FRP") +
        ggtitle(title)
  p + plot_opts
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# -------- by mean fire interval
  x.pts <- x.sub.int %>% group_by(year_int) %>% summarize(avg = mean(MaxFRP))
  
  title <- paste0("Mean FRP by year interval, ", year)
  plot.name <- paste0("AK_FRP mean FRP by year ", year, ".png")
  
  p <- ggplot(x.pts, aes(year_int, avg)) + 
        geom_point() + 
        geom_smooth(method = 'lm', se = FALSE, size = 0.4) +
        labs(x = "Years since prior burn", y = "FRP") +
        annotate("text", x = 5, y = 400, label = lm_eqn(x.pts, "avg", "year_int"), 
                  parse = TRUE, hjust = 0, vjust = 0, color = "#000000") +
        ggtitle(title)
  p + plot_opts
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)
  
  
# Calculate semivariograms
  x.copy <- x

# convert degrees to km distances
  x.copy$lon_km <- 110. * (x.copy$longitude - (-125))*cos(x.copy$latitude/(360/(2*pi)))
  x.copy$lat_km <- 110. * (x.copy$latitude-65)
  coordinates(x.copy) = ~lon_km+lat_km
  
  
# ------- plot points in x,y km space  
  plot.name <- paste0("AK_FRP bubble plot ", year, ".png")
  png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
  print(bubble(x.copy, zcol = 'MaxFRP', fill = T, do.sqrt = F, maxsize = 1.8))
  dev.off()

# ------- plot variogram
  v = variogram(MaxFRP~1, x.copy)
  level <- c(0.5, 0.95)
  
  plot.name <- paste0("AK_FRP variogram ", year, ".png")
  png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
  print(plot(v, fraction = 0.65, level=level))
  dev.off()

  
# # --------- directional variogram
#   v.fit = fit.variogram(v, vgm("Sph"))
#   x.copy.var.det <- variogram(MaxFRP~lat_km+lon_km, data = x.copy)
#   
#   plot.name <- paste0("AK_FRP directional variogram ", year, ".png")
#   p <- plot(x.copy.var.det)
#   p
  
  plot.name <- paste0("AK_FRP directional variogram ", year, ".png")
  
  x.copy.var.dir <- variogram(MaxFRP~1, data = x.copy, alpha = c(0,45,90,135))
  png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
  print(plot(x.copy.var.dir))
  dev.off()
  
}

