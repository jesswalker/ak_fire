# 
file.name <- "FirePerimeters_1970_2015_discDate_DENAfuel15kmbuffer_tabAreas.csv"

x <- read.csv(file.path(path.in, file.name), header = T)

# Need ptid and polyid
x <- x[, c(1, 2, 5:27, 34)]
#x <- x[, c("ptid", "polyid", "acres", "date", "year", "julian", "doy", names(x[5:34]))]
#y <- x[, c('FID', 'acres', names(paste0("VALUE_", seq(1:25))), 'DiscDate')]


# Format date columns
x$date <- as.Date(x$DiscDate, format = "%m/%d/%Y")
x$year <- as.numeric(format(x$date, '%Y')) # year
x$julian <- julian(x$date)  # julian date
x$doy <- yday(x$date) # day of year

x$DiscDate <- NULL

colnames(x)[1] <- 'polyid'
colnames(x)[which(colnames(x) == "FID_1")] <- "ptid"

#Get the start/end of the veg categories
class1 <- which(colnames(x) == "VALUE_1")
classN <- which(colnames(x) == "VALUE_25")

# Convert m^2 to hectare
x[, class1:classN] <- x[, class1:classN] * 0.0001

# Get rid of super small acreage polys
x <- subset(x, acres > 0.5)   # previously 0.01.  Doesn't seem to make a difference, even up to 1ac

# Get rank and time between fires
x <- getReburnRanks(x)

x.badburns <- subset(x, reburn == 1 & year_int == 0)
x.clean <- x[!(x$ptid %in% x.badburns$ptid), ]
x <- x.clean

# Redo the rank and reburn analysis to account for the trimmed dataset
x <- getReburnRanks(x)

# Slim it all down a bit; we really just want the reburn tag
drops <- c("ptid", "acres", "julian", "doy", "yr_rank", "year_int", "julian_int", "size_rank")
x.slim <- x[ , !(names(x) %in% drops)]

# Convert to long format by creating 2 new columns: "veg_cat" (veg category) and "veg_ha" (veg hectares)
# This simply reformats the data
x.slim.long <- x.slim %>% tidyr::gather(veg_cat, veg_ha, -c(year, date, polyid, reburn))


#Consolidate categories
x.slim.long$veg_coarse <- ""
cat1 <- c("VALUE_1", "VALUE_2", "VALUE_3")
cat2 <- c("FIRE_SCAR")
cat3 <- c("FRESHWATER_OR_SALTWATER", "ICE_SNOW")
cat4 <- c("URBAN__AGRICULTURE__ROAD")
cat5 <- c("HERBACEOUS__AQUATIC_", "HERBACEOUS__MARSH___INTERIOR_ALASKA__COOK_INLET_BASIN_", 
          "HERBACEOUS__MARSH___NORTHERN_AND_WESTERN_ALASKA_", 
          "HERBACEOUS__MESIC___INTERIOR_ALASKA__COOK_INLET_BASIN_", 
          "HERBACEOUS__MESIC___NORTHERN_AND_WESTERN_ALASKA_", 
          "HERBACEOUS__WET___INTERIOR_ALASKA__COOK_INLET_BASIN_", 
          "HERBACEOUS__WET___NORTHERN_AND_WESTERN_ALASKA_", "HERBACEOUS__WET_MARSH___TIDAL_")
cat6 <- c("MOSS", "LICHEN", "LOW_SHRUB", "LOW_SHRUB_LICHEN", "LOW_SHRUB_OR_TALL_SHRUB__OPEN_CLOSED_",
          "TALL_SHRUB__OPEN_CLOSED_", "DWARF_SHRUB", "DWARF_SHRUB_LICHEN", 
          "SPARSE_VEGETATION__INTERIOR_ALASKA__COOK_INLET_BASIN_",
          "SPARSE_VEGETATION__NORTHERN_AND_WESTERN_ALASKA_")
cat7 <- c("TUSSOCK_TUNDRA__LOW_SHRUB_OR_HERBACEOUS_")
cat8 <- c("HEMLOCK__WOODLAND_CLOSED_", "WHITE_SPRUCE_OR_BLACK_SPRUCE__OPEN_CLOSED_",
          "WHITE_SPRUCE_OR_BLACK_SPRUCE__WOODLAND_", "WHITE_SPRUCE_OR_BLACK_SPRUCE_DECIDUOUS__OPEN_CLOSED_",
          "WHITE_SPRUCE_OR_BLACK_SPRUCE_LICHEN__WOODLAND_OPEN_")
cat9 <- c("BAREGROUND")

# Assign existing veg categories to new, consolidated ones 
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat1] <- "Spruce"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat2] <- "Fire scar"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat3] <- "Water/snow/ice"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat4] <- "Urban/ag/road"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat5] <- "Herbaceous"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat6] <- "Shrub"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat7] <- "Tussock/tundra"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat8] <- "Coniferous"
x.slim.long$veg_coarse[x.slim.long$veg_cat %in% cat9] <- "Bareground"
