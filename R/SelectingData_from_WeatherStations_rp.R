rm(list=ls(all=TRUE))
setwd("U://external/wgsc/data/BiophysicalRS/San Carlos LiDAR/Z_projects/LCS/G47/FY16/Roy/Projects/Alaska_Reburn/")
.libPaths( c( .libPaths(), "U://external/wgsc/data/BiophysicalRS/San Carlos LiDAR/Z_projects/LCS/G47/FY16/Roy/R_packages") )

# Read the weather data into R
akff_data<-read.csv("FireBehavior_Weather/ii_Meteorological_Vars/UpdateFocusAreas/akff_1994_2015.csv")

# Adding the SQL Package
library(sqldf)

# Sub-setting the Focus Area Weather Stations, one-by-one
ANIAK<-sqldf("select * from akff_data where NAME = 'ANIAK'")
BETHEL<-sqldf("select * from akff_data where NAME = 'BETHEL'")
#CHICKEN_CREEK<-sqldf("select * from akff_data where NAME = 'CHICKEN CREEK'")
CHISANA<-sqldf("select * from akff_data where NAME = 'CHISANA'")
CHISTOCHINA<-sqldf("select * from akff_data where NAME = 'CHISTOCHINA'")
#CHITITU<-sqldf("select * from akff_data where NAME = 'CHITITU'")
DENALI_VC<-sqldf("select * from akff_data where NAME = 'DENALI VISITOR CENTER'")
#DUNKLE_HILLS<-sqldf("select * from akff_data where NAME = 'DUNKLE HILLS'")
#EIELSON_VC<-sqldf("select * from akff_data where NAME = 'EIELSON VISITOR CENTER'")     # not EIELSON
#GATES_GLACIER<-sqldf("select * from akff_data where NAME = 'GATES GLACIER'")
GULKANA<-sqldf("select * from akff_data where NAME = 'GULKANA'")
HEALY<-sqldf("select * from akff_data where NAME = 'HEALY'")
JATAHMUND_LAKE<-sqldf("select * from akff_data where NAME = 'JATAHMUND LAKE'")
KENNY_LAKE<-sqldf("select * from akff_data where NAME = 'KENNY LAKE'")
KILBUCK<-sqldf("select * from akff_data where NAME = 'KILBUCK'")
KLAWASI<-sqldf("select * from akff_data where NAME = 'KLAWASI'")
LAKE_MINCHUMINA<-sqldf("select * from akff_data where NAME = 'LAKE MINCHUMINA'")
MAY_CREEK<-sqldf("select * from akff_data where NAME = 'MAY CREEK'")
MCKINLEY_RIVER<-sqldf("select * from akff_data where NAME = 'MCKINLEY RIVER'")
REINDEER_RIVER<-sqldf("select * from akff_data where NAME = 'REINDEER RIVER'")
#ROCK_CREEK_LOW<-sqldf("select * from akff_data where NAME = 'ROCK CREEK LOWER'")
RUTH_GLACIER<-sqldf("select * from akff_data where NAME = 'RUTH GLACIER'")
SLANA<-sqldf("select * from akff_data where NAME = 'SLANA'")
STAMPEDE<-sqldf("select * from akff_data where NAME = 'STAMPEDE'")
STRELNA<-sqldf("select * from akff_data where NAME = 'STRELNA'")
#TANA_KNOB<-sqldf("select * from akff_data where NAME = 'TANA KNOB'")                  # not TANANA
TAZLINA_VILLAGE<-sqldf("select * from akff_data where NAME = 'TAZLINA VILLAGE'")
#TEBAY<-sqldf("select * from akff_data where NAME = 'TEBAY'")
TELIDA<-sqldf("select * from akff_data where NAME = 'TELIDA'")
TOKLAT<-sqldf("select * from akff_data where NAME = 'TOKLAT'")
WEIN_LAKE<-sqldf("select * from akff_data where NAME = 'WEIN LAKE'")
WIGAND<-sqldf("select * from akff_data where NAME = 'WIGAND'")
WONDER_LAKE<-sqldf("select * from akff_data where NAME = 'WONDER LAKE'")



# Select the station information that I want and export a table
DateSelection<-subset(WEIN_LAKE, WEIN_LAKE$DATE>="19980607" & WEIN_LAKE$DATE<="19980702")
FWI_WL<-DateSelection[,c(3,14)]