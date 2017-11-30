# Roy Petrakis


rm(list=ls(all=TRUE))
setwd("U://external/wgsc/data/BiophysicalRS/San Carlos LiDAR/Z_projects/LCS/G47/FY16/Roy/Projects/Alaska_Reburn/")
setwd("D:\\projects\\Fire_AK_reburn")
.libPaths( c( .libPaths(), "U://external/wgsc/data/BiophysicalRS/San Carlos LiDAR/Z_projects/LCS/G47/FY16/Roy/R_packages") )

#1 read the data into R
akff_data<-read.csv("tables/akff_1994_2015.csv", header = T)

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


# Get date range for each weather station
# ANIAK
Min_ANIAK<-min(ANIAK$DATE)
Max_ANIAK<-max(ANIAK$DATE)
# BETHEL
Min_BETHEL<-min(BETHEL$DATE)
Max_BETHEL<-max(BETHEL$DATE)
# CHISANA
Min_CHISANA<-min(CHISANA$DATE)
Max_CHISANA<-max(CHISANA$DATE)
# CHISTOCHINA
Min_CHISTOCHINA<-min(CHISTOCHINA$DATE)
Max_CHISTOCHINA<-max(CHISTOCHINA$DATE)
# DENALI_VC
Min_DENALI_VC<-min(DENALI_VC$DATE)
Max_DENALI_VC<-max(DENALI_VC$DATE)
# GULKANA
Min_GULKANA<-min(GULKANA$DATE)
Max_GULKANA<-max(GULKANA$DATE)
# HEALY
Min_HEALY<-min(HEALY$DATE)
Max_HEALY<-max(HEALY$DATE)
# JATAHMUND_LAKE
Min_JATAHMUND_LAKE<-min(JATAHMUND_LAKE$DATE)
Max_JATAHMUND_LAKE<-max(JATAHMUND_LAKE$DATE)
# KENNY_LAKE
Min_KENNY_LAKE<-min(KENNY_LAKE$DATE)
Max_KENNY_LAKE<-max(KENNY_LAKE$DATE)
# KILBUCK
Min_KILBUCK<-min(KILBUCK$DATE)
Max_KILBUCK<-max(KILBUCK$DATE)
# KLAWASI
Min_KLAWASI<-min(KLAWASI$DATE)
Max_KLAWASI<-max(KLAWASI$DATE)
# LAKE_MINCHUMINA
Min_LAKE_MINCHUMINA<-min(LAKE_MINCHUMINA$DATE)
Max_LAKE_MINCHUMINA<-max(LAKE_MINCHUMINA$DATE)
# MAY_CREEK
Min_MAY_CREEK<-min(MAY_CREEK$DATE)
Max_MAY_CREEK<-max(MAY_CREEK$DATE)
# MCKINLEY_RIVER
Min_MCKINLEY_RIVER<-min(MCKINLEY_RIVER$DATE)
Max_MCKINLEY_RIVER<-max(MCKINLEY_RIVER$DATE)
# REINDEER_RIVER
Min_REINDEER_RIVER<-min(REINDEER_RIVER$DATE)
Max_REINDEER_RIVER<-max(REINDEER_RIVER$DATE)
# RUTH_GLACIER
Min_RUTH_GLACIER<-min(RUTH_GLACIER$DATE)
Max_RUTH_GLACIER<-max(RUTH_GLACIER$DATE)
# SLANA
Min_SLANA<-min(SLANA$DATE)
Max_SLANA<-max(SLANA$DATE)
# STAMPEDE
Min_STAMPEDE<-min(STAMPEDE$DATE)
Max_STAMPEDE<-max(STAMPEDE$DATE)
# STRELNA
Min_STRELNA<-min(STRELNA$DATE)
Max_STRELNA<-max(STRELNA$DATE)
# TAZLINA_VILLAGE
Min_TAZLINA_VILLAGE<-min(TAZLINA_VILLAGE$DATE)
Max_TAZLINA_VILLAGE<-max(TAZLINA_VILLAGE$DATE)
# TELIDA
Min_TELIDA<-min(TELIDA$DATE)
Max_TELIDA<-max(TELIDA$DATE)
# TOKLAT
Min_TOKLAT<-min(TOKLAT$DATE)
Max_TOKLAT<-max(TOKLAT$DATE)
# WEIN_LAKE
Min_WEIN_LAKE<-min(WEIN_LAKE$DATE)
Max_WEIN_LAKE<-max(WEIN_LAKE$DATE)
# WIGAND
Min_WIGAND<-min(WIGAND$DATE)
Max_WIGAND<-max(WIGAND$DATE)
# WONDER_LAKE
Min_WONDER_LAKE<-min(WONDER_LAKE$DATE)
Max_WONDER_LAKE<-max(WONDER_LAKE$DATE)


# CREATING A NEW SPREADSHEET WITH MIN AND MAX DATE VALUES
StationName<-c('ANIAK','BETHEL','CHISANA','CHISTOCHINA','DENALI_VC','GULKANA','HEALY','JATAHMUND_LAKE','KENNY_LAKE','KILBUCK',
               'KLAWASI','LAKE_MINCHUMINA','MAY_CREEK','MCKINLEY_RIVER','REINDEER_RIVER','RUTH_GLACIER','SLANA','STAMPEDE',
               'STRELNA','TAZLINA_VILLAGE','TELIDA','TOKLAT','WEIN_LAKE','WIGAND','WONDER_LAKE')
StartDate<-c(Min_ANIAK,Min_BETHEL,Min_CHISANA,Min_CHISTOCHINA,Min_DENALI_VC,Min_GULKANA,Min_HEALY,Min_JATAHMUND_LAKE,
             Min_KENNY_LAKE,Min_KILBUCK,Min_KLAWASI,Min_LAKE_MINCHUMINA,Min_MAY_CREEK,Min_MCKINLEY_RIVER,Min_REINDEER_RIVER,
             Min_RUTH_GLACIER,Min_SLANA,Min_STAMPEDE,Min_STRELNA,Min_TAZLINA_VILLAGE,Min_TELIDA,Min_TOKLAT,Min_WEIN_LAKE,Min_WIGAND,Min_WONDER_LAKE)
EndDate<-c(Max_ANIAK,Max_BETHEL,Max_CHISANA,Max_CHISTOCHINA,Max_DENALI_VC,Max_GULKANA,Max_HEALY,Max_JATAHMUND_LAKE,
           Max_KENNY_LAKE,Max_KILBUCK,Max_KLAWASI,Max_LAKE_MINCHUMINA,Max_MAY_CREEK,Max_MCKINLEY_RIVER,Max_REINDEER_RIVER,
           Max_RUTH_GLACIER,Max_SLANA,Max_STAMPEDE,Max_STRELNA,Max_TAZLINA_VILLAGE,Max_TELIDA,Max_TOKLAT,Max_WEIN_LAKE,Max_WIGAND,Max_WONDER_LAKE)
StationDates<-data.frame(StationName,StartDate,EndDate)
write.csv(StationDates,file="FireBehavior_Weather/ii_Meteorological_Vars/UpdateFocusAreas/StationData/akff_StationDates.csv")



