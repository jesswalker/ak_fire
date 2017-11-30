# -----------------------------------------------------------------------------
# Name: selected_pts_to_raster.py

# Author: rpetrakis

# Created May 2, 2017
# Copyright: (c) rpetrakis 2017
# ------------------------------------------------------------------------------

# The goal of this script it to autmoatically select certain points, dependent
# on date, and create a raster file with those points. The script will select
# the next date continuously, through the 'select by attributes' tool and then
# create a raster using those pionts with'points to raster' tool. The rasters
# will be put into a folder for the correct year of the points.


#-----------------------------------------------------
# ------------- User input required ------------------
# ----------------------------------------------------


# Required Libraries
import arcpy
from arcpy import env


#Function checks to see if a feature class exists and then deletes it
def checkAndDelete(path):
    if arcpy.Exists(path):
        print(path +": Exists, Deleted")
        arcpy.Delete_management(path)
    else:
        print(path +": Does Not Exist")


# Folder for input yearly fire behavior files - USER DEFINED BY CHAINGING THE OUTPUT GEODATABASE
env.workspace = r"U:\external\wgsc\data\BiophysicalRS\San Carlos LiDAR\Z_projects\LCS\G47\FY16\Roy\Projects\Alaska_Reburn\FireBehavior_Weather\i_Fire_Behavior\VIIRS_FireBehavior_2014_2016_ak\VIIRS_FirePts.gdb"

# Project to meter based coordinate system (NAD 83 Alaska Albers) - USER DEFINED BY CHAINGING THE INPUT AND OUTPUT FEATURE CLASSES
arcpy.Project_management("VIIRS_BandI_FireBehav2016FA", "VIIRS_FireBehav_2016FA_nad83AkAlbers", "PROJCS['NAD_1983_Alaska_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-154.0],PARAMETER['Standard_Parallel_1',55.0],PARAMETER['Standard_Parallel_2',65.0],PARAMETER['Latitude_Of_Origin',50.0],UNIT['Meter',1.0]]", "", "GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]", "NO_PRESERVE_SHAPE", "", "NO_VERTICAL")

firePts = "VIIRS_FireBehav_2016FA_nad83AkAlbers"


# Loop the select by attributes and convert point to raster
with arcpy.da.SearchCursor(firePts, ["JULIAN"]) as cursor:
    list=sorted({row[0] for row in cursor})

valField = "JULIAN"
assignmentType = "MOST_FREQUENT"
priorityField = "NONE"
cellSize = 375

for item in list:
    selected = env.workspace + r"\selected" + "_" +str(item)
    outRaster = env.workspace + r"\raster" + "_" +str(item)
    print item
    checkAndDelete(selected)
    checkAndDelete(outRaster)
    arcpy.Select_analysis(firePts, selected, "JULIAN = " + str(item))

    arcpy.PointToRaster_conversion(selected, valField, outRaster, assignmentType, priorityField, cellSize)
