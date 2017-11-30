#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      jjwalker
#
# Created:     24/05/2017
# Copyright:   (c) jjwalker 2017
# Licence:     <your licence>
#-------------------------------------------------------------------------------

def main():
    pass

if __name__ == '__main__':
    main()


# Import arcpy module

import arcpy
from arcpy import env
arcpy.env.workspace = "D:\\projects\\ak_fire\\gis\\data\\temp2"

# Set to overwrite
arcpy.env.overwriteOutput = True
file_list = arcpy.ListFeatureClasses()
target_shp = "allpolys.shp"
##min_n = 3501
##max_n = 4179
##target_shp = "allpolys" + str(min_n) + "_" + str(max_n) + ".shp"

arcpy.CopyFeatures_management("poly1.shp", target_shp)
print "list of files:" + str(file_list)
for f in file_list:
    print f
    arcpy.Append_management([f], target_shp, "NO_TEST")
print "Done!"
