# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# calculate_mean_center.py
# Created on: 2017-02-22 14:18:52.00000
#   (generated by ArcGIS/ModelBuilder)
# Description:
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Overwrite things.  We're feeling confident.
arcpy.env.overwriteOutput = True

# Local variables:
in_shp = "D:\\projects\\Fire_AK_reburn\\data\\AICC_fire_perimeters\\FireAreaHistory.shp"
out_shp1 = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_mean_center_weightedByAcres3.shp"
out_shp2 = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_mean_center3.shp"


# Process: Mean Center weighted by acres
arcpy.MeanCenter_stats(in_shp, out_shp1, "CalcAcres", "FireYear", "")
arcpy.MeanCenter_stats(in_shp, out_shp2, "", "FireYear", "")