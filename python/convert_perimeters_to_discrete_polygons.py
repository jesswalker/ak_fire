# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# convert_perimeters_to_discrete_polygons.py
# Description: This script ingests a file of polygons and splits it such
# that each overlapping section becomes a discrete polygon--i.e., it converts
# overlapping polygons into non-overlapping polygons.  A file is created that
# contains the attributes of each polygon that contributes to an overlap.
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

# Overwrite existing files
arcpy.env.overwriteOutput = True

# These files have to exist:
# ----------- User input required here -------------------------

# Arc workspace
env.workspace = "D:\\projects\\projects\\Fire_AK_reburn\\data'"

# Input file of polygons with overlapping perimeters
input_file = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_gt1000ac_notPrescribed.shp"

# Dummy file of empty points
fc_empty_pts = "D:\\projects\\Fire_AK_reburn\\data\\fc_empty_points.shp"
# -------------------------------------------------------------

# Intermediate files - created and deleted
dissolved_polys = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_dissolved_temp.shp"
polys_and_gaps = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_indPolys_and_gaps_temp.shp"
individual_pts = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_individual_pts_temp.shp"

# End files - created and retained
individual_polys = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_individual_polys_bufferedIn300m.shp"
final_pts = "D:\\projects\\Fire_AK_reburn\\data\\firePerimeters_1940_2016_dates_for_each_burn_bufferedIn300m.shp"

# In original polygon file:
# Create field on which to dissolve polys; populate; dissolve
# This makes a single polygon out of all fire areas
arcpy.AddField_management(input_file, "junk", "SHORT", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
arcpy.CalculateField_management(input_file, "junk", "1", "VB", "")
arcpy.Dissolve_management(input_file, dissolved_polys, "", "", "MULTI_PART", "DISSOLVE_LINES")
print 'Polygons dissolved..."

# Create non-overlapping file of polygons
# Here gaps between polygons show up as 'real' polygons
arcpy.FeatureToPolygon_management(input_file, polys_and_gaps, "", "ATTRIBUTES", fc_empty_pts)

# Intersect dissolved polys with polys and gaps to remove gaps
arcpy.Intersect_analysis([dissolved_polys, polys_and_gaps], individual_polys, "ALL", "", "INPUT")
print 'Polygons intersected...'

# Calculate acreage for non-overlapping polys
arcpy.AddField_management(individual_polys, "acres", "DOUBLE", "15", "3", "", "", "NULLABLE", "NON_REQUIRED", "")
arcpy.CalculateField_management(individual_polys, "acres", "!Shape.Area@acres!", "PYTHON_9.3", "")
print 'Acreage calculated...'

# Convert non-overlapping polys to points
arcpy.FeatureToPoint_management(individual_polys, individual_pts, "INSIDE")
print 'Polygons converted to points...'

# Intersect points with original polygon file
arcpy.Intersect_analysis([input_file, individual_pts], final_pts, "ALL", "", "INPUT")
print 'Points intersected with original polygon file...'
print 'Done! Polygon file written to ' +

# Delete the raster row
arcpy.Delete_management(dissolved_polys)
arcpy.Delete_management(polys_and_gaps)
arcpy.Delete_management(individual_pts)


