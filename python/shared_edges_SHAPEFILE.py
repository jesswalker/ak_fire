# ---------------------------------------------------------------------------
# shared_edges_SHAPEFILE.py
#
# Created on: 2017-11-08 JJWalker
#
# Description:
# This script analyzes all burn/reburn pairs to retrieve the perimeter
# shared between a reburn and the original fire; i.e., the section
# of the older fire border "within" the more recent fire polygon.  The
# perimeter is returned as a shapefile.

# This script differs from earlier versions in that it intersects the previous
# fire and current fire to get *all* intersection polygons.  Earlier versions
# intersected each individual reburn polygon. This script improves on
# shared_edges_LENGTH in that it returns the complete shapefil.

# 'pairwise_analysis' in ArcMap 10.4+ now does exactly what this script
# does. Thanks, ESRI!


# Input files:

# 1.  processed_x.shp

# Shapefile created from the output of process_alaska_burn_data.R,
# which assigns burn numbers and fire intervals to the individual (i.e.,
# non-overlapping) polygons created from the AICC database. The file contains
# a point feature for each polygon burn, resulting in muliple entries for each
# reburned polygon.
# I.e.,
#
# id ptid polyid  acres lat    lon     date       parentid  parentac  parentname burn_num
#  ...
# 10   3  11   12357.6	57.02 -154.32  11/26/1950  660	33808.2	   Olga Bay Fire  1
# 11   4  11   12357.6	57.02 -154.32  4/16/1997  1040	14852.9	   Moser Bay      2

# Here ^^ polyid 11 burned in 1950 in the Olga Bay Fire and then again in 1997
# in the Moser Bay fire.

# ------------------
# 2.  firePerimeters_1940_2016_individual_polys_copy.shp
#
# Non-overlapping shapefile of fire polygons

# FID Shape  acres   polyid
# ...
# 11 Polygon 12357.6  11

# ----------------------------------------------------------
#  Output:   D:\\projects\\Fire_AK_reburn\\data\\temp
#  intersectAreas_.shp
#
# Process:
# - Choose a polygon that has burned multiple times (file: processed_x)
# - Get parentID of parent (current) whole fire (processed_x)
# - Use polyID and previous fire# to get previous fire parentID (processed_x)
# - Intersect current and previous fires to get all common areas
# - PolygonNeighbors command on reburn polygon and previous fire to get perimeter
# - Write table to dbf
# - Merge all tables

# Notes:
# - This script relies on shapefiles
# - Currently only uses 2nd generation burns; i.e., the 1st reburn of an area
#   is linked to the underlying original burn.
#
# Note that the file produced has a separate entry for each separate polygon in
# the individual polygon database.  Thus duplicates need to be removed in R.
# Requires post-processing in R to calculate the fire interval and metrics.
# See process_intersected_fire_pairs.R

# -----------------------------------------------------------------------

# Import arcpy module
import arcpy, os, time
from arcpy import env

# Set to overwrite
arcpy.env.overwriteOutput = True


# ********  INPUT REQUIRED HERE **************************
# Paths
arcpy.env.workspace = "D:\\projects\\ak_fire\\gis\\data"
ws = os.path.join(arcpy.env.workspace, "temp")

# Local variables
reburn_num = 2  # reburn number of interest (will look for this and n-1)
filename_add = "_all"  # tag for filename
out_shp_name = "reburns_x" + str(reburn_num) + "_shared_edges" + filename_add + ".shp"
# ********************************************************

pt_file = "processed_x.shp"
original_polys = "firePerimeters_1940_2016_gt1000ac_notPrescribed_copy.shp"

# some commands require layers, not fcs
arcpy.MakeFeatureLayer_management(pt_file, "pt_lyr")
arcpy.MakeFeatureLayer_management(original_polys, "orig_polys_lyr")

# set up initial filter
whereClause = '"burn_num" < ' + str(reburn_num + 1) + ' AND "acres" > 5' # AND "FID" < 90'
arcpy.SelectLayerByAttribute_management("pt_lyr", "NEW_SELECTION", whereClause)

# iterate through all polygons in the shapefile
burn_num = "burn_num"
poly_id_field = "polyid"
parent_id_field = "parentid"

# we want the subset of burns < burn_num
cursor = arcpy.SearchCursor("pt_lyr")

# initialize lists to hold consolidated feature classes/tables
fcs_list = []

# initialize counter
n_poly = 0

# start the clock
ts0 = time.time()

# step through all rows in the POINTS layer
for row in cursor:
    try:
        if row.getValue(burn_num) == reburn_num:

            # track number of processed polygons
            n_poly = n_poly + 1

            # get the polygon and parent id of a burn
            poly_id = row.getValue(poly_id_field)
            newer_parent_id = row.getValue(parent_id_field)

            # get the older burn info for the same polygon
            sql_older_burn = '"polyid" = ' + str(poly_id) + ' AND "burn_num" = ' + str(reburn_num - 1)

            # initialize cursor to find id of the older burn
            cursor2 = arcpy.SearchCursor("pt_lyr", sql_older_burn)

            # get both newer and older burn polys
            for row2 in cursor2:
                older_parent_id = row2.getValue(parent_id_field)  # get the older fire id
                sql_newer_poly = '"parentid" = ' + str(newer_parent_id)
                sql_older_poly = '"parentid" = ' + str(older_parent_id)
                sql_both_polys = '"parentid" = ' + str(older_parent_id) + ' OR "parentid" = ' + str(newer_parent_id)  # sql to get both older, newer burn polys
                print '----------------------------------'
                print 'Processing fire FIDs ' + str(older_parent_id) + ' and ' + str(newer_parent_id)

                # Prior fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_older_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/older_fc")

                # Reburn fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_newer_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/newer_fc")

            # Intersect prior fire and newer fire polygons to get the overlapping sections (reburn areas)
                arcpy.Intersect_analysis(["in_memory/older_fc", "in_memory/newer_fc"], "in_memory/reburn_fc", "ALL")

            # Intersect prior fire and reburn area to get perimeter of prior fire within newer burn
                shared_line = os.path.join(ws, "line" + str(n_poly) + ".shp")
                arcpy.Intersect_analysis(["in_memory/older_fc", "in_memory/reburn_fc"], shared_line, "ALL", "#", "LINE")

            # Add perimeter shapefile to list
                fcs_list.append(os.path.join(ws, shared_line))

                # clean up
                print 'Deleting memory...'
                arcpy.Delete_management('in_memory')

    except:
        print ' ** Could not save polygon ' + str(poly_id)
        ts1 = time.time()
        print 'Time elapsed is: ' + str(ts1 - ts0)
        print arcpy.GetMessages()
        continue


# Merge individual polygons and tables
print ' ====================================================='
print ' ====================================================='
print 'Total number of processed polygons ' + str(n_poly)

# Merge tables
print 'Merging all shapes...'

# Copy 1st element to list
sample_fcs = fcs_list[0]
arcpy.CopyFeatures_management(sample_fcs, os.path.join(ws, out_shp_name))

# Iterate through all but the 1st element
iterfcs = iter(fcs_list)
next(iterfcs)
for fc in iterfcs:
    arcpy.Append_management([fc], os.path.join(ws, out_shp_name), "NO_TEST")

# Clean up
print 'Deleting files...'
arcpy.Delete_management('in_memory')

for fc in fcs_list:
    arcpy.Delete_management(fc)

print 'Done! Files written to: '
print os.path.join(ws, out_shp_name)

ts1 = time.time()
print 'Time elapsed: ' + str(ts1 - ts0) + ' seconds'



