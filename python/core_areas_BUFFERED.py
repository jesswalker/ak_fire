# ---------------------------------------------------------------------------
# Buffered_CORE_AREAS.py
#
# Created on: 2017-11-08 JJWalker
#
# Description:
# This script analyzes all burn/reburn pairs to retrieve the area of
# reburn within its prior fire, which has been buffered inward a user-specified
# amount. I.e., the identified polygon is the amount of reburn beyond a given
# distance from the original fire boundary.

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
# - Buffer previous fire inward a user-specified amount
# - Intersect buffered previous fire with reburn polygon
# - Write polygon to shapefile
#
# - Merge all shapefiles


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
arcpy.env.workspace = "D:\\projects\\Fire_AK_reburn\\data\\"
ws = os.path.join(arcpy.env.workspace, "temp")

# Local variables
buffer_size = -500  #negative for inward buffer
reburn_num = 2  # reburn number of interest (will look for this and n-1)
filename_add = "_JUNKTESTiter2"  # tag for filename
out_shp_name = "reburns_x" + str(reburn_num) + "_core_areas_" + str(abs(buffer_size)) + "mbuffer" + filename_add + ".shp"
# ********************************************************

pt_file = "processed_x.shp"
original_polys = "firePerimeters_1940_2016_gt1000ac_notPrescribed_copy.shp"

# some commands require layers, not fcs
arcpy.MakeFeatureLayer_management(pt_file, "pt_lyr")
arcpy.MakeFeatureLayer_management(original_polys, "orig_polys_lyr")

# set up initial filter
whereClause = '"burn_num" < ' + str(reburn_num + 1) + ' AND "acres" > 5 AND "FID" < 50'
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

            # get the PRIOR burn info for the same polygon
            sql_older_burn = '"polyid" = ' + str(poly_id) + ' AND "burn_num" = ' + str(reburn_num - 1)

            # initialize cursor to find id of the original burn
            cursor2 = arcpy.SearchCursor("pt_lyr", sql_older_burn)

            # get both current and previous burn polys
            for row2 in cursor2:
                older_parent_id = row2.getValue(parent_id_field)  # get the older fire id
                sql_newer_poly = '"parentid" = ' + str(newer_parent_id)
                sql_older_poly = '"parentid" = ' + str(older_parent_id)
                sql_both_polys = '"parentid" = ' + str(older_parent_id) + ' OR "parentid" = ' + str(newer_parent_id)  # sql to get both newer, older burn polys
                print '----------------------------------'
                print 'Processing fire FIDs ' + str(older_parent_id) + ' and ' + str(newer_parent_id)

                # Older fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_older_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/older_fc")

                # Newer fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_newer_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/newer_fc")

            # Intersect older fire and newer fire to get the overlapping sections (reburn)
                intsct_poly = os.path.join(ws, "reburn_poly" + str(n_poly) + ".shp")
                arcpy.Intersect_analysis(["in_memory/older_fc", "in_memory/newer_fc"], "in_memory/reburn_fc", "ALL")

            # Calculate area of reburn within prior fire, beyond buffer zone
                # Buffer prior fire poly
                arcpy.Buffer_analysis("in_memory/older_fc", "in_memory/older_buffd", buffer_size, "FULL", "#", "NONE")

                # Intersect buffered prior fire with reburn poly
                reburn_buff_poly = os.path.join(ws, "reburn_beyond_buffer" + str(n_poly) + ".shp")
                arcpy.Intersect_analysis(["in_memory/older_buffd", "in_memory/reburn_fc"], reburn_buff_poly)

                # Add buffered area shapefile to list
                fcs_list.append(os.path.join(ws, reburn_buff_poly))

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

# Merge shapes
print 'Merging all shapefiles...'

# Populate list with 1st element
sample_fcs = fcs_list[0]
arcpy.CopyFeatures_management(sample_fcs, os.path.join(ws, out_shp_name))

# Iterate through all but 1st list element (already completed)
iterfcs = iter(fcs_list)
next(iterfcs)
for fcs in iterfcs:
    arcpy.Append_management([fcs], os.path.join(ws, out_shp_name), "NO_TEST")

# Clean up
print 'Deleting files...'
arcpy.Delete_management('in_memory')

for fc in fcs_list:
    arcpy.Delete_management(fc)

print 'Done! Files written to: '
print os.path.join(ws, out_shp_name)

ts1 = time.time()
print 'Time elapsed: ' + str(ts1 - ts0) + ' seconds'



