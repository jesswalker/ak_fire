# ---------------------------------------------------------------------------
# shared_edges_TABLE.py
#
# Created on: 2017-11-08 JJWalker
#
# Description:
# This script analyzes all burn/reburn pairs to retrieve the length of
# perimeter shared between a reburn and the older fire; i.e., the length
# of border of the original fire "within" the more recent fire polygon.
# Output is a table of polyline lenghts (in meters).

# This script differs from earlier versions in that it intersects the previous
# fire and current fire to get *all* intersection polygons.  Earlier versions
# intersected each individual reburn polygon. This script has been superseded
# by shared_edges_SHAPEFILE.py, which simply retrieves the entire shapefile of
# the shared line.

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
#  Output:   D:\\projects\\Fire_AK_reburn\\data\\temp\\
#
#
# Process:
# - Choose a polygon that has burned multiple times (file: processed_x)
# - Get parentID of parent (current) whole fire (processed_x)
# - Use polyID and previous fire# to get previous fire parentID (processed_x)
# - Intersect current and previous fires to get all common areas
# - PolygonNeighbors command on reburn polygon and previous fire to get perimeter
# - Write table to dbf
#
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
arcpy.env.workspace = "D:\\projects\\Fire_AK_reburn\\data\\"
ws = os.path.join(arcpy.env.workspace, "temp")

# Local variables
reburn_num = 2  # reburn number of interest (will look for this and n-1)
filename_add = "_JUNKTEST"  # tag for filename
out_tbl_name = "reburns_x" + str(reburn_num) + "_shared_edges" + filename_add + ".dbf"
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
tbl_list = []

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
                sql_both_polys = '"parentid" = ' + str(older_parent_id) + ' OR "parentid" = ' + str(newer_parent_id)  # sql to get both current, previous burns polys
                print '----------------------------------'
                print 'Processing fire FIDs ' + str(older_parent_id) + ' and ' + str(newer_parent_id)

                # Prior fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_older_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/older_fc")

                # Reburn fire
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_newer_poly)
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/newer_fc")

            # Intersect prior fire and reburn fire to get the overlapping sections (reburn areas)
                intsct_poly = os.path.join(ws, "reburn_poly" + str(n_poly) + ".shp")
                arcpy.Intersect_analysis(["in_memory/older_fc", "in_memory/newer_fc"], "in_memory/reburn_fc", "ALL")

            # Calculate length of shared perimeter between reburn and prior burn
            #   = portion of the prior burn perimeter within the later burn
                # Control fields
                fieldMappings = arcpy.FieldMappings()
                fieldMappings.addTable("in_memory/older_fc")
                fieldMappings.addTable("in_memory/reburn_fc")

                # Name the fields to retain from both tables
                fields_to_keep = ['FireName', 'FireName_1', 'parentid', 'acres', 'parentid_1', 'acres_1']

                # Remove all other fields
                for field in fieldMappings.fields:
                    if field.name not in fields_to_keep:
                        fieldMappings.removeFieldMap(fieldMappings.findFieldMapIndex(field.name))

                # Merge original fire poly with reburn poly; cannot calculate shared perimeter if they're in separate feature classes
                #merged_polys = os.path.join(ws, "merged_polys" + str(n_poly) + ".shp") #"in_memory/merged_polys"
                arcpy.Merge_management(["in_memory/older_fc", "in_memory/reburn_fc"], "in_memory/merged_polys", fieldMappings)

                # Get length of shared boundary between original fire and reburn area
                shared_table = os.path.join(ws, "table" + str(n_poly) + ".dbf")
                arcpy.PolygonNeighbors_analysis("in_memory/merged_polys", shared_table, ['FireName', 'FireName_1', 'acres', 'acres_1', 'parentid', 'parentid_1'], "NO_AREA_OVERLAP", "NO_BOTH_SIDES", "#", "METERS")

               # Add boundary length table to list
                tbl_list.append(os.path.join(ws, shared_table))

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
print 'Merging all tables...'

sample_tbl = tbl_list[0]
arcpy.CreateTable_management(ws, out_tbl_name, sample_tbl)

for tbl in tbl_list:
    arcpy.Append_management([tbl], os.path.join(ws, out_tbl_name), "NO_TEST")

# Clean up
print 'Deleting files...'
arcpy.Delete_management('in_memory')

for tbl in tbl_list:
    arcpy.Delete_management(tbl)

print 'Done! Files written to: '
print os.path.join(ws, out_tbl_name)

ts1 = time.time()
print 'Time elapsed: ' + str(ts1 - ts0) + ' seconds'



