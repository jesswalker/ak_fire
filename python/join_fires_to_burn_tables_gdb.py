# ---------------------------------------------------------------------------
# join_fires_to_burn_tables.py
#
# Created on: 2017-05-12
#
# Description:
# This script retrieves the length of the shared perimeter of a reburn polygon
# and the original burn polygon.  It also identifies (via intersect) the area
# of the reburn polygon beyond a specified boundary buffer along the original
# burn polygon.

# Necessary input files:

# processed_x.shp

# Shapefile created from the output of process_alaska_burn_data_w_ecoregions.R,
# which assigns burn numbers and fire intervals to the individual (i.e.,
# non-overlapping) polygons created from the AICC database. The file contains
# a point feature for each polygon burn, resulting in muliple entries for each
# reburned polygon.
# I.e.,
#
# id ptid polyid  acres lat    lon     date       parentid  parentac  parentname
#  ...
# 10   3  11   12357.6	57.02 -154.32  11/26/1950  660	33808.2	   Olga Bay Fire
# 11   4  11   12357.6	57.02 -154.32  4/16/1997  1040	14852.9	   Moser Bay

# ------------------
# firePerimeters_1940_2016_individual_polys_copy.shp -
# Non-overlapping shapefile of fire polygons

# FID Shape  acres   polyid
# ...
# 11 Polygon 12357.6  11

# -----------------
# firePerimeters_1940_2016_gt1000ac_notPrescribed_copy.shp -
# Filtered file of original (overlapping) fire polygons

# FID Shape    FireName     FireYear Acres   parentid
# ...
# 660 Polygon  Olga Bay Fire 1950 33803.2     660

# ---------------------------
#
# Overall process:
# Choose a polygon that has burned multiple times and find its earlier burn info
# in processed_x; link via parentID to the original fire; link via polyid to the
# individual polygon; intersect to get area, shared perimeter.

# -----------------------------------------------------------------------

# Import arcpy module
import arcpy, os, time
from arcpy import env

# Set to overwrite
arcpy.env.overwriteOutput = True
arcpy.env.workspace = "D:\\projects\\Fire_AK_reburn\\data\\output.gdb"
ws = os.path.join(arcpy.env.workspace, "temp")

# Local variables
buffer_size = -100  #negative for inward buffer
pt_file = "processed_x"
ind_polys = "firePerimeters_1940_2016_individual_polys_copy"
original_polys = "firePerimeters_1940_2016_gt1000ac_notPrescribed_copy"

# some commands require layers, not fcs
arcpy.MakeFeatureLayer_management(pt_file, "pt_lyr")
arcpy.MakeFeatureLayer_management(ind_polys, "ind_polys_lyr")
arcpy.MakeFeatureLayer_management(original_polys, "orig_polys_lyr")

# set up initial filter
whereClause = '"burn_num" < 3 AND "acres" > 5 AND "FID" < 1000'
arcpy.SelectLayerByAttribute_management("pt_lyr", "NEW_SELECTION", whereClause)

# iterate through all polygons in the shapefile
burn_num = "burn_num"
poly_id_field = "polyid"
parent_id_field = "parentid"

# can't put the where clause here b/c we want the subset of burns < burn_num
cursor = arcpy.SearchCursor("pt_lyr")

# initialize lists to hold consolidated feature classes/tables
fcs = []
tbs = []

# initialize counter
n_poly = 0

# start the clock
ts0 = time.time()

# step through all rows in the POINTS layer
for row in cursor:
    try:
        if row.getValue(burn_num) == 2:

            # track number of processed polygons
            n_poly = n_poly + 1

            # get the polygon id of a burn
            poly_id = row.getValue(poly_id_field)

            # get the ORIGINAL burn info for the same polygon
            sql_prior_burn = '"polyid" = ' + str(poly_id) + ' AND "burn_num" = 1'

            # initialize new cursor to find id of the original burn
            cursor2 = arcpy.SearchCursor("pt_lyr", sql_prior_burn)

            # kludgy. Is there another way to do this? cursor2 should be unique, no
            # need to step through multiple rows
            for row2 in cursor2:
                parent_id = row2.getValue(parent_id_field) # get the original fire id
                sql_orig_poly = '"parentid" = ' + str(parent_id) # id for non-overlapping (original) poly
                sql_ind_poly = '"polyid" = ' + str(poly_id)  # id for overlapping (individual) poly

                # info
                print '----------------------------------'
                print 'Processing original fire FID ' + str(parent_id)

                # Get the correct original and individual fire polys
                arcpy.SelectLayerByAttribute_management("ind_polys_lyr", "NEW_SELECTION", sql_ind_poly)
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_orig_poly)

        # Calculate acres in buffer from original fire perimeter.
                # Buffer parent poly.  Need to convert lyr back to fc first
                print 'Buffering original poly...'
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/orig_fc")
                arcpy.Buffer_analysis("in_memory/orig_fc", "in_memory/orig_buff", buffer_size, "FULL", "#", "NONE")

                # Intersect the buffered original poly with the individual (reburn) poly
                print 'Intersecting original poly with reburn poly...'
                buff_reburn = "reburn" + str(n_poly)
                arcpy.Intersect_analysis(["in_memory/orig_buff", "ind_polys_lyr"], buff_reburn)

                # Add calculated acres:

                # 'acres' field is already populated with individual polygon acres
                # Create new field for buffered acreage
                # Calculate area geometry
                # Create new field for within-buffer acreage
                # Populate with acres - buffered acreage

        # Calculate length of shared perimeter

                # Control fields
                print 'Mapping fields...'
                fieldMappings = arcpy.FieldMappings()
                fieldMappings.addTable("orig_polys_lyr")
                fieldMappings.addTable("ind_polys_lyr")

                # Name the fields to retain from both tables
                fields_to_keep = ["parentid", "acres", "polyid"]

                # Remove all other fields
                for field in fieldMappings.fields:
                    if field.name not in fields_to_keep:
                        fieldMappings.removeFieldMap(fieldMappings.findFieldMapIndex(field.name))

                # Merge original fire poly with reburn poly. Cannot calculate shared perimeter
                # if they're in separate feature classes
                print 'Merging unbuffered original with reburn...'
                merged_polys = "in_memory/merged_polys"
                arcpy.Merge_management(["orig_polys_lyr", "ind_polys_lyr"], merged_polys, fieldMappings)

                # Get length of shared boundary between original fire and individual poly
                print 'Getting shared boundary length...'
                shared_table = "table" + str(n_poly)
                arcpy.PolygonNeighbors_analysis(merged_polys, shared_table, ['polyid', 'acres', 'parentid'], "NO_AREA_OVERLAP", "NO_BOTH_SIDES", "#", "METERS")

               # add the output polygon/table to the list
                print 'Appending files...'
                fcs.append(buff_reburn)
                tbs.append(shared_table)

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
print 'Merging all polygons...'
arcpy.Merge_management(fcs, "allpolys")

print 'Merging all tables...'
arcpy.Merge_management(tbs, "alltables")

# Clean up
print 'Deleting files...'
arcpy.Delete_management('in_memory')
for fc in fcs:
    arcpy.Delete_management(fc)

for tb in tbs:
    arcpy.Delete_management(tb)

print 'Done!'
ts1 = time.time()
print 'Time elapsed is: ' + str(ts1 - ts0)

# FID < 1000 in 355x


