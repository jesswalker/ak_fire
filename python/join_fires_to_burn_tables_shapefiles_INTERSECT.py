# ---------------------------------------------------------------------------
# join_fires_to_burn_tables_shapefiles.py
#
# Created on: 2017-05-12
#
# Description:
# This script intersects pairs of overlapping polygons: burns and prior burns.
# Given a file of multiple overlapping polygons, Arc has no functionality to
# intersect unique pairs; "intersect" takes the intersection of all spatial
# neighbors.  This script steps through and takes the pairwise intersection.


# Input files:

# 1.  processed_x.shp

# Shapefile created from the output of process_alaska_burn_data_w_ecoregions.R,
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
#  Output:
#  D:\\projects\\Fire_AK_reburn\\data\\temp
#  1. intersectAreas_.shp
#
# Process:
# - Choose a polygon that has burned multiple times (file: processed_x)
# - Get parentID of current whole fire (processed_x)
# - Use polyID and previous burn# to get previous burn parentID (processed_x)
# - Intersect current and previous fires


# Notes:
# This script differs in that it relies on shapefiles
# Currently only uses 2nd generation burns; i.e., the 1st reburn of an area
# is linked to the underlying original burn. Edited 6/7/2017
#
#
# Note that the file produced has 2 entries for each identified intersection:
# one for each "parent" fire.  Requires post-processing in R to calculate
# the fire interval and remove the earlier fire.  Completed in R script
# intersected_polys.R
# -----------------------------------------------------------------------

# Import arcpy module
import arcpy, os, time
from arcpy import env

# Set to overwrite
arcpy.env.overwriteOutput = True
arcpy.env.workspace = "D:\\projects\\Fire_AK_reburn\\data\\"
ws = os.path.join(arcpy.env.workspace, "temp")

# ********  INPUT REQUIRED HERE **************************
# Local variables
reburn_num = 2  # reburn number of interest (will look for this and n-1)
filename_add = "test3"
# ********************************************************

pt_file = "processed_x.shp"
original_polys = "firePerimeters_1940_2016_gt1000ac_notPrescribed_copy.shp"

# some commands require layers, not fcs
arcpy.MakeFeatureLayer_management(pt_file, "pt_lyr")
arcpy.MakeFeatureLayer_management(original_polys, "orig_polys_lyr")

# set up initial filter
whereClause = '"burn_num" < ' + str(reburn_num + 1) + ' AND "acres" > 5'  #FID
arcpy.SelectLayerByAttribute_management("pt_lyr", "NEW_SELECTION", whereClause)

# iterate through all polygons in the shapefile
burn_num = "burn_num"
poly_id_field = "polyid"
parent_id_field = "parentid"

# we want the subset of burns < burn_num
cursor = arcpy.SearchCursor("pt_lyr")

# initialize lists to hold consolidated feature classes/tables
fcs_list = []
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
            curr_parent_id = row.getValue(parent_id_field)

            # get the PRIOR burn info for the same polygon
            sql_prior_burn = '"polyid" = ' + str(poly_id) + ' AND "burn_num" = ' + str(reburn_num - 1)

            # initialize new cursor to find id of the original burn
            cursor2 = arcpy.SearchCursor("pt_lyr", sql_prior_burn)

            # get both current and previous burn polys.
            # (note: kludgy. Is there another way to do this? cursor2 should be unique, no
            # need to step through multiple rows)
            for row2 in cursor2:
                prior_parent_id = row2.getValue(parent_id_field)  # get the prior fire id
                sql_polys = '"parentid" = ' + str(prior_parent_id) + ' OR "parentid" = ' + str(curr_parent_id)  # sql to get both current, previous burns polys
                print '----------------------------------'
                print 'Processing fire FIDs ' + str(prior_parent_id) + ' and ' + str(curr_parent_id)

                # Get the current and previous burn polys
                arcpy.SelectLayerByAttribute_management("orig_polys_lyr", "NEW_SELECTION", sql_polys)

            # Intersect current and previous burns
                print 'Intersecting polygons...'
                arcpy.CopyFeatures_management("orig_polys_lyr", "in_memory/orig_fc")
                reburn_poly = os.path.join(ws, "poly" + str(n_poly) + ".shp")
                arcpy.Intersect_analysis("in_memory/orig_fc", reburn_poly)

               # add the output polygon/table to the list
                print 'Appending files...'
                fcs_list.append(os.path.join(ws, reburn_poly))

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

# set up shapefile to write to. Append to the 1st one in the list
# kludgy but works.  Note that the first element in each output
# will be duplicated.

# Merge shapes
sample_fcs = fcs_list[0]
out_shp_name = "intersect_reburnNEW2" + str(reburn_num) + filename_add + ".shp"
arcpy.CopyFeatures_management(sample_fcs, os.path.join(ws, out_shp_name))

for fcs in fcs_list:
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



