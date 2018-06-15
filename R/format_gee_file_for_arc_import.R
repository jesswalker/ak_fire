########################################################################### #
# 
# format_gee_file_for_arc_import.R
#
# This helper module imports the csv files that result from running the script
# AK_FRP in GEE. It reformats column headings so they can be read in Arc; 
# output files can be then processed via the python script 
# intersect_frp_data_with_fire_perimeters.py
#
# Rbinding into a single file creates a world of trouble for Arc, hence the 
# multiple-file output.
#
# Input: D:/projects/ak_fire/data/tables/gee_output + files of mxd14a1_ak_GEE_yyyy.csv
# Output: D:/projects/ak_fire/data/tables/gee_output + files of mxd14a1_ak_GEE_yyyy_processed.csv
#
########################################################################### #


# Set path info
path.in = "D:/projects/ak_fire/data/tables/gee_output"
filenames.in <- list.files(path.in, pattern = "*.csv")

# Process each file
for (filename.in in filenames.in) {
  
  x <- read.csv(file.path(path.in, filename.in), header = T)
  filename.sub <- substr(basename(filename.in), 1, nchar(basename(filename.in))-4)
  filename.out <- paste0(filename.sub, "_processed.csv")

# rename first column for convenience
  colnames(x)[1] <- "index"
 
# R slaps an "X" in front of numerical columns; switch it to "class"
  names.sub <- names(x)[which((substring(names(x), 1, 1) == "X"))]
  names(x)[which((substring(names(x), 1,1) == "X"))] <- paste0("class", substring(names.sub, 2))

# drop .geo column
if (".geo" %in% names(x)) {
  x$.geo <- NULL
}

# drop histogram column
if ("histogram" %in% names(x)) {
  x$histogram <- NULL
}

# drop label column
if ("label" %in% names(x)) {
  x$label <- NULL
}

# drop date column
if ("date" %in% names(x)) {
  x$date <- NULL
}

# drop lonID column
if ("lonID" %in% names(x)) {
  x$lonID <- NULL
}

# drop 1st row, which is blank (1 exists for each file)
x <- x[-1, ]

# save output
write.csv(x, file = file.path(path.in, filename.out), row.names = FALSE)
}

