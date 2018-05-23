path.in = "D:/projects/ak_fire/data/tables/gee_output"
filenames.in <- list.files(path.in, pattern = "*.csv")

x <- do.call(rbind, lapply(file.path(path.in, filenames.in), read.csv, header=T))

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

# drop 1st row, which is blank (1 exists for each file)
x <- x[-1, ]

# save file
filename.out = "mxd14a1_ak_GEE_all.csv"
write.csv(x, file = file.path(path.in, filename.out), row.names = FALSE)

