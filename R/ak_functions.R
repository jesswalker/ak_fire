########################################################################### #
#
# Helper functions for AK reburn project scripts
#
########################################################################### #

# --------------------- assign decade id -------------------

# This one doesn't seem like it should work correctly, but setting the breaks this way forces it to
# do the right thing: 1950 is part of the 1955 decade (1950 - 1959), etc. Seems wrong, acts right.

setDecade <- function(df) {
  # Assign year to decade 
  df$decade <- cut(df$year, breaks = c(1940, 1949, 1959, 1969, 1979, 1989, 1999, 2009, 2019), 
                   include.lowest = T, labels = c(1945, 1955, 1965, 1975, 1985, 1995, 2005, 2015))
  return(df)
}


# ------------- getReburnRanks -------------------------

# Low-level initial processing:
# 1. Gets the rank of duplicates by year (1st fire = rank 1, 2nd = 2, etc.)
# 2. Gets time between burns in years
# 3. Gets time between burns in julian days
# 4. Orders fires by size for each year, burn status (initial/reburn)

getReburnRanks <- function(df) {
 
  # Make sure files are ordered by year
  df <- df[order(df$date),]
  
  # Get the ranks of the duplicates (i.e., reburns) by year in which they burned
  # i.e., each reburned area is ranked by time into 1, 2, 3, etc.
  df$polyid <- as.factor(df$polyid)
  df$burn_num <- ave(df$year, df$polyid, FUN=rank)
  
  # set reburn to no (first fire; 0) or yes (reburn; 1)
  df$reburn <- 0
  df[which(as.numeric(df$burn_num) > 1), ]$reburn <- 1
  
  # get time between burns in years
  df$year_int <- ave(df$year, factor(df$polyid), FUN=function(y) c(NA, diff(y)))
  
  # get time between burns in julian days
 # df$julian_int <- ave(df$julian, factor(df$polyid), FUN=function(y) c(NA, diff(y)))
  
  # order the fires by size.  Rank(-x) means they're ranked from largest to smallest
  df$size_rank <- ave(df$acres, df$year, df$reburn, FUN=function(y) rank(-y))
  
  return(df)
}


# ------------------- lm_eqn -------------------------------------

# Compute and format regression equation for output on plot

lm_eqn <- function(df, Y, X){
  f <- paste(Y, "~", X); # this is so the x and y cols can be passed in
  m <- do.call("lm", list(as.formula(f), data=df))
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~c, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2),
                        c = format(summary(m)$coefficients[2,4], digits=2),
                        r2 = format(summary(m)$r.squared, digits = 3)));
  as.character(as.expression(eq));                 
}




# -------------------- assign ecoregion -------------------------

assignEcoreg <- function(df) {
  df$ecoreg <- names(df[c("tundra", "maritime", "boreal")])[max.col(df[c("tundra", "maritime", "boreal")])]
  return(df)
}



# ----------------- pad annual time series  ----------------------------------------

padTimeSeries <- function(df, startYr, endYr) {
  
  # Create sequence of years
  seq_years <- zoo(, seq(startYr, endYr, by = 1))
  
  # Convert df to zoo object. Drop the 'year' column as that will be the zoo ordering index
  drops <- c("year")
  df.zoo <- zoo(df[ , !(names(df) %in% drops)], order.by = df$year)
  
  # Merge the original files with year sequence
  df.zoo.merge <- merge(df.zoo, seq_years, all = T)
  
  # Convert back to time series
  df.ts <- data.frame(date = time(df.zoo.merge), 
                        data = df.zoo.merge, 
                        check.names = F, 
                        row.names = NULL)
  
  colnames(df.ts) <- c("year", colnames(df[!(names(df) %in% drops)]))
  
  return(df.ts)
}