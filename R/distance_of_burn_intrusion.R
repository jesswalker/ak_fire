# Processing of near_analysis table
# 
# Input:  reburns_x2_near_analysis_all_dissolved.csv
#         firePerimeters_1940_2016_gt1000ac_notPrescribed_table.csv
# Output: 


# 1 Dec 2017 JJWalker

# 
library(dplyr)

# fires
fires <- read.csv("data/x_original.csv", header = T)
# input
x <- read.csv("data/reburns_x2_near_analysis_all_dissolved.csv", header = T)

# > head(x)
#      FID pointid   NEAR_DIST   NEAR_X  NEAR_Y NEAR_ANGLE parentid1 parentid2
# 1 360950       1   0.1954738 89856.51 1573780   96.84267         2       877
# 2 360951       2   0.1929139 90106.57 1573780   79.21534         2       877
# 3 360952       3  92.7670997 89062.11 1573611  118.61040         2       877
# 4 360953       4 188.8429913 89334.03 1573718   96.84267         2       877
# 5 360954       5 218.6288633 89580.48 1573747   96.84267         2       877
# 6 360955       6 248.4147354 89826.94 1573777   96.84267         2       877

x$parentid1 <- as.factor(x$parentid1)
x$parentid2 <- as.factor(x$parentid2)

# Get the maximum distance of penetration for each fire ID pair
x.max <- x %>%  
          group_by(parentid1, parentid2) %>%
          summarize(max_dist = max(NEAR_DIST))

# > head(x.max)
# Source: local data frame [6 x 3]
# Groups: parentid1 [3]
# 
#   parentid1 parentid2 max_dist
#       <fctr>    <fctr>    <dbl>
# 1         2       877 3328.921
# 2         2      1817 2582.504
# 3         3       249 2074.284
# 4         6      1909 3398.801
# 5         6      2030 1130.461
# 6         6      2068 2274.626
# 


merge1 <- merge(x.max, fires.sub, by.x = "parentid1", by.y = "parentid")
head(merge1)
colnames(merge1) <- c("parentid1", "parentid2", "max_dist", "year1", "name1", "doy1", "ac1", "ecoreg1")
merge2 <- merge(merge1, fires.sub, by.x = "parentid2", by.y = "parentid")
colnames(merge2) <- c("parentid2", "parentid1", "max_dist", "year1", "name1", "doy1", "ac1", 
                      "ecoreg1", "year2", "name2", "doy2","ac2", "ecoreg2")

# Take out the smaller ones--could be due to poor border delineation
merge2.gt100m <- subset(merge2, max_dist > 100)

## max_dist and ac1 are correlated

test <- lm(max_dist ~ int_yr + ac1, data = subset(merge2.gt100m, ecoreg1 == "boreal"))
summary(test)
# 
# Call:
#   lm(formula = max_dist ~ int_yr + ac1, data = subset(merge2.gt100m, 
#                                                       ecoreg1 == "boreal"))
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9864.7 -1218.0  -540.1   628.8 21123.8 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.136e+03  1.128e+02  10.075  < 2e-16 ***
#   int_yr      2.530e+01  3.858e+00   6.557 7.61e-11 ***
#   ac1         5.788e-03  3.358e-04  17.237  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2462 on 1442 degrees of freedom
# Multiple R-squared:  0.2205,	Adjusted R-squared:  0.2194 
# F-statistic: 203.9 on 2 and 1442 DF,  p-value: < 2.2e-16
