# This script processes the shared perimeter and core areas files to retrieve 
# basic landscape metrics. 

setwd("D:/projects/Fire_AK_Reburn/tables")
library(plyr)

edge2 <- read.csv("individual_polys_shared_perimeter_burn_num2b_edit.csv", header = T)
edge3 <- read.csv("individual_polys_shared_perimeter_burn_num3b_edit.csv", header = T)
edge4 <- read.csv("individual_polys_shared_perimeter_burn_num4b_edit.csv", header = T)

# to find equal area circle (EAC)
# area circle = pi*r2
# area polygon = pi*r2
# >> circle w radius r = sqrt(area polygon/pi)
# 


# Get the table with the info for the buffer acreage and perimeter length
# These have been edited in Arc to include buffer ac and perimeter in arc

# Get the table with the info for the buffer acreage and perimeter
poly2 <- read.csv("coreAreas_200mbuffer_reburn2.csv", header = T)
poly3 <- read.csv("coreAreas_200mbuffer_reburn3.csv", header = T)
poly4 <- read.csv("coreAreas_200mbuffer_reburn4.csv", header = T)


# Process to break into categories of years between fires
process_buffer_files = function(df, z, burnNum) {

  # set up edge file
  df$area_cir <- (df$perim_m^2)/(4*pi)
  df$area_m2 <- df$acres * 4046.86
  df$para <- df$perim_m/df$area_m2
  df$p2a <- df$perim_m^2/df$area_m2
  df$edge2perim <- df$edge_m/df$perim_m
  df$edge2area <- df$edge_m/df$area_m2
  df$shape <- df$perim_m/(2*sqrt(pi*df$area_m2)) #Schumaker 1996
  df$shape2 <- sqrt(df$area_m2/df$area_cir)
  df$r <- sqrt(df$area_m2/pi)
  df$eac_perim <- 2*pi*df$r
  df$frac <- 2*log(df$perim_m)/log(df$area_m2) # log in R = ln
  
  #set up perimeter file
  z$CalcAcres <- NULL
  z$DiscDate <- NULL
  z$FireName <- NULL
  z$parentid <- NULL
  z$FireYear <- NULL # this is the date of first fire
  
  keep_cols <- c("polyid", "core_ac", "core_per")
  z <- z[,keep_cols]
  df.merge <- merge(df, z)
  df.merge2 <- df.merge

#colnames(df.merge2)[which(colnames(df.merge2) == "buff_ac")] <- "core_ac"
#colnames(df.merge2)[which(colnames(df.merge2) == "buffper_m")] <- "core_per"

# acres >> m2
  df.merge2$core_m2 <- df.merge2$core_ac * 4046.86
  df.merge2$core_prop <- df.merge2$core_m2/df.merge2$area_m2

# Subset based on edge length and core acreage
  df.merge.sub <- subset(df.merge2, df.merge2$edge_m > 100 & df.merge2$core_ac > 5)

  df.bor <- subset(df.merge.sub, ecoreg1 == "boreal")
  df.tun <- subset(df.merge.sub, ecoreg1 == "tundra")
  df.mar <- subset(df.merge.sub, ecoreg1 == "maritime")

  keep_cols <- c('polyid', 'year_int', 'year','edge_m', 'area_m2', 'para', 'p2a', 'edge2perim', 'edge2area', 'frac', 'shape', 'shape2',
               'core_m2', 'core_prop')

# subset again to boreal, tundra, maritime, even though only boreal is worth anything
  df.bor.sub <- df.bor[, keep_cols]
  df.tun.sub <- df.tun[, keep_cols]
  df.mar.sub <- df.mar[, keep_cols]


# group data by years between fires (year_int) by 10 years, then convert back to df
  chopped <- split(df.bor.sub, cut(df.bor.sub$year_int, seq(1, 75, by = 10), right = F, labels = F))
  tt <- ldply(chopped, data.frame)
  colnames(tt)[1] <- "int_10"

# group data by year_int: 15 years
  test <- split(tt, cut(tt$year_int, seq(1, 90, by = 15), right = F, labels = F))
  t2 <- ldply(test, data.frame)
  colnames(t2)[1] <- "int_15"

# group data by year_int: 20 years. For some reason the last interval kept getting dropped
  test2 <- split(t2, cut(t2$year_int, seq(1, 90, by = 20), right = F, labels = F))
  t3 <- ldply(test2, data.frame)
  colnames(t3)[1] <- "int_20"
  
# break data into pre, post-2004 sets
  t3$pre_post2004 <- 0
  t3[which(t3$year >= 2004),]$pre_post2004 <- 1
  t3$pre_post2004 <- as.factor(t3$pre_post2004)

  t3$int_10 <- as.factor(t3$int_10)
  t3$int_15 <- as.factor(t3$int_15)
  t3$int_20 <- as.factor(t3$int_20)
  
# prep for eventual burn number
  t3$burn_num <- burnNum

  return(t3)
}

df.burn2 <- process_buffer_files(edge2, poly2, 2)
df.burn3 <- process_buffer_files(edge3, poly3, 3)
df.burn4 <- process_buffer_files(edge4, poly4, 4)

df.burns <- rbind(df.burn2, df.burn3, df.burn4)
df.burns$burn_num <- as.factor(df.burns$burn_num)
## plots
#   
labels_15 <- c("1-15", "16-30", "31-45", "46-60", "60-75")
labels_20 <- c("1-20", "21-40", "41-60", "61-80")




#### plots of all intervals, all fires- ------------------
# core proportion
ggplot(df.burn2, aes(as.factor(int_20), core_prop)) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("Core Proportion") +
  ggtitle("Core proportion of reburn area to interval between reburns") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = labels_20)

# edge2perimeter
ggplot(df_burn2, aes(as.factor(int_20), edge2perim)) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("Edge:perimeter") +
  ggtitle("Shape index (edge to perimeter) to interval between reburns\nBoreal ecoregion") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = labels_20)

# area_m2
ggplot(df_burn2, aes(as.factor(int_20), log10(area_m2))) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("Log(area (m2))") +
  ggtitle("Shape index (area) to interval between reburns\nBoreal ecoregion") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = labels_20)

# p2a
ggplot(df_burn2, aes(as.factor(int_20), log10(p2a))) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("Perimeter^2:area") +
  ggtitle("Shape index (p^2:area) to interval between reburns\nBoreal ecoregion") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = labels_20)


# frac
ggplot(df_burn2, aes(as.factor(int_20), frac)) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("frac") +
  ggtitle("Shape index (frac) to interval between reburns\nBoreal ecoregion") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = c("1-20", "21-40", "41-60", "61-80"))

# para
ggplot(df_burn2, aes(as.factor(int_20), log10(para))) +
  geom_boxplot(fill = '#A4A4A4') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Years between reburns") +
  ylab("log(para)") +
  ggtitle("Shape index (para) to interval between reburns\nBoreal ecoregion") +
  scale_x_discrete(breaks = c(1,2,3,4), labels = c("1-20", "21-40", "41-60", "61-80"))




plot_metric <- function(df, interval, metric, title, labels, fill) {

  legend.title <- "Time period"
  p <- ggplot(df, aes_string(interval, metric, fill = fill)) + 
       geom_boxplot() + 
       theme_bw() +
       ggtitle(title) +
       theme(plot.title = element_text(hjust = 0.5)) +
       xlab("Years between reburns") +
       scale_x_discrete(labels = labels) +
       scale_fill_manual(legend.title, values=c("gray","lightsalmon"), labels = c("1957-2003", "2004-2016"))
}

p <- plot_metric(subset(df.burns, burn_num == 4), "int_20", "log10(para)", "Index (para) to years between reburns, by time period\nBoreal ecoregion",
                 labels_15, "pre_post2004")

p

p <- plot_metric("shape", "Index (shape) to years between reburns, by time period\nBoreal ecoregion",
                 labels_15)

p <- plot_metric(df.burns, "int_20", "shape2", "Index (shape2) to years between reburns, by time period\nBoreal ecoregion",
                 labels_20, "pre_post2004")

p 
p <- plot_metric(df.burns, "int_20", "frac", "Index (frac) to interval years reburns, by time period\nBoreal ecoregion", 
                 labels_15, "pre_post2004")
p

p <- plot_metric("edge2perim", "Index (edge2perim) to years between reburns, by time period\nBoreal ecoregion",
                 labels_15)

p <- plot_metric("edge2area", "Index (edge2area) to years between reburns, by time period\nBoreal ecoregion",
                 labels_15)

p <- plot_metric("core_prop", 
                 "Index (proportion of core to overall reburn area) to years between reburns, by time period\nBoreal ecoregion",
                 labels_15)

p <- plot_metric("log10(area_m2)", 
                 "Reburn area to years between reburns, by time period\nBoreal ecoregion",
                 labels_15)

legend.title <- "Time period"

# trend lines
ggplot(df.burns, aes(year_int, shape, group = pre_post2004, color = pre_post2004)) + 
  geom_point() + 
  stat_smooth(method = lm) + 
  theme_bw() + 
  scale_color_manual(legend.title, values=c("darkgray","lightsalmon"), labels = c("1957-2003", "2004-2016"))


ggsave(filename = "para_vs_FRIs.png",
       path = "D:/projects/fire_AK_reburn/plots/", width = 10, height = 7, units = c("in"),
       dpi = 600)


load("D:/projects/Fire_AK_reburn/R/reburn_perimeters_and_core_areas.RData")


save.image(file = "D:/projects/Fire_AK_reburn/R/reburn_perimeters_and_core_areas.RData")
