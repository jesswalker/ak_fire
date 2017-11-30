# plot_CAN_burn_data.R
#
# This script ingests the file created via process_cAN_burn_data.R.
# August 2016
# JWalker



# Remove existing data and values
 rm(list=ls())

# Load libraries
library(ggplot2)
library(lubridate)

# Set input
path.in <- "D:/projects/Fire_AK_reburn/tables"
path.out <- "D:/projects/Fire_AK_reburn/R"
path.plots <- "D:/projects/fire_AK_reburn/plots"
rdata <- "CAN_reburn_files.RData"

# Load function file
source(file.path(path.out, "ak_functions.R"))

# Load associated R dataframe file if it exists

load(file = file.path(path.out, rdata))

# if (exists(file.path(path.out, rdata))) {
#   
#   print(paste0("Loading data frame ", rdata))
#   load(file = file.path(path.out, rdata))
#   
# } else {
#   
#   message("Run 'process_burn_data.R' to produce input data")
# }

# Get starting and ending years for the dataset
yr_start <- 1970 #year(min(x$date))
yr_end <- year(max(x$date))


# ###########
# PLOTS
# ###########

# ------------------------------------
# Bar plot of burn/reburns
# ------------------------------------

# side by side
title <- paste0("Acres burned per year, ", yr_start, " - ", yr_end)
plot.name <- paste0("CAN_Acres burned per year_side by side ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start), aes(x = year, y = sum_ac, fill = factor(reburn))) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  ggtitle(title) +
  xlab("Year") +
  ylab("Acres x1000") +
  scale_fill_discrete("Burn status", labels = c("First burn", "Reburn")) +
  theme(legend.key = element_blank()) + # get rid of boxes around legend items
  theme(plot.title = element_text(hjust = 0.5)) + # center title

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# on top of each other
plot.name <- paste0("CAN_Acres burned per year_top ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start), aes(x = year, y = sum_ac/1000, fill = factor(reburn))) + 
  geom_bar(stat="identity") +
  theme_bw() +
  ggtitle(title) +
  xlab("Year") +
  ylab("Acres (x1000)") +
  xlim(yr_start, yr_end) +
  scale_fill_discrete("Burn status",labels = c("First burn", "Reburn")) +
  theme(legend.key = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Cumulative distribution of area
# -------------------------------------------------------------------

title <- paste0("Cumulative area burned per year, ", yr_start, " - ", yr_end)
plot.name <- paste0("CAN_Cumulative area burned per year ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start), aes(year, cumsum_ac/total_ac, group = reburn)) + 
  geom_point(aes(color=as.factor(reburn))) + 
  geom_line(aes(color = as.factor(reburn))) +
  theme_bw() + 
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  xlab("Year") +
  ylab("Proportion") +
  scale_color_discrete("Burn status",labels = c("First burn", "Reburn")) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------------------------------------------------------- 
# Average size of 5 largest fires/year (burn/reburn)
# -------------------------------------------------------

title <- paste0("Average size of 5 largest fires, ", yr_start, " - ", yr_end,
                "\n Initial burns and reburns")
plot.name <- paste0("CAN_Average size of 5 largest fires ALL ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.top5size, year >= yr_start), aes(year, mean_ac, group = as.factor(reburn))) +
  geom_point(aes(color = as.factor(reburn))) +
  geom_smooth(method = 'lm', se= F, na.rm = T, aes(x=year, y=mean_ac, color = as.factor(reburn))) +
  theme_bw() +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  xlab("Year") +
  ylab("Area (acres)") +
  scale_color_discrete("Burn status", labels = c("First burn", "Reburn")) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------
# Average fire start date (burn/reburn)
# -------------------------------------------------

title <- paste0("Average fire start date, ", yr_start, " - ", yr_end, 
                "\n Initial fires and reburns")
plot.name <- paste0("CAN_Average fire start date ALL ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.all.sum, year >= yr_start), aes(year, mean_start)) + 
  geom_point(shape = 21, size = 2, fill = "red") + 
  geom_errorbar(aes(ymax = mean_start + se_start, ymin = mean_start - se_start), 
                width = 0.31,
                size = 0.2) +
  geom_smooth(method = 'lm', se = FALSE, color = "black", size = 0.4) +
  theme_bw() +
  xlab("Year") +
  ylab("Day of year") +
  ylim(120, 220) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_text(x = yr_start, y = 220, label = lm_eqn(subset(x.all.sum, year >= yr_start), "mean_start", "year"), parse = TRUE, hjust = 0, vjust = 0) +
  annotate("text", x = yr_start, y = 215, label = "Bars represent +/- 1 SE", hjust = 0, vjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------------------------------------------------
# Average fire start date - 1st burns only
# ------------------------------------------------

title <- paste0("Average fire start date, ", yr_start, " - ", yr_end, 
                "\n Initial fires only")
plot.name <- paste0("CAN_Average fire start date INITIAL ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 0), aes(year, mean_start)) + 
  geom_point(shape = 21, size = 2, fill = "red") + 
  geom_errorbar(aes(ymax = mean_start + se_start, ymin = mean_start - se_start), 
                width =0.31,
                size = 0.2) +
  geom_smooth(method = 'lm', se = FALSE, color = "black", size = 0.4) +
  theme_bw() +
  xlab("Year") +
  ylab("Day of year") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_text(x = yr_start + 30, y = 215, label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 0), "mean_start", "year"),
            parse = TRUE, hjust = 0, vjust = 0) +
  annotate("text", x = yr_start + 30, y = 210, label = "Bars represent +/- 1 SE", hjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ---------------------------------------------------
# Average fire start date - reburns only
# ---------------------------------------------------

title <- paste0("Average fire start date, ", yr_start, " - ", yr_end, "\n Reburns only")
plot.name <- paste0("CAN_Average fire start date REBURNS_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 1), aes(year, mean_start)) + 
  geom_point(shape = 21, size = 2, fill = "red") + 
  geom_errorbar(aes(ymax = mean_start + se_start, ymin = mean_start - se_start), 
                width =0.31,
                size = 0.2) +
  geom_smooth(method = 'lm', se = FALSE, color = "black", size = 0.4) +
  theme_bw() +
  ylim(110, 225) +
  xlab("Year") +
  ylab("Day of year of fire start") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_text(x = yr_start, y = 225, label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 1),
                                                  "mean_start", "year"), parse = TRUE, hjust = 0) +
  annotate("text", x = yr_start, y = 220, label = "Bars represent +/- 1 SE", hjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Length of fire START (first burn start date - last burn start dates)
# -------------------------------------------------------------------

title <- paste0("Length of fire season, ", yr_start, " - ", 
                yr_end, "\n First start date to last start date", "\n Canada")
plot.name <- paste0("CAN_Length of fire season_1st to last start dates ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.all.sum, year >= yr_start), aes(year, length_start)) + 
  geom_point(shape = 21, size = 2, fill = "red") + 
  theme_bw() +
  geom_smooth(method = 'lm', se = FALSE, color = "black", size = 0.4) +
  xlab("Year") +
  ylab("Days") +
  ggtitle(title) + 
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_text(x = yr_start + 30, y = 120, label = lm_eqn(subset(x.all.sum, year >= yr_start), "length_start", "year"), 
            parse = TRUE, hjust = 0) +
 
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Fire duration (start date - end date for each fire)
# -------------------------------------------------------------------

# There are no valid end dates for CAN data, so this plot routine doesn't work

title <- paste0("Average fire duration, ", yr_start, " - ", 
                yr_end, "\n Fire start date to end date", "\n Canada")
plot.name <- paste0("CAN_Average fire duration ", yr_start, "_", yr_end, ".png")

# ggplot(x.fire.length, aes(year, mean)) + 
#   geom_point(shape = 21, size = 2, fill = "red") + 
#   geom_errorbar(aes(ymax = mean + sd_length, ymin = mean - sd_length), 
#                 width = 0.32,
#                 size = 0.2) +
#   theme_bw() +
#   geom_smooth(method = 'lm', se = FALSE, color = "black", size = 0.4) +
#   xlab("Year") +
#   ylab("Length (days)") +
#   ggtitle(title) + 
#   theme(plot.title = element_text(hjust = 0.5)) + # center title
#   geom_text(x = 1970, y = 200, label = lm_eqn(x.fire.length, "mean", "year"), parse = TRUE, hjust = 0) +
#   annotate("text", x = 1970, y = 180, label = "Bars represent +/- 1 SD", hjust = 0) +
#   
#   ggsave(filename = plot.name,
#          path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Top 5% of fires by acreage
# -------------------------------------------------------------------

title <- paste0("Average size of the largest 5% fires, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average size of the largest 5 percent fires ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burn.top5, top5 == 1 & year >= yr_start), aes(year, mean_ac/1000, group = as.factor(reburn))) +
  geom_point(aes(color = as.factor(reburn))) +
  geom_smooth(method = 'lm', se= F, na.rm = T, aes(x=year, y = mean_ac/1000, 
                                                   color = as.factor(reburn))) +
  scale_color_discrete("Burn status",labels = c("First burn", "Reburn")) +
  theme_bw() +
  xlab("Year") +
  ylab("Acres x 1000") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title

  geom_text(x = yr_start + 30, y = 85, label = lm_eqn(subset(x.burn.top5, top5 == 1 & reburn == 0 & year >= yr_start), 
                           "mean_ac", "year"), parse = TRUE, hjust=0, vjust=0) +
  geom_text(x = yr_start + 30, y = 80, label = lm_eqn(subset(x.burn.top5, top5 == 1 & reburn == 1 & year >= yr_start),
                            "mean_ac", "year"), parse = TRUE, hjust=0, vjust=0) +
  geom_text(x = yr_start + 25, y = 85, label = "First burn", hjust=0, vjust=0) +
  geom_text(x = yr_start + 25, y = 80, label = "Reburn", hjust=0, vjust=0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Top 10% of fires by acreage
# -------------------------------------------------------------------

title <- paste0("Average size of the largest 10% fires, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average size of the largest 10 percent fires ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burn.top10, top10 == 1 & year >= yr_start), aes(year, mean_ac/1000, group = as.factor(reburn))) +
  geom_point(aes(color = as.factor(reburn))) +
  theme_bw() +
  xlab("Year") +
  ylab("Acres x 1000") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  scale_color_discrete("Burn status",labels = c("First burn", "Reburn")) +
  geom_smooth(method = 'lm', se= F, na.rm = T, aes(x=year, y=mean_ac/1000, color = as.factor(reburn))) +
  geom_text(x = yr_start + 5, y = 169, label = lm_eqn(subset(x.burn.top10, top10 == 1 & reburn == 0 & year >= yr_start), 
                                                                "mean_ac", "year"), parse = TRUE, hjust=0, vjust=0) +
  geom_text(x = yr_start + 5, y = 159, label = lm_eqn(subset(x.burn.top10, top10 == 1 & reburn == 1 & year >= yr_start),
                                                                "mean_ac", "year"), parse = TRUE, hjust=0, vjust=0) +
  geom_text(x = yr_start, y = 170, label = "First burn", hjust=0, vjust=0) +
  geom_text(x = yr_start, y = 160, label = "Reburn", hjust=0, vjust=0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# -------------------------------------------------------------------
# Acres of reburn type (1st, 2nd, etc.) burned per year
# -------------------------------------------------------------------
# want log? Just add log10 to the aes

title <- paste0("Acres in each reburn category, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Acres in each reburn category per year ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burn.ac.yr, burn_num > 1 & year >= yr_start), aes(year, sum_acres/1000, 
                                                               group = as.factor(burn_num), 
                                                               color = as.factor(burn_num))) + 
  geom_point() + 
  #  geom_line() + 
  geom_smooth(method = "lm", se = F) + 
  theme_bw() +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  xlab("Year") +
  ylab("Acres x1000") +
  scale_color_discrete("Burn frequency") +

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)



# -------------------------------------------
# Time between fires 
# -------------------------------------------

title <- paste0("Average number of years between reburns, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average number of years between reburns ", yr_start, "_", yr_end, ".png")

limits <- aes(ymax = mean_int + se_int, ymin = mean_int - se_int)

ggplot(subset(x.burntype.sum, year >= yr_start), aes(year, mean_int)) + 
  geom_point() + 
  geom_errorbar(limits, size = 0.2) +
  theme_bw() +
  xlab("Year") +
  ylab("Time since last burn (years)") +
  geom_smooth(method = 'lm', se = F, na.rm = F, aes(x = year, y = mean_int), color = "red", size = 0.4) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_text(x = yr_start, y = 50, label = lm_eqn(subset(x.burntype.sum, year >= yr_start), "mean_int", "year"), parse = TRUE, hjust = 0) +
  annotate("text", x = yr_start, y = 47, label = "Bars represent +/- 1 SE", hjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)



# ------------------------------------------------------------------
# Average proportion of original fire burned in 1st reburn
# ------------------------------------------------------------------

title <- paste0("Average proportion of original fire burned in 1st reburn, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average proportion of original fire burned in 1st reburn ", yr_start, "_", yr_end, ".png")

ggplot(subset(x.reburn.by.year.rank, burn_num == 2 & year >= yr_start), aes(year, mean_prop_reburn)) + 
  geom_point(aes(size = mean_orig_ac), shape = 16, color = "darkblue") + 
  theme_bw() +
  xlab("Year") +
  ylab("Proportion") +
  scale_size(guide = guide_legend(title = "Mean acreage of \noriginal fires")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
 # geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE, color = "black", size = 0.4) +
  geom_text(x = 2006, y = 1.0, 
            label = lm_eqn(subset(x.reburn.by.year.rank, burn_num == 2 & year >= yr_start), 
                           "mean_prop_reburn", "year"), parse = TRUE, hjust = 0, vjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------------------------------------------------------------------
# Average latitude of initial fires by year 
# ------------------------------------------------------------------

title <- paste0("Average latitude of initial fire areas, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average latitude INITIAL_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 0), aes(year, mean_lat)) + 
  geom_point() + 
  theme_bw() +
  xlab("Year") +
  ylim(61.5, 68.2) +
  ylab(bquote("Latitude ("~degree*"N)")) +
  scale_color_discrete("Reburn #") + #,labels = c("First burn", "Reburn")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  geom_text(x = yr_start, y = 68, 
            label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 0), "mean_lat", "year"), 
            parse = TRUE, hjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 11, height = 8, units = c("in"), dpi = 600)


# ------------------------------------------------------------------
# Average latitude of reburns by year 
# ------------------------------------------------------------------

title <- paste0("Average latitude of reburn areas, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average latitude REBURNS_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 1), aes(year, mean_lat)) + 
  geom_point() + 
  theme_bw() +
  xlab("Year") +
  ylim(61.5, 68.2) +
  ylab(bquote("Latitude ("~degree*"N)")) +
  scale_color_discrete("Reburn #") + #,labels = c("First burn", "Reburn")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  geom_text(x = yr_start, y = 68, 
            label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 1), "mean_lat", "year"), 
            parse = TRUE, hjust = 0) +
  
  ggsave(filename = plot.name, path = path.plots, width = 11, height = 8, units = c("in"), dpi = 600)




# ------------------------------------------------------------------
# Average latitude of reburns by year and reburn frequency
# ------------------------------------------------------------------

# no reburn freqs > 4 in Canadian database

title <- paste0("Average latitude of reburn areas by frequency, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average latitude ALL_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.reburn.by.year.rank, year >= yr_start), aes(year, mean_lat, group = burn_num)) + 
  geom_point(aes(color = burn_num)) + 
  theme_bw() +
  xlab("Year") +
  ylim(61.5, 68.2) +
  ylab(bquote("Latitude ("~degree*"N)")) +
  scale_color_discrete("Reburn #") + #,labels = c("First burn", "Reburn")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE, aes(color = as.factor(burn_num)), size = 0.4) +
  geom_text(x = yr_start, y = 68.4, 
            label = lm_eqn(subset(x.reburn.by.year.rank, burn_num == 2 & year >= yr_start), "mean_lat", "year"), 
            parse = TRUE, hjust = 0, color = "firebrick3") +
  geom_text(x = yr_start, y = 68.2, 
            label = lm_eqn(subset(x.reburn.by.year.rank, burn_num == 3 & year >= yr_start), "mean_lat", "year"), 
            parse = TRUE, hjust = 0, color = "khaki3") +
  geom_text(x = yr_start, y = 68.0, 
            label = lm_eqn(subset(x.reburn.by.year.rank, burn_num == 4 & year >= yr_start), "mean_lat", "year"), 
            parse = TRUE, hjust = 0, color = "green3") +

    ggsave(filename = plot.name, path = path.plots, width = 11, height = 8, units = c("in"), dpi = 600)

# ------------------------------------------------------------------
# Average longitude of initial fire areas by year 
# ------------------------------------------------------------------


title <- paste0("Average longitude of initial fire areas, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average longitude INITIAL_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 0), aes(year, mean_long)) + 
  geom_point() + 
  theme_bw() +
  xlab("Year") +
  ylim(-140, -122) +
  ylab(bquote("Longitude ("~degree*"W)")) +
  scale_color_discrete("Reburn #") + #,labels = c("First burn", "Reburn")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  geom_smooth(data=subset(x.burntype.sum, year > 1999 & reburn == 0), method = 'lm', se = FALSE, na.rm = TRUE, 
              color = "red", size = 0.4) +
  geom_text(x = yr_start, y = -123, 
            label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 0), "mean_long", "year"), 
            parse = TRUE, hjust = 0, color = "blue") +
  geom_text(x = yr_start, y = -124, 
            label = lm_eqn(subset(x.burntype.sum, year >= 1999 & reburn == 0), "mean_long", "year"), 
            parse = TRUE, hjust = 0, color = "red") +
  
  ggsave(filename = plot.name, path = path.plots, width = 11, height = 8, units = c("in"), dpi = 600)

# ------------------------------------------------------------------
# Average longitude of reburn areas by year 
# ------------------------------------------------------------------

title <- paste0("Average longitude of reburn areas, ", yr_start, " - ", yr_end, "\n Canada")
plot.name <- paste0("CAN_Average longitude REBURNS_", yr_start, "_", yr_end, ".png")

ggplot(subset(x.burntype.sum, year >= yr_start & reburn == 1), aes(year, mean_long)) + 
  geom_point() + 
  theme_bw() +
  xlab("Year") +
  ylim(-140, -122) +
  ylab(bquote("Longitude ("~degree*"W)")) +
  scale_color_discrete("Reburn #") + #,labels = c("First burn", "Reburn")) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  geom_smooth(data=subset(x.burntype.sum, year > 1999 & reburn == 1), method = 'lm', se = FALSE, na.rm = TRUE, 
              color = "red", size = 0.4) +
  geom_text(x = yr_start, y = -123, 
            label = lm_eqn(subset(x.burntype.sum, year >= yr_start & reburn == 1), "mean_long", "year"), 
            parse = TRUE, hjust = 0, color = "blue") +
  geom_text(x = yr_start, y = -124, 
            label = lm_eqn(subset(x.burntype.sum, year >= 1999 & reburn == 1), "mean_long", "year"), 
            parse = TRUE, hjust = 0, color = "red") +
  
  ggsave(filename = plot.name, path = path.plots, width = 11, height = 8, units = c("in"), dpi = 600)






