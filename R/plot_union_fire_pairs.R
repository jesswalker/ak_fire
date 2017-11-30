########################################################################### #
#
# plot_union_fire_pairs.R
#
# Objective:
# This script ingests the file created via process_union_fire_pairs.R and
# produces various plots from the data.

# Input:
# shape_metrics.RData - file of R data
# OR run
# process_union_pairs.R
#
# Output:
# Plots of PARA, shape, shape2, frac 
#  
# August 2017 JWalker 
#
########################################################################### #

# ---- Set environment ----
# Remove existing data and values
rm(list=ls())

# Load libraries
library(ggplot2)

# Set folder paths
path.in <- "D:/projects/Fire_AK_reburn/" 
path.plots <- "D:/projects/Fire_AK_reburn/plots"

# Load data saved from 'process_alaska_burn_data.R'
rdata <- "shape.metrics.RData"
load(file = file.path(path.in, "R", rdata))

# Set plot parameters ----
dodge = position_dodge(0.16)
theme_set(theme_bw())
plot_opts <- theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank(), # hide gridlines
                   legend.key = element_blank(),  # remove boxes around legend items
                   plot.title = element_text(hjust = 0.5)) # center title

# Set palettes...lots of palettes
eco1Palette2 <- c("#fc8d62", "#8da0cb", "#66c2a5")  # pink, blue, green
eco1Palette <- c("#45882b", "#003b98", "#ff5604") # green, blue, orange 
eco1PaletteSub <- eco1Palette[c(1,3)]
eco1PaletteBright <- c("forestgreen", "blue", "orange")
eco1PaletteGray <- c("gray20", "gray65", "gray87")
eco1PaletteGray2 <- c("gray65", "gray20", "gray87")
eco2Palette <- c('#8c510a', '#bf812d', '#dfc27d','#f6e8c3','#c7eae5','#80cdc1','#35978f', '#01665e')
eco2Palette2 <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00')
defaultPalette <- c("#F8766D", "#00BFC4") # default ggplot colors:red (initial) blue (reburn)
burnPalette <- c("red", "blue")
borTunPalette <- c("chartreuse4", "chartreuse3", "blue4", "blue")
borTunPaletteBright <- c("chartreuse1", "orangered1" )


# -----   PLOTS   ----

# --------------------------------------------------------------------------------------- #
# ---- Reburn proportion // TUNDRA ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.sub.means, year_int < 65 & ecoreg1 == "tundra")
title <- "Proportion of reburn in each reburn fire pair (initial and subsequent burn) -- TUNDRA"
plot.name <- "AK_Prop burned by time interval_TUNDRA.png" 

p <- ggplot(data.sub, aes(year_int, mean, group = type, color = type)) + 
      geom_point() + 
      geom_smooth(se = T, size = 0.9, span = 0.9) +
      scale_color_manual("Burn", values = burnPalette, labels = c("Initial", "Later")) +
      ylab("Mean proportion of reburn in each fire") +
      xlab("Interval between fires (years)") +
      ggtitle(title)

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- Reburn proportion // BOREAL ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.sub.means, year_int < 70 & ecoreg1 == "boreal")
title <- "Proportion of reburn in each reburn fire pair (initial and subsequent burn) -- BOREAL"
plot.name <- "AK_Prop burned by time interval_BOREAL.png" 

p <- ggplot(data.sub, aes(year_int, mean, group = type, color = type)) + 
      geom_point() + 
      geom_smooth(se = T, size = 0.9, span = 0.9) +
      scale_color_manual("Burn", values = burnPalette, labels = c("Initial", "Later")) +
      ylab("Mean proportion of reburn in each fire") +
      xlab("Interval between fires (years)") +
      ggtitle(title) 
      #geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
      #                width = 0.31, size = 0.2, position = dodge, color = "black")  +
      #annotate("text", x = 0, y = 95, label = "Bars represent +/- 1 SE", hjust = 0, vjust = 0) 
      
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# --------------------------------------------------------------------------------------- #
# ---- PARA // BOREAL ----
# --------------------------------------------------------------------------------------- #

#  Not an appropriate metric to use because of its size dependency
data.sub <- subset(x.metrics, ecoreg1 == "boreal")
title <- "Shape metric (para)  in each reburn fire pair type -- -- BOREAL"
plot.name <- "AK_Shape metric PARA by time interval_BOREAL.png" 

p <- ggplot(data.sub, aes(year_int, log10(para), group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("log(Para)") +
  xlab("Interval between fires (years)") +
 # ylim(0, 0.025) +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- PARA // TUNDRA ----
# --------------------------------------------------------------------------------------- #
#  Not an appropriate metric to use because of its size dependency
data.sub <- subset(x.metrics, ecoreg1 == "tundra")
title <- "Shape metric (para)  in each reburn fire pair type -- -- TUNDRA"
plot.name <- "AK_Shape metric PARA by time interval_TUNDRA.png" 

p <- ggplot(data.sub, aes(year_int, log10(para), group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("log(Para)") +
  xlab("Interval between fires (years)") +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- SHAPE // BOREAL ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "boreal")
title <- "Shape metric (shape) in each reburn fire pair type -- BOREAL"
plot.name <- "AK_Shape metric SHAPE by time interval_BOREAL.png" 

p <- ggplot(data.sub, aes(year_int, shape, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Shape") +
  xlab("Interval between fires (years)") +
  ylim(0, 11) +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- SHAPE // TUNDRA ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "tundra")
title <- "Shape metric (shape) in each reburn fire pair type -- TUNDRA"
plot.name <- "AK_Shape metric SHAPE by time interval_TUNDRA.png" 

p <- ggplot(data.sub, aes(year_int, shape, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Shape") +
  xlab("Interval between fires (years)") +
  # ylim(0, 0.025) +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# --------------------------------------------------------------------------------------- #
# ---- SHAPE2 // BOREAL ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "boreal")
title <- "Shape metric (shape2)  in each reburn fire pair type -- BOREAL"
plot.name <- "AK_Shape metric SHAPE2 by time interval_BOREAL.png" 

p <- ggplot(data.sub, aes(year_int, shape2, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Shape2") +
  xlab("Interval between fires (years)") +
  # ylim(0, 0.025) +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# --------------------------------------------------------------------------------------- #
# ---- SHAPE2 // TUNDRA ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "tundra")
title <- "Shape metric (shape2)  in each reburn fire pair type -- TUNDRA"
plot.name <- "AK_Shape metric SHAPE2 by time interval_TUNDRA.png" 

p <- ggplot(data.sub, aes(year_int, shape2, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Shape2") +
  xlab("Interval between fires (years)") +
  # ylim(0, 0.025) +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- FRAC // BOREAL ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "boreal")
title <- "Shape metric (frac)  in each reburn fire pair type -- BOREAL"
plot.name <- "AK_Shape metric FRAC by time interval_BOREAL.png" 

p <- ggplot(data.sub, aes(year_int, frac, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Frac") +
  xlab("Interval between fires (years)") +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# --------------------------------------------------------------------------------------- #
# ---- FRAC // TUNDRA ----
# --------------------------------------------------------------------------------------- #

data.sub <- subset(x.metrics, ecoreg1 == "tundra")
title <- "Shape metric (frac)  in each reburn fire pair type -- TUNDRA"
plot.name <- "AK_Shape metric FRAC by time interval_TUNDRA.png" 

p <- ggplot(data.sub, aes(year_int, frac, group = type, color = type)) + 
  geom_point() + 
  geom_smooth(se = T, size = 0.9, span = 0.9) +
  scale_color_manual("Burn", values = eco1PaletteBright, labels = c("Initial-unreburned", "Later-unreburned", "Reburn")) +
  ylab("Frac") +
  xlab("Interval between fires (years)") +
  ggtitle(title) 

p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)