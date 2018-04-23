########################################################################### #
#
# plot_ak_frp_data.R 
#
# Objective:  Plot processed MxD14A1 FRP data.
#
# Input:     .RData file produced by process_ak_frp_data.R
#
#   
#   JWalker 17 April 2018
#  
########################################################################### #


# ---- Set environment ----

# Remove existing data and values
rm(list=ls())

# Load libraries
library(ggplot2)
library(lubridate)

# Set folder paths
path.in <- "D:/projects/ak_fire" 
path.plots <- "D:/projects/ak_fire/output/plots/frp"

# Load functions
source(file.path(path.in, "R", "ak_functions.R"))

# Load data saved from 'process_alaska_burn_data.R'
rdata <- "process_ak_frp_data.R"
load(file = file.path(path.in, "data", rdata))


# ------------------------------------ -
# PLOTS
# ------------------------------------

# Set consistent plot parameters
dodge = position_dodge(0.16)
theme_set(theme_bw())
plot_opts <- theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank(), # hide gridlines
                   legend.key = element_blank(),  # remove boxes around legend items
                   plot.title = element_text(hjust = 0.5)) # center title


# All data, all years
# Only the highest burn count is retained to avoid overcounting contributions

# ------- PLOT by burn number
title <- "FRP by burn number, all years"
plot.name <- "AK_FRP by burn number_all years.png"

# Get means
means <- aggregate(MaxFRP ~ burn_num, x, mean)

# Plot boxplot with mean values 
p <- ggplot(x, aes(as.factor(burn_num), MaxFRP)) + 
  geom_boxplot(aes(fill = as.factor(burn_num)), show.legend = F) +
  geom_point(data=means, aes(as.factor(burn_num), MaxFRP, shape = "mean"), size = 7) +
  scale_shape_manual("", values = 18) +
  geom_text(data = means, aes(label = round(MaxFRP, 2), y = MaxFRP + 16), hjust = -0.2) +
  coord_cartesian(ylim = c(0,500), expand = TRUE) +
  labs(x = "Burn number", y = "FRP") +
  ggtitle(title)
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------- PLOT by burn number, log plot
title <- "Log FRP by burn number, all years"
plot.name <- "AK_FRP by burn number_all years_log.png"


# Plot boxplot with mean values 
p <- ggplot(x, aes(as.factor(burn_num), log10(MaxFRP))) + 
  geom_boxplot(aes(fill = as.factor(burn_num)), show.legend = F) +
  geom_point(data=means, aes(as.factor(burn_num), log10(MaxFRP), shape = "mean"), size = 7) +
  scale_shape_manual("", values = 18) +
  geom_text(data = means, aes(label = round(log10(MaxFRP), 2), y = log10(MaxFRP) + 0.1), hjust = -0.2) +
  labs(x = "Burn number", y = "log FRP") +
  ggtitle(title)
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)





# For individual years. This mostly groups data geographically as well.

for (yr in c(2003, 2004, 2005, 2009)) {
  
  x.yr <- subset(x, year == yr)
  
# ------- PLOT by burn number
  title <- paste0("FRP by burn number, ", yr)
  plot.name <- paste0("AK_FRP by burn number_", yr, ".png")

  p <- ggplot(x.yr, aes(as.factor(burn_num), log10(MaxFRP))) + 
    geom_boxplot() + 
    #ylim(0, 1000) +
    labs(x = "Burn number", y = "log FRP") +
    ggtitle(title)
  p + plot_opts

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------ PLOT by years since prior burn
# Drop factors for which there are fewer than 10 points
  keep <- levels(as.factor(x.yr$year_int))[table(x.yr$year_int) > 10]
  x.sub.int <- x.yr[x.yr$year_int %in% keep, ]

  title <- paste0("FRP by years since prior burn, ", yr)
  plot.name <- paste0("AK_FRP by years since prior burn_", yr, ".png")

  p <- ggplot(x.sub.int, aes(as.factor(year_int), log10(MaxFRP))) + 
       geom_boxplot() + 
       #ylim(0, 1000) + 
       labs(x = "Years since prior burn", y = "log FRP") +
       ggtitle(title)
  p + plot_opts

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------- PLOT by year of fire
  keep <- levels(as.factor(x.yr$FireYear))[table(x.yr$FireYear) > 10]
  x.sub.yr <- x.yr[x.yr$FireYear %in% keep, ]

  title <- paste0("FRP by year of fire, ", yr)
  plot.name <- paste0("AK_FRP by year of fire_", yr, ".png")

  p <- ggplot(x.sub.yr, aes(as.factor(FireYear), log10(MaxFRP))) + 
       geom_boxplot() + 
       #ylim(0, 1000) +
       labs(x = "Year of fire", y = "log FRP") +
       ggtitle(title)
  p + plot_opts

  ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# -------- PLOT by mean fire interval
  x.pts <- x.sub.int %>% group_by(year_int) %>% summarize(avg = mean(MaxFRP))

  title <- paste0("Mean FRP by year interval, ", yr)
  plot.name <- paste0("AK_FRP mean FRP by year ", yr, ".png")

  p <- ggplot(x.pts, aes(year_int, avg)) + 
       geom_point() + 
       geom_smooth(method = 'lm', se = FALSE, size = 0.4) +
       labs(x = "Years since prior burn", y = "FRP") +
       annotate("text", x = 5, y = 400, label = lm_eqn(x.pts, "avg", "year_int"), 
           parse = TRUE, hjust = 0, vjust = 0, color = "#000000") +
       ggtitle(title)
  p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# Calculate semivariograms
x.copy <- x.yr

# convert degrees to km distances
x.copy$lon_km <- 110. * (x.copy$longitude - (-125))*cos(x.copy$latitude/(360/(2*pi)))
x.copy$lat_km <- 110. * (x.copy$latitude-65)
coordinates(x.copy) = ~lon_km+lat_km


# ------- PLOT points in x,y km space  
plot.name <- paste0("AK_FRP bubble plot ", yr, ".png")
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(bubble(x.copy, zcol = 'MaxFRP', fill = T, do.sqrt = F, maxsize = 1.8))
dev.off()

# ------- PLOT variogram
v = variogram(MaxFRP~1, x.copy)
level <- c(0.5, 0.95)

plot.name <- paste0("AK_FRP variogram ", yr, ".png")
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(plot(v, fraction = 0.65, level=level))
dev.off()


# # ------PLOT directional variogram
#   v.fit = fit.variogram(v, vgm("Sph"))
#   x.copy.var.det <- variogram(MaxFRP~lat_km+lon_km, data = x.copy)
#   
#   plot.name <- paste0("AK_FRP directional variogram ", yr, ".png")
#   p <- plot(x.copy.var.det)
#   p

plot.name <- paste0("AK_FRP directional variogram ", yr, ".png")

x.copy.var.dir <- variogram(MaxFRP~1, data = x.copy, alpha = c(0, 45, 90, 135))
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(plot(x.copy.var.dir))
dev.off()

}