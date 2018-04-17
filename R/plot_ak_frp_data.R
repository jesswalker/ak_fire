########################################################################### #
#
# plot_ak_frp_data.R 
#
# Objective:  Plot processed MxD14A1 FRP data.
#
# Input:     .RData file produced by process_ak_frp_data.R
#
#   
#   JWalker 6 April 2018
#  
########################################################################### #

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

# ------- by burn number

title <- paste0("FRP by burn number, ", year)
plot.name <- paste0("AK_FRP by burn number_", year, ".png")

p <- ggplot(x, aes(as.factor(burn_num), log(MaxFRP))) + 
  geom_boxplot() + 
  #ylim(0, 1000) +
  labs(x = "Burn number", y = "FRP") +
  ggtitle(title)
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------ by years since prior burn
# Drop factors for which there are fewer than 10 points
keep <- levels(as.factor(x$year_int))[table(x$year_int) > 10]
x.sub.int <- x[x$year_int %in% keep, ]

title <- paste0("FRP by years since prior burn, ", year)
plot.name <- paste0("AK_FRP by years since prior burn_", year, ".png")

p <- ggplot(x.sub.int, aes(as.factor(year_int), MaxFRP)) + 
  geom_boxplot() + 
  ylim(0, 1000) + 
  labs(x = "Years since prior burn", y = "FRP") +
  ggtitle(title)
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


# ------- by year of fire
keep <- levels(as.factor(x$FireYear))[table(x$FireYear) > 10]
x.sub.yr <- x[x$FireYear %in% keep, ]

title <- paste0("FRP by year of fire, ", year)
plot.name <- paste0("AK_FRP by year of fire_", year, ".png")

p <- ggplot(x.sub.yr, aes(as.factor(FireYear), MaxFRP)) + 
  geom_boxplot() + 
  ylim(0, 1000) +
  labs(x = "Year of fire", y = "FRP") +
  ggtitle(title)
p + plot_opts

ggsave(filename = plot.name, path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)

# -------- by mean fire interval
x.pts <- x.sub.int %>% group_by(year_int) %>% summarize(avg = mean(MaxFRP))

title <- paste0("Mean FRP by year interval, ", year)
plot.name <- paste0("AK_FRP mean FRP by year ", year, ".png")

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
x.copy <- x

# convert degrees to km distances
x.copy$lon_km <- 110. * (x.copy$longitude - (-125))*cos(x.copy$latitude/(360/(2*pi)))
x.copy$lat_km <- 110. * (x.copy$latitude-65)
coordinates(x.copy) = ~lon_km+lat_km


# ------- plot points in x,y km space  
plot.name <- paste0("AK_FRP bubble plot ", year, ".png")
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(bubble(x.copy, zcol = 'MaxFRP', fill = T, do.sqrt = F, maxsize = 1.8))
dev.off()

# ------- plot variogram
v = variogram(MaxFRP~1, x.copy)
level <- c(0.5, 0.95)

plot.name <- paste0("AK_FRP variogram ", year, ".png")
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(plot(v, fraction = 0.65, level=level))
dev.off()


# # --------- directional variogram
#   v.fit = fit.variogram(v, vgm("Sph"))
#   x.copy.var.det <- variogram(MaxFRP~lat_km+lon_km, data = x.copy)
#   
#   plot.name <- paste0("AK_FRP directional variogram ", year, ".png")
#   p <- plot(x.copy.var.det)
#   p

plot.name <- paste0("AK_FRP directional variogram ", year, ".png")

x.copy.var.dir <- variogram(MaxFRP~1, data = x.copy, alpha = c(0,45,90,135))
png(file = file.path(path.plots, plot.name), width = 10, height = 7, units = c("in"), res = 600)
print(plot(x.copy.var.dir))
dev.off()
