

# Set consistent plot parameters
dodge = position_dodge(0.16)
theme_set(theme_bw())
plot_opts <- theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank(), # hide gridlines
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank(), # hide gridlines
                   legend.key = element_blank(),  # remove boxes around legend items
                   legend.text = element_text(size = 14),
                   legend.title = element_text(size = 18),
                   plot.title = element_text(hjust = 0.5), # center title
                   axis.text = element_text(size = 16),
                   axis.title = element_text(size = 18, face="bold"))

data.sub <- subset(x.burntype.sum, year >= yr_start)
title <- paste0("Acres burned per year, ", yr_start, " - ", yr_end, "\n Boreal initial burns and reburns")
plot.name <- paste0("AK_Acres burned per year_side by side_BOREAL_", yr_start, "_", yr_end, ".png")

p <- ggplot(data.sub, aes(x = year, y = bor_ac/1000, fill = reburn, shape = reburn)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_line(aes(year, bor_10yrMove/1000, color = reburn, linetype = reburn), size = 0.8) +
  labs(x = "Year", y = "Acres x1000") +
  scale_fill_manual("Burn Type", labels = c("Initial", "Reburn"), values = c("gray55", "gray87")) + 
  scale_color_manual("Burn Type", values = burnPalette, labels = c("Initial", "Reburn")) +
  scale_linetype_manual("Burn Type", labels = c("Initial", "Reburn"), values = c(rep("solid", 2))) +
  scale_x_continuous(breaks = seq(yr_start, yr_end, 10)) +
  annotate("text", x = 1940, y = 5000, label = "Lines show 10-year moving average", 
           fontface = "italic", hjust = 0, vjust = 0, size = 6) 
#  ggtitle(title)
p + plot_opts

ggsave(filename = "Acres burned_poster.png", path = path.plots, width = 12, height = 8, units = c("in"), dpi = 600)
