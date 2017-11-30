# plotting


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




ggplot(x.grouped.int.decade, aes(decade, summ_ac/1000, group = int_gp, fill= int_gp)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual("Fire interval\n(years)", labels = c("0-10", "11-30", "31-50", "50+"),
                    values = eco2Palette2) +
  
  labs(x = "Decade", y = "Acres x1000") +
  plot_opts


ggsave(filename = "Fire interval by decade_proportion.png", path = path.plots, width = 10, height = 7, units = c("in"), dpi = 600)


ggplot(DF, aes(decade, prop, group = int_gp, fill= int_gp)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual("Fire interval\n(years)", labels = c("0-10", "11-30", "31-50", "50+"),
                    values = eco2Palette2) +
  
  labs(x = "Decade", y = "Proportion") +
  plot_opts



x.grouped.int.decade <- setDecade(x.grouped.int)
x.grouped.int.decade <- ddply(x.grouped.int.decade, .(decade, int_gp), summarize, summ_ac = sum(sum_ac))
