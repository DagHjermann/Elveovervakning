#
# Just for testing plot in script 07 (section 3) in separate window
# Run 0 - 2 in script 07 first
#

# pie(rep(1,15), col = terrain.colors(15))

cols <- RColorBrewer::brewer.pal(3, "Dark2")


xlimits <- c(3, 20)
ylimits <- c(58, 67.3)

# df <- data.frame(long = 10.66, lat = 59.92)
gg <- ggplot(df_stations) +
  annotation_map(map_norway, aes(long, lat), fill = "navajowhite2") +
  geom_point(aes(Long, Lat, fill = Målinger), pch = 21, size = rel(3)) +
  scale_fill_manual(values = cols) +
  coord_map("lambert", parameters = c(64, 12), xlim = c(-1,30), ylim = c(57, 72)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "none")

gg3 <- gg +
  geom_text_repel(aes(Long, Lat, label = sub("*", "", Shortname, fixed = TRUE)), size = 4,
                  point.padding = 0.2, force = 0.3)

windows(10,10)
gg3 + coord_map("lambert", parameters = c(60, 80), xlim = xlimits, ylim = ylimits)  



