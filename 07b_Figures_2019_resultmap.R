#
# Just for testing plot in script 07 (section 7) in separate window
# Run 0 - 2 in script 07 first
#

xlimits <- c(3, 20)
ylimits <- c(58, 67.3)

df_pies$width = 0.7*0.8    # Sets size of pies
df_pies$height = 0.3*0.8   

# Put the pie of 12. and 15 *slightly* (ca 3 km) to the south
# Make variables Long_pie and Lat_pie solely for this, used for both pies and geom_text_repel
sel_pienudge <- substr(df_pies$Shortname,1,2) %in% c("12", "15")
df_pies$Lat_pie <- df_pies$Lat
df_pies$Long_pie <- df_pies$Long
df_pies$Lat_pie[sel_pienudge] <- df_pies$Lat[sel_pienudge] - 0.025

sel <- with(df_pies, 
            Long > xlimits1[1] & Long < xlimits1[2] &
              Lat > ylimits1[1] & Lat < ylimits1[2])

gg <- ggplot(data=df_range, aes(x, y)) + 
  geom_blank() +
  annotation_map(map_norway_h, aes(long, lat), fill = "navajowhite2") +
  geom_subview(data = df_pies, aes(x=Long_pie, y=Lat_pie, subview=pie, width=width, height=height))

gg2 <- gg + 
  geom_text_repel(data = df_pies, aes(Long, Lat, label = Shortname),
                  size = 4,
                  point.padding = 0.5, force = 0.3) +
  coord_cartesian(xlim = xlimits, ylim = ylimits) +
  theme_nothing()

if (save_plots){
  ggsave("Figures/07_Map_tilstand_Entire.png", gg2, width = 11, height = 12, dpi = 500) 
  ggsave("Figures/07_Map_tilstand_Entire.svg", gg2, width = 11, height = 12) 
}


# windows(10,10)
gg2

