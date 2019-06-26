
#
# See script 06 (using base plot)
# This script tried to use ggplot and I couldn't make it work
#

library(ggplot2)
library(ggmap)
library(grid)
library(dplyr)

#
# locator() for ggplot
# Based on 
# 
# https://stackoverflow.com/a/46372259/1734247
ggloc <- function(object){
  seekViewport("layout")
  p <-  as.numeric(grid.locator("npc"))
  dat_x <- object$data[[object$mapping$x]]
  dat_y <- object$data[[object$mapping$y]]
  locatedX <- min(dat_x) + p[1]*diff(range(dat_x))
  locatedY <- min(dat_y) + p[2]*diff(range(dat_y))
  c(locatedX, locatedY)
  }

# Find which row of 'df' that is closest to point 'point'
get_closest <- function(point, df, xvar = "Long", yvar = "Lat"){
  dist <- sqrt((df[[xvar]] - point[1])**2 + 
               (df[[yvar]] - point[2])**2
               )
  which.min(dist)
  }

write_to_clipboard <- function(vec){
  df <- data.frame(x=vec[1], y = vec[2])
  df <- round(df, 5)
  write.table(df, "clipboard", sep = "\t", dec = ",", row.names = FALSE, col.names = FALSE)
  }


#
# Data
#
df_stations_vf <- readxl::read_excel("Data/03_df_stations_vf.xlsx")

#
# Coastline map
#
load("Data_input/Norway_coastline_longlat2.RData")
map_norway_h <- norway_coast_longlat2
rm(norway_coast_longlat)

xlimits <- c(7,12)
ylimits <- c(57.6, 60.7)
between <- function(x, lim)
  x >= lim[1] & x <= lim[2]

#
# Plot only SE Norway
#
sel <- with(df_stations_vf, between(Long, xlimits) & between(Lat, ylimits))
df <- df_stations_vf[sel,]
gg <- ggplot(df, aes(Long, Lat)) +
    annotation_map(map_norway_h, aes(long, lat), fill = "grey60") +
  geom_point()  +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
gg

debugonce(ggloc)
ggloc(gg)


#
# Repeat for each oint you want to "move"
#

# click on the point you want to "move"  => will give you the row number of that point
debugonce(get_closest)
debugonce(ggloc)
get_closest(ggloc(gg), df_stations_vf)

# click on the new location of the point => copies new location to clipboard, go to Excel and insert it there
write_to_clipboard(ggloc(gg))


View(df %>% mutate(i = 1:nrow(df)) %>% arrange(Lat))
ggloc(gg)






