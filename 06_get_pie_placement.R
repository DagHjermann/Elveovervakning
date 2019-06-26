
library(dplyr)

# Find which row of 'df' that is closest to point 'point'
get_closest <- function(point, df, xvar = "Long", yvar = "Lat"){
  dist <- sqrt((df[[xvar]] - point$x[1])**2 + 
               (df[[yvar]] - point$y[1])**2
               )
  which.min(dist)
  }

write_to_clipboard <- function(point){
  df <- data.frame(x=point$x[1], y = point$y[1])
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

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot only SE Norway
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

sel <- with(df_stations_vf, between(Long, xlimits) & between(Lat, ylimits))
df <- df_stations_vf[sel,]
plot(Lat ~ Long, data = df)
points(lat ~long, data = map_norway_h, pch = 20, cex = 0.3, col = "red")

# See data frame
View(df %>% mutate(i = 1:nrow(df)) %>% arrange(Lat))

#
# Repeat for each oint you want to "move"
#

# click on the point you want to "move"  => will give you the row number of that point
get_closest(locator(1), df_stations_vf)

# click on the new location of the point => copies new location to clipboard, go to Excel and insert it there
write_to_clipboard(locator(1))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Plot only SE Norway
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

plot(Lat ~ Long, data = df_stations_vf)
points(lat ~long, data = sample_n(map_norway_h,10000), pch = 20, cex = 0.3, col = "red")

# See data frame
View(df %>% mutate(i = 1:nrow(df)) %>% arrange(Lat))

#
# Repeat for each oint you want to "move"
#

# click on the point you want to "move"  => will give you the row number of that point
get_closest(locator(1), df_stations_vf)

# click on the new location of the point => copies new location to clipboard, go to Excel and insert it there
write_to_clipboard(locator(1))

#
# Check map
#
df_stations_vf_mod <- readxl::read_excel("Data/03_df_stations_vf_with_pie_positions.xlsx")
points(Lat_pie ~ Long_pie, data = df_stations_vf_mod, pch = 20, col = "blue")


