#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# b. Functions for converting from UTM to long/lat  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Converts utm to long-lat and (if return_entire_dataframe = TRUE) adds the variables to the data frame
# For one single UTM zone
utm2longlat_onezone <- function(df, xvar = "x", yvar = "y", zone, return_entire_dataframe = FALSE){
  crs_utm <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m")
  SP.utm <- SpatialPoints(df[,c(xvar, yvar)], 
                          proj4string=CRS(crs_utm)
  )
  SP.longlat <- spTransform(SP.utm, CRS("+proj=longlat"))
  if (!return_entire_dataframe){
    df <- data.frame(Long = SP.longlat@coords[,1], Lat = SP.longlat@coords[,2])
  } else {
    df$Long <- SP.longlat@coords[,1]
    df$Lat <- SP.longlat@coords[,2]
  }
  df
}

# utm2longlat_onezone(df_stations[1:10, ], "X-koordinat", "Y-koordinat", zone = 32)

# As 'utm2longlat_onezone', but also picks data for that zone (used in 'utm2longlat')
utm2longlat_onezone2 <- function(df, zonevar, zone, ...){
  df2 <- df[df[[zonevar]] %in% zone,]
  utm2longlat_onezone(df2, zone = zone, ...)
}

# utm2longlat_onezone2(df_stations[38:42, ], "Sone", zone = 32, xvar = "X-koordinat", yvar = "Y-koordinat")

# Converts utm to long-lat and (if return_entire_dataframe = TRUE) adds the variables to the data frame
# UTM zone given as a variable in the data 
# Note: the order the of output data is shuffled 
utm2longlat <- function(df, xvar = "x", yvar = "y", zonevar = "zone", return_entire_dataframe = FALSE){
  zones <- unique(df[[zonevar]])
  df <-   
    purrr::map_df(zones, ~utm2longlat_onezone2(df, zonevar = zonevar, zone = ., xvar = xvar, yvar = yvar, 
                                               return_entire_dataframe = return_entire_dataframe)
    )
  df
}

# df_stations[38:42, "Sone"]
# utm2longlat(df_stations[38:42, ], "X-koordinat", "Y-koordinat", "Sone")
# utm2longlat(df_stations[38:42, ], "X-koordinat", "Y-koordinat", "Sone", return_entire_dataframe = TRUE)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make one pie
# Fixed colorscheme1 
# measurements_present tells us which of the pies that should be coloured according to the colour scheme
# Those pies with measurements missing gets a grey colour
#
# Pies start at top 12:00 and goes anti-clockwise
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_pie <- function(measurements_present, col = colorscheme1, col_lacking = "grey80", linewidth = 0.5){
  col[!measurements_present] <- col_lacking
  pie_data <- data.frame(x = vars, y = rep(1,6))
  ggplot(pie_data, aes(x=1, y, fill=x)) + 
    geom_bar(stat="identity", color = "black", linewidth = linewidth) + coord_polar(theta="y") +
    scale_fill_manual(values = col) +
    theme_void() + theme(legend.position="none") + theme_transparent()
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make one pie from a vecor of colours
#
# Pies start at top 12:00 and goes anti-clockwise
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_pie_from_colours <- function(cols, linewidth = 0.5){
  N <- length(cols)
  pie_data <- tibble(x = letters[1:N], y = rep(1,N), no = 1:N) %>% as.data.frame()
  pie_data$z = paste(pie_data$no, pie_data$cols, sep = "_")
  ggplot(pie_data, aes(x=1, y, fill = z)) + 
    geom_bar(stat="identity", color = "black", linewidth = linewidth) + coord_polar(theta="y") +
    scale_fill_manual(values = cols) +
    theme_void() + theme(legend.position="none") + theme_transparent()
}

# make_pie_from_colours(c("red", "blue"))
# make_pie_from_colours(c("blue", "red", "green"))
# make_pie_from_colours(c("blue", "blue", "green"))
# make_pie_from_colours(colors()[1:15])