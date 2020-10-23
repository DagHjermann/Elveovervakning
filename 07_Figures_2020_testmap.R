


# Filter using entire name:
group1 <- c("Gruve.O5","Gruve.O4","Gruve.O3","Gruve.Ya")
group2a <- c("GruveF2","Gruve.F4") 
group2b <- c("Gruve.F5","Gruve.F7") 
# Filter using the two first characters (using substr):
group3 <- c("01","03","04") 
group4a <- c("47","50","51") 
group4b <- c("48","49") 
group5 <- c("35","36","37") 
group6a <- c("11","12","13") 
group6b <- c("08","09","10") 


get_x <- function(name, data = df_stations)
  subset(data, Shortname %in% name)$x
get_y <- function(name, data = df_stations)
  subset(data, Shortname %in% name)$y

# For setting ylim:
ggdata <- layer_data(gg_back)

gg2 <- gg_back +
  geom_point(
    data = df_stations, 
    aes(label = Shortname, shape = Stasjonstype, color = Stasjonstype), size = rel(2)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(Shortname %in% group1), 
    xdist = 30E3, y_even_dist = 20E3, xadj_start = 7E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = df_stations %>% filter(Shortname %in% group2a), 
    xdist = 20E3, y_even_dist = 20E3, y_even_pos = 40E3, xadj_start = 7E3,
    order = "W to E",
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(Shortname %in% group2b), 
    xdist = 20E3, y_even_dist = 20E3, y_even_pos = -40E3, xadj_start = 7E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group3), 
    xdist = 30E3, xadj_start = 7E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group4a), 
    xdist = 50E3, xadj_start = 7E3, y_even_dist = 20E3, y_even_pos = 40E3, 
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group5), 
    xdist = 35E3, xadj_start = 7E3, y_even_dist = 20E3, y_even_pos = 40E3, 
    order = "W to E",
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group6a),
    xdist = 30E3, y_even_dist = 20E3, y_even_pos = -60E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group6b),
    xdist = 30E3, y_even_dist = 20E3, y_even_pos = -60E3,
    color = "blue3", segment.color = "blue3") +
  xlim(range(ggdata$x) + c(-50E3, 50E3)) +  
  ylim(range(ggdata$y) + c(-50E3, 0))   

#
# Calculate boxes in UTM
#

# Make data set with 2 lines, lower left and upper right corner  
get_corners <- function(box) 
  tibble(Long = sort(c(box$lng1, box$lng2)), Lat = sort(c(box$lat1, box$lat2)))

# from corners,make data set with 4 lines, one from each corner (anticlockwise from lower left)  
corners_to_polygons <- function(df)
  tibble(x = c(df$x[1], df$x[2], df$x[2], df$x[1]), y = c(df$y[1], df$y[1], df$y[2], df$y[2]))

# List of corners (with UTM coordinates)
box_corners <- boxlist %>% 
  purrr::map(get_corners) %>%
  purrr::map(add_utm_coord, name_long = "Long", name_lat = "Lat")

# Data frame of polygons (UTM coordinates)
box_polygons <- box_corners %>%
  purrr::map_dfr(corners_to_polygons, .id = "boxnumber")

#
# Add boxes on main map
#
gg3 <- gg2 +
  geom_polygon(data = box_polygons, aes(group = boxnumber),
               fill = NA, colour = "red3")

gg3





