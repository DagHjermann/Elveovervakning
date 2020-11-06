---
title: "07 Figures 2018 - maps and barplots"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true

---

Overview maps (types of stations) for Methods chapter (section 3)  
* using UTM zone 32V as projection  
* Data for 7c are saved in 4c   
* "Lange tidsserier" ('long time-series' stations) are used for Methods map, not in Results  

Overview of process:    
1. Packages and load scripts      
2. Data, includes    
    - station data  
    - background map (coast- + county lines)  
    - rivers  
    - the last two: downloaded as shape (UTM33), but coordinares extracted and converted to UTM32  
3. Defines inset map boxes using a leaflet map; therefore boxes are defined using lat/long  
    - boxes are saved as a list, each of which defining bottom left and top right corner   
4. Prepare data sets in UTM32  
    - station data   
    - box_corners (used to make the inset plots, 'make_subplot_nolabels', and to filter stations for these)
    - box_polygons (used to plot inset rectangles on the big map, 6b)  
5. Is used only as a fast way to make a draft map with labels (used as help for 6a)
6. Build gg3a plot
    - Build gg2 plot based on background map (gg_back + rivers) + labels, group by group  
    - gg3a map = gg2 map plus boxes   
7. Functions for making insets  
8. Make insets including labels. the resukt is a list of ggplot objects)
9. Make final plot by adding legend (gg3b), then plot gg3b plus each of the insets   
    - position of insets is quite a bit try and error   



## 1. Prerequisites  

### a. Whether and where to save figures  

```r
#
# Save figures to file, or just show them in this report?
#
#
save_plots <- TRUE
# save_plots <- FALSE


#
# Where to save figures
#
folder_fig <- "Figures/2020"
```

### b. Packages + map  

```r
library(tidyverse)  # filter, group_by, summary, etc.
```

```
## Warning: replacing previous import 'vctrs::data_frame' by 'tibble::data_frame'
## when loading 'dplyr'
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.4     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## Warning: package 'tibble' was built under R version 4.0.3
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(gridGraphics)
```

```
## Warning: package 'gridGraphics' was built under R version 4.0.3
```

```
## Loading required package: grid
```

```r
library(maps)       # Norway map (NOTE: must be loaded after tidyverse)
```

```
## 
## Attaching package: 'maps'
```

```
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(readxl)

# map_norway <- map_data("world", "Norway")
# map_norway_h <- readRDS("Data_input/map_norway_hires.RData")

# Long/lat map (including the smaller islands) - lacking Mjøsa, though! 
#   (and including some North Finnish lakes?)
load("Data_input/Norway_coastline_longlat2.RData")
# rename
map_norway_h <- norway_coast_longlat2
rm(norway_coast_longlat)
```

```
## Warning in rm(norway_coast_longlat): object 'norway_coast_longlat' not found
```

```r
# Test plot
# ggplot(map_norway_h) + geom_path(aes(long, lat, group = group))

library(mapproj)    # mapproject
library(ggrepel)    # geom_text_repel()
```

```
## Warning: package 'ggrepel' was built under R version 4.0.3
```

```r
library(sp)         # SpatialPoints(), CRS(), spTransform()
library(sf)         # st_read
```

```
## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

```r
library(ggimage)    # geom_subview(), theme_transparent()
```

```
## Warning: package 'ggimage' was built under R version 4.0.3
```

```r
library(glue)       # used in ggplot2() function
```

```
## Warning: package 'glue' was built under R version 4.0.3
```

```
## 
## Attaching package: 'glue'
```

```
## The following object is masked from 'package:dplyr':
## 
##     collapse
```

```r
library(leaflet)

library(cowplot)
```

```
## 
## ********************************************************
```

```
## Note: As of version 1.0.0, cowplot does not change the
```

```
##   default ggplot2 theme anymore. To recover the previous
```

```
##   behavior, execute:
##   theme_set(theme_cowplot())
```

```
## ********************************************************
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggimage':
## 
##     theme_nothing
```

```r
source("07_Figures_functions.R")

# Define crs strings for conversion between long/lat and UTM
crs_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
crs_utm <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m"
```


## 2. Data

### a. Data from script 7a

```r
df_stations <- readRDS("Data/2020/07a_df_stations_2019.rds")
```

### b. Map with counties (fylke)  

```r
# 
# Gotten from Geonorge ("Norske fylker og kommuner illustrasjonsdata 2019 (klippet etter kyst)")
#   https://kartkatalog.geonorge.no/metadata/cb02ab77-d3e6-4500-8a92-ea67367e7734 
#   https://nedlasting.geonorge.no/geonorge/Generell/AdmEnh_klippet_etter_kyst2019.zip  
map_counties_utm33 <- sf::st_read("Data_input/2019/AdmEnh_klippet_etter_kyst2019/Fylker19.geojson")
```

```
## Reading layer `Fylker' from data source `C:\Data\seksjon 318\Elveovervakning\Data_input\2019\AdmEnh_klippet_etter_kyst2019\Fylker19.geojson' using driver `GeoJSON'
## Simple feature collection with 18 features and 2 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -77299 ymin: 6448400 xmax: 1115101 ymax: 7939978
## projected CRS:  ETRS89 / UTM zone 33N
```

```r
# UTM zone 33
# plot(map_counties_utm)
```

### c. Get coordinates of county map (df_counties)  
Used to make the background map  
* We convert the map from UTM zone 33 to UTM 32, as this is more fitting for Southern Norway   (https://upload.wikimedia.org/wikipedia/commons/9/9e/LA2-Europe-UTM-zones.png)

```r
# Extract coordinates and Transform from UTM33 to UTM32
df_counties <- st_coordinates(map_counties_utm33) %>% 
  as.data.frame() %>%
  add_transformed_coord(
    name_x = "X", name_y = "Y", 
    from = "utm", from_zone = 33,
    to = "utm", to_zone = 32
  ) %>%
  select(-X, -Y) %>%
  rename(group = L2, id = L3)


# In the first version, we convert map to long-lat, then 
#   extract coordinates, then convert back to UTM 
if (FALSE){
  # Transform data to to long-lat  
  map_counties <- sf::st_transform(map_counties_utm, "+proj=longlat +ellps=WGS84")
  
  df_counties <- st_coordinates(map_counties) %>% as.data.frame()
  
  colnames(df_counties) <- c("long", "lat", "L1", "group", "id")
  
  # Add UTM coordinates (x and y) to map
  df_counties <- add_utm_coord(df_counties)
  
  str(df_counties, 1)
  
}
```



### d. Make background map (gg_back)    

```r
# Check counties: it turns out that 11 = Nordland, 3 = Troms, 2 = Finnmark
if (FALSE){
  df_counties %>% 
    filter(L1 == 1) %>%
    group_by(id) %>%
    summarise(across(, mean)) %>%
    arrange(y)
}

# Remove these 3 counties
df_counties <- df_counties %>%
  filter(!id %in% c(11,3,2))

# Get remaining county numbers
county_numbers <- unique(df_counties$id)

# Start 'gg_back'
gg_back <- ggplot(data = df_counties, aes(x, y)) + 
  geom_blank() + 
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        )

# Add one county after the other
for (i in county_numbers){
  gg_back <- gg_back + 
    geom_polygon(data = df_counties %>% filter(L1 == 1 & id %in% i), 
                 aes(x, y, group = group), fill = "navajowhite", color = "grey30")
}

# For adjusting limits:
ggdata <- layer_data(gg_back)

gg_back
```

![](07b_Figures_2020_labelmap_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# test use of ggdata
if (FALSE)
  gg_back + ylim(range(ggdata$y) + c(-100E3, 0))
```

### e. Rivers


```r
nc_rivers_strahler4 <- st_read(
  "K:\\Kart\\ELVIS\\Hovedelv\\elvis_hovedelv.shp",
  query = "SELECT * FROM elvis_hovedelv WHERE STRAHLER >= 4")  # only big rivers; saves a little time
```

```
## Reading layer `elvis_hovedelv' from data source `K:\Kart\ELVIS\Hovedelv\elvis_hovedelv.shp' using driver `ESRI Shapefile'
## Simple feature collection with 6107 features and 7 fields
## geometry type:  MULTILINESTRING
## dimension:      XY
## bbox:           xmin: -47969.41 ymin: 6455620 xmax: 1115094 ymax: 7935058
## projected CRS:  WGS 84 / UTM zone 33N
```

```r
#  UTM zone 33

# Extract coordinates and Transform from UTM33 to UTM32
df_rivers_strahler5 <- nc_rivers_strahler4 %>%
  filter(STRAHLER >= 5) %>% 
  st_coordinates %>%
  as.data.frame() %>%
  add_transformed_coord(
    name_x = "X", name_y = "Y", 
    from = "utm", from_zone = 33,
    to = "utm", to_zone = 32
  ) %>%
  select(-X, -Y) %>%
  rename(group = L2) %>%
  fix_river_errors(limit_meters = 2000)  # Fix errors (i.e. remove extremely long segments)

df_rivers_strahler7 <- nc_rivers_strahler4 %>%
  filter(STRAHLER >= 7) %>% 
  st_coordinates %>%
  as.data.frame() %>%
  add_transformed_coord(
    name_x = "X", name_y = "Y", 
    from = "utm", from_zone = 33,
    to = "utm", to_zone = 32
  ) %>%
  select(-X, -Y) %>%
  rename(group = L2) %>%
  fix_river_errors(limit_meters = 2000) # Fix errors (i.e. remove extremely long segments)


north_limit <- 7240000    # number found using ggplotly

# Remove data for North Norway (filter doesn't work, as some data are reshuffeled)
sel <- !is.na(df_rivers_strahler5$y) & df_rivers_strahler5$y > north_limit  
df_rivers_strahler5[sel, c("x","y")] <- NA

sel <- !is.na(df_rivers_strahler7$y) & df_rivers_strahler7$y > north_limit
df_rivers_strahler7[sel, c("x","y")] <- NA

rivercol <- "lightskyblue"

if (FALSE){
  # Test plot
  ggplot() +
    geom_path(data = df_rivers_strahler7, aes(x, y, group = group), col = "red") 
}
```


### f. Lakes  

Get coordinates (in zone 32)   

```r
nc_lakes <- st_read("K:/Kart/N1000/VANN.shp")
```

```
## Reading layer `VANN' from data source `K:\Kart\N1000\VANN.SHP' using driver `ESRI Shapefile'
## Simple feature collection with 5542 features and 4 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: -56292 ymin: 6456204 xmax: 1111080 ymax: 7926197
## CRS:            NA
```

```r
st_crs(nc_lakes) <- "+proj=utm +zone=33"  # set coordinate system

# Extract coordinates and Transform from UTM33 to UTM32
df_lakes <- st_coordinates(nc_lakes) %>% 
  as.data.frame() %>%
  add_transformed_coord(
    name_x = "X", name_y = "Y", 
    from = "utm", from_zone = 33,
    to = "utm", to_zone = 32
  ) %>%
  select(-X, -Y) %>%
  rename(group = L2)

# Mjøsa only 
df_lakes_mjosa <- df_lakes %>% filter(group == 3462)
```



## 3. Define inset boxes (boxlist)   

### a. Define boxes which for inset maps    
Made reiteratively, by running b first and looking at station coordinates   

```r
boxlist <- list(
  
  list(
    lng1 = 5.55, lat1 = 61.25, 
    lng2 = 6.35, lat2 = 61.65
  ),
  list(
    lng1 = 5.7, lat1 = 60.5, 
    lng2 = 6.5, lat2 = 60.9
  ),
  list(
    lng1 = 5.8, lat1 = 59.4, 
    lng2 = 6.5, lat2 = 59.7
  ),
  list(
    lng1 = 5.4, lat1 = 58.4, 
    lng2 = 6.2, lat2 = 58.9
  ),
  list(
    lng1 = 8.75, lat1 = 58.55, 
    lng2 = 9.0, lat2 = 58.7
  )
  
)
```


### b. Leaflet map, stations only    

```r
# Base map

leaf1 <- leaflet(df_stations) %>%
  addTiles() %>%
  addCircleMarkers(lng = df_stations$Long, lat = df_stations$Lat,
             popup = paste0(
               df_stations$Shortname, "<br>",
               df_stations$Long, ", ", df_stations$Lat),
             radius = 5)
```


### c. Leaflet map, with boxes  

```r
leaf2 <- leaf1  
for (box in boxlist)
  leaf2 <- leaf2 %>% addRectangles2(list = box, fillColor = "transparent", color = "red")

leaf2
```

<!--html_preserve--><div id="htmlwidget-a50e52d8253a3ff103af" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-a50e52d8253a3ff103af">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircleMarkers","args":[[60.21616,59.616207,59.279728,58.648552,58.62969,58.641249,58.3663788,58.1746335,58.1423644,58.4962994,58.3427091,58.163,58.6411747,58.52963,58.47812,58.7398226,58.73132,59.45515,59.4816033,59.5722525,59.576908,59.545312,59.5439895,59.5162688,60.63837,60.6329787,60.6464892,60.64691,60.7891441,60.737,60.73495,61.07177,61.05082,61.09814,61.3554,61.3390986,61.36949,61.4530123,61.43958,61.4502879,61.579589,61.576175,61.5692813,61.90551,61.89168,61.9049497,62.1836823,62.187892,62.1956,62.13395936,62.13446,62.81276,62.84275111,63.0441,62.56469806,62.20221435],[11.353682,11.138558,11.134265,8.849526,8.85514,8.903564,7.5298291,7.5539046,7.5450547,7.1734313,7.2040544,7.088,6.1277959,6.03124,5.99556,5.5911722,5.528,6.39945,6.2700295,6.102345,6.120869,6.001525,5.98509,5.9296672,6.43797,6.4447748,6.1301133,6.00025,5.9713838,5.80307,5.80927,7.93884,7.63963,7.52163,6.12466,6.14075,5.68688,6.011804,6.00546,5.8963099,5.947009,5.894954,5.7910184,6.00212,7.10157,6.7320199,6.1957191,6.1453352,9.7749,9.96558684,10.14196,10.00828,9.90410022,9.7163,10.33456254,9.60163138],5,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2},null,null,["01. GLO3<br>11.353682, 60.21616","03. GLO2<br>11.138558, 59.616207","04. GLO1<br>11.134265, 59.279728","05. STO3<br>8.849526, 58.648552","06. STO2<br>8.85514, 58.62969","07. STO1<br>8.903564, 58.641249","08. MAN3<br>7.5298291, 58.3663788","09. MAN2<br>7.5539046, 58.1746335","10. MAN1<br>7.5450547, 58.1423644","11. LYG3<br>7.1734313, 58.4962994","12. LYG2<br>7.2040544, 58.3427091","13. LYG1<br>7.088, 58.163","14. BJE4<br>6.1277959, 58.6411747","15. BJE2<br>6.03124, 58.52963","16. BJE1<br>5.99556, 58.47812","17. ORR2<br>5.5911722, 58.7398226","18. ORR1<br>5.528, 58.73132","19. SUL2<br>6.39945, 59.45515","20. SUL1<br>6.2700295, 59.4816033","21. KVI20_7 (2)<br>6.102345, 59.5722525","23. KVI21_5<br>6.120869, 59.576908","24. KVI12_4<br>6.001525, 59.545312","25. KVI11_3 (2)<br>5.98509, 59.5439895","27. KVI17_1<br>5.9296672, 59.5162688","28. VOS4<br>6.43797, 60.63837","29. VOS3<br>6.4447748, 60.6329787","30. VOS2<br>6.1301133, 60.6464892","31. VOS1<br>6.00025, 60.64691","32. EKS4<br>5.9713838, 60.7891441","33. EKS2<br>5.80307, 60.737","34. EKS1<br>5.80927, 60.73495","35. LÆR3<br>7.93884, 61.07177","36. LÆR2<br>7.63963, 61.05082","37. LÆR1<br>7.52163, 61.09814","38. GAU5<br>6.12466, 61.3554","39. GAU4<br>6.14075, 61.3390986","40. GAU1<br>5.68688, 61.36949","41. JØL3<br>6.011804, 61.4530123","42. JØL2<br>6.00546, 61.43958","43. JØL1<br>5.8963099, 61.4502879","44. NAU3<br>5.947009, 61.579589","45. NAU4<br>5.894954, 61.576175","46. NAU2<br>5.7910184, 61.5692813","47. EID1<br>6.00212, 61.90551","48. STR2<br>7.10157, 61.89168","49. STR1<br>6.7320199, 61.9049497","50. ØRS2<br>6.1957191, 62.1836823","51. ØRS1<br>6.1453352, 62.187892","Gruve.F4<br>9.7749, 62.1956","Gruve.F5<br>9.96558684, 62.13395936","Gruve.F7<br>10.14196, 62.13446","Gruve.O3<br>10.00828, 62.81276","Gruve.O4<br>9.90410022, 62.84275111","Gruve.O5<br>9.7163, 63.0441","Gruve.Ya<br>10.33456254, 62.56469806","GruveF2<br>9.60163138, 62.20221435"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addRectangles","args":[61.25,5.55,61.65,6.35,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"transparent","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addRectangles","args":[60.5,5.7,60.9,6.5,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"transparent","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addRectangles","args":[59.4,5.8,59.7,6.5,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"transparent","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addRectangles","args":[58.4,5.4,58.9,6.2,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"transparent","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addRectangles","args":[58.55,8.75,58.7,9,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"transparent","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[58.1423644,63.0441],"lng":[5.4,11.353682]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



## 4. Prepare datasets with UTM coordinates  

### a. Stations, add UTM coordinates

```r
# The default transformation for 'add_transformed_coord' is from latlong to 
df_stations <- add_transformed_coord(df_stations, 
                                     name_x = "Long", name_y = "Lat",
                                     from = "longlat", to = "utm", to_zone = 32)
```

### b. Inset boxes, set UTM coordinates and dimensions  
From 'boxlist' to 'box_polygons'  

```r
# from 4 numbers, make data set with 2 lines, lower left and upper right corner  
get_corners <- function(box) 
  tibble(Long = sort(c(box$lng1, box$lng2)), Lat = sort(c(box$lat1, box$lat2)))

# from corners, make data set with 4 lines, one from each corner (anticlockwise from lower left)  
corners_to_polygons <- function(df)
  tibble(x = c(df$x[1], df$x[2], df$x[2], df$x[1]), y = c(df$y[1], df$y[1], df$y[2], df$y[2]))

# List of corners (with UTM coordinates)
box_corners <- boxlist %>% 
  purrr::map(get_corners) %>%
  purrr::map(add_transformed_coord, 
             name_x = "Long", name_y = "Lat",
             from = "longlat", to = "utm", to_zone = 32)

# Box 5 is very small (it turns out in part 9), we make it a bit bigger
# 5 km in all directions
box_corners[[5]]$x <- box_corners[[5]]$x + c(-7000, 7000)
box_corners[[5]]$y <- box_corners[[5]]$y + c(-5000, 5000)
box_corners[[5]]$Long <- NA  # now this is wrong, so we delete it (to be safe)
box_corners[[5]]$Lat <- NA   #    "

box_dim <- box_corners %>% purrr::map_dfr(~tibble(x = diff(.$x), y = diff(.$y)), .id = "boxnumber")


# Data frame of polygons (UTM coordinates)
box_polygons <- box_corners %>%
  purrr::map_dfr(corners_to_polygons, .id = "boxnumber")
```

### c. Save datasets

```r
saveRDS(df_stations, "Data/2020/07b_df_stations.rds")
saveRDS(box_dim, "Data/2020/07b_box_dim.rds")
saveRDS(box_polygons, "Data/2020/07b_box_polygons.rds")
```


## 5. Map with names, ggrepel

### a. Area outside boxes  

```r
df_stations_outside <- df_stations
for (box in boxlist){
  df_stations_outside <- df_stations_outside %>%
    filter(!(Long > box$lng1 & Long < box$lng2 & Lat > box$lat1 & Lat < box$lat2))
}

# df_stations_inside <- df_stations %>%
#   filter(!Shortname %in% df_stations_outside$Shortname)

nrow(df_stations)
```

```
## [1] 56
```

```r
nrow(df_stations_outside)
```

```
## [1] 25
```

```r
# nrow(df_stations_inside)

# Leaflet plot
if (FALSE){
  
  leaf3 <- leaflet(df_stations_outside) %>%
    addTiles() %>%
    addCircleMarkers(lng = df_stations_outside$Long, lat = df_stations_outside$Lat,
                     popup = paste0(
                       df_stations_outside$Shortname, "<br>",
                       df_stations_outside$Long, ", ", df_stations_outside$Lat, "<br>",
                       round(df_stations_outside$x, 0), ", ", 
                       round(df_stations_outside$y, 0)
                       ),
                     radius = 5)
  
  for (box in boxlist)
    leaf3 <- leaf3 %>% addRectangles2(list = box, fillColor = "transparent")
  
  leaf3
  
}
```

### b. Test plot 'outside' stations   
All stations, and labels for stations outside inset boxes

```r
gg1 <- gg_back +
  geom_point(
    data = df_stations, 
    aes(label = Shortname, shape = Stasjonstype)) +
  theme(  legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.box.background = element_rect(fill = "white", color = NA), 
          legend.margin = margin(6, 6, 6, 6),
          legend.key = element_rect(fill = "white", color = NA),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)
  ) +
  geom_label_repel(
    data = df_stations_outside, 
    aes(label = Shortname, shape = Stasjonstype), color = "blue3", label.padding = 0.15)
```

```
## Warning: Ignoring unknown aesthetics: label
```

```
## Warning: Ignoring unknown aesthetics: shape
```

```r
if (save_plots)
  ggsave2("{folder_fig}/07b_labelmap_test.png", gg1, width = 8, height = 12)

# gg1
```




## 6. Map with names, using geom_text_left/right   


### a. Build gg2 plot   
* Includes labels and rivers   
NOTE: The less frustrating way to build this code is to code in an ordinary R script (e.g.,  '07_Figures_2020_testmap.R')   
* Easier to 'try and fail' when the plot is made in a separate window    
* When finished, copy code back to this chunk  

```r
# Filter using entire Shortname:
group1 <- c("Gruve.O5","Gruve.O4","Gruve.O3","Gruve.Ya")
group2a <- c("GruveF2","Gruve.F4") 
group2b <- c("Gruve.F5","Gruve.F7") 
# Filter using the two first characters (using substr() on Shortname):
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
  geom_path(data = df_rivers_strahler7, 
            aes(group = group), col = rivercol) +
  geom_polygon(data = df_lakes_mjosa, 
               aes(group = group), col = rivercol, fill = rivercol) +
  geom_point(
    data = df_stations,
    aes(label = Shortname, shape = Stasjonstype, fill = Stasjonstype), size = rel(2)) +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(21,22,24)) +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(Shortname %in% group1), 
    xdist = 30E3, y_even_dist = 20E3, xadj_start = 7E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = df_stations %>% filter(Shortname %in% group2a), 
    xdist = 20E3, y_even_dist = 20E3, y_even_pos = 40E3, xadj_start = 7E3,
    order = "E to W",
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
    xdist = 40E3, xadj_start = 7E3, y_even_dist = 20E3, y_even_pos = 50E3, 
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group4b), 
    xdist = 50E3, xadj_start = 7E3, y_even_dist = 20E3, y_even_pos = -30E3, 
    order = "E to W",
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = df_stations %>% filter(substr(Shortname,1,2) %in% group5), 
    xdist = 35E3, xadj_start = 7E3, y_even_dist = 20E3, y_even_pos = -40E3, 
    order = "E to W",
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
```

```
## Warning: Ignoring unknown aesthetics: label
```

```r
gg2
```

```
## Warning: Removed 40481 row(s) containing missing values (geom_path).
```

![](07b_Figures_2020_labelmap_files/figure-html/unnamed-chunk-17-1.png)<!-- -->




### b. Map with inset boxes  (gg3a)   

```r
#
# Make data set for the letters beside the box
#
box_letter_pos <- box_polygons %>%
  group_by(boxnumber) %>%
  summarise(
    x = max(x) + 7E3,  # left of left edge 
    y = max(y),
    .groups = "drop")         # top edge (we use vjust to adjust the position)
box_letter_pos$letter <- LETTERS[as.numeric(box_letter_pos$boxnumber)]

#
# Add boxes and letters
#
gg3a <- gg2 +
  geom_polygon(data = box_polygons, aes(group = boxnumber),
               fill = NA, colour = "red3", size = rel(1.5)) +
  geom_text(data = box_letter_pos, aes(label = letter), 
            size = rel(8), colour = "red3", hjust = 0, vjust = 1) 

# gg3a

if (save_plots)
  ggsave2("{folder_fig}/07b_labelmap_test2.png", gg3a, width = 8, height = 12)
```

```
## Warning: Removed 40481 row(s) containing missing values (geom_path).
```



## 7. Prepare insets    

### a. Functions used  

```r
# Function for finding points inside box
point_inside <- function(df, xrange, yrange)
  df$x > xrange[1] & df$x < xrange[2] & df$y > yrange[1] & df$y < yrange[2] 

# Function making basic plot without labels
# Global: gg_back, df_rivers_strahler5, data_box, box_corners
make_subplot_nolabels <- function(i){
  gg_back +
    geom_path(data = df_rivers_strahler5, 
              aes(group = group), col = rivercol) +
    geom_polygon(data = df_lakes, 
               aes(group = group), col = rivercol, fill = rivercol) +
    geom_point(
      data = data_box[[i]], 
      aes(shape = Stasjonstype, fill = Stasjonstype), size = rel(2)) +
    scale_fill_brewer(palette = "Set1") +
    scale_shape_manual(values = c(21,22,24)) +
    annotate("text", label = LETTERS[i], 
             x = min(box_corners[[i]]$x) + 1200, y = max(box_corners[[i]]$y) - 1500, 
             size = rel(8), colour = "red3") +
    coord_fixed(xlim = box_corners[[i]]$x, ylim = box_corners[[i]]$y) +
    theme(legend.position = "none",
          plot.background = element_rect(
            colour = "black",               
            size = 1)
          )
          
}


# Function for making a test plot using gg_repel
# Global: gg_box and data_box
test_subplot <- function(i){
  gg <- gg_box[[i]] +
    geom_text_repel(
      data = data_box[[i]], 
      aes(label = Shortname), color = "blue3", 
      label.padding = 0.15, point.padding = 1)
  windows()
  print(gg)
}
```



## 8. Insets {.tabset}  
NOTE: The less frustrating way to build this code is to code in an ordinary R script (e.g.,  '07_Figures_2020_test_insets.R')   
* Easier to 'try and fail' when the plot is made in a separate window    
* When finished, copy code back to this chunk  

Prepare lists

```r
gg_box <- vector("list", length(boxlist))
data_box <- vector("list", length(boxlist))
```

Show each inset in markdown document?  

```r
show_insets <- FALSE
```


### Inset 1

```r
i <- 1

# df_rivers_strahler5 <- fix_river_errors(df_rivers_strahler5, limit_meters = 3000)

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
if (FALSE){
  test_subplot(i)
}

# Add 'group' (we will make different labels for each group)
data_box[[i]] <- data_box[[i]] %>%
  mutate(group = case_when(
    substr(Shortname,1,2) %in% c("43","46") ~ "left",
    substr(Shortname,1,2) %in% "45" ~ "above",
    TRUE ~ "right"))

# Add labels  
gg_box[[i]] <- gg_box[[i]] +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "right"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "left"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "above"),
    xdist = 4E3, same_x = FALSE, xadj_start = 900, 
    y_even_dist = 0, y_even_pos = 3E3,
    color = "blue3", segment.color = "blue3")

if (show_insets)
  gg_box[[i]] 
```

### Inset 2

```r
i <- 2

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
if (FALSE){
  test_subplot(i)
}

# Add labels  
gg_box[[i]] <- gg_box[[i]] +
  geom_text_repel(
      data = data_box[[i]], 
      aes(label = Shortname), color = "blue3", 
      label.padding = 0.15, point.padding = 1)
```

```
## Warning: Ignoring unknown parameters: label.padding
```

```r
if (show_insets)
  gg_box[[i]]
```


### Inset 3

```r
i <- 3

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
if (FALSE){
  test_subplot(i)
}

# Add 'group' (we will make different labels for each group)
data_box[[i]] <- data_box[[i]] %>%
  mutate(group = case_when(
    substr(Shortname,1,2) %in% c("19") ~ "left",
    substr(Shortname,1,2) %in% c("21") ~ "right up",
    substr(Shortname,1,2) %in% c("25") ~ "left up",
    TRUE ~ "right"))

# Add labels  
gg_box[[i]] <- gg_box[[i]] +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "right"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "left"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "right up"),
    xdist = 0.1E3, same_x = FALSE, xadj_start = 900, y_pos = 4E3,
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "left up"),
    xdist = 0.1E3, same_x = FALSE, xadj_start = 900, y_pos = 4E3,  
    color = "blue3", segment.color = "blue3")

  
if (show_insets)
  gg_box[[i]] 
```

### Inset 4

```r
i <- 4

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
if (FALSE){
  test_subplot(i)
}

# Add 'group' (we will make different labels for each group)
data_box[[i]] <- data_box[[i]] %>%
  mutate(group = case_when(
    substr(Shortname,1,2) %in% c("18") ~ "rightbelow",
    substr(Shortname,1,2) %in% c("14") ~ "left",
    TRUE ~ "right"))

# Add labels  
gg_box[[i]] <- gg_box[[i]] +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "right"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "left"),
    xdist = 3.5E3, same_x = FALSE, xadj_start = 900, 
    color = "blue3", segment.color = "blue3") +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "rightbelow"),
    xdist = 4E3, same_x = FALSE, xadj_start = 900, 
    y_even_dist = 0, y_even_pos = -3E3,
    color = "blue3", segment.color = "blue3")
  
if (show_insets)
  gg_box[[i]] 
```


### Inset 5

```r
i <- 5

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
if (FALSE){
  test_subplot(i)
}


# Add 'group' (we will make different labels for each group)
data_box[[i]] <- data_box[[i]] %>%
  mutate(group = case_when(
    substr(Shortname,1,2) %in% c("07") ~ "right",
    TRUE ~ "left"))

# Add labels  
gg_box[[i]] <- gg_box[[i]] +
  geom_text_right(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "right"),
    xdist = 2E3, same_x = FALSE, xadj_start = 400, 
    color = "blue3", segment.color = "blue3") +
  geom_text_left(
    aes(label = Shortname), 
    data = data_box[[i]] %>% filter(group == "left"),
    xdist = 2E3, same_x = FALSE, xadj_start = 400,  
    color = "blue3", segment.color = "blue3")

if (show_insets)
  gg_box[[i]] 
```


## 9. Entire area with insets   

### Test plot

```r
if (FALSE){
  gg4 <- ggdraw() +
    draw_plot(gg3a, x = 0.15, y = 0, scale = 0.7) +
    draw_plot(gg_box[[2]], x = 0.37, y = 0.6, width = 0.3, height = 0.3) +
    draw_plot(gg_box[[2]], x = 0.07, y = 0.6, width = 0.3, height = 0.3) +
    draw_plot(gg_box[[2]], x = 0.07, y = 0.4, width = 0.3, height = 0.3) +
    draw_plot(gg_box[[2]], x = 0.07, y = 0.2, width = 0.3, height = 0.3) +
    draw_plot(gg_box[[2]], x = 0, y = 0.2, width = 0.18, height = 0.18)
  
  fn <- glue("{folder_fig}/07b_labelmap_test_boxes.png")
  scale <- 1.5
  ggsave(fn, gg4, width = 18*scale, height = 25*scale, units = "cm", dpi = 500/scale)  
}
```


### Real plot

```r
# Positions on scale 0 to 1 (of lower left corner)
# These are strangely hard to set correctly
box_pos <- tibble::tribble(
  ~x,    ~y,
  0.37,  0.6,
  0.07,  0.6,
  0.07,  0.44,  # 
  0.07,  0.15,
  0.70,  0.15
)

# Size, given on scale 0 to 1 
# box_dim$x[2] = 0.3 in size:
box_dim$x_scale = box_dim$x/box_dim$x[2]*0.3
box_dim$y_scale = box_dim$y/box_dim$x[2]*0.3

# if (TRUE){
gg3b <- gg3a +
  theme(  legend.position = "none")

# }

gg4 <- ggdraw() +
  draw_plot(gg3b, x = 0.15, y = 0, scale = 0.7)
```

```
## Warning: Removed 40481 row(s) containing missing values (geom_path).
```

```r
for (i in 1:5){
  gg4 <- gg4 + draw_plot(
    gg_box[[i]], 
    x = box_pos$x[i], y = box_pos$y[i], 
    width = box_dim$x_scale[i], height = box_dim$y_scale[i]
    )
}
```

```
## Warning: Removed 312562 row(s) containing missing values (geom_path).
```

```
## Warning: Removed 312562 row(s) containing missing values (geom_path).

## Warning: Removed 312562 row(s) containing missing values (geom_path).

## Warning: Removed 312562 row(s) containing missing values (geom_path).

## Warning: Removed 312562 row(s) containing missing values (geom_path).
```

```r
# gg5 <- gg4 + draw_plot(gg_legend, x = 0.15, y = 0.5, scale = 0.7)


if (save_plots){
  fn <- glue("{folder_fig}/07b_labelmap_nolegend.png")
  scale <- 1.5
  ggsave(fn, gg4, width = 18*scale, height = 25*scale, units = "cm", dpi = 500/scale)  
}
```


### Make legend

```r
gg3_for_legend <- gg3a +
  theme(  legend.position = c(.1, .5),
          legend.justification = c("left", "bottom"),
          legend.box.just = "right",
          legend.box.background = element_rect(fill = "white", color = NA), 
          legend.margin = margin(6, 6, 6, 6),
          legend.key = element_rect(fill = "white", color = NA),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 18)
  )

gg_legend <- get_legend(gg3_for_legend)
```

```
## Warning: Removed 40481 row(s) containing missing values (geom_path).
```

```r
  grid.newpage()
  grid.draw(gg_legend)
```

![](07b_Figures_2020_labelmap_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```r
if (FALSE){
  fn <- glue("{folder_fig}/07b_labelmap_legend.png")
  png(fn, width = 7, height = 5, res = 400)
  dev.off()
}
```


## APPENDIX 1. Rivers   


```r
if (FALSE){
  
  nc_rivers <- st_read(
    "K:\\Kart\\ELVIS\\Hovedelv\\elvis_hovedelv.shp",
    query = "SELECT * FROM elvis_hovedelv WHERE STRAHLER >= 7")  # saves a little time
  #  UTM zone 33
  xtabs(~STRAHLER, nc_rivers)
  
  # Select large rivers
  nc_rivers_8 <- nc_rivers %>%
    filter(STRAHLER %in% 8)
  xtabs(~ELVENAVN, nc_rivers_8)
  
  # Extract coordinates and Transform from UTM33 to UTM32
  df_rivers_8 <- st_coordinates(nc_rivers_8) %>% 
    as.data.frame() %>%
    add_transformed_coord(
      name_x = "X", name_y = "Y", 
      from = "utm", from_zone = 33,
      to = "utm", to_zone = 32
    ) %>%
    select(-X, -Y) %>%
    rename(group = L2)
  
  # xtabs(~group, df_rivers_sel)
  
  df_rivers_8 <- fix_river_errors(df_rivers_8)
  
  ### Rank 8 rivers, plot    
  
  gg_back +
    geom_path(data = df_rivers_8, aes(group = group), col = "red") 
  
  
  #
  # Alternative, one cn use plotly
  #
  df_rivers_8$i <- 1:nrow(df_rivers_8)  # for showong in plotly plot
  gg <- ggplot() +
    geom_path(data = df_rivers_8, aes(x = x, y = y, group = group, text = paste(group, i)), col = "red")
  plotly::ggplotly(gg)
  
}
```

## APPENDIX 2. Lakes   





### Get data  

```r
if (FALSE){

  nc_lakes <- st_read("K:/Kart/N1000/VANN.shp")
  st_crs(nc_lakes) <- "+proj=utm +zone=33"  # set coordinate system
  
  # Plot without background, using geom_sf
  ggplot() +
    geom_sf(data = nc_lakes)
    
}
```

### Get coordinates (in zone 32)  

```r
if (FALSE){
    
  # Extract coordinates and Transform from UTM33 to UTM32
  df_lakes <- st_coordinates(nc_lakes) %>% 
    as.data.frame() %>%
    add_transformed_coord(
      name_x = "X", name_y = "Y", 
      from = "utm", from_zone = 33,
      to = "utm", to_zone = 32
    ) %>%
    select(-X, -Y) %>%
    rename(group = L2)


  gg_back +
    geom_polygon(data = df_lakes, aes(group = group), col = "red", fill = "blue3") 
  
}
```

### Get Mjøsa only  

```r
# Use plotly to find group for Mjøsa   
if (FALSE){

  gg <- ggplot(data = df_lakes, aes(x, y, group = group, text = group)) +
    geom_path(col = "red") +
    coord_fixed(xlim = c(570936, 625000), ylim = c(6676542, 6798102))
  plotly::ggplotly(gg, tooltip = "text")

  }

if (FALSE){

  # Mjøsa = group 3462, see code above 
  gg_back +
    geom_polygon(data = df_lakes %>% filter(group == 3462), 
                 aes(group = group), col = "red", fill = "blue3") +
    coord_fixed(xlim = c(570936, 625000), ylim = c(6676542, 6798102))

}
```








