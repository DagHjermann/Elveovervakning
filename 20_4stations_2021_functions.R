
#
# Get OpenStreetmap map, for one river   
# Input:
# - data = data incluning variables "Elv", "Long", "Lat"  
# - dlon, dlat is "distance" from the middle coordinate    
#
get_map <- function(river, data, dlon, dlat, maptype = 'osm'){
  
  df <- data %>%
    filter(Elv == river)
  
  p1 <- c(mean(range(df$Lat)) + dlat/2, mean(range(df$Long)) - dlon/2)
  p2 <- c(mean(range(df$Lat)) - dlat/2, mean(range(df$Long)) + dlon/2)
  
  mp_webmerc <- openmap(p1, p2, type = maptype)  
  
  mp_webmerc
  
}

# TEST
if (FALSE) 
  get_map("Moelva", data = dat, dlon = 0.10, dlat = 0.05)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Change raw data to spatial sf data on Web Mercator projection
# Input: data incluning variables "Elv", "Long", "Lat"   
# Output : spatial sf data   
get_data_webmercator <- function(data){
  datsf_longlat <- sf::st_as_sf(
    data,
    coords = c("Long", "Lat"), 
    crs = crs_longlat, 
    agr = "constant")  
  
  # Transform to Web mercator
  datsf_wm <- st_transform(datsf_longlat, 3857)  
  
  datsf_wm
  
}

# TEST
if (FALSE) 
  get_data_webmercator(dat)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Get data for plotting pies
# Input = spatial sf data 
# Output = ordinary data frame for one river, including 
#   coordinates X and Y
get_riverdata <- function(river, sfdata_webmercator){
  
  # As in 3a, but use datsf_wm instead of datsf
  river_data <- sfdata_webmercator %>%
    filter(Elv == river)
  
  coord <- river_data %>% 
    st_coordinates() %>%
    as.data.frame()
  
  river_data <- bind_cols(river_data, coord)
  
  river_data
  
}

# TEST
if (FALSE) 
  get_riverdata("Moelva", sfdata_webmercator = datsf)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# plot_river1 
# - plots background map plus pies (no river lines)
#
# Input: data incluning variables "Elv", "Long", "Lat" plus three columns for color 
# - NOTE: hard-coded for plotting the 3 columns Påvekst_col, HBI_col, Bunndyr_col


plot_river1 <- function(river, dlon, dlat, radius = 200, 
                       aspect_adjustment = 0.8, 
                       data, 
                       maptype = 'osm'){
  
  data_wm <- get_data_webmercator(data)
  
  riverdata <- get_riverdata(
    river = river, 
    sfdata_webmercator = data_wm)
  
  mp_webmerc <- get_map(
    river = river, 
    data = data, 
    dlon = dlon, dlat = dlat, 
    maptype = maptype)
  
  
  # Plot map, plus points as test
  plot(mp_webmerc)  
  
  # Aspect from plot dimensions (used in the original version of floating.pie)
  aspect1 <- par("pin")[1]/par("pin")[2]
  
  # Plot pies   
  for (i in 1:nrow(riverdata))
    floating.pie(riverdata$X[i], riverdata$Y[i], c(1,1,1), radius = radius,
                 col=c(riverdata$Påvekst_col[i], 
                       riverdata$HBI_col[i], 
                       riverdata$Bunndyr_col[i]), 
                 aspect = aspect1*0.8)  # set by trial
  
}

# TEST
if (FALSE)
  plot_river1("Moelva", dlon = 0.10, dlat = 0.05, data = dat)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# from package 'plotrix'
# modified version, addin the possibility to set 'aspect'  
#
floating.pie <- function (xpos = 0, ypos = 0, x, edges = 200, radius = 1, col = NULL, 
                          startpos = 0, shadow = FALSE, 
                          shadow.col = c("#ffffff", "#cccccc"), explode = 0, 
                          aspect = NULL, ...)     # NEW
{
  if (is.null(dev.list)) 
    plot(0, xlim = c(-1.5, 1.5) * radius + xpos, ylim = c(-1.5, 
                                                          1.5) * radius + ypos, type = "n", axes = FALSE, 
         xlab = "", ylab = "")
  if (!is.numeric(x)) 
    stop("floating.pie: x values must be numeric.")
  validx <- which(!is.na(x) & x > 0)
  col <- col[validx]
  x <- c(0, cumsum(x[validx])/sum(x[validx]))
  dx <- diff(x)
  nx <- length(dx)
  if (is.null(col)) 
    col <- rainbow(nx)
  else if (length(col) < nx) 
    col <- rep(col, nx)
  xylim <- par("usr")
  # THIS PART IS MODIFIED, IN ORDERR TO ALLOW FOR USER-SET ASPECT
  if (is.null(aspect)){
    plotdim <- par("pin")
    aspect <- plotdim[1]/plotdim[2]
  }
  yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * 
    aspect
  # END OF CHANGE
  bc <- 2 * pi * (x[1:nx] + dx/2) + startpos
  if (shadow && all(explode == 0)) {
    xc <- c(cos(seq(0, 2 * pi, length = edges)) * radius + 
              xpos)
    yc <- c(sin(seq(0, 2 * pi, length = edges)) * yradius + 
              ypos)
    polygon.shadow(xc, yc, col = shadow.col)
  }
  if (length(explode) < nx) 
    explode <- rep(explode, nx)
  for (i in 1:nx) {
    n <- max(2, floor(edges * dx[i]))
    t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos
    xc <- c(cos(t2p) * radius + xpos, xpos)
    yc <- c(sin(t2p) * yradius + ypos, ypos)
    if (explode[i]) {
      xc <- xc + cos(bc[i]) * explode[i]
      yc <- yc + sin(bc[i]) * explode[i]
    }
    polygon(xc, yc, col = col[i], ...)
    t2p <- 2 * pi * mean(x[i + 0:1]) + startpos
    xc <- cos(t2p) * radius
    yc <- sin(t2p) * radius
  }
  invisible(bc)
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# From "91_WMS_kartverket_functions.R"
# Here: 
#   "K:\Avdeling\214-Oseanografi\DHJ\Prosjekter\Referanseelver\91_test_WMS2\91_WMS_kartverket_functions.R"


#
# function for getting river geometry data
#

library(jsonlite)

get_riverlines <- function(WaterbodyID){
  txt1 <- "https://vann-nett.no/arcgis/rest/services/WFD/Vannforekomster/FeatureServer/3/query?f=json&where=WaterBodyID%20%3D%20%27"
  txt2 <- "%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*"
  url <- paste0(txt1, WaterbodyID, txt2)
  result_list <- fromJSON(url)
  result_list$features$geometry$paths %>% purrr::map_dfr(~data.frame(x = .[,,1], y = .[,,2]), .id = "ID")
  # result_list$features$geometry$paths
}

if (FALSE)
  get_riverlines("002-894-R") %>% head()

#
# function for getting river geometry data as well as RiverBasinID and CatchmentID
# 
# called 'get_riverdata' in "91_WMS_kartverket_functions.R"
#
get_riverlines_data <- function(WaterbodyID){
  txt1 <- "https://vann-nett.no/arcgis/rest/services/WFD/Vannforekomster/FeatureServer/3/query?f=json&where=WaterBodyID%20%3D%20%27"
  txt2 <- "%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*"
  url <- paste0(txt1, WaterbodyID, txt2)
  result_list <- fromJSON(url)
  list(RiverBasinID = result_list$features$attributes$RiverBasinID[1],
       CatchmentID = result_list$features$attributes$CatchmentID[1],
       geometry_df = result_list$features$geometry$paths %>% 
         purrr::map_dfr(~data.frame(x = .[,,1], y = .[,,2]), .id = "ID")
  )
}
if (FALSE)
  get_riverlines_data("002-894-R") %>% str(1)


#
# Input: the output of 'get_riverlines_data' (a list)
# Output: a data frame with 3 columns: ID (line number), X and Y
#   Note: the lines have to be plotted for each separate ID
#

get_riverlines_webmerc <- function(vannforekomst_object){
  
  # For easy plotting in mapvio 
  rivergeom_utm <- sf::st_as_sf(
    vannforekomst_object$geometry_df,
    coords = c("x", "y"), 
    crs = crs_utm33, 
    agr = "constant")  
  
  # Transform to Web mercator
  rivergeom_wm <- st_transform(rivergeom_utm, 3857)  
  
  # Retrieve coordinates
  river_coord <- st_coordinates(rivergeom_wm)
  river_coord <- data.frame(river_coord)
  river_coord$ID <- as.data.frame(rivergeom_utm)[["ID"]]

  # Return
  river_coord
  
}


# TEST
if (FALSE){
  obj <- get_riverlines_data("002-2590-R")
  debugonce(get_riverlines_webmerc)
  coo <- get_riverlines_webmerc(obj)
  head(coo)
}

#
# Get text positions 
#
get_textpos_webmerc <- function(river_coord, 
                                text_position = "right", 
                                text_distance = 200){
  
  # text_distance is given by 1 number (the usual) but also 
  #   2 numbers (where the second number is the distance in the other direction)
  # EDIT: this doesn't make sense as in the main script, text_distance is given as a vector, not a list
  # - it was a good idea and it's not so hard to chang, but I dont bother now
  
  #if (length(text_distance) == 1){      
  text_distance <- c(text_distance, 0)
  #}
  
  box <- list(
    p1 = list(X = min(river_coord[,"X"]), Y = min(river_coord[,"Y"])),  # lower left
    p2 = list(X = max(river_coord[,"X"]), Y = max(river_coord[,"Y"])))  # upper right
  
  text_position <- paste0("^", text_position)
  
  if (grepl(text_position, "right")){
    text_coord <- list(X = box$p2$X + text_distance[1], 
                       Y = (box$p1$Y + box$p2$Y)/2 + text_distance[2], 
                       adj = 0) 
  } else if (grepl(text_position, "left")){
    text_coord <- list(X = box$p1$X - text_distance[1], 
                       Y = (box$p1$Y + box$p2$Y)/2 + text_distance[2], 
                       adj = 1) 
  } else if (grepl(text_position, "below")){
    text_coord <- list(X = (box$p1$X + box$p2$X)/2 + text_distance[2], 
                       Y = box$p1$Y - text_distance[1], 
                       adj = 0.5) 
  } else if (grepl(text_position, "above")){
    text_coord <- list(X = (box$p1$X + box$p2$X)/2 + text_distance[2], 
                       Y = box$p2$Y + text_distance[1], 
                       adj = 0.5) 
  } else {
    stop("'test_position' must be 'right', 'left', 'above', 'below' or their starting letter(s)") 
  }
  
  text_coord
  
}

# TEST
if (FALSE){
  obj <- get_riverlines_data("002-2590-R")
  obj2 <- get_river_plotdata(obj)
  str(obj2, 1)
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# plot_river2 
# - plots background map, pies, river lines and name of Vannforekomst
#
# Input: data incluning variables "Elv", "Long", "Lat" plus three columns for color 
# - NOTE: hard-coded for plotting the 3 columns Påvekst_col, HBI_col, Bunndyr_col


plot_river2 <- function(river, dlon, dlat, radius = 200, 
                        aspect_adjustment = 0.8, 
                        river_col = "status", river_lwd = 3,
                        id_pos = "right", id_distance = 300, id_size = 0.8,
                        data, 
                        maptype = 'osm'){
  
  data_wm <- get_data_webmercator(data)
  
  riverdata <- get_riverdata(
    river = river, 
    sfdata_webmercator = data_wm)
  
  mp_webmerc <- get_map(
    river = river, 
    data = data, 
    dlon = dlon, dlat = dlat, 
    maptype = maptype)
  
  
  # Plot map, plus points as test
  plot(mp_webmerc)  
  
  # Get name (ID) of Vannforekomster
  vannfor_ids <- unique(riverdata$`Vannforekomst ID`) %>% sort()
  
  # id_pos and id_distance can be be given as a single text/number,
  #   or several texts/numbers (one per Vannforekomst)
  # if given as a single text/number, it is elongated to length of Vannforekomst
  if (length(id_pos) != length(vannfor_ids)){
    id_pos = rep(id_pos, length(vannfor_ids))
  }
  if (length(id_distance) != length(vannfor_ids)){
    id_distance = rep(id_distance, length(vannfor_ids))
  }
  
  # For each Vannforekomst
  for (j in seq_along(vannfor_ids)){
    
    # Get river data (a list with both coordinates, RiverBasinID and CatchmentID)
    vannfor_list <- get_riverlines_data(vannfor_ids[j])
    
    # Get coordinates on Web Mercator project, as data frame (incl. ID column) 
    river_coord_all <- get_riverlines_webmerc(vannfor_list)
    
    # Split 'river_coord_all' into a list of data frames (one per segment)
    river_coord_list <- river_coord_all %>% split(.$ID)
    
    # river_col <- "blue"
    # River color
    if (tolower(river_col) == "status"){
      river_col2 <- riverdata %>%
        filter(`Vannforekomst ID` %in% vannfor_ids[j]) %>%
        pull(Combined_col)
      river_col2 <- river_col2[1]
    } else {
      river_col2 <- river_col
    }
    
    # Plot river, segment by segment
    for (river_coord in river_coord_list)
      lines(river_coord[,"X"], river_coord[,"Y"], col = river_col2, lwd = river_lwd)
    
    # Get positions for text
    text_coord <- get_textpos_webmerc(river_coord_all, 
                                      id_pos[j], id_distance[j])
    
    # Plot text 
    text(text_coord$X, text_coord$Y, vannfor_ids[j], 
         adj = text_coord$adj, cex = id_size)
    
    # Aspect from plot dimensions (used in the original version of floating.pie)
    aspect1 <- par("pin")[1]/par("pin")[2]
  }
  
  # For each station - plot pies   
  for (i in 1:nrow(riverdata))
    floating.pie(riverdata$X[i], riverdata$Y[i], c(1,1,1), radius = radius,
                 col=c(riverdata$Påvekst_col[i], 
                       riverdata$HBI_col[i], 
                       riverdata$Bunndyr_col[i]), 
                 aspect = aspect1*0.8)  # set by trial
  
  
  invisible(NULL)
  
}

# TEST
if (FALSE)
  plot_river1("Moelva", dlon = 0.10, dlat = 0.05, data = dat)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Original version form plotrix
# from package 'plotrix'
#

floating.pie_ORIGINAL <- function (xpos = 0, ypos = 0, x, edges = 200, radius = 1, col = NULL, 
          startpos = 0, shadow = FALSE, 
          shadow.col = c("#ffffff","#cccccc"), explode = 0, ...) 
{
  if (is.null(dev.list)) 
    plot(0, xlim = c(-1.5, 1.5) * radius + xpos, ylim = c(-1.5, 
                                                          1.5) * radius + ypos, type = "n", axes = FALSE, 
         xlab = "", ylab = "")
  if (!is.numeric(x)) 
    stop("floating.pie: x values must be numeric.")
  validx <- which(!is.na(x) & x > 0)
  col <- col[validx]
  x <- c(0, cumsum(x[validx])/sum(x[validx]))
  dx <- diff(x)
  nx <- length(dx)
  if (is.null(col)) 
    col <- rainbow(nx)
  else if (length(col) < nx) 
    col <- rep(col, nx)
  xylim <- par("usr")
  plotdim <- par("pin")
  yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * 
    plotdim[1]/plotdim[2]
  bc <- 2 * pi * (x[1:nx] + dx/2) + startpos
  if (shadow && all(explode == 0)) {
    xc <- c(cos(seq(0, 2 * pi, length = edges)) * radius + 
              xpos)
    yc <- c(sin(seq(0, 2 * pi, length = edges)) * yradius + 
              ypos)
    polygon.shadow(xc, yc, col = shadow.col)
  }
  if (length(explode) < nx) 
    explode <- rep(explode, nx)
  for (i in 1:nx) {
    n <- max(2, floor(edges * dx[i]))
    t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos
    xc <- c(cos(t2p) * radius + xpos, xpos)
    yc <- c(sin(t2p) * yradius + ypos, ypos)
    if (explode[i]) {
      xc <- xc + cos(bc[i]) * explode[i]
      yc <- yc + sin(bc[i]) * explode[i]
    }
    polygon(xc, yc, col = col[i], ...)
    t2p <- 2 * pi * mean(x[i + 0:1]) + startpos
    xc <- cos(t2p) * radius
    yc <- sin(t2p) * radius
  }
  invisible(bc)
}
