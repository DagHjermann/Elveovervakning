
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Utility functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Function for supplying file name to ggsave using a "glue-able" file name
#   Example (see part 6): 
#   ggsave2(paste0("{folder_fig}/07_", plotno, "_", label, ".png"), gg)
ggsave2 <- function(txt, ...)
  ggsave(filename = glue(txt), ...)

#
# Function for renaming variables
# Suitable also in functions and for tibbles
#

rename2 <- function(df, oldname, newname){
  names(df)[names(df) %in% oldname] <- newname
  df
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Utility functions, mapping ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# leaflet function for supplying long + lat to leaflet::addRectangles using a list
#
addRectangles2 <- function(list, ...){
  addRectangles(lng1 = list$lng1, lat1 = list$lat1, 
                lng2 = list$lng2, lat2 = list$lat2, ...)
}


#
# General function for transformation 
#
# Adds coordinates x and y
add_transformed_coord <- function(data, name_x = "long", name_y = "lat",
                                  from = "longlat", from_zone = NA, 
                                  to = "utm", to_zone = 32){
  
  data <- data %>%
    mutate(x = as.numeric(NA), y = as.numeric(NA))
  
  # Define crs strings for conversion between long/lat and UTM
  crs_from <- crs_string(from, from_zone)
  crs_to <- crs_string(to, to_zone)
  
  coordinate_exists <- !is.na(data[[name_x]])   # sp doesn't like NAs
  sp_original <- SpatialPoints(data[coordinate_exists, c(name_x, name_y)],
                               proj4string = CRS(crs_from)
  )
  sp_transformed <- spTransform(sp_original, CRS(crs_to))
  
  # Add transformed coords to data set
  data$x[coordinate_exists] <- sp_transformed@coords[,1]
  data$y[coordinate_exists] <- sp_transformed@coords[,2]
  
  data
  
}

# helper function for 'add_transformed_coord'
crs_string <- function(projection, zone = NA){
  if (projection == "longlat"){
    crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  } else if (projection == "utm"){
    crs <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m")
  } else {
    stop("'projection' must be longlat or utm")
  }
  crs
}

# test
# crs_string("longlat")
# crs_string("utm", 32)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for defining coordinates of insets ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Make list for box, given box center as input
make_box_center <- function(center, dlon = 0.25, dlat = 0.15){
  list(
    lng1 = center[1] - dlon/2, lat1 = center[2] - dlat/2, 
    lng2 = center[1] + dlon/2, lat2 = center[2] + dlat/2
  )
}

# As 'make_box_center', but just returns the code for making the 
#   list
make_box_code_center <- function(center, dlon = 0.25, dlat = 0.15){
  glue(
  "list(
    lng1 = {center[1] - dlon/2}, lat1 = {center[2] - dlat/2}, 
    lng2 = {center[1] + dlon/2}, lat2 = {center[2] + dlat/2}
  )")
}

get_center <- function(data, sel){
  if (length(sel) != nrow(data))
    stop("'sel' must have same length as number of rows in 'data'")
  message("Number of selected rows: ", sum(sel))
  data <- as.data.frame(data)
  c(
    round(mean(data[sel, "Long"], na.rm = TRUE), 4),
    round(mean(data[sel, "Lat"], na.rm = TRUE), 4)
  )
} 

# Make list for box, given data and selected ponts
# - center is defined from the selected points  
make_box <- function(data, sel, dlon, dlat){
  center <- get_center(data = data, sel = sel)
  make_box_center(center, dlon = dlon, dlat = dlat)
}

make_box_code <- function(data, sel, dlon, dlat){
  center <- get_center(data = data, sel = sel)
  make_box_code_center(center, dlon = dlon, dlat = dlat)
}


if (FALSE){
  make_box_code_center(c(8.8, 60))
  make_box_center(c(8.8, 60))
  get_center(df_stations, grepl("ALN", df_stations$Shortname))
  make_box_code(df_stations, grepl("ALN", df_stations$Shortname))
  make_box(df_stations, grepl("ALN", df_stations$Shortname))
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Fix river errors ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


fix_river_errors <- function(riverdata, limit_meters = 5000, print = FALSE){
  
  check <- riverdata %>%
    group_by(group) %>%
    mutate(dx = x - lag(x), dy = y - lag(y),
           dist = sqrt((dx^2) + (dy^2)))
  
  # Find big jump in the segment
  i <- which(check$dist > limit_meters)
  
  # Delete last point befre the big jump
  if (length(i) > 0){
    riverdata$x[i] <- NA
    riverdata$y[i] <- NA
  }
  
  if (print){
    cat("Deleted data at line", paste(i, collapse = ", "), "\n")
  }
  riverdata
  
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Labeling functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# Positioning for label placement, left or right side
#

# Codes for position:
#   - xdist + xadj_start (what's the difference?) - distance to point in x direction   
#   - y_even_dist (if set) - labels put in even y distance 
#   - y_even_pos - relative y position
#   - y_even_dist not set - labels put just straight to the left/right


# Ideas for improvement:
#   - same_x => straight
#   - and/or: slant = NA (straight = FALSE), slant = 0 (straght vertical), slant = 1 (same slope as points)
#   - xadj_start => point.padding? or line_shorten. Default = 0.05 (say) of median length taken at 45 degrees 
#   - include angle, line.shape, line.size, line.color, text.color, text.alpha, arrow
#   - also make 'geom_label_left' and 'geom_label_right'
#
# The way have used these functions in 7b and 7c is actually not very good.
# We should have used pos_text() in 7b and saved these in a list, and then plotted labels based on these 
#   positions in both 7b and 7c. (Instead we have copy-pasted code from 7b to 7c.) 
#
# Instead of using separate geom_text_right() and geom_text_left(),
#   an improvement would be to have a syntax withing one "sentence". 
# Present use:
#   geom_text_right(
#     aes(label = Shortname),
#     data = df_stations %>% filter(Shortname %in% group1),
#     [code for position and colors]) +
#   geom_text_left(
#     aes(label = Shortname),
#     data = df_stations %>% filter(Shortname %in% group2),
#     [code for position and colors, some of which are the same as the one above])
# Future:  
#   geom_text_place(
#     aes(label = Shortname),
#     data = df_stations,
#     [code for colors etc.],
#     x_distance = 3E3,     # for all labels, unless 'specify' specifies differety 
#     x_pos = 2E3,          #  "
#     specify = case_when(
#       Shortname %in% group1 ~ list(side = "right"),
#       Shortname %in% group2 ~ list(side = "left"),
#       Shortname %in% group3 ~ list(side = "right", x_pos = -2E3))
#     )
# In addition, one can instead use position_text_place() and later use the input in
#   ggplot2's geom_text and geom_label
#    
# Also see directlabels package, especially this? 
#   http://directlabels.r-forge.r-project.org/docs/utility.function/posfuns/calc.boxes.html

pos_text <- function(data, xvar = "x", yvar = "y",
                     side = "left", order = "n to s", same_x = TRUE, 
                     xdist = 0, y_pos = 0, 
                     y_even_dist = NULL, y_even_pos = 0, y_even_sel = NULL,
                     xadj_start = 0, xadj_end = xadj_start){ 
  data <- data[(!is.na(data[[xvar]])) & (!is.na(data[[yvar]])),]
  if (tolower(order) == "n to s"){
    data <- data[order(data[[yvar]], decreasing = TRUE),]
  } else if (tolower(order) == "s to n"){
    data <- data[order(data[[yvar]], decreasing = FALSE),]
  } else if (tolower(order) == "w to e"){
    data <- data[order(data[[xvar]], decreasing = FALSE),]
  } else if (tolower(order) == "e to w"){
    data <- data[order(data[[xvar]], decreasing = TRUE),]
  } else {
    stop("'order' must be 'N to S', 'W to E' or similar (lower case also accepted)" )
  }
  data_label <- data
  if (side %in% c("l", "left") & same_x){
    data_label[[xvar]] <- min(data_label[[xvar]]) - xdist
  } else if (side %in% c("r", "right") & same_x){
    data_label[[xvar]] <- max(data_label[[xvar]]) + xdist
  } else if (side %in% c("l", "left") & !same_x){
    data_label[[xvar]] <- data_label[[xvar]] - xdist
  } else if (side %in% c("r", "right") & !same_x){
    data_label[[xvar]] <- data_label[[xvar]] + xdist
  } else {
    stop("'side' must be 'left', 'right', 'l' or 'r'")
  }
  if (!is.null(y_even_dist) & is.null(y_even_sel)){
    y <- 
      max(data_label[[yvar]]) + 
      seq(0, length = nrow(data_label), by = -y_even_dist)
    data_label[[yvar]] <- y + 
      mean(range(data[[yvar]])) - 
      mean(range(y)) + 
      y_even_pos
  } else if (!is.null(y_even_dist) & class(y_even_sel) == "integer"){
    sel <- y_even_sel
    y_label_orig <- data_label[[yvar]][sel]
    y_label_even <- max(y_label_orig) + seq(0, length = length(y_label_orig), by = -y_even_dist)
    y_label_even <- y_label_even + mean(range(y_label_orig)) - mean(range(y_label_even)) + y_even_pos
    data_label[[yvar]][sel] <- y_label_even
  } else if (!is.null(y_even_dist) & class(y_even_sel) == "list"){
    for (sel in y_even_sel){
      y_label_orig <- data_label[[yvar]][sel]
      y_label_even <- max(y_label_orig) + seq(0, length = length(y_label_orig), by = -y_even_dist)
      y_label_even <- y_label_even + mean(range(y_label_orig)) - mean(range(y_label_even)) + y_even_pos
      data_label[[yvar]][sel] <- y_label_even
    }
  }
  # Apply y_pos
  data_label[[yvar]] <- data_label[[yvar]] + y_pos
  # Make 'data_segment'
  segmentend <- data_label
  names(segmentend)[names(data_label) == xvar] <- "xend"
  names(segmentend)[names(data_label) == yvar] <- "yend"
  data_segment <- bind_cols(
    data,
    segmentend[c("xend", "yend")]
  )
  # Apply 'xadj_start' and 'xadj_end'
  if (side %in% c("l", "left")){
    dx <- data_segment[["xend"]] - data_segment[[xvar]]
    dy <- data_segment[["yend"]] - data_segment[[yvar]]
    xmove <- xadj_start*cos(atan(abs(dy/dx)))
    data_segment[[xvar]] <- data_segment[[xvar]] - xmove
    data_segment[["xend"]] <- data_segment[["xend"]] + xmove
    data_segment[[yvar]] <- data_segment[[yvar]] - dy/dx*xmove
    data_segment[["yend"]] <- data_segment[["yend"]] + xmove*dy/dx
  } else if (side %in% c("r", "right")){
    dx <- data_segment[["xend"]] - data_segment[[xvar]]
    dy <- data_segment[["yend"]] - data_segment[[yvar]]
    xmove <- xadj_start*cos(atan(abs(dy/dx)))
    data_segment[[xvar]] <- data_segment[[xvar]] + xmove
    data_segment[["xend"]] <- data_segment[["xend"]] - xmove
    data_segment[[yvar]] <- data_segment[[yvar]] + dy/dx*xmove
    data_segment[["yend"]] <- data_segment[["yend"]] - xmove*dy/dx
  }
  list(data_label = data_label, data_segment = data_segment)
  # data_segment
}


#
# Making labels, left side
#
geom_text_left <- function(..., data, xvar = "x", yvar = "y", 
                           order = "n to s", same_x = TRUE,
                           xdist = 0, y_pos = 0, 
                           y_even_dist = NULL, y_even_pos = 0, y_even_sel = NULL,
                           xadj_start = 0, xadj_end = xadj_start,
                           segment.color = "black"){
  position_list <- pos_text(data = data, xvar = xvar, yvar = yvar, order = order, same_x = same_x, 
                            xdist = xdist, y_pos = y_pos, 
                            y_even_dist = y_even_dist, y_even_pos = y_even_pos, y_even_sel = y_even_sel,
                            xadj_start = xadj_start, xadj_end = xadj_end,
                            side = "left")
  list(
    geom_text(data = position_list$data_label, hjust = 1, ...),
    geom_segment(data = position_list$data_segment, aes(xend = xend, yend = yend), 
                 color = segment.color)
  )
}



#
# Making labels, left side
#
geom_text_right <- function(..., data, xvar = "x", yvar = "y", 
                            order = "n to s", same_x = TRUE,
                           xdist = 0, y_pos = 0, 
                           y_even_dist = NULL, y_even_pos = 0, y_even_sel = NULL,
                           xadj_start = 0, xadj_end = xadj_start,
                           segment.color = "black"){
  position_list <- pos_text(data = data, xvar = xvar, yvar = yvar, order = order, same_x = same_x,
                            xdist = xdist, y_pos = y_pos, 
                            y_even_dist = y_even_dist, y_even_pos = y_even_pos, y_even_sel = y_even_sel,
                            xadj_start = xadj_start, xadj_end = xadj_end,
                            side = "right")
  list(
    geom_text(data = position_list$data_label, hjust = 0, ...),
    geom_segment(data = position_list$data_segment, aes(xend = xend, yend = yend), 
                 color = segment.color)
  )
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Pie functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

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


make_pie <- function(measurements_present, col = colorscheme1, col_lacking = "grey80"){
  col[!measurements_present] <- col_lacking
  pie_data <- data.frame(x = vars, y = rep(1,6))
  ggplot(pie_data, aes(x=1, y, fill=x)) + 
    geom_bar(stat="identity", color = "black") + coord_polar(theta="y") +
    scale_fill_manual(values = col) +
    theme_void() + theme(legend.position="none") + theme_transparent()
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make one pie from a vector of colours
#
# Pies start at top 12:00 and goes anti-clockwise
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_pie_from_colours <- function(cols){
  N <- length(cols)
  pie_data <- tibble(x = letters[1:N], y = rep(1,N), no = 1:N) %>% as.data.frame()
  pie_data$z = paste(pie_data$no, pie_data$cols, sep = "_")
  ggplot(pie_data, aes(x=1, y, fill = z)) + 
    geom_bar(stat="identity", color = "black", size = 1) + coord_polar(theta="y") +
    scale_fill_manual(values = cols) +
    theme_void() + theme(legend.position="none") + theme_transparent()
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# BArplot function ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# Make horizontal barplot for data_index (column names: varname_index, varname_navn) on level Vannforekomst
#
make_barplot_index <- function(data_index, varname_index, 
                               scale_label, 
                               varname_navn = "Vannforekomst, kortnavn"){
  
  data_index <- rename2(data_index, oldname = varname_navn,  newname = "Rapportnavn")
  data_index <- rename2(data_index, oldname = varname_index, newname = "Indeksverdi")
  data_index <- as.data.frame(data_index)
  
  data_index$Tilstandsklasse <- data_index[[paste0(varname_index, "_klasse")]]

  # Get Rapportnavn 01 on top
  data_index$Rapportnavn <- factor(data_index$Rapportnavn, levels = rev(data_index$Rapportnavn))
  
  # Plot
  gg <- ggplot(data_index, aes(Rapportnavn, Indeksverdi, fill = Tilstandsklasse)) + 
    # geom_hline(yintercept = seq(0, 1, 0.2), size = rel(0.5), linetype = 2) +
    geom_hline(yintercept = 0.6, size = rel(1), linetype = 1) +
    geom_col(width = 0.75) +
    scale_fill_manual("Tilstandsklasse", values = class_colors, drop = FALSE) +
    scale_y_continuous(minor_breaks = seq(0, 1, 1), breaks = seq(0, 1, 0.2), 
                       limits = c(0,1.06), expand = c(0,0)) +         # limits + expand: no space on left side + a little space on right side 
    coord_flip() + 
    theme(axis.text.y = element_text(hjust = 0)) +
    theme(legend.position = "bottom") +
    labs(x = "", y = scale_label) +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          panel.grid.major.x = element_line(color = "black", linetype = 1),
          panel.grid.major.y = element_line(color = "white"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  gg
}

