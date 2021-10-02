

#
# Basic
#
df <- tibble(
  label = letters[1:4],
  x = rep(10,4), y = c(21,21.5,23,24)
)

# default: all numbers = 0, i.e. xend,yend = x,y
pos_text(df)$data_label    # positioning of text label
pos_text(df)$data_segment  # positioning of line segment from point to label

# position labels 2 units to the left (x direction) - xend changes
pos_text(df, xdist = 2)$data_label    # positioning of text label
pos_text(df, xdist = 2)$data_segment

# as above, but also put the labels 1 unit apart in the y direction 
pos_text(df, xdist = 2, y_even_dist = 1)$data_segment

# put the labels 1 unit apart, but also 5 units up
pos_text(df, xdist = 2, y_even_dist = 1, y_even_pos = 5)$data_segment

# Adjust x position of start and end of line segments
pos_text(df, xdist = 2, y_even_dist = 2, y_even_pos = 5, xadj_start = 0.1)$data_segment

# 'y_pos' adjusts all y positions, independent of the 'y_even' parameters 
pos_text(df, xdist = 2, y_pos = 3)$data_segment

# put them on the right side instead (xend changes)
pos_text(df, xdist = 2, y_even_dist = 2, y_even_pos = 5, side = "right")$data_segment

# position labels 2 units above (y direction) - xend changes
# debugonce(pos_text)
pos_text(df, xdist = 0)$data_label    # positioning of text label
pos_text(df, xdist = 0)$data_segment


x <- pos_text(df, xdist = 2, y_even_dist = 1, y_even_pos = 5, side = "right")$data_segment
x
dx <- x[["xend"]] - x[["x"]]
dx
dy <- x[["yend"]] - x[["y"]]
dy
dy/dx
x$x + 0.1
x$y + 0.1*dy/dx

#
# More advanced: using y_even_sel 
#
df <- tibble(
  label = letters[1:10],
  x = rep(1,10), y = c(1,1.5,2,2.5,3,7,9,10,10.5,11)
)

# 'y_even_dist' is set, but not 'y_even_sel' not set': all labels 1 unit apart 
pos_text(df, y_even_dist = 1)$data_segment

# Set 'y_even_sel' to a vector of integers: Label 6 to 10 (the 5 LOWEST labels) are set 1 unit apart
#   The rest are at the original y position
# Note that 6:10 refers to 'counting from top to bottom in the graph', not counting from the start
#   of the data set!
pos_text(df, y_even_dist = 1, y_even_sel = 6:10)$data_segment

# Set 'y_even_sel' to a list of integers: Label 1 to 3 and 6 to 10 are set 1 unit apart
pos_text(df, y_even_dist = 1, y_even_sel = list(1:3, 6:10))$data_segment

#
# Test using different variable names
#
df <- tibble(
  label = letters[1:8],
  xx = rep(1,8), yy = c(1,1.5,2,2.5,3,7,9,10)
)
pos_text(df, xvar = "xx", yvar = "yy")

#
# Direction
#
df <- tibble(
  code = paste0(letters[1:3], round(runif(3,1E5,9E5))),
  x = c(3,4,5), y = c(1.3,1.1,1.2)
)
# Default is to put the northernmost label at the top 
# May result in crossed lines, see below  
pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1)
# Alternative: southernmost label at the top 
pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1,
         order = "S to N")
# Easternmost label at the top - avoids crossing lines 
pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1,
         order = "N to S")


#
# Graphic test
#
df <- tibble(
  code = paste0(letters[1:10], round(runif(10,1E5,9E5))),
  x = seq(3,4,length = 10), y = c(1,1.2,1.5,1.9,3,7,9,10.5,10.8,11)
)

gg <- ggplot(df, aes(x,y)) +
  geom_point() +
  xlim(0,4) 
gg 
gg + geom_text_left(aes(label = code), data = df)

# position labels 2 units to the left (x direction) - xend changes
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3)

# Default is that same_x = TRUE (straight 'border' of labels)
# If FALSE, xdist is applied to each point independently
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, same_x = FALSE)

# 'y_pos' adjusts all y positions, independent of the 'y_even' parameters 
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, y_pos = 1)

# shorten line segment a bit in both ends
# NOTE: the name 'xadj_start' is  is given by the user as adjustment in x direction,
#  but the end of the line is also moved a bit in the y direction
# NOTE: If only xadj_start adjustment at the point) is given,
#   xadj_end (adjustment at the label) is set to the same)
gg + geom_text_left(aes(label = code), data = df, xdist = 0.5, xadj_start = 0.05)

# as above, but also put the labels 1 unit apart in the y direction 
gg + geom_text_left(aes(label = code), data = df, xdist = 0.5, xadj_start = 0.05, 
                    y_even_dist = 1)
# put the labels 1 unit apart, but also 2 units up
gg + geom_text_left(aes(label = code), data = df, xdist = 0.5, xadj_start = 0.05, 
                    y_even_dist = 1, y_even_pos = 2)

# put them on the right side instead (xend changes)
# Also put the labels a bit down
# Need to chnage x limits as well
gg + geom_text_right(aes(label = code), data = df, xdist = 0.5, xadj_start = 0.05, 
                    y_even_dist = 1, y_even_pos = -2) +
  xlim(2,6)

# Adjust the top three and the bottom 5, the rest keep the same position  
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, xadj_start = 0.05, 
                    y_even_dist = 0.6, y_even_sel = list(1:3, 6:10))


#
# Direction
#
df <- tibble(
  code = paste0(letters[1:3], round(runif(3,1E5,9E5))),
  x = c(3,4,5), y = c(1.3,1.1,1.2)
)
gg <- ggplot(df, aes(x,y)) + geom_point() + xlim(0,5) + ylim(-2, 5)
# Default is to put the northernmost label at the top 
# Results in crossed lines
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, 
                    y_even_dist = 0.3, y_even_pos = 1)
# Alternative: southernmost label at the top (still crossing lines)
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, 
                    y_even_dist = 0.3, y_even_pos = 1,
                    order = "S to N")
# Easternmost label at the top - avoids crossing lines  
gg + geom_text_left(aes(label = code), data = df, xdist = 0.3, 
                    y_even_dist = 0.3, y_even_pos = 1,
                    order = "E to W")
pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1,
         order = "S to N")

pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1,
         order = "N to S")

# debugonce(pos_text)
pos_text(data = df, xdist = 0.3, 
         y_even_dist = 0.3, y_even_pos = 1,
         order = "S to N")
