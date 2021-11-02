
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Collect ggplots in a single file 
#
# NOTE: must first run '07c_Figures_2021_labelmap.Rmd' through part 8, plus Northern Norway part   
#
# Figures later moved to 
# C:\Data\seksjon 318\Elveovervakning\Figures\2021_main\test
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# parameters in draw_plot() are to some degree set on a trial-and-error basis 
# - as long as one cannot place every plot in an otherwise clean rectangle,
#   where other figures are allowed to enter (as in 'gtable'), it needs to be this way
# - see links at bottom of the gtable vignette (https://cran.r-project.org/web/packages/gridExtra/vignettes/gtable.html)  
# - example, this one: https://stackoverflow.com/questions/17736434/aligning-distinct-non-facet-plots-in-ggplot2-using-rpy2-in-python/17768224#17768224 
#
# However, what could make this less painful/slow is to make "mock" figures with just a rectangle 
#  (or simple polygon around the coast? ) replacing the actual maps. TODO.
#
# (Note that the coastline/county map can be simplied using 'st_simplify', see
#  https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/)

# For reading/showing the finished file 
library(magick)
# - use image_read + print to show in RStudio viewer
# - the click button in top right corner of viewer to show plot in browser
# - when plot is updated re

# set scale parameter - defines size of plot (and thereby the size of symbols and letters on the plot)  
scale <- 1.5

#
# Tried to make function, but for some reason it doesn't work  
# - so we just run those lines separately (after )
#
# plot_to_file <- function(ggobject, filename = "07c_test.png", scale = 1.5, res = 200){
#   png(filename = filename, width = 18*scale, height = 25*scale, units = "cm", res = res)
#   Sys.sleep(0.5)
#   ggobject
#   Sys.sleep(0.5)
#   dev.off()
# }


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Main map only ----
#
# gg3b
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

ggcomb <- ggdraw() +
  draw_plot(gg3b, x = -0.20, y = -0.17, scale = 0.8)

# Plot to file
png(filename = "07c_test1.png", width = 18*scale, height = 25*scale, units = "cm", res = 200)
ggcomb
dev.off()

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test1.png')
print(plt)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# N.Norway (Finnmark) only ----
#
# gg2_north
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

ggcomb <- ggdraw() +
  draw_plot(gg2_north, x = 0, y = 1.36, vjust = 1, scale = 0.55)

# Plot to file
png(filename = "07c_test1.png", width = 18*scale, height = 25*scale, units = "cm", res = 200)
ggcomb
dev.off()

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test1.png')
print(plt)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Main + N.Norway ----
#
# gg3b + gg2_north
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

ggcomb <- ggdraw() +
  draw_plot(gg3b, x = -0.35, y = -0.15, scale = 1.12) +
  draw_plot(gg2_north, x = 0.15, y = 1.32, vjust = 1, scale = 0.63)

# Plot to file
scale <- 1.5
png(filename = "07c_test1a.png", width = 18*scale, height = 25*scale, units = "cm", res = 200)
ggcomb
dev.off()

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test1a.png')
print(plt)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Boxes only ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

box_pos <- tibble::tribble(
  ~x,    ~y,
  0.78,  0.15 -0.10,
  0.6,  0.551 -0.10,
  0.6,  0.291 -0.10,  # 
  0.6,  0.15 -0.10
)

# Size, given on scale 0 to 1 
# box_dim$x[2] = 0.3 in size:
box_dim$x_scale = box_dim$x/box_dim$x[2]*0.3
box_dim$y_scale = box_dim$y/box_dim$x[2]*0.3

ggcomb <- ggdraw()
for (i in seq_along(gg_box)){
  ggcomb <- ggcomb + draw_plot(
    gg_box[[i]], 
    x = box_pos$x[i], y = box_pos$y[i], 
    width = box_dim$x_scale[i], height = box_dim$y_scale[i]
  )
}

# Plot to file
scale <- 1.5
png(filename = "07c_test2.png", width = 18*scale, height = 25*scale, units = "cm", res = 200)
ggcomb
dev.off()

# beepr::beep(3)

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test2.png')
print(plt)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Main + N.Norway + boxes ----
#
# gg3b + gg2_north
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Set box_pos and x_scale/y_scale
box_pos <- tibble::tribble(
  ~x,    ~y,
  0.78,  0.15 -0.10,
  0.6,  0.551 -0.10,
  0.6,  0.291 -0.10,  # 
  0.6,  0.15 -0.10
)

# Size, given on scale 0 to 1 
# box_dim$x[2] = 0.3 in size:
box_dim$x_scale = box_dim$x/box_dim$x[2]*0.3
box_dim$y_scale = box_dim$y/box_dim$x[2]*0.3

# Make ggcomb
ggcomb <- ggdraw() +
  draw_plot(gg3b, x = -0.35, y = -0.15, scale = 1.12) +
  draw_plot(gg2_north, x = 0.15, y = 1.32, vjust = 1, scale = 0.63)
for (i in seq_along(gg_box)){
  ggcomb <- ggcomb + draw_plot(
    gg_box[[i]], 
    x = box_pos$x[i], y = box_pos$y[i], 
    width = box_dim$x_scale[i], height = box_dim$y_scale[i]
  )
}

# Plot to file
scale <- 1.5
png(filename = "07c_test2.png", width = 18*scale, height = 25*scale, units = "cm", res = 200)
ggcomb
dev.off()

# beepr::beep(2)

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test2.png')
print(plt)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Legend only ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (FALSE){
  # cropping legend made in powerpoint (ONLY DO THIS ONCE)
  plt <- image_read('Qualitymap_2021_legend.png')
  plt_cropped <- image_trim(plt)
  image_write(plt_cropped, path = "Qualitymap_2021_legend_cropped.png", format = "png")
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Main + N.Norway + boxes + legend ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# wait until "Plot to file" is finished until you plot the next
plt <- image_read('07c_test2.png')
plt_legend <- image_read('Qualitymap_2021_legend_cropped.png')

plt_legend_scale <- image_scale(plt_legend, "400x")  # = x pixels width

# bigdatafrink <- image_scale(image_rotate(image_background(frink, "none"), 300), "x200")

plt_with_legend <- image_composite(plt, plt_legend_scale, offset = "+280+200")
image_write(plt_with_legend, path = "07c_test4.png", format = "png")


# If happy with the result:
# file.copy("07c_test4.png", "Figures/2021_main/07c_qualitymap_complete.png")




              
              