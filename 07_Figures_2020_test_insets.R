
#
# SUBPLOT i
#


i <- 3

data_box[[i]] <- df_stations %>%
  filter(point_inside(., box_corners[[i]]$x, box_corners[[i]]$y))

gg_box[[i]] <- make_subplot_nolabels(i)

if (FALSE){
  test_subplot(i)
}

data_box[[i]] <- data_box[[i]] %>%
  mutate(group = case_when(
    substr(Shortname,1,2) %in% c("07") ~ "right",
    TRUE ~ "left"))

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

gg_box[[i]] 


if (FALSE){
  scaling <- 1/10000
  scaling <- 1/5000
  windows(diff(box_corners[[i]]$x)*scaling, diff(box_corners[[i]]$y)*scaling)
  gg_box[[i]] 
}

test


windows()
gg_box[[i]]

gg_box[[i]]

  


