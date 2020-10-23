

# 
windows(22,18)

library(cowplot)
ggdraw() +
  draw_plot(gg3, x = 0.3, y = 0, width = 0.7, height = 1) +
  draw_plot(gg_box[[1]], x = 0.3, y = 0.7, width = 0.4, height = 0.3) +
  draw_plot(gg_box[[2]], x = 0.05, y = 0.7, width = 0.3, height = 0.3) +
  draw_plot(gg_box[[3]], x = 0.05, y = 0.4, width = 0.3, height = 0.3) +
  draw_plot(gg_box[[4]], x = 0.05, y = 0.1, width = 0.4, height = 0.3) +
  draw_plot(gg_box[[5]], x = 0.6, y = 0, width = 0.3, height = 0.2)



