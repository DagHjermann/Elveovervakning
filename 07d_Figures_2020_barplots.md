---
title: "Barplot figures (2020)"
author: "DHJ"
date: "23 10 2020"
output: 
  html_document:
    keep_md: true
    toc: true
    
---

#### Bar plots for nEQR quality   

* Data are prepared in script 07a and 07b  
* Plotting uses function `make_barplot_index()` in script 07..  

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

### b. Libs + map  

```r
library(tidyverse)  # filter, group_by, summary, etc.
library(readxl)
library(cowplot)
library(glue)

source("07_Figures_functions.R")

# Define crs strings for conversion between long/lat and UTM
crs_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
crs_utm <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m"

source("07_Figures_functions.R")
```

### c. Define colours for classes  

```r
# Order in plots etc.
class_levels <- c("Svært dårlig", "Dårlig", "Moderat", "God", "Svært god", "Uklassifisert")

# Read standard colors
df_colors <- read_excel("Data_input/Fargekoder RGB for tilstandsklassifisering.xlsx", sheet = 2) %>%
  mutate(
    Status_no = case_when(
      Status %in% "High" ~ "Svært god",
      Status %in% "Good" ~ "God",
      Status %in% "Moderate" ~ "Moderat",
      Status %in% "Poor" ~ "Dårlig",
      Status %in% "Bad" ~ "Svært dårlig",
      Status %in% "Unclassified" ~ "Uklassifisert"),
    Status_no = factor(Status_no, levels = class_levels),
    Class_color = rgb(R/255, G/255, B/255)
  ) %>%
  arrange(Status_no)
# df_colors


# Show colors
par(mar = c(0,0,2,0))
pie(rep(1,6), 
    col = df_colors$Class_color, 
    labels = df_colors$Status_no, 
    main = "Colors for nEQr classes")  
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Also make named list
class_colors <- df_colors$Class_color
names(class_colors) <- df_colors$Status_no
```

## 2. Data   

### a. Data from script 7a and 7b  

```r
# From script 7a
# df_index <- readRDS("Data/2020/07a_df_index_2019.rds")
df_index_barplot <- readRDS("Data/2020/07a_df_index_barplot_2019.rds")

# From script 7b, part 4c
# df_stations <- readRDS("Data/2020/07b_df_stations.rds")   # with UTM 32 coordinates
```


## 3. Bar plots
### a. Eutrofiering

```r
label <- "Samlet eutrofieringstilstand (nEQR)"
plotno <- "1a"
# debugonce(make_indexplot)

gg <- make_barplot_index(df_index_barplot, "nEQR_eutrofi", label)
if (save_plots){
  ggsave2(paste0("{folder_fig}/07_", plotno, "_", label, ".png"), gg, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2(paste0("{folder_fig}/07_", plotno, "_", label, ".svg"), gg, 
         width = 8.5, height = 10.5)
}
gg
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### b. Forsuring (acidification)

```r
label <- "Samlet forsuringstilstand (nEQR)"
plotno <- "1b"
gg <- make_barplot_index(df_index_barplot, "nEQR_forsuring", label)
if (save_plots){
  ggsave2("{folder_fig}/07_{plotno}_{label}.png", gg, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}.svg", gg, 
         width = 8.5, height = 10.5, dpi = 500)
  }
```

```
## Warning: Removed 5 rows containing missing values (position_stack).

## Warning: Removed 5 rows containing missing values (position_stack).
```

```r
gg
```

```
## Warning: Removed 5 rows containing missing values (position_stack).
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


### c. Samlet økologisk tilstand 

```r
label <- "Samlet økologisk tilstand (nEQR)"
plotno <- "1c"
gg1 <- make_barplot_index(df_index_barplot, "nEQR_ecol", label)
gg2 <- gg1 + 
  # Add all-white label (to cover the vertical lines)
  # (this is a workaround, as 'colour' seems to control both text and border colour...)
  # geom_label(aes(Rapportnavn, Indeksverdi, label = Label),
  #            hjust = 0, nudge_y = 0.005, nudge_x = 0, color = "white", fill = "white") +
  # Add text
  geom_text(aes(Rapportnavn, Indeksverdi, label = Label),
             hjust = 0, nudge_y = 0.005, nudge_x = 0)
gg3 <- gg2 + 
  theme(axis.text.y = element_blank())
#gg3

if (save_plots){
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver1.png", gg1, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver1.svg", gg1, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver2.png", gg2, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver2.svg", gg2, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver3.png", gg3, 
         width = 8.5, height = 10.5, dpi = 500)
  ggsave2("{folder_fig}/07_{plotno}_{label}_ver3.svg", gg3, 
         width = 8.5, height = 10.5, dpi = 500)
}

gg1
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
gg2
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
gg3
```

![](07d_Figures_2020_barplots_files/figure-html/unnamed-chunk-7-3.png)<!-- -->
