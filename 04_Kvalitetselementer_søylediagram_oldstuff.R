## 1. Forsuring
```{r}
data_forsur <- read_excel("Data_input/ØkologiskTilstandsklassifisering2017_1nov18_SRA_VER2_ENDELIG_ALT OK (002).xlsx", 
                          sheet = 1, range = "A1:G37", na = "NA")

data_forsur <- data_forsur %>% mutate(pH = as.numeric(pH))
data_forsur <- data_forsur %>% mutate(ANC = as.numeric(ANC))
data_forsur <- data_forsur %>% mutate(LAI = as.numeric(LAI))
```

### AIP
```{r}
label <- "AIP-indeks"
plotno <- "1a"
gg <- make_indexplot(data_forsur, "AIP", label)

if (save_plots)
  ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = 500)

# gg
```

### RAMI
```{r}
label <- "RAMI-indeks"
plotno <- "1b"
gg <- make_indexplot(data_forsur, "RAMI", label)

if (save_plots)
  ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = 500)

# gg
```
### pH
```{r, warning = FALSE}
label <- "pH-indeks"
plotno <- "1c"
gg <- make_indexplot(data_forsur, "pH", label)

if (save_plots)
  ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = 500)

# gg
```

### ANC
```{r, warning = FALSE}
label <- "ANC-indeks"
plotno <- "1d"
gg <- make_indexplot(data_forsur, "ANC", label)

if (save_plots)
  ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = 500)

# gg
```

### LAI
```{r, warning = FALSE}
label <- "LAI-indeks"
plotno <- "1e"
gg <- make_indexplot(data_forsur, "LAI", label)

if (save_plots)
  ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = 500)

# gg
```
