
#
# Function for renaming variables
# Suitable also in functions and for tibbles
#

rename2 <- function(df, oldname, newname){
  names(df)[names(df) %in% oldname] <- newname
  df
}

#
# Make horizontal barplot for data_index (column names: varname_index, varname_navn) on level Vannforekomst
#
make_indexplot <- function(data_index, varname_index, 
                           scale_label, 
                           varname_navn = "Vannforekomst, kortnavn"){
  
  data_index <- rename2(data_index, oldname = varname_navn,  newname = "Rapportnavn")
  data_index <- rename2(data_index, oldname = varname_index, newname = "Indeksverdi")
  data_index <- as.data.frame(data_index)
  
  # Set Tilstandsklasse
  x1 <- data_index$Indeksverdi
  x2 <- 6 - as.numeric(cut(x1, breaks =  seq(0,1,0.2) + 0.001, label = FALSE))
  x3 <- c("Svært god", "God", "Moderat", "Dårlig", "Svært dårlig", "Uklassifisert")[x2]
  data_index$Tilstandsklasse <- factor(x3, levels = c("Svært god", "God", "Moderat", "Dårlig", "Svært dårlig", "Uklassifisert"))
  data_index$Tilstandsklasse <- droplevels(data_index$Tilstandsklasse)
  
  # Get Rapportnavn 01 on top
  data_index$Rapportnavn <- factor(data_index$Rapportnavn, levels = rev(data_index$Rapportnavn))
  
  # Plot
  gg <- ggplot(data_index, aes(Rapportnavn, Indeksverdi, fill = Tilstandsklasse)) + 
    geom_hline(yintercept = seq(0, 1, 0.2), size = rel(0.5), linetype = 2) +
    geom_hline(yintercept = 0.6, size = rel(1), linetype = 1) +
    geom_col(width = 0.75) +
    scale_fill_manual("Tilstandsklasse", values = class_colors) +
    scale_y_continuous(minor_breaks = seq(0, 1, 0.2), breaks = seq(0, 1, 0.2), 
                       limits = c(0,1.08), expand = c(0,0)) +         # limits + expand: no space on left side + a little space on right side 
    coord_flip() + 
    theme(axis.text.y = element_text(hjust = 0)) +
    theme(legend.position = "bottom") +
    labs(x = "", y = scale_label) +
    theme_bw() +
    theme(axis.text = element_text(color = "black"))
  gg
}

#
# Make horizontal barplot for data_index (column names: varname_index, varname_navn) on level Vannforekomst
#   with extra dots for sample site level in data data_index_points (column names: varname_index_p, varname_navn_p) 
# Called by make_indexplot_dots()
#
make_indexplot_dots_givendata <- function(data_index, data_index_points, 
                                varname_index, varname_navn,
                                varname_index_p, varname_navn_p,
                                scale_label,
                                varname_kalkrik = NULL){
  
  data_index <- data_index %>% 
    rename2(oldname = varname_navn,  newname = "Rapportnavn") %>%
    rename2(oldname = varname_index, newname = "Indeksverdi") %>%
    as.data.frame(data_index)
  
  # Set Tilstandsklasse
  x1 <- data_index$Indeksverdi
  x2 <- 6 - as.numeric(cut(x1, breaks =  seq(0,1,0.2) + 0.001, label = FALSE))
  x3 <- c("Svært god", "God", "Moderat", "Dårlig", "Svært dårlig", "Uklassifisert")[x2]
  if (!is.null(varname_kalkrik))
    x3[data_index[[varname_kalkrik]] %in% "x"] <- "Uklassifisert"
  data_index$Tilstandsklasse <- factor(x3, levels = c("Svært god", "God", "Moderat", "Dårlig", "Svært dårlig", "Uklassifisert"))
  data_index$Tilstandsklasse <- droplevels(data_index$Tilstandsklasse)
  
  # Get Rapportnavn 01 on top
  data_index$Rapportnavn <- factor(data_index$Rapportnavn, levels = rev(data_index$Rapportnavn))
  
  # Plot
  gg <- ggplot(data_index, aes(Rapportnavn, Indeksverdi)) + 
    geom_col(aes(fill = Tilstandsklasse)) +
    scale_fill_manual("Tilstandsklasse", values = class_colors) +
    geom_point(data = data_index_points, aes(Kortnavn_1, nEQR_2)) +
    coord_flip() + 
    theme(legend.position = "bottom") +
    labs(x = "", y = scale_label)
  
  gg
}

#
# As make_indexplot_dots_givendata()
# But input is file and sheet name for excel sheet, label (used both on plot and in filename) and plotnot (used in filename)
#

make_indexplot_dots <- function(excelsheet, label, plotno, save_plots = TRUE, dpi = 150,
                                excelfile = "Data_input/TilstandsklassifiseringPrKEogVF2017.xlsx", range = "A2:G53",
                                including_kalkrik = FALSE){
  
  dat <- read_excel(excelfile, sheet = excelsheet, range = range, na = "NA")
  
  # names(dat) %>% dput() 
  names(dat)[1:7] <- c("Fylke", 
                  "Kortnavn_1", "nEQR_1", "Tilstandsklasse_1", 
                  "Kortnavn_2", "nEQR_2", "Tilstandsklasse_2")
  
  # Fill out missing values of 'Kortnavn_1' (Vannforekomstnavn)
  x <- dat[["Kortnavn_1"]]
  for (i in 1:length(x))
    if (is.na(x[i])) x[i] <- x[i-1]
  dat[["Kortnavn_1"]] <- x
  
  # Mean values for vannforekomst 
  if (including_kalkrik){
    dat_vannforekomst <- dat %>%
      filter(!is.na(nEQR_2)) %>%
      group_by(Kortnavn_1) %>%
      summarise(Indeksverdi = mean(nEQR_2), Moderat_kalkrik = first(Moderat_kalkrik))
    varname_kalkrik <- "Moderat_kalkrik"
  } else {
    dat_vannforekomst <- dat %>%
      group_by(Kortnavn_1) %>%
      summarise(Indeksverdi = mean(nEQR_2))
    varname_kalkrik <- NULL
  }
  
  gg <- make_indexplot_dots_givendata(dat_vannforekomst, dat,
                                      "Indeksverdi", "Kortnavn_1", 
                                      "nEQR_2", "Kortnavn_2", 
                                      scale_label = label, 
                                      varname_kalkrik = varname_kalkrik)
  
  if (save_plots)
    ggsave(paste0("Figures/04_", plotno, "_", label, ".png"), gg, width = 8.5, height = 10.5, dpi = dpi)
  
  gg
}
