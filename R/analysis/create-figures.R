# revised: 2022-10-25 ----

# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from statistical tests
library(lemon)  ## To manipulate faceted ggplots
library(kableExtra)  ## For tables in rmarkdown
library(ggpubr)  ## For qq plots

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_3-derived")
path_plots <- here("output/plots")

# Define helpers 
index_dl<- "Duff & litter"
index_units_dl <- "Depth in centimeters"
index_wd <- "Coarse woody debris"
index_units_wd <- "Metric tons per hectare"

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "statistical-tests.R"))
source(file = here(path_fxn, "data-transformation.R"))

# Define plot colors ----
plot_colors <- 
  read_csv(here(path_lookup, 
                "plot-colors.csv")) %>%
  arrange(palette, palette_subset, levels)

colors_year <- 
  plot_colors %>%
  filter(palette %in% "year", 
         palette_subset %in% "1pre_3post") %>%
  pull(hex_code) 

colors_dl <- c("#7da8b0", "#c4c4c4")

# ========================================================== -----
# CREATE DATA FRAMES -----
# ========================================================== -----

# Duff and litter ----
# Boxplot by year for all, duff, litter
# Include significant comparisons 
# Coarse woody debris ---
# Boxplot by year for all and each fuel class
# Include significant comparisons 