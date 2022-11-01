# revised: 2022-10-28 ----
# About this data set ----
# hr1000r: 29 of the 30 values were 0 
# we now have additional fuels data for forests that were burned in Tubbs and recently thinned
# It will not be a repeated measures test because we only have two time steps. 
# The key thing to account for is that our new forest plots have three transects per plot instead of just two 
# All other sampling is the same as the RXF fuels data. 
# There should be five plots in this small set - FOR05, 06, 07, 08, 10.

# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from metrical tests
library(ggpubr)  ## For preset plot templates
library(patchwork)  ## To arrange multiple plots 
# library(lemon)  ## To manipulate faceted ggplots

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_3-derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "statistical-tests.R"))
source(file = here(path_fxn, "data-transformation.R"))
# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

# Define plot colors 
colors_trmt_bright <- c("#9d9596", "#069879")
colors_trmt_faded <- c("#bfbabb", "#75bca8")
color_palette <- thematic::okabe_ito(2)
colors_thin <- 
  read_csv(here(path_lookup, "plot-colors.csv")) %>%
  arrange(palette, palette_subset, levels) %>%
  filter(palette %in% "year", 
         palette_subset %in% "1pre_3post") %>%
  pull(hex_code) 

# ========================================================== -----
# DUFF & LITTER (DL) ----
# Read and subset data  ----
dl_transform <- 
  read_csv(here(path_derived, "thin_dl_transformed_metric-units.csv")) %>%
  arrange(timing, plot_id, data_type, fuel_class) %>%
  mutate_if(is.character, as_factor)   

dl_transform_total <- 
  dl_transform %>%
  filter(data_type %in% "dl", 
         metric %in% "total")

dl_transform_class <- 
  dl_transform %>%
  filter(data_type %in% "dl", 
         metric %in% "mean")


# One-way ANOVA  ----
# Effect of timing (duff and litter combined)
dl_transform_total %>%
  fxn_aov_me(index_value = "value_tran", 
             index_id = "plot_id", 
             index_time = "timing")  %>%
  fxn_signif_adj() %>%
  mutate(data_type = index_dl,
         fuel_class = "All") %>%
  select(data_type, 
         fuel_class, 
         effect, 
         starts_with("p_adj"),
         statistic, 
         starts_with("d_"), 
         ges)

# Two-way ANOVA: Interaction between timing x fuel class  ----
dl_transform_class %>%
  fxn_aov2_me(index_value = "value_tran", 
              index_id = "plot_id", 
              index_time = "timing", 
              index_variable = "fuel_class") %>%
  fxn_signif_adj() %>%
  mutate(data_type = index_dl) %>%
  select(data_type, 
         effect, 
         starts_with("p_adj"), 
         statistic, 
         starts_with("d_"), 
         ges)  %>%
  fxn_kable()

# Significant interaction between timing and fuel class 

# Two-way post hoc tests: timing by fuel class ----
dl_transform_class %>%
  fxn_pwc2(index_value = "value_tran", 
           index_id = "plot_id", 
           index_time = "lab_thin", 
           index_variable = "fuel_class") %>%
  mutate(data_type = index_dl) %>%
  select(data_type, 
         fuel_class = variable, 
         starts_with("group"), 
         starts_with("p_adj"), 
         statistic, 
         starts_with("d_")) %>%
  fxn_kable()
# Significant difference in litter between pre- and post-thinning

# ========================================================== -----
