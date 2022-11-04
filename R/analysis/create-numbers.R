# revised: 2022-11-03 ----
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_3-derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers ----
# index_dl <- "Duff & litter"
# index_wd <- "Coarse woody debris"
# 
lookup_units <-
  tibble(data_type = c("dl", "wd"),
         units_si = c("Depth in centimeters",
                      "Metric tons per hectare"), 
         units = c("Depth in inches",
                   "Tons per acre"))


# Create Tubbs data  ----
tubbs <- 
  read_csv(here(path_derived, "tubbs_total-by-plot-type-yr.csv")) %>%
  mutate(lab_fuel = "All", 
         fuel_class = "all") %>%
  bind_rows(read_csv(here(path_derived, "tubbs_mean-by-plot-type-class-yr.csv"))) %>%
  left_join(lookup_units, "data_type") %>%
  select(fuel_type = data_type, 
         fuel_class, 
         plot_id, 
         year, 
         starts_with("value"),
         metric = statistic,
         starts_with("units"),
         starts_with("lab"))

# Create thin data  ----
thin <-
  read_csv(here(path_derived, "thin_total-by-plot-type-trmt.csv")) %>%
  bind_rows(read_csv(here(path_derived, "thin_mean-by-plot-type-class-trmt.csv"))) %>%
  mutate(timing = ifelse(survey %in% "cont", "survey1", "survey2")) %>%
  left_join(lookup_units, "data_type") %>%
  select(fuel_type = data_type, 
           fuel_class, 
           plot_id, 
           timing,
         starts_with("value"),
         metric = statistic,
         starts_with("units"),
           starts_with("lab"))

  

# ========================================================== -----
# CALCULATE SUMMARY STATISTICS -----
# Total ----
tubbs %>%
  group_by(lab_type, lab_fuel, year, units) %>%
  summarize(mean = mean(value)) %>%
  spread(year, mean)

thin %>%
  group_by(lab_type, lab_fuel, timing, units) %>%
  summarize(mean = mean(value)) %>%
  spread(timing, mean) %>%
  rename(before_thin = survey1, 
         after_thin = survey2)
