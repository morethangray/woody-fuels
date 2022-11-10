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
# path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_derived")
path_out <- here("output/summary-tables")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
tubbs <- 
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
  mutate(plot_type = "tubbs", 
         time = as.character(time))
 
thin <-
  read_csv(here(path_derived, "thin_derived.csv")) %>%
  mutate(plot_type = "thin")
  
input_all <- 
  bind_rows(tubbs, thin) %>%
  select(plot_type, 
         fuel_type, 
         fuel_class, 
         plot_id, 
         time, 
         starts_with("si_"),
         metric,
         starts_with("lab"))
# ========================================================== -----
# CALCULATE SUMMARY STATISTICS -----
input_all %>%
  unite(type_time, c(plot_type, time)) %>%
  group_by(type_time, 
           lab_fuel, 
           lab_class,
           metric, 
           si_units) %>%
  summarize(mean = mean(si_value)) %>%
  spread(type_time, mean) %>%
  arrange(lab_fuel, 
          desc(metric)) %>%
  write_csv(here(path_out, 
                 paste0("mean_by-time_", 
                        Sys.Date(), 
                        ".csv")))
