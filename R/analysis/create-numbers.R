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
         starts_with("us"),
         metric,
         starts_with("lab"))
# ========================================================== -----
# CALCULATE SUMMARY STATISTICS -----
# SI units ----
si_summary_long <- 
  input_all %>%
  unite(proj_time, c(plot_type, time)) %>%
  unite(type_class, c(fuel_type, fuel_class)) %>%
  group_by(proj_time, 
           type_class, 
           lab_fuel, 
           lab_class,
           metric, 
           si_units) %>%
  summarize(a_mean = mean(si_value, na.rm = TRUE), 
            b_min = min(si_value, na.rm = TRUE), 
            c_max = max(si_value, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(statistic, value, a_mean:c_max)

# Calculate percent change in mean values for pre- and post-thinning data 
si_thin_chg <- 
  si_summary_long %>%
  filter(statistic %in% "a_mean") %>%
  spread(proj_time, value) %>%
  mutate(thin_chg = (thin_t2 - thin_t1) / thin_t1) %>%
  select(type_class, thin_chg)

si_summary_wide <- 
  si_summary_long  %>%
  unite(proj_time_stat, c(proj_time, statistic)) %>%
  spread(proj_time_stat, value) %>%
  left_join(si_thin_chg, "type_class") %>%
  relocate(lab_fuel, 
           lab_class, 
           metric,
           si_units, 
           starts_with("tubbs"), 
           starts_with("thin")) %>%
  arrange(lab_fuel, desc(metric), lab_class)

si_summary_wide %>%
  write_csv(here(path_out,
                 paste0("summary-table_by-time_si_",
                        Sys.Date(),
                        ".csv")))

# US units ----
us_summary_long <- 
  input_all %>%
  unite(proj_time, c(plot_type, time)) %>%
  unite(type_class, c(fuel_type, fuel_class)) %>%
  group_by(proj_time, 
           type_class, 
           lab_fuel, 
           lab_class,
           metric, 
           us_units) %>%
  summarize(a_mean = mean(us_value, na.rm = TRUE), 
            b_min = min(us_value, na.rm = TRUE), 
            c_max = max(us_value, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(statistic, value, a_mean:c_max)

# Calculate percent change in mean values for pre- and post-thinning data 
us_thin_chg <- 
  us_summary_long %>%
  filter(statistic %in% "a_mean") %>%
  spread(proj_time, value) %>%
  mutate(thin_chg = (thin_t2 - thin_t1) / thin_t1) %>%
  select(type_class, thin_chg)

us_summary_wide <- 
  us_summary_long  %>%
  unite(proj_time_stat, c(proj_time, statistic)) %>%
  spread(proj_time_stat, value) %>%
  left_join(us_thin_chg, "type_class") %>%
  relocate(lab_fuel, 
           lab_class, 
           metric,
           us_units, 
           starts_with("tubbs"), 
           starts_with("thin")) %>%
  arrange(lab_fuel, desc(metric), lab_class)

us_summary_wide %>%
  write_csv(here(path_out,
                 paste0("summary-table_by-time_us_",
                        Sys.Date(),
                        ".csv")))
