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
# All plots -----
index_units <- "si"
index_df <- input_all
# Write function -----
fxn_summary_all_plots <- function(index_df, index_units){
  
  name_units <- paste0(index_units, "_units")
  name_value <- paste0(index_units, "_value")
  
 summary_long <- 
    index_df %>%
    unite(proj_time, c(plot_type, time)) %>%
    unite(type_class, c(fuel_type, fuel_class)) %>%
    rename(units = all_of(name_units), 
           value = all_of(name_value)) %>%
    group_by(proj_time, 
             type_class, 
             lab_fuel, 
             lab_class,
             metric, 
             units) %>%
    summarize(a_mean = mean(value, na.rm = TRUE), 
              b_min = min(value, na.rm = TRUE), 
              c_max = max(value, na.rm = TRUE)) %>%
    ungroup() %>%
    gather(statistic, value, a_mean:c_max)
  
  # Calculate percent change in mean values for pre- and post-thinning data 
  thin_chg <- 
    summary_long %>%
    filter(statistic %in% "a_mean") %>%
    spread(proj_time, value) %>%
    mutate(thin_chg = (thin_t2 - thin_t1) / thin_t1) %>%
    select(type_class, thin_chg)
  
  summary_long  %>%
    unite(proj_time_stat, c(proj_time, statistic)) %>%
    spread(proj_time_stat, value) %>%
    # Join column for percent change
    left_join(thin_chg, "type_class") %>%
    relocate(lab_fuel, 
             lab_class, 
             metric,
             units, 
             starts_with("tubbs"), 
             starts_with("thin")) %>%
    arrange(lab_fuel, desc(metric), lab_class) %>%
    write_csv(here(path_out,
                   paste0("summary-table_by-time_all-plots_",
                          index_units, 
                          "_",
                          Sys.Date(),
                          ".csv")))
  
}
# Summarize by unit type (SI, US) ----
fxn_summary_all_plots(index_df = input_all, 
                      index_units = "si")
fxn_summary_all_plots(index_df = input_all, 
                      index_units = "us")

# Plot-level range ----
input_all %>%
  filter(plot_type %in% "tubbs", 
         fuel_type %in% "wd", 
         fuel_class %in% "all", 
         time %nin% "2016") %>%
  summarize(min = min(si_value, na.rm = TRUE) ,
            max = max(si_value, na.rm = TRUE))
