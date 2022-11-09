# README ----
# title: Prepare raw data for analysis 
# author: Morgan Gray
# created: 2022-02-23
# revised: 2022-11-04
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(readxl)   ## To read .xlsx files
library(here)   ## To manage directories
library(lubridate)   ## To work with dates and times
library(janitor)   ## To tidy data frames
library(collapse)  ## For advanced data frame manipulation
library(bestNormalize)

# Set file paths
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_raw <- here("input/data_raw")
path_derived <- here("input/data_derived")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "data-transformation.R"))

# Create lookup tables  ----
lookup_time <- read_csv(here(path_lookup, "lookup_time.csv"))
lookup_fuel <- read_csv(here(path_lookup, "lookup_fuel.csv"))
# ========================================================== -----
# TUBBS: TWO TRANSECTS PER PLOT (n = 9 plots) ----
# About this data set ----
# Each year has 36 data points for litter and duff (9 plots x 2 transects x 2 quadrats)
# Each year has 18 data points for downed woody debris (9 plots x 2 transects x 1 quadrat)
# Reshape and annotate raw data ----
#   tubbs_raw_wd: Coarse woody debris (wd) ----
tubbs_raw_wd <- 
  read_excel(here(path_raw,  "RXF Fuels Data for R 2022-02-07.xlsx"), 
             sheet = "Downed Woody Debris")  %>%
  # Clean column names 
  clean_names() %>%
  # Rename columns
  rename(fuel_class_orig_tubbs = fuel_class, 
         us_value = tons_acre, 
         n_transect = transect) %>%
  # Create attributes for subsequent analysis and figures
  mutate(
    # Convert year to character to allow left_join with lookup_time
    time = as.character(year), 
    # Calculate values in metric units
    # 1 US ton per acre = 2.242 metric tons per ha
    si_value = us_value * 2.242, 
    # Standardize transect number
    n_transect = str_remove(n_transect, "Fuels"), 
    # Convert datetime values to date values
    date = as_date(date)
  ) %>%
  # Create plot_tran for plot-level calculations
  unite(plot_tran,
        c(plot_id, n_transect), 
        remove = FALSE) %>%
  # Add fuel attributes
  left_join(lookup_fuel, "fuel_class_orig_tubbs") %>%
  # Add time attributes 
  left_join(lookup_time, "time") %>%
  select(
    fuel_type, 
    fuel_class,
    plot_id, 
    n_transect, 
    plot_tran, 
    time, 
    si_value, 
    si_units, 
    us_value, 
    us_units, 
    starts_with("lab"), 
    starts_with("order"))

#   tubbs_raw_dl: Duff and litter (dl) ----
# Exclude the derived values in columns litter_tons_acre and duff_tons_acre 
tubbs_raw_dl <- 
  read_excel(here(path_raw, "RXF Fuels Data for R 2022-02-07.xlsx"), 
             sheet = "Litter and Duff")  %>%
  # Clean column names 
  clean_names() %>%
  # Components from the gathered column will be derived below
  gather(class_tran_quad, us_value, litter1a:duff2b) %>%
  # Create attributes for subsequent analysis and figures
  mutate(
    # Convert year to character to allow left_join with lookup_time
    time = as.character(year),
    # Calculate values in metric units
    # 1 in =  2.54 cm
    si_value = us_value * 2.54, 
    # Identify fuel_class
    fuel_class = ifelse(str_detect(class_tran_quad, "litter"), "litter", "duff"), 
    # Derive transect and quadrat 
    tran_quad = str_remove_all(class_tran_quad, fuel_class), 
    n_transect = str_sub(tran_quad, 1, 1),
    quadrat = str_sub(tran_quad, 2, 2),
    # Convert datetime values to date values
    date = as_date(date)
    ) %>%
  # Create plot_tran for plot-level calculations
  unite(plot_tran,
        c(plot_id, n_transect), 
        remove = FALSE) %>%
  # Add fuel attributes
  left_join(lookup_fuel %>%
              # Exclude attributes for thin plot_types
              filter(fuel_type %in% "dl" & !is.na(fuel_class_orig_tubbs)), 
            "fuel_class") %>%
  # Add time attributes 
  left_join(lookup_time, "time") %>%
  select(
    fuel_type, 
    fuel_class,
    plot_id, 
    n_transect, 
    quadrat,
    plot_tran, 
    time, 
    si_value, 
    si_units, 
    us_value, 
    us_units, 
    starts_with("lab"), 
    starts_with("order")) 

#   tubbs_raw_tidy: Combine tidy (raw) dl and wd data frames, write to csv ----
tubbs_raw_tidy <- 
  bind_rows(tubbs_raw_dl, tubbs_raw_wd) %>%
  write_csv(here(path_derived, "tubbs_raw-tidy.csv"))

# Derive plot-level mean and total values  ----
# Calculate plot-level mean by fuel_type x fuel_class x time 
tubbs_derived_mean <- 
  tubbs_raw_tidy %>%
  group_by(fuel_type, 
           fuel_class, 
           plot_id, 
           time, 
           si_units, 
           us_units, 
           lab_fuel, 
           lab_class,
           lab_time, 
           lab_time_abbr, 
           order_class,
           order_time) %>%
  summarize(us_value = mean(us_value, na.rm = TRUE), 
            si_value = mean(si_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subset_by = "type_class_time_plot", 
         metric = "mean") 

# Calculate plot-level total by fuel_type x time 
tubbs_derived_total <- 
  tubbs_derived_mean %>%
  group_by(fuel_type, 
           plot_id, 
           time, 
           si_units, 
           us_units, 
           lab_fuel, 
           lab_time, 
           lab_time_abbr, 
           order_time) %>%
    summarize(us_value = sum(us_value, na.rm = TRUE), 
              si_value = sum(si_value, na.rm = TRUE)) %>%
  ungroup() %>%
    mutate(subset_by = "type_time_plot", 
           metric = "total", 
           fuel_class = "all", 
           lab_class = "All", 
           order_class = 1) 

# Combine derived data frames and write to csv 
tubbs_derived_all <- 
  bind_rows(tubbs_derived_mean, 
            tubbs_derived_total) %>%
  relocate(
    fuel_type, 
    fuel_class,
    plot_id, 
    time, 
    si_value, 
    si_units, 
    us_value, 
    us_units, 
    starts_with("lab"), 
    starts_with("order")
  ) %>%
  write_csv(here(path_derived, "tubbs_derived.csv"))

# Normalize (and standardize) data ---- 
# ordNorm: total, 1, 100, 1000s
# sqrt: 10, 1000r
#
# Total: arcsine, log, ordnorm
# 1: ordnorm (outlier)
# 10: sqrt (outlier)
# 100: ordnorm (outlier), (nothing helped norm)
# 1000r: sqrt
# 1000s: ordnorm

tubbs_norm_dl <- 
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
  filter(fuel_type %in% "dl") %>%
  group_by(fuel_class) %>%
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) %>%
  ungroup()

tubbs_norm_wd_ord <-  
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
  filter(fuel_type %in% "wd",
         fuel_class %in% c("all", "hr0001", "hr0100", "hr1000s")) %>%
  group_by(fuel_class) %>%
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) %>%
  ungroup()

tubbs_norm_wd_sqr <-  
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
  filter(fuel_type %in% "wd",
         fuel_class %in% c("hr0010", "hr1000r")) %>%
  group_by(fuel_class) %>%
  mutate(value_norm = sqrt_x(si_value, standardize = TRUE)$x.t) %>%
  ungroup()

tubbs_derived_norm <- 
  bind_rows(tubbs_norm_dl, 
            tubbs_norm_wd_ord, 
            tubbs_norm_wd_sqr) %>%
  relocate(value_norm, .before = si_value) %>%
  write_csv(here(path_derived, "tubbs_derived-norm.csv"))

# ========================================================== -----
# THIN: THREE TRANSECTS PER PLOT (n = 5 plots) ----
# About this data set ----
# we now have additional fuels data for forests that were burned in Tubbs and recently thinned
# do a quick ANOVA on those data for this conference - really focusing it on fuel load management
# It will not be a repeated measures test because we only have two time steps. 
# The key thing to account for is that our new forest plots have three transects per plot instead of just two 
# All other sampling is the same as the RXF fuels data. 
# There should be five plots in this small set - FOR05, 06, 07, 08, 10.
# Reshape and annotate raw data ----
thin_raw_tidy <- 
  read_excel(here(path_raw, "FOR_Fuels_MorganANOVA.xlsx"), 
             sheet = "Data") %>%
  # Clean column names 
  clean_names() %>%
  rename(lab_time = timing) %>%
  # Components from the gathered column will be derived below
  gather(fuel_class_orig_thin, us_value, x1hr_load:duff2) %>%
  # Add fuel attributes
  left_join(lookup_fuel %>%
              # Exclude attributes for tubbs plot_types
              filter(!is.na(fuel_class_orig_thin)), 
            "fuel_class_orig_thin") %>%
  # Add time attributes 
  left_join(lookup_time, "lab_time") %>%
  # Create attributes for subsequent analysis and figures
  mutate(
    # Convert measurements to metric units
    si_value = 
      case_when(fuel_type == "wd" ~ us_value *  2.242, 
                fuel_type == "dl" ~ us_value * 2.54),
    # Derive transect and quadrat 
    n_transect = str_sub(transect_id, 2, 2),
    # Use as.numeric to exclude wd values 
    quadrat = as.numeric(str_remove_all(fuel_class_orig_thin, fuel_class)),
    # Convert datetime values to date values
    date = as_date(date)
  ) %>%
  # Create plot_tran for plot-level calculations
  unite(plot_tran,
        c(plot_id, n_transect),
        remove = FALSE) %>%
  select(
    fuel_type, 
    fuel_class,
    plot_id, 
    n_transect, 
    quadrat,
    plot_tran, 
    time, 
    si_value, 
    si_units, 
    us_value, 
    us_units, 
    starts_with("lab"), 
    starts_with("order"))

thin_raw_tidy %>%
  write_csv(here(path_derived, "thin_raw-tidy.csv"))

# Derive plot-level mean and total values  ----
# Calculate plot-level mean by fuel_type x fuel_class x time 
thin_derived_mean <-  
  thin_raw_tidy %>%
  group_by(fuel_type, 
           fuel_class, 
           plot_id, 
           time, 
           si_units, 
           us_units, 
           lab_fuel, 
           lab_class,
           lab_time, 
           lab_time_abbr, 
           order_class,
           order_time) %>%
  summarize(us_value = mean(us_value, na.rm = TRUE), 
            si_value = mean(si_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subset_by = "type_class_time_plot", 
         metric = "mean") 

# Calculate plot-level total by fuel_type x time 
thin_derived_total <- 
  thin_derived_mean %>%
  group_by(fuel_type, 
           plot_id, 
           time, 
           si_units, 
           us_units, 
           lab_fuel, 
           lab_time, 
           lab_time_abbr, 
           order_time) %>%
  summarize(us_value = sum(us_value, na.rm = TRUE), 
            si_value = sum(si_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subset_by = "type_time_plot", 
         metric = "total", 
         fuel_class = "all", 
         lab_class = "All", 
         order_class = 1) 

# Combine derived data frames and write to csv 
thin_derived_all <- 
  bind_rows(thin_derived_mean, 
            thin_derived_total) %>%
  relocate(
    fuel_type, 
    fuel_class,
    plot_id, 
    time, 
    si_value, 
    si_units, 
    us_value, 
    us_units, 
    starts_with("lab"), 
    starts_with("order")
  ) %>%
  write_csv(here(path_derived, "thin_derived.csv"))

# Normalize (and standardize) data ---- 
thin_derived_norm <- 
  read_csv(here(path_derived, "thin_derived.csv")) %>%
  group_by(fuel_class) %>%
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) %>%
  ungroup() %>%
  relocate(value_norm, .before = si_value) %>%
  write_csv(here(path_derived, "thin_derived-norm.csv"))

# ========================================================== -----
# GRAVEYARD ----
# Create lookup tables  ----
# lookup_units <-
#   tibble(fuel_type = c("dl", "wd"),
#          lab_type = c("Duff & litter", 
#                       "Coarse woody debris"),
#          units_si = c("Depth in centimeters",
#                       "Metric tons per hectare"), 
#          units = c("Depth in inches",
#                    "Tons per acre"))

# lookup_year <- 
#   lookup_variables %>%
#   filter(subset %in% c("year_timing", "year_timing_abbr")) %>%
#   select(year = name, 
#          lab_year = label) %>%
#   mutate(year = as.numeric(year)) %>%
#   group_by(year) %>%
#   mutate(n = 1:n()) %>%
#   ungroup() %>%
#   spread(n, lab_year) %>%
#   set_names("year", "lab_year", "lab_year_abbr") 
# 
# lookup_fuel <-  
#   lookup_variables %>%
#   filter(subset %in% "fuel_class") %>%
#   select(fuel_type = data_type, 
#          fuel_class = name, 
#          lab_fuel = label) %>%
#   distinct() %>%
#   left_join(lookup_units, "fuel_type")
#   
# 
# rename_wd_fuel <-
#   lookup_variables %>%
#   filter(metric %in% "fuel", 
#          data_type %in% "wd", 
#          subset %in% "fuel_class") %>%
#   select(fuel_class_orig = name_orig, 
#          fuel_class = name) 
# 
# rename_thin <- 
#   lookup_variables %>%
#   filter(subset %in% "thin") %>%
#   select(fuel_class_orig = name_orig, 
#          fuel_class = name, 
#          lab_fuel = label,
#          data_type = data_type,
#          transect_rep = transect_rep,
#          units = units) 


# [NOT RUN] CALCULATE DIFFERENCE (TRMT - CONTROL) ----
# # Calculate plot-level mean and total values: Difference between treatment and control  
# # By plot x transect x quadrat 
# fuels_thin_diff <- 
#   fuels_thin_raw %>%
#   select(survey, 
#          plot_id, 
#          data_type, 
#          fuel_class, 
#          plot_tran, 
#          transect_id, 
#          transect_rep, 
#          starts_with("value"), 
#          starts_with("units"), 
#          lab_fuel) %>%
#   spread(survey, value) %>%
#   mutate(diff = trmt - cont) %>%
#   write_csv(here(path_derived, "thin_tidy-data_all_diff.csv"))
# 
# # Calculate plot-level mean by fuel_class x plot x survey  
# thin_diff_plot_class_mean <-  
#   fuels_thin_diff %>%
#   group_by(plot_id, 
#            data_type, 
#            fuel_class,
#            lab_fuel) %>%
#   summarize(mean = mean(diff)) %>%
#   ungroup() %>%
#   left_join(lab_type, "data_type") %>%
#   mutate(subset = "survey_plot_class") %>%
#   relocate(mean, .after = fuel_class) %>%
#   write_csv(here(path_derived, "thin_diff_mean-by-plot-type-class.csv"))
# 
# # Calculate plot-level total by summing the mean for each class 
# thin_diff_plot_total <- 
#   thin_diff_plot_class_mean  %>%
#   group_by(plot_id, 
#            data_type, 
#            lab_type) %>%
#   summarize(total = sum(mean)) %>%
#   ungroup()  %>%
#   mutate(subset = "survey_plot") %>%
#   relocate(total, .after = data_type) %>%
#   write_csv(here(path_derived, "thin_diff_total-by-plot-type.csv"))

