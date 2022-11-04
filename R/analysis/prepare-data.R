# README ----
# title: Prepare raw data for analysis 
# author: Morgan Gray
# created: 2022-02-23
# revised: 2022-11-01
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(readxl)   ## To read .xlsx files
library(here)   ## To manage directories
library(lubridate)   ## To work with dates and times
library(janitor)   ## To tidy data frames

# Set file paths
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_raw <- here("input/data_1-raw")
path_tidy <- here("input/data_2-raw-tidy")
path_derived <- here("input/data_3-derived")

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
    starts_with("order"), 
    -starts_with("fuel_class_orig"))

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
  bind_rows(tubbs_raw_wd, tubbs_raw_dl) %>%
  write_csv(here(path_tidy, "tubbs_raw-tidy.csv"))

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
  tubbs_raw_tidy %>%
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

# ========================================================== -----
# THIN: THREE TRANSECTS PER PLOT (n = 5 plots) ----
# About this data set ----
# we now have additional fuels data for forests that were burned in Tubbs and recently thinned
# do a quick ANOVA on those data for this conference - really focusing it on fuel load management
# It will not be a repeated measures test because we only have two time steps. 
# The key thing to account for is that our new forest plots have three transects per plot instead of just two 
# All other sampling is the same as the RXF fuels data. 
# There should be five plots in this small set - FOR05, 06, 07, 08, 10.
# Create lookup tables  ----
lookup_variables <- read_csv(here(path_lookup, "lookup_variables.csv"))

lab_type <- 
  lookup_variables %>%
  filter(metric %in% "fuel", 
         data_type %in% "all", 
         subset %in% "fuel_type") %>%
  select(data_type = name, 
         lab_type = label)

lab_fuel <-  
  lookup_variables %>%
  filter(metric %in% "fuel", 
         data_type %in% "all", 
         subset %in% "fuel_class") %>%
  select(fuel_class = name, 
         lab_fuel = label)



# lab_trmt <- 

# Read raw data; reshape and annotate ----
fuels_thin_raw <- 
  read_excel(here(path_raw, 
                  "FOR_Fuels_MorganANOVA.xlsx"), 
             sheet = "Data") %>%
  # Clean column names 
  clean_names() %>%
  rename(lab_thin = timing) %>%
  gather(fuel_class_orig, value, x1hr_load:duff2) %>%
  left_join(rename_thin, "fuel_class_orig") %>%
  left_join(lab_type, "data_type") %>%
  # Convert datetime values to date values
  mutate(date = as_date(date), 
         year = year(date), 
         survey = ifelse(lab_thin %in% "Pre-thinning", "cont", "trmt")) %>%  
  # Convert measurements to metric units
  mutate(value_si = 
           case_when(data_type == "wd" ~ value *  2.242, 
                     data_type == "dl" ~ value * 2.54), 
         units_si = 
           case_when(data_type == "wd" ~ "tons_per_hectare", 
                     data_type == "dl" ~ "depth_in_cm")) %>%
  unite(plot_tran,
        c(plot_id, transect_id), 
        remove = FALSE) %>%
  select(year, 
         plot_id, 
         survey,
         date, 
         plot_tran,
         transect_id, 
         transect_rep,
         data_type,
         fuel_class, 
         starts_with("value"),
         starts_with("units"), 
         lab_fuel,
         starts_with("lab"))  %>%
  write_csv(here(path_tidy, "thin_tidy-data.csv"))

# Calculate plot-level mean and total values ----
# Calculate plot-level mean by fuel_class x plot x survey  
thin_plot_class_mean <-  
  fuels_thin_raw %>%
  group_by(survey, 
           plot_id, 
           data_type, 
           fuel_class,
           lab_type,
           lab_fuel, 
           lab_thin, 
           units, 
           units_si) %>%
  summarize(value = mean(value), 
            value_si = mean(value_si)) %>%
  ungroup() %>%
  mutate(subset = "survey_plot_class", 
         statistic = "mean") %>%
  arrange(data_type, fuel_class, survey) %>%
  relocate(starts_with("value"), .after = fuel_class) %>%
  write_csv(here(path_derived, "thin_mean-by-plot-type-class-trmt.csv"))

# Calculate plot-level total by summing the mean for each class 
thin_plot_total <- 
  thin_plot_class_mean  %>%
  group_by(survey, 
           plot_id, 
           data_type, 
           lab_type,
           lab_thin, 
           units, 
           units_si) %>%
  summarize(value = sum(value), 
            value_si = sum(value_si)) %>%
  ungroup()  %>%
  mutate(lab_fuel = "All", 
         fuel_class = "all",
         subset = "survey_plot", 
         statistic = "total") %>%
  arrange(data_type, lab_type, survey) %>%
  relocate(starts_with("value"), .after = data_type) %>%
  write_csv(here(path_derived, "thin_total-by-plot-type-trmt.csv"))

# ---------------------------------------------------------- -----
# Standardize and normalize data ---- 
input_thin <- 
  read_csv(here(path_derived, "thin_total-by-plot-type-trmt.csv")) %>%
  bind_rows(read_csv(here(path_derived, "thin_mean-by-plot-type-class-trmt.csv"))) %>%
  # Rename column bc "statistic" conflicts with shapiro test
  rename(metric = statistic) %>%
  mutate(value = fxn_digit(value_si), 
         units = units_si, 
         timing = ifelse(survey %in% "cont", "survey1", "survey2")) %>%
  arrange(survey, plot_id, data_type, fuel_class) %>%
  mutate_if(is.character, as_factor)  %>%
  select(-value_si, 
         -units_si) %>%
  relocate(c(metric, subset), .after = value)  

# Duff and litter ----
dl_tran <- 
  input_thin %>%
  filter(data_type %in% "dl") %>%
  fxn_tranform_ordnorm(index_list = c("all", "duff", "litter"))  %>%
  select(metric, 
         data_type, 
         fuel_class, 
         plot_id, 
         timing, 
         value_tran, 
         value_raw,
         units, 
         transform,
         starts_with("lab")) %>%
  write_csv(here(path_derived, "thin_dl_transformed_metric-units.csv"))

# Coarse woody debris  ----
list_classes_wd <- 
  input_thin %>%
  filter(data_type %in% "wd") %>%
  distinct(fuel_class) %>%
  pull()

wd_tran <- 
  input_thin %>%
  filter(data_type %in% "wd") %>%
  fxn_tranform_ordnorm(index_list = list_classes_wd)  %>%
  select(metric, 
         data_type, 
         fuel_class, 
         plot_id, 
         timing, 
         value_tran, 
         value_raw,
         units, 
         transform,
         starts_with("lab")) 

wd_tran %>%
  write_csv(here(path_derived, "thin_wd_transformed_metric-units.csv"))
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
#   write_csv(here(path_tidy, "thin_tidy-data_all_diff.csv"))
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

