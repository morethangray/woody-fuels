# README ----
# title: Prepare raw data for analysis 
# author: Morgan Gray
# created: 2022-02-23
# revised: 2022-10-25
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
path_tidy <- here("input/data_2-tidy")
path_derived <- here("input/data_3-derived")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "data-transformation.R"))

# ========================================================== -----
# TUBBS: TWO TRANSECTS PER PLOT ----
# About this data set ----
# Each year has 36 data points for litter and duff (9 plots x 2 transects x 2 quadrats)
# Each year has 18 data points for downed woody debris (9 plots x 2 transects x 1 quadrat)
# Create lookup tables  ----
lookup_variables <- read_csv(here(path_lookup, "lookup_variables.csv"))

lab_year <- 
  lookup_variables %>%
  filter(metric %in% "fuel", 
         data_type %in% "all", 
         subset %in% c("year_timing", "year_timing_abbr")) %>%
  select(year = name, 
         lab_year = label) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  mutate(n = 1:n()) %>%
  ungroup() %>%
  spread(n, lab_year) %>%
  set_names("year", "lab_year", "lab_year_abbr") 

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

rename_wd_fuel <-
  lookup_variables %>%
  filter(metric %in% "fuel", 
         data_type %in% "wd", 
         subset %in% "fuel_class") %>%
  select(fuel_class_orig = name_orig, 
         fuel_class = name) 

# Write functions ----
#   fxn_yr_plot_class_mean  ----
# Mean value by class x plot x year
fxn_yr_plot_class_mean <- function(df){
  df %>%
    group_by(data_type, 
             year, 
             plot_id, 
             fuel_class) %>%
    summarize(value = mean(value), 
              value_si = mean(value_si)) %>%
    ungroup() %>%
    left_join(lab_year, "year") %>%
    left_join(lab_fuel, "fuel_class") %>%
    left_join(lab_type, "data_type") %>%
    mutate(subset = "yr_plot_class", 
           statistic = "mean") %>%
    arrange(year) 
}

#   fxn_yr_plot_total ----
# Sum of class mean values by plot x year
fxn_yr_plot_total <- function(df){
  df %>%
    group_by(data_type, 
             year, 
             plot_id) %>%
    summarize(value = mean(value), 
              value_si = mean(value_si)) %>%
    ungroup()  %>%
    left_join(lab_year, "year") %>%
    left_join(lab_type, "data_type") %>%
    mutate(subset = "yr_plot", 
           statistic = "total")
}

# ---------------------------------------------------------- -----
# COARSE WOODY DEBRIS (wd) -----
# Read raw data; reshape and annotate ----
fuels_wd_raw <- 
  read_excel(here(path_raw, 
                  "RXF Fuels Data for R 2022-02-07.xlsx"), 
             sheet = "Downed Woody Debris")  %>%
  # Clean column names 
  clean_names() %>%
  # Reorder columns
  rename(fuel_class_orig = fuel_class, 
         value = tons_acre) %>%
  left_join(rename_wd_fuel, "fuel_class_orig") %>%
  left_join(lab_year, "year") %>%
  left_join(lab_fuel, "fuel_class") %>%
  select(-fuel_class_orig) %>%
  # Convert datetime values to date values
  mutate(date = as_date(date), 
         # Convert measurements to metric units
         # 1 US ton per acre = 2.242 metric tons per ha
         value_si = value * 2.242, 
         # Standardize transect number
         transect_n = str_remove(transect, "Fuels"),
         transect_rep = "a",
         data_type = "wd",
         units = "tons_per_acre", 
         units_si = "tons_per_hectare",
         lab_type = "Coarse woody debris") %>%
  unite(plot_tran,
        c(plot_id, transect_n), 
        remove = FALSE) %>%
  select(year, 
         plot_id, 
         timing, 
         date, 
         plot_tran,
         transect_n, 
         transect_rep,
         data_type,
         fuel_class, 
         starts_with("value"),
         starts_with("units"), 
         lab_fuel,
         starts_with("lab_year"), 
         lab_type)

fuels_wd_raw %>%
  write_csv(here(path_tidy, "tubbs_tidy-data_wd.csv"))

# Calculate plot-level mean and total values ----
# Calculate plot-level mean by class  
wd_yr_plot_class_mean <- fxn_yr_plot_class_mean(fuels_wd_raw)  

# Calculate plot-level total by summing the mean for each class  
wd_yr_plot_total <- fxn_yr_plot_total(wd_yr_plot_class_mean)  

# ---------------------------------------------------------- -----

# DUFF AND LITTER (dl) -----
# Read raw data; reshape and annotate ----
fuels_dl_raw <- 
  read_excel(here(path_raw, 
                  "RXF Fuels Data for R 2022-02-07.xlsx"), 
             sheet = "Litter and Duff")  %>%
  # Clean column names 
  clean_names() %>%
  gather(fuel_class, value, litter1a:duff2b) %>%
  # Convert datetime values to date values
  mutate(date = as_date(date), 
         # Convert measurements to metric units
         # 1 in =  2.54 cm
         value_si = value * 2.54, 
         # Standardize transect number
         transect_n = substrRight(fuel_class, 2),
         transect_rep = substrRight(transect_n, 1),
         transect_n = str_sub(transect_n, 1, 1),
         fuel_class = ifelse(str_detect(fuel_class, "litter"),
                             "litter",
                             "duff"),
         # transect_n = str_remove(transect, "Fuels"),
         data_type = "dl",
         units = "depth_in_inches", 
         units_si = "depth_in_cm",
         lab_type = "Duff & litter") %>%
  left_join(lab_year, "year") %>%
  left_join(lab_fuel, "fuel_class") %>%
  unite(plot_tran,
        c(plot_id, transect_n), 
        remove = FALSE) %>%
  select(year, 
         plot_id, 
         timing, 
         date, 
         plot_tran,
         transect_n, 
         transect_rep,
         data_type,
         fuel_class, 
         starts_with("value"),
         starts_with("units"), 
         lab_fuel,
         starts_with("lab_year"), 
         lab_type)

fuels_dl_raw %>%
  write_csv(here(path_tidy, "tubbs_tidy-data_dl.csv"))

# Calculate plot-level mean and total values ----
# Calculate plot-level mean by class 
dl_yr_plot_class_mean <- fxn_yr_plot_class_mean(fuels_dl_raw)  

# Calculate plot-level total by summing the mean for each class  
dl_yr_plot_total <- fxn_yr_plot_total(dl_yr_plot_class_mean)  

# ---------------------------------------------------------- -----
# Combine wd and dl data tables; write csv ----
all_fuel_raw <- 
  bind_rows(fuels_dl_raw, fuels_wd_raw) %>%
  # left_join(ord_fuel, "fuel_class") %>%
  arrange(year, data_type, fuel_class) %>%
  mutate(fuel_class = as_factor(fuel_class), 
         lab_year = as_factor(lab_year), 
         lab_fuel = as_factor(lab_fuel),
         year = as_factor(year)) %>%
  write_csv(here(path_tidy, "tubbs_tidy-data_all.csv"))

#   Derived data
all_fuel_class_mean <- 
  bind_rows(dl_yr_plot_class_mean, 
            wd_yr_plot_class_mean) %>%
  write_csv(here(path_derived, "tubbs_mean-by-plot-type-class-yr.csv"))

all_fuel_total <- 
  bind_rows(dl_yr_plot_total, 
            wd_yr_plot_total)  %>%
  write_csv(here(path_derived, "tubbs_total-by-plot-type-yr.csv"))

# ========================================================== -----
# THIN: THREE TRANSECTS PER PLOT ----
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

rename_thin <- 
  lookup_variables %>%
  filter(subset %in% "thin") %>%
  select(fuel_class_orig = name_orig, 
         fuel_class = name, 
         lab_fuel = label,
         data_type = data_type,
         transect_rep = transect_rep,
         units = units) 

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

# wd_tran <- 
  input_thin %>%
  filter(data_type %in% "wd") %>%
  # fxn_tranform_ordnorm(index_list = list_classes_wd)  %>%
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

# wd_tran %>%
#   write_csv(here(path_derived, "thin_wd_transformed_metric-units.csv"))
# ---------------------------------------------------------- -----
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

