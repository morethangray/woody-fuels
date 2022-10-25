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
path_raw <- here("input/raw-data")
path_lookup <- here("input/lookup-tables")
path_tidy <- here("input/tidy-data")
path_cache <- here("input/cache")

# Write functions
"%nin%" <- Negate("%in%")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
# ========================================================== -----
# TWO TRANSECTS PER PLOT (TUBBS FIRE) ----
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
    summarize(value = mean(value)) %>%
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
    summarize(value = sum(value)) %>%
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
         # Standardize transect number
         transect_n = str_remove(transect, "Fuels"),
         transect_rep = "a",
         data_type = "wd",
         units = "tons_per_acre", 
         lab_type = "Downed woody debris") %>%
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
         value,
         units, 
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
         data_type = "dl",
         units = "depth_in_inches", 
         transect_n = substrRight(fuel_class, 2),
         transect_rep = substrRight(transect_n, 1),
         transect_n = str_sub(transect_n, 1, 1),
         fuel_class = ifelse(str_detect(fuel_class, "litter"), 
                             "litter",
                             "duff"), 
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
         value,
         units, 
         lab_fuel,
         starts_with("lab_year"), 
         lab_type)

fuels_dl_raw %>%
  write_csv(here(path_tidy, "tubbs_raw-data_dl.csv"))

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
  write_csv(here(path_cache, "tubbs_mean-by-plot-type-class-yr.csv"))

all_fuel_total <- 
  bind_rows(dl_yr_plot_total, 
            wd_yr_plot_total)  %>%
  write_csv(here(path_cache, "tubbs_total-by-plot-type-yr.csv"))

# ========================================================== -----
# THREE TRANSECTS PER PLOT (thin) ----
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

# Read raw data; reshape and annotate ----
fuels_thin_raw <- 
  read_excel(here(path_raw, 
                  "FOR_Fuels_MorganANOVA.xlsx"), 
             sheet = "Data") %>%
  # Clean column names 
  clean_names() %>%
  rename(lab_timing = timing) %>%
  gather(fuel_class_orig, value, x1hr_load:duff2) %>%
  left_join(rename_thin, "fuel_class_orig") %>%
  # Convert datetime values to date values
  mutate(date = as_date(date), 
         year = year(date), 
         timing = str_to_lower(str_sub(lab_timing, 1, 4)), 
         timing = str_remove_all(timing, "-"), 
         survey = ifelse(timing %in% "pre", "cont", "trmt")) %>%  
  unite(plot_tran,
        c(plot_id, transect_id), 
        remove = FALSE) %>%
  select(year, 
         plot_id, 
         survey,
         timing, 
         date, 
         plot_tran,
         transect_id, 
         transect_rep,
         data_type,
         fuel_class, 
         value,
         units, 
         lab_fuel,
         starts_with("lab"))  %>%
  write_csv(here(path_tidy, "thin_tidy-data.csv"))

# Calculate plot-level mean and total values: Raw data ----
# Calculate plot-level mean by fuel_class x plot x survey  
thin_plot_class_mean <-  
  fuels_thin_raw %>%
  group_by(survey, 
           plot_id, 
           data_type, 
           fuel_class,
           lab_fuel) %>%
  summarize(mean = mean(value)) %>%
  ungroup() %>%
  left_join(lab_type, "data_type") %>%
  mutate(subset = "survey_plot_class") %>%
  arrange(survey) %>%
  relocate(mean, .after = fuel_class) %>%
  write_csv(here(path_cache, "thin_mean-by-plot-type-class-trmt.csv"))

# Calculate plot-level total by summing the mean for each class 
thin_plot_total <- 
  thin_plot_class_mean  %>%
  group_by(survey, 
           plot_id, 
           data_type) %>%
  summarize(total = sum(mean)) %>%
  ungroup()  %>%
  mutate(subset = "survey_plot") %>%
  relocate(total, .after = data_type) %>%
  write_csv(here(path_cache, "thin_total-by-plot-type-trmt.csv"))

# Calculate plot-level mean and total values: Difference between treatment and control ----
# By plot x transect x quadrat 
fuels_thin_diff <- 
  fuels_thin_raw %>%
  select(survey, 
         plot_id, 
         data_type, 
         fuel_class, 
         plot_tran, 
         transect_id, 
         transect_rep, 
         value, 
         lab_fuel) %>%
  spread(survey, value) %>%
  mutate(diff = trmt - cont) %>%
  write_csv(here(path_tidy, "thin_tidy-data_all_diff.csv"))

# Calculate plot-level mean by fuel_class x plot x survey  
thin_diff_plot_class_mean <-  
  fuels_thin_diff %>%
  group_by(plot_id, 
           data_type, 
           fuel_class,
           lab_fuel) %>%
  summarize(mean = mean(diff)) %>%
  ungroup() %>%
  left_join(lab_type, "data_type") %>%
  mutate(subset = "survey_plot_class") %>%
  relocate(mean, .after = fuel_class) %>%
  write_csv(here(path_cache, "thin_diff_mean-by-plot-type-class.csv"))

# Calculate plot-level total by summing the mean for each class 
thin_diff_plot_total <- 
  thin_diff_plot_class_mean  %>%
  group_by(plot_id, 
           data_type, 
           lab_type) %>%
  summarize(total = sum(mean)) %>%
  ungroup()  %>%
  mutate(subset = "survey_plot") %>%
  relocate(total, .after = data_type) %>%
  write_csv(here(path_cache, "thin_diff_total-by-plot-type.csv"))

