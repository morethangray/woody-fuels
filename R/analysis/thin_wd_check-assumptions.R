# revised: 2022-11-01 ----
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

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_3-derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "data-transformation.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_wd <- "Coarse woody debris"

# Define plot colors 
colors_thin_bright <- c("#9d9596", "#069879")
colors_thin_faded <- c("#bfbabb", "#75bca8")

# Read and subset thin data  
input_wd <- 
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
  relocate(c(metric, subset), .after = value)  %>%
  filter(data_type %in% "wd")

list_classes_wd <- 
  input_wd %>%
  filter(metric %in% "mean") %>%
    distinct(fuel_class) %>%
    pull()

list_classes_all <- unique(input_wd$fuel_class)
# ========================================================== -----
# COARSE WOODY DEBRIS (WD) ----
# About this data set ----
# hr1000r: 29 of the 30 transect measurements were 0 
# we now have additional fuels data for forests that were burned in Tubbs and recently thinned
# It will not be a repeated measures test because we only have two time steps. 
# The key thing to account for is that our new forest plots have three transects per plot instead of just two 
# All other sampling is the same as the RXF fuels data. 
# There should be five plots in this small set - FOR05, 06, 07, 08, 10.

# Check assumptions ----
#   Outliers ----
input_wd %>%
  group_by(timing, fuel_class, metric) %>% 
  identify_outliers(value) %>%
  clean_names() %>%
  select(data_type, 
         metric,
         fuel_class, 
         timing, 
         plot_id, 
         value, 
         starts_with("is")) %>%
  filter(is_extreme == TRUE)

# No extreme outliers for total CWD
# Three extreme outliers by class : 
#   Pre-thinning hr0100 and hr1000r in FOR08
#   Post-thinning hr0001 in FOR06

#   Normality ----
input_wd %>%
  # hr1000r: 29 of the 30 values were 0 
  # Exclude hr1000r because we already know it's not normal
  filter(fuel_class %nin% "hr1000r") %>%
  select(id = plot_id, 
         time = timing, 
         score = value, 
         fuel_class, 
         metric) %>%
  group_by(fuel_class, time, metric) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         timing = time, 
         metric,
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, timing)   %>%
  filter(is_normal == FALSE)

# Shapiro test results indicated total CWD was normally distributed at each time point
# Two fuel classes in the wd subset are not normally distributed: 100-hr, 1000-hr sound. 
# Shapiro test results by fuel class indicated 100-hr, 1000-hr sound (pre-thin) were not normally distributed 

#   Create QQ plots ----
# Plot raw data by fuel_class x timing  
ggqqplot(input_wd, 
         "value", 
         palette = colors_thin_bright,
         color = "lab_thin") + 
  facet_wrap(~lab_fuel, scales = "free") + 
  theme(legend.position = "right") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "gray30")  

# Subset to 100-hr, 1000-hr sound (not normally distributed) and 1-hr (outlier):
input_wd %>%
  filter(fuel_class %in% c("hr0001", "hr0100", "hr1000s")) %>%
  ggqqplot("value", 
           palette = colors_thin_bright,
           color = "lab_thin") + 
  # facet_grid(~lab_thin, scales = "free") + 
  facet_wrap(~lab_fuel, scales = "free") + 
  theme(legend.position = "top") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "gray78")

# Identify appropriate transformation ----
wd_transform_eval <- 
  fxn_tranform_eval(index_data = input_wd, 
                    index_list = list_classes_all)  %>%
  gather(metric, value, value_raw:value_sqrt)  

#   Check outliers  ---- 
check_transform_outlier <- 
  wd_transform_eval %>%
  group_by(timing, fuel_class, metric) %>% 
  identify_outliers(value) %>%
  clean_names() %>%
  select(metric,
         fuel_class, 
         timing, 
         plot_id, 
         value, 
         starts_with("is")) 
# Pre-thinning hr0100 in FOR08 was improved
check_transform_outlier %>%
  filter(fuel_class %in% "hr0100" & timing %in% "survey1" & plot_id %in% "FOR08", 
         is_extreme == FALSE) 
# orderNorm
# log
# arcsine

# Post-thinning hr0001 in FOR06
check_transform_outlier %>%
  filter(fuel_class %in% "hr0001" & timing %in% "survey2" & plot_id %in% "FOR06", 
         is_extreme == FALSE) 
# orderNorm

# Pre-thinning hr1000r in FOR08
check_transform_outlier %>%
  filter(fuel_class %in% "hr1000r" & timing %in% "survey1" & plot_id %in% "FOR08", 
         is_extreme == FALSE) 
# No transformation helped this value (remained extreme outlier)

# 1-hr: ordernorm
# 10-hr: N/A
# 100-hr: ordernorm, log, arcsine
# 1000-hr rotten: no tranformation helped
# 1000-hr sound: N/A

#   Check normality  ---- 
# Two fuel classes in the wd subset were not normally distributed: 100-hr, 1000-hr sound. 
check_transform_normal <- 
  wd_transform_eval %>%
  # hr1000r: 29 of the 30 values were 0 
  # Exclude hr1000r because we already know it's not normal
  filter(fuel_class %nin% "hr1000r") %>%
  select(id = plot_id, 
         time = timing, 
         score = value, 
         fuel_class, 
         metric) %>%
  group_by(fuel_class, time, metric) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         timing = time, 
         metric,
         is_normal,
         p, 
         statistic)

# Pre-thinning hr0100 
check_transform_normal %>%
  filter(fuel_class %in% "hr0100" & timing %in% "survey1", 
         is_normal == TRUE) 
# orderNorm
# log
# arcsine

# Pre-thinning hr1000s 
check_transform_normal %>%
  filter(fuel_class %in% "hr1000s" & timing %in% "survey1", 
         is_normal == TRUE) 
# No transformation normalized hr1000s

# 1-hr: N/A
# 10-hr: N/A
# 100-hr: ordernorm, log, arcsine
# 1000-hr rotten: N/A
# 1000-hr sound: No transformation helped

wd_transform_eval %>%
  filter(metric %nin% "value_sqrt") %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(fuel_class~metric, scales = "free") +
  theme(panel.spacing = unit(1, "lines"), 
        legend.position = "right") +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density") + 
  xlim(-5, 10)
# START WORKING HERE -----

input_class_transform <- 
  fxn_tranform_ordnorm(index_data = input_wd, 
                       index_list = list_classes_all) %>%
  rename(value = value_ordnorm)

input_class_transform %>%
  select(fuel_class, value_raw, value) %>% 
  gather(metric, value, value_raw:value) %>%
  group_by(fuel_class, metric) %>%
  summarize(mean = fxn_digit(mean(value, na.rm = TRUE)), 
            sd = fxn_digit(sd(value, na.rm = TRUE))) %>%
  ungroup() %>%
  gather(metric, number, mean:sd) %>%
  spread(fuel_class, number) 

# The following plot shows the raw (untransformed) values on the left and the transformed values on the right.
input_class_transform %>%
  rename(raw_values = value_raw, transformed_values = value) %>%
  gather(metric, value, raw_values:transformed_values) %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(metric ~ fuel_class, scales = "free") +
  theme(legend.position = "right", 
        axis.text = element_text(size = 8),
        panel.spacing = unit(1, "lines"),
        legend.text=element_text(size=8),
        # strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "gray91", color = "gray91"),
        panel.border = element_rect(fill = NA, color = "gray91", size = 2)) + 
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density", 
       color = "Fuel class") + 
  xlim(-5, 10)

# Recheck assumptions for standardized data   ----
input_total_transform %>%
  select(id = plot_id, 
         time = timing, 
         score = value) %>%
  group_by(time) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(data_type = index_type,
         fuel_class = "All", 
         metric = fxn_digit(metric), 
         is_normal = p>0.05) %>%
  select(data_type, 
         fuel_class, 
         timing = time, 
         is_normal,
         p, 
         metric) %>%
  arrange(is_normal, timing)  
# filter(is_normal == FALSE)  

ggqqplot(input_total_transform, "value", 
         facet.by = "timing", 
         color = "timing",
         palette = colors_thin)

norm_class_transform <- 
  input_class_transform %>%
  filter(fuel_class %nin% "hr1000s") %>%
  select(id = plot_id, 
         time = timing, 
         score = value, 
         treatment = fuel_class) %>%
  group_by(time, treatment) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(data_type = index_type,
         metric = fxn_digit(metric), 
         is_normal = p>0.05) %>%
  select(data_type, 
         fuel_class = treatment, 
         timing = time, 
         is_normal,
         p, 
         metric) %>%
  arrange(is_normal, fuel_class, timing) %>%
  filter(is_normal == FALSE) 

ggqqplot(input_class_transform, 
         "value", 
         facet.by = "fuel_class", 
         color = "timing",
         palette = colors_thin) +
  labs(caption = "Values have been normalized and standardized by fuel class")

# ========================================================== -----
