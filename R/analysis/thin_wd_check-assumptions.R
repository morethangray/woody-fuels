# revised: 2022-10-31 ----
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
input_data <- 
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

# ========================================================== -----
# COARSE WOODY DEBRIS (WD) ----
# Notes about input data ----
# hr1000r: 29 of the 30 values were 0 
# Create data frames ----
input_wd <- 
  input_data %>%
  filter(data_type %in% "wd")

# Check outliers ----
input_wd %>%
  group_by(timing, fuel_class) %>% 
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

# Check normality ----
input_wd %>%
  # hr1000r: 29 of the 30 values were 0 
  # Exclude hr1000r because we already know it's not normal
  filter(fuel_class %nin% "hr1000r") %>%
  select(id = plot_id, 
         time = timing, 
         score = value, 
         fuel_class) %>%
  group_by(fuel_class, time) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         timing = time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, timing)   %>%
  filter(is_normal == FALSE)

# Shapiro test results indicated total CWD was normally distributed at each time point
# Two fuel classes in the dw subset are not normally distributed: 100-hr, 1000-hr sound. 
# Shapiro test results by fuel class indicated 100-hr, 1000-hr sound (pre-thin) were not normally distributed 

# Plot raw data by fuel_class x timing ----
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


# 1-hr: ordernorm
# 10-hr: ordernorm, sqrt
# 100-hr: nothing looks great, maybe ordernorm?
# 1000-hr rotten: nothing looks great, maybe sqrt? maybe ordernorm?
# 1000-hr sound: raw values look good, then ordernorm  

all_transform <- 
  fxn_tranform_2()  %>%
  relocate(starts_with("value"), .after = "plot_id")  

# Apply functions to normalize and standardize the data by fuel class 
bn_hr_0001 <- all_transform %>% 
  filter(fuel_class %in% "hr0001") %>%
  select(fuel_class, year, plot_id, value_raw, value_ordnorm)

bn_hr_0010 <- all_transform %>% 
  filter(fuel_class %in% "hr0010") %>%
  select(fuel_class, year, plot_id, value_raw, value_ordnorm)

bn_hr_0100 <- all_transform %>% 
  filter(fuel_class %in% "hr0100") %>%
  select(fuel_class, year, plot_id, value_raw, value_ordnorm)

bn_hr_1000r <- all_transform %>% 
  filter(fuel_class %in% "hr1000r") %>%
  select(fuel_class, year, plot_id, value_raw, value_ordnorm)

bn_hr_1000s <- all_transform %>% 
  filter(fuel_class %in% "hr1000s") %>%
  select(fuel_class, year, plot_id, value_raw, value_ordnorm)

# Combine subsets 
input_class_transform <- bind_rows(bn_hr_0001, 
                                   bn_hr_0010,
                                   bn_hr_0100, 
                                   bn_hr_1000r, 
                                   bn_hr_1000s)  

input_class_transform %>%
  gather(metric, value, value_raw:value_ordnorm) %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(fuel_class~metric, scales = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density") + 
  xlim(-5, 10)

input_class_transform %>%
  gather(metric, value, value_raw:value_ordnorm) %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(metric ~ fuel_class, scales = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density") + 
  xlim(-5, 10)

# List the transformations to use by fuel class
# list_transform <- c("log_x", "log_x", "log_x", "arcsinh_x", "arcsinh_x")

# 1-hr: ordernorm
# 10-hr: ordernorm, sqrt
# 100-hr: nothing looks great, maybe ordernorm?
# 1000-hr rotten: nothing looks great, maybe sqrt? maybe ordernorm?
# 1000-hr sound: raw values look good, then ordernorm  

# # Apply functions to normalize and standardize the data by fuel class 
# bn_hr_0001 <- fxn_transform_log(index_class = "hr0001")
# bn_hr_0010 <- fxn_transform_log(index_class = "hr0010")
# bn_hr_0100 <- fxn_transform_log(index_class = "hr0100")
# bn_hr_1000r <- fxn_transform_arcsine(index_class = "hr1000r")
# bn_hr_1000s <- fxn_transform_arcsine(index_class = "hr1000s")

# Combine subsets 
input_class_transform <- fxn_tranform_ordnorm() %>%
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
nput_class_transform %>%
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


# Standardize data subsets by fuel class ----
# We applied an arcsine transformation to normalize the plot-level values for total coarse woody debris, calculated as log(x + sqrt(x\^2 + 1)). The arcsine transformation (also called the arcsine square root transformation, or the angular transformation) was identified as the most suitable method using the R function bestNormalize() [bestNormalize package]. Plot-level values for total and for mean duff and litter were standardized to have a mean of 0 and standard deviation of 1.
# 
# The transformed values for total coarse woody debris were normally distributed at each time point (p \> 0.05), as assessed by Shapiro-Wilk's test.

bn_total <- bestNormalize::arcsinh_x(input_total$value, standardize = TRUE)

input_total_transform <- 
  input_total %>%
  mutate(value = bn_total$x.t) 

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

# The two outliers (not extreme) detected in the untransformed data were absent after the transformation was applied.
input_total_transform %>%
  group_by(timing) %>%
  identify_outliers(value)  %>%
  mutate(data_type = index_type,
         fuel_class = "All") %>%
  clean_names() %>%
  select(data_type, fuel_class, timing, plot_id, value, 
         starts_with("is"))  
# filter(is_extreme == TRUE)  

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
