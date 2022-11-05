# revised: 2022-11-04 ----
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
library(bestNormalize)

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
# source(file = here(path_fxn, "data-transformation.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

# Define plot colors 
colors_tubbs_bright <- c("#9d9596", "#069879")
colors_tubbs_faded <- c("#bfbabb", "#75bca8")

# Read and subset tubbs data  
tubbs_input <- 
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
  arrange(time, plot_id, fuel_type, fuel_class) %>%
  mutate_if(is.character, as_factor) 

input_dl <-
  tubbs_input %>%
  filter(fuel_type %in% "dl")

input_wd <-
  tubbs_input %>%
  filter(fuel_type %in% "wd")

# ========================================================== -----
# DUFF & LITTER (DL)  ----
# Check assumptions using raw values  ----
#   Outliers ----
input_dl %>%
  group_by(time, fuel_class) %>% 
  identify_outliers(si_value) %>%
  clean_names() %>%
  select(fuel_type, 
         metric,
         fuel_class, 
         time, 
         plot_id, 
         si_value, 
         starts_with("is")) %>%
  filter(is_extreme == TRUE)

# Extreme outliers:
#   Total: 2017 at RxF03 
#   Duff: 2021 at RxF03, 2021 at RxF04

#   Normality ----
input_dl %>%
  select(id = plot_id, 
         time = time, 
         score = si_value, 
         fuel_class) %>%
  group_by(fuel_class, time) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, time)  %>%
  filter(is_normal == FALSE)
# Shapiro test results indicated total, duff, litter were normally distributed at each time point except: 
#  Total: 2017
#  Litter: 2017
#  Duff: 2021

#   Create QQ plots ----
# Plot raw data by fuel_class x time  
# ggqqplot(input_dl, 
#          "si_value", 
#          palette = colors_tubbs_bright,
#          color = "lab_time") + 
#   facet_wrap(~lab_class, scales = "free") + 
#   theme(legend.position = "right") +
#   geom_hline(yintercept = 0, linetype = "longdash", color = "gray30")  

# Evaluate multiple normalization methods ----
dl_eval <- 
  input_dl %>%
  group_by(fuel_class) %>%
  mutate(value_std = as.vector(scale(si_value)), 
         value_arcsine = arcsinh_x(si_value, standardize = TRUE)$x.t, 
         value_log = log_x(si_value, standardize = TRUE)$x.t, 
         value_ordnorm = orderNorm(si_value, standardize = TRUE)$x.t, 
         value_sqrt = sqrt_x(si_value, standardize = TRUE)$x.t) %>%
  ungroup() %>%
  relocate(c(si_value, us_value), .after = last_col()) %>%
  gather(method, number, value_std:us_value) %>%
  select(-starts_with("lab"), 
         -starts_with("order"))

#   Outliers ----
dl_eval_outlier <- 
  dl_eval %>%
  group_by(fuel_class, time, method) %>%
  identify_outliers(number) %>%
  clean_names()  

dl_eval_outlier %>%
  filter(is_extreme == TRUE) %>%
  group_by(fuel_class, time, is_extreme, method) %>%
  count() 
# 18 extreme outliers 

# Extreme outliers in raw data:
#   Total: 2017 at RxF03 
#   Duff: 2021 at RxF03, 2021 at RxF04

# Total in 2017 at RxF03 was improved: log, arcsine
dl_eval_outlier %>%
  filter(fuel_class %in% "all" & time %in% "2017" & plot_id %in% "RxF03", 
         is_extreme == FALSE) 

# Duff in 2021 at RxF03 was not improved by any transformation
dl_eval_outlier %>%
  filter(fuel_class %in% "duff" & time %in% "2021" & plot_id %in% "RxF03", 
         is_extreme == FALSE) 
# Duff in 2021 at RxF04 was not improved by any transformation
dl_eval_outlier %>%
  filter(fuel_class %in% "duff" & time %in% "2021" & plot_id %in% "RxF04", 
         is_extreme == FALSE) 

#   Normality ----
dl_eval_normal <- 
  dl_eval %>%
  select(id = plot_id, 
         time,
         score = number,
         treatment = fuel_class,
         method) %>%
  group_by(time, treatment, method) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(is_normal = p>0.05) %>%
  select(fuel_class = treatment,
         time,
         is_normal,
         method,
         p,
         statistic) %>%
  arrange(is_normal, fuel_class, method, time) 

dl_eval_normal %>%
  filter(is_normal == FALSE)
# 16 non-normal

#  Total: 2017
#  Litter: 2017
#  Duff: 2021

# Total in 2017: ordnorm, log, arcsine, sqrt
dl_eval_normal %>%
  filter(fuel_class %in% "all" & time %in% "2017", 
         is_normal == TRUE) 
# Litter in 2017: ordnorm, log, arcsine, sqrt
dl_eval_normal %>%
  filter(fuel_class %in% "litter" & time %in% "2017", 
         is_normal == TRUE) 
# Duff in 2021: No transformations improved this 
dl_eval_normal %>%
  filter(fuel_class %in% "duff" & time %in% "2021", 
         is_normal == TRUE) 

#   Plot data distribution by transformation type  ----
dl_eval %>%
  filter(method %nin% "value_sqrt") %>%
  ggdensity(x = "number", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~method, scales = "free") +
  theme(panel.spacing = unit(1, "lines"), 
        legend.position = "right") +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Fuel load",
       y = "Density") + 
  xlim(-5, 10)

# Normalize values using best method (orderNorm) ----
dl_norm <- 
  input_dl %>% 
  group_by(fuel_class) %>%
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) 

# ========================================================== -----
# COARSE WOODY DEBRIS (WD) ----

# Check assumptions using raw values ----
#   Outliers ----
input_wd %>%
  group_by(time, fuel_class, metric) %>% 
  identify_outliers(si_value) %>%
  clean_names() %>%
  select(fuel_type, 
         metric,
         fuel_class, 
         time, 
         plot_id, 
         si_value, 
         starts_with("is")) %>%
  filter(is_extreme == TRUE) %>%
  select(fuel_class, time, plot_id, metric, is_extreme)

# No extreme outliers for total CWD
# Nine extreme outliers by class : 
# fuel_class  time plot_id metric is_extreme
# 1 hr1000r     2016 RxF04   mean   TRUE      
# 2 hr0100      2017 RxF03   mean   TRUE      
# 3 hr0100      2017 RxF08   mean   TRUE      
# 4 hr0001      2021 RxF03   mean   TRUE      
# 5 hr0001      2021 RxF04   mean   TRUE      
# 6 hr0001      2021 RxF05   mean   TRUE      
# 7 hr0001      2021 RxF08   mean   TRUE      
# 8 hr0100      2021 RxF04   mean   TRUE      
# 9 hr0100      2021 RxF05   mean   TRUE      

#   Normality ----
input_wd %>%
  # hr1000r: 67 of the 72 values were 0 
  # Exclude hr1000r because we already know it's not normal
  filter(fuel_class %nin% "hr1000r") %>%
  select(id = plot_id, 
         time = time, 
         score = si_value, 
         fuel_class, 
         metric) %>%
  group_by(fuel_class, time, metric) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(is_normal = p>0.05) %>%
  select(fuel_class, 
         time, 
         metric,
         is_normal,
         p, 
         statistic) %>%
  arrange(fuel_class, time)   %>%
  filter(is_normal == FALSE)

# fuel_class  time metric is_normal         p statistic
# 1 all         2016 total  FALSE     0.0133        0.784
# 2 all         2017 total  FALSE     0.00905       0.769
# 3 all         2019 total  FALSE     0.00160       0.705
# 4 hr0010      2017 mean   FALSE     0.00594       0.753
# 5 hr0100      2017 mean   FALSE     0.0000258     0.551
# 6 hr0100      2019 mean   FALSE     0.000166      0.620
# 7 hr0100      2021 mean   FALSE     0.0000216     0.545
# 8 hr1000s     2016 mean   FALSE     0.00362       0.735
# 9 hr1000s     2017 mean   FALSE     0.00661       0.757
# 10 hr1000s     2019 mean   FALSE     0.000852      0.681
# 11 hr1000s     2021 mean   FALSE     0.0320        0.817

# Shapiro test results indicated values were NOT normally distributed for: 
#  Total in 2016, 2017, 2019
#  10h in 2017
#  100h in 2017, 2019, 2021
#  1000hs in 2016, 2017, 2019, 2021

#   Create QQ plots ----
# Plot raw data by fuel_class x time  
ggqqplot(input_wd, 
         "si_value", 
         # palette = colors_tubbs_bright,
         color = "lab_time") + 
  facet_wrap(~lab_class, scales = "free") + 
  theme(legend.position = "right") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "gray30")  

# Evaluate multiple normalization methods ----
wd_eval <- 
  input_wd %>%
  group_by(fuel_class) %>%
  mutate(value_std = as.vector(scale(si_value)), 
         value_arcsine = arcsinh_x(si_value, standardize = TRUE)$x.t, 
         value_log = log_x(si_value, standardize = TRUE)$x.t, 
         value_ordnorm = orderNorm(si_value, standardize = TRUE)$x.t, 
         value_sqrt = sqrt_x(si_value, standardize = TRUE)$x.t) %>%
  ungroup() %>%
  relocate(c(si_value, us_value), .after = last_col()) %>%
  gather(method, number, value_std:us_value) %>%
  select(-starts_with("lab"), 
         -starts_with("order"))
#   Check outliers  ---- 
wd_eval_outlier <- 
  wd_eval %>%
  group_by(fuel_class, time, method) %>%
  identify_outliers(number) %>%
  clean_names()  

wd_eval_outlier %>%
  filter(is_extreme == TRUE) %>%
  group_by(fuel_class, time, is_extreme, method) %>%
  count() 

wd_eval_outlier %>%
  filter(is_extreme == TRUE) %>%
  distinct(fuel_class, time, plot_id)

wd_eval_outlier %>%
  filter(fuel_class %in% "hr0001",
         time %in% "2021",
         is_extreme == FALSE) %>%
  arrange(time) %>%
  select(fuel_class, time, method, plot_id) %>%
  spread(method, fuel_class)
# hr0001 in 2021 at RxF08 : ordnorm
# hr0001 in 2021 at RxF03: nothing helped
# hr0001 in 2021 at RxF04 : nothing helped
# hr0001 in 2021 at RxF05 : nothing helped

wd_eval_outlier %>%
  filter(fuel_class %in% "hr0010",
         is_extreme == FALSE) %>%
  arrange(time) %>%
  select(fuel_class, time, method, plot_id) %>%
  spread(method, fuel_class)
# hr0010 in 2016 at RxF03: sqrt
# hr0010 in 2021 at RxF06 : sqrt, arcsine, log  

wd_eval_outlier %>%
  filter(fuel_class %in% "hr0100",
         is_extreme == FALSE) %>%
  arrange(time) %>%
  select(fuel_class, time, method, plot_id) %>%
  spread(method, fuel_class)
# hr0100 in 2016 at RxF02: ordnorm  
# hr0100 in 2016 at RxF03: ordnorm
# hr0100 in 2017 at RxF03: nothing helped
# hr0100 in 2017 at RxF08 : nothing helped
# hr0100 in 2021 at RxF04 : nothing helped
# hr0100 in 2021 at RxF05 : nothing helped

wd_eval_outlier %>%
  filter(fuel_class %in% "hr1000r",
         is_extreme == FALSE) %>%
  arrange(time) %>%
  select(fuel_class, time, method, plot_id) %>%
  spread(method, fuel_class)
# hr1000r in 2016 at RxF04: sqrt

wd_eval_outlier %>%
  filter(fuel_class %in% "hr1000s",
         is_extreme == FALSE) %>%
  arrange(time) %>%
  select(fuel_class, time, method, plot_id) %>%
  spread(method, fuel_class)
# hr1000s in 2021 at RxF02:  

#   Check normality  ---- 
wd_eval_normal <- 
  wd_eval %>%
  # hr1000r: 29 of the 30 values were 0 
  # Exclude hr1000r because we already know it's not normal
  filter(fuel_class %nin% "hr1000r") %>%
  select(id = plot_id, 
         time,
         score = number,
         treatment = fuel_class,
         method) %>%
  group_by(time, treatment, method) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(is_normal = p>0.05) %>%
  select(fuel_class = treatment,
         time,
         is_normal,
         method,
         p,
         statistic) %>%
  arrange(is_normal, fuel_class, method, time) 

#  Total in 2016, 2017, 2019: arcsine, log, ordnorm
wd_eval_normal %>%
  filter(fuel_class %in% "all",
         is_normal == TRUE) %>%
  arrange(time) %>%
  select(fuel_class, time, method) %>%
  spread(method, fuel_class)

#  10h in 2017: nothing helped 
wd_eval_normal %>%
  filter(fuel_class %in% "hr0010",
         is_normal == TRUE) %>%
  arrange(time) %>%
  select(fuel_class, time, method) %>%
  spread(method, fuel_class)

#  100h in 2017, 2019, 2021: nothing helped 
wd_eval_normal %>%
  filter(fuel_class %in% "hr0100",
         is_normal == TRUE) %>%
  arrange(time) %>%
  select(fuel_class, time, method) %>%
  spread(method, fuel_class)

#  1000hs in 2016: nothing helped
#  1000hs in 2017, 2019, 2021: ordnorm
wd_eval_normal %>%
  filter(fuel_class %in% "hr1000s",
         is_normal == TRUE) %>%
  arrange(time) %>%
  select(fuel_class, time, method) %>%
  spread(method, fuel_class)

#   Plot data distribution by transformation type  ----
wd_eval %>%
  filter(method %nin% "us_value") %>%
  ggdensity(x = "number", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(fuel_class~method, scales = "free") +
  theme(panel.spacing = unit(1, "lines"), 
        legend.position = "right") +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Fuel load",
       y = "Density") + 
  xlim(-5, 10)

# Normalize values using best method (orderNorm) ----
# NEED TO NORMALIZE SUBSETS USING DIFFERENT METHODS
# ordNorm: total, 1, 100, 1000s
# sqrt: 10, 1000r
#
# Total: arcsine, log, ordnorm
# 1: ordnorm (outlier)
# 10: sqrt (outlier)
# 100: ordnorm (outlier), (nothing helped norm)
# 1000r: sqrt
# 1000s: ordnorm

wd_norm_ord <- 
  input_wd %>% 
  filter(fuel_class %in% c("all", "hr0001", "hr0100", "hr1000s"))
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) 
  
  


# [NOT RUN] ----
# Recheck assumptions for standardized data 
# wd_norm %>%
#   group_by(fuel_class, time) %>%
#   summarize(mean_si = fxn_digit(mean(si_value, na.rm = TRUE)), 
#             mean_us = fxn_digit(mean(us_value, na.rm = TRUE))) %>%
#   ungroup() %>%
#   arrange(time, fuel_class)
#   
# # The following plot shows the raw (untransformed) values on the left and the transformed values on the right.
# input_class_transform %>%
#   rename(raw_values = value_raw, transformed_values = value) %>%
#   gather(metric, si_value, raw_values:transformed_values) %>%
#   ggdensity(x = "value", color = "fuel_class") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   facet_grid(metric ~ fuel_class, scales = "free") +
#   theme(legend.position = "right", 
#         axis.text = element_text(size = 8),
#         panel.spacing = unit(1, "lines"),
#         legend.text=element_text(size=8),
#         # strip.text = element_text(size = 8),
#         strip.background = element_rect(fill = "gray91", color = "gray91"),
#         panel.border = element_rect(fill = NA, color = "gray91", size = 2)) + 
#   labs(title = "Distribution of values, by fuel class and transformation",
#        caption = "Note: axis scales differ between plots",
#        x = "Mean tons per acre",
#        y = "Density", 
#        color = "Fuel class") + 
#   xlim(-5, 10)
# 
# input_total_transform %>%
#   select(id = plot_id, 
#          time = time, 
#          score = value) %>%
#   group_by(time) %>%
#   shapiro_test(score)  %>%
#   clean_names() %>%
#   mutate(fuel_type = index_type,
#          fuel_class = "All", 
#          metric = fxn_digit(metric), 
#          is_normal = p>0.05) %>%
#   select(fuel_type, 
#          fuel_class, 
#          time = time, 
#          is_normal,
#          p, 
#          metric) %>%
#   arrange(is_normal, time)  
# # filter(is_normal == FALSE)  
# 
# ggqqplot(input_total_transform, "value", 
#          facet.by = "time", 
#          color = "time",
#          palette = colors_tubbs)
# 
# norm_class_transform <- 
#   input_class_transform %>%
#   filter(fuel_class %nin% "hr1000s") %>%
#   select(id = plot_id, 
#          time = time, 
#          score = si_value, 
#          treatment = fuel_class) %>%
#   group_by(time, treatment) %>%
#   shapiro_test(score)  %>%
#   clean_names() %>%
#   mutate(fuel_type = index_type,
#          metric = fxn_digit(metric), 
#          is_normal = p>0.05) %>%
#   select(fuel_type, 
#          fuel_class = treatment, 
#          time = time, 
#          is_normal,
#          p, 
#          metric) %>%
#   arrange(is_normal, fuel_class, time) %>%
#   filter(is_normal == FALSE) 
# 
# ggqqplot(input_class_transform, 
#          "value", 
#          facet.by = "fuel_class", 
#          color = "time",
#          palette = colors_tubbs) +
#   labs(caption = "Values have been normalized and standardized by fuel class")

# ========================================================== -----
