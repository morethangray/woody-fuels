# revised: 2022-11-04 ----
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
path_derived <- here("input/data_derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "data-transformation.R"))

# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

# Define plot colors 
colors_thin_bright <- c("#9d9596", "#069879")
colors_thin_faded <- c("#bfbabb", "#75bca8")

# Read and subset thin data  
thin_input <- 
  read_csv(here(path_derived, "thin_derived.csv")) %>%
  mutate(si_value = fxn_digit(si_value)) %>%
  arrange(time, plot_id, fuel_type, fuel_class) %>%
  mutate_if(is.character, as_factor) 

input_dl <-
  thin_input %>%
  filter(fuel_type %in% "dl")

input_wd <-
  thin_input %>%
  filter(fuel_type %in% "wd")

# ========================================================== -----
# DUFF & LITTER (DL)  ----
# Check assumptions ----
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
# No extreme outliers for total duff and litter 
# Two extreme outliers for litter in post-thinning subset 

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
# Shapiro test results indicated total and mean duff and litter were normally distributed at each time point

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

# Check outliers 
dl_eval_outlier <- 
  dl_eval %>%
  group_by(fuel_class, time, method) %>%
  identify_outliers(number) %>%
  clean_names()  

dl_eval_outlier %>%
  filter(is_extreme == TRUE) %>%
  group_by(fuel_class, time, is_extreme, method) %>%
  count() 
# No outliers for orderNorm 

# fuel_class time  is_extreme method            n
# 1 litter     t2    TRUE       si_value          2
# 2 litter     t2    TRUE       us_value          2
# 3 litter     t2    TRUE       value_arcsine     2
# 4 litter     t2    TRUE       value_log         2
# 5 litter     t2    TRUE       value_sqrt        2
# 6 litter     t2    TRUE       value_std         2

# Check normality
dl_eval %>%
  select(id = plot_id, 
         time = time, 
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
  arrange(is_normal, method, fuel_class, time) %>%
  filter(is_normal == FALSE) 
# log-transformed values for duff not normal in t1 

#   Plot data distribution by transformation type  ----
dl_eval %>%
  ggdensity(x = "number", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~method) + 
  xlim(-5, 10)   +
  labs(title = "Distribution of values, by fuel class and transformation",
       x = "Mean fuel depth (cm)",
       y = "Density")   
# Normalize values using best method (orderNorm) ----
dl_norm <- 
  input_dl %>% 
  mutate(value_norm = orderNorm(si_value, standardize = TRUE)$x.t) 
# Check normalized data ----
#   Create boxplots of transformed data by fuel_class x time ----
dl_norm %>%
  ggboxplot(x = "lab_time", 
            y = "value_norm", 
            fill = "time", 
            outlier.size = 0.3,
            palette = colors_thin_faded) + 
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray30") +
  theme(legend.position = "none", 
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(lab_time) str_wrap(lab_time, width = 20)) + 
  labs(title = paste0(index_dl, " (all)"), 
       y = "Mean fuel depth (cm)", 
       caption = "Values have been ordNorm transformed and standardized by fuel class") 

#   Compare statistical test output: raw vs transformed data ----
dl_norm %>%
  filter(fuel_class %in% "all") %>%
  pairwise_t_test(value_norm ~ time, paired = TRUE, p.adjust.method = "bonferroni")  

dl_norm %>%
  filter(fuel_class %in% "all") %>%
  pairwise_t_test(si_value ~ time, paired = TRUE, p.adjust.method = "bonferroni") 

dl_norm %>%
  filter(fuel_class %in% "all") %>%
  anova_test(dv = si_value, wid = plot_id, within = time)

dl_norm %>%
  filter(fuel_class %in% "all") %>%
  anova_test(dv = value_norm, wid = plot_id, within = time)
# ========================================================== -----

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
         time = time, 
         score = si_value, 
         fuel_class, 
         metric) %>%
  group_by(fuel_class, time, metric) %>%
  shapiro_test(score) %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         time, 
         metric,
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, time)   %>%
  filter(is_normal == FALSE)

# Shapiro test results indicated total CWD was normally distributed at each time point
# Two fuel classes in the wd subset are not normally distributed: 100-hr, 1000-hr sound. 
# Shapiro test results by fuel class indicated 100-hr, 1000-hr sound (pre-thin) were not normally distributed 

#   Create QQ plots ----
# Plot raw data by fuel_class x time  
ggqqplot(input_wd, 
         "si_value", 
         palette = colors_thin_bright,
         color = "lab_time") + 
  facet_wrap(~lab_class, scales = "free") + 
  theme(legend.position = "right") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "gray30")  

# Subset to 100-hr, 1000-hr sound (not normally distributed) and 1-hr (outlier):
input_wd %>%
  filter(fuel_class %in% c("hr0001", "hr0100", "hr1000s")) %>%
  ggqqplot("si_value", 
           palette = colors_thin_bright,
           color = "lab_time") + 
  facet_wrap(~lab_class, scales = "free") + 
  theme(legend.position = "top") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "gray78")

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

# # Groups:   fuel_class, time, is_extreme, method [17]
# fuel_class time  is_extreme method            n
# 1 hr0001     t2    TRUE       si_value          1
# 2 hr0001     t2    TRUE       us_value          1
# 3 hr0001     t2    TRUE       value_arcsine     1
# 4 hr0001     t2    TRUE       value_log         2
# 5 hr0001     t2    TRUE       value_sqrt        1
# 6 hr0001     t2    TRUE       value_std         1
# 7 hr0100     t1    TRUE       si_value          1
# 8 hr0100     t1    TRUE       us_value          1
# 9 hr0100     t1    TRUE       value_sqrt        1
# 10 hr0100     t1    TRUE       value_std         1
# 11 hr1000r    t1    TRUE       si_value          1
# 12 hr1000r    t1    TRUE       us_value          1
# 13 hr1000r    t1    TRUE       value_arcsine     1
# 14 hr1000r    t1    TRUE       value_log         1
# 15 hr1000r    t1    TRUE       value_ordnorm     1
# 16 hr1000r    t1    TRUE       value_sqrt        1
# 17 hr1000r    t1    TRUE       value_std         1

# SRTART WORKING HERE ----
# Pre-thinning hr0100 in FOR08 was improved
wd_eval_outlier %>%
  filter(fuel_class %in% "hr0100" & time %in% "survey1" & plot_id %in% "FOR08", 
         is_extreme == FALSE) 
# orderNorm
# log
# arcsine

# Post-thinning hr0001 in FOR06
wd_eval_outlier %>%
  filter(fuel_class %in% "hr0001" & time %in% "survey2" & plot_id %in% "FOR06", 
         is_extreme == FALSE) 
# orderNorm

# Pre-thinning hr1000r in FOR08
wd_eval_outlier %>%
  filter(fuel_class %in% "hr1000r" & time %in% "survey1" & plot_id %in% "FOR08", 
         is_extreme == FALSE) 
# No transformation helped this si_value (remained extreme outlier)

# 1-hr: ordernorm
# 10-hr: N/A
# 100-hr: ordernorm, log, arcsine
# 1000-hr rotten: no tranformation helped
# 1000-hr sound: N/A


#   Check normality  ---- 
# from dl for reference
# wd_eval %>%
#   select(id = plot_id, 
#          time = time, 
#          score = number, 
#          treatment = fuel_class, 
#          method) %>%
#   group_by(time, treatment, method) %>%
#   shapiro_test(score)  %>%
#   clean_names() %>%
#   mutate(is_normal = p>0.05) %>%
#   select(fuel_class = treatment, 
#          time, 
#          is_normal,
#          method,
#          p, 
#          statistic) %>%
#   arrange(is_normal, method, fuel_class, time) %>%
#   filter(is_normal == FALSE) 
# # log-transformed values for duff not normal in t1 

# Two fuel classes in the wd subset were not normally distributed: 100-hr, 1000-hr sound. 
check_transform_normal <- 
  wd_transform_eval %>%
  # hr1000r: 29 of the 30 values were 0 
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
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class, 
         time = time, 
         metric,
         is_normal,
         p, 
         statistic)

# Pre-thinning hr0100 
check_transform_normal %>%
  filter(fuel_class %in% "hr0100" & time %in% "survey1", 
         is_normal == TRUE) 
# orderNorm
# log
# arcsine

# Pre-thinning hr1000s 
check_transform_normal %>%
  filter(fuel_class %in% "hr1000s" & time %in% "survey1", 
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
  rename(si_value = value_ordnorm)

input_class_transform %>%
  select(fuel_class, value_raw, value) %>% 
  gather(metric, si_value, value_raw:value) %>%
  group_by(fuel_class, metric) %>%
  summarize(mean = fxn_digit(mean(si_value, na.rm = TRUE)), 
            sd = fxn_digit(sd(si_value, na.rm = TRUE))) %>%
  ungroup() %>%
  gather(metric, number, mean:sd) %>%
  spread(fuel_class, number) 

# The following plot shows the raw (untransformed) values on the left and the transformed values on the right.
input_class_transform %>%
  rename(raw_values = value_raw, transformed_values = value) %>%
  gather(metric, si_value, raw_values:transformed_values) %>%
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
         time = time, 
         score = value) %>%
  group_by(time) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(fuel_type = index_type,
         fuel_class = "All", 
         metric = fxn_digit(metric), 
         is_normal = p>0.05) %>%
  select(fuel_type, 
         fuel_class, 
         time = time, 
         is_normal,
         p, 
         metric) %>%
  arrange(is_normal, time)  
# filter(is_normal == FALSE)  

ggqqplot(input_total_transform, "value", 
         facet.by = "time", 
         color = "time",
         palette = colors_thin)

norm_class_transform <- 
  input_class_transform %>%
  filter(fuel_class %nin% "hr1000s") %>%
  select(id = plot_id, 
         time = time, 
         score = si_value, 
         treatment = fuel_class) %>%
  group_by(time, treatment) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(fuel_type = index_type,
         metric = fxn_digit(metric), 
         is_normal = p>0.05) %>%
  select(fuel_type, 
         fuel_class = treatment, 
         time = time, 
         is_normal,
         p, 
         metric) %>%
  arrange(is_normal, fuel_class, time) %>%
  filter(is_normal == FALSE) 

ggqqplot(input_class_transform, 
         "value", 
         facet.by = "fuel_class", 
         color = "time",
         palette = colors_thin) +
  labs(caption = "Values have been normalized and standardized by fuel class")

# ========================================================== -----
