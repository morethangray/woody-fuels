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
  select(data_type, 
         metric,
         fuel_class, 
         timing, 
         plot_id, 
         value, 
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
  arrange(is_normal, timing)  %>%
  filter(is_normal == FALSE)
# Shapiro test results indicated total and mean duff and litter were normally distributed at each time point

# Identify appropriate transformation ----
dl_transform_eval <- 
  fxn_tranform_eval(index_data = input_dl, 
                    index_list = c("all", "duff", "litter"))  %>%
  gather(metric, value, value_raw:value_sqrt)  

# Check outliers 
dl_transform_eval %>%
  group_by(fuel_class, timing, metric) %>%
  identify_outliers(value) %>%
  clean_names()  %>%
  filter(is_extreme == TRUE) %>%
  group_by(fuel_class, timing, metric) %>%
  count() 

# Post-thinning litter with an ordernorm transformation had no outliers 
# fuel_class timing  metric        is_extreme     n
# 1 duff       survey1 value_log     FALSE          1
# 2 litter     survey2 value_arcsine TRUE           2
# 3 litter     survey2 value_log     TRUE           2
# 4 litter     survey2 value_ordnorm FALSE          1
# 5 litter     survey2 value_raw     TRUE           2
# 6 litter     survey2 value_sqrt    TRUE           2

# Check normality
dl_transform_eval %>%
  select(id = plot_id, 
         time = timing, 
         score = value, 
         treatment = fuel_class, 
         metric) %>%
  group_by(time, treatment, metric) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class = treatment, 
         year = time, 
         is_normal,
         metric,
         p, 
         statistic) %>%
  arrange(is_normal, metric, fuel_class, year) %>%
  filter(is_normal == FALSE) 

# Plot data distribution by transformation type  ----
dl_transform_eval %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric) + 
  xlim(-5, 10)   +
  labs(title = "Distribution of values, by fuel class and transformation",
       x = "Mean fuel depth (cm)",
       y = "Density")   
# Create transformed data ----
dl_value_ordnorm <- bestNormalize::orderNorm(input_dl$value, standardize = TRUE)

dl_transform <- 
  input_dl %>% 
  mutate(value_tran = dl_value_ordnorm$x.t) %>%
  relocate(value_tran, .before = value)

# Create boxplots of transformed data by fuel_class x timing ----
dl_transform %>%
  ggboxplot(x = "lab_thin", 
            y = "value_tran", 
            fill = "timing", 
            outlier.size = 0.3,
            palette = colors_thin_faded) + 
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray30") +
  theme(legend.position = "none", 
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(lab_thin) str_wrap(lab_thin, width = 20)) + 
  labs(title = paste0(index_dl, " (all)"), 
       y = "Mean fuel depth (cm)", 
       caption = "Values have been ordNorm transformed and standardized by fuel class") 

# Compare statistical test output: raw vs transformed data ----
dl_transform %>%
  filter(fuel_class %in% "all") %>%
  pairwise_t_test(value_tran ~ timing, paired = TRUE, p.adjust.method = "bonferroni")  

dl_transform %>%
  filter(fuel_class %in% "all") %>%
  pairwise_t_test(value ~ timing, paired = TRUE, p.adjust.method = "bonferroni") 

dl_transform %>%
  filter(fuel_class %in% "all") %>%
  anova_test(dv = value, wid = plot_id, within = timing)

dl_transform %>%
  filter(fuel_class %in% "all") %>%
  anova_test(dv = value_tran, wid = plot_id, within = timing)
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
