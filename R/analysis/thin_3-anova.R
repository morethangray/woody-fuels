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
library(patchwork)  ## To arrange multiple plots 
# library(lemon)  ## To manipulate faceted ggplots

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "statistical-tests.R"))
source(file = here(path_fxn, "data-transformation.R"))
# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

# Define plot colors 
colors_trmt_bright <- c("#9d9596", "#069879")
colors_trmt_faded <- c("#bfbabb", "#75bca8")
color_palette <- thematic::okabe_ito(2)
colors_thin <- 
  read_csv(here(path_lookup, "plot-colors.csv")) %>%
  arrange(palette, palette_subset, levels) %>%
  filter(palette %in% "year", 
         palette_subset %in% "1pre_3post") %>%
  pull(hex_code) 

# ========================================================== -----
# DUFF & LITTER (DL) ----
# Read and subset data  ----
dl_transform <- 
  read_csv(here(path_derived, "thin_dl_transformed_metric-units.csv")) %>%
  arrange(timing, plot_id, data_type, fuel_class) %>%
  mutate_if(is.character, as_factor)   

dl_transform_total <- 
  dl_transform %>%
  filter(data_type %in% "dl", 
         metric %in% "total")

dl_transform_class <- 
  dl_transform %>%
  filter(data_type %in% "dl", 
         metric %in% "mean")


# One-way ANOVA  ----
# Effect of timing (duff and litter combined)
dl_transform_total %>%
  fxn_aov_me(index_value = "value_tran", 
             index_id = "plot_id", 
             index_time = "timing")  %>%
  fxn_signif_adj() %>%
  mutate(data_type = index_dl,
         fuel_class = "All") %>%
  select(data_type, 
         fuel_class, 
         effect, 
         starts_with("p_adj"),
         statistic, 
         starts_with("d_"), 
         ges)

# Two-way ANOVA: Interaction between timing x fuel class  ----
dl_transform_class %>%
  fxn_aov2_me(index_value = "value_tran", 
              index_id = "plot_id", 
              index_time = "timing", 
              index_variable = "fuel_class") %>%
  fxn_signif_adj() %>%
  mutate(data_type = index_dl) %>%
  select(data_type, 
         effect, 
         starts_with("p_adj"), 
         statistic, 
         starts_with("d_"), 
         ges)  %>%
  fxn_kable()

# Significant interaction between timing and fuel class 

# Two-way post hoc tests: timing by fuel class ----
dl_transform_class %>%
  fxn_pwc2(index_value = "value_tran", 
           index_id = "plot_id", 
           index_time = "lab_thin", 
           index_variable = "fuel_class") %>%
  mutate(data_type = index_dl) %>%
  select(data_type, 
         fuel_class = variable, 
         starts_with("group"), 
         starts_with("p_adj"), 
         statistic, 
         starts_with("d_")) %>%
  fxn_kable()
# Significant difference in litter between pre- and post-thinning

# ========================================================== -----

# COARSE WOODY DEBRIS (WD) ----
# Notes about input data ----
# hr1000r: 29 of the 30 values were 0 
#
# Nine extreme outliers for CWD: 7/9 for hr1000s; the other two at FOR08
# data_type fuel_class timing  plot_id value is_outlier is_extreme
# 1 wd        all        survey1 FOR08   147.  TRUE       FALSE     
# 2 wd        hr1000s    survey1 FOR05    29.0 TRUE       TRUE      
# 3 wd        hr1000s    survey1 FOR07    45.3 TRUE       TRUE      
# 4 wd        hr0100     survey1 FOR08   110.  TRUE       TRUE      
# 5 wd        hr1000r    survey1 FOR08    34.8 TRUE       TRUE      
# 6 wd        hr1000s    survey2 FOR05   327.  TRUE       TRUE      
# 7 wd        hr1000s    survey2 FOR06    76.9 TRUE       TRUE      
# 8 wd        hr1000s    survey2 FOR07   373.  TRUE       TRUE      
# 9 wd        hr1000s    survey2 FOR08    61.4 TRUE       TRUE      
#10 wd        hr1000s    survey2 FOR10    41.5 TRUE       TRUE      
#
# Data for total is normally distributed 
#
# Shapiro test results indicate two fuel classes are not normally distributed: 100-hr, 1000-hr sound
# Identify appropriate transformation ----
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


# Standardize data and repeat checks ----
# We applied an arcsine transformation to normalize the plot-level values for total coarse woody debris, calculated as log(x + sqrt(x\^2 + 1)). The arcsine transformation (also called the arcsine square root transformation, or the angular transformation) was identified as the most suitable method using the R function bestNormalize() [bestNormalize package]. Plot-level values for total and for mean duff and litter were standardized to have a mean of 0 and standard deviation of 1.
# 
# The transformed values for total coarse woody debris were normally distributed at each time point (p \> 0.05), as assessed by Shapiro-Wilk's test.

bn_total <- bestNormalize::arcsinh_x(input_total$value, standardize = TRUE)

input_total_transform <- 
  input_total %>%
  mutate(value = bn_total$x.t) 

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

# One-way ANOVA  ----
# Effect of timing (all fuel classes)
input_total_transform %>%
  fxn_aov_me() %>%
  fxn_signif() %>%
  mutate(fuel_class = "All") %>%
  clean_names() %>%
  select(-p, 
         -p_05) %>%
  relocate(data_type, 
           fuel_class,
           effect, 
           starts_with("p_adj"), 
           metric, 
           starts_with("d_"), 
           ges) %>%
  fxn_kable()
# One-way post hoc tests ----
# We conducted post hoc pairwise comparisons between the levels of the within-subjects factor (here, timing). The result of paired t-tests between timings showed no significant difference in fuel load between timings at a significance level of \< 0.05. This finding is consistent with the lack of main effect found for timing.
# The results from the pairwise comparisons are shown below as (1) a boxplot of total coarse woody debris by timing (the lack of p-values reflects the absence of significant comparisons), and (2) a table of test results.

pwc <- 
  input_total_transform %>%
  pairwise_t_test(value ~ timing, paired = TRUE, p.adjust.method = "bonferroni")  %>% 
  add_xy_position(x = "timing")

aov_me <- 
  input_total_transform %>%
  anova_test(dv = value, wid = plot_id, within = timing)

p_total + 
  labs(subtitle = get_test_label(aov_me, detailed = TRUE),
       caption = get_pwc_label(pwc)) +
  stat_pvalue_manual(pwc, hide.ns = TRUE) 

# Effect of timing (all fuel classes)
input_total_transform %>%
  fxn_pwc() %>%
  mutate(metric = fxn_digit(metric),
         data_type = index_type,
         fuel_class = "All") %>%
  select(-p) %>%
  relocate(data_type, 
           fuel_class,
           starts_with("group"), 
           starts_with("p_adj"), 
           metric, 
           starts_with("d")) %>%
  arrange(p_adj)  

# Two-way ANOVA: Interaction between timing x fuel class  ----
input_class_transform  %>%
  fxn_aov2_me() %>%
  fxn_signif() %>%
  relocate(data_type, 
           effect, 
           starts_with("p_adj"), 
           metric, 
           starts_with("d_"), 
           ges)
# Two-way post hoc tests ----
#   1-hour vs. timing  ----
fxn_posthoc_plot_bt(index_subset = "hr0001", input_class_transform)  
fxn_posthoc_table(index_subset = "hr0001", input_class_transform)

#   10-hour vs. timing  ----
fxn_posthoc_plot_bt(index_subset = "hr0010", input_class_transform)
fxn_posthoc_table(index_subset = "hr0010", input_class_transform)

#   100-hour vs. timing  ----
fxn_posthoc_plot_bt(index_subset = "hr0100", input_class_transform)
fxn_posthoc_table(index_subset = "hr0100", input_class_transform)

#   1000-hour rotten vs. timing  ----
fxn_posthoc_plot_bt(index_subset = "hr1000r", input_class_transform)
fxn_posthoc_table(index_subset = "hr1000r", input_class_transform)

#   1000-hour sound vs. timing  ----
fxn_posthoc_plot_bt(index_subset = "hr1000s", input_class_transform)
fxn_posthoc_table(index_subset = "hr1000s", input_class_transform)

# ========================================================== -----
# CONDUCT STATISTICAL TESTS -----
# Plot-level total by fuel type (dw, gf) ----
# mean total fuel load ANOVA results and plot.
#   Main effect ----
fuels_by_plot_total %>%
  filter(data_type %in% "dw") %>%
  fxn_aov_me(index_time = "survey")

fuels_by_plot_total %>%
  filter(data_type %in% "gf") %>%
  fxn_aov_me(index_time = "survey")

#   Interaction between timing x fuel type ----
fuels_by_plot_total %>%
  fxn_aov2_me(index_time = "timing", 
              index_variable = "data_type")

#   Pairwise comparisons ----
fuels_by_plot_total %>%
  fxn_pwc2(index_time = "timing", index_variable= "data_type")

# Plot-level mean by fuel class ----
fuels_by_plot_mean %>%
  fxn_aov2_me(index_time = "timing", 
              index_variable = "fuel_class")


# ========================================================== -----

