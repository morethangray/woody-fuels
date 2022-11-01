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
index_dl <- "Duff & litter"

# Define plot colors 
colors_thin_bright <- c("#9d9596", "#069879")
colors_thin_faded <- c("#bfbabb", "#75bca8")

# Read and subset thin data  
input_dl <- 
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
  filter(data_type %in% "dl")

# ========================================================== -----
# DUFF & LITTER (DL)  ----
# Check assumptions ----
#   Outliers ----
input_dl %>%
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
# No extreme outliers for total duff and litter 
# Two extreme outliers for litter in post-thinning subset 

#   Normality ----
input_dl %>%
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
