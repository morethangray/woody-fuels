# revised: 2022-10-28 ----
# About this data set ----
# we now have additional fuels data for forests that were burned in Tubbs and recently thinned
# do a quick ANOVA on those data for this conference - really focusing it on fuel load management
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
library(broom)  ## To format output tables from statistical tests
library(ggpubr)  ## For preset plot templates
library(patchwork)  ## To arrange multiple plots 
# library(lemon)  ## To manipulate faceted ggplots

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_3-derived")
path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "statistical-tests.R"))
source(file = here(path_fxn, "data-transformation.R"))
# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers ----
lookup_units <- 
  tibble(data_type = c("dl", "wd"),
         units = c("Depth in centimeters",
                   "Metric tons per hectare"))

# # To iterate barbell plots by fuel class 
# list_lab_fuel <- unique(thin$lab_fuel)

# # Define plot colors 
colors_trmt_bright <- c("#9d9596", "#069879")
colors_trmt_faded <- c("#bfbabb", "#75bca8")
color_palette <- thematic::okabe_ito(2)

# Create thin data  ----
thin <-
  read_csv(here(path_derived, "thin_total-by-plot-type-trmt.csv")) %>%
  bind_rows(read_csv(here(path_derived, "thin_mean-by-plot-type-class-trmt.csv"))) %>%
  arrange(survey, plot_id, data_type, fuel_class) %>%
  mutate_if(is.character, as_factor)  %>%
  mutate(value = fxn_digit(value_si)) %>%
  select(-value_si, 
         -units) %>%
  rename(units = units_si)

# thin_dl <- 
#   thin %>%
#   filter(data_type %in% "dl")  
# 
# thin_wd <- 
#   thin %>%
#   filter(data_type %in% "wd")  

# ========================================================== -----
# EXPLORE DATA ----
# Create summary table ----
# Groups: data_type, fuel_class
thin %>%
  group_by(survey, data_type, fuel_class, statistic, units) %>%
  get_summary_stats(value, type = "mean") %>%
  spread(survey, mean)  
# Check assumptions ----

#   Check for outliers ----
input_total %>%
  group_by(timing) %>%
  identify_outliers(value)  %>%
  mutate(data_type = index_type,
         fuel_class = "All") %>%
  clean_names() %>%
  select(data_type, fuel_class, timing, plot_id, value, 
         starts_with("is"))

#   Check for normality ----

# Plot-level values for coarse woody debris were not normally distributed for three of the four time points (2016, 2017, 2019), as assessed by Shapiro-Wilk's test.
nput_total %>%
  select(id = plot_id, 
         time = timing, 
         score = value) %>%
  group_by(time) %>%
  shapiro_test(score)  %>%
    clean_names() %>%
  mutate(data_type = index_type,
         fuel_class = "All", 
         statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(data_type, 
         fuel_class, 
         timing = time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, timing) %>%
      filter(is_normal == FALSE) %>%
     fxn_kable()

ggqqplot(input_total, "value", 
         facet.by = "timing", 
         color = "timing",
         palette = colors_timing)
#   Evaluate transformations  ----
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
  gather(statistic, number, mean:sd) %>%
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


#   Normalize data and repeat checks ----

# We applied an arcsine transformation to normalize the plot-level values for total coarse woody debris, calculated as log(x + sqrt(x\^2 + 1)). The arcsine transformation (also called the arcsine square root transformation, or the angular transformation) was identified as the most suitable method using the R function bestNormalize() [bestNormalize package]. Values were standardized upon normalization to have a mean of 0 and standard deviation of 1.
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
         statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(data_type, 
         fuel_class, 
         timing = time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, timing)  
  # filter(is_normal == FALSE)  

ggqqplot(input_total_transform, "value", 
         facet.by = "timing", 
         color = "timing",
         palette = colors_timing)

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
         statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(data_type, 
         fuel_class = treatment, 
         timing = time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, fuel_class, timing) %>%
  filter(is_normal == FALSE) 

ggqqplot(input_class_transform, 
         "value", 
         facet.by = "fuel_class", 
         color = "timing",
         palette = colors_timing) +
  labs(caption = "Values have been normalized and standardized by fuel class")

# ========================================================== -----
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
           statistic, 
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
  mutate(statistic = fxn_digit(statistic),
         data_type = index_type,
         fuel_class = "All") %>%
  select(-p) %>%
  relocate(data_type, 
           fuel_class,
           starts_with("group"), 
           starts_with("p_adj"), 
           statistic, 
           starts_with("d")) %>%
  arrange(p_adj)  

# Two-way ANOVA: Interaction between timing x fuel class  ----
input_class_transform  %>%
  fxn_aov2_me() %>%
  fxn_signif() %>%
  relocate(data_type, 
           effect, 
           starts_with("p_adj"), 
           statistic, 
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

#   Duff vs. timing ----
fxn_posthoc_plot(index_subset = "duff", input_class) +
  theme(legend.position = "right")
fxn_posthoc_table(index_subset = "duff", input_class)

#   Litter vs. timing ----
fxn_posthoc_plot(index_subset = "litter", input_class) +
  theme(legend.position = "right")

fxn_posthoc_table(index_subset = "litter", input_class)
#   Duff vs.  timing ----

# Create annotations to visualize p-values on boxplot  
pwc_class <- 
  input_class %>%
  group_by(timing) %>%
  pairwise_t_test(value ~ fuel_class, paired = TRUE, p.adjust.method = "bonferroni") %>%
  add_xy_position(x = "timing")

# Create boxplot comparing fuel class by timing  
input_class %>%
  ggboxplot(x = "lab_thin", 
            y = "value", 
            fill = "lab_fuel", 
            palette = colors_dl, 
            width = 0.6) +
  scale_x_discrete(labels = function(lab_thin) str_wrap(lab_thin, width = 20)) + 
  labs(title = paste0(index_type), 
       y = paste0(index_units_si, " (mean)"),
       fill = "Fuel class") +
  theme(axis.title.x = element_blank(), 
        legend.position = "right")  + 
  font("x.text", size = 10)  +
  stat_pvalue_manual(pwc_class, tip.length = 0, hide.ns = TRUE) 

# Create summary table   
pwc_class %>%
  mutate(data_type = index_type,
         statistic = fxn_digit(statistic), 
         method = "pwc") %>%
  clean_names() %>%
  select(data_type, 
         timing,
         group1, group2,
         starts_with("p_adj"), 
         statistic, 
         starts_with("d")) 

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

# GRAVEYARD ----
# thin_by_plot <- 
#   read_csv(here(path_in, "thin_for-aov_2022.csv")) %>%
#   mutate(value = fxn_digit(value))  
# 
# thin_by_plot_diff <- 
#   read_csv(here(path_in, "thin_for-aov_diff_2022.csv"))
#
# Raw data (by transect) 
# fuels_by_transect <- read_csv(here(path_in, "fuel_for-aov_raw-data_2022.csv"))
# ========================================================== -----
# DIFFERENCE BETWEEN CONTROL VS. TREATMENT ----
# Create data frames ----
thin_diff <- 
  read_csv(here(path_in, "fuel_thin_diff_mean-by-plot-type-class_2022.csv")) %>%
  mutate(mean = fxn_digit(mean)) %>%
  arrange(plot_id, data_type, fuel_class) %>%
  # Convert variable labels to factors for ANOVA
  mutate_if(is.character, as_factor)  

thin_plot_total_diff <- 
  read_csv(here(path_in, "fuel_thin_diff_total-by-plot-type_2022.csv")) %>%
  mutate(total = fxn_digit(total)) %>%
  arrange(plot_id, data_type) %>%
  # Convert variable labels to factors for ANOVA
  mutate_if(is.character, as_factor)  

# Summarize total by fuel type  -----

thin_plot_total_diff %>%
  group_by(data_type) %>%
  get_summary_stats(total, type = "mean_sd")  

# Summarize mean by fuel class -----

summary_diff_type_class_plot <- 
  thin_diff %>%
  group_by(data_type, fuel_class, plot_id) %>%
  get_summary_stats(mean, type = "mean") %>%
  spread(plot_id, mean)

summary_diff_type_class_plot %>%
  arrange(data_type, fuel_class) %>%
  mutate(fuel_class = as_factor(fuel_class)) %>%
  gather(plot_id, mean, FOR05:FOR10) %>%
  mutate(plot_n = as.numeric(str_sub(plot_id, 4, 5)), 
         plot_n = as_factor(plot_n)) %>%
  ggbarplot(x = "plot_n", 
            y = "mean", 
            fill = "fuel_class",
            # palette = colors_fuel,
            xlab = "Plot ID", 
            ylab = "Treatment - Control") + 
  # geom_col() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~fuel_class, scales = "free", ncol = 5) +
  theme(legend.position = "none") 

# # Visualize data: Dumbbell plot  ---- 
# # Write a function to do this because I want the y-axis to sort each fuel class by the maximum treatment value
# # I can't figure out how to facet the plots such that each y-axis differs (maybe switch x and y axes?)
# # Instead it was quicker to write a function that creates each ordered barbell plot, then combine with patchwork
# 
# # index_data <- thin
# # index_class <- list_lab_fuel[1]
# fxn_plot_bars <- function(index_data, index_class, with_legend){
#   
#   subset <- 
#     index_data %>%
#     filter(lab_fuel %in% index_class)
#   
#   # text_title <- paste0("Plot-level mean fuel load (", index_class, ")")
#   
#   segment_helper <- 
#     subset |>
#     select(plot_id, fuel_class, lab_fuel, survey, mean) |>
#     spread(survey, mean) |>
#     mutate(change = trmt - cont)  |>
#     arrange(fuel_class, trmt) %>%
#     mutate(plot_id_f = as_factor(as.character(plot_id)))  
#   
#   barbell_plot <- 
#   ggplot() +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     geom_segment(
#       data = segment_helper,
#       aes(x = cont, xend = trmt, y = plot_id_f, yend = plot_id_f),
#       col = 'grey60',
#       size = 1.25) +
#     geom_point(
#       data = subset, 
#       aes(x = mean, y = plot_id, color = lab_thin), 
#       size = 4) +
#     scale_color_manual(values = color_palette) +
#     theme(
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor.x = element_blank(), 
#       panel.grid.major.x = element_line(color = "gray90"),
#       panel.background =  element_rect(fill = 'white', colour = 'black'),
#       legend.position = "right",
#       panel.spacing = unit(1, "lines")) + 
#     labs(x = 'Fuel load',
#          y = element_blank(),
#          title = index_class)
#   
#   if(with_legend == TRUE){
#     barbell_plot
#   }else{
#     barbell_plot +
#       theme(legend.position = "none")
#   }
#   
#   
#   
# }
# 
# p1 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[1], with_legend = FALSE)
# p2 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[2], with_legend = FALSE)
# p3 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[3], with_legend = FALSE)
# p4 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[4], with_legend = FALSE)
# p5 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[5], with_legend = FALSE)
# p6 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[6], with_legend = FALSE)
# p7 <- fxn_plot_bars(index_data = thin, index_class = list_lab_fuel[7], with_legend = TRUE)
# 
# # Group plots using patchwork
# # 
# (p1 | p2 | p3 | p4 | p5) /
#   (p6 | p7 | plot_spacer() | plot_spacer() | plot_spacer()) 
# 

# ========================================================== -----