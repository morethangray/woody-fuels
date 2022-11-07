# revised: 2022-11-07 ----
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
path_out <- here("output/summary-tables")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
# source(file = here(path_fxn, "statistical-tests.R"))
# ========================================================== -----
# CREATE DATA FRAMES ----
# Create helpers 
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

thin_norm <- 
  read_csv(here(path_derived, "thin_derived-norm.csv")) %>%
  arrange(time) %>%
  # Convert time to factor for ANOVA
  mutate(time = as_factor(time)) %>%
  mutate_if(is.character, as_factor) %>%
  select(fuel_type, 
         metric, 
         value_norm, 
         plot_id, 
         time = lab_time_abbr, 
         fuel_class)

# ========================================================== -----
# DUFF & LITTER (DL) ----
# Subset data  ----
dl_norm_total <- 
  thin_norm %>%
  filter(fuel_type %in% "dl", 
         metric %in% "total") 

dl_norm_class <- 
  thin_norm %>%
  filter(fuel_type %in% "dl", 
         metric %in% "mean")

# Total fuels ----
#   One-way ANOVA  ----
# Effect of time (duff and litter combined)
thin_dl_total_me <- 
  dl_norm_total %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "me",
         fuel_type = index_dl,
         fuel_class = "All",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "total") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj)
# No significant effect of year on total fuels (p-adj = 0.065)

#   Pairwise comparison  ----
# Effect of time (duff and litter combined)
thin_dl_total_pwc <- 
  dl_norm_total %>%
  pairwise_t_test(
    value_norm ~ time, 
    paired = TRUE,
    p.adjust.method = "bonferroni") %>%
  clean_names() %>%
  mutate(method = "pwc", 
         statistic = fxn_digit(statistic), 
         fuel_type = index_dl,
         fuel_class = "All",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "total") %>%
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         starts_with("group"),
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         starts_with("n"),
         starts_with("index"), 
         - p_adj_signif) %>%
  arrange(p_adj)
# No significant difference in total fuels between pre-thin and post-thin (p-adj = 0.065)
# Mean by fuel class ----
#   Two-way ANOVA: Interaction between time x fuel class  ----
thin_dl_class_interaction <- 
  dl_norm_class %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = c(time, fuel_class)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "interaction",
         fuel_type = index_dl,
         fuel_class = "By class",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj()  %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj) %>%
  filter(effect %in% "time:fuel_class") 
# Significant interaction between time and fuel class (p-adj <0.05)

#   Main effect: time on fuel class ----
thin_dl_class_me <- 
  dl_norm_class %>%
  group_by(fuel_class) %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "me",
         fuel_type = index_dl,
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj)
# Significant effect of year on fuel class for litter (p-adj <0.05); n.s. for duff

#   Two-way post hoc tests: time by fuel class ----
thin_dl_class_pwc <- 
  dl_norm_class %>%
  group_by(fuel_class) %>%
  pairwise_t_test(
    value_norm ~ time, 
    paired = TRUE,
    p.adjust.method = "bonferroni") %>%
  clean_names() %>%
  mutate(method = "pwc2", 
         statistic = fxn_digit(statistic), 
         fuel_type = index_dl,
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         starts_with("group"),
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         starts_with("n"),
         starts_with("index"), 
         - p_adj_signif) %>%
  arrange(p_adj)

# Significant pairwise differences between pre-thin and post-thin:
#   Duff (n.s.)
#   Litter: p-adj <0.01
# Significant difference in litter between pre- and post-thinning

# ========================================================== -----

# COARSE WOODY DEBRIS (WD) ----
# Subset data  ----
wd_norm_total <- 
  thin_norm %>%
  filter(fuel_type %in% "wd", 
         metric %in% "total") 

wd_norm_class <- 
  thin_norm %>%
  filter(fuel_type %in% "wd", 
         metric %in% "mean")

# Total fuels ----
#   One-way ANOVA  ----
# Effect of time (duff and litter combined)
thin_wd_total_me <- 
  wd_norm_total %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "me",
         fuel_type = index_wd,
         fuel_class = "All",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "total") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj)
# No significant effect of year on total fuels (p-adj = 0.11)

#   Pairwise comparison  ----
# Effect of time (duff and litter combined)
thin_wd_total_pwc <- 
  wd_norm_total %>%
  pairwise_t_test(
    value_norm ~ time, 
    paired = TRUE,
    p.adjust.method = "bonferroni") %>%
  clean_names() %>%
  mutate(method = "pwc", 
         statistic = fxn_digit(statistic), 
         fuel_type = index_wd,
         fuel_class = "All",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "total") %>%
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         starts_with("group"),
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         starts_with("n"),
         starts_with("index"), 
         - p_adj_signif) %>%
  arrange(p_adj)
# No significant difference in total fuels in pairwise comparison between years 
# We conducted post hoc pairwise comparisons between the levels of the within-subjects factor (here, timing). The result of paired t-tests between time showed no significant difference in fuel load between pre-thin and post-thin values at a significance level of \< 0.05. This finding is consistent with the lack of main effect found for time

# Mean by fuel class  ----
#   Two-way ANOVA: Interaction between time x fuel class  ----
thin_wd_class_interaction <- 
  wd_norm_class %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = c(time, fuel_class)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "interaction",
         fuel_type = index_wd,
         fuel_class = "By class",
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj()  %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj) %>%
  filter(effect %in% "time:fuel_class") 
# No significant interaction between time and fuel class (p-adj = 0.249)
#   Main effect: time on fuel class ----
thin_wd_class_me <- 
  wd_norm_class %>%
  group_by(fuel_class) %>%
  anova_test(dv = value_norm, 
             wid = plot_id, 
             within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(method = "me",
         fuel_type = index_wd,
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  rename(statistic = f) %>% 
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index")) %>%
  arrange(p_adj)

# Significant effect of time on fuel class for 1000-hr sound (p-adj <0.01); 10-hr (p-adj <0.05)

#   Two-way post hoc tests ----
thin_wd_class_pwc <- 
  wd_norm_class %>%
  group_by(fuel_class) %>%
  pairwise_t_test(
    value_norm ~ time, 
    paired = TRUE,
    p.adjust.method = "bonferroni") %>%
  clean_names() %>%
  mutate(method = "pwc2", 
         statistic = fxn_digit(statistic), 
         fuel_type = index_wd,
         index_value = "value_norm", 
         index_plot = "thin", 
         index_metric = "mean") %>%
  fxn_signif_adj() %>%
  select(fuel_type, 
         fuel_class, 
         method, 
         starts_with("group"),
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         starts_with("n"),
         starts_with("index"), 
         - p_adj_signif) %>%
  arrange(p_adj)


# Significant pairwise difference between pre-thin and post-thin values
#   10-hr (p-adj <0.01)
#   1000-hr sound (p-adj <0.001)

# ========================================================== -----

# BIND AND WRITE TO CSV ----
thin_summary <- 
  bind_rows(thin_dl_total_me, 
            thin_dl_total_pwc, 
            thin_dl_class_interaction, 
            thin_dl_class_me,
            thin_dl_class_pwc, 
            thin_wd_total_me, 
            thin_wd_total_pwc, 
            thin_wd_class_interaction, 
            thin_wd_class_me,
            thin_wd_class_pwc) %>%
  # Combine degrees of freedom columns
  unite(df_nd, c(d_fn, d_fd), sep = ",", na.rm = TRUE) %>%
  mutate(df = ifelse(is.na(df), df_nd, df)) %>%
  rename(n = n1) %>%
  select(-df_nd, -n2) %>%
  relocate(fuel_type, 
           fuel_class, 
           method, 
           starts_with("group"), 
           effect, 
           starts_with("p_adj"), 
           df,
           statistic,
           ges, 
           n, 
           starts_with("index"))

thin_summary %>%
  write_csv(here(path_out, "thin_statistical-tests.csv"), 
            na = "")
# ========================================================== -----
# [NOT RUN] -----
# # Plot-level total by fuel type (dw, gf) 
# # mean total fuel load ANOVA results and plot.
# #   Main effect 
# fuels_by_plot_total %>%
#   filter(data_type %in% "dw") %>%
#   fxn_aov_me(index_time = "survey")
# 
# fuels_by_plot_total %>%
#   filter(data_type %in% "gf") %>%
#   fxn_aov_me(index_time = "survey")
# 
# #   Interaction between timing x fuel type 
# fuels_by_plot_total %>%
#   fxn_aov2_me(index_time = "timing", 
#               index_variable = "data_type")
# 
# #   Pairwise comparisons 
# fuels_by_plot_total %>%
#   fxn_pwc2(index_time = "timing", index_variable= "data_type")
# 
# # Plot-level mean by fuel class 
# fuels_by_plot_mean %>%
#   fxn_aov2_me(index_time = "timing", 
#               index_variable = "fuel_class")
# 
# # Notes about input data 
# # hr1000r: 29 of the 30 values were 0 
# #
# # Nine extreme outliers for CWD: 7/9 for hr1000s; the other two at FOR08
# # data_type fuel_class timing  plot_id value is_outlier is_extreme
# # 1 wd        all        survey1 FOR08   147.  TRUE       FALSE     
# # 2 wd        hr1000s    survey1 FOR05    29.0 TRUE       TRUE      
# # 3 wd        hr1000s    survey1 FOR07    45.3 TRUE       TRUE      
# # 4 wd        hr0100     survey1 FOR08   110.  TRUE       TRUE      
# # 5 wd        hr1000r    survey1 FOR08    34.8 TRUE       TRUE      
# # 6 wd        hr1000s    survey2 FOR05   327.  TRUE       TRUE      
# # 7 wd        hr1000s    survey2 FOR06    76.9 TRUE       TRUE      
# # 8 wd        hr1000s    survey2 FOR07   373.  TRUE       TRUE      
# # 9 wd        hr1000s    survey2 FOR08    61.4 TRUE       TRUE      
# #10 wd        hr1000s    survey2 FOR10    41.5 TRUE       TRUE      
# #
# # Data for total is normally distributed 
# #
# # Shapiro test results indicate two fuel classes are not normally distributed: 100-hr, 1000-hr sound
# # Identify appropriate transformation  
# # List the transformations to use by fuel class
# # list_transform <- c("log_x", "log_x", "log_x", "arcsinh_x", "arcsinh_x")
# 
# # 1-hr: ordernorm
# # 10-hr: ordernorm, sqrt
# # 100-hr: nothing looks great, maybe ordernorm?
# # 1000-hr rotten: nothing looks great, maybe sqrt? maybe ordernorm?
# # 1000-hr sound: raw values look good, then ordernorm  
# 
# # # Apply functions to normalize and standardize the data by fuel class 
# # bn_hr_0001 <- fxn_transform_log(index_class = "hr0001")
# # bn_hr_0010 <- fxn_transform_log(index_class = "hr0010")
# # bn_hr_0100 <- fxn_transform_log(index_class = "hr0100")
# # bn_hr_1000r <- fxn_transform_arcsine(index_class = "hr1000r")
# # bn_hr_1000s <- fxn_transform_arcsine(index_class = "hr1000s")
# 
# # Combine subsets 
# input_class_transform <- fxn_tranform_ordnorm() %>%
#   rename(value = value_ordnorm)
# 
# input_class_transform %>%
#   select(fuel_class, value_raw, value) %>% 
#   gather(metric, value, value_raw:value) %>%
#   group_by(fuel_class, metric) %>%
#   summarize(mean = fxn_digit(mean(value, na.rm = TRUE)), 
#             sd = fxn_digit(sd(value, na.rm = TRUE))) %>%
#   ungroup() %>%
#   gather(metric, number, mean:sd) %>%
#   spread(fuel_class, number) 
# 
# # The following plot shows the raw (untransformed) values on the left and the transformed values on the right.
# nput_class_transform %>%
#   rename(raw_values = value_raw, transformed_values = value) %>%
#   
#   gather(metric, value, raw_values:transformed_values) %>%
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
# 
# # Standardize data and repeat checks 
# # We applied an arcsine transformation to normalize the plot-level values for total coarse woody debris, calculated as log(x + sqrt(x\^2 + 1)). The arcsine transformation (also called the arcsine square root transformation, or the angular transformation) was identified as the most suitable method using the R function bestNormalize() [bestNormalize package]. Plot-level values for total and for mean duff and litter were standardized to have a mean of 0 and standard deviation of 1.
# # 
# # The transformed values for total coarse woody debris were normally distributed at each time point (p \> 0.05), as assessed by Shapiro-Wilk's test.
# 
# bn_total <- bestNormalize::arcsinh_x(input_total$value, standardize = TRUE)
# 
# input_total_transform <- 
#   input_total %>%
#   mutate(value = bn_total$x.t) 
# 
# input_total_transform %>%
#   select(id = plot_id, 
#          time = timing, 
#          score = value) %>%
#   group_by(time) %>%
#   shapiro_test(score)  %>%
#   clean_names() %>%
#   mutate(data_type = index_type,
#          fuel_class = "All", 
#          metric = fxn_digit(metric), 
#          is_normal = p>0.05) %>%
#   select(data_type, 
#          fuel_class, 
#          timing = time, 
#          is_normal,
#          p, 
#          metric) %>%
#   arrange(is_normal, timing)  
# # filter(is_normal == FALSE)  
# 
# ggqqplot(input_total_transform, "value", 
#          facet.by = "timing", 
#          color = "timing",
#          palette = colors_thin)
# 
# # The two outliers (not extreme) detected in the untransformed data were absent after the transformation was applied.
# input_total_transform %>%
#   group_by(timing) %>%
#   identify_outliers(value)  %>%
#   mutate(data_type = index_type,
#          fuel_class = "All") %>%
#   clean_names() %>%
#   select(data_type, fuel_class, timing, plot_id, value, 
#          starts_with("is"))  
# # filter(is_extreme == TRUE)  
# 
# norm_class_transform <- 
#   input_class_transform %>%
#   filter(fuel_class %nin% "hr1000s") %>%
#   select(id = plot_id, 
#          time = timing, 
#          score = value, 
#          treatment = fuel_class) %>%
#   group_by(time, treatment) %>%
#   shapiro_test(score)  %>%
#   clean_names() %>%
#   mutate(data_type = index_type,
#          metric = fxn_digit(metric), 
#          is_normal = p>0.05) %>%
#   select(data_type, 
#          fuel_class = treatment, 
#          timing = time, 
#          is_normal,
#          p, 
#          metric) %>%
#   arrange(is_normal, fuel_class, timing) %>%
#   filter(is_normal == FALSE) 
# 
# ggqqplot(input_class_transform, 
#          "value", 
#          facet.by = "fuel_class", 
#          color = "timing",
#          palette = colors_thin) +
#   labs(caption = "Values have been normalized and standardized by fuel class")

