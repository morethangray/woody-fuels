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
tubbs_norm <- 
  read_csv(here(path_derived, "tubbs_derived-norm.csv")) %>%
  arrange(time) %>%
  # Convert time to factor for ANOVA
  mutate(time = as_factor(time)) %>%
  mutate_if(is.character, as_factor) %>%
  select(fuel_type, 
         metric, 
         value_norm, 
         plot_id, 
         time, 
         fuel_class)

# ========================================================== -----
# DUFF & LITTER (DL) ----
# Subset data  ----
dl_norm_total <- 
  tubbs_norm %>%
  filter(fuel_type %in% "dl", 
         metric %in% "total") 

dl_norm_class <- 
  tubbs_norm %>%
  filter(fuel_type %in% "dl", 
         metric %in% "mean")

# Total fuels ----
#   One-way ANOVA  ----
# Effect of time (duff and litter combined)
tubbs_dl_total_me <- 
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
         index_plot = "tubbs", 
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
# Significant effect of year on total fuels (p-adj < 0.001)

#   Pairwise comparison  ----
# Effect of time (duff and litter combined)
tubbs_dl_total_pwc <- 
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
         index_plot = "tubbs", 
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
# Significant difference in total fuels between 2016 and 2017 (p-adj <0.001); 2016 and 2021 (p-adj <0.01)

# Mean by fuel class ----
#   Two-way ANOVA: Interaction between time x fuel class  ----
tubbs_dl_class_interaction <- 
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
         index_plot = "tubbs", 
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
# Significant interaction between time and fuel class (p-adj <0.001)
#   Main effect: time on fuel class ----
tubbs_dl_class_me <- 
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
         index_plot = "tubbs", 
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

# Significant effect of year on fuel class for duff and litter (p-adj <0.001)

#   Two-way post hoc tests: time by fuel class ----
tubbs_dl_class_pwc <- 
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
         index_plot = "tubbs", 
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

# Significant pairwise difference between years
#   Duff: 2016 vs. 2017, 2019, 2021 (p-adj <0.001); 2017 vs. 2019, 2021 (p-adj <0.01)
#   Litter:2017 vs. 2016, 2019  (p-adj <0.01)

# ========================================================== -----
# COARSE WOODY DEBRIS (WD) ----
# Subset data  ----
wd_norm_total <- 
  tubbs_norm %>%
  filter(fuel_type %in% "wd", 
         metric %in% "total") 

wd_norm_class <- 
  tubbs_norm %>%
  filter(fuel_type %in% "wd", 
         metric %in% "mean")

# Total fuels ----
#   One-way ANOVA  ----
# Effect of time (duff and litter combined)
tubbs_wd_total_me <- 
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
         index_plot = "tubbs", 
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
# No significant effect of year on total fuels (p-adj = 0.5)

#   Pairwise comparison  ----
# Effect of time (duff and litter combined)
tubbs_wd_total_pwc <- 
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
         index_plot = "tubbs", 
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


# Mean by fuel class  ----
#   Two-way ANOVA: Interaction between time x fuel class  ----
tubbs_wd_class_interaction <- 
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
         index_plot = "tubbs", 
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
# Significant interaction between time and fuel class (p-adj <0.001)
#   Main effect: time on fuel class ----
tubbs_wd_class_me <- 
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
         index_plot = "tubbs", 
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

# Significant effect of year on fuel class for duff and litter (p-adj <0.001)

#   Two-way post hoc tests ----
tubbs_wd_class_pwc <- 
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
         index_plot = "tubbs", 
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

# Significant pairwise difference between years
#   1-hr: 2016 vs. 2021 (p-adj <0.001); 2017 vs. 2016, 2019 (p-adj <0.01); 2019 vs. 2021 (p-adj <0.05)
#   10-hr:  2016 vs. 2017, 2021 (p-adj <0.001); 2016 vs. 2019 (p-adj <0.01)

# ========================================================== -----

# BIND AND WRITE TO CSV ----
tubbs_summary <- 
  bind_rows(tubbs_dl_total_me, 
          tubbs_dl_total_pwc, 
          tubbs_dl_class_interaction, 
          tubbs_dl_class_me,
          tubbs_dl_class_pwc, 
          tubbs_wd_total_me, 
          tubbs_wd_total_pwc, 
          tubbs_wd_class_interaction, 
          tubbs_wd_class_me,
          tubbs_wd_class_pwc) %>%
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

tubbs_summary %>%
  write_csv(here(path_out, "tubbs_statistical-tests.csv"), 
            na = "")
