# updated: 2022-09-22 ----
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For statistical tests 
# library(broom)  ## To format output tables from statistical tests
# library(lemon)  ## To manipulate faceted ggplots
library(ggpubr)  ## For plots

# Define file paths 
path_in <- here("input")
path_out <- here("output/2022/oak-symposium")
path_out_plot <- here(path_out, "plots")

# Define helpers 
date_today <- Sys.Date()
index_type <- "Downed woody debris"

# lookup_dwd <- read_csv(here(path_in, "lookup_variables.csv")) %>%
#   filter(data_type %in% "dw") %>%
#   select(fuel_class = name, 
#          lab_fuel = label)


# ========================================================== -----
# WRITE FUNCTIONS ----
# Basic functions ----

"%nin%" <- Negate("%in%")

fxn_digit <- function(x){
  as.numeric(format(round(x, 3), nsmall = 3))
}

fxn_signif <- function(df){
  df %>%
    mutate(p_adj_sig = case_when(
      p_adj < 0.001 ~ "***", 
      p_adj > 0.001 & p_adj < 0.01 ~ "**", 
      p_adj > 0.01 & p_adj < 0.05 ~ "*", 
      p_adj > 0.05 ~ "n.s.")) %>%
    arrange(p_adj)
  
}

# Statistical tests ----
#   fxn_aov_me ----
fxn_aov_me <- function(df){
  df %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = year) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "me", 
           data_type = index_type) %>%
    rename(statistic = f) %>% 
    relocate(data_type, 
             method, 
             effect, 
             starts_with("p_adj"), 
             statistic,
             starts_with("d"), 
             ges)
}

#   fxn_pwc ----
# Excludes fuel_class, is for pwc of fuel_type only
fxn_pwc <- function(df){
  df %>%
    pairwise_t_test(
      value ~ year, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           data_type = index_type, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(data_type, 
           method, 
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"),  
           starts_with("p"))
}

#   fxn_aov2_me ----
fxn_aov2_me <- function(df){
  df %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = c(fuel_class, year)) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "aov2", 
           data_type = index_type)  %>%
    rename(statistic = f) %>%
    select(data_type, 
           method, 
           effect, 
           starts_with("p_adj"), 
           statistic,
           starts_with("d"), 
           ges)
}

#   fxn_pwc2 ----
# Includes a column for fuel_class
fxn_pwc2 <- function(df){
  df %>%
    pairwise_t_test(
      value ~ year, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           data_type = index_type, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(data_type, 
           method, 
           fuel_class,
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"))
  
  
}
# Summary plots and tables ----
#   fxn_posthoc_table ----
fxn_posthoc_table <- function(index_subset, index_input){
  
  title <- 
    index_input %>%
    filter(fuel_class %in% index_subset) %>%
    distinct(lab_fuel) %>%
    pull()
  
  index_input %>%
    filter(fuel_class %in% index_subset) %>%
    fxn_pwc() %>%
    mutate(fuel_class = title) %>%
    relocate(data_type, 
             fuel_class, 
             starts_with("group"), 
             starts_with("p_adj"), 
             statistic, 
             starts_with("d")) %>%
    select(-p) %>%
    arrange(p_adj) %>%
    fxn_kable()
}

# ========================================================== -----
# Create data frames ----
input_class <- 
  read_csv(here(path_in, "fuel_mean-by-plot-yr-type-class_2022.csv")) %>%
  arrange(year) %>%
  mutate(year = as_factor(year), 
         lab_year = as_factor(lab_year), 
         lab_year_abbr = as_factor(lab_year_abbr), 
         value = fxn_digit(value)) %>%
  relocate(c(statistic, subset), .after = value)  %>%
  filter(lab_type %in% index_type)

list_classes <- unique(input_class$fuel_class)

# Assess normality with Shapiro test ----
input_class %>%
  filter(fuel_class %nin% "hr1000s") %>%
  select(id = plot_id, 
         time = year, 
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
         year = time, 
         is_normal,
         p, 
         statistic) %>%
  arrange(is_normal, fuel_class, year) %>%
  filter(is_normal == FALSE)

# data_type           fuel_class year  is_normal         p statistic
# 1 Downed woody debris hr0010     2017  FALSE     0.00592       0.753
# 2 Downed woody debris hr0100     2017  FALSE     0.0000258     0.551
# 3 Downed woody debris hr0100     2019  FALSE     0.000166      0.62 
# 4 Downed woody debris hr0100     2021  FALSE     0.0000215     0.545
# 5 Downed woody debris hr1000r    2016  FALSE     0.00362       0.735
# 6 Downed woody debris hr1000r    2017  FALSE     0.00661       0.757
# 7 Downed woody debris hr1000r    2019  FALSE     0.000851      0.681
# 8 Downed woody debris hr1000r    2021  FALSE     0.0320        0.817
# 
# one 10-hr, three 100-hr, four 1000-hr rotten

# Plot raw data distribution ----
# input_class %>%
#   mutate(fuel_class = lab_fuel) %>%
#   ggdensity(x = "value") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   facet_wrap(~fuel_class, scales = "free") +
#   theme(panel.spacing = unit(2, "lines")) + 
#   labs(title = "Distribution of raw values, by fuel class", 
#        caption = "Note: axis scales differ between plots", 
#        x = "Mean tons per acre",  
#        y = "Density") 

# Data transformation (normalize and standardize) ----
#   Write function: fxn_transform ----
# index_class <- list_classes[5]
# list_transform <- c("log_x", "log_x", "log_x", "arcsinh_x", "arcsinh_x")
# index_transformation: "arcsinh_x", "boxcox", "exp_x", "log_x",  "orderNorm", "sqrt_x"
fxn_tranform <- function(){
  
  datalist_normalized <- list()
  for(index_class in list_classes){
    
    subset <- 
      input_class %>%
      filter(fuel_class %in% index_class) %>%
      rename(value_raw = value) 
    
    bn_a <- bestNormalize::arcsinh_x(subset$value_raw, standardize = TRUE)
    bn_e <- bestNormalize::exp_x(subset$value_raw, standardize = TRUE)
    bn_l <- bestNormalize::log_x(subset$value_raw, standardize = TRUE)
    bn_o <- bestNormalize::orderNorm(subset$value_raw, standardize = TRUE)
    bn_s <- bestNormalize::sqrt_x(subset$value_raw, standardize = TRUE)
   
    datalist_normalized[[index_class]] <- 
        subset %>%
        mutate(value_a = bn_a$x.t, 
               value_e = bn_e$x.t, 
               value_l = bn_l$x.t, 
               value_o = bn_o$x.t, 
               value_s = bn_s$x.t) %>%
        select(fuel_class, year, plot_id, starts_with("value")) 

      
    }
     
  bind_norm <- do.call(bind_rows, datalist_normalized)

}

#   Conduct boxcox transformation for 1-hr ----
# No other fuel class is eligible for this transformation
hr0001 <- 
  input_class %>%
  rename(value_raw = value) %>%
  filter(fuel_class == "hr0001")

hr0001_bn_b <- bestNormalize::boxcox(hr0001$value_raw, standardize = TRUE)
hr0001$value_b <- hr0001_bn_b$x.t

hr0001_b <- 
  hr0001 %>%
  mutate(value_b =  hr0001_bn_b$x.t) %>%
  unite(class_yr_plot, c(fuel_class, year, plot_id)) %>%
  select(class_yr_plot, value_b)

#   Conduct transformations for all fuel classes ----
class_transform <- 
  fxn_tranform()  %>%
  unite(class_yr_plot, c(fuel_class, year, plot_id), remove = F) %>%
  left_join(hr0001_b, "class_yr_plot") %>%
  select(-class_yr_plot) %>%
  relocate(starts_with("value"), .after = "plot_id") %>%
  gather(metric, value, value_raw:value_b)  %>%
  drop_na(value)

norm_class_transform <- 
  class_transform %>%
  filter(fuel_class %nin% "hr1000s") %>%
  select(id = plot_id, 
         time = year, 
         score = value, 
         treatment = fuel_class, 
         metric) %>%
  group_by(time, treatment, metric) %>%
  shapiro_test(score)  %>%
  clean_names() %>%
  mutate(data_type = index_type,
         statistic = fxn_digit(statistic), 
         is_normal = p>0.05) %>%
  select(fuel_class = treatment, 
         year = time, 
         is_normal,
         metric,
         p, 
         statistic) %>%
  arrange(is_normal, metric, fuel_class, year) %>%
  filter(is_normal == FALSE) 

norm_class_transform   %>%
  group_by(metric, fuel_class) %>%
  count() %>%
  ungroup() %>%
  filter(n < 2) %>%
  spread(fuel_class, n)


# metric    hr0001 hr0010 hr1000r
# <chr>      <int>  <int>   <int>
# 1 value_a       NA      1      NA
# 2 value_l        1     NA      NA
# 3 value_o       NA      1       1
# 4 value_raw     NA      1      NA
# 5 value_s       NA     NA       1

# Plot data distribution ----
class_transform %>%
  filter(fuel_class %in% "hr0001") %>%
  ggdensity(x = "value", color = "metric") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free") + 
  xlim(-5, 10)

class_transform %>%
  filter(fuel_class %in% "hr0010") %>%
  ggdensity(x = "value", color = "metric") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free") + 
  xlim(-5, 10)

class_transform %>%
  filter(fuel_class %in% "hr0100") %>%
  ggdensity(x = "value", color = "metric") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free") + 
  xlim(-5, 10)

class_transform %>%
  filter(fuel_class %in% "hr1000r") %>%
  ggdensity(x = "value", color = "metric") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free") + 
  xlim(-5, 10)

class_transform %>%
  filter(fuel_class %in% "hr1000s") %>%
  ggdensity(x = "value", color = "metric") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~metric, scales = "free")  + 
  xlim(-5, 5)

class_transform %>%
  filter(fuel_class %in% "hr1000s") %>%
  group_by(metric) %>%
  summarize(min = min(value), 
            max = max(value))

# 1-hr: ordernorm
# 10-hr: ordernorm, sqrt
# 100-hr: nothing looks great, maybe ordernorm?
# 1000-hr rotten: nothing looks great, maybe sqrt? maybe ordernorm?
# 1000-hr sound: raw values look good, then ordernorm  

class_transform %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(fuel_class~metric, scales = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density")   
  # xlim(-5, 10)

class_transform %>%
  ggdensity(x = "value", color = "fuel_class") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(metric ~ fuel_class, scales = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = "Distribution of values, by fuel class and transformation",
       caption = "Note: axis scales differ between plots",
       x = "Mean tons per acre",
       y = "Density") + 
  xlim(-5, 10)


fxn_tranform_2 <- function(){
  
  datalist_normalized <- list()
  for(index_class in list_classes){
    
    subset <- 
      input_class %>%
      filter(fuel_class %in% index_class) %>%
      rename(value_raw = value) 
    
    bn_a <- bestNormalize::arcsinh_x(subset$value_raw, standardize = TRUE)
    bn_l <- bestNormalize::log_x(subset$value_raw, standardize = TRUE)
    bn_o <- bestNormalize::orderNorm(subset$value_raw, standardize = TRUE)
    bn_s <- bestNormalize::sqrt_x(subset$value_raw, standardize = TRUE)
    
    datalist_normalized[[index_class]] <- 
      subset %>%
      mutate(value_arcsine = bn_a$x.t, 
             value_log = bn_l$x.t, 
             value_ordnorm = bn_o$x.t, 
             value_sqrt = bn_s$x.t) %>%
      select(fuel_class, year, plot_id, starts_with("value")) 
    
  }
  
  bind_norm <- do.call(bind_rows, datalist_normalized)
  
}

# 1-hr: ordernorm
# 10-hr: ordernorm, sqrt
# 100-hr: nothing looks great, maybe ordernorm?
# 1000-hr rotten: nothing looks great, maybe sqrt? maybe ordernorm?
# 1000-hr sound: raw values look good, then ordernorm  

all_transform <- 
  fxn_tranform_2()  %>%
  relocate(starts_with("value"), .after = "plot_id")  

# Apply functions to normalize and standardize the data by fuel class ----
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

# ========================================================== -----

# Configure preferences -----
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from statistical tests
library(kableExtra)  ## For tables in rmarkdown
library(ggpubr)  ## For qq plots
library(bestNormalize)

path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_derived")

source(file = here(path_fxn, "helpers_woody-debris.R"))
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "plot-themes.R"))
# ========================================================== -----
# Read input data: derived total and mean ----
input_wd <-
  read_csv(here(path_derived, "tubbs_derived-norm.csv")) %>%
  arrange(time, fuel_type, fuel_class) %>%
  mutate_if(is.character, as_factor)  %>%
  mutate(time = as_factor(time), 
         si_value = fxn_digit(si_value), 
         units = ifelse(fuel_type %in% "wd", index_wd_si_abbr, index_wd_si_abbr)) %>%
  filter(fuel_type %in% "wd")  

wd_class <-
  input_wd %>%
  filter(metric %in% "mean")

wd_total <-
  input_wd %>%
  filter(metric %in% "total")  

wd_eval <- 
  input_wd %>%
  group_by(lab_class, fuel_class) %>%
  mutate(value_std = as.vector(scale(si_value)), 
         value_arcsine = arcsinh_x(si_value, standardize = TRUE)$x.t, 
         value_log = log_x(si_value, standardize = TRUE)$x.t, 
         value_ordnorm = orderNorm(si_value, standardize = TRUE)$x.t, 
         value_sqrt = sqrt_x(si_value, standardize = TRUE)$x.t) %>%
  ungroup() %>%
  relocate(c(si_value, us_value, value_norm), .after = last_col()) %>%
  gather(method, number, value_std:value_norm) %>%
  select(fuel_class, lab_class, plot_id, time, lab_time_abbr, metric, method, number) 


# Total ----
wd_total %>%
  mutate(time = lab_time_abbr) %>%
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
         index_value = "value_norm") %>% 
  rename(statistic = f) %>% 
  fxn_signif_adj()  %>%
  select(fuel_class, 
         method, 
         effect, 
         starts_with("p_adj"), 
         statistic,
         starts_with("d"), 
         ges, 
         starts_with("index"))  %>%
  fxn_kable()


wd_total %>%
  pairwise_t_test(
    value_norm ~ time,
    paired = TRUE,
    p.adjust.method = "bonferroni") %>%
  clean_names() %>%
  mutate(method = "pwc",
         statistic = fxn_digit(statistic),
         fuel_type = index_wd,
         fuel_class = "All",
         index_value = "value_norm") %>%
  fxn_signif_adj() %>%
  select(fuel_class,
         method,
         starts_with("group"),
         starts_with("p_adj"),
         statistic,
         starts_with("d"),
         starts_with("index"),
         - p_adj_signif) %>%
  arrange(p_adj)  %>%
  fxn_kable()
# Mean ----
