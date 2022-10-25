# revised: 2022-10-25 ----
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
library(lubridate)   ## To work with dates and times
library(janitor)   ## To tidy data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from statistical tests
library(ggpubr)  ## For preset plot templates
library(patchwork)  ## To arrange multiple plots 

# Set file paths
# path_raw <- here("raw_data")
path_in <- here("input/2022")
path_out <- here("output/2022/oak-symposium/thin")
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
# ========================================================== -----
# CREATE DATA FRAMES -----
# Create data tables for derived data  ----
#   thin_plot_type_total ----
thin_plot_type_total <- 
  read_csv(here(path_in, "fuel_thin_total-by-plot-type_2022.csv")) %>%
  mutate(total = fxn_digit(total)) %>%
  arrange(survey, plot_id, data_type) %>%
  # Convert variable labels to factors for ANOVA
  mutate_if(is.character, as_factor)  %>%
  mutate(lab_trmt = ifelse(survey %in% "cont", "Not thinned", "Thinned"))

#   thin_plot_class_mean ----
thin_plot_class_mean <- 
  read_csv(here(path_in, "fuel_thin_mean-by-plot-type-class_2022.csv")) %>%
  mutate(mean = fxn_digit(mean)) %>%
  arrange(survey, plot_id, data_type, fuel_class) %>%
  # Convert variable labels to factors for ANOVA
  mutate_if(is.character, as_factor) %>%
  mutate(lab_trmt = ifelse(survey %in% "cont", "Not thinned", "Thinned"))

#   list_lab_fuel ----
# To iterate barbell plots by fuel class 
list_lab_fuel <- unique(thin_plot_class_mean$lab_fuel)
# Define plot colors ----
colors_trmt_bright <- c("#9d9596", "#069879")
colors_trmt_faded <- c("#bfbabb", "#75bca8")
color_palette <- thematic::okabe_ito(2)
#
# colors_fire2 <- c("gray78", "#c38368")
# plot_colors <- 
#   read_csv(here(path_in, 
#                 "plot-colors.csv")) %>%
#   arrange(palette, palette_subset, levels)
# colors_fuel <- 
#   plot_colors %>%
#   filter(palette %in% "fuel_class", 
#          palette_subset %in% "all") %>%
#   arrange(file_label) %>%
#   pull(hex_code)
# ========================================================== -----
# PLOT-LEVEL ESTIMATES BY TREATMENT ----
# Total by fuel class  -----
#   Visualize data: Boxplot ---- 
# Boxplot of plot-level total by fuel class  (control vs treatment) 
thin_plot_type_total %>%
  ggboxplot(x = "lab_trmt", 
            y = "total", 
            fill = "survey", 
            outlier.size = 0.3, 
            palette = colors_trmt_faded) + 
  facet_wrap(~lab_fuel, 
             ncol = 5,
             scales = "free")  +
  ylab("Fuel load") + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        panel.spacing = unit(2, "lines"))

# Mean by fuel class -----
# mean total fuel load ANOVA results and plot.
#   Create summary table ----
# Groups: data_type, fuel_class
thin_plot_class_mean %>%
  group_by(survey, data_type, fuel_class) %>%
  get_summary_stats(mean, type = "mean") %>%
  spread(survey, mean)

#   Visualize data: Paired boxplot ---- 

# Boxplot of plot-level mean by fuel class x fuel type 
# Data from the same plot are paired (control vs treatment)  
thin_plot_class_mean %>%
  select(-survey) %>%
  spread(lab_trmt, mean) %>%
  ggpaired(cond1 = "Not thinned", 
           cond2 = "Thinned",
           fill = "condition", 
           color = "black",
           width = 0.5,
           point.size = 1.2,
           line.size = 0.2,
           line.color = "gray15",
           linetype = "solid",
           palette = colors_trmt_faded, 
           xlab = "Treatment",
           ylab = "Fuel load", 
           title = "Plot-level mean fuel load, by thinning treatment") +
   facet_wrap(~lab_fuel, 
             ncol = 5,
             scales = "free")  +
  theme(legend.position = "none", 
        panel.spacing = unit(1, "lines"))
 
#   Visualize data: Dumbbell plot  ---- 
# Write a function to do this because I want the y-axis to sort each fuel class by the maximum treatment value
# I can't figure out how to facet the plots such that each y-axis differs (maybe switch x and y axes?)
# Instead it was quicker to write a function that creates each ordered barbell plot, then combine with patchwork

# index_data <- thin_plot_class_mean
# index_class <- list_lab_fuel[1]
fxn_plot_bars <- function(index_data, index_class, with_legend){
  
  subset <- 
    index_data %>%
    filter(lab_fuel %in% index_class)
  
  # text_title <- paste0("Plot-level mean fuel load (", index_class, ")")
  
  segment_helper <- 
    subset |>
    select(plot_id, fuel_class, lab_fuel, survey, mean) |>
    spread(survey, mean) |>
    mutate(change = trmt - cont)  |>
    arrange(fuel_class, trmt) %>%
    mutate(plot_id_f = as_factor(as.character(plot_id)))  
  
  barbell_plot <- 
  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_segment(
      data = segment_helper,
      aes(x = cont, xend = trmt, y = plot_id_f, yend = plot_id_f),
      col = 'grey60',
      size = 1.25) +
    geom_point(
      data = subset, 
      aes(x = mean, y = plot_id, color = lab_trmt), 
      size = 4) +
    scale_color_manual(values = color_palette) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(), 
      panel.grid.major.x = element_line(color = "gray90"),
      panel.background =  element_rect(fill = 'white', colour = 'black'),
      legend.position = "right",
      panel.spacing = unit(1, "lines")) + 
    labs(x = 'Fuel load',
         y = element_blank(),
         title = index_class)
  
  if(with_legend == TRUE){
    barbell_plot
  }else{
    barbell_plot +
      theme(legend.position = "none")
  }
  
  
  
}

p1 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[1], with_legend = FALSE)
p2 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[2], with_legend = FALSE)
p3 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[3], with_legend = FALSE)
p4 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[4], with_legend = FALSE)
p5 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[5], with_legend = FALSE)
p6 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[6], with_legend = FALSE)
p7 <- fxn_plot_bars(index_data = thin_plot_class_mean, index_class = list_lab_fuel[7], with_legend = TRUE)

# Group plots using patchwork
# 
(p1 | p2 | p3 | p4 | p5) /
  (p6 | p7 | plot_spacer() | plot_spacer() | plot_spacer()) 


# ========================================================== -----
# CONDUCT STATISTICAL TESTS -----
# Write functions -----
#   fxn_aov_me ----
fxn_aov_me <- function(df, index_time){
  subset <- 
    df %>%
    select(value, plot_id, time = all_of(index_time))
  
  subset %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = time) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "me") %>%
    rename(statistic = f) %>% 
    relocate(method, 
             effect, 
             starts_with("p_adj"), 
             statistic,
             starts_with("d"), 
             ges)
}

#   fxn_pwc ----
# Excludes fuel_class, is for pwc of fuel_type only
fxn_pwc <- function(df, index_time){
  subset <- 
    df %>%
    select(value, plot_id, time = all_of(index_time))
  
  subset %>%
    pairwise_t_test(
      value ~ time, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    relocate(method, 
             starts_with("group"),
             starts_with("p_adj"), 
             statistic,
             starts_with("d"),  
             starts_with("p"))
}

#   fxn_aov2_me ----
fxn_aov2_me <- function(df, index_time, index_variable){
  subset <- 
    df %>%
    select(value, 
           plot_id,
           time = all_of(index_time), 
           variable = all_of(index_variable))
  
  subset %>%
    anova_test(dv = value, 
               wid = plot_id, 
               within = c(time, variable)) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "aov2", 
           index_variable = index_variable)  %>%
    rename(statistic = f) %>%
    relocate(index_variable, 
             method, 
             effect, 
             starts_with("p_adj"), 
             statistic,
             starts_with("d"), 
             ges)
  
  
}

#   fxn_pwc2 ----
# Includes a column for fuel_class
fxn_pwc2 <- function(df, index_time, index_variable){
  
  subset <- 
    df %>%
    select(value, 
           plot_id,
           time = all_of(index_time), 
           variable = all_of(index_variable))
  
  subset %>%
    group_by(variable) %>%
    pairwise_t_test(
      value ~ time, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           index_variable = index_variable, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    relocate(index_variable, 
             variable, 
             method,
             starts_with("group"),
             starts_with("p_adj"), 
             statistic,
             starts_with("d"))
  
}


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
thin_plot_class_mean_diff <- 
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
  thin_plot_class_mean_diff %>%
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

# ========================================================== -----