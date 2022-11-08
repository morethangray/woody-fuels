# revised: 2022-11-08 ----
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from metrical tests
library(lemon)  ## To manipulate faceted ggplots
library(ggpubr)  ## For plot templates 
library(ggsignif)  ## To add significance results to plots

# Define file paths 
path_r <- here("R")
path_fxn <- here(path_r, "functions")
path_lookup <- here("input/lookup-tables")
path_derived <- here("input/data_derived")
# Create a new folder for new iteration of plots
folder_plots_datetime <- 
  as.character(Sys.time()) %>%
  str_replace_all(" ", "_") %>%
  str_replace_all(":", ".")
path_plots_datetime <- here("output/plots", folder_plots_datetime)
dir.create(path_plots_datetime)
# path_plots <- here("output/plots")

# Source functions 
source(file = here(path_fxn, "basic-functions.R"))
source(file = here(path_fxn, "plot-themes.R"))
# ========================================================== -----
# CREATE DATA FRAMES ----
# Define plot settings ----
colors_year <- c("gray70", "#C47E61", "#dbae84", "#e9d097")
colors_thin_faded <- c("#bfbabb", "#75bca8")
colors_stacked <- c("gray25", 
                    "#5c5c5c",
                    "#858585",
                    "#adadab",
                    "gray85")
# colors_thin_bright <- c("#9d9596", "#069879")
# Create helpers ----
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

index_dl_si <- "Mean fuel depth (centimeters)"
index_wd_si <- "Mean fuel load (metric tons per hectare)"


# For box plots  ----
tubbs <- 
  read_csv(here(path_derived, "tubbs_derived-norm.csv")) %>%
  arrange(time) %>%
  mutate(time = as_factor(time), 
         lab_time = as_factor(lab_time), 
         lab_time_abbr = as_factor(lab_time_abbr), 
         si_value = fxn_digit(si_value)) %>%
  group_by(fuel_type) %>%
  arrange(time, fuel_class) %>%
  mutate_if(is.character, as_factor) %>%
  ungroup() %>%
  relocate(c(metric, subset_by), .after = si_value) 

tubbs_dl <- 
  tubbs %>%
  filter(fuel_type %in% "dl")  

tubbs_wd <- 
  tubbs %>%
  filter(fuel_type %in% "wd")  

thin <-
  read_csv(here(path_derived, "thin_derived-norm.csv")) %>%
  arrange(time, fuel_type, fuel_class) %>%
  mutate_if(is.character, as_factor)  %>%
  mutate(si_value = fxn_digit(si_value))
  
thin_dl <- 
  thin %>%
  filter(fuel_type %in% "dl")  

thin_wd <- 
  thin %>%
  filter(fuel_type %in% "wd")  

# For stacked bar plots  ----
# Calculate mean CWD by time x fuel class for stacked bar plot
tubbs_wd_stack <- 
  # Read the data for mean by plot x time x fuel class 
  tubbs %>%
  filter(metric %in% "mean", 
         fuel_type %in% "wd") %>%
  arrange(time, order_class) %>%
  mutate_if(is.character, as_factor) %>%
  # Calculate summary metrics by fuel class x time 
  group_by(fuel_class, 
           lab_class,
           time, 
           lab_time,
           lab_time_abbr,
           units = si_units) %>%
  summarize(mean = mean(si_value)) %>%
  ungroup() 

thin_wd_stack <- 
  # Read the data for mean by plot x time x fuel class 
  thin   %>%
  filter(fuel_class %nin% "all", 
         fuel_type %in% "wd") %>%
  arrange(time, order_class) %>%
  mutate_if(is.character, as_factor) %>%
  # Calculate summary metrics by fuel class x time 
  group_by(fuel_class, 
           lab_class,
           time, 
           lab_time,
           lab_time_abbr,
           units = si_units) %>%
  summarize(mean = mean(si_value)) %>%
  ungroup() 

# Coarse woody debris, stacked by size class 
# ========================================================== -----
# TUBBS  -----
# Duff and litter (dl) -----
# Visualize duff and litter by time and fuel class as faceted boxplot 
# Without significant comparisons
# Without x-axis labels or title 
tubbs_dl %>%
  ggplot(aes(x = time, 
             y = si_value, 
             # Wrap the long label name at 15 characters
             fill = as_factor(str_wrap(lab_time_abbr, 15)))) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = index_dl_si, 
       fill = "Year") + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) + 
  theme_fuels() +
  facet_wrap(~lab_class, scales = "fixed", nrow = 1) +
  scale_y_continuous(limits = c(0, 14.5), 
                     breaks = c(0, 4, 8, 12)) 

ggsave(filename = here(path_plots_datetime, "tubbs_dl_boxplot.png"), 
       width = 9.25, 
       height = 5)

# Add significant comparisons (POST-PROCESSING BY HAND)
# Identify significant pairwise comparisons
# Use these for post-processing
tubbs_dl %>%
  group_by(fuel_class) %>%
  pairwise_t_test(value_norm ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
  filter(p.adj < 0.05)

# [NOT RUN] 
# Add year (YYYY) as x-axis label 
# Without significant comparison labels
# tubbs_dl_box_x <- 
#   tubbs_dl_box_base +
#   labs(x = "Year") + 
#   theme(
#     axis.title.x = element_text(size = 18, vjust = -1.2),
#     axis.text.x = element_text(size = 12, color = "gray50"),
#     # Increase margins around plot to accommodate repositioned x-axis title
#     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
#   )
# tubbs_dl_box_x
# 
# ggsave(tubbs_dl_box_x, 
#        filename = here(path_plots_datetime, "tubbs_dl_boxplot_legend-right_with-x.png"), 
#        width = 9.25, 
#        height = 5)


# Coarse woody debris (wd) -----
# Visualize coarse woody debris by year and fuel class as faceted boxplot 
# Without significant comparisons
# Without x-axis labels or title 
tubbs_wd %>%
  ggplot(aes(x = time, 
             # y = us_value,
             y = si_value, 
             # Wrap the long label name at 15 characters
             fill = as_factor(str_wrap(lab_time_abbr, 15)))) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = index_wd_si,
       fill = "Year") + 
  facet_wrap(~lab_class, scales = "free", nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) +
  theme_fuels()

ggsave(filename = here(path_plots_datetime, "tubbs_wd_boxplot.png"), 
       width = 16, 
       height = 5)

# Add significant comparisons (POST-PROCESSING BY HAND)
# Identify significant pairwise comparisons
# Use these for post-processing
tubbs_wd %>%
  group_by(fuel_class) %>%
  pairwise_t_test(value_norm ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
  filter(p.adj < 0.05)


# [NOT RUN] 
# # Plot with US units for comparison 
# tubbs_wd %>%
#   ggplot(aes(x = time, 
#              y = us_value,
#              # Wrap the long label name at 15 characters
#              fill = as_factor(str_wrap(lab_time_abbr, 15)))) +
#   geom_hline(yintercept = 0, 
#              linetype = "longdash", 
#              color = "gray50") +
#   geom_boxplot(outlier.size = 0.8) +
#   labs(y = "Mean fuel load (tons per acre)",
#        fill = "Year") + 
#   facet_wrap(~lab_class, scales = "free", nrow = 1) +
#   guides(fill = guide_legend(byrow = TRUE)) +
#   scale_fill_manual(values = colors_year) +
#   theme_fuels()
# 
# ggsave(filename = here(path_plots_datetime, "tubbs_wd_boxplot_us-units.png"), 
#        width = 16, 
#        height = 5)

# Add year (YYYY) as x-axis label 
# Without significant comparison labels
# tubbs_wd_box_x <- 
#   tubbs_wd_box_base +
#   labs(x = "Year") + 
#   theme(
#     axis.title.x = element_text(size = 18, vjust = -1.2),
#     axis.text.x = element_text(size = 11, color = "gray50"),
#     # Increase margins around plot to accommodate repositioned x-axis title
#     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
#   )
# 
# tubbs_wd_box_x
# ggsave(tubbs_wd_box_x, 
#        filename = here(path_plots_datetime, "tubbs_wd_boxplot_legend-right_with-x.png"), 
#        width = 16, 
#        height = 5)
# 
# Add year (YYYY) and move legend to above the plot 
# Without significant comparisons
# Without x-axis labels or title 
# tubbs_wd %>%
#   ggboxplot(x = "time",
#             y = "si_value",
#             fill = "lab_time_abbr",
#             outlier.size = 0.8,
#             palette = colors_year) +
#   geom_hline(yintercept = 0, 
#              linetype = "longdash", 
#              color = "gray15") +
#   labs(y = "Mean fuel load (metric tons per hectare)",
#        x = "Year",
#        fill = "Year") +
#   facet_wrap(~lab_class,  scales = "free", nrow = 1) +
#   theme(
#     # Format the facet strip
#     panel.spacing = unit(1.2, "lines"),
#     panel.border = element_rect(fill = NA, color = "gray91", size = 2),
#     panel.grid = element_line(color = "white"),
#     strip.text = element_text(size = 16),
#     strip.background = element_rect(fill = "gray91", color = "gray91"),
#     # Format the y-axis text 
#     axis.title.y = element_text(size = 18, vjust = 1.5),
#     axis.text.y = element_text(size = 16, color = "gray50"),
#     # Format x-axis  
#     axis.title.x = element_text(size = 18, vjust = -1.2),
#     axis.text.x = element_text(size = 14, color = "gray50"),
#     axis.ticks.x = element_blank(),
#     # Increase margins around plot to accommodate repositioned x-axis title
#     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
#     # Format the legend
#     legend.position = "top", 
#     legend.box.spacing = unit(1.2, "lines"), 
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 18, color = "gray30"), 
#     legend.spacing.y = unit(0.5, 'cm')
#   )  
# 
# ggsave(filename = here(path_plots_datetime, "tubbs_wd_boxplot_legend-top_with-x.png"),
#        width = 17.5,
#        height = 5.5)





# ========================================================== -----
# THIN  -----
# Duff and litter (dl) -----
# Without significant comparisons
thin_dl %>%
  ggplot(aes(x = lab_time, 
             y = si_value,  
             fill = lab_time))  +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = index_dl_si, 
       fill = "Timing") + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_thin_faded) +
  theme_fuels() +
  facet_wrap(~lab_class, nrow = 1) +
  scale_y_continuous(limits = c(0, 6.5),
                     breaks = c(0, 2, 4, 6)) 

ggsave(filename = here(path_plots_datetime, "thin_dl_boxplot.png"), 
       width = 9.25, 
       height = 5)

# Add significant comparisons (POST-PROCESSING BY HAND)
# Identify significant pairwise comparisons
# Use these for post-processing
thin_dl %>%
  group_by(fuel_class) %>%
  pairwise_t_test(value_norm ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
  filter(p.adj < 0.05)


# Coarse woody debris (wd) -----
# Without significant comparisons
thin_wd %>%
  ggplot(aes(x = lab_time, 
             y = si_value,  
             fill = lab_time))  +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = index_wd_si, 
       fill = "Timing") + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_thin_faded) +
  theme_fuels() +
  facet_wrap(~lab_class, scales = "free", nrow = 1)  

ggsave(filename = here(path_plots_datetime, "thin_wd_boxplot.png"), 
       width = 16, 
       height = 5)

# Add significant comparisons (POST-PROCESSING BY HAND)
# Identify significant pairwise comparisons
# Use these for post-processing
thin_wd %>%
  group_by(fuel_class) %>%
  pairwise_t_test(value_norm ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
  filter(p.adj < 0.05)


# ========================================================== -----
# STACKED BAR CHARTS ----

# Tubbs ----
tubbs_wd_stack %>% 
  ggplot(aes(x = lab_time_abbr, 
             y = mean, 
             fill = lab_class)) +
  geom_bar(stat = "identity",
           width = 0.5, 
           color = "black", 
           size = 0.05,
           position = "stack") +
  scale_fill_manual(values = colors_stacked, 
                    name = "Size class") + 
  theme_stacked() + 
  labs(y = index_wd_si) + 
  # Remove space below 0 on y-axis
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(here(path_plots_datetime, "tubbs_wd_stacked-barchart.png"),
       width = 10, 
       height = 5.2)

# Thin ----
thin_wd_stack %>% 
  ggplot(aes(x = lab_time, 
             y = mean, 
             fill = lab_class)) +
  geom_bar(stat = "identity",
           width = 0.5, 
           position = "stack") +
  scale_fill_manual(values = colors_stacked, 
                    name = "Size class") + 
  theme_stacked() + 
  labs(y = index_wd_si) + 
  # Remove space below 0 on y-axis
  scale_y_continuous(limits = c(0,200), expand = c(0, 0)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(here(path_plots_datetime, "thin_wd_stacked-barchart.png"),
       width = 6.5, 
       height = 5)

