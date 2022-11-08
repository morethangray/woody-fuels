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
folder_datetime <- 
  as.character(Sys.time()) %>%
  str_replace_all(" ", "_") %>%
  str_replace_all(":", ".")
path_datetime <- here("output/plots", folder_datetime)
dir.create(path_datetime)
# path_plots <- here("output/plots", path_datetime)

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

# Create Tubbs and thin data frames  ----
index_dl <- "Duff & litter"
index_wd <- "Coarse woody debris"

index_dl_si <- "Mean fuel depth (centimeters)"
index_wd_si <- "Mean fuel load (metric tons per hectare)"

tubbs <- 
  read_csv(here(path_derived, "tubbs_derived.csv")) %>%
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

tubbs %>%
  distinct(si_units)
tubbs_dl <- 
  tubbs %>%
  filter(fuel_type %in% "dl")  

tubbs_wd <- 
  tubbs %>%
  filter(fuel_type %in% "wd")  

thin <-
  read_csv(here(path_derived, "thin_derived.csv")) %>%
  arrange(time, fuel_type, fuel_class) %>%
  mutate_if(is.character, as_factor)  %>%
  mutate(si_value = fxn_digit(si_value))
  
thin_dl <- 
  thin %>%
  filter(fuel_type %in% "dl")  

thin_wd <- 
  thin %>%
  filter(fuel_type %in% "wd")  



# ========================================================== -----
# TUBBS  -----
# DUFF AND LITTER (dl) -----
# Visualize duff and litter by time and fuel class as faceted boxplot 
#   Create base box plot ----
# Without significant comparisons
# Without x-axis labels or title 
tubbs_dl_box_base <- 
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
  facet_wrap(~lab_class, nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) +
  scale_y_continuous(limits = c(0, 12.5), 
                     breaks = c(0, 4, 8, 12)) + 
  theme_fuels()

tubbs_dl_box_base 

ggsave(tubbs_dl_box_base, 
       filename = here(path_plots, "tubbs_dl_boxplot_legend-right.png"), 
       width = 9.25, 
       height = 5)


#   Add year (YYYY) as x-axis label ----
# Without significant comparison labels
tubbs_dl_box_x <- 
  tubbs_dl_box_base +
  labs(x = "Year") + 
  theme(
    axis.title.x = element_text(size = 18, vjust = -1.2),
    axis.text.x = element_text(size = 12, color = "gray50"),
    # Increase margins around plot to accommodate repositioned x-axis title
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
tubbs_dl_box_x

ggsave(tubbs_dl_box_x, 
       filename = here(path_plots, "tubbs_dl_boxplot_legend-right_with-x.png"), 
       width = 9.25, 
       height = 5)

# COARSE WOODY DEBRIS (wd) -----
# Visualize coarse woody debris by year and fuel class as faceted boxplot 
# Include significant comparisons ?
#   Create base box plot ----
# Legend above the plot 
# Without significant comparisons
# Without x-axis labels or title 
tubbs_wd_box_base <- 
  tubbs_wd %>%
  ggplot(aes(x = time, 
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

tubbs_wd_box_base 

ggsave(tubbs_wd_box_base, 
       filename = here(path_plots, "tubbs_wd_boxplot_legend-right.png"), 
       width = 16, 
       height = 5)
#   Add year (YYYY) as x-axis label ----
# Without significant comparison labels
tubbs_wd_box_x <- 
  tubbs_wd_box_base +
  labs(x = "Year") + 
  theme(
    axis.title.x = element_text(size = 18, vjust = -1.2),
    axis.text.x = element_text(size = 11, color = "gray50"),
    # Increase margins around plot to accommodate repositioned x-axis title
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

tubbs_wd_box_x
ggsave(tubbs_wd_box_x, 
       filename = here(path_plots, "tubbs_wd_boxplot_legend-right_with-x.png"), 
       width = 16, 
       height = 5)

#   Add year (YYYY) and move legend to above the plot ----
# Without significant comparisons
# Without x-axis labels or title 
tubbs_wd %>%
  ggboxplot(x = "time",
            y = "si_value",
            fill = "lab_time_abbr",
            outlier.size = 0.8,
            palette = colors_year) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray15") +
  labs(y = "Mean fuel load (metric tons per hectare)",
       x = "Year",
       fill = "Year") +
  facet_wrap(~lab_class,  scales = "free", nrow = 1) +
  theme(
    # Format the facet strip
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(fill = NA, color = "gray91", size = 2),
    panel.grid = element_line(color = "white"),
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "gray91", color = "gray91"),
    # Format the y-axis text 
    axis.title.y = element_text(size = 18, vjust = 1.5),
    axis.text.y = element_text(size = 16, color = "gray50"),
    # Format x-axis  
    axis.title.x = element_text(size = 18, vjust = -1.2),
    axis.text.x = element_text(size = 14, color = "gray50"),
    axis.ticks.x = element_blank(),
    # Increase margins around plot to accommodate repositioned x-axis title
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    # Format the legend
    legend.position = "top", 
    legend.box.spacing = unit(1.2, "lines"), 
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18, color = "gray30"), 
    legend.spacing.y = unit(0.5, 'cm')
  )  

ggsave(filename = here(path_plots, "tubbs_wd_boxplot_legend-top_with-x.png"),
       width = 17.5,
       height = 5.5)



# ---------------------------------------------------------- -----
# [NOT RUN] ----
# Add significant comparisons (POST-PROCESSING BY HAND)
# Shows the n.s. comparison bars 
# Manually correct significance labels 

# # tubbs_dl_box_x_pwc <- 
# tubbs_dl_box_x +
#   stat_compare_means(
#     aes(label = ..p.signif..),
#     comparisons = tubbs_comparisons,
#     group.by = "fuel_class",
#     paired = TRUE,
#     method = "t.test",
#     hide.ns = TRUE,
#     tip.length = 0,
#     vjust = 0.7,
#     step.increase = 0.05)  
# 
# ggsave(filename = 
#          here(path_plots, 
#               "tubbs_dl_boxplot_legend-right_with-x-signif.png"), 
#        width = 9.25, 
#        height = 5)

# Create separate plots, join with patchwork 
# Create annotations to visualize p-values on boxplot 
# tubbs_dl_pwc <- 
#   tubbs_dl %>%
#   group_by(fuel_class) %>%
#   pairwise_t_test(value ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
#   add_xy_position(x = "time")  

# Add brackets and p-values 
# Without significant comparisons
# Without x-axis labels or title 
# 
# tubbs_dl %>%
#   filter(fuel_class %in% "all") %>%
#   ggplot(aes(x = time, 
#              y = si_value, 
#              fill = as_factor(str_wrap(lab_time_abbr, 15)))) +
#   geom_boxplot(outlier.size = 0.8) +
#   theme_minimal() + 
#   labs(y = "Mean fuel depth (centimeters)", 
#        fill = "Year") + 
#   theme( 
#     # Format the facet strip
#     panel.spacing = unit(1.2, "lines"),
#     panel.border = element_rect(fill = NA, color = "gray91", size = 2),
#     panel.grid = element_line(color = "white"),
#     strip.text = element_text(size = 16),
#     strip.background = element_rect(fill = "gray91", color = "gray91"),
#     axis.line = element_line(color = "black"),
#     # Format the y-axis text 
#     axis.title.y = element_text(size = 18, vjust = 1.5),
#     axis.text.y = element_text(size = 16, color = "gray50"),
#     # Exclude x-axis labels because they are in legend
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(), 
#     axis.ticks.x = element_blank(),
#     # Format the legend
#     legend.position = "right", 
#     legend.box.spacing = unit(1.2, "lines"), 
#     legend.title = element_text(size = 18),
#     legend.text = element_text(size = 16, color = "gray30"), 
#     legend.spacing.y = unit(0.5, 'cm'))  +
#   guides(fill = guide_legend(byrow = TRUE)) +
#   scale_fill_manual(values = colors_year) 
# 
# tubbs_dl_box_x +
#   geom_bracket(
#     xmin = "2016", xmax = "2017", y.position = 30,
#     label = "t-test, p < 0.05"
#   )
# # 

# tubbs_comparisons <-  
#   list(c("2016", "2017"), 
#        c("2016", "2019"), 
#        c("2016", "2021"), 
#        c("2017", "2019"),
#        c("2017", "2021"),
#        c("2019", "2021"))
# tubbs_dl %>%
#   ggboxplot(x = "time",
#             y = "value",
#             fill = "time",
#             facet.by = "lab_class") +
#   stat_compare_means(
#     aes(label = ..p.signif..),
#     comparisons = tubbs_comparisons,
#     group.by = "fuel_class",
#     paired = TRUE,
#     method = "t.test",
#     hide.ns = TRUE,
#     tip.length = 0,
#     vjust = 0.7,
#     step.increase = 0.05)

# # Add p-values of `stat.test` and `stat.test2`
# # 1. Add stat.test
# stat.test <- 
#   tubbs_dl %>%
#   group_by(fuel_class) %>%
#   pairwise_t_test(value ~ time, paired = TRUE) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>%
#   add_xy_position(x = "time")  
# 
# # Create a box plot
# bxp <- 
#   tubbs_dl %>%
#   ggboxplot(x = "time",
#             y = "value",
#             fill = "time",
#             facet.by = "lab_class")  
# 
# # Make facet and add p-values
# bxp + stat_pvalue_manual(stat.test)
# 
# # Make the facet scale free and add jitter points
# bxp <- ggboxplot(
#   df, x = "supp", y = "len", fill = "#00AFBB", 
#   facet.by = "dose", scales = "free", add = "jitter"
# )
# 
# # Move down the bracket using `bracket.nudge.y`
# # Hide ns (non-significant)
# # Show adjusted p-values and significance levels
# # Add 10% spaces between the p-value labels and the plot border
# bxp +  
#   stat_pvalue_manual(
#     stat.test, bracket.nudge.y = -2, hide.ns = TRUE,
#     label = "{p.adj}{p.adj.signif}"
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))



# x_pwc <- compare_means(value ~ time, data = tubbs_dl, group.by = "fuel_class", paired = TRUE, method = "t.test")
# x_pwc %>%
#   filter(p.adj > 0.05)

# tubbs_dl %>%
#   ggboxplot(x = "time", 
#             y = "value",
#             fill = "time", 
#             facet.by = "lab_class") + 
#   stat_pvalue_manual(tubbs_dl_pwc, hide.ns = TRUE, label = "p.adj.signif") 
  
# stat_pvalue_manual(
#     # aes(label = ..p.signif..),
#     comparisons = tubbs_comparisons, 
#     group.by = "fuel_class", 
#     paired = TRUE, 
#     method = "t.test", 
#     label = "p.adj",
#     hide.ns = TRUE, 
#     tip.length = 0, 
#     step.increase = 0.1)

# tubbs_dl %>%
#   filter(lab_class %in% "All") %>%
#   ggplot(aes(x = time, 
#              y = si_value, 
#              fill = time)) +
#   geom_boxplot(outlier.size = 0.8) +
#   geom_signif(comparisons = list(c("2016", "2017"), 
#                                  c("2016", "2019"), 
#                                  c("2016", "2021")), 
#               map_signif_level=TRUE)
# ========================================================== -----
# THIN  -----
# DUFF AND LITTER (dl) -----
#   Create base box plot ----
# Without significant comparisons
thin_dl_box_base <- 
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
  facet_wrap(~lab_class, nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_thin_faded) +
  scale_y_continuous(limits = c(0, 6.5),
                     breaks = c(0, 2, 4, 6)) +
  theme_fuels() 

thin_dl_box_base 

ggsave(thin_dl_box_base, 
       filename = here(path_plots, "thin_dl_boxplot.png"), 
       width = 9.25, 
       height = 5)

#   Add significant comparisons (POST-PROCESSING BY HAND) ----
# Shows the n.s. comparison bars 
# Manually correct significance labels 

thin_dl %>%
  ggplot(aes(x = lab_time, 
             y = si_value,  
             fill = lab_time))  +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = "Mean fuel depth (centimeters)", 
       fill = "Timing") + 
  facet_wrap(~lab_class, nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_thin_faded) +
  scale_y_continuous(limits = c(0, 6.5),
                     breaks = c(0, 2, 4, 6)) +
  theme_fuels() +
  stat_compare_means(
    aes(label = ..p.signif..),
    comparisons = list(c("Pre-thinning", "Post-thinning")),
    group.by = "fuel_class",
    paired = TRUE,
    method = "t.test",
    hide.ns = TRUE,
    tip.length = 0,
    vjust = 0.7,
    step.increase = 0.05)

ggsave(filename =
         here(path_plots,
              "thin_dl_boxplot_with-signif.png"),
       width = 9.25,
       height = 5)

# COARSE WOODY DEBRIS (wd) -----
#   Create base box plot ----
# Without significant comparisons
thin_wd_box_base <- 
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
  facet_wrap(~lab_class, scales = "free", nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_thin_faded) +
  theme_fuels() 

thin_wd_box_base 

ggsave(thin_wd_box_base, 
       filename = here(path_plots, "thin_wd_boxplot.png"), 
       width = 16, 
       height = 5)

# # No significant comparisons to add 
# thin_wd %>%
#   group_by(fuel_class) %>%
#   pairwise_t_test(si_value ~ lab_time, 
#                   paired = TRUE,
#                   p.adjust.method = "bonferroni")  

# ========================================================== -----
# STACKED BAR CHARTS ----
# Create data frames ----
# Calculate mean CWD by time x fuel class for stacked bar plot
tubbs_wd_stack <- 
  # Read the data for mean by plot x time x fuel class 
  tubbs %>%
  filter(fuel_class %nin% "all", 
         fuel_type %in% "wd") %>%
  arrange(time, order_class) %>%
  mutate_if(is.character, as_factor) %>%
  # Calculate summary metrics by fuel class x time 
  group_by(fuel_class, 
           lab_class,
           time, 
           lab_time,
           lab_time_abbr) %>%
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
           lab_time_abbr) %>%
  summarize(mean = mean(si_value)) %>%
  ungroup() 


# Coarse woody debris, stacked by size class 
# TUBBS ----
#   With legend ----
tubbs_wd_stack %>% 
  ggplot(aes(x = lab_time_abbr, 
             y = mean, 
             fill = lab_class)) +
  geom_bar(stat = "identity",
           width = 0.5, 
           color = "black", 
           size = 0.05,
           position = "stack") +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             color = "gray30") +
  scale_fill_manual(values = colors_stacked, 
                    name = "Size class") + 
  theme_stacked() + 
  labs(x = "Chronosequence", 
       y = index_wd_si) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(here(path_plots, "tubbs_wd_stacked-barchart.png"),
       width = 10, 
       height = 5.2)

# THIN ----
#   With legend ----
thin_wd_stack %>% 
  ggplot(aes(x = lab_time, 
             y = mean, 
             fill = lab_class)) +
  geom_bar(stat = "identity",
           width = 0.5, 
           position = "stack") +
  geom_hline(yintercept = 0,
             linetype = "longdash",
             color = "gray30") +
  scale_fill_manual(values = colors_stacked, 
                    name = "Size class") + 
  theme_stacked() + 
  labs(x = "Chronosequence", 
       y = index_wd_si) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(here(path_plots, "thin_wd_stacked-barchart.png"),
       width = 6.5, 
       height = 5)
