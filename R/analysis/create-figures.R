# revised: 2022-10-27 ----
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries 
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(janitor)   ## To clean data frames
library(rstatix)  ## For repeated measure ANOVA
library(broom)  ## To format output tables from statistical tests
library(lemon)  ## To manipulate faceted ggplots
# library(kableExtra)  ## For tables in rmarkdown
library(ggpubr)  ## For plot templates 
library(ggsignif)  ## To add significance results to plots

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

# Define plot settings ----
colors_year <- c("gray70", "#C47E61", "#dbae84", "#e9d097")
colors_dl <- c("#7da8b0", "#c4c4c4")
# plot_colors <- 
#   read_csv(here(path_lookup, 
#                 "plot-colors.csv")) %>%
#   arrange(palette, palette_subset, levels)
# 
# colors_year <- 
#   plot_colors %>%
#   filter(palette %in% "year", 
#          palette_subset %in% "1pre_3post") %>%
#   pull(hex_code) 

theme_fuels <- function(){
  theme_minimal() +
    theme( 
      # Format the facet strip
      panel.spacing = unit(1.2, "lines"),
      panel.border = element_rect(fill = NA, color = "gray91", size = 2),
      panel.grid = element_line(color = "white"),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "gray91", color = "gray91"),
      # axis.line.x = element_line(color = "black"),
      # axis.line.y = element_line(color = "black"),
      # Format the y-axis text 
      axis.title.y = element_text(size = 18, vjust = 1.5),
      axis.text.y = element_text(size = 16, color = "gray50"),
      # Format x-axis 
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      # Format the legend
      legend.position = "right", 
      legend.box.spacing = unit(1.2, "lines"), 
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16, color = "gray30"), 
      legend.spacing.y = unit(0.5, 'cm'))  
}
  
# Create Tubbs data  ----
tubbs_input <-
  read_csv(here(path_derived, "tubbs_total-by-plot-type-yr.csv")) %>%
  mutate(lab_fuel = "All", 
         fuel_class = "all")

tubbs <- 
  read_csv(here(path_derived, "tubbs_mean-by-plot-type-class-yr.csv")) %>%
  bind_rows(tubbs_input) %>%
  left_join(lookup_units, "data_type") %>%
  arrange(year) %>%
  mutate(year = as_factor(year), 
         lab_year = as_factor(lab_year), 
         lab_year_abbr = as_factor(lab_year_abbr), 
         value = fxn_digit(value_si)) %>%
  group_by(data_type) %>%
  arrange(year, fuel_class) %>%
  mutate_if(is.character, as_factor) %>%
  ungroup() %>%
  relocate(c(statistic, subset), .after = value)   

tubbs_dl <- 
  tubbs %>%
  filter(data_type %in% "dl")  

tubbs_wd <- 
  tubbs %>%
  filter(data_type %in% "wd")  



# Create thin data  ----
thin_input <-
  read_csv(here(path_derived, "thin_total-by-plot-type-trmt.csv")) %>%
  mutate(lab_fuel = "All", 
         fuel_class = "all")

thin <- 
  read_csv(here(path_derived, "thin_mean-by-plot-type-class-trmt.csv")) %>%
  bind_rows(thin_input) %>%
  left_join(lookup_units, "data_type") %>%
  arrange(survey, plot_id, data_type) %>%
  mutate_if(is.character, as_factor)  %>%
  mutate(lab_trmt = ifelse(survey %in% "cont", "Not thinned", "Thinned"),
         value = fxn_digit(value_si)) %>%
  relocate(c(statistic, subset), .after = value) %>%
  select(-lab_type)
  

thin_dl <- 
  thin %>%
  filter(data_type %in% "dl")  

thin_wd <- 
  thin %>%
  filter(data_type %in% "wd")  


# ========================================================== -----
# DUFF AND LITTER (dl) -----
# Visualize duff and litter by year and fuel class as faceted boxplot 

# ---------------------------------------------------------- -----
# TUBBS  -----
# Visualize duff and litter by year and fuel class as faceted boxplot 
# Create base box plot ----
# Without significant comparisons
# Without x-axis labels or title 
tubbs_dl_box_base <- 
  tubbs_dl %>%
  ggplot(aes(x = year, 
           y = value, 
           fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = "Mean fuel depth (centimeters)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel, nrow = 1) +
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


# Add year (YYYY) as x-axis label ----
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

# Add significant comparisons (POST-PROCESSING BY HAND) ----
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



# [NOT RUN] Create separate plots, join with patchwork ----
# Create annotations to visualize p-values on boxplot 
# tubbs_dl_pwc <- 
#   tubbs_dl %>%
#   group_by(fuel_class) %>%
#   pairwise_t_test(value ~ year, paired = TRUE, p.adjust.method = "bonferroni")  %>%
#   add_xy_position(x = "year")  

# Add brackets and p-values 
# Without significant comparisons
# Without x-axis labels or title 
# 
# tubbs_dl %>%
#   filter(fuel_class %in% "all") %>%
#   ggplot(aes(x = year, 
#              y = value, 
#              fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
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
#   ggboxplot(x = "year",
#             y = "value",
#             fill = "year",
#             facet.by = "lab_fuel") +
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
#   pairwise_t_test(value ~ year, paired = TRUE) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>%
#   add_xy_position(x = "year")  
# 
# # Create a box plot
# bxp <- 
#   tubbs_dl %>%
#   ggboxplot(x = "year",
#             y = "value",
#             fill = "year",
#             facet.by = "lab_fuel")  
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



# x_pwc <- compare_means(value ~ year, data = tubbs_dl, group.by = "fuel_class", paired = TRUE, method = "t.test")
# x_pwc %>%
#   filter(p.adj > 0.05)

# tubbs_dl %>%
#   ggboxplot(x = "year", 
#             y = "value",
#             fill = "year", 
#             facet.by = "lab_fuel") + 
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
#   filter(lab_fuel %in% "All") %>%
#   ggplot(aes(x = year, 
#              y = value, 
#              fill = year)) +
#   geom_boxplot(outlier.size = 0.8) +
#   geom_signif(comparisons = list(c("2016", "2017"), 
#                                  c("2016", "2019"), 
#                                  c("2016", "2021")), 
#               map_signif_level=TRUE)
# ---------------------------------------------------------- -----
# THIN  -----
# Create base box plot ----
# Without significant comparisons
# Without x-axis labels or title 
# thin_dl_box_base <- 
  thin_dl %>%
  ggplot(aes(x = lab_trmt, 
             y = value, 
             fill = as_factor(str_wrap(lab_trmt, 15)))) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = "Mean fuel depth (centimeters)", 
       fill = "Treatment") + 
  facet_wrap(~lab_fuel, nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) +
  scale_y_continuous(limits = c(0, 12.5), 
                     breaks = c(0, 4, 8, 12)) + 
  theme_fuels()

thin_dl_box_base 

ggsave(thin_dl_box_base, 
       filename = here(path_plots, "thin_dl_boxplot_legend-right.png"), 
       width = 9.25, 
       height = 5)


# Add year (YYYY) as x-axis label ----
# Without significant comparison labels
thin_dl_box_x <- 
  thin_dl_box_base +
  labs(x = "Year") + 
  theme(
    axis.title.x = element_text(size = 18, vjust = -1.2),
    axis.text.x = element_text(size = 12, color = "gray50"),
    # Increase margins around plot to accommodate repositioned x-axis title
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
thin_dl_box_x
ggsave(thin_dl_box_x, 
       filename = here(path_plots, "thin_dl_boxplot_legend-right_with-x.png"), 
       width = 9.25, 
       height = 5)

# Add significant comparisons (POST-PROCESSING BY HAND) ----
# Shows the n.s. comparison bars 
# Manually correct significance labels 

# # thin_dl_box_x_pwc <- 
# thin_dl_box_x +
#   stat_compare_means(
#     aes(label = ..p.signif..),
#     comparisons = thin_comparisons,
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
#               "thin_dl_boxplot_legend-right_with-x-signif.png"), 
#        width = 9.25, 
#        height = 5)



# ========================================================== -----
# COARSE WOODY DEBRIS (wd) -----
# ---------------------------------------------------------- -----
# TUBBS -----
# README: About the CWD box plots ----
# Visualize coarse woody debris by year and fuel class as faceted boxplot 
# Include significant comparisons ?
# CAPTION: A box plot of coarse woody debris by year and fuel class showed the post-fire trends differed among the five coarse woody debris fuel classes. [Note: The y-axis scale in the figure below differs by fuel class]

# Create base box plot ----
# Legend above the plot 
# Without significant comparisons
# Without x-axis labels or title 
tubbs_wd_box_base <- 
  tubbs_wd %>%
  ggplot(aes(x = year, 
             y = value, 
             fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray50") +
  geom_boxplot(outlier.size = 0.8) +
  labs(y = "Mean fuel load (metric tons per hectare)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel, scales = "free", nrow = 1) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) +
  theme_fuels()

tubbs_wd_box_base 

ggsave(tubbs_wd_box_base, 
       filename = here(path_plots, "tubbs_wd_boxplot_legend-right.png"), 
       width = 16, 
       height = 5)
# Add year (YYYY) as x-axis label ----
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

# Add year (YYYY) and move legend to above the plot ----
# Without significant comparisons
# Without x-axis labels or title 
tubbs_wd %>%
  ggboxplot(x = "year",
            y = "value",
            fill = "lab_year_abbr",
            outlier.size = 0.8,
            palette = colors_year) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "gray15") +
  labs(y = "Mean fuel load (metric tons per hectare)",
       x = "Year",
       fill = "Year") +
  facet_wrap(~lab_fuel,  scales = "free", nrow = 1) +
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
# THIN  -----
# ========================================================== -----
# GRAVEYARD ----