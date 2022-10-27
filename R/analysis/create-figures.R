# revised: 2022-10-25 ----

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
library(kableExtra)  ## For tables in rmarkdown
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

# Create helpers: Lookups and plot colors 
lookup_units <- 
  tibble(data_type = c("dl", "wd"),
         units = c("Depth in centimeters",
                   "Metric tons per hectare"))

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

# ========================================================== -----
# TUBBS -----
# Create data frames ----
tubbs_input <-
  read_csv(here(path_derived, "tubbs_total-by-plot-type-yr.csv")) %>%
  mutate(lab_fuel = "All", 
         fuel_class = "all")

tubbs <- 
  read_csv(here(path_derived, "tubbs_mean-by-plot-type-class-yr.csv")) %>%
  bind_rows(tubbs_input) %>%
  left_join(lookup_units, "data_type") %>%
  # distinct(data_type, fuel_class, lab_fuel, lab_type, units, statistic)
  arrange(year) %>%
  mutate(year = as_factor(year), 
         lab_year = as_factor(lab_year), 
         lab_year_abbr = as_factor(lab_year_abbr), 
         value = fxn_digit(value_si)) %>%
  relocate(c(statistic, subset), .after = value)   

# ---------------------------------------------------------- -----
# DUFF AND LITTER (dl) -----
# Create data subset ----
tubbs_dl <- 
  tubbs %>%
  filter(data_type %in% "dl") %>%
  arrange(year, fuel_class) %>%
  mutate_if(is.character, as_factor) 
# Create boxplot ----
#   Without significant comparisons ----
# Visualize duff and litter by year and fuel class as faceted boxplot 
tubbs_dl %>%
  ggplot(aes(x = year, 
           y = value, 
           fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_minimal() + 
  labs(y = "Mean fuel depth (centimeters)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel,  scales = "free", nrow = 1) + 
  theme( 
    # Format the facet strip
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(fill = NA, color = "gray91", size = 2),
    panel.grid = element_line(color = "white"),
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "gray91", color = "gray91"),
    axis.line = element_line(color = "black"),
    # Format the y-axis text 
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16, color = "gray50"),
    # Exclude x-axis labels because they are in legend
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Format the legend
    legend.position = "right", 
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, color = "gray30"), 
    legend.spacing.y = unit(0.5, 'cm'))  +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) 

ggsave(filename = here(path_plots, "tubbs_dl_boxplot_legend-right.png"), 
       width = 9.25, 
       height = 5)

#   Add significant comparisons ----
# Create annotations to visualize p-values on boxplot  
tubbs_dl_pwc <- 
  tubbs_dl %>%
  group_by(fuel_class) %>%
  pairwise_t_test(value ~ year, paired = TRUE, p.adjust.method = "bonferroni")  %>%
  add_xy_position(x = "year")  

my_comparisons = list(c("2016", "2017"), 
                      c("2016", "2019"), 
                      c("2016", "2021"), 
                      c("2017", "2019"),
                      c("2017", "2021"),
                      c("2019", "2021"))

# # Shows the n.s. comparison bars 
# tubbs_dl %>%
#   ggboxplot(x = "year",
#             y = "value",
#             fill = "year",
#             facet.by = "lab_fuel") +
#   stat_compare_means(
#     aes(label = ..p.signif..),
#     comparisons = my_comparisons,
#     group.by = "fuel_class",
#     paired = TRUE,
#     method = "t.test",
#     hide.ns = TRUE,
#     tip.length = 0,
#     step.increase = 0.09)

# 
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

tubbs_dl %>%
  ggboxplot(x = "year", 
            y = "value",
            fill = "year", 
            facet.by = "lab_fuel") + 
  stat_pvalue_manual(tubbs_dl_pwc, hide.ns = TRUE, label = "p.adj.signif") 
  
# stat_pvalue_manual(
#     # aes(label = ..p.signif..),
#     comparisons = my_comparisons, 
#     group.by = "fuel_class", 
#     paired = TRUE, 
#     method = "t.test", 
#     label = "p.adj",
#     hide.ns = TRUE, 
#     tip.length = 0, 
#     step.increase = 0.1)
          

  
  stat_compare_means(comparisons = my_comparisons)
                     
# Box plot facetted by "dose"
p <- ggboxplot(ToothGrowth, 
               x = "supp",
               y = "len",
               color = "supp", 
               palette = "npg",
               add = "jitter",
               facet.by = "dose", 
               short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.

tubbs_dl %>%
  filter(lab_fuel %in% "All") %>%
  ggplot(aes(x = year, 
             y = value, 
             fill = year)) +
  geom_boxplot(outlier.size = 0.8) +
  geom_signif(comparisons = list(c("2016", "2017"), 
                                 c("2016", "2019"), 
                                 c("2016", "2021")), 
              map_signif_level=TRUE)

tubbs_dl %>%
  ggplot(aes(x = year,
             y = value,
             fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_minimal() + 
  labs(y = "Mean fuel depth (centimeters)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel,  scales = "free", nrow = 1) +
  theme( 
    # Format the facet strip
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(fill = NA, color = "gray91", size = 2),
    panel.grid = element_line(color = "white"),
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "gray91", color = "gray91"),
    axis.line = element_line(color = "black"),
    # Format the y-axis text 
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16, color = "gray50"),
    # Exclude x-axis labels because they are in legend
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Format the legend
    legend.position = "right", 
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, color = "gray30"), 
    legend.spacing.y = unit(0.5, 'cm'))  +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) 

  # stat_pvalue_manual(tubbs_dl_pwc, tip.length = 0, hide.ns = TRUE) 

# ---------------------------------------------------------- -----
# COARSE WOODY DEBRIS (wd) -----
# Create data subset ----
tubbs_wd <- 
  tubbs %>%
  filter(data_type %in% "wd") %>%
  arrange(year, fuel_class) %>%
  mutate_if(is.character, as_factor)

# Create boxplot ----
#   About the boxplots ----
# Visualize coarse woody debris by year and fuel class as faceted boxplot 
# Include significant comparisons ?
# CAPTION: A box plot of coarse woody debris by year and fuel class showed the post-fire trends differed among the five coarse woody debris fuel classes. [Note: The y-axis scale in the figure below differs by fuel class]

#   Legend above the plot ----
tubbs_wd %>%
  ggboxplot(x = "year", 
            y = "value", 
            fill = "lab_year_abbr",
            outlier.size = 0.8,
            palette = colors_year) + 
  labs(y = "Mean fuel load (metric tons per hectare)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel,  scales = "free", nrow = 1) + 
  theme( 
    # Format the facet strip
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(fill = NA, color = "gray91", size = 2),
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "gray91", color = "gray91"),
    # Format the y-axis text 
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16, color = "gray50"),
    # Exclude x-axis labels because they are in legend
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Format the legend
    legend.position = "top", 
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ) 

ggsave(filename = here(path_plots, "tubbs_wd_boxplot_legend-top.png"), 
       width = 14, 
       height = 5.5)
  
#   Legend to the right of the plot -----
tubbs_wd %>%
  ggplot(aes(x = year, 
             y = value, 
             fill = as_factor(str_wrap(lab_year_abbr, 15)))) +
  geom_boxplot(outlier.size = 0.8) +
  theme_minimal() + 
  labs(y = "Mean fuel load (metric tons per hectare)", 
       fill = "Year") + 
  facet_wrap(~lab_fuel,  scales = "free", nrow = 1) + 
  theme( 
    # Format the facet strip
    panel.spacing = unit(1.2, "lines"),
    panel.border = element_rect(fill = NA, color = "gray91", size = 2),
    panel.grid = element_line(color = "white"),
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "gray91", color = "gray91"),
    axis.line = element_line(color = "black"),
    # Format the y-axis text 
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16, color = "gray50"),
    # Exclude x-axis labels because they are in legend
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Format the legend
    legend.position = "right", 
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, color = "gray30"), 
    legend.spacing.y = unit(0.5, 'cm'))  +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = colors_year) 

ggsave(filename = here(path_plots, "tubbs_wd_boxplot_legend-right.png"), 
       width = 16, 
       height = 5)



