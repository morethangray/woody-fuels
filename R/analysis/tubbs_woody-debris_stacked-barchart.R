# title: Create stacked barchart for downed woody debris
# revised: 2022-09-28 ----
# ========================================================== -----
# CONFIGURE SETTINGS -----
# Load libraries
rm(list = ls())
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(lubridate)   ## To work with dates and times
library(janitor)   ## To tidy data frames

# Set file paths
# path_raw <- here("raw_data")
path_in <- here("input")
path_out <- here("output/2022/oak-symposium/plots")

# Write functions 
"%nin%" <- Negate("%in%")
# ========================================================== -----
# CREATE DATA FRAMES -----
# Read data for plot-level mean by fuel class -----
# Create helper to order the fuel classes for the plot
ord_fuel <- 
  read_csv(here(path_in, "lookup_variables.csv")) %>%
  filter(metric %in% "fuel", 
         data_type %in% "dw", 
         subset %in% "fuel_class") %>%
  select(fuel_class = name, 
         ord_fuel = order)

# Read the data for mean by plot x year x fuel class 
# See oak-symposium_prepare-data.R for the calculations used to create these data
dw_class_input <-
  read_csv(here(path_in, "fuel_mean-by-plot-yr-type-class_2022.csv")) %>%
  filter(data_type == "dw") %>%
  arrange(year) %>%
  mutate(year = as_factor(year), 
         lab_year = as_factor(lab_year), 
         lab_year_abbr = as_factor(lab_year_abbr)) %>%
  relocate(c(statistic, subset), .after = value)   %>%
  left_join(ord_fuel, "fuel_class") %>%
  arrange(year, ord_fuel) %>%
  mutate(fuel_class = as_factor(fuel_class), 
         lab_year = as_factor(lab_year), 
         lab_fuel = as_factor(lab_fuel),
         year = as_factor(year))

# ========================================================== -----
# CREATE SUMMARIES  ----
# Calculate summary statistics by fuel class x year -----
# Create the input data for the stacked bar chart
dw_class_summary <- 
  dw_class_input %>% 
  group_by(fuel_class, 
           lab_fuel,
           year, 
           lab_year) %>%
  summarize(total = sum(value), 
            mean = mean(value), 
            sd = sd(value), 
            n = n()) %>%
  mutate(se = sd/(sqrt(n))) %>%
  ungroup() 

# Reshape the summary into a table to accompany the figure ----
# Create a helper to order the statistics in the table
ord_stat <- tibble(statistic = c("total", "mean", "sd", "n"), ord_stat = 1:4)

dw_class_summary %>%
  gather(statistic, value, total:n) %>%
  select(fuel_class, lab_fuel, year, statistic, value) %>%
  spread(year, value) %>%
  left_join(ord_stat, "statistic") %>%
  arrange(fuel_class, ord_stat) %>%
  select(-fuel_class, -ord_stat) %>%
  write_csv(here(path_out, "fuel_dwd_by-class_summary-table.csv"))

# Create a stacked bar chart  ----
# Define plot colors  
stacked_colors <- c("gray25", 
                    "#5c5c5c",
                    "#858585",
                    "#adadab",
                    "gray85")

# Downed woody debris, stacked by size class 
dw_class_summary %>% 
  ggplot(aes(x = lab_year, 
             y = mean, 
             fill = lab_fuel)) +
  geom_bar(stat = "identity",
           width = 0.5, 
           position = "stack") +
  theme_minimal() +
  ggtitle("Fuel load by size class") + 
  # Legend is turned off 
  #guides(fill = "none") +
  xlab("Chronosequence") + 
  ylab("Mean fuel load (tons/acre)") + 
  theme(plot.title = element_text(margin = margin(b = 0)), 
        legend.position = "right") +
  scale_fill_manual(values = stacked_colors, 
                    name = "Size class") 

# Save as .png
ggsave(here(path_out, 
            paste0("fuel_dwd_stacked-barchart_", 
                   Sys.Date(), 
                   ".png")), 
       width = 8, 
       height = 3)

