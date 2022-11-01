# ========================================================== -----
# STATISTICAL TESTS ----
# ---------------------------------------------------------- -----
# Conduct tests ----
# REQUIRES: fxn_digit ----
# fxn_digit <- function(x){
#   as.numeric(format(round(x, 3), nsmall = 3))
# }
# Functions to subset and rename data frames ----
fxn_subset_aov <- function(df, index_value, index_id, index_time){
  
  df %>%
    select(value = all_of(index_value), 
           sample_id = all_of(index_id), 
           time = all_of(index_time))

}
fxn_subset_aov2 <- function(df, index_value, index_id, index_time, index_variable){
  
  df %>%
    select(value = all_of(index_value), 
           sample_id = all_of(index_id), 
           time = all_of(index_time),
           variable = all_of(index_variable))
  
}
# Functions to conduct statistical tests ----
#   fxn_aov_me ----
fxn_aov_me <- function(df, index_value, index_id, index_time){
  
  df %>% 
    fxn_subset_aov(index_value = index_value, 
                   index_id = index_id, 
                   index_time = index_time) %>%
    anova_test(dv = value, 
               wid = sample_id, 
               within = time) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "me", 
           index_value = index_value, 
           index_id = index_id, 
           index_time = index_time) %>%
    rename(statistic = f) %>% 
    select(method, 
           effect, 
           starts_with("p_adj"), 
           statistic,
           starts_with("d"), 
           ges, 
           starts_with("index"))
}


#   fxn_pwc ----
# Excludes fuel_class, is for pwc of fuel_type only
fxn_pwc <- function(df, index_value, index_id, index_time){
  
  df %>%
    fxn_subset_aov(index_value = index_value, 
                   index_id = index_id, 
                   index_time = index_time) %>%
    pairwise_t_test(
      value ~ time, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           statistic = fxn_digit(statistic), 
           index_value = index_value, 
           index_id = index_id, 
           index_time = index_time) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(method, 
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"), 
           starts_with("index"))
  
}

#   fxn_aov2_me ----
fxn_aov2_me <- function(df, index_value, index_id, index_time, index_variable){
  
  df %>%
    fxn_subset_aov2(index_value = index_value, 
                    index_id = index_id, 
                    index_time = index_time,
                    index_variable = index_variable) %>%
    anova_test(dv = value, 
               wid = sample_id, 
               within = c(time, variable)) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni") %>%
    as_tibble() %>%
    clean_names() %>%
    mutate(method = "aov2", 
           index_value = index_value, 
           index_id = index_id, 
           index_time = index_time,
           index_variable = index_variable)  %>%
    rename(statistic = f) %>%
    select(method, 
           effect, 
           starts_with("p_adj"), 
           statistic,
           starts_with("d"), 
           ges,  
           starts_with("index"))

}


#   fxn_pwc2 ----
# Includes a column for fuel_class
fxn_pwc2 <- function(df, index_value, index_id, index_time, index_variable){
  
  df %>%
    fxn_subset_aov2(index_value = index_value, 
                    index_id = index_id, 
                    index_time = index_time,
                    index_variable = index_variable) %>%
    group_by(variable) %>%
    pairwise_t_test(
      value ~ time, 
      paired = TRUE,
      p.adjust.method = "bonferroni") %>%
    clean_names() %>%
    mutate(method = "pwc", 
           index_value = index_value, 
           index_id = index_id, 
           index_time = index_time,
           index_variable = index_variable, 
           statistic = fxn_digit(statistic)) %>%
    rename(p_adj_sig = p_adj_signif) %>%
    select(index_variable, 
           variable, 
           method,
           starts_with("group"),
           starts_with("p_adj"), 
           statistic,
           starts_with("d"),  
           starts_with("index"))
  
}

# ---------------------------------------------------------- -----
# PLot test results (by year) ----
# REQUIRES: colors_year ----
# colors_year <- c("gray70","#C47E61", "#dbae84", "#e9d097")

#   fxn_posthoc_plot ----
fxn_posthoc_plot <- function(index_subset, index_input){
  
  # Create annotations for boxplot ----
  annotation <- 
    index_input %>%
    filter(fuel_class %in% index_subset) %>%
    pairwise_t_test(value ~ year, paired = TRUE, p.adjust.method = "bonferroni")  %>%
    add_xy_position(x = "year") 
  
  title <- 
    index_input %>%
    filter(fuel_class %in% index_subset) %>%
    distinct(lab_fuel) %>%
    pull()
  
  label_y <- 
    if(index_subset %in% c("hr0001", "hr0010", "hr0100", "hr1000r", "hr1000s")){
      "Mean tons per acre"
    }else{
      "Mean depth in inches"
    }
  
  # Create boxplot comparing years  ----
  index_input %>%
    filter(fuel_class %in% index_subset) %>%
    ggboxplot(x = "year", 
              y = "value", 
              fill = "lab_year_abbr", 
              palette = colors_year, 
              outlier.size = 0.3,
              width = 0.6) +
    labs(title = title, 
         fill = "Year", 
         y = label_y) +
    theme(axis.title.x = element_blank(), 
          legend.position = "top")  + 
    # Add p-values
    stat_pvalue_manual(annotation, tip.length = 0, hide.ns = TRUE) 
  
}
#   fxn_posthoc_plot_bt ----
# index_subset <- "litter"
# index_time <- "timing"
# index_value <- "value_tran"

fxn_posthoc_plot_bt <- function(index_subset, index_input, index_time, index_value){
  
  subset <- 
    index_input %>%
    filter(fuel_class %in% index_subset) %>%
    select(time = all_of(index_time), 
           value = all_of(index_value), 
           fuel_class, 
           plot_id, 
           starts_with("lab")) 
    
  # Create annotations for boxplot ----
  annotation <- 
    subset %>%
    pairwise_t_test(value ~ time, paired = TRUE, p.adjust.method = "bonferroni")  %>%
    add_xy_position(x = "time") 
  
  title <- 
    subset %>%
    distinct(lab_fuel) %>%
    pull()
  
  label_y <- 
    if(index_subset %in% c("hr0001", "hr0010", "hr0100", "hr1000r", "hr1000s")){
      "Mean fuel load (metric tons per hectare"
    }else{
      "Mean fuel depth (centimeters)"
    }
  
  # Create boxplot comparing years  ----
  subset %>%
    ggboxplot(x = "time", 
              y = "value", 
              fill = "time", 
              # palette = colors_year, 
              outlier.size = 0.5,
              width = 0.6) +
    theme(legend.position = "right",
          axis.title.x = element_blank()) +
          # legend.text=element_text(size=12),
          # axis.text = element_text(size = 12))  + 
    labs(title = title, 
         fill = "Time", 
         x = "Time",
         y = label_y) +
    # Add p-values
    stat_pvalue_manual(annotation, tip.length = 0, hide.ns = TRUE) 
  
}

# ---------------------------------------------------------- -----
# Summarize results (by year) ----
# REQUIRES: fxn_kable ----
# fxn_kable <- function(df){
#   
#   require(knitr)
#   require(kableExtra)
#   
#   df  %>%
#     knitr::kable() %>%
#     kable_styling(bootstrap_options = c("striped", "hover"), 
#                   full_width = F,  
#                   position = "left", 
#                   fixed_thead = T)
# }
#   fxn_posthoc_table ----
fxn_posthoc_table <- function(index_subset, index_input, index_value, index_id, index_time){
  
  title <- 
    index_input %>%
    filter(fuel_class %in% index_subset) %>%
    distinct(lab_fuel) %>%
    pull()
  
  index_input %>%
    filter(fuel_class %in% index_subset) %>%
    fxn_pwc(df = index_input, 
            index_value = index_value, 
            index_id = index_id, 
            index_time = index_time) %>%
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
