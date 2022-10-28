# ========================================================== -----
# STATISTICAL TESTS ----
# ---------------------------------------------------------- -----
# Conduct tests ----
# REQUIRES: fxn_digit ----
# fxn_digit <- function(x){
#   as.numeric(format(round(x, 3), nsmall = 3))
# }
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
fxn_posthoc_plot_bt <- function(index_subset, index_input){
  
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
              y = "value_raw", 
              fill = "lab_year_abbr", 
              palette = colors_year, 
              outlier.size = 0.3,
              width = 0.6) +
    theme(legend.position = "top", 
          legend.text=element_text(size=8),
          axis.text = element_text(size = 8))  + 
    labs(title = title, 
         fill = "Year", 
         x = "Year",
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
