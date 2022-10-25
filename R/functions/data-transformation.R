# Data transformation (normalize and standardize) ----
# REQUIRES: list_classes ----
#   fxn_tranform_ordnorm ----
fxn_tranform_ordnorm <- function(){
  
  datalist_normalized <- list()
  for(index_class in list_classes){
    
    subset <- 
      input_class %>%
      filter(fuel_class %in% index_class) %>%
      rename(value_raw = value) 
    
    bn_o <- bestNormalize::orderNorm(subset$value_raw, standardize = TRUE)
    
    datalist_normalized[[index_class]] <- 
      subset %>%
      mutate(value_ordnorm = bn_o$x.t) %>%
      select(fuel_class, year, plot_id, starts_with("value"), starts_with("lab")) 
    
  }
  
  bind_norm <- do.call(bind_rows, datalist_normalized)
  
}

#   fxn_transform_arcsine ----
fxn_transform_arcsine <- function(index_class){
  
  subset <- 
    input_class %>%
    filter(fuel_class %in% index_class)  %>%
    rename(value_raw = value) 
  
  bn <- bestNormalize::arcsinh_x(subset$value_raw, standardize = TRUE)
  
  normalized <-
    subset %>%
    mutate(value = bn$x.t, 
           transform = "arcsine") %>%
    relocate(value, .after = value_raw) %>%
    relocate(transform, .after = value)
}
#   fxn_transform_log ----
fxn_transform_log <- function(index_class){
  
  subset <- 
    input_class %>%
    filter(fuel_class %in% index_class)  %>%
    rename(value_raw = value) 
  
  bn <- bestNormalize::log_x(subset$value_raw, standardize = TRUE)
  
  normalized <-
    subset %>%
    mutate(value = bn$x.t, 
           transform = "log") %>%
    relocate(value, .after = value_raw) %>%
    relocate(transform, .after = value)
  
}