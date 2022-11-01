# Data transformation (normalize and standardize) ----
# REQUIRES: list_classes ----
#   fxn_tranform_ordnorm ----
fxn_tranform_ordnorm <- function(index_data, index_list){
  
  datalist_normalized <- list()
  for(index_class in index_list){
    
    subset <- 
      index_data %>%
      filter(fuel_class %in% index_class) %>%
      rename(value_raw = value) 
    
    bn_o <- bestNormalize::orderNorm(subset$value_raw, standardize = TRUE)
    
    datalist_normalized[[index_class]] <- 
      subset %>%
      mutate(value_tran = bn_o$x.t,
             transform = "ordernorm") %>%
      relocate(value_tran, .after = value_raw) %>%
      relocate(transform, .after = value_tran) 
    
  }
  
  bind_norm <- do.call(bind_rows, datalist_normalized)
  
}

#   fxn_transform_arcsine ----
fxn_transform_arcsine <- function(index_data, index_list){
  
  subset <- 
    index_data %>%
    filter(fuel_class %in% index_list)  %>%
    rename(value_raw = value) 
  
  bn <- bestNormalize::arcsinh_x(subset$value_raw, standardize = TRUE)
  
  normalized <-
    subset %>%
    mutate(value_tran = bn$x.t, 
           transform = "arcsine") %>%
    relocate(value_tran, .after = value_raw) %>%
    relocate(transform, .after = value_tran) 
}
#   fxn_transform_log ----
fxn_transform_log <- function(index_data, index_list){
  
  subset <- 
    index_data %>%
    filter(fuel_class %in% index_list)  %>%
    rename(value_raw = value) 
  
  bn <- bestNormalize::log_x(subset$value_raw, standardize = TRUE)
  
  normalized <-
    subset %>%
    mutate(value_tran = bn$x.t, 
           transform = "log") %>%
    relocate(value_tran, .after = value_raw) %>%
    relocate(transform, .after = value_tran) 
}
# Evaluate multiple tranformations ----
#   fxn_tranform_eval ----
fxn_tranform_eval <- function(index_data, index_list){
  
  datalist_normalized <- list()
  for(index_class in index_list){
    
    subset <- 
      index_data %>%
      filter(fuel_class %in% index_class) %>%
      rename(value_raw = value) 
    
    bn_a <- bestNormalize::arcsinh_x(subset$value_raw, standardize = TRUE)
    bn_l <- bestNormalize::log_x(subset$value_raw, standardize = TRUE)
    bn_o <- bestNormalize::orderNorm(subset$value_raw, standardize = TRUE)
    bn_s <- bestNormalize::sqrt_x(subset$value_raw, standardize = TRUE)
    
    datalist_normalized[[index_class]] <- 
      subset %>%
      mutate(value_std = as.vector(scale(value_raw)), 
             value_arcsine = bn_a$x.t, 
             value_log = bn_l$x.t, 
             value_ordnorm = bn_o$x.t, 
             value_sqrt = bn_s$x.t) %>%
      select(fuel_class, timing, plot_id, starts_with("value")) 
    
  }
  
  bind_norm <- do.call(bind_rows, datalist_normalized)
  
}

