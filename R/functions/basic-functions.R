# %nin% ----
"%nin%" <- Negate("%in%")
# fxn_digit ----
fxn_digit <- function(x){
  as.numeric(format(round(x, 3), nsmall = 3))
}
# fxn_signif ----
fxn_signif <- function(df){
  df %>%
    mutate(p_adj_sig = case_when(
      p_adj < 0.001 ~ "***", 
      p_adj > 0.001 & p_adj < 0.01 ~ "**", 
      p_adj > 0.01 & p_adj < 0.05 ~ "*", 
      p_adj > 0.05 ~ "n.s.")) %>%
    arrange(p_adj)
  
}
# fxn_kable ----
fxn_kable <- function(df){
  
  require(knitr)
  require(kableExtra)
  
  df  %>%
    knitr::kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = F,  
                  position = "left", 
                  fixed_thead = T)
}
