#   %nin%: Negates in ----
"%nin%" <- Negate("%in%")
#   substrRight: Subset character string from the right ----
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#   fxn_digit: Round to 3 digits ----
fxn_digit <- function(x){
  as.numeric(format(round(x, 3), nsmall = 3))
}
#   fxn_signif_adj: Add significance stars based on p-value ----
fxn_signif_adj <- function(df){
  df %>%
    mutate(p_adj_sig = case_when(
      p_adj <= 0.001 ~ "***", 
      p_adj > 0.001 & p_adj <= 0.01 ~ "**", 
      p_adj > 0.01 & p_adj <= 0.05 ~ "*", 
      p_adj > 0.05 ~ "n.s.")) %>%
    arrange(p_adj)
  
}
#   fxn_kable: Format table for markdown ----
fxn_kable <- function(df){
  
  require(knitr)
  require(kableExtra)
  
  df  %>%
    knitr::kable() %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), 
                  full_width = F,  
                  position = "left", 
                  fixed_thead = T)
}
