binary <-
function(data, threshold, varname, variable){
  data %>% 
    pivot_wider(
      names_from = Series,
      values_from = Value,
      id_cols = c(Date, Banks, Sector)
    ) %>% 
    mutate(!!sym(glue("{varname}")) := case_when(
      !!sym(glue("{variable}")) < threshold ~ "No",
      !!sym(glue("{variable}")) > threshold ~ "Yes"
    )
    )
}
