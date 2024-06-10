pivot_function <-
function(tbl) {
  tbl %>% 
    pivot_longer(
      cols = -c("Date", "Banks", "Sector"),
      names_to = "Series",
      values_to = "Value"
    )
}
