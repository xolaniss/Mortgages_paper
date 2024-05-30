distribution_gg <-
function(
    data = data,
    fix_flex_indicator = "Fixed_share",
    year_version = FALSE,
    title_tag = "household",
    ylabel = "Log(FRM share)"){
  if(year_version == FALSE) {
  data %>% 
    filter(Series == fix_flex_indicator) %>%
    filter(!Banks == "Total Banks") %>% 
    ggplot(aes(y = log(Value), fill = Banks)) +
    geom_histogram(binwidth =  0.1) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      title = glue("Distribution of {title_tag} corporate mortgage lending shares (2008-2023)"),
      x = "Count",
      y = glue("{ylabel}")
    ) +
    coord_flip()
  } else {
    data %>% 
    filter(Series == fix_flex_indicator) %>%
    filter(!Banks == "Total Banks") %>% 
    ggplot(aes(y = log(Value), fill = Banks)) +
    geom_histogram(binwidth =  0.1) +
    theme_minimal() +
    facet_wrap(~lubridate::year(Date)) +
    theme(legend.position = "bottom") +
    labs(
      title = glue("Distribution of {title_tag} mortgage lending shares (2008-2023)"),
      x = "Count",
      y = glue("{ylabel}")
    ) +
    coord_flip()
  }
}
