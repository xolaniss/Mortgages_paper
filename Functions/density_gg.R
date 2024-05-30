density_gg <-
function(
  data = data,
  fix_flex_indicator = "Fixed_share",
  title_tag = "household",
  year_version = FALSE,
  xlabel = "Log(FRM share)"){
  gg_one <- 
  if(year_version == FALSE){
  data %>% 
    filter(Series == fix_flex_indicator) %>%
    filter(!Banks == "Total Banks") %>%
    ggplot(aes(x = Value)) +
    geom_density(fill = "yellow", alpha = 0.5) +
    scale_x_log10(labels = scales::label_log()) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("Total density plot") +
    labs(
      title = "Overall density",
      x = glue("{xlabel}"),
      y = "Density") +
    scale_color_viridis_d()
    } else {
    data %>% 
    filter(Series == fix_flex_indicator) %>%
    filter(!Banks == "Total Banks") %>% 
    ggplot(aes(x = Value)) +
    geom_density(fill = "yellow", alpha = 0.5) +
    scale_x_log10(labels = scales::label_log()) +
    facet_wrap(~lubridate::year(Date), scale = "free", ncol = 2) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "Overall density",
      x = glue("{xlabel}"),
      y = "Density") +
      scale_color_viridis_d()
    }
  gg_two <- 
    if(year_version == FALSE){
      data %>% 
        filter(Series == fix_flex_indicator) %>%
        filter(!Banks == "Total Banks") %>%
        ggplot(aes(x = Value, color = Banks)) +
        geom_density() +
        scale_x_log10(labels = scales::label_log()) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle("Bank level density") +
        labs(
          # title = glue("Density of {title_tag} mortgage lending shares (2008-2023)"),
          x = glue("{xlabel}"),
          y = "Density") +
        scale_color_viridis_d()
    } else {
      data %>% 
        filter(Series == fix_flex_indicator) %>%
        filter(!Banks == "Total Banks") %>% 
        ggplot(aes(x = Value, color = Banks)) +
        geom_density() +
        scale_x_log10(labels = scales::label_log()) +
        facet_wrap(~lubridate::year(Date), scale = "free", ncol = 2) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle("Bank level density") +
        labs(
          # title = glue("Density of {title_tag} mortgage lending shares (2008-2023)"),
          x = glue("{xlabel}"),
          y = "Density") +
        scale_color_viridis_d()
    }
  
 patchwork <-  gg_one + gg_two 
 patchwork +
   plot_annotation(tag_levels = "A", 
                   title = glue("Density of {title_tag} mortgage lending shares (2008-2023)")
                   )
}
