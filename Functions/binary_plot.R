binary_plot <-
function(data, type){
  data %>% 
    mutate(!!sym(glue("{type}_share_binary")) := factor(!!sym(glue("{type}_share_binary")), levels = c("No", "Yes"))) %>% 
    # plot
    ggplot(aes(x = !!sym(glue("{type}_share_binary")))) +
    geom_bar(fill = "black") +
    labs(
      title = glue("{type} share binary distribution"),
      x = "",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
}
