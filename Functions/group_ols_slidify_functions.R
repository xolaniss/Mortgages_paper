ols_slidify_models_crisis <-
  function(data, window = 250){
    rolling_reg_spec <-
      slidify(
        .f =  ~coeftest(lm(..1 ~ ..2 + ..3 + ..4 + ..5)),
        .period = window,
        .align = "right",
        .unlist = FALSE,
        .partial = FALSE
      )
    
    data %>% 
      group_by(Category) %>% 
      mutate(models = rolling_reg_spec(CSAD, 
                                        dummy_abs, 
                                        anti_dummy_abs,
                                        dummy_squared,
                                        anti_dummy_squared )) %>% 
      unnest_rol_col_crisis(rol_column = models)
  }

ols_slidify_models_standard <-
function(data, window = 250){
  rolling_reg_spec <-
    slidify(
      .f =  ~coeftest(lm(..1 ~ ..2 + ..3)),
      .period = window,
      .align = "right",
      .unlist = FALSE,
      .partial = FALSE
    )
  
  data %>% 
    group_by(Category) %>% 
    mutate(models = rolling_reg_spec(CSAD, abs(`Market Return`), I(`Market Return` ^ 2))) %>% 
    unnest_rol_col_standard(rol_column = models)
}
unnest_rol_col_standard <-
function(data, rol_column) {
  data %>% 
    mutate(tidy = map({{ rol_column }}, broom::tidy)) %>% 
    unnest(tidy) %>% 
    dplyr::select(Date, term:estimate, statistic) %>% 
    drop_na() %>% 
    pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
    dplyr::rename("a0" = `estimate_(Intercept)`,
                  "a1" = `estimate_..2`,
                  "a2" = `estimate_..3`,
                  "t-statistic a0" = `statistic_(Intercept)`,
                  "t-statistic a1" = `statistic_..2`,
                  "t-statistic a2" = `statistic_..3` ) # may delete depending on number of variables or focus
}
unnest_rol_col_crisis <-
  function(data, rol_column) {
    data %>% 
      mutate(tidy = map({{ rol_column }}, broom::tidy)) %>% 
      unnest(tidy) %>% 
      dplyr::select(Date, term:estimate, statistic) %>% 
      drop_na() %>% 
      pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
      dplyr::rename("a0" = `estimate_(Intercept)`,
                    "a1" = `estimate_..2`,
                    "a2" = `estimate_..3`,
                    "a3" = `estimate_..4`,
                    "a4" = `estimate_..5`,
                    "t-statistic a0" = `statistic_(Intercept)`,
                    "t-statistic a1" = `statistic_..2`,
                    "t-statistic a2" = `statistic_..3`,
                    "t-statistic a3" = `statistic_..4`,
                    "t-statistic a4" = `statistic_..5`) # may delete depending on number of variables or focus
  }
fx_recode_prep_standard <-
function(data){
  data %>% 
    pivot_longer(c(-Date, -Category), names_to = "Series", values_to = "Value") %>% 
    mutate(Series = dplyr::recode(
      Series,
      "a0" = "alpha",
      "a1" = "gamma[1]",
      "a2" = "gamma[2]",
      "t-statistic a0" = "t-statistic:alpha",
      "t-statistic a1" = "t-statistic:gamma[1]",
      "t-statistic a2" = "t-statistic:gamma[2]"
    ))
}
fx_recode_prep_crisis <-
  function(data){
    data %>% 
      pivot_longer(c(-Date, -Category), names_to = "Series", values_to = "Value") %>% 
      mutate(Series = dplyr::recode(
        Series,
        "a0" = "alpha",
        "a1" = "gamma[1]",
        "a2" = "gamma[2]",
        "a3" = "gamma[3]",
        "a4" = "gamma[4]",
        "t-statistic a0" = "t-statistic:alpha",
        "t-statistic a1" = "t-statistic:gamma[1]",
        "t-statistic a2" = "t-statistic:gamma[2]",
        "t-statistic a3" = "t-statistic:gamma[3]",
        "t-statistic a4" = "t-statistic:gamma[4]"
      ))
  }
fx_recode_plot <-
function (data, plotname = " ", variables_color = 12, col_pallet = "Cascades",  ncol = NULL, nrow = NULL) {
    ggplot(
      data,
      aes(x = Date, y = Value, color = Category, group = Category)
    ) +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", labeller = label_parsed, ncol, nrow) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8),
        legend.position = "bottom"
      ) +
      labs(x = "", y = plotname, color = NULL) +
      scale_color_manual(values = pnw_palette(col_pallet, variables_color))
  }
slidyfy_gg_workflow_standard <-
function(data_model_rol){
  data_model_rol %>% 
    fx_recode_prep_standard() %>% 
    fx_recode_plot(variables_color = 6)
}
slidyfy_gg_workflow_crisis <-
  function(data_model_rol){
    data_model_rol %>% 
      fx_recode_prep_crisis() %>% 
      fx_recode_plot(variables_color = 6)
  }
