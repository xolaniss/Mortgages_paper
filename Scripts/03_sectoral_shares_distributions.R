# Description -----------------------------------------------------------
# Sectoral shares distributions - May 2024 Xolani Sibande

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
pivot_function <- function(tbl) {
  tbl %>% 
    pivot_longer(
      cols = -c("Date", "Banks", "Sector"),
      names_to = "Series",
      values_to = "Value"
    )
}

distribution_gg <- function(
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

density_gg <- function(
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

# Import -------------------------------------------------------------
mortgage_split <- read_rds(here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))
mortgage_split_tbl <- mortgage_split$mortgage_split_tbl %>% pivot_function()
housing_split_tbl <- mortgage_split$household_mortgage_split_tbl %>% pivot_function()
corporate_split_tbl <- mortgage_split$corporate_mortgage_split_tbl %>% pivot_function()

# Household sector view ---------------------------------------------------
housing_fixed_hist_gg <- 
  distribution_gg(
  data = housing_split_tbl,
  fix_flex_indicator = "Fixed_share",
  year_version = FALSE,
  title_tag = "household",
  ylabel = "Log(FRM share)"
)

housing_fixed_hist_gg

housing_fixed_hist_by_year_gg <- 
  distribution_gg(
    data = housing_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = TRUE,
    title_tag = "household",
    ylabel = "Log(FRM share)"
  )

housing_fixed_hist_by_year_gg 

# Corporate sector view ---------------------------------------------------
corporate_fixed_hist_gg <- 
  distribution_gg(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = FALSE,
    title_tag = "corporate"
  )

corporate_fixed_hist_gg

corporate_fixed_hist_by_year_gg <- 
  distribution_gg(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = TRUE,
    title_tag = "corporate",
    ylabel = "Log(FRM share)"
  )

corporate_fixed_hist_by_year_gg

# Density estimates -------------------------------------------------------
housing_density_gg <- 
  density_gg(
    data = housing_split_tbl,
    fix_flex_indicator = "Fixed_share",
    title_tag = "household",
    xlabel = as.expression(quote(log_10(FRM_share)))
  ) 
housing_density_gg

housing_density_year_gg <- 
  density_gg(
  data = housing_split_tbl,
  fix_flex_indicator = "Fixed_share",
  title_tag = "household",
  year_version = TRUE
)
housing_density_year_gg

corporate_density_gg <- 
  corporate_split_tbl %>% 
  density_gg(
    fix_flex_indicator = "Fixed_share",
    title_tag = "corporate"
  )
corporate_density_gg

corporate_density_year_gg <-
  density_gg(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    title_tag = "corporate",
    year_version = TRUE
  )
corporate_density_year_gg

# Export ---------------------------------------------------------------
artifacts_distributions <- list (
  histograms = list(
    housing_fixed_hist_gg = housing_fixed_hist_gg,
    housing_fixed_hist_by_year_gg = housing_fixed_hist_by_year_gg,
    corporate_fixed_hist_gg = corporate_fixed_hist_gg,
    corporate_fixed_hist_by_year_gg = corporate_fixed_hist_by_year_gg
  ),
  densities = list(
    housing_density_gg = housing_density_gg,
    housing_density_year_gg = housing_density_year_gg,
    corporate_density_gg = corporate_density_gg,
    corporate_density_year_gg = corporate_density_year_gg
  )
)

write_rds(artifacts_distributions, file = here("Outputs", "BA930", "artifacts_distributions.rds"))


