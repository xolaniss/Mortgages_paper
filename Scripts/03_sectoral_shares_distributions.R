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

distribution_gg_function <- function(
    data = data,
    fix_flex_indicator = "Fixed_share",
    year_version = FALSE,
    title_tag = "household"){
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
      y = "Log(FRM share)"
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
      y = "Log(FRM share)"
    ) +
    coord_flip()
  }
}

# Import -------------------------------------------------------------
mortgage_split <- read_rds(here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))
mortgage_split_tbl <- mortgage_split$mortgage_split_tbl %>% pivot_function()
housing_split_tbl <- mortgage_split$household_mortgage_split_tbl %>% pivot_function()
corporate_split_tbl <- mortgage_split$corporate_mortgage_split_tbl %>% pivot_function()

# Household sector view ---------------------------------------------------

housing_fixed_hist_gg <- 
  distribution_gg_function(
  data = housing_split_tbl,
  fix_flex_indicator = "Fixed_share",
  year_version = FALSE,
  title_tag = "household"
)

housing_fixed_hist_gg

housing_fixed_hist_by_year_gg <- 
  distribution_gg_function(
    data = housing_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = TRUE,
    title_tag = "household"
  )

housing_fixed_hist_by_year_gg 

# Corporate sector view ---------------------------------------------------
corporate_fixed_hist_gg <- 
  distribution_gg_function(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = FALSE,
    title_tag = "corporate"
  )

corporate_fixed_hist_gg

corporate_fixed_hist_by_year_gg <- 
  distribution_gg_function(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    year_version = TRUE,
    title_tag = "corporate"
  )

corporate_fixed_hist_by_year_gg

# Export ---------------------------------------------------------------
artifacts_distributions <- list (
  gg = list(
    housing_fixed_hist_gg = housing_fixed_hist_gg,
    housing_fixed_hist_by_year_gg = housing_fixed_hist_by_year_gg,
    corporate_fixed_hist_gg = corporate_fixed_hist_gg,
    corporate_fixed_hist_by_year_gg = corporate_fixed_hist_by_year_gg
  )
)

write_rds(artifacts_distributions, file = here("Outputs", "BA930", "artifacts_distributions.rds"))


