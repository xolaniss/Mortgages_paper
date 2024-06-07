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
source(here("Functions", "density_gg.R"))
source(here("Functions", "distribution_gg.R"))
pivot_function <- function(tbl) {
  tbl %>% 
    pivot_longer(
      cols = -c("Date", "Banks", "Sector"),
      names_to = "Series",
      values_to = "Value"
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
    fix_flex_indicator = "Flexible_share",
    year_version = FALSE,
    title_tag = "corporate"
  )

corporate_fixed_hist_gg

corporate_fixed_hist_by_year_gg <- 
  distribution_gg(
    data = corporate_split_tbl,
    fix_flex_indicator = "Flexible_share",
    year_version = TRUE,
    title_tag = "corporate",
    ylabel = "Log(FRM share)"
  )

corporate_fixed_hist_by_year_gg

# Density estimates -------------------------------------------------------
housing_density_gg <- 
  density_gg(
    data = housing_split_tbl,
    fix_flex_indicator = "Flexible_share",
    title_tag = "household",
    xlabel = "FRM share"
  ) 
housing_density_gg

housing_density_year_gg <- 
  density_gg(
  data = housing_split_tbl,
  fix_flex_indicator = "Flexible_share",
  title_tag = "household",
  year_version = TRUE,
  xlabel = "FRM share"
)
housing_density_year_gg

corporate_density_gg <- 
  corporate_split_tbl %>% 
  density_gg(
    fix_flex_indicator = "Fixed_share",
    title_tag = "corporate",
    xlabel = "FRM share"
  )
corporate_density_gg

corporate_density_year_gg <-
  density_gg(
    data = corporate_split_tbl,
    fix_flex_indicator = "Fixed_share",
    title_tag = "corporate",
    year_version = TRUE,
    xlabel = "FRM share"
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


