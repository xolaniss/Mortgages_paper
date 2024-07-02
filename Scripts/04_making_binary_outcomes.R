# Description
# Categorising into binary outcomes - Xolani Sibande June 2024
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
source(here("Functions", "pivot.R"))
source(here("Functions", "binary.R"))
source(here("Functions", "binary_plot.R"))


# Import -------------------------------------------------------------
mortgage_split <- read_rds(here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))
mortgage_split_tbl <- mortgage_split$mortgage_split_tbl %>% pivot_function()
housing_split_tbl <- mortgage_split$household_mortgage_split_tbl %>% pivot_function()
corporate_split_tbl <- mortgage_split$corporate_mortgage_split_tbl %>% pivot_function()


# Fixed binaries ----------------------------------------------------------

## Household binaries --------------------------------------------------------
housing_split_fixed_binary_tbl <- 
  housing_split_tbl %>% 
  binary(threshold = 10, varname = "Fixed_share_binary", variable = "Fixed_share")
  
    
housing_split_fixed_binary_tbl %>% 
  filter(!Fixed_share_binary %in% c("Yes", "No")) %>% 
  dplyr::select(Date, Banks, Sector, Fixed_share, Fixed_share_binary) # test for NAs

housing_split_fixed_binary_gg <- 
  housing_split_fixed_binary_tbl %>%    
  binary_plot(type = "Fixed") +
  ggtitle("Household FRM share binary distribution")
  
housing_split_fixed_binary_gg

## Corporate binaries --------------------------------------------------------
corporate_split_fixed_binary_tbl <- 
  corporate_split_tbl %>% 
  binary(threshold = 20, varname = "Fixed_share_binary", variable = "Fixed_share")

corporate_split_fixed_binary_tbl %>%
  filter(!Fixed_share_binary %in% c("Yes", "No")) %>% 
  dplyr::select(Date, Banks, Sector, Fixed_share, Fixed_share_binary) # test for NAs

corporate_split_fixed_binary_gg <-
  corporate_split_fixed_binary_tbl %>%    
  binary_plot(type = "Fixed") +
  ggtitle("Corporate fixed share binary distribution")

corporate_split_fixed_binary_gg

## Combined ----------------------------------------------------------------
combined_fixed_gg <- 
  housing_split_fixed_binary_gg + 
  corporate_split_fixed_binary_gg +
  plot_annotation(tag_levels = 'A')

combined_fixed_gg

# Flexible binaries ----------------------------------------------------------

## Household binaries --------------------------------------------------------
housing_split_flexible_binary_tbl <- 
  housing_split_tbl %>% 
  binary(threshold = 20, varname = "Flexible_share_binary", variable = "Flexible_share")

housing_split_flexible_binary_gg <- 
  housing_split_flexible_binary_tbl %>%    
  binary_plot(type = "Flexible") +
  ggtitle("Household ARM share binary distribution")

housing_split_flexible_binary_gg

## Corporate binaries --------------------------------------------------------
corporate_split_flexible_binary_tbl <- 
  corporate_split_tbl %>% 
  binary(threshold = 20, varname = "Flexible_share_binary", variable = "Flexible_share")

corporate_split_flexible_binary_gg <-
  corporate_split_flexible_binary_tbl %>%    
  binary_plot(type = "Flexible") +
  ggtitle("Corporate flexible share binary distribution")

corporate_split_flexible_binary_gg

## Combined ----------------------------------------------------------------
combined_flexible_gg <- 
  housing_split_flexible_binary_gg + 
  corporate_split_flexible_binary_gg +
  plot_annotation(tag_levels = 'A')

combined_flexible_gg

# Combined ----------------------------------------------------------------
combined_gg <- combined_fixed_gg /
  combined_flexible_gg +
  plot_annotation(tag_levels = 'A')

combined_gg
# Export ---------------------------------------------------------------
artifacts_binary <- list (
  data = list(
    housing_split_fixed_binary_tbl = housing_split_fixed_binary_tbl,
    corporate_split_fixed_binary_tbl = corporate_split_fixed_binary_tbl,
    housing_split_flexible_binary_tbl = housing_split_flexible_binary_tbl,
    corporate_split_flexible_binary_tbl = corporate_split_flexible_binary_tbl
    ),
  plots = list(
    housing_split_fixed_binary_gg = housing_split_fixed_binary_gg,
    corporate_split_fixed_binary_gg = corporate_split_fixed_binary_gg,
    housing_split_flexible_binary_gg = housing_split_flexible_binary_gg,
    corporate_split_flexible_binary_gg = corporate_split_flexible_binary_gg,
    combined_fixed_gg = combined_fixed_gg,
    combined_flexible_gg = combined_flexible_gg,
    combined_gg = combined_gg
  )
)

write_rds(artifacts_binary, file = here("Outputs", "BA930", "artifacts_binary.rds"))


