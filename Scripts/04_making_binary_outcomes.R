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
pivot_function <- function(tbl) {
  tbl %>% 
    pivot_longer(
      cols = -c("Date", "Banks", "Sector"),
      names_to = "Series",
      values_to = "Value"
    )
}
binary <- function(data, threshold, varname, variable){
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
binary_plot <- function(data){
  data %>% 
    mutate(Fixed_share_binary = factor(Fixed_share_binary, levels = c("No", "Yes"))) %>% 
    # plot
    ggplot(aes(x = Fixed_share_binary)) +
    geom_bar(fill = "black") +
    labs(
      title = "Fixed share binary distribution",
      x = "",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
}

# Import -------------------------------------------------------------
mortgage_split <- read_rds(here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))
mortgage_split_tbl <- mortgage_split$mortgage_split_tbl %>% pivot_function()
housing_split_tbl <- mortgage_split$household_mortgage_split_tbl %>% pivot_function()
corporate_split_tbl <- mortgage_split$corporate_mortgage_split_tbl %>% pivot_function()

# Household binaries --------------------------------------------------------
housing_split_binary_tbl <- 
  housing_split_tbl %>% 
  binary(threshold = 10, varname = "Fixed_share_binary", variable = "Fixed_share")
  
    
housing_split_binary_tbl %>% 
  filter(!Fixed_share_binary %in% c("Yes", "No")) %>% 
  dplyr::select(Date, Banks, Sector, Fixed_share, Fixed_share_binary) # test for NAs

housing_split_binary_gg <- 
  housing_split_binary_tbl %>%    
  binary_plot() +
  ggtitle("Household fixed share binary distribution")
  
housing_split_binary_gg

# Corporate binaries --------------------------------------------------------
corporate_split_binary_tbl <- 
  corporate_split_tbl %>% 
  binary(threshold = 10, varname = "Fixed_share_binary", variable = "Fixed_share")

corporate_split_binary_tbl %>%
  filter(!Fixed_share_binary %in% c("Yes", "No")) %>% 
  dplyr::select(Date, Banks, Sector, Fixed_share, Fixed_share_binary) # test for NAs

corporate_split_binary_gg <-
  corporate_split_binary_tbl %>%    
  binary_plot() +
  ggtitle("Corporate fixed share binary distribution")

corporate_split_binary_gg

# Combined ----------------------------------------------------------------
combined_gg <- 
  housing_split_binary_gg + 
  corporate_split_binary_gg +
  plot_annotation(tag_levels = 'A')


# Export ---------------------------------------------------------------
artifacts_binary <- list (
  data = list(
    housing_split_binary_tbl = housing_split_binary_tbl,
    corporate_split_binary_tbl = corporate_split_binary_tbl
    ),
  plots = list(
    housing_split_binary_gg = housing_split_binary_gg,
    corporate_split_binary_gg = corporate_split_binary_gg,
    combined_gg = combined_gg
  )
)

write_rds(artifacts_binary, file = here("Outputs", "BA930", "artifacts_binary.rds"))


