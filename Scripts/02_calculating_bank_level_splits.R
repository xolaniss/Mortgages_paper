# Description
# Calculating fixed vs flexible shares of mortgage lending for each bank - May 2024 Xolani Sibande
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

# Import -------------------------------------------------------------
mortgage_lending <- read_rds(here("Outputs", "BA930", "artifacts_BA930_mortgages.rds"))
mortgage_lending_tbl <- mortgage_lending$mortgage_lending_tbl

# Filtering Banks with less than 50% completion rate on outstanding balance --------
mortgage_lending_tbl_skim <- mortgage_lending_tbl %>% group_by(Banks) %>% skim() 

remaing_banks_vec <- 
  mortgage_lending_tbl_skim %>% 
  filter(skim_variable == "Outstanding balance at month end R'000") %>% 
  filter(complete_rate > 0.5 ) %>% 
  dplyr::select(Banks)

mortgage_lending_filtered_tbl <- 
  mortgage_lending_tbl %>% 
  filter(Banks %in% remaing_banks_vec$Banks)

# Calculating bank level fixed vs flexible % --------------------------------




# Splitting households and corporates -------------------------------------
household_mortgage_lending_filtered_tbl <- 
  mortgage_lending_filtered_tbl %>% 
  filter(Sector == "Household sector")

corporate_mortgage_lending_filtered_tbl <-
  mortgage_lending_filtered_tbl %>% 
  filter(Sector == "Corporate sector")


# Export ---------------------------------------------------------------
artifacts_bank_level_shares <- list (
  mortgage_lending_filtered_tbl = mortgage_lending_filtered_tbl
)

write_rds(artifacts_bank_level_shares, 
          file = here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))


