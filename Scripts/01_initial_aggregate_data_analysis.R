# Description
# Initial aggregate data clean-up 20 May 2023 - Xolani Sibande

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
library(janitor)

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
data <- read_csv(here("Data", "danie_data.csv"), na = c("None"))

# Cleaning -----------------------------------------------------------------
mortgage_shares_tbl <- 
  data %>% 
  rename("Date" = Period) %>% 
  drop_na() %>% 
  mutate(`HH new flex%` = str_replace_all(`HH new flex%`,"DivZero", "NA")) %>% 
  mutate(`HH new flex%` = as.numeric(`HH new flex%`)) %>% 
  fill(`HH new flex%`, .direction = "up") %>% # some values in new flex balance are zeros for some reason. Fill up to compensate
  mutate(Date = parse_date_time(Date, orders = "ym")) %>% 
  slice(-1) %>% 
  clean_names()

household_mortgage_shares_tbl <- 
  mortgage_shares_tbl %>%
  dplyr::select(date, repo_rate, starts_with("hh"))

corporate_mortgage_shares_tbl <- 
  mortgage_shares_tbl %>% 
  dplyr::select(date, repo_rate, starts_with("corp"))

# EDA ---------------------------------------------------------------
mortgage_shares_tbl %>% skim()

# Graphing ---------------------------------------------------------------
mortgage_shares_gg <- 
  mortgage_shares_tbl %>% 
  rename(Date = date) %>%
  fx_plot(variables_color = 18)

# Export ---------------------------------------------------------------
artifacts_mortgage_shares <- list (
  data = list(
    mortgage_shares_tbl = mortgage_shares_tbl,
    household_mortgage_shares_tbl = household_mortgage_shares_tbl,
    corporate_mortgage_shares_tbl = corporate_mortgage_shares_tbl
  ),
  gg = list(
    mortgage_shares_gg = mortgage_shares_gg
  )
)

write_rds(artifacts_mortgage_shares, file = here("Outputs", "artifacts_mortgage_shares.rds"))


