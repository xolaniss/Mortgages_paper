# Description
# Initial data clean-up 21 August 2023 - Xolani Sibande

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
data <- read_csv(here("Data", "danie_data.csv"), na = c("None"))

# Cleaning -----------------------------------------------------------------
mortgage_rate_tbl <- 
  data %>% 
  rename("Date" = Period) %>% 
  drop_na() %>% 
  mutate(`HH new flex%` = str_replace_all(`HH new flex%`,"DivZero", "NA")) %>% 
  mutate(`HH new flex%` = as.numeric(`HH new flex%`)) %>% 
  fill(`HH new flex%`, .direction = "up") %>% 
  mutate(Date = parse_date_time(Date, orders = "ym")) %>% 
  slice(-1)

glimpse(mortgage_rate_tbl)

# EDA ---------------------------------------------------------------
mortgage_rate_tbl %>% skim()

# Graphing ---------------------------------------------------------------
mortgage_rate_gg <- 
  mortgage_rate_tbl %>% 
  fx_plot(variables_color = 18)

mortgage_rate_gg 
# Export ---------------------------------------------------------------
artifacts_mortgage_rates <- list (
  mortgage_rate_tbl = mortgage_rate_tbl,
  mortgage_rate_gg = mortgage_rate_gg
)

write_rds(artifacts_mortgage_rates, file = here("Outputs", "artifacts_mortgage_rates.rds"))


