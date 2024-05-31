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

missingness <- function(data, 
                        rows_print = 100){
  data %>% 
    group_by(Banks) %>% 
    summarise(n = n(), 
              missing_values = sum(is.na(`Outstanding balance at month end R'000`), na.rm = T), 
              nan_values = sum(is.nan(`Outstanding balance at month end R'000`), na.rm = T),
              zero_values = sum(`Outstanding balance at month end R'000` == 0, na.rm = T),
              negative_values = sum(`Outstanding balance at month end R'000` < 0, na.rm = T)
    ) %>% 
    mutate(recon = (n - missing_values - zero_values - nan_values - negative_values) / n) %>%
    arrange(desc(recon)) %>% 
    print(n = rows_print)  
}

# Import -------------------------------------------------------------
mortgage_lending <- read_rds(here("Outputs", "BA930", "artifacts_BA930_mortgages.rds"))
mortgage_lending_tbl <- mortgage_lending$mortgage_lending_tbl

# Banks to keep ---------------------
missingness_tbl <- 
  mortgage_lending_tbl %>% 
  missingness()

# Keeping banks with more than 50% recon -------------------------
banks_to_keep_vec <- 
  missingness_tbl %>% 
  filter(recon > 0.49) %>% 
  pull(Banks). # 8 banks at the end

mortgage_lending_filtered_tbl <- 
  mortgage_lending_tbl %>% 
  filter(Banks %in% banks_to_keep_vec)

# Calculating bank level fixed vs. flexible % --------------------------------
mortgage_split_tbl <- 
  mortgage_lending_filtered_tbl %>% 
  pivot_wider(names_from = "Rate type", values_from = "Outstanding balance at month end R'000", 
              id_cols = c("Date", "Banks", "Sector"), names_prefix = "Balance on ", values_fill = 0) %>% 
  mutate(Fixed_share = (`Balance on Fixed rate` / (`Balance on Fixed rate` + `Balance on Flexible rate`))*100) %>%
  mutate(Flexible_share = (`Balance on Flexible rate` / (`Balance on Fixed rate` + `Balance on Flexible rate`))*100) %>% 
  mutate(Fixed_share = case_when(is.nan(Fixed_share) ~ 0, TRUE ~ Fixed_share)) %>%
  mutate(Flexible_share = case_when(is.nan(Flexible_share) ~ 0, TRUE ~ Flexible_share))

# Splitting households and corporates -------------------------------------
household_mortgage_split_tbl <- 
  mortgage_split_tbl %>% 
  filter(Sector == "Household sector")

corporate_mortgage_split_tbl <-
  mortgage_split_tbl %>% 
  filter(Sector == "Corporate sector")

# Export ---------------------------------------------------------------
artifacts_bank_level_shares <- list (
  mortgage_lending_filtered_tbl = mortgage_lending_filtered_tbl,
  mortgage_split_tbl = mortgage_split_tbl,
  household_mortgage_split_tbl = household_mortgage_split_tbl,
  corporate_mortgage_split_tbl = corporate_mortgage_split_tbl
)

write_rds(artifacts_bank_level_shares, 
          file = here("Outputs", "BA930", "artifacts_bank_level_shares.rds"))


