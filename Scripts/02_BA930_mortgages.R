# Description
# Creating BA930 mortgages data set for all banks - May 2024 Xolani Sibande

# Preliminaries -----------------------------------------------------------
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(tseries)
library(broom)
library(glue)
library(vars)
library(PNWColors)
library(patchwork)
library(psych)
library(kableExtra)
library(strucchange)
library(timetk)
library(pins)
library(uniqtag)
library(scales)
library(urca)
library(mFilter)
library(skimr)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "Sheets_import.R"))
source(here("Functions", "excel_import_sheet.R"))
# Import -------------------------------------------------------------
sheets <- excel_sheets(here("Data","BA930", "4.1 BA930 Multiple Bank Export (2023).xlsx"))
sheet_list <-  as.list(sheets[!sheets %in% c("VBS", "GBS", "FINBond", "Export Parameters")])
names(sheet_list) <- sheets[!sheets %in% c("VBS", "GBS", "FINBond", "Export Parameters")]

year_2008_2015 <- c(2008:2015)
path_list_2008_2015 <- 
  year_2008_2015 %>% 
  map(~ glue(
    here("Data","BA930", "4.1 BA930 Multiple Bank Export ({.}).xlsx")
  ))

mortgage_lending_2008_2015 <- 
  path_list_2008_2015 %>% 
  purrr::set_names(year_2008_2015) %>% 
  map(~excel_import_sheet(
    path = .,
    sheet_list = sheet_list,
    skip = 560,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric") 
  )) 

year_2016_2018 <- c(2016:2018)
path_list_2016_2018 <- 
  year_2016_2018 %>% 
  map(~ glue(
    here("Data","BA930", "4.1 BA930 Multiple Bank Export ({.}).xlsx")
  ))

mortgage_lending_2016_2018 <- 
  path_list_2016_2018 %>% 
  purrr::set_names(year_2016_2018) %>% 
  map(~excel_import_sheet(
    path = .,
    sheet_list = sheet_list,
    skip = 5,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric") 
  )) 

year_2019_2021 <- c(2019:2021) # excluding 2022 which is included in the 2023 sheet
path_list_2019_2021 <- 
  year_2019_2021 %>% 
  map(~ glue(
    here("Data","BA930", "4.1 BA930 Multiple Bank Export ({.}).xlsx")
  ))

mortgage_lending_2019_2021 <- 
  path_list_2019_2021 %>% 
  purrr::set_names(year_2019_2021) %>% 
  map(~excel_import_sheet(
    path = .,
    sheet_list = sheet_list,
    skip = 560,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric") 
  )) 

year_2022_2023 <- c(2023) # excluding 2022 which is included in the 2023 sheet
path_list_2022_2023 <- 
  year_2022_2023 %>% 
  map(~ glue(
    here("Data","BA930", "4.1 BA930 Multiple Bank Export ({.}).xlsx")
  ))

mortgage_lending_2022_2023 <- 
  path_list_2022_2023 %>% 
  purrr::set_names(year_2022_2023) %>% 
  map(~excel_import_sheet(
    path = .,
    sheet_list = sheet_list,
    skip = 1113,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric") 
  )) 

mortgage_lending <-
  c(mortgage_lending_2008_2015,
    mortgage_lending_2016_2018,
    mortgage_lending_2019_2021,
    mortgage_lending_2022_2023)

mortgage_lending
# Cleaning -----------------------------------------------------------------
names <- c("Banks", 
           "Description", 
           "Date",  
           "Item", 
           "Outstanding balance at month end R'000",
           "Weighted average rate (%)",
           "New loans granted during the month R'000",
           "New loans repaid during the month Rate")
mortgage_lending_no_names <- 
  mortgage_lending %>%
  map(~bind_rows(., .id = "Banks")) %>% 
  map(~setNames(., names)) %>% 
  map(~ filter(., Item > 46)) %>% 
  map(~dplyr::select(., 1:8))

mortgage_lending_tbl <- 
  mortgage_lending_no_names %>% 
  bind_rows() %>% 
  relocate(Date,.before = Banks) %>% 
  mutate(Date = parse_date_time(Date, "b-Y")) %>% 
  mutate(Description = na.locf(Description)) %>%
  mutate(`Item Description` = dplyr::case_when(
    Item == 47 | Item == 48 | Item == 49 | Item == 51 | Item == 53 | Item ==55 | Item == 56  ~ "corporate sector",
    Item == 50 ~ "corporate sector installment sale agreements",
    Item == 52 ~ "corporate sector leasing transactions",
    Item == 54 ~ "corporate sector mortgage advances", 
    Item == 57 | Item == 58 | Item == 59 |  Item == 61 | Item == 63 | Item ==65 | Item ==66 ~  "household sector",
    Item == 60 ~ "household sector installment sale agreements",
    Item == 62 ~ "household sector leasing transactions",
    Item == 64 ~ "household sector mortgage advances",
    Item == 67 | Item == 68 | Item == 69 | 
      Item == 70 | Item == 71 | Item == 72 | 
      Item == 73 ~ "foreign sector",
    Item == 74 | Item ==75 | Item == 76 | Item ==78 |  Item ==80 | Item == 82 | Item == 83  ~ "domestic private sector", 
    Item == 77 ~ "domestic private sector installment sale agreements",
    Item == 79 ~ "domestic private sector leasing transactions",
    Item == 81 ~ "domestic private sector mortgage advances",
    Item == 84 ~ "Micro loans",
    Item == 85 ~ "Interbank lending rate",
    Item == 86 ~ "Hash total"
  )) %>%
  relocate('Item Description', .after = `Item Description`) %>% 
  mutate(Description = str_replace_all(Description, "-", "")) %>%
  mutate(Description = str_c(Description, ": ", `Item Description`)) %>% 
  dplyr::select(-`Item Description`) %>% 
  mutate(Description = str_remove_all(Description, "\"")) %>% 
  mutate(Description = str_replace_all(Description, ":", "-")) %>% 
  mutate(Description = str_replace_all(Description, "- -", "-")) %>% 
  filter(str_detect(Description, "Mortgage|mortgage")) %>% # filtering for mortgages
  filter(!str_detect(Description, "domestic private sector|foreign sector")) # removing these categories

mortgage_lending_tbl
mortgage_lending_tbl %>% group_by(Banks) %>%  skim()

unique(mortgage_lending_tbl$Description)

# Export ---------------------------------------------------------------
artifacts_BA930_mortgages <- list (
    mortgage_lending_tbl = mortgage_lending_tbl
)

write_rds(artifacts_BA930_mortgages, file = here("Outputs", "BA930", "artifacts_BA930_mortgages.rds"))
