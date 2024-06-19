# Description
# Getting selected banks balance sheet. June 2024- Xolani Sibande

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
source(here("Functions", "Sheets_import.R"))
source(here("Functions", "excel_import_sheet.R"))
source(here("Functions", "cleanup.R"))
source(here("Functions", "filter_and_stings.R"))
source(here("Functions", "balance_sheet_rename_gg.R"))
source(here("Functions", "ba900_aggregation.R"))
source(here("Functions", "ba900_aggregation_shares.R")) 


# Import ------------------------------------------------------------------
path = here("Data", "BA900", "BA900_line_item_103_to_277_updated_to_Aug_2023.xlsx")

banks_to_keep_list <- list(
  "Total Banks"  = "Sheet1",
  "Standard_Bank" = "Sheet6",
  "Nedbank" = "Sheet5",
  "First_Rand" = "Sheet3",
  "Investec" = "Sheet4",
  "ABSA" = "Sheet2",
  "Taiwan" = "Sheet23",
  "Grindrod" = "Sheet17",
  "Albaraka" = "Sheet16"
)

col_types = c("text", rep(x = "numeric", 188)) # Change with update

ba900_balance_sheet <- 
  excel_import_sheet(
    path = path,
    sheet_list = banks_to_keep_list,
    skip = 5,
    col_types = col_types 
  ) 


ba900_balance_sheet %>% glimpse()

# Cleaning ----------------------------------------------------------
total_tbl <- cleanup(ba900_balance_sheet$`Total Banks`, date_size = 176) 
absa_tbl <- cleanup(ba900_balance_sheet$ABSA, date_size = 176)
fnb_tbl <- cleanup(ba900_balance_sheet$First_Rand, date_size = 175)
nedbank_tbl <- cleanup(ba900_balance_sheet$Nedbank, date_size = 175)
standard_tbl <- cleanup(ba900_balance_sheet$Standard_Bank, date_size = 175)
investec_tbl <- cleanup(ba900_balance_sheet$Investec, date_size = 175)
grindord_tbl <- cleanup(ba900_balance_sheet$Grindrod, date_size = 175)
alabaraka_tbl <- cleanup(ba900_balance_sheet$Albaraka, date_size = 175)
taiwan_tbl <- cleanup(ba900_balance_sheet$Taiwan, date_size = 177)

unique(total_tbl$Series)

# EDA ---------------------------------------------------------------------
total_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
absa_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
fnb_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
nedbank_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
standard_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
investec_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
grindord_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
alabaraka_tbl%>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
taiwan_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()

#  Filtering --------------------------------------------------------------

## Filter vec -----------------------------------------------------------
filter_vec <- 
  c(
    "CENTRAL BANK MONEY AND GOLD (total of items 104 to 106)"                                                                                                        ,"South African bank notes and subsidiary coin"                                                                                                                   ,"Gold coin and bullion"                                                                                                                                          ,"Domestic currency deposits with SA Reserve Bank (total of items 107 to 109)"                                                                                    ,"Cash reserve deposits: Interest bearing"                                                                                                                        ,"Cash reserve deposits: Non-interest bearing"                                                                                                                    ,"Other deposits"                                                                                                                                                 ,"DEPOSITS, LOANS AND ADVANCES (total of items 111, 117, 118, 126, 135, 139, 150, 166, 171 and 180, less item 194)"                                               ,"SA banksb (total of items 112 and 116)"                                                                                                                      
   ,"NCDs/PNsc issued by banks, with an unexpired maturity of: (total of items 113 to 115)"                                                                       
   ,"Up to 1 month"                                                                                                                                               
   ,"More than 1 month to 6 months"                                                                                                                               
   ,"More than 6 months"                                                                                                                                          
   ,"Other deposits with and loans and advances to SA banksb"                                                                                                     
   ,"Deposits with and loans and advances to foreign banks, denominated in rand"                                                                                  
   ,"Loans granted under resale agreements to:  (total of items 119 to 125)"                                                                                      
   ,"SA Reserve Bank"                                                                                                                                             
   ,"Banksd"                                                                                                                                                      
   ,"Insurers"                                                                                                                                                    
   ,"Pension funds"                                                                                                                                               
   ,"Other financial corporate sectorb"                                                                                                                           
   ,"Non-financial corporate sector"                                                                                                                              
   ,"Other"                                                                                                                                                       
   ,"Foreign currency loans and advances (total of items 127 to 130, 133 and 134)"                                                                                
   ,"Foreign currency notes and coin"                                                                                                                             
   ,"Deposits with and advances to SA Reserve Bank"                                                                                                               
   ,"Deposits with and advances to SA banksd"                                                                                                                     
   ,"Other advances to: (total of items 131 and 132)"                                                                                                             
   ,"SA Financial corporate sectorc"                                                                                                                              
   ,"SA Non-financial corporate sector and other"                                                                                                                 
   ,"Deposits with and advances to foreign banks"                                                                                                                 
   ,"Other advances to foreign sector"                                                                                                                            
   ,"Redeemable preference shares issued by: (total items 136 to 138)"                                                                                            
   ,"Banksd-1"                                                                                                                                                    
   ,"Financial corporate sectorc"                                                                                                                                 
   ,"Non-financial corporate sector and other"                                                                                                                    
   ,"Instalment debtors, suspensive sales and leases (total of items 140 and 145)"                                                                                
   ,"Instalment sales (total of items 141 to 144)"                                                                                                                
   ,"Financial corporate sector"                                                                                                                                  
   ,"Non-financial corporate sector-1"                                                                                                                            
   ,"Household sector"                                                                                                                                            
   ,"Otherb"                                                                                                                                                      
   ,"Leasing transactions (total of items 146 to 149)"                                                                                                            
   ,"Financial corporate sector-1"                                                                                                                                
   ,"Non-financial corporate sector-2"                                                                                                                            
   ,"Household sector-1"                                                                                                                                          
   ,"Otherb-1"                                                                                                                                                    
   ,"Mortgage advances (total of items 151, 155 and 159)"                                                                                                         
   ,"Farm mortgages: (total of items 152 to 154)"                                                                                                                 
   ,"Corporate sector"                                                                                                                                            
   ,"Household sector-2"                                                                                                                                          
   ,"Otherb-2"                                                                                                                                                    
   ,"Residential mortgages: (total of items 156 to 158)"                                                                                                          
   ,"Corporate sector-1"                                                                                                                                          
   ,"Household sector-3"                                                                                                                                          
   ,"Otherb-3"                                                                                                                                                    
   ,"Commercial and other mortgage advances: (total of items 160 to 165)"                                                                                         
   ,"Public financial corporate sector"                                                                                                                           
   ,"Public non-financial corporate sector"                                                                                                                       
   ,"Private financial corporate sector"                                                                                                                          
   ,"Private non-financial corporate sector"                                                                                                                      
   ,"Household sector-4"                                                                                                                                          
   ,"Otherb-4"                                                                                                                                                    
   ,"Credit-card debtors (total of items 167 to 170)"                                                                                                             
   ,"Financial corporate sector-2"                                                                                                                                
   ,"Non-financial corporate sector-3"                                                                                                                            
   ,"Household sector-5"                                                                                                                                          
   ,"Otherb-5"                                                                                                                                                    
   ,"Overdrafts, loans and advances: public sector (total of items 172 to 179)"                                                                                   
   ,"Central government of the Republic (excluding social security funds)"                                                                                        
   ,"Social security funds"                                                                                                                                       
   ,"Provincial governments"                                                                                                                                      
   ,"Local government"                                                                                                                                            
   ,"Land Bank"                                                                                                                                                   
   ,"Other public financial corporate sector (such as IDC)c"                                                                                                      
   ,"Public non-financial corporate sector (such as Transnet, Eskom and Telkom)"                                                                                  
   ,"Foreign public sector"                                                                                                                                       
   ,"Overdrafts, loans and advances: private sector (total of items 181, 187 and 188)"                                                                            
   ,"Overdrafts, including overdrafts under cash-management schemes: (total of items 182 to 186)"                                                                 
   ,"Financial corporate sector-3"                                                                                                                                
   ,"Non-financial corporate sector-4"                                                                                                                            
   ,"Unincorporated business enterprises of households"                                                                                                           
   ,"Households"                                                                                                                                                  
   ,"Non-profit organisations serving households"                                                                                                                 
   ,"Factoring debtors"                                                                                                                                           
   ,"Other loans and advances: (total of items 189 to 193)"                                                                                                       
   ,"Financial corporate sector-4"                                                                                                                                
   ,"Non-financial corporate sector-5"                                                                                                                            
   ,"Unincorporated business enterprises of households-1"                                                                                                         
   ,"Households-1"                                                                                                                                                
   ,"Non-profit organisations serving households-1"                                                                                                               
   ,"Less: credit impairments in respect of loans and advances"                                                                                                   
   ,"INVESTMENTS AND BILLS, including trading portfolio assets (total of items 196, 207, 213, 217, 221, 225, 229, 233, 237, 241 and 246, less item 245)"          
   ,"Interest-bearing central or provincial government securities (total of items 197, 198 and 203 to 206)"                                                       
   ,"Non-marketable government stock"                                                                                                                             
   ,"Marketable government stock (total of item 199 and 201)"                                                                                                     
   ,"Unexpired maturity of up to 3 years"                                                                                                                         
   ,"Memo: Nominal value of such stock"                                                                                                                           
   ,"Unexpired maturity of more than 3 years"                                                                                                                     
   ,"Memo: Nominal value of such stock-1"                                                                                                                         
   ,"Government loan levies"                                                                                                                                      
   ,"Securities of provincial governments"                                                                                                                        
   ,"Securities of social security funds"                                                                                                                         
   ,"Securities of other central government institutionse"                                                                                                        
   ,"Other public-sector interest-bearing securities (total of items 208 to 212)"                                                                                 
   ,"SA Reserve Bank debentures"                                                                                                                                  
   ,"Securities (including debentures) issued by the Land Bank"                                                                                                   
   ,"Securities issued by other public financial corporate sectorb (such as IDC, DBSA)"                                                                           
   ,"Securities issued by public non-financial corporate sector (such as Transnet and Eskom)"                                                                     
   ,"Securities of local authorities"                                                                                                                             
   ,"Debentures and other interest bearing security investments of private sector (total of items 214 to 216)"                                                    
   ,"Banksd-2"                                                                                                                                                    
   ,"Financial corporate sectorc-1"                                                                                                                               
   ,"Non-financial corporate sector and other-1"                                                                                                                  
   ,"Equity holdings in subsidiaries (total of items 218 to 220)"                                                                                                 
   ,"Banksc"                                                                                                                                                      
   ,"Financial corporate sectorb"                                                                                                                                 
   ,"Non-financial corporate sector-6"                                                                                                                            
   ,"Equity holdings in associates including joint ventures (total of items 222 to 224)"                                                                          
   ,"Banksc-1"                                                                                                                                                    
   ,"Financial corporate sectorb-1"                                                                                                                               
   ,"Non-financial corporate sector-7"                                                                                                                            
   ,"Listed equities (total of items 226 to 228)"                                                                                                                 
   ,"Banksc-2"                                                                                                                                                    
   ,"Financial corporate sectorb-2"                                                                                                                               
   ,"Non-financial corporate sector-8"                                                                                                                            
   ,"Unlisted equities (total of items 230 to 232)"                                                                                                               
   ,"Banksc-3"                                                                                                                                                    
   ,"Financial corporate sectorb-3"                                                                                                                               
   ,"Non-financial corporate sector-9"                                                                                                                            
   ,"Securitisation/ asset-backed securities: (total of items 234 to 236)"                                                                                        
   ,"Banksc-4"                                                                                                                                                    
   ,"Financial corporate sectorb-4"                                                                                                                               
   ,"Non-financial corporate sector-10"                                                                                                                           
   ,"Derivative instruments issued by: (total of items 238 to 240)"                                                                                               
   ,"Banksc and other monetary institutionsf"                                                                                                                     
   ,"Financial corporate sectorb-5"                                                                                                                               
   ,"Non-financial corporate sector and other-2"                                                                                                                  
   ,"Other investments (total of items 242 to 244)"                                                                                                               
   ,"Banksc-5"                                                                                                                                                    
   ,"Financial corporate sectorb-6"                                                                                                                               
   ,"Non-financial corporate sector-11"                                                                                                                           
   ,"Less: Allowances for impairments i.r.o investments"                                                                                                          
   ,"Acceptances, commercial paper, bills, promissory notes and similar acknowledgements of debt discounted or purchased (total of items 247, 250 to 254 and 257)"
   ,"Bankers' acceptances (total of items 248 and 249)"                                                                                                           
   ,"Own bankers' acceptances"                                                                                                                                    
   ,"Other bankers' acceptances"                                                                                                                                  
   ,"Treasury bills"                                                                                                                                              
   ,"SA Reserve Bank bills"                                                                                                                                       
   ,"Promissory notes"                                                                                                                                            
   ,"Commercial paper"                                                                                                                                            
   ,"Land Bank bills (total of items 255 and 256)"                                                                                                                
   ,"Liquid"                                                                                                                                                      
   ,"Non-liquid"                                                                                                                                                  
   ,"Other-1"                                                                                                                                                     
   ,"NON-FINANCIAL ASSETS (total of items 259 and 264)"                                                                                                           
   ,"Tangible assets (total of items 260 to 263)"                                                                                                                 
   ,"Premises of the bank"                                                                                                                                        
   ,"Other fixed property"                                                                                                                                        
   ,"Computer equipment, including peripherals"                                                                                                                   
   ,"Other tangible assets, including vehicles, equipment, furniture and fittings"                                                                                
   ,"Intangible assets (total of items 265 and 266)"                                                                                                              
   ,"Computer software"                                                                                                                                           
   ,"Other intangible assets including purchased goodwill"                                                                                                        
   ,"OTHER ASSETS (total of items 268 to 272 and 276)"                                                                                                            
   ,"Clients' liabilities per contra (total of items 280 to 283)"                                                                                                 
   ,"Remittances in transit"                                                                                                                                      
   ,"Current income tax receivables and deferred income tax assets"                                                                                               
   ,"Retirement benefit assets"                                                                                                                                   
   ,"Assets acquired or bought in to protect an advance or investment (total of items 273 to 275)"                                                                
   ,"Fixed property"                                                                                                                                              
   ,"Shares"                                                                                                                                                      
   ,"Vehicles and other assets"                                                                                                                                   
   ,"Other-2"                                                                                                                                                     
   ,"TOTAL ASSETS (total of items 103, 110, 195, 258 and 267)"                                                                                                    
)




#### GET THINGS TO CALCULATE





## Filtering and graphs -------------------------------------------------------------
total_filtered_tbl <- filter_and_strings(total_tbl, filter_vec) %>%  balance_sheet_rename() 
total_filtered_tbl 
absa_filtered_tbl <- filter_and_strings(absa_tbl, filter_vec) %>%  balance_sheet_rename()
absa_filtered_tbl
fnb_filtered_tbl <- filter_and_strings(fnb_tbl, filter_vec) %>%  balance_sheet_rename()
fnb_filtered_tbl
nedbank_filtered_tbl <- filter_and_strings(nedbank_tbl, filter_vec) %>%  balance_sheet_rename()
nedbank_filtered_tbl
standard_filtered_tbl <- filter_and_strings(standard_tbl, filter_vec) %>%  balance_sheet_rename()
standard_filtered_tbl
capitec_filtered_tbl <- filter_and_strings(capitec_tbl, filter_vec) %>%  balance_sheet_rename()
capitec_filtered_tbl

capitec_filtered_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>% skim()
totals_gg <-
  total_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
absa_gg <-
  absa_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
fnb_gg <-
  fnb_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
nedbank_gg <-
  nedbank_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
standard_gg <-
  standard_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
capitec_gg <-
  capitec_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
capitec_filtered_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>% skim()

unique(total_filtered_tbl$Series) 

## Aggregation and graphs -----------------------------------------------------------
total_aggregation_tbl <- ba900_aggregration(total_filtered_tbl)
total_aggregation_gg <- 
  total_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

absa_aggregation_tbl <- ba900_aggregration(absa_filtered_tbl)
absa_aggregation_tbl <- ba900_aggregration(absa_filtered_tbl)
absa_aggregation_gg <- 
  absa_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

fnb_aggregation_tbl <- ba900_aggregration(fnb_filtered_tbl)
fnb_aggregation_gg <- 
  fnb_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

nedbank_aggregation_tbl <- ba900_aggregration(nedbank_filtered_tbl)
nedbank_aggregation_gg <- 
  nedbank_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

standard_aggregation_tbl <- ba900_aggregration(standard_filtered_tbl)
standard_aggregation_gg <- 
  standard_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

capitec_aggregation_tbl <- ba900_aggregration(capitec_filtered_tbl)
capitec_aggregation_gg <- 
  capitec_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 10)

## Combined aggregated data ---------------------------------------------------------
combined_aggregated_lending_tbl <- 
  bind_rows(
    total_aggregation_tbl = total_aggregation_tbl,
    absa_aggregation_tbl = absa_aggregation_tbl,
    fnb_aggregation_tbl = fnb_aggregation_tbl,
    nedbank_aggregation_tbl = nedbank_aggregation_tbl,
    standard_aggregation_tbl = standard_aggregation_tbl,
    capitec_aggregation_tbl = capitec_aggregation_tbl,
    .id = "Banks") %>% 
  mutate(Banks = str_replace_all(Banks, "_aggregation_tbl", "")) %>% 
  mutate(Banks = str_to_title(Banks)) %>% 
  mutate(Banks = str_replace_all(Banks, "Total", "Total Banks")) %>%
  mutate(Banks = str_replace_all(Banks, "Absa", "Absa Bank")) %>%
  mutate(Banks = str_replace_all(Banks, "Fnb", "FNB")) %>% 
  mutate(Banks = str_replace_all(Banks, "Standard", "Standard Bank"))

combined_aggregated_lending_tbl %>% 
  pivot_wider(id_cols = c(Date, Banks),names_from = Series, values_from = Value) %>%
  skim()

# Aggregated data shares ------------------------------------------------------------
total_banks_shares_tbl <-   
  ba900_aggregration_shares(
    filtered_data= total_filtered_tbl,
    aggregated_data = total_aggregation_tbl
  ) %>% 
  mutate(Banks = "Total Banks")

absa_banks_shares_tbl <-
  ba900_aggregration_shares(
    filtered_data= absa_filtered_tbl,
    aggregated_data = absa_aggregation_tbl
  ) %>% 
  mutate(Banks = "Absa Bank")

fnb_banks_shares_tbl <-
  ba900_aggregration_shares(
    filtered_data= fnb_filtered_tbl,
    aggregated_data = fnb_aggregation_tbl
  ) %>% 
  mutate(Banks = "FNB")

nedbank_banks_shares_tbl <-
  ba900_aggregration_shares(
    filtered_data= nedbank_filtered_tbl,
    aggregated_data = nedbank_aggregation_tbl
  ) %>% 
  mutate(Banks = "Nedbank")

standard_banks_shares_tbl <-
  ba900_aggregration_shares(
    filtered_data= standard_filtered_tbl,
    aggregated_data = standard_aggregation_tbl
  ) %>% 
  mutate(Banks = "Standard Bank")

capitec_banks_shares_tbl <-
  ba900_aggregration_shares(
    filtered_data= capitec_filtered_tbl,
    aggregated_data = capitec_aggregation_tbl
  ) %>% 
  mutate(Banks = "Capitec")
capitec_banks_shares_tbl %>% pivot_wider(names_from = Series,
                                         values_from = Value) %>% 
  skim()
# combine shares tbl
combined_shares_tbl <- 
  bind_rows(
    total_banks_shares_tbl,
    absa_banks_shares_tbl,
    fnb_banks_shares_tbl,
    nedbank_banks_shares_tbl,
    standard_banks_shares_tbl,
    capitec_banks_shares_tbl
  ) %>% 
  relocate(Banks, .after = 1) 


# Export ------------------------------------------------------------------
data_list = list(
  total_tbl = total_tbl,
  absa_tbl = absa_tbl,
  fnb_tbl = fnb_tbl,
  nedbank_tbl = nedbank_tbl,
  standard_tbl = standard_tbl,
  capitec_tbl = capitec_tbl)

files <- paste0(here("Outputs"), "/", "BA900/", names(data_list), ".csv")

walk2(data_list, files, ~ write.csv(x = .x, file = .y, row.names = F )) # exporting top five banks to CSV (as an example)

artifacts_BA900 <- list (
  filtered_data = list(
    total_filtered_tbl = total_filtered_tbl,
    absa_filtered_tbl = absa_filtered_tbl,
    fnb_filtered_tbl = fnb_filtered_tbl,
    nedbank_filtered_tbl = nedbank_filtered_tbl,
    standard_filtered_tbl = standard_filtered_tbl,
    capitec_filtered_tbl = capitec_filtered_tbl
  ),
  aggregated_data = list(
    total_aggregation_tbl = total_aggregation_tbl,
    absa_aggregation_tbl = absa_aggregation_tbl,
    fnb_aggregation_tbl = fnb_aggregation_tbl,
    nedbank_aggregation_tbl = nedbank_aggregation_tbl,
    standard_aggregation_tbl = standard_aggregation_tbl,
    capitec_aggregation_tbl = capitec_aggregation_tbl,
    combined_aggregated_lending_tbl = combined_aggregated_lending_tbl
  ),
  shares_data = list(
    total_banks_shares_tbl = total_banks_shares_tbl,
    absa_banks_shares_tbl = absa_banks_shares_tbl,
    fnb_banks_shares_tbl = fnb_banks_shares_tbl,
    nedbank_banks_shares_tbl = nedbank_banks_shares_tbl,
    standard_banks_shares_tbl = standard_banks_shares_tbl,
    capitec_banks_shares_tbl = capitec_banks_shares_tbl,
    combined_shares_tbl = combined_shares_tbl
  ),
  filtered_graphs = list(
    totals_gg = totals_gg,
    absa_gg = absa_gg,
    fnb_gg = fnb_gg,
    nedbank_gg = nedbank_gg,
    standard_gg = standard_gg,
    capitec_gg = capitec_gg
  ),
  aggregated_graphs = list(
    total_aggregation_gg = total_aggregation_gg,
    absa_aggregation_gg = absa_aggregation_gg,
    fnb_aggregation_gg = fnb_aggregation_gg,
    nedbank_aggregation_gg = nedbank_aggregation_gg,
    standard_aggregation_gg = standard_aggregation_gg,
    capitec_aggregation_gg = capitec_aggregation_gg
  )
)

write_rds(artifacts_BA900, file = here("Outputs", "BA900", "artifacts_BA900.rds"))









