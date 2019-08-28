# load packages
library(tidyverse)
library(lubridate)

# read data
data_raw <- read.table("FAR_Finidx.txt", sep = "\t", 
                       header = TRUE, encoding = "UTF-8")
data_profit <- read.table("FS_Comins.txt", sep = "\t",
                          header = TRUE, encoding = "UTF-8")

# data cleaning
data <- data_raw %>%
  # Select Variables
  dplyr::rename(Code = Stkcd,          # Stock Code
                ROE = T40801,          # Return on Equity
                AL = T30100,           # Asset Liability Ratio
                PM = T40700) %>%       # Profit Margin (Profit/Revenue)
  mutate(Year = year(as.Date(Accper)), # Year
         Asset = log(A100000)) %>%     # Log-transformed Total Assets
  # Order data by company and year
  arrange(Code, Year) %>%
  # Get rid of discontinous data
  group_by(Code) %>%
  mutate(Diff = c(diff(Year),1)) %>%
  filter(Diff == 1) %>%
  # Get response (ROE of next year)
  group_by(Code) %>%
  mutate(Y = lead(ROE)) %>%            # Y is to predict ROE of next year
  select(Code, Year, Y, ROE, Asset, AL, PM) %>%
  # Join profit data
  left_join(data_profit %>%
              mutate(Code = Stkcd,                 # Stock Code
                     Date = as.Date(Accper)) %>%   # Date
              filter(substr(Date,6,7) == "12") %>% # Get profit data in December every year
              group_by(Code, Date) %>%
              dplyr::summarise(Sales = max(B001100000, 
                                           na.rm = T)) %>% # Sales of current year
              # Get rid of discontinous data
              group_by(Code) %>%
              mutate(Diff = c(diff(year(Date)),1)) %>%
              filter(Diff == 1) %>%
              # Get Sales Growth
              group_by(Code) %>%
              mutate(# Sales Growth = (Sales - Sales_Lag1) / Sales_Lag1
                Growth = (Sales - lag(Sales)) / lag(Sales),
                Year = year(Date)) %>%
              select(Code, Year, Sales, Growth),
            by = c("Code", "Year")) %>%
  mutate(ATO = Sales / exp(Asset)) %>% # Asset Turnover Ratio (Total Sales/Total Asset)
  select(-Sales) %>%
  filter(Growth != Inf) %>%
  as.data.frame() %>%
  na.omit()

save(data, file = "ROE.Rdata")