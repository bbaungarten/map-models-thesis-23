library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)
library(tsibble)
library(WDI)

# World Bank Data - Controls

trade <- WDI(indicator = c("trade" = "NE.TRD.GNFS.ZS"),
             country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_constant_lcu <-
  WDI(indicator = c("gdp_constant_lcu"  =  "NY.GDP.MKTP.KN"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_ppp <-
  WDI(indicator = c("gdp_ppp_current_dollar" = "NY.GDP.MKTP.PP.CD"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdppc_current_lcu <-
  WDI(indicator = c("gdp_pc_current_lcu"  = "NY.GDP.PCAP.CN"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdppc_dollar2015 <-
  WDI(indicator = c("gdp_pc_dollar2015" = "NY.GDP.PCAP.KD"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_pc_growth <-
  WDI(indicator = c("gdp_pc_growth" = "NY.GDP.PCAP.KD.ZG"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

interest_rate <-
  WDI(indicator = c("interest_rate" = "FR.INR.RINR"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

exchange_rate <-
  WDI(indicator = c("exchange_rate" = "REER"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

cpia <-
  WDI(indicator = c("cpia" = "IQ.CPA.BREG.XQ"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

cpia_financial_sector <-
  WDI(indicator = c("cpia_financial_sector" = "IQ.CPA.FINS.XQ"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

cpia_property_rigts <-
  WDI(indicator = c("cpia_property_rights" = "IQ.CPA.FINS.XQ"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

regulatory_quality_estimate <-
  WDI(indicator = c("regulatory_quality_estimate" = "RQ.EST"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_growth <-
  WDI(indicator = c("gdp_growth" = "NY.GDP.MKTP.KD.ZG"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_current_dollar <-
  WDI(indicator = c("gdp_pc_current_dollar" = "NY.GDP.PCAP.CD"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

gdp_dollar2015 <-
  WDI(indicator = c("gdp_dollar2015" = "NY.GDP.MKTP.KD"),
      country = "all", start = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

lending_interest_rate <-
  WDI(indicator = c("lending_interest_rate" = "FR.INR.LEND"),
      country = "all", star = 2000, end = 2020, extra = FALSE) %>%
  select(-iso2c)

wdi_controls <- 
  gdp_constant_lcu %>%
  left_join(gdp_ppp, by = c("country", "year")) %>%
  left_join(gdppc_current_lcu, by = c("country", "year")) %>%
  left_join(gdppc_dollar2015, by = c("country", "year")) %>%
  left_join(gdp_pc_growth, by = c("country", "year")) %>%
  left_join(interest_rate, by = c("country", "year")) %>%
  left_join(exchange_rate, by = c("country", "year")) %>%
  left_join(trade, by = c("country", "year")) %>%
  left_join(cpia, by = c("country", "year")) %>%
  left_join(cpia_financial_sector, by = c("country", "year")) %>%
  left_join(cpia_property_rigts, by = c("country", "year")) %>%
  left_join(regulatory_quality_estimate, by = c("country", "year")) %>%
  left_join(gdp_growth, by = c("country", "year")) %>%
  left_join(gdp_current_dollar, by = c("country", "year")) %>%
  left_join(gdp_current_dollar, by = c("country", "year")) %>%
  left_join(gdp_dollar2015, by = c("country", "year")) %>%
  left_join(lending_interest_rate, by = c("country", "year"))

drop_na(wdi_controls)

write_csv(wdi_controls, file = "./datasets/wdi.csv")



