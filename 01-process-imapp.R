library(tidyverse)
library(lubridate)
library(tsibble)

map <- as_tibble(readxl::read_excel("./data/iMaPP_database -- 2020-09-08.xlsx", sheet = 4))

#Transformando as variáveis em inteiros e substituindo NAs por 0

imapp <- 
  map %>%
  mutate(across(c(CCB, Conservation, Capital, LVR, SIFI, LLP, RR,  LTV,
                  DSTI, LLP, Tax, LCG, LoanR, Liquidity, LFX, RR, LTD, LFC, OT), as.integer))

imapp <- 
  imapp %>%
  mutate(across(c(CCB, Conservation, Capital, LVR, SIFI, LLP, RR,  LTV,
                  DSTI, LLP, Tax, LCG, LoanR, Liquidity, LFX, RR, LTD, LFC, OT), ~replace_na(.,0)))

# Transformando as variáveis ano e mês para character

imapp$Year <- as.character(imapp$Year)
imapp$Month <- as.character(imapp$Month)

# Organização e fomatação da variável de data
imapp <- imapp %>%
  mutate(Month = case_when(Month == "1" | Month == "2" | Month == "3" | Month == "4" | Month == "5" | 
                             Month == "6" | Month == "7" | Month == "8" | Month == "9" ~ paste0("0", Month),
                           TRUE ~ Month))

imapp$date <- as.Date(paste0(imapp$Month, imapp$Year, "01"), format = "%m%Y%d")

imapp$Year <- as.integer(imapp$Year)
imapp$Month <- as.integer(imapp$Month)

# Variáveis acumuladas por categoria de medidas macroprudenciais

imapp <- imapp %>%
  mutate(capital = (CCB + Conservation + Capital + SIFI + LVR),
         credit = (LTV + DSTI + LLP + LFC + Tax + LCG + LoanR),
         liquidity = (Liquidity + LFX + LTD + RR),
         others = OT
  )

# Dataframe para o modelo

imapp <-
  imapp %>%
  mutate(year = year(date),
         Country = str_replace(Country, "TaiwanProvinceofChina", "ChineseTaipei"))

imapp <-
  imapp %>% 
  group_by(Country, ifscode, year, AE) %>% 
  summarise(
    capital = sum(capital),
    credit = sum(credit),
    liquidity = sum(liquidity),
    imapp = sum(SUM_17)
    ) %>%
  mutate(
    capital_cum = case_when(capital > 0 ~ 1,
                            capital < 0 ~ -1,
                            TRUE ~ 0),
    credit_cum = case_when(credit > 0 ~ 1,
                           credit < 0 ~ -1,
                           TRUE ~ 0),
    liquidity_cum = case_when(liquidity > 0 ~ 1,
                              liquidity < 0 ~ -1,
                              TRUE ~ 0),
    imapp_cum = case_when(imapp > 0 ~ 1,
                          imapp < 0 ~ -1,
                          TRUE ~ 0),
    capital = case_when(capital > 0 ~ 1,
                        capital < 0 ~ 0,
                        TRUE ~ 0),
    credit = case_when(credit > 0 ~ 1,
                       credit < 0 ~ 0,
                      TRUE ~ 0),
    liquidity = case_when(liquidity > 0 ~ 1,
                          liquidity < 0 ~ 0,
                          TRUE ~ 0),
    imapp = case_when(imapp > 0 ~ 1,
                      imapp < 0 ~ 0,
                      TRUE ~ 0)
    )

imapp <-
  imapp %>%
  group_by(ifscode) %>%
  mutate(
    capital_cum = cumsum(capital_cum),
    credit_cum = cumsum(credit_cum),
    liquidity_cum = cumsum(liquidity_cum),
    imapp_cum = cumsum(imapp_cum),
  )

write.csv(imapp, file = "./datasets/imapp.csv")
