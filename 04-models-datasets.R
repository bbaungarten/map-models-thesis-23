library("tidyverse")
library("tsibble")

# Dados IMAPP
imapp <- as_tibble(read.csv(file = "./datasets/imapp.csv"))

imapp <- imapp %>%
  rename(country = Country)


imapp <- imapp %>%
  mutate(
    country = case_when(
    (country != "UnitedKingdom" & country != "UnitedStates" & country != "ChineseTaipei" & country != "HongKongSAR" &
       country != "SaudiArabia"  & country != "SouthAfrica") ~ country,
    country == "UnitedKingdom" ~ "United Kingdom",
    country == "UnitedStates" ~ "United States",
    country == "ChineseTaipei" ~ "Chinese Taipei",
    country == "HongKongSAR" ~ "Hong Kong SAR",
    country == "SaudiArabia" ~ "Saudi Arabia",
    country == "SouthAfrica" ~ "South Africa"
  ))

# Dados WDI
wdi_controls <- as_tibble(read.csv(file = "./datasets/wdi.csv"))

# Trilemma
trilemma <-as_tibble(read.csv(file = "./data/trilemma_indexes_update2019.csv"))

trilemma <- trilemma %>%
  rename(ifscode = IMF.World.Bank.Country.Code)

## Credit to GDP gap
credit_gap <- as_tibble(read_csv(file = "./data/WEBSTATS_CREDIT_GAP_DATAFLOW_Export (1).csv"))

credit_gap <-
  credit_gap %>%
  rename(
    year_quarter = `TIME_PERIOD:Time period or range`,
    credit_gdp_gap = `OBS_VALUE:Observation Value`
  ) %>%
  mutate(
    country = str_sub(`BORROWERS_CTY:Borrowers' country`, start = 5L)
  ) %>%
  select(
    country, year_quarter, credit_gdp_gap
  )

credit_gap$year_quarter <- yearquarter(credit_gap$year_quarter)

credit_gap <-
  credit_gap %>%
  mutate(year = lubridate::year(year_quarter))

credit_gap <-
  credit_gap %>%
  group_by(year, country) %>%
  summarise(credit_gdp_gap = mean(credit_gdp_gap))

# Property Prices
property_prices <- as_tibble(read_csv(file = "./data/property_prices.csv"))

property_prices$year_quarter <- yearquarter(property_prices$year_quarter)

property_prices <-
  property_prices %>%
  mutate(year = lubridate::year(year_quarter)) %>%
  group_by(year, country) %>%
  summarise(pp_index = mean(pp_index))

# Jutando os dataframes
controls <- imapp %>%
  left_join(wdi_controls, by = c("country", "year"))

controls <- controls %>%
  left_join(trilemma, by = c("ifscode", "year" = "year"))

controls <-
  controls %>%
  left_join(credit_gap, by = c("country", "year"))

controls <-
  controls %>%
  left_join(property_prices, by = c("country", "year"))

controls <-
  controls %>%
  rename(
    monetary_independence_index = Monetary.Independence.Index,
    exchange_rate_stability_index = Exchange.Rate.Stability.Index,
    kaopen = Financial.Openness.Index
  )

# BIS CROSS BORDER
bis_cross_border <- as_tibble(read.csv(file = "./datasets/bis_cross_border.csv"))

bis_cross_border$year_quarter <- tsibble::yearquarter(bis_cross_border$year_quarter)

df_cross_border <-
  bis_cross_border %>%
  inner_join(controls, by = c("country", "year"))

df_cross_border <-
  df_cross_border %>%
  rename(
    map = imapp,
    map_cumulative = imapp_cum,
    map_credit = credit,
    map_capital = capital,
    map_liquidity = liquidity,
    map_credit_cumulative = credit_cum,
    map_capital_cumulative = capital_cum,
    map_liquidity_cumulative = liquidity_cum
  )

df_cross_border <- df_cross_border %>%
  group_by(year) %>%
  mutate(rank_claims = percent_rank(international_claims),
         rank_gdp = percent_rank(gdp_dollar2015))

df_cross_border <-
  df_cross_border %>%
  group_by(country) %>%
  mutate(lag_claims = dplyr::lag(international_claims, order_by = year),
         lag_loans_deposits = dplyr::lag(international_loans_deposits, order_by = year),
         claims_flow = international_claims - lag_claims,
         loans_deposits_flow = international_loans_deposits - lag_loans_deposits)

df_cross_border <-
  df_cross_border %>%
  select(-L_REP_BANK_TYPE.Type.of.reporting.institutions, -reporting_country, -L_POS_TYPE.Position.type, 
          -OBS_PRE_BREAK.Pre.Break.Observation)

df_cross_border <-
  df_cross_border %>%
  mutate(weighted_map = map*(1+rank_claims),
         weighted_capital_map = map_capital*(1+rank_claims),
         weighted_credit_map = map_credit*(1+rank_claims),
         weighted_liquidity_map = map_liquidity*(1+rank_claims),
         weighted_cumulative_map = map_cumulative*(1+rank_claims),
         weighted_cumulative_capital_map = map_capital_cumulative*(1+rank_claims),
         weighted_cumulative_credit_map = map_credit_cumulative*(1+rank_claims),
         weighted_cumulative_liquidity_map = map_liquidity_cumulative*(1+rank_claims))

df_cross_border <- df_cross_border %>%
  group_by(year) %>%
  mutate(rank_loans_deposits = percent_rank(international_loans_deposits))

df_cross_border <-
  df_cross_border %>%
  group_by(country, year) %>%
  mutate(
    weighted_loans_credit_map = map_credit*(1+rank_loans_deposits),
    weighted_loans_cumulative_credit_map = map_credit_cumulative*(1+rank_loans_deposits)
  )

write.csv(df_cross_border, file = "./datasets/df-bis-claims-cross-border.csv")

