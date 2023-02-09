library("tidyverse")
library("tsibble")

# Leitura do CSV
bis_raw <- as_tibble(read_csv(file = "./data/WEBSTATS_LBS.csv"))

# Aplicando filtros
bis <-
  bis_raw %>%
  filter(
    `L_MEASURE:Measure` == "S: Amounts outstanding / Stocks" &
      `L_DENOM:Currency denomination` == "TO1: All currencies" &
      `L_CURR_TYPE:Currency type of reporting country` == "A: All currencies (=D+F+U)" &
      `L_PARENT_CTY:Parent country` == "5J: All countries" &
      `L_CP_COUNTRY:Counterparty country` == "5J: All countries" &
      `L_CP_SECTOR:Counterparty sector` == "A: All sectors"
    )

# Pivotando o dataframe
bis <-
  bis %>%
  pivot_wider(
    names_from = "L_POSITION:Balance sheet position",
    values_from = "OBS_VALUE:Observation Value"
  )

bis <- 
  bis %>%
  filter(
    `L_INSTR:Type of instruments` == "A: All instruments" |
    `L_INSTR:Type of instruments` == "D: Debt securities" |
    `L_INSTR:Type of instruments` == "G: Loans and deposits"
    ) %>%
  pivot_wider(
    names_from = `L_INSTR:Type of instruments`,
    values_from = c("C: Total claims", "L: Total liabilities")
    ) 

# Renomeando variáveis
bis <-
  bis %>%
  rename(
    claims_all_instruments =  `C: Total claims_A: All instruments`,
    claims_debt_securities = `C: Total claims_D: Debt securities`,
    claims_loans_deposits = `C: Total claims_G: Loans and deposits`,
    liabilities_all_instruments =  `L: Total liabilities_A: All instruments`,
    liabilities_debt_securities = `L: Total liabilities_D: Debt securities`,
    liabilities_loans_deposits = `L: Total liabilities_G: Loans and deposits`,
  )

# Limpando o dataframe
bis <-
  bis %>%
  select(
    -`Dataflow`,
    -`FREQ:Frequency`,
    -`L_MEASURE:Measure`,
    -`L_DENOM:Currency denomination`,
    -`L_CURR_TYPE:Currency type of reporting country`,
    -`L_PARENT_CTY:Parent country`,
    -`L_CP_SECTOR:Counterparty sector`,
    -`L_CP_COUNTRY:Counterparty country`,
    -`DECIMALS:Decimals`,
    -`UNIT_MEASURE:Unit of measure`,
    -`UNIT_MULT:Unit Multiplier`,
    -`TIME_FORMAT:Time Format Code`,
    -`COLLECTION:Collection Indicator`,
    -`ORG_VISIBILITY:Organisation visibility`,
    -`OBS_CONF:Observation Confidentiality`
    ) %>%
  rename(
    reporting_country = `L_REP_CTY:Reporting country`,
    period = `TIME_PERIOD:Time period or range`
  )

# Ajustando as varáveis de país e período
bis <-
  bis %>%
  mutate(
    country = str_sub(reporting_country, start = 5L),
    year_quarter = tsibble::yearquarter(period)
  ) %>%
  filter(
    year_quarter >= tsibble::yearquarter('2000 Q1')
  )

bis <-
  bis %>%
  mutate(
    year = lubridate::year(year_quarter)
  )

bis <-
  bis %>%
  filter(
    str_detect(year_quarter, 'Q4')
  )

# Dataframe CROSS BORDER
bis_cross_border <-
  bis %>%
  filter(`L_REP_BANK_TYPE:Type of reporting institutions` == "A: All reporting banks/institutions (domestic, foreign, consortium and unclassified)" &
           `L_POS_TYPE:Position type` == "N: Cross-border" 
         & `OBS_STATUS:Observation Status` == 'A: Normal value'
           )

bis_cross_border <-
  bis_cross_border %>%
  rename(
    international_claims = claims_all_instruments,
    international_loans_deposits = claims_loans_deposits
  )

write_csv(bis_cross_border, file = "./datasets/bis_cross_border.csv")
