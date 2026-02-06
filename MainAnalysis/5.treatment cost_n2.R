############################################################
# Script: 05_treatment_cost_n2.R
# Purpose: Estimate country-level treatment costs for 2019–2050 by scaling US disease-level
#          spending to other countries using prevalence ratios and projecting costs using
#          per-capita health expenditure growth.
# Inputs:
#   - gdp_161950.csv (GDP baseline)
#   - API_SH.XPD.CHEX.GD.ZS... (health expenditure as % of GDP)
#   - USA_cost.CSV (IHME disease-level spending for the USA)
#   - IHME prevalence data (GBD 2021)
#   - GDP per capita series and healthexp_161950.csv
# Outputs:
#   - direct cost_2019_n2.csv (direct treatment spending in 2019 USD)
#   - treatment cost_1950_n2.csv (projected treatment costs in 2017 international dollars, in billions)
############################################################

library(dplyr)
library(readxl)

# Load GDP data and convert to billions (2019 snapshot)
data <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/gdp_161950.csv")
seldata <- data %>%
  mutate(gdp_b = val / 1e9) %>%
  select(., c(2,4,5)) %>%
  subset(year == 2019)

# Load country identifiers (complete list of 204 countries and WB codes)
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id.csv")
country_old <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")

# Read health expenditure (% of GDP) and compute total health spending (2019)
health <- read.csv('/dssg/home/economic2/data/data/health expenditure/API_SH.XPD.CHEX.GD.ZS_DS2_en_csv_v2_5715587.csv')
health19 <- health %>%
  select(., c(1,2,64)) %>%
  rename(WBcode = Country.Code, location_name = Country.Name)

# Merge GDP and health expenditure; convert to 2019 USD using GDP deflator indices
seldata_n <- seldata %>%
  merge(., health19, by = "WBcode", all.x = TRUE) %>%
  filter(!is.na(X2019)) %>%
  mutate(cost = gdp_b * X2019 / 100 / 102.76 * 106.85) # convert to 2019$ (billion)

# Load US disease-level treatment spending from IHME and scale to billions (2019)
data_treat <- read.csv('/dssg/home/economic2/data/data/treatment/IHME 2021/USA_cost.CSV')
data_treat19 <- data_treat %>%
  subset(year_id == 2019 & location_name == "United States" &
           payer == "All payers" & toc == "All toc" & age_name == "Age/sex-standardized") %>%
  select(c("cause_name", 'spend_mean', 'spend_lower', 'spend_upper')) %>%
  mutate(spend_mean = spend_mean / 1e9,
         spend_lower = spend_lower / 1e9,
         spend_upper = spend_upper / 1e9)

# Total US health spending used as denominator for share calculation (in 2019$ billion)
USAcost <- 2433.27

# Compute US disease spending share
disease <- data_treat19 %>%
  mutate(share = spend_mean / USAcost) %>%
  filter(!is.na(share))

# Load prevalence data and merge with country list
prev <- read.csv("/dssg/home/economic2/data/data/prevalence/IHME-GBD_2021_DATA-9000cc9a-1.csv")
prev_n <- prev %>%
  merge(country, ., by = 'location_id', all.x = TRUE) %>%
  rename(WBcode = Country.Code)

# Merge GDP/health spending data with prevalence
seldata_n2 <- seldata_n %>%
  merge(prev_n, ., by = c("WBcode", 'year'), all.y = TRUE) %>%
  select(., c('WBcode', 'cause_name', 'year', 'val', 'cost'))

# Function to estimate country-specific spending share from prevalence ratios
# Note: the function is provided for reference but not used below in the vectorized implementation
cost <- function(data1, data2, disease, country) {
  prevn <- data1$val[data1$cause_name == "disease" & data1$WBcode == "country"]
  prevUSA <- data1$val[data1$cause_name == "disease" & data1$WBcode == "USA"]
  shareUSA <- data2$share[data2$cause_name == "disease"]
  sharen <- prevn * shareUSA / prevUSA
  return(sharen)
}

# Build country–disease shares by scaling US shares with country prevalence ratios
country_id <- unique(seldata_n2$WBcode)
disease_name <- unique(disease$cause_name)
loss_data <- data.frame()

for (i in 1:length(country_id)) {
  current_country <- country_id[i]
  if (current_country == "USA") next  # skip USA (handled separately)
  for (j in 1:length(disease_name)) {
    current_disease <- disease_name[j]
    prev_country <- seldata_n2$val[seldata_n2$cause_name == current_disease & seldata_n2$WBcode == current_country]
    prev_USA <- seldata_n2$val[seldata_n2$cause_name == current_disease & seldata_n2$WBcode == "USA"]
    share_USA <- disease$share[disease$cause_name == current_disease]
    if (length(prev_country) > 0 & length(prev_USA) > 0 & length(share_USA) > 0) {
      share_est <- prev_country * share_USA / prev_USA
      loss_data <- rbind(loss_data, data.frame(WBcode = current_country, cause_name = current_disease, share = share_est))
    }
  }
}

# Construct 2019 treatment expense estimates
non_USA_expense <- seldata_n2 %>%
  filter(WBcode != "USA") %>%
  merge(loss_data, by = c("WBcode", "cause_name")) %>%
  mutate(expense = cost * share) %>%
  select(WBcode, cause_name, year, expense)

# USA expenses taken directly from IHME (spend_mean already in billions)
USA_expense <- disease %>%
  filter(!is.na(spend_mean)) %>%
  mutate(WBcode = "USA", year = 2019, expense = spend_mean) %>%
  select(WBcode, cause_name, year, expense)

# Combine US and non-US expenses: direct treatment spending in 2019 USD (billion)
health_expense <- rbind(non_USA_expense, USA_expense) # ~178 countries, 44 diseases in 2019$

write.csv(health_expense, "/dssg/home/economic2/outcome/rural outcome/direct cost_2019_n2.csv")

# Project treatment costs forward by assuming treatment costs grow at the same rate
# as per-capita health expenditure (converted to 2017$ baseline)

# Load GDP per capita (2021 USD) and convert to 2017$ using GDP deflators
gdp21 <- read.csv("/dssg/home/economic2/data/data/GDP/GDP_per capital_in 2021 usd.CSV")
gdp21_n <- gdp21 %>%
  merge(., country_old, by = "location_name", all.y = TRUE) %>%
  select(c('Country.Code', 'year', 'gdp_ppp_mean')) %>%
  rename(WBcode = Country.Code, gdp21 = gdp_ppp_mean) %>%
  arrange(WBcode, year) %>%
  subset(year >= 2019) %>%
  mutate(gdp17 = gdp21 / 113.21 * 102.92) %>%
  select(c('WBcode', 'year', 'gdp17'))  # in 2017$

# Load time series of health expenditure shares (2016–2050)
healthexp_161950 <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/healthexp_161950.csv")
healthexp_161950 <- healthexp_161950[, c(2:4)]

# Compute per-capita health expenditure (in 2017$)
exp_cap <- gdp21_n %>%
  merge(healthexp_161950, ., by = c('WBcode', 'year')) %>%
  mutate(exp = val * gdp17)

# Compute year-on-year growth rate of per-capita health expenditure by country
exp_growth <- exp_cap %>%
  group_by(WBcode) %>%
  mutate(exp_growth_rate = (exp - lag(exp, default = first(exp))) / lag(exp, default = first(exp))) %>%
  select(-exp)

# Extend health_expense to include 2020–2050 rows with expense = 0 (placeholders)
cost_extended <- health_expense
years_to_add <- 2020:2050
for (year in years_to_add) {
  new_data <- health_expense
  new_data$year <- year
  new_data$expense <- 0
  cost_extended <- rbind(cost_extended, new_data)
}

# Merge growth rates and propagate expenses forward using per-capita health expenditure growth
cost_1950 <- cost_extended %>%
  merge(exp_growth, by = c("WBcode", "year")) %>%
  arrange(., cause_name, WBcode, year) %>%
  group_by(., cause_name, WBcode) %>%
  mutate(grp = cur_group_id()) %>%
  select(., c(1:4, 7, 8))

proj_cost <- as.data.frame(matrix(nrow = 0, ncol = 6))
for (i in 1:n_distinct(cost_1950$grp)) {
  tmp <- subset(cost_1950, cost_1950$grp == i)
  for (j in 2:nrow(tmp)) {
    tmp[j, 4] <- tmp[j - 1, 4] * (1 + tmp[j, 5])
  }
  proj_cost <- rbind(proj_cost, tmp)
}

# Keep 2019–2050 and convert from 2019$ to 2017$ using GDP deflators
proj_cost_n <- proj_cost %>%
  select(., c(1:4)) %>%
  subset(., year >= 2019 & year <= 2050) %>%
  arrange(WBcode, cause_name, year) %>%
  mutate(expense = expense / 106.85 * 102.76) # convert from 2019$ to 2017$

# Save projected treatment costs (in 2017$ billion). Note: convert units out of billions if needed.
write.csv(proj_cost_n, "/dssg/home/economic2/outcome/rural outcome/treatment cost_1950_n2.csv")
