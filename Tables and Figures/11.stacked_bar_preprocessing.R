###########################################################
# Script: 11_stacked_bar_preprocessing.R
# Purpose:
#   Preprocess country-level macroeconomic burden results
#   for stacked-bar plots. Produces GDP-percent and burden
#   (billion USD) tables by source (Air pollution, APM, HAP, AOP)
#   across Global / Income group / Region / Top-10 countries.
#
# Notes:
#   - File paths are preserved from the original pipeline.
#   - Only comments were converted to English and standardized.
###########################################################

library(dplyr)
library(readxl)

# Load input datasets
cost_air <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_rei.csv') # in million USD

country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(3,4)) %>% rename(WBcode = Country.Code, country = location_id)
country_name <- country %>% select(c(2,3)) %>% rename(WBcode = Country.Code, country = location_name)

# Aggregate GDP by country (2019-2050) and convert to million USD
gdp <- read.csv('/dssg/home/economic2/outcome/rural outcome/all/gdp_161950_n.csv')
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  group_by(WBcode) %>%
  mutate(val.gdp = sum(val.gdp) / 10^6) %>%   # convert to million USD
  select(-year) %>%
  distinct(WBcode, country, .keep_all = TRUE)

# Income group mapping (used for income-group summaries)
inc_grp <- read_xlsx("/dssg/home/economic2/data/data/income group/CLASS.xlsx", sheet = 1)
inc_grp_n <- inc_grp %>%
  select(c(2:4)) %>%
  head(n = 218) %>%
  rename(WBcode = Code, income = `Income group`) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  mutate(
    income = ifelse(WBcode %in% c('COK', 'NIU', 'TKL', 'VEN'), 'Others', income),
    Region = ifelse(WBcode %in% c('COK', 'NIU', 'TKL', 'VEN'), 'Others', Region)
  )

# -----------------------------------------------------------------------------
# fig_pile:
#   Compute percent of GDP for stacked bar plotting.
#   Returns percent-of-GDP values for the requested REI across:
#     - Top 10 countries by total risk (country-level)
#     - Region-level aggregates
#     - Income-group aggregates (ordered: High, Upper middle, Lower middle, Low)
#     - Global aggregate
# -----------------------------------------------------------------------------
fig_pile <- function(data, rei) {
  # Identify top-10 countries by aggregate 'Air pollution' burden
  air_country <- cost_air %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    subset(rei_name == 'Air pollution') %>%
    arrange(desc(burden)) %>%
    slice(1:10)
  
  country_code <- unique(air_country$WBcode)
  
  # Country-level percent (only top-10 countries)
  data_country <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    filter(WBcode %in% country_code) %>%
    arrange(rei_name, WBcode) %>%
    group_by(rei_name, WBcode) %>%
    mutate(percent = burden / val.gdp * 100) %>%
    arrange(desc(burden)) %>%
    select(c('rei_name', 'WBcode', 'percent')) %>%
    merge(country_name, by = 'WBcode') %>%
    select(-WBcode) %>%
    rename(location = country)
  
  # Region-level percent
  data_region <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name, Region) %>%
    group_by(rei_name, Region) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(rei_name, Region, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100) %>%
    select(c('rei_name', 'Region', 'percent')) %>%
    rename(location = Region) %>%
    rbind(data_country)
  
  # Income-group percent (ensure ordering)
  data_inc <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name, income, Region) %>%
    group_by(rei_name, income) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(rei_name, income, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100) %>%
    select(c('rei_name', 'income', 'percent')) %>%
    rename(location = income) %>%
    mutate(location = factor(location, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>%
    arrange(location) %>%
    rbind(data_region)
  
  # Global percent
  data_global <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name) %>%
    group_by(rei_name) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(burden, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100) %>%
    select(c('rei_name', 'percent')) %>%
    mutate(location = 'Global') %>%
    rbind(data_inc) %>%
    filter(!c(location == 'Others')) %>%
    subset(rei_name == rei)
  
  return(data_global)
}

# Prepare percent-of-GDP stacked data for three air sub-sources
air_apm_pile <- fig_pile(cost_air, 'Ambient particulate matter pollution')
air_hap_pile <- fig_pile(cost_air, 'Household air pollution from solid fuels')
air_aop_pile <- fig_pile(cost_air, 'Ambient ozone pollution')

# Combine AOP/HAP/APM for plotting
air_fig_pile <- air_aop_pile %>% rbind(air_hap_pile) %>% rbind(air_apm_pile)
write.csv(air_fig_pile, '/dssg/home/economic2/outcome/rural outcome/air_fig_pile.csv')

# -----------------------------------------------------------------------------
# fig_data:
#   Produce a dataset for stacked-bar panels where burden is shown in billion USD
#   and percent-of-GDP for the requested REI across Global / Income / Region / Top-10.
# -----------------------------------------------------------------------------
fig_data <- function(data, rei) {
  # Identify top-10 countries by the specified REI
  data_country <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    subset(rei_name == rei) %>%
    arrange(desc(burden)) %>%
    slice(1:10)
  
  country_code <- unique(data_country$WBcode)
  
  # Country-level: keep top-10 and compute percent; convert burden to billion USD
  data_country <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    filter(WBcode %in% country_code) %>%
    arrange(rei_name, WBcode) %>%
    group_by(rei_name, WBcode) %>%
    mutate(percent = burden / val.gdp * 100, burden = burden / 1000) %>%  # million -> billion
    select(c('rei_name', 'WBcode', 'burden', 'percent')) %>%
    merge(country_name, by = 'WBcode') %>%
    select(-WBcode) %>%
    rename(location = country) %>%
    arrange(desc(burden))
  
  # Region-level: aggregate and convert burden to billion
  data_region <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name, Region) %>%
    group_by(rei_name, Region) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(rei_name, Region, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100, burden = burden / 1000) %>%
    select(c('rei_name', 'Region', 'burden', 'percent')) %>%
    rename(location = Region) %>%
    rbind(data_country)
  
  # Income-level: aggregate and convert burden to billion
  data_inc <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name, income, Region) %>%
    group_by(rei_name, income) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(rei_name, income, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100, burden = burden / 1000) %>%
    select(c('rei_name', 'income', 'burden', 'percent')) %>%
    rename(location = income) %>%
    mutate(location = factor(location, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>%
    arrange(location) %>%
    rbind(data_region)
  
  # Global-level summary (burden in billion USD)
  data_global <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    arrange(rei_name) %>%
    group_by(rei_name) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp)
    ) %>%
    distinct(burden, .keep_all = TRUE) %>%
    mutate(percent = burden / gdp * 100, burden = burden / 1000) %>%
    select(c('rei_name', 'burden', 'percent')) %>%
    mutate(location = 'Global') %>%
    rbind(data_inc) %>%
    filter(!c(location == 'Others')) %>%
    subset(rei_name == rei)
  
  return(data_global)
}

# Prepare datasets for stacked-barpanels (both percent-of-GDP and burden-in-billion)
air_all <- fig_data(cost_air, 'Air pollution')
air_apm <- fig_data(cost_air, 'Ambient particulate matter pollution')
air_hap <- fig_data(cost_air, 'Household air pollution from solid fuels')
air_aop <- fig_data(cost_air, 'Ambient ozone pollution')

air_fig3 <- air_aop %>% rbind(air_hap) %>% rbind(air_apm) %>% rbind(air_all)
write.csv(air_fig3, '/dssg/home/economic2/outcome/rural outcome/air_fig3.csv')

