############################################################
# Script: 09_table_stratified_analysis.R
# Purpose:
#   Prepare stratified tables for manuscript (country-level,
#   income-group, region summaries) and per-cause tables for
#   air and metabolic risk results. Outputs are saved as CSVs
#   for inclusion in tables and figures.
#
# Inputs:
#   - population_both.xlsx (UN population estimates + projections)
#   - air_rei.csv, air_allcause.csv, air_apm.csv, air_hap.csv, air_aop.csv
#   - gdp_161950_n.csv, healthexp/time series, income group CLASS.xlsx
#
# Outputs:
#   - pop_country_mean.csv
#   - air_country_all.csv, air_country_apm.csv, air_country_hap.csv, air_country_aop.csv
#   - air_inc&reg_*.csv
#   - air_allcause_grp.csv, air_apm_grp.csv, air_hap_grp.csv
#   - met_cause_grp.csv, met_rei_grp.csv
############################################################

library(readxl)
library(dplyr)
library(tidyr)

# -----------------------------
# Compute country-level mean population (2020–2050)
# -----------------------------
num1 <- read_xlsx("/dssg/home/economic2/data/data/population/population_both.xlsx",
                  sheet = 'Estimates', col_names = FALSE)
colname <- read_xlsx("/dssg/home/economic2/data/data/population/population_both.xlsx",
                     sheet = 'Estimates', col_names = FALSE, n_max = 1)
colnames(num1) <- colname
num1 <- num1[-1,]

num2 <- read_xlsx("/dssg/home/economic2/data/data/population/population_both.xlsx",
                  sheet = 'Medium variant', col_names = FALSE)
colnames(num2) <- colname
num2 <- num2[-1,]

num <- num1 %>% rbind(num2)

num_n <- num %>%
  subset(Year >= 2020 & Year <= 2050) %>%
  rename(country = `Region, subregion, country or area *`, WBcode = `ISO3 Alpha-code`, year = Year) %>%
  select(country, WBcode, 11:32) %>%
  arrange(country, year) %>%
  mutate(across(3:24, as.numeric)) %>%
  pivot_longer(cols = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                        '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79',
                        '80-84','85-89','90-94','95-99','100+'),
               names_to = "age", values_to = "number") %>%
  group_by(country, year) %>%
  mutate(num = sum(number)) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  group_by(country) %>%
  mutate(num_m = mean(num) * 1000) %>%   # convert mean population to persons
  select(country, WBcode, num_m) %>%
  distinct(country, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-country)

write.csv(num_n, '/dssg/home/economic2/outcome/rural outcome/pop_country_mean.csv')

# -----------------------------
# Load intermediate results and crosswalks
# -----------------------------
cost_air <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_rei.csv') # in million$
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(3,4)) %>% rename(WBcode = Country.Code, country = location_id)
country_name <- country %>% select(c(2,3)) %>% rename(WBcode = Country.Code, country = location_name)

gdp <- read.csv('/dssg/home/economic2/outcome/rural outcome/all/gdp_161950_n.csv')
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  group_by(WBcode) %>%
  mutate(val.gdp = sum(val.gdp) / 1e6) %>%  # total (2020–2050) in million$
  select(-year) %>%
  distinct(WBcode, country, .keep_all = TRUE)

inc_grp <- read_xlsx("/dssg/home/economic2/data/data/income group/CLASS.xlsx", sheet = 1)
inc_grp_n <- inc_grp %>%
  select(c(2:4)) %>%
  head(n = 218) %>%
  rename(WBcode = Code, income = `Income group`) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  mutate(income = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'), 'Others', income),
         Region = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'), 'Others', Region))

# -----------------------------
# Helper: build country-level table for a given REI name
# -----------------------------
country_table <- function(data, rei) {
  data_country <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = 'country') %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    merge(num_n, by = 'WBcode') %>%
    arrange(Region, country) %>%
    subset(rei_name == rei) %>%
    mutate(
      num_m = num_m / 1e6,    # convert population to millions
      percent_val = round(burden / val.gdp * 100, 3),
      burden = round(burden),
      percent_lower = round(lower / val.gdp * 100, 3),
      lower = round(lower),
      percent_upper = round(upper / val.gdp * 100, 3),
      upper = round(upper),
      capita_val = round(burden / num_m),
      capita_lower = round(lower / num_m),
      capita_upper = round(upper / num_m),
      cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
      percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
      capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
    ) %>%
    merge(country_name, by = 'WBcode') %>%
    select(c('Region', 'country.y', 'cost', 'percent', 'capita'))
  
  return(data_country)
}

air_country_all <- country_table(cost_air, 'Air pollution')
write.csv(air_country_all, "/dssg/home/economic2/outcome/rural outcome/air_country_all.csv")

air_country_apm <- country_table(cost_air, 'Ambient particulate matter pollution')
write.csv(air_country_apm, "/dssg/home/economic2/outcome/rural outcome/air_country_apm.csv")

air_country_hap <- country_table(cost_air, 'Household air pollution from solid fuels')
write.csv(air_country_hap, "/dssg/home/economic2/outcome/rural outcome/air_country_hap.csv")

air_country_aop <- country_table(cost_air, 'Ambient ozone pollution')
write.csv(air_country_aop, "/dssg/home/economic2/outcome/rural outcome/air_country_aop.csv")

# -----------------------------
# Helper: build grouped summaries (global, income-group, region)
# -----------------------------
group_summary <- function(data, rei) {
  # Global summary (aggregate across all countries)
  data_global <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = c('country')) %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    merge(num_n, by = 'WBcode') %>%
    subset(rei_name == rei) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp),
      num = sum(num_m)
    ) %>%
    distinct(burden, .keep_all = TRUE) %>%
    select(c('Region', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
    mutate(
      burden = round(burden / 1e3),
      lower = round(lower / 1e3),
      upper = round(upper / 1e3),
      gdp = gdp / 1e3,
      num = num / 1e9,
      percent_val = round(burden / gdp * 100, 3),
      percent_lower = round(lower / gdp * 100, 3),
      percent_upper = round(upper / gdp * 100, 3),
      capita_val = round(burden / num),
      capita_lower = round(lower / num),
      capita_upper = round(upper / num),
      cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
      percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
      capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
    ) %>%
    select(c('cost', 'percent', 'capita'))
  
  # Income-group summary
  data_inc <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = c('country')) %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    merge(num_n, by = 'WBcode') %>%
    arrange(income, Region) %>%
    subset(rei_name == rei) %>%
    group_by(income) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp),
      num = sum(num_m)
    ) %>%
    distinct(income, .keep_all = TRUE) %>%
    select(c('income', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
    mutate(
      burden = round(burden / 1e3),
      lower = round(lower / 1e3),
      upper = round(upper / 1e3),
      gdp = gdp / 1e3,
      num = num / 1e9,
      percent_val = round(burden / gdp * 100, 3),
      percent_lower = round(lower / gdp * 100, 3),
      percent_upper = round(upper / gdp * 100, 3),
      capita_val = round(burden / num),
      capita_lower = round(lower / num),
      capita_upper = round(upper / num),
      cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
      percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
      capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
    ) %>%
    select(c('income', 'cost', 'percent', 'capita')) %>%
    rbind(data_global)
  
  # Region summary appended to income summary
  data_region <- data %>%
    select(-X) %>%
    merge(., inc_grp_n, by = c('country')) %>%
    merge(gdp_country, by = c('country', 'WBcode')) %>%
    merge(num_n, by = 'WBcode') %>%
    arrange(Region) %>%
    subset(rei_name == rei) %>%
    group_by(Region) %>%
    mutate(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(val.gdp),
      num = sum(num_m)
    ) %>%
    distinct(Region, .keep_all = TRUE) %>%
    select(c('Region', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
    mutate(
      burden = round(burden / 1e3),
      lower = round(lower / 1e3),
      upper = round(upper / 1e3),
      gdp = gdp / 1e3,
      num = num / 1e9,
      percent_val = round(burden / gdp * 100, 3),
      percent_lower = round(lower / gdp * 100, 3),
      percent_upper = round(upper / gdp * 100, 3),
      capita_val = round(burden / num),
      capita_lower = round(lower / num),
      capita_upper = round(upper / num),
      cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
      percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
      capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
    ) %>%
    select(c('Region', 'cost', 'percent', 'capita')) %>%
    rbind(data_inc)
  
  return(data_region)
}

air_group_all <- group_summary(cost_air, 'Air pollution')
write.csv(air_group_all, "/dssg/home/economic2/outcome/rural outcome/air_inc&reg_all.csv")

air_group_apm <- group_summary(cost_air, 'Ambient particulate matter pollution')
write.csv(air_group_apm, "/dssg/home/economic2/outcome/rural outcome/air_inc&reg_apm.csv")

air_group_hap <- group_summary(cost_air, 'Household air pollution from solid fuels')
write.csv(air_group_hap, "/dssg/home/economic2/outcome/rural outcome/air_inc&reg_hap.csv")

air_group_aop <- group_summary(cost_air, 'Ambient ozone pollution')
write.csv(air_group_aop, "/dssg/home/economic2/outcome/rural outcome/air_inc&reg_aop.csv")
# -----------------------------
# Per-cause air results (Table 2 variants)
# -----------------------------
air_allcause <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_allcause.csv')
air_allcause_grp <- air_allcause %>%
  select(-X) %>%
  merge(gdp_country, by = c('country')) %>%
  merge(num_n, by = 'WBcode') %>%
  arrange(cause_name) %>%
  group_by(cause_name) %>%
  mutate(
    burden = round(sum(burden), 2),
    lower = round(sum(lower), 2),
    upper = round(sum(upper), 2),
    gdp = sum(val.gdp),
    num = sum(num_m)
  ) %>%
  distinct(burden, .keep_all = TRUE) %>%
  select(c('cause_name', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
  mutate(
    burden = round(burden / 1e3),
    lower = round(lower / 1e3),
    upper = round(upper / 1e3),
    gdp = gdp / 1e3,
    num = num / 1e9,
    percent_val = round(burden / gdp * 100, 3),
    percent_lower = round(lower / gdp * 100, 3),
    percent_upper = round(upper / gdp * 100, 3),
    capita_val = round(burden / num),
    capita_lower = round(lower / num),
    capita_upper = round(upper / num),
    cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
    percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
    capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
  ) %>%
  select(c('cause_name', 'cost', 'percent', 'capita'))

write.csv(air_allcause_grp, "/dssg/home/economic2/outcome/rural outcome/air_allcause_grp.csv")

# Repeat for APM and HAP
air_apm <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_apm.csv')
air_apm_grp <- air_apm %>%
  select(-X) %>%
  merge(gdp_country, by = c('country')) %>%
  merge(num_n, by = 'WBcode') %>%
  arrange(cause_name) %>%
  group_by(cause_name) %>%
  mutate(
    burden = round(sum(burden), 2),
    lower = round(sum(lower), 2),
    upper = round(sum(upper), 2),
    gdp = sum(val.gdp),
    num = sum(num_m)
  ) %>%
  distinct(burden, .keep_all = TRUE) %>%
  select(c('cause_name', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
  mutate(
    burden = round(burden / 1e3),
    lower = round(lower / 1e3),
    upper = round(upper / 1e3),
    gdp = gdp / 1e3,
    num = num / 1e9,
    percent_val = round(burden / gdp * 100, 3),
    percent_lower = round(lower / gdp * 100, 3),
    percent_upper = round(upper / gdp * 100, 3),
    capita_val = round(burden / num),
    capita_lower = round(lower / num),
    capita_upper = round(upper / num),
    cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
    percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
    capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
  ) %>%
  select(c('cause_name', 'cost', 'percent', 'capita'))

write.csv(air_apm_grp, "/dssg/home/economic2/outcome/rural outcome/air_apm_grp.csv")

air_hap <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_hap.csv')
air_hap_grp <- air_hap %>%
  select(-X) %>%
  merge(gdp_country, by = c('country')) %>%
  merge(num_n, by = 'WBcode') %>%
  arrange(cause_name) %>%
  group_by(cause_name) %>%
  mutate(
    burden = round(sum(burden), 2),
    lower = round(sum(lower), 2),
    upper = round(sum(upper), 2),
    gdp = sum(val.gdp),
    num = sum(num_m)
  ) %>%
  distinct(burden, .keep_all = TRUE) %>%
  select(c('cause_name', 'burden', 'lower', 'upper', 'gdp', 'num')) %>%
  mutate(
    burden = round(burden / 1e3),
    lower = round(lower / 1e3),
    upper = round(upper / 1e3),
    gdp = gdp / 1e3,
    num = num / 1e9,
    percent_val = round(burden / gdp * 100, 3),
    percent_lower = round(lower / gdp * 100, 3),
    percent_upper = round(upper / gdp * 100, 3),
    capita_val = round(burden / num),
    capita_lower = round(lower / num),
    capita_upper = round(upper / num),
    cost = paste(burden, " ", "(", lower, "-", upper, ")", sep = ""),
    percent = paste(percent_val, " ", "(", percent_lower, "-", percent_upper, ")", sep = ""),
    capita = paste(capita_val, " ", "(", capita_lower, "-", capita_upper, ")", sep = "")
  ) %>%
  select(c('cause_name', 'cost', 'percent', 'capita'))

write.csv(air_hap_grp, "/dssg/home/economic2/outcome/rural outcome/air_hap_grp.csv")
