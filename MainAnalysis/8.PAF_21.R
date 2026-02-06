############################################################
# Script: 08_PAF_21.R
# Purpose:
#   Combine imputed country–cause economic burden estimates
#   with GBD population-attributable fractions (PAFs) to
#   compute risk-attributable burdens by REI (risk exposure
#   identifier) and by cause. Export tables for figures/tables.
#
# Inputs:
#   - impburden_cause_country_ui.csv   (imputed burden by cause & country)
#   - IHME PAF files (air & metabolic; age-standardized 2021)
#
# Outputs:
#   - air_rei.csv, air_allcause.csv, air_apm.csv, air_hap.csv, air_aop.csv
#   - met_rei.csv, met_cause.csv, met_rei_cause.csv
############################################################

library(dplyr)

# Load imputed burden by country–cause (val in local currency units)
data <- read.csv("/dssg/home/economic2/outcome/rural outcome/impburden_cause_country_ui.csv")
data <- data %>% select(c('country','cause_name','val','lower','upper'))

# Load GBD PAFs (use age-standardized 2021 values)
paf_air <- read.csv("/dssg/home/economic2/data/data/PAF/air/IHME-GBD_2021_DATA-b54e5652-1.csv")
paf_met <- read.csv("/dssg/home/economic2/data/data/PAF/met/IHME-GBD_2021_DATA-8e7e257f-1.csv")

# Prepare PAF tables: set negative lower bounds to zero, select relevant fields,
# rename GBD location id to 'country' to enable merges with burden table.
paf_air_n <- paf_air %>%
  mutate(val = ifelse(lower < 0, 0, val)) %>%
  select(., c(12, 10, 3, 16)) %>%
  rename(country = location_id) %>%
  arrange(rei_name, cause_name, country) %>%
  filter(cause_name != 'Asthma' & cause_name != 'Blindness and vision loss')

paf_met_n <- paf_met %>%
  mutate(val = ifelse(lower < 0, 0, val)) %>%
  select(., c(12, 10, 3, 16)) %>%
  rename(country = location_id) %>%
  arrange(rei_name, cause_name, country) %>%
  filter(
    cause_name != 'Bladder cancer' &
      cause_name != 'Blindness and vision loss' &
      cause_name != 'Breast cancer' &
      cause_name != 'Multiple myeloma' &
      cause_name != 'Ovarian cancer' &
      cause_name != 'Tracheal, bronchus, and lung cancer'
  )

# ------------------------------------------------------------------
# Air pollution: compute burden by REI (all REI levels) and by cause
# Note: multiply country–cause burden by corresponding PAF; convert to millions
# ------------------------------------------------------------------

# Air: aggregated by REI and country (sums across causes)
air_rei <- data %>%
  merge(., paf_air_n, by = c('cause_name', 'country')) %>%
  mutate(burden = val.x * val.y,
         lower_n = lower * val.y,
         upper_n = upper * val.y) %>%
  arrange(rei_name, country, cause_name) %>%
  group_by(rei_name, country) %>%
  mutate(burden_n = sum(burden) / 1e6,
         lower_n = sum(lower_n) / 1e6,
         upper_n = sum(upper_n) / 1e6) %>%
  select(rei_name, country, burden_n, lower_n, upper_n) %>%
  rename(burden = burden_n, lower = lower_n, upper = upper_n) %>%
  distinct(rei_name, country, .keep_all = TRUE)

write.csv(air_rei, "/dssg/home/economic2/outcome/rural outcome/air_rei.csv")

# Air: all-cause (Air pollution aggregate)
air_allcause <- data %>%
  merge(., paf_air_n, by = c('cause_name', 'country')) %>%
  subset(rei_name == 'Air pollution') %>%
  mutate(burden = val.x * val.y / 1e6,
         lower = lower * val.y / 1e6,
         upper = upper * val.y / 1e6) %>%
  arrange(cause_name, country) %>%
  select(cause_name, country, burden, lower, upper)

write.csv(air_allcause, "/dssg/home/economic2/outcome/rural outcome/air_allcause.csv")

# Air: ambient particulate matter
air_apm <- data %>%
  merge(., paf_air_n, by = c('cause_name', 'country')) %>%
  subset(rei_name == 'Ambient particulate matter pollution') %>%
  mutate(burden = val.x * val.y / 1e6,
         lower = lower * val.y / 1e6,
         upper = upper * val.y / 1e6) %>%
  arrange(cause_name, country) %>%
  select(cause_name, country, burden, lower, upper)

write.csv(air_apm, "/dssg/home/economic2/outcome/rural outcome/air_apm.csv")

# Air: household air pollution from solid fuels
air_hap <- data %>%
  merge(., paf_air_n, by = c('cause_name', 'country')) %>%
  subset(rei_name == 'Household air pollution from solid fuels') %>%
  mutate(burden = val.x * val.y / 1e6,
         lower = lower * val.y / 1e6,
         upper = upper * val.y / 1e6) %>%
  arrange(cause_name, country) %>%
  select(cause_name, country, burden, lower, upper)

write.csv(air_hap, "/dssg/home/economic2/outcome/rural outcome/air_hap.csv")

# Air: ambient ozone pollution
air_aop <- data %>%
  merge(., paf_air_n, by = c('cause_name', 'country')) %>%
  subset(rei_name == 'Ambient ozone pollution') %>%
  mutate(burden = val.x * val.y / 1e6,
         lower = lower * val.y / 1e6,
         upper = upper * val.y / 1e6) %>%
  arrange(cause_name, country) %>%
  select(cause_name, country, burden, lower, upper)

write.csv(air_aop, "/dssg/home/economic2/outcome/rural outcome/air_aop.csv")

# ------------------------------------------------------------------
# Metabolic risks: by REI (aggregated) and by cause
# ------------------------------------------------------------------

met_rei <- data %>%
  merge(., paf_met_n, by = c('cause_name', 'country')) %>%
  mutate(burden = val.x * val.y,
         lower_n = lower * val.y,
         upper_n = upper * val.y) %>%
  arrange(rei_name, country, cause_name) %>%
  group_by(rei_name, country) %>%
  mutate(burden_n = sum(burden) / 1e6,
         lower_n = sum(lower_n) / 1e6,
         upper_n = sum(upper_n) / 1e6) %>%
  select(rei_name, country, burden_n, lower_n, upper_n) %>%
  rename(burden = burden_n, lower = lower_n, upper = upper_n) %>%
  distinct(rei_name, country, .keep_all = TRUE)

write.csv(met_rei, "/dssg/home/economic2/outcome/rural outcome/met_rei.csv")

# Metabolic: by cause (Metabolic risks aggregate)
met_cause <- data %>%
  merge(., paf_met_n, by = c('cause_name', 'country')) %>%
  subset(rei_name == 'Metabolic risks') %>%
  mutate(burden = val.x * val.y / 1e6,
         lower = lower * val.y / 1e6,
         upper = upper * val.y / 1e6) %>%
  arrange(cause_name, country) %>%
  select(cause_name, country, burden, lower, upper)

write.csv(met_cause, "/dssg/home/economic2/outcome/rural outcome/met_cause.csv")

# Metabolic: REI × cause aggregation (useful for stacked plots)
met_rei_cause <- data %>%
  merge(., paf_met_n, by = c('cause_name', 'country')) %>%
  mutate(burden = val.x * val.y,
         lower_n = lower * val.y,
         upper_n = upper * val.y) %>%
  arrange(rei_name, cause_name, country) %>%
  group_by(rei_name, cause_name) %>%
  mutate(burden_n = sum(burden) / 1e6,
         lower_n = sum(lower_n) / 1e6,
         upper_n = sum(upper_n) / 1e6) %>%
  select(rei_name, cause_name, burden_n, lower_n, upper_n) %>%
  rename(burden = burden_n, lower = lower_n, upper = upper_n) %>%
  distinct(rei_name, cause_name, .keep_all = TRUE)

write.csv(met_rei_cause, "/dssg/home/economic2/outcome/rural outcome/met_rei_cause.csv")
