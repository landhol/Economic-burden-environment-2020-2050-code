## Nt: number of individuals in each age–sex group (a, s)
## lt: labor-force participation rate
## ht: human capital endowment

## Parameters
# η1: Mincer elasticity of education = 0.091
# η2: First-degree Mincer elasticity of experience = 0.1301
# η3: Second-degree Mincer elasticity of experience = −0.0023
# yet(a,s): average years of education
# Age (a) is approximated by the midpoint of each age group

labor <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/labor participation.csv")
edu <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/edu level_m.csv")

## Calculate human capital endowment (ht)
library(dplyr)

ht <- edu %>%
  mutate(ageto = ifelse(ageto == 999, 70, ageto)) %>%
  mutate(., ht = exp(
    0.091 * Interpolated_Data +
      0.1301 * (((ageto + agefrom) / 2) - Interpolated_Data - 5) -
      0.0023 * ((((ageto + agefrom) / 2) - Interpolated_Data - 5)^2)
  )) %>%
  subset(., year_n >= 2019 & year_n <= 2050) %>%
  select(., c(2:4, 6:8)) %>%
  mutate(sex = ifelse(sex == "FALSE", "female", sex),
         sex = ifelse(sex == "M", "male", sex)) %>%
  mutate(agefrom = ifelse(agefrom == "15", "15-19", agefrom),
         agefrom = ifelse(agefrom == "20", "20-24", agefrom),
         agefrom = ifelse(agefrom == "25", "25-29", agefrom),
         agefrom = ifelse(agefrom == "30", "30-34", agefrom),
         agefrom = ifelse(agefrom == "35", "35-39", agefrom),
         agefrom = ifelse(agefrom == "40", "40-44", agefrom),
         agefrom = ifelse(agefrom == "45", "45-49", agefrom),
         agefrom = ifelse(agefrom == "50", "50-54", agefrom),
         agefrom = ifelse(agefrom == "55", "55-59", agefrom),
         agefrom = ifelse(agefrom == "60", "60-64", agefrom),
         agefrom = ifelse(agefrom == "65", "65+", agefrom)) %>%
  rename(age = agefrom, year = year_n) %>%
  select(., -5)  ## 146 country–age–sex–year observations retained

lt <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/labor participation.csv")
lt_n <- lt[, c(2:6)]  # Labor-force participation rates for 204 countries

## Merge datasets and calculate aggregate human capital (Ht)
nt <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/number_1950.csv")
nt_n <- nt[, c(2:6)]  # Population counts for 236 countries

data_merged <- merge(ht, lt_n, by = c('WBcode','sex','age','year'), all.y = TRUE)
data_merged <- merge(data_merged, nt_n, by = c('WBcode','sex','age','year'), all.x = TRUE)

data_n <- data_merged %>%
  arrange(., WBcode, year, sex, age) %>%
  group_by(., WBcode, year) %>%
  mutate(Ht = ht * val * number,
         Ht_a = sum(Ht)) %>%
  select(., c(1,4,9)) %>%
  distinct(WBcode, year, .keep_all = TRUE)  # Ht_a is the aggregate human capital; 111 countries

write.csv(data_n,"/dssg/home/economic2/outcome/rural outcome/all/Ht.csv")

## Calculate aggregate human capital under counterfactual scenarios
lt_c <- read.csv("/dssg/home/economic2/outcome/rural outcome/labor_cf_1950_ui.csv")
lt_cn <- lt_c[, c(2:9)]

nt_c <- read.csv("/dssg/home/economic2/outcome/rural outcome/number_cf_1950_ui.csv")
nt_cn <- nt_c[, c(2:9)]

data_merged <- merge(lt_cn, ht, by = c('WBcode','sex','age','year'), all.x = TRUE)
data_merged <- merge(data_merged, nt_cn, by = c('cause_name','WBcode','sex','age','year'), all.x = TRUE)

data_c <- data_merged %>%
  arrange(., cause_name, WBcode, year, sex, age) %>%
  group_by(., cause_name, WBcode, year) %>%
  mutate(Ht_val = ht * L_val * N_val,
         Ht_val_a = sum(Ht_val),
         Ht_val_a = ifelse(Ht_val_a == 0, NA, Ht_val_a),
         Ht_lower = ht * L_lower * N_lower,
         Ht_lower_a = sum(Ht_lower),
         Ht_lower_a = ifelse(Ht_lower_a == 0, NA, Ht_lower_a),
         Ht_upper = ht * L_upper * N_upper,
         Ht_upper_a = sum(Ht_upper),
         Ht_upper_a = ifelse(Ht_upper_a == 0, NA, Ht_upper_a)) %>%
  select(., c(1,2,5,14,16,18)) %>%
  distinct(cause_name, WBcode, year, .keep_all = TRUE)  # Aggregate Ht under counterfactual scenarios

write.csv(data_c,"/dssg/home/economic2/outcome/rural outcome/Ht_cf.csv")
