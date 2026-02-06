############################################################
# Script: 07_imputation_new.R
# Purpose:
#   Impute missing country–cause economic burden estimates by
#   leveraging relationships between DALY rates and observed
#   burden/GDP ratios. Produce filled country-level burden
#   estimates for 2020–2050.
#
# Key steps:
#   1. Project DALY rates (2010–2019 growth) to 2020–2050 and
#      compute age-standardized DALY rates (2019–2050 average).
#   2. Calculate observed burden-to-GDP ratios for 2020–2050.
#   3. Fit linear models of burden/GDP ratio on standardized DALY
#      rates and use models to impute missing country values.
#   4. For causes with weak statistical relationships, perform
#      imputation within income groups and fallback to medians.
#
# Inputs:
#   - DALY rate files (GBD 2010–2021)
#   - population (age/sex) for standardization
#   - observed burden outputs (val/lower/upper) for 2020–2050
#   - GDP and income-group crosswalks
#
# Outputs:
#   - impburden_cause_country_ui.csv (imputed burden by cause-country)
#   - impburden_cause_tot_ui.csv (imputed total burden by cause)
############################################################

library(data.table)
library(dplyr)
library(progress)
library(readxl)

# -----------------------------
# Load DALY rate files (2010–2021) and combine
# -----------------------------
dirname <- dir("/dssg/home/economic2/data/data/DALYs/daly_rate/")
file <- paste0("/dssg/home/economic2/data/data/DALYs/daly_rate/", dirname)

daly_1021 <- as.data.frame(matrix(nrow = 0, ncol = 16))
for (a in file) {
  data <- fread(a)
  daly_1021 <- rbind(daly_1021, data)
}

# Load country id crosswalk (use old IDs to match some inputs)
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(., c(3,4)) %>% rename(WBcode = Country.Code)

# Subset and arrange DALY data (prepare for projection)
daly_n <- daly_1021 %>%
  merge(., country_n, by = 'location_id') %>%
  select(., c(10,17,13,6,8,14)) %>%
  arrange(cause_name, WBcode, year, sex_name)
# Note: 'val' currently represents rate (per 100,000 in GBD); projection treats it as rate

# -----------------------------
# Projection function for DALY rates
# -----------------------------
proj <- function(data) {
  data_1021_n <- data %>%
    arrange(cause_name, WBcode, sex_name, age_name, year) %>%
    group_by(cause_name, WBcode, sex_name, age_name) %>%
    filter(!(year == 2021 | year == 2020)) %>% # exclude 2020–2021 when calculating growth
    mutate(rate = (val - lag(val)) / lag(val)) %>% # year-on-year growth
    mutate(rate_m = mean(rate, na.rm = TRUE)) %>% # mean annual growth across 2010–2019
    mutate(rate_m = ifelse(rate_m > 0.02, 0.02, rate_m)) %>% # cap growth at 2%
    mutate(rate_m = ifelse(is.na(rate_m), 0, rate_m))
  
  data_1921 <- data %>% subset(year == 2019 | year == 2020 | year == 2021)
  
  data_21 <- data_1021_n %>%
    select(-c(rate, year, val)) %>%
    distinct(cause_name, WBcode, sex_name, age_name, rate_m, .keep_all = TRUE) %>%
    merge(data, by = c('cause_name','WBcode','sex_name','age_name')) %>%
    subset(year == 2021) %>%
    group_by(cause_name, WBcode, sex_name, age_name)
  
  # Progress bar for year projections (2022–2050: 29 years)
  pb <- progress_bar$new(
    format = "Predicting groups [:bar] :percent | Group :current/:total | eta: :eta",
    total = 29,
    width = 60
  )
  
  result_list <- list()
  for (i in 1:29) {
    pb$tick()
    mutated_data <- data_21 %>%
      mutate(val = val * ((1 + rate_m)^i), year = 2021 + i) %>%
      select(-c(rate_m))
    result_list[[i]] <- mutated_data
  }
  
  final_data <- do.call(rbind, result_list)
  final_data <- data_1921 %>% rbind(final_data)
  return(final_data)
}

# -----------------------------
# Compute 2019 age–sex standardized DALY rates (2019–2050 mean)
# -----------------------------
num <- read.csv('/dssg/home/economic2/outcome/rural outcome/number_1950_origin.csv')

num_19 <- num %>%
  subset(year == 2019) %>%
  select(-c(X, year)) %>%
  arrange(WBcode, sex) %>%
  group_by(WBcode, sex) %>%
  mutate(num_tot = sum(number), rate = number / num_tot) %>%
  select(-c(number, num_tot))

daly_2050 <- daly_n %>%
  proj(.) %>%
  rename(sex = sex_name, age = age_name) %>%
  mutate(
    age = ifelse(age == "<5 years", "0-4", age),
    age = ifelse(age == "5-9 years", "5-9", age),
    age = ifelse(age == "10-14 years", "10-14", age),
    age = ifelse(age == "15-19 years", "15-19", age),
    age = ifelse(age == "20-24 years", "20-24", age),
    age = ifelse(age == "25-29 years", "25-29", age),
    age = ifelse(age == "30-34 years", "30-34", age),
    age = ifelse(age == "35-39 years", "35-39", age),
    age = ifelse(age == "40-44 years", "40-44", age),
    age = ifelse(age == "45-49 years", "45-49", age),
    age = ifelse(age == "50-54 years", "50-54", age),
    age = ifelse(age == "55-59 years", "55-59", age),
    age = ifelse(age == "60-64 years", "60-64", age),
    age = ifelse(age == "65-69 years", "65-69", age),
    age = ifelse(age == "70-74 years", "70-74", age),
    age = ifelse(age == "75-79 years", "75-79", age),
    age = ifelse(age == "80-84 years", "80-84", age),
    age = ifelse(age == "85-89 years", "85-89", age),
    age = ifelse(age == "90-94 years", "90-94", age),
    age = ifelse(age == "95+ years", "95+", age),
    sex = ifelse(sex == "Female", "female", sex),
    sex = ifelse(sex == "Male", "male", sex)
  ) %>%
  merge(num_19, by = c('WBcode','sex','age')) %>%
  arrange(cause_name, WBcode, year, sex, age) %>%
  group_by(cause_name, WBcode, year) %>%
  mutate(val_stand = val * rate, val_tot = sum(val_stand)) %>%
  select(cause_name, WBcode, year, val_tot) %>%
  distinct(cause_name, WBcode, year, val_tot, .keep_all = TRUE) %>%
  group_by(cause_name, WBcode) %>%
  mutate(daly_m = mean(val_tot) / 100000) %>% # convert to proportion (per person)
  select(cause_name, WBcode, daly_m) %>%
  distinct(cause_name, WBcode, .keep_all = TRUE)

write.csv(daly_2050, '/dssg/home/economic2/outcome/rural outcome/daly_stand.csv')

# Reload standardized DALY file (cleaned)
daly_2050 <- read.csv('/dssg/home/economic2/outcome/rural outcome/daly_stand.csv')
daly_2050 <- daly_2050 %>% select(-X)

# -----------------------------
# Load observed burden/GDP series (2020–2050)
# -----------------------------
data_val <- read.csv("/dssg/home/economic2/outcome/rural outcome/burden_cause_country_val.csv")
data_lower <- read.csv("/dssg/home/economic2/outcome/rural outcome/burden_cause_country_lower.csv")
data_upper <- read.csv("/dssg/home/economic2/outcome/rural outcome/burden_cause_country_upper.csv")

GDP <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/gdp_161950_n.csv")
GDP_n <- GDP %>%
  select(., c(2:4)) %>%
  subset(year > 2019) %>%
  group_by(WBcode) %>%
  mutate(totgdp = sum(val.gdp)) %>%
  select(., c(1,4)) %>%
  distinct(WBcode, .keep_all = TRUE)

# Prepare disease and country lists
disease_name <- unique(data_val$cause_name)
country_code <- unique(GDP_n$WBcode)

# -----------------------------
# Helper: expand to full country–cause grid
# -----------------------------
fill_2050 <- function(a) {
  all_combinations <- expand.grid(cause_name = disease_name,
                                  WBcode = country_code)
  result_df <- all_combinations %>%
    merge(., a, by = c('cause_name','WBcode'), all.x = TRUE) %>%
    arrange(cause_name, WBcode)
  return(result_df)
}

# -----------------------------
# Imputation function using linear model (percent ~ daly_m)
# -----------------------------
imputation <- function(data) {
  imp <- data %>%
    select(., c(2:4)) %>%
    fill_2050(.) %>%
    merge(., GDP_n, by = 'WBcode', all.x = TRUE) %>%
    mutate(percent = burden / totgdp)
  
  imp_n <- imp %>% merge(., daly_2050, by = c('cause_name','WBcode'))
  
  result_df <- data.frame()
  
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent | :current/:total | eta: :eta",
    total = length(disease_name),
    width = 60
  )
  
  for (i in 1:length(disease_name)) {
    pb$tick()
    df <- imp_n %>% subset(cause_name == disease_name[i])
    df_no_missing <- na.omit(df)
    
    linear_model <- lm(percent ~ daly_m, data = df_no_missing)
    model_sum <- summary(linear_model)
    
    intercept_p <- coef(model_sum)[1, 4]
    daly_p <- coef(model_sum)[2, 4]
    
    # If slope is not significant, print index for review
    if (!is.na(daly_p) && daly_p > 0.05) {
      print(i)
    }
    
    # If intercept not significant, refit without intercept
    if (!is.na(intercept_p) && intercept_p > 0.05) {
      linear_model <- lm(percent ~ 0 + daly_m, data = df_no_missing)
    }
    
    df$predicted_b <- predict(linear_model, newdata = df)
    df_n <- df %>%
      mutate(percent = ifelse(is.na(percent), predicted_b, percent)) %>%
      mutate(burden = ifelse(is.na(burden), percent * totgdp, burden))
    
    result_df <- rbind(result_df, df_n)
  }
  
  country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
  country_n <- country %>%
    select(., c(3,4)) %>%
    rename(country = location_id, WBcode = Country.Code)
  
  result_df_n <- result_df %>% merge(country_n, by = 'WBcode') # add country name
  return(result_df_n)
}

result_val <- imputation(data_val)
result_lower <- imputation(data_lower)
result_upper <- imputation(data_upper)

# -----------------------------
# For causes with weak relationships, do income-group-level imputation
# -----------------------------
inc_grp <- read_xlsx("/dssg/home/economic2/data/data/income group/CLASS.xlsx", sheet = 1)
inc_grp_n <- inc_grp %>%
  select(., c(2:4)) %>%
  head(n = 218) %>%
  rename(WBcode = Code, income = `Income group`) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  mutate(income = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'), 'Others', income),
         Region = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'), 'Others', Region)) %>%
  select(WBcode, income)

imputation_2 <- function(data, i, data_2) {
  imp <- data %>%
    select(., c(2:4)) %>%
    fill_2050(.) %>%
    merge(., GDP_n, by = 'WBcode', all.x = TRUE) %>%
    mutate(percent = burden / totgdp) %>%
    merge(., inc_grp_n, by = 'WBcode', all.x = TRUE)
  
  imp_n <- imp %>% merge(., daly_2050, by = c('cause_name','WBcode'))
  
  result_df <- data.frame()
  df <- imp_n %>% subset(cause_name == disease_name[i])
  
  income_groups <- unique(df$income)
  for (j in 1:length(income_groups)) {
    df_group <- df %>% filter(income == income_groups[j])
    df_no_missing <- na.omit(df_group)
    
    if (nrow(df_no_missing) > 2) {
      linear_model <- lm(percent ~ daly_m, data = df_no_missing)
      model_sum <- summary(linear_model)
      intercept_p <- coef(model_sum)[1, 4]
      daly_m_p <- coef(model_sum)[2, 4]
      
      # If slope non-significant, fill missing using group median percent
      if (!is.na(daly_m_p) && daly_m_p > 0.05) {
        quantile_val <- quantile(df_no_missing$percent, probs = 0.5, na.rm = TRUE)
        df_group <- df_group %>%
          mutate(percent = ifelse(is.na(percent), quantile_val, percent)) %>%
          mutate(burden = ifelse(is.na(burden), percent * totgdp, burden))
        result_df <- rbind(result_df, df_group)
        next
      }
      
      # If intercept not significant, refit without intercept
      if (!is.na(intercept_p) && intercept_p > 0.05) {
        linear_model <- lm(percent ~ 0 + daly_m, data = df_no_missing)
      }
      
      df_group$predicted_b <- predict(linear_model, newdata = df_group)
    } else {
      # If insufficient datapoints, fallback to global imputation results (data_2)
      df_group <- df_group %>%
        merge(data_2, by = c('cause_name', 'WBcode', 'totgdp', 'daly_m')) %>%
        mutate(burden.x = burden.y) %>%
        rename(burden = burden.x, percent = percent.x) %>%
        select(cause_name, WBcode, burden, totgdp, percent, income, daly_m)
      df_group$predicted_b <- NA
      result_df <- rbind(result_df, df_group)
      next
    }
    
    df_group <- df_group %>%
      mutate(percent = ifelse(is.na(percent), predicted_b, percent)) %>%
      mutate(burden = ifelse(is.na(burden), percent * totgdp, burden))
    
    result_df <- rbind(result_df, df_group)
  }
  
  country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
  country_n <- country %>%
    select(., c(3,4)) %>%
    rename(country = location_id, WBcode = Country.Code)
  
  result_df_n <- result_df %>% merge(country_n, by = 'WBcode')
  return(result_df_n)
}

# Apply income-group-level imputation for specific cause indices (example indices 35 and 43)
result_35_val <- imputation_2(data_val, 35, result_val) %>% select(-income)
result_35_lower <- imputation_2(data_lower, 35, result_lower) %>% select(-income)
result_35_upper <- imputation_2(data_upper, 35, result_upper) %>% select(-income)

result_43_val <- imputation_2(data_val, 43, result_val) %>% select(-income)
result_43_lower <- imputation_2(data_lower, 43, result_lower) %>% select(-income)

result_m_val <- rbind(result_35_val, result_43_val)
result_m_lower <- rbind(result_35_lower, result_43_lower)

# Merge income-group adjustments back into global imputation results
result_val_n <- result_val %>%
  merge(result_m_val, by = c('WBcode','cause_name','country'), all.x = TRUE) %>%
  mutate(burden.x = ifelse(!is.na(burden.y), burden.y, burden.x)) %>%
  select(c(1:8)) %>%
  rename(burden = burden.x, totgdp = totgdp.x, percent = percent.x, daly_m = daly_m.x, predicted_b = predicted_b.x)

result_lower_n <- result_lower %>%
  merge(result_m_lower, by = c('WBcode','cause_name','country'), all.x = TRUE) %>%
  mutate(burden.x = ifelse(!is.na(burden.y), burden.y, burden.x)) %>%
  select(c(1:8)) %>%
  rename(burden = burden.x, totgdp = totgdp.x, percent = percent.x, daly_m = daly_m.x, predicted_b = predicted_b.x)

result_upper_n <- result_upper %>%
  merge(result_35_upper, by = c('WBcode','cause_name','country'), all.x = TRUE) %>%
  mutate(burden.x = ifelse(!is.na(burden.y), burden.y, burden.x)) %>%
  select(c(1:8)) %>%
  rename(burden = burden.x, totgdp = totgdp.x, percent = percent.x, daly_m = daly_m.x, predicted_b = predicted_b.x)

# Finalize imputed country–cause table and compute per-cause totals
result_df_n <- result_val_n %>%
  select(c(1:5)) %>%
  merge(., result_lower_n, by = c('cause_name','WBcode','country')) %>%
  select(c(1:6)) %>%
  merge(., result_upper_n, by = c('cause_name','WBcode','country')) %>%
  select(c(1:7)) %>%
  rename(val = burden.x, lower = burden.y, upper = burden)

imp_val <- result_val_n %>%
  select('cause_name','burden') %>%
  group_by(cause_name) %>%
  mutate(burden = sum(burden, na.rm = TRUE)) %>%
  distinct(cause_name, .keep_all = TRUE)

imp_lower <- result_lower_n %>%
  select('cause_name','burden') %>%
  group_by(cause_name) %>%
  mutate(burden = sum(burden, na.rm = TRUE)) %>%
  distinct(cause_name, .keep_all = TRUE)

imp_upper <- result_upper_n %>%
  select('cause_name','burden') %>%
  group_by(cause_name) %>%
  mutate(burden = sum(burden, na.rm = TRUE)) %>%
  distinct(cause_name, .keep_all = TRUE)

imp_result <- imp_val %>%
  merge(., imp_lower, by = c('cause_name')) %>%
  merge(., imp_upper, by = c('cause_name')) %>%
  rename(val = burden.x, lower = burden.y, upper = burden)

# Write outputs
write.csv(result_df_n, "/dssg/home/economic2/outcome/rural outcome/impburden_cause_country_ui.csv")
write.csv(imp_result, "/dssg/home/economic2/outcome/rural outcome/impburden_cause_tot_ui.csv")
