############################################################
# Script: 06_Y_new.R
# Purpose: 
#   Project counterfactual output and estimate macroeconomic
#   burden attributable to risk-related reductions in 
#   human capital and treatment costs.
#
# Approach:
#   1. Reconstruct capital stock using a dynamic accumulation model.
#   2. Estimate total factor productivity (TFP).
#   3. Project counterfactual output under alternative 
#      human-capital scenarios.
#   4. Calculate cumulative economic burden (undiscounted 
#      and discounted at 2%).
############################################################

library(dplyr)

# -----------------------------
# Load macroeconomic inputs
# -----------------------------

GDP <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/gdp_161950_n.csv")
GDP_n <- GDP %>%
  select(., c(2:4)) %>%
  subset(year > 2016)

Ht <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/Ht.csv")
Ht <- Ht[, c(2:4)]

Kt <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/physical capital stock_2019.csv")
Kt <- Kt[, c(2:4)]

st <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/saving rate.csv")
st <- st[, c(2:3)]

alpha <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/capital share.csv")
alpha_n <- alpha[, c(2,4)]

delta <- 0.05   # Depreciation rate

# -----------------------------
# Capital stock projection
# -----------------------------

Kt_n <- Kt
years_to_add <- 2020:2050

for (year in years_to_add) {
  new_data <- Kt
  new_data$year <- year
  new_data$cn <- 0
  Kt_n <- rbind(Kt_n, new_data)
}

output <- Kt_n %>%
  merge(GDP_n, by = c('WBcode','year'), all.y = TRUE) %>%
  merge(st, by = c('WBcode')) %>%
  arrange(WBcode, year) %>%
  group_by(WBcode) %>%
  mutate(grp = cur_group_id())

proj_K <- as.data.frame(matrix(nrow = 0, ncol = 6))

for (i in 1:n_distinct(output$grp)) {
  tmp <- subset(output, output$grp == i)
  for (j in 2:nrow(tmp)) {
    tmp[j,3] <- tmp[j-1,5] * tmp[j-1,4] + tmp[j-1,3] * (1 - delta)
  }
  proj_K <- rbind(proj_K, tmp)
}

# -----------------------------
# Estimate total factor productivity (TFP)
# -----------------------------

At <- proj_K %>%
  merge(Ht, by = c('WBcode','year'), all.x = TRUE) %>%
  merge(alpha_n, by = c('WBcode')) %>%
  arrange(WBcode, year) %>%
  mutate(tech = val.gdp / (cn^alpha) / (Ht_a^(1 - alpha))) %>%
  select(., c(1,2,9))

# -----------------------------
# Load counterfactual inputs
# -----------------------------

Ht_c <- read.csv("/dssg/home/economic2/outcome/rural outcome/Ht_cf.csv")

Ht_val   <- Ht_c %>% select(c(2:4,'Ht_val_a'))
Ht_lower <- Ht_c %>% select(c(2:4,'Ht_lower_a'))
Ht_upper <- Ht_c %>% select(c(2:4,'Ht_upper_a'))

TCt <- read.csv("/dssg/home/economic2/outcome/rural outcome/treatment cost_1950_n2.csv")
TCt_n <- TCt[, c(2:5)]
TCt_n$expense <- TCt_n$expense * 1e9  # Convert from billions

# -----------------------------
# Counterfactual output function
# -----------------------------

Yfunc <- function(data){
  
  output_c <- Kt_n %>%
    merge(data, by = c('WBcode','year')) %>%
    merge(TCt_n, by = c('cause_name','WBcode','year')) %>%
    merge(At, by = c('WBcode','year')) %>%
    merge(st, by = c('WBcode')) %>%
    merge(alpha_n, by = c('WBcode')) %>%
    select(., c(3,1,2,4:9)) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(Yt = 0, grp = cur_group_id())
  
  proj_output_c <- as.data.frame(matrix(nrow = 0, ncol = 11))
  
  for (i in 1:n_distinct(output_c$grp)) {
    tmp <- subset(output_c, output_c$grp == i)
    
    for (j in 2:(nrow(tmp) + 1)) {
      tmp[j-1,10] <- tmp[j-1,7] *
        (tmp[j-1,4]^tmp[j-1,9]) *
        (tmp[j-1,5]^(1 - tmp[j-1,9]))
      
      if (j == nrow(tmp) + 1) {
        break
      } else {
        tmp[j,4] <- (1 - delta) * tmp[j-1,4] +
          tmp[j-1,8] * tmp[j-1,10] +
          tmp[j-1,8] * tmp[j-1,6]
      }
    }
    
    proj_output_c <- rbind(proj_output_c, tmp)
  }
  
  proj_output_cn <- proj_output_c %>%
    select(., c(1:3,10)) %>%
    mutate(Yt = ifelse(Yt == 0, NA, Yt))
  
  return(proj_output_cn)
}

proj_output_val   <- Yfunc(Ht_val)
proj_output_lower <- Yfunc(Ht_lower)
proj_output_upper <- Yfunc(Ht_upper)

proj_output_n <- proj_output_val %>%
  merge(proj_output_lower, by = c('cause_name','WBcode','year')) %>%
  merge(proj_output_upper, by = c('cause_name','WBcode','year')) %>%
  rename(Y_val = Yt.x, Y_lower = Yt.y, Y_upper = Yt)

# -----------------------------
# Economic burden calculation
# -----------------------------

output <- function(data){
  
  output_tot <- data %>%
    merge(GDP_n, by = c('WBcode','year')) %>%
    select(., c(3,1,2,4,5)) %>%
    subset(year >= 2020 & year <= 2050) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(burden = sum(Yt - val.gdp)) %>%
    select(., c(1,2,6)) %>%
    distinct(cause_name, WBcode, .keep_all = TRUE)
  
  return(output_tot)
}

output_tot_val   <- output(proj_output_val)
output_tot_lower <- output(proj_output_lower)
output_tot_upper <- output(proj_output_upper)

output_tot_n <- output_tot_val %>%
  merge(output_tot_lower, by = c('cause_name','WBcode')) %>%
  merge(output_tot_upper, by = c('cause_name','WBcode')) %>%
  rename(burden_val = burden.x,
         burden_lower = burden.y,
         burden_upper = burden)

# -----------------------------
# Discounted burden (2%)
# -----------------------------

output2 <- function(data){
  
  r <- 0.02
  base_year <- 2020
  
  output_tot <- data %>%
    merge(GDP_n, by = c('WBcode','year')) %>%
    select(., c(3,1,2,4,5)) %>%
    subset(year >= 2020 & year <= 2050) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(
      discount_factor = 1 / (1 + r)^(year - base_year),
      discounted_burden = (Yt - val.gdp) * discount_factor
    ) %>%
    summarise(burden = sum(discounted_burden, na.rm = TRUE), .groups = "drop") %>%
    select(cause_name, WBcode, burden)
  
  return(output_tot)
}

output_tot_val2   <- output2(proj_output_val)
output_tot_lower2 <- output2(proj_output_lower)
output_tot_upper2 <- output2(proj_output_upper)

# -----------------------------
# Aggregate burden by cause
# -----------------------------

output_cause_tot <- output_tot_n %>%
  group_by(cause_name) %>%
  mutate(
    burden_val   = sum(burden_val, na.rm = TRUE),
    burden_lower = sum(burden_lower, na.rm = TRUE),
    burden_upper = sum(burden_upper, na.rm = TRUE)
  ) %>%
  distinct(cause_name, .keep_all = TRUE)

write.csv(output_cause_tot,
          "/dssg/home/economic2/outcome/rural outcome/burden_cause_tot_ui.csv")
