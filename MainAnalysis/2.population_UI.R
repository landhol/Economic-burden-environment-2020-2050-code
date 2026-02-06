############################################################
# Population Data Processing and Counterfactual Estimation
# Original population counts are in thousands
############################################################

library(readxl)
library(dplyr)
library(tidyr)
library(progress)

#-----------------------------
# 1. Load population data
#-----------------------------

## Male population: historical and projected
num1 <- read_xlsx("/dssg/home/economic2/data/data/population/population_male.xlsx",
                  sheet = 'Estimates', col_names = FALSE)  # Includes 2020–2021
colname <- read_xlsx("/dssg/home/economic2/data/data/population/population_male.xlsx",
                     sheet = 'Estimates', col_names = FALSE, n_max = 1)
colnames(num1) <- colname
num1 <- num1[-1, ]  # Remove header row

num2 <- read_xlsx("/dssg/home/economic2/data/data/population/population_male.xlsx",
                  sheet = 'Medium variant', col_names = FALSE)  # Projections to 2050
colnames(num2) <- colname
num2 <- num2[-1, ]

num_male <- bind_rows(num1, num2) %>%
  mutate(sex = 'male')

## Female population: historical and projected
num1 <- read_xlsx("/dssg/home/economic2/data/data/population/population_female.xlsx",
                  sheet = 'Estimates', col_names = FALSE)
colnames(num1) <- colname
num1 <- num1[-1, ]

num2 <- read_xlsx("/dssg/home/economic2/data/data/population/population_female.xlsx",
                  sheet = 'Medium variant', col_names = FALSE)
colnames(num2) <- colname
num2 <- num2[-1, ]

num_female <- bind_rows(num1, num2) %>%
  mutate(sex = 'female')


#-----------------------------
# 2. Prepare population for counterfactual scenario
#-----------------------------

# Load cause-specific mortality data
death <- read.csv("/dssg/home/economic2/outcome/rural outcome/death_1950_n.csv")
death_n <- death[, 2:9]

# Combine male and female populations, harmonise age groups
num <- bind_rows(num_male, num_female) %>%
  filter(Year >= 2019 & Year <= 2050) %>%
  select(c(6, 33, 11:32)) %>%
  arrange(`ISO3 Alpha-code`, sex, Year) %>%
  mutate(across(4:24, as.numeric),
         `95+` = `95-99` + `100+`) %>%
  select(c(1:22, 25))

# Convert to long format
num_n <- num %>%
  pivot_longer(cols = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                        '40-44','45-49','50-54','55-59','60-64','65-69','70-74',
                        '75-79','80-84','85-89','90-94','95+'),
               names_to = "age",
               values_to = "number") %>%
  filter(!is.na(`ISO3 Alpha-code`))

# Standardise column names and convert to persons
nt_n <- num_n %>%
  rename(WBcode = `ISO3 Alpha-code`, year = Year) %>%
  mutate(number = number * 1000) %>%
  arrange(WBcode, sex, year) %>%
  group_by(WBcode, sex, year)

write.csv(nt_n,
          "/dssg/home/economic2/outcome/rural outcome/number_1950_origin.csv")


#-----------------------------
# 3. Merge population with deaths for counterfactual estimation
#-----------------------------

Nt <- merge(death_n, nt_n, by = c('WBcode','year','sex','age'), all.x = TRUE) %>%
  select(c(5,1,3,2,4,6:9)) %>%
  arrange(cause_name, WBcode, sex, year)

# Baseline population (2019)
Nt_19 <- Nt %>%
  filter(year == 2019) %>%
  select(c(1:5,9)) %>%
  rename(N = number)

Nt_2050 <- Nt %>% filter(year != 2019)


#-----------------------------
# 4. Helper objects and functions
#-----------------------------

disease_name <- unique(Nt$cause_name)
country_code <- unique(Nt$WBcode)
sex_name     <- unique(Nt$sex)
age_name     <- c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                  '40-44','45-49','50-54','55-59','60-64','65-69','70-74',
                  '75-79','80-84','85-89','90-94','95+')
year_name    <- 2019:2050

# Extract age-year specific data
data_matrix <- function(data, a, t, measure){
  data %>%
    filter(age == a & year == t) %>%
    select(c('cause_name','WBcode',measure))
}


#-----------------------------
# 5. Counterfactual population calculation
#-----------------------------

calculate_N <- function(data, s){
  
  data_m <- data %>%
    filter(sex == s) %>%
    mutate(D_val = 1 - val,
           D_lower = 1 - lower,
           D_upper = 1 - upper)
  
  Nt_ca <- data.frame(matrix(nrow = 0, ncol = 7))
  
  total_iterations <- n_distinct(data$age) * n_distinct(data$year)
  pb <- progress_bar$new(
    format = "Calculating [:bar] :percent | :current/:total | eta: :eta",
    total = total_iterations,
    width = 60
  )
  
  for (a in 1:n_distinct(data$age)) {
    Nt_ct <- data.frame(matrix(nrow = 0, ncol = 6))
    for (t in 1:n_distinct(data$year)) {
      pb$tick()
      
      # Initialise matrices
      result_val   <- matrix(1, nrow = 204, ncol = 48)
      result_lower <- matrix(1, nrow = 204, ncol = 48)
      result_upper <- matrix(1, nrow = 204, ncol = 48)
      
      data_N <- data_matrix(data_m, age_name[a], year_name[t], 'number')
      N_matrix <- spread(data_N, key = cause_name, value = number) %>%
        select(2:47)
      
      if(min(a,t) == 1 | t < 6){
        result_val   <- N_matrix
        result_lower <- N_matrix
        result_upper <- N_matrix
      } else {
        # Backward cohort reconstruction (point estimate and uncertainty)
        for (i in 0:(min(a, floor((t+4)/5)) - 2)) {
          # Survival probabilities
          data_D1 <- data_matrix(data_m, age_name[a-1-i], year_name[t-5-5*i], 'D_val')
          data_D2 <- data_matrix(data_m, age_name[a-1-i], year_name[t-4-5*i], 'D_val')
          data_D3 <- data_matrix(data_m, age_name[a-1-i], year_name[t-3-5*i], 'D_val')
          data_D4 <- data_matrix(data_m, age_name[a-1-i], year_name[t-2-5*i], 'D_val')
          data_D5 <- data_matrix(data_m, age_name[a-1-i], year_name[t-1-5*i], 'D_val')
          
          D1_matrix <- select(spread(data_D1, key = cause_name, value = D_val), 2:47)
          D2_matrix <- select(spread(data_D2, key = cause_name, value = D_val), 2:47)
          D3_matrix <- select(spread(data_D3, key = cause_name, value = D_val), 2:47)
          D4_matrix <- select(spread(data_D4, key = cause_name, value = D_val), 2:47)
          D5_matrix <- select(spread(data_D5, key = cause_name, value = D_val), 2:47)
          
          result_val <- result_val * D1_matrix * D2_matrix * D3_matrix * D4_matrix * D5_matrix
        }
        
        result_val   <- N_matrix / result_val
        result_lower <- N_matrix / result_lower
        result_upper <- N_matrix / result_upper
      }
      
      # Convert to long format
      result_val <- cbind(as.data.frame(result_val), WBcode = country_code)
      result_lower <- cbind(as.data.frame(result_lower), WBcode = country_code)
      result_upper <- cbind(as.data.frame(result_upper), WBcode = country_code)
      
      result_val_data   <- gather(result_val,   key = "cause_name", value = "N_val",   -WBcode)
      result_lower_data <- gather(result_lower, key = "cause_name", value = "N_lower", -WBcode)
      result_upper_data <- gather(result_upper, key = "cause_name", value = "N_upper", -WBcode)
      
      result_data <- result_val_data %>%
        merge(result_lower_data, by = c('cause_name','WBcode')) %>%
        merge(result_upper_data, by = c('cause_name','WBcode')) %>%
        mutate(year = year_name[t])
      
      Nt_ct <- rbind(Nt_ct, result_data)
    }
    Nt_ct$age <- age_name[a]
    Nt_ca <- rbind(Nt_ca, Nt_ct)
  }
  
  Nt_ca$sex <- s
  return(Nt_ca)
}


#-----------------------------
# 6. Run counterfactual by sex
#-----------------------------

Nt_cm <- calculate_N(Nt,'male')
Nt_cf <- calculate_N(Nt,'female')

Nt_c <- bind_rows(Nt_cm, Nt_cf) %>%
  select(c(2,1,8,6,7,3:5)) %>%
  arrange(cause_name, WBcode, sex, year, age)


#-----------------------------
# 7. Aggregate population aged ≥65 into a single group
#-----------------------------

combind_N <- function(data){
  
  filtered_data <- data %>%
    filter(age %in% c("65-69","70-74","75-79","80-84","85-89","90-94","95+")) %>%
    group_by(cause_name, WBcode, sex, year) %>%
    summarise(N_val   = sum(N_val),
              N_lower = sum(N_lower),
              N_upper = sum(N_upper),
              .groups = 'drop') %>%
    mutate(age = '65+')
  
  data_new <- data %>%
    filter(!(age %in% c('0-4','5-9','10-14',
                        "65-69","70-74","75-79","80-84","85-89","90-94","95+")))
  
  bind_rows(data_new, filtered_data) %>%
    arrange(cause_name, WBcode, sex, year, age)
}

Nt_cn <- combind_N(Nt_c)

write.csv(Nt_cn,
          "/dssg/home/economic2/outcome/rural outcome/number_cf_1950_ui.csv")
