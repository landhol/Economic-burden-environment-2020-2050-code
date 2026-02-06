# Mortality & Morbidity
# Rates are expressed per 100,000 population in the original data
# Data are stratified by 5-year age groups, sex, disease, and location
# Calculate the average annual growth rate for 2010–2019
# Use the estimated growth rate to project mortality and morbidity for 2022–2050
# Annual growth rates are capped at 2%

library(data.table)
library(dplyr)

dirname <- dir("/dssg/home/economic2/data/data/mortality/")  
file <- paste0("/dssg/home/economic2/data/data/mortality/",dirname) ## read all mortality files

death_1021<-  as.data.frame(matrix(nrow=0,ncol=16)) 
for (a in file) {
  data <- fread(a)  
  death_1021 <- rbind(death_1021,data)
} 

dirname <- dir("/dssg/home/economic2/data/data/YLL/")  
file <- paste0("/dssg/home/economic2/data/data/YLL/",dirname) ## mortality-related YLL
YLL_1021<-  as.data.frame(matrix(nrow=0,ncol=16)) 
for (a in file) {
  data <- fread(a)  
  YLL_1021 <- rbind(YLL_1021,data)
} 

dirname <- dir("/dssg/home/economic2/data/data/YLD/")  
file <- paste0("/dssg/home/economic2/data/data/YLD/",dirname) ## morbidity (YLD)
YLD_1021<-  as.data.frame(matrix(nrow=0,ncol=16)) 
for (a in file) {
  data <- fread(a)  
  YLD_1021 <- rbind(YLD_1021,data)
} 

# Projection function
proj <- function(data){
  data_1021_n <- data %>%
    select(.,c('measure_name','cause_name','location_id','sex_name','age_name','year','val','lower','upper')) %>%
    arrange(.,cause_name,location_id,sex_name,age_name,year) %>%
    group_by(.,cause_name,location_id,sex_name,age_name) %>%
    filter(!(year==2021|year==2020)) %>% ## use 2010–2019 data to calculate growth rates
    mutate(rate_v=(val-lag(val))/lag(val),rate_l=(lower-lag(lower))/lag(lower),
           rate_u=(upper-lag(upper))/lag(upper)) %>% # calculate annual growth rates and then average them
    mutate(rate_v_m=mean(rate_v, na.rm = TRUE),rate_l_m=mean(rate_l, na.rm = TRUE),
           rate_u_m=mean(rate_u, na.rm = TRUE)) %>% # compute mean growth rate
    mutate(rate_v_m = ifelse(rate_v_m > 0.02, 0.02, rate_v_m),
           rate_l_m = ifelse(rate_l_m > 0.02, 0.02, rate_l_m),
           rate_u_m = ifelse(rate_u_m > 0.02, 0.02, rate_u_m)) # cap mean growth rates at 2%
  
  high_growth_data <- data_1021_n %>%
    group_by(cause_name, location_id, sex_name, age_name) %>%
    filter(any(rate_v_m > 0.02 | rate_l_m > 0.02 | rate_u_m > 0.02)) # check whether any growth rate exceeds 2%
  
  data_1921 <- data_1021_n %>%
    subset(year==2019|year==2020|year==2021) %>%
    select(!c(rate_v,rate_l,rate_u))
  
  data_n <- data %>% select(.,c('measure_name','cause_name','location_id','sex_name','age_name','year','val','lower','upper')) %>%
    arrange(.,cause_name,location_id,sex_name,age_name,year) 
  
  data_21 <- data_1021_n %>%
    select(!c(rate_v,rate_l,rate_u,year,val,lower,upper)) %>%
    distinct(cause_name,location_id,sex_name,age_name,rate_v_m,rate_l_m,rate_u_m,.keep_all=TRUE) %>%
    merge(data_n,by=c('measure_name','cause_name','location_id','sex_name','age_name')) %>%
    subset(year==2021) %>%
    group_by(cause_name,location_id,sex_name,age_name)
  
  # Create an empty list to store projections for each year
  result_list <- list()
  
  for (i in 1:29) {
    mutated_data <- data_21 %>%
      mutate(val = val * ((1+rate_v_m)^i),lower = lower * ((1+rate_l_m)^i),
             upper = upper * ((1+rate_u_m)^i), year = 2021 + i) %>%
      select(-c(rate_v_m,rate_l_m,rate_u_m))
    
    result_list[[i]] <- mutated_data
  }
  
  # Combine all projected results
  final_data <- do.call(rbind, result_list)
  final_data <- data_1921 %>%
    rbind(final_data) %>%
    select(-c(rate_v_m,rate_l_m,rate_u_m))
  return(final_data)
}

death_2050 <- proj(death_1021)
YLL_2050 <- proj(YLL_1021)
YLD_2050 <- proj(YLD_1021)

# Output includes 2019–2021 observed data and projections;
# YLD, death, and YLL together cover 46 diseases

## Country codes for 204 countries and WBcode
## GBD 2021 updated country names; this file aligns names using unchanged location_id
## It is recommended to verify countries using location_id
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")

country_n <- country %>%
  select(.,c(3,4)) %>%
  rename(country=location_id,WBcode=Country.Code)

country_name <- country$location_id
disease_name <- unique(YLD_2050$cause_name)

## Expand dataset to include all 204 countries,
## population aged ≥15 years, and all relevant causes from 2019–2050
fill <- function(data){
  data_1950_n <- data %>%
    arrange(.,cause_name,location_id,sex_name,age_name,year) %>% 
    mutate(val=val/100000,lower=lower/100000,upper=upper/100000) 
  ### Rates in GBD are per 100,000 population; convert to proportions
  
  data_n <- data_1950_n %>%
    select(.,c(2:9)) %>%
    rename(country=location_id,sex=sex_name,age=age_name) %>% ## standardize variable names
    mutate(sex = ifelse(sex == "Female", "female", sex),
           sex = ifelse(sex == "Male", "male", sex)) %>%
    arrange(cause_name,country,year,sex,age)
  
  # Create a dataframe containing all possible combinations
  all_combinations <- expand.grid(cause_name = disease_name,
                                  country = country_name,
                                  year = 2019:2050,
                                  sex = c('female','male'),
                                  age = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59',
                                          '60-64','65-69','70-74','75-79','80-84','85-89','90-94','95+'))
  
  # Merge original data with full combinations and fill missing values with zero
  result_df <- all_combinations %>%
    merge(.,data_n, by = c("cause_name",'country','year','sex','age'), all.x = TRUE) %>%
    merge(country_n,by = 'country') %>%
    select(cause_name,WBcode,year,sex,age,val,lower,upper) %>%
    arrange(cause_name,WBcode,year,sex,age) %>%
    replace(.,is.na(.),0)
  
  return(result_df)
}

death_1950_n <- death_2050 %>% fill()
YLL_1950_n <- YLL_2050 %>% fill()
YLD_1950_n <- YLD_2050 %>% fill()

write.csv(death_1950_n,"/dssg/home/economic2/outcome/rural outcome/death_1950_n.csv")
write.csv(YLL_1950_n,"/dssg/home/economic2/outcome/rural outcome/YLL_1950_n.csv")
write.csv(YLD_1950_n,"/dssg/home/economic2/outcome/rural outcome/YLD_1950_n.csv")
