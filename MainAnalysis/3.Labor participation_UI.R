############################################################
# Counterfactual labour participation estimation
#
# Definitions:
# p  : probability of remaining in morbidity state in each year
#      approximated as 1 − incidence/prevalence
#      Assumed constant over time and derived from 2019 data
#
# ξ (s): ratio of YLD to YLL, varying over time
#
# Population data (2019–2050) are used to construct the 65+ age group
############################################################


############################################################
# Step 1. Compute ξ = YLD / YLL
############################################################

library(dplyr)

# Read YLD and YLL inputs
YLD <- read.csv("/dssg/home/economic2/outcome/rural outcome/YLD_1950_n.csv") %>%
  select(c(2:9))

YLL <- read.csv("/dssg/home/economic2/outcome/rural outcome/YLL_1950_n.csv") %>%
  select(c(2:9))


############################################################
# Step 2. Aggregate population aged ≥65 years
############################################################

# Population counts used for age-weighted aggregation
pop <- read.csv("/dssg/home/economic2/outcome/rural outcome/number_1950_origin.csv")
pop <- pop[,c(2:6)]


# Aggregate age groups ≥65 using population-weighted averages
combind <- function(data){
  
  filtered_data <- data %>%
    merge(pop, by = c('WBcode','sex','year','age')) %>%
    select(c(5,1:4,6:9)) %>%
    filter(age %in% c("65-69","70-74","75-79","80-84","85-89",'90-94','95+')) %>%
    arrange(cause_name,WBcode,sex,year,age) %>%
    group_by(cause_name,WBcode,sex,year) %>%
    mutate(
      total_num = sum(number),
      adj_val   = (val * number) / total_num,
      adj_lower = (lower * number) / total_num,
      adj_upper = (upper * number) / total_num,
      age = '65+',
      total_val   = sum(adj_val),
      total_lower = sum(adj_lower),
      total_upper = sum(adj_upper)
    ) %>%
    select(c(1:5),'total_val','total_lower','total_upper') %>%
    rename(val = total_val,
           lower = total_lower,
           upper = total_upper) %>%
    distinct(cause_name,WBcode,sex,year,.keep_all = TRUE)
  
  # Remove original ≥65 age groups
  data_new <- data %>%
    filter(!(age %in% c("65-69","70-74","75-79","80-84","85-89",'90-94','95+')))
  
  # Combine datasets
  data_final <- data_new %>%
    bind_rows(filtered_data) %>%
    select(c(1,2,4,3,5:8)) %>%
    arrange(cause_name,WBcode,sex,year,age)
  
  return(data_final)
}

YLD_n <- YLD %>% combind()
YLL_n <- YLL %>% combind()


############################################################
# Step 3. Calculate ξ (YLD/YLL)
############################################################

data_s <- YLD_n %>%
  merge(YLL_n, by = c('cause_name','WBcode','sex','year','age')) %>%
  mutate(
    lower.y = ifelse((val.y != 0) & (lower.y == 0), val.y, lower.y),
    upper.y = ifelse((val.y != 0) & (upper.y == 0), val.y, upper.y),
    s_val   = val.x / val.y,
    s_lower = lower.x / lower.y,
    s_upper = upper.x / upper.y
  ) %>%
  select(c(1:5,12:14)) %>%
  mutate(
    s_val   = ifelse(is.nan(s_val) | is.infinite(s_val), 0, s_val),
    s_lower = ifelse(is.nan(s_lower) | is.infinite(s_lower), 0, s_lower),
    s_upper = ifelse(is.nan(s_upper) | is.infinite(s_upper), 0, s_upper)
  )
# NaN values occur when both YLL and YLD equal zero


############################################################
# Step 4. Estimate p (probability of remaining in morbidity)
############################################################

data_p_rural_2 <- read.csv('/dssg/home/economic2/data/data/p/global.csv')
disease_name <- unique(YLD_n$cause_name)

data_p_rural_n2 <- data_p_rural_2 %>%
  select(c(2,10,14)) %>%
  subset(cause_name %in% disease_name) %>%
  arrange(measure_name,cause_name)


# Incidence and prevalence
inc_2 <- data_p_rural_n2 %>%
  subset(measure_name == 'Incidence') %>%
  select(c(2:3))

pre_2 <- data_p_rural_n2 %>%
  subset(measure_name == 'Prevalence') %>%
  select(c(2:3))


# Global p estimates
data_p2 <- inc_2 %>%
  merge(pre_2, by = c('cause_name'), all.y = TRUE) %>%
  mutate(
    p = 1 - val.x / val.y,
    p = ifelse(is.nan(p) | is.infinite(p), NA, p),
    p = ifelse(p < 0, 0, p)
  ) %>%
  mutate(
    p = ifelse(cause_name=='Blindness and vision loss',1-1/20,p),
    p = ifelse(cause_name=='Hypertensive heart disease',1-1/30,p),
    p = ifelse(cause_name=='Tuberculosis',1-1/1.1,p)
  ) %>%
  select(c(1,4))


############################################################
# Step 5. Country-specific p estimates
############################################################

data_p_rural <- read.csv('/dssg/home/economic2/data/data/p/age_standard.csv')

country <- read.csv("/dssg/home/economic2/data/data/country name/location_id.csv")

country_n <- country %>%
  select(c(3,4)) %>%
  rename(country = location_id, WBcode = Country.Code)

country_code <- country$Country.Code
disease_name <- data_p2$cause_name


# Ensure full country–disease coverage
all_combinations <- expand.grid(
  measure_name = c('Incidence','Prevalence'),
  cause_name = disease_name,
  WBcode = country_code
)


data_p_rural_n <- data_p_rural %>%
  select(c(2,10,3,14)) %>%
  rename(country = location_id) %>%
  merge(country_n, by = 'country') %>%
  merge(all_combinations, by=c('measure_name','WBcode','cause_name'), all.y=TRUE) %>%
  select(c(1:3,5)) %>%
  arrange(measure_name,cause_name,WBcode)


inc <- data_p_rural_n %>%
  subset(measure_name == 'Incidence') %>%
  select(c(2:4))

pre <- data_p_rural_n %>%
  subset(measure_name == 'Prevalence') %>%
  select(c(2:4))


data_p <- inc %>%
  merge(pre, by = c('cause_name','WBcode')) %>%
  mutate(
    p = 1 - val.x / val.y,
    p = ifelse(is.nan(p) | is.infinite(p), NA, p),
    p = ifelse(p < 0, 0, p)
  ) %>%
  select(c(1,2,5)) %>%
  merge(data_p2, by='cause_name') %>%
  mutate(p.x = ifelse(is.na(p.x), p.y, p.x)) %>%
  select(-p.y) %>%
  rename(p = p.x)


############################################################
# Step 6. Merge datasets for labour participation modelling
############################################################

death <- read.csv("/dssg/home/economic2/outcome/rural outcome/death_1950_n.csv")
death <- death[,c(2:9)]

lt_n <- read.csv("/dssg/home/economic2/outcome/rural outcome/all/labor participation_all ages.csv")
lt_n <- lt_n[,c(2:6)]

death_n <- combind(death)

data_lt <- data_p %>%
  merge(lt_n, by = c('WBcode')) %>%
  merge(data_s, by = c('cause_name','WBcode','sex','year','age')) %>%
  merge(death_n, by = c('cause_name','WBcode','sex','year','age'), all.y = TRUE) %>%
  subset(cause_name %in% disease_name) %>%
  arrange(cause_name,WBcode,sex,year,age)

write.csv(data_lt,
          "/dssg/home/economic2/outcome/rural outcome/data_lt.csv")


############################################################
# Step 7. Counterfactual labour participation calculation
############################################################

lt_19 <- data_lt %>%
  subset(year == '2019') %>%
  select(c(1:5,7)) %>%
  rename(L = val.x)

lt_2050 <- data_lt %>%
  filter(year != '2019')


############################################################
# Matrix-based reconstruction of counterfactual labour supply
# Uses cohort survival and disability adjustment
############################################################

library(progress)

calculate_l <- function(data,S){
  data_m <- data %>%
    subset(.,sex == S) %>%
    mutate(D_val=s_val*val.y, D_lower=s_lower*lower, D_upper=s_upper*upper)
  
  library(tidyr)
  Nt_ca<- as.data.frame(matrix(nrow=0,ncol=7))
  
  total_iterations <- n_distinct(data$age) * n_distinct(data$year)
  pb <- progress_bar$new(
    format = "Calculating [:bar] :percent | :current/:total | eta: :eta",
    total = total_iterations,
    width = 60
  )
  
  for (a in 1:n_distinct(data$age)) {
    Nt_ct<- as.data.frame(matrix(nrow=0,ncol=6))
    for (t in 1:n_distinct(data$year)) {
      pb$tick()
      result_val<- as.data.frame(matrix(nrow=204,ncol=45,1))
      result_lower<- as.data.frame(matrix(nrow=204,ncol=45,1))
      result_upper<- as.data.frame(matrix(nrow=204,ncol=45,1))
      tmp<- as.data.frame(matrix(nrow=204,ncol=45))
      data_L <- data_matrix(data_m,age_name[a],year_name[t],'val.x')
      L_matrix <- spread(data_L, key = cause_name, value = val.x)
      L_matrix <- select(L_matrix,c(2:46))
      if(min(a,t) == 1 | t<6){
        result_val <- L_matrix
        result_lower <- L_matrix
        result_upper <- L_matrix
      }else{
        for (i in 0:(min(a, floor((t+4)/5))-2)) {
          data_D_val <- data_matrix(data_m,age_name[a-1-i],year_name[t-5-5*i],'D_val')
          data_D_lower <- data_matrix(data_m,age_name[a-1-i],year_name[t-5-5*i],'D_lower')
          data_D_upper <- data_matrix(data_m,age_name[a-1-i],year_name[t-5-5*i],'D_upper')

          D_val_matrix <- spread(data_D_val, key = cause_name, value = D_val)
          D_lower_matrix <- spread(data_D_lower, key = cause_name, value = D_lower)
          D_upper_matrix <- spread(data_D_upper, key = cause_name, value = D_upper)
          D_val_matrix <- select(D_val_matrix,c(2:46))
          D_lower_matrix <- select(D_lower_matrix,c(2:46))
          D_upper_matrix <- select(D_upper_matrix,c(2:46))
          data_P <- data_matrix(data_m,age_name[a-1-i],year_name[t-5-5*i],'p')

          P_matrix <- spread(data_P, key = cause_name, value = p)
          P_matrix <- select(P_matrix,c(2:46))
          tmp_val <- 1-(P_matrix^i)*D_val_matrix
          result_val <- result_val*tmp_val
          tmp_lower <- 1-(P_matrix^i)*D_lower_matrix
          result_lower <- result_lower*tmp_lower
          tmp_upper <- 1-(P_matrix^i)*D_upper_matrix
          result_upper <- result_upper*tmp_upper
        }
        result_val <- L_matrix/result_val
        result_lower <- L_matrix/result_lower
        result_upper <- L_matrix/result_upper
      }
      result_val$WBcode <- WB_name
      result_lower$WBcode <- WB_name
      result_upper$WBcode <- WB_name

      result_val_data <- gather(data = as.data.frame(result_val), key = "cause_name", value = "L_val", -WBcode)
      result_lower_data <- gather(data = as.data.frame(result_lower), key = "cause_name", value = "L_lower", -WBcode)
      result_upper_data <- gather(data = as.data.frame(result_upper), key = "cause_name", value = "L_upper", -WBcode)
      result_data <- result_val_data %>% merge(.,result_lower_data,by=c('cause_name','WBcode'))
      result_data <- result_data %>% merge(.,result_upper_data,by=c('cause_name','WBcode'))
      result_data$year <- year_name[t]
      Nt_ct <- rbind(Nt_ct,result_data)
    }
    Nt_ct$age <- age_name[a]
    Nt_ca <- rbind(Nt_ca,Nt_ct)
  }
  Nt_ca$sex <- S
  return(Nt_ca)
}


lt_cm <- calculate_l(data_lt,'male')
lt_cf <- calculate_l(data_lt,'female')


lt_c <- lt_cm %>%
  rbind(lt_cf) %>%
  select(.,c(2,1,8,6,7,3:5)) %>%
  arrange(.,cause_name,WBcode,sex,year,age) %>%
  filter(!(age %in% c('0-4','5-9','10-14')))

write.csv(lt_c,"/dssg/home/economic2/outcome/rural outcome/labor_cf_1950_ui.csv")
