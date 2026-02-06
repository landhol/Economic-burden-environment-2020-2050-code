################################################################################################################
# Heat Map: Data Preparation and Plotting
# Purpose: Process risk-related economic burden data and generate heatmaps comparing countries, regions,
#          income groups, and global metrics.
################################################################################################################

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#-----------------------------
# 1. Load Country Data
#-----------------------------
country <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/country name/location_id_old.csv")
country_n <- country %>% select(3,4) %>% rename(WBcode = Country.Code, country = location_id)
country_name <- country %>% select(2,3) %>% rename(WBcode = Country.Code, country = location_name)

#-----------------------------
# 2. Load GDP Data
#-----------------------------
gdp <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/gdp_161950_n.csv')
gdp_country <- gdp %>% 
  select(2:4) %>% 
  subset(year > 2019) %>% 
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  group_by(WBcode) %>% 
  mutate(val.gdp = sum(val.gdp) / 10^6) %>%  # convert to million USD
  select(-year) %>% 
  distinct(WBcode, country, .keep_all = TRUE)

#-----------------------------
# 3. Load Income Group Data
#-----------------------------
inc_grp <- read_xlsx("/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/income group/CLASS.xlsx", sheet = 1)
inc_grp_n <- inc_grp %>% 
  select(2:4) %>%
  head(218) %>%
  rename(WBcode = Code, income = `Income group`) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  mutate(
    income = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'),'Others',income),
    Region = ifelse(WBcode %in% c('COK','NIU','TKL','VEN'),'Others',Region)
  )

#-----------------------------
# 4. Load Risk-Specific Economic Burden Data (Air, Water, Temperature, Other, Metabolic, Occupational)
#-----------------------------
cost_air <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_rei.csv')
cost_wat <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_rei.csv')
cost_tem <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_rei.csv')

# All-cause and cause-specific data (Air)
air_allcause <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_allcause.csv')
air_apm <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm.csv')
air_hap <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap.csv')
air_aop <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_aop.csv')

# Temperature, Other, Metabolic, and Environmental/Occupational
tem_allcause <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_allcause.csv')
tem_ht <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_ht.csv')
tem_lt <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_lt.csv')

#-----------------------------
# 5. Function: Compute Percent GDP Burden and Aggregate by Location
#-----------------------------
fig_heat <- function(data1, data2, rei){
  # Select top 10 countries by risk burden
  data_country <- data1 %>% select(-X) %>% merge(inc_grp_n, by = 'country') %>%
    filter(rei_name == rei) %>% arrange(desc(burden)) %>% slice(1:10)
  
  country_code <- unique(data_country$WBcode)
  
  # Country-level percent of GDP
  data_country <- data2 %>% merge(inc_grp_n, by='country') %>%
    merge(gdp_country, by=c('country','WBcode')) %>%
    filter(WBcode %in% country_code) %>% merge(country_name, by='WBcode') %>%
    mutate(WBcode = factor(WBcode, levels = country_code)) %>%
    arrange(cause_name, WBcode) %>%
    group_by(cause_name, WBcode) %>%
    mutate(percent = burden / val.gdp * 100) %>% ungroup() %>%
    select(c('cause_name','country','percent')) %>% rename(location = country)
  
  # Region-level aggregation
  data_region <- data2 %>% select(-X) %>% merge(inc_grp_n, by='country') %>%
    merge(gdp_country, by=c('country','WBcode')) %>%
    group_by(cause_name, Region) %>%
    summarise(burden = sum(burden), gdp = sum(val.gdp), .groups='drop') %>%
    mutate(percent = burden / gdp * 100, location = Region) %>%
    select(c('cause_name','location','percent','burden'))
  
  # Income group aggregation
  data_inc <- data2 %>% select(-X) %>% merge(inc_grp_n, by='country') %>%
    merge(gdp_country, by=c('country','WBcode')) %>%
    group_by(cause_name, income) %>%
    summarise(burden = sum(burden), gdp = sum(val.gdp), .groups='drop') %>%
    mutate(percent = burden / gdp * 100, location = income) %>%
    mutate(location = factor(location, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>%
    rbind(data_region)
  
  # Global-level aggregation
  data_global <- data2 %>% select(-X) %>% merge(inc_grp_n, by='country') %>%
    merge(gdp_country, by=c('country','WBcode')) %>%
    group_by(cause_name) %>%
    summarise(burden = sum(burden), gdp = sum(val.gdp), .groups='drop') %>%
    mutate(percent = burden / gdp * 100, location = 'Global') %>%
    rbind(data_inc)
  
  return(data_global)
}

air_all_heat <- fig_heat(cost_air,air_allcause,'Air pollution')
write.csv(air_all_heat,'/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_all_heat.csv')

air_apm_heat <- fig_heat(cost_air,air_apm,'Ambient particulate matter pollution')
write.csv(air_apm_heat,'/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm_heat.csv')

air_hap_heat <- fig_heat(cost_air,air_hap,'Household air pollution from solid fuels')
write.csv(air_hap_heat,'/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap_heat.csv')

air_aop_heat <- fig_heat(cost_air,air_aop,'Ambient ozone pollution')##只有COPDmet


tem_all_heat <- fig_heat(cost_tem, tem_allcause, 'Non-optimal temperature')
write.csv(tem_all_heat, '/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_all_heat.csv')

tem_ht_heat <- fig_heat(cost_tem, tem_ht, 'High temperature')
write.csv(tem_ht_heat, '/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_ht_heat.csv')

tem_lt_heat <- fig_heat(cost_tem, tem_lt, 'Low temperature')
write.csv(tem_lt_heat, '/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_lt_heat.csv')


####################################
library(dplyr)
library(tidyr)

heat_plot <- function(data,rei){
  national_means <- data %>%
    filter(location == "Global") %>%
    select(cause_name, national_mean1 = burden, national_mean2 = percent)
  
  df_plot <- data %>%
    left_join(national_means, by = "cause_name") %>%
    mutate(category1 = case_when(
      burden < 0.2 * national_mean1 ~ "Very low",
      burden < 0.4 * national_mean1 ~ "Low",
      burden <= 0.6 * national_mean1 ~ "Moderate",
      burden <= 0.8 * national_mean1 ~ "High",
      TRUE ~ "Very high"
    ),
    cause_abbr = case_when(
      cause_name == "Age-related and other hearing loss" ~ "ARHL",
      cause_name == "Asthma" ~ "Asthma",
      cause_name == "Cardiomyopathy and myocarditis" ~ "CM",
      cause_name == "Chronic kidney disease" ~ "CKD",
      cause_name == "Chronic obstructive pulmonary disease" ~ "COPD",
      cause_name == "Diabetes mellitus type 1" ~ "T1DM",
      cause_name == "Diabetes mellitus type 2" ~ "T2DM",
      cause_name == "Enteric infections" ~ "EI",
      cause_name == "Exposure to mechanical forces" ~ "EMF",
      cause_name == "Falls" ~ "Falls",
      cause_name == "Hemolytic disease and other neonatal jaundice" ~ "HDNJ",
      cause_name == "Hypertensive heart disease" ~ "HHD",
      cause_name == "Idiopathic developmental intellectual disability" ~ "IDID",
      cause_name == "Interpersonal violence" ~ "IPV",
      cause_name == "Ischemic heart disease" ~ "IHD",
      cause_name == "Larynx cancer" ~ "LCa",
      cause_name == "Leukemia" ~ "Leukemia",
      cause_name == "Low back pain" ~ "LBP",
      cause_name == "Lower respiratory infections" ~ "LRI",
      cause_name == "Mesothelioma" ~ "Mesothelioma",
      cause_name == "Nasopharynx cancer" ~ "NPCa",
      cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" ~ "NEBAT",
      cause_name == "Neonatal preterm birth" ~ "NPE",
      cause_name == "Neonatal sepsis and other neonatal infections" ~ "NSNI",
      cause_name == "Other infectious diseases" ~ "OID",
      cause_name == "Other unintentional injuries" ~ "OUI",
      cause_name == "Ovarian cancer" ~ "OCa",
      cause_name == "Pneumoconiosis" ~ "Pneumoconiosis",
      cause_name == "Self-harm" ~ "Self-harm",
      cause_name == "Stroke" ~ "Stroke",
      cause_name == "Tracheal, bronchus, and lung cancer" ~ "TBL",
      cause_name == "Transport injuries" ~ "TI"
    )) %>%
    mutate(category2 = case_when(
      percent < 0.5 * national_mean2 ~ "Significantly lower",
      percent < 0.8 * national_mean2 ~ "Slightly lower",
      percent <= 1.2 * national_mean2 ~ "Similar to global",
      percent <= 1.5 * national_mean2 ~ "Slightly higher",
      TRUE ~ "Significantly higher"
    ))%>%
    mutate(location = factor(location, levels = rev(unique(location))),
           category1 = factor(category1, levels = c(
             "Very low",
             "Low",
             "Moderate",
             "High",
             "Very high")),
           category2 = factor(category2, levels = c(
             "Significantly lower",
             "Slightly lower",
             "Similar to global",
             "Slightly higher",
             "Significantly higher")),
           )
  
  desired_locs <- c(
    "Global",
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income",
    "East Asia & Pacific",
    "Europe & Central Asia",
    "Latin America & Caribbean",
    "Middle East & North Africa",
    "North America",
    "South Asia",
    "Sub-Saharan Africa"
  )

  
  order_abbr <- df_plot %>%
    filter(location == "Global") %>%
    arrange(desc(burden)) %>%
    mutate(cause_abbr = case_when(
      cause_name == "Age-related and other hearing loss" ~ "ARHL",
      cause_name == "Asthma" ~ "Asthma",
      cause_name == "Cardiomyopathy and myocarditis" ~ "CM",
      cause_name == "Chronic kidney disease" ~ "CKD",
      cause_name == "Chronic obstructive pulmonary disease" ~ "COPD",
      cause_name == "Diabetes mellitus type 1" ~ "T1DM",
      cause_name == "Diabetes mellitus type 2" ~ "T2DM",
      cause_name == "Enteric infections" ~ "EI",
      cause_name == "Exposure to mechanical forces" ~ "EMF",
      cause_name == "Falls" ~ "Falls",
      cause_name == "Hemolytic disease and other neonatal jaundice" ~ "HDNJ",
      cause_name == "Hypertensive heart disease" ~ "HHD",
      cause_name == "Idiopathic developmental intellectual disability" ~ "IDID",
      cause_name == "Interpersonal violence" ~ "IPV",
      cause_name == "Ischemic heart disease" ~ "IHD",
      cause_name == "Larynx cancer" ~ "LCa",
      cause_name == "Leukemia" ~ "Leukemia",
      cause_name == "Low back pain" ~ "LBP",
      cause_name == "Lower respiratory infections" ~ "LRI",
      cause_name == "Mesothelioma" ~ "Mesothelioma",
      cause_name == "Nasopharynx cancer" ~ "NPCa",
      cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" ~ "NEBAT",
      cause_name == "Neonatal preterm birth" ~ "NPE",
      cause_name == "Neonatal sepsis and other neonatal infections" ~ "NSNI",
      cause_name == "Other infectious diseases" ~ "OID",
      cause_name == "Other unintentional injuries" ~ "OUI",
      cause_name == "Ovarian cancer" ~ "OCa",
      cause_name == "Pneumoconiosis" ~ "Pneumoconiosis",
      cause_name == "Self-harm" ~ "Self-harm",
      cause_name == "Stroke" ~ "Stroke",
      cause_name == "Tracheal, bronchus, and lung cancer" ~ "TBL",
      cause_name == "Transport injuries" ~ "TI"
    )) %>%
    pull(cause_abbr)
  
  df_long <- df_plot %>%
    pivot_longer(
      cols = c(burden, percent),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
           category = ifelse(metric == "burden", as.character(category1), as.character(category2)),
           category = factor(category, levels = c(
             "Very low", "Low", "Moderate", "High", "Very high",
             "Significantly lower", "Slightly lower", "Similar to global",
             "Slightly higher", "Significantly higher"
           )),
           label = ifelse(metric == "percent",
                          paste0(round(value * 100, 2), "%"),
                          format(round(value, 0), big.mark = ",")),

      metric = factor(metric, levels = c("burden", "percent")),)%>% mutate(cause_abbr = factor(cause_abbr, levels = order_abbr))
  
  df_long <- df_long %>%
    mutate(
      location = factor(location, levels = (rev(desired_locs)))
    )
  # 2. Define a sophisticated color palette
  my_colors <- c(
    "Significantly lower"       = "#699bbb",  
    "Slightly lower"            = "#A3B1C0",  
    "Similar to global"  = "#E8E8E8",  
    "Slightly higher"           = "#C4A0A1",  
    "Significantly higher"      = "#B77A76" ,
      "Very low"               = "#699bbb", 
      "Low"           = "#A3B1C0",
      "Moderate"  = "#E8E8E8",
      "High"          = "#C4A0A1", 
      "Very high"      = "#B77A76" 
  )

  plot <- ggplot(df_long, aes(x = metric, y = location, fill = category)) +
    geom_tile(color = "white", size = 0.4) +
    geom_text(aes(label = label), size = 3, fontface = "bold") + 
    scale_fill_manual(values = my_colors, name = "Compared to Global Mean") +
    facet_grid(~ cause_abbr, scales = "free_x", space = "free_x") +
    coord_cartesian(expand = FALSE) +  
    theme_minimal(base_size = 16) + 
    theme(
      strip.text       = element_text(size = 16, face = "bold"),
      axis.text.x      = element_text(size = 14, face = "bold", color = "black"),
      axis.text.y      = element_text(size = 14, face = "bold", color = "black"),
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      legend.position  = "bottom",
      legend.text      = element_text(size = 14, face = "bold"),
      legend.title     = element_text(size = 14, face = "bold"),
      panel.grid       = element_blank(),
      panel.spacing.x  = unit(0.2, "lines"), 
      plot.margin      = margin(10, 10, 10, 10)
    )+
    labs(
      title    = paste("Macroeconomic burden of", rei),
      subtitle = "Burden and as a percent of GDP by disease type (in millions)",
      x        = NULL,
      y        = NULL
    )
}
 
air_all_plot <- heat_plot(air_all_heat,'air pollution')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/air_all.pdf',air_all_plot, width=18, height=10)

air_apm_plot <- heat_plot(air_apm_heat,'ambient particulate matter pollution')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/air_apm.pdf',air_apm_plot, width=18, height=10)

air_hap_plot <- heat_plot(air_hap_heat,'household air pollution from solid fuels')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/air_hap.pdf',air_hap_plot, width=18, height=10)

##
tem_all_plot <- heat_plot(tem_all_heat, 'non-optimal temperature')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/tem_all.pdf', tem_all_plot, width=18, height=10)

tem_ht_plot <- heat_plot(tem_ht_heat, 'high temperature')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/tem_ht.pdf', tem_ht_plot, width=18, height=10)

tem_lt_plot <- heat_plot(tem_lt_heat, 'low temperature')
ggsave(file='/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/figure/air&met/heat map/tem_lt.pdf', tem_lt_plot, width=18, height=10)
