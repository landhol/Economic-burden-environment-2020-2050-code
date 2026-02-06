############################################################
# Script: 10_by_country_map.R
# Purpose:
#   Prepare and plot country-level choropleth maps of
#   macroeconomic burdens and related indicators for the
#   manuscript. Functions harmonize country names between
#   data and map tiles, bin values into quantiles, and
#   produce global and regional maps and insets.
#
# Notes:
#   - This script preserves original data paths and logic.
#   - Only comments were translated/standardized to English.
############################################################

library(dplyr)
library(sf)
library(calibrate)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(maps)

# -----------------------------------------------------------------------------
# Helper: mapmain_10
#   Join country-level values to world polygon data and cut into decile bins.
#   Input: data.frame with two columns (value, location).
#   Output: data frame ready for ggplot (with a categorical 'val2' field).
# -----------------------------------------------------------------------------
mapmain_10 <- function(data) {
  ASR <- data
  colnames(ASR) <- c("val", "location")
  
  # Load world polygon data (map tiles)
  worldData <- map_data('world')
  country_asr <- ASR
  country_asr$location <- as.character(country_asr$location)
  
  # Harmonize country names between input dataset and map_data regions
  country_asr$location[country_asr$location == 'United States of America'] <- 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] <- 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] <- 'UK'
  country_asr$location[country_asr$location == 'Congo'] <- 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] <- 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] <- 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] <- 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] <- 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] <- 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] <- 'Ivory Coast'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] <- 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] <- 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] <- 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] <- 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] <- 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] <- 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] <- 'Micronesia'
  country_asr$location[country_asr$location == "Macedonia"] <- 'North Macedonia'
  country_asr$location[country_asr$location == "Cabo Verde"] <- 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] <- 'Virgin Islands'
  country_asr$location[country_asr$location == "Eswatini"] <- 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] <- 'Brunei'
  
  # Split multi-island country entries into separate rows expected by the map tiles
  original_row <- country_asr[country_asr$location == "Trinidad and Tobago", ]
  Trinidad_row <- original_row; Trinidad_row$location <- "Trinidad"
  Tobago_row <- original_row; Tobago_row$location <- "Tobago"
  country_asr <- country_asr[country_asr$location != "Trinidad and Tobago", ]
  country_asr <- rbind(country_asr, Trinidad_row, Tobago_row)
  
  original_row <- country_asr[country_asr$location == "Antigua and Barbuda", ]
  antigua_row <- original_row; antigua_row$location <- "Antigua"
  barbuda_row <- original_row; barbuda_row$location <- "Barbuda"
  country_asr <- country_asr[country_asr$location != "Antigua and Barbuda", ]
  country_asr <- rbind(country_asr, antigua_row, barbuda_row)
  
  original_row <- country_asr[country_asr$location == "Saint Kitts and Nevis", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Kitts"
  Nevis_row <- original_row; Nevis_row$location <- "Nevis"
  country_asr <- country_asr[country_asr$location != "Saint Kitts and Nevis", ]
  country_asr <- rbind(country_asr, Saint_row, Nevis_row)
  
  original_row <- country_asr[country_asr$location == "Saint Vincent and the Grenadines", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Vincent"
  Grenadines_row <- original_row; Grenadines_row$location <- "Grenadines"
  country_asr <- country_asr[country_asr$location != "Saint Vincent and the Grenadines", ]
  country_asr <- rbind(country_asr, Saint_row, Grenadines_row)
  
  # Join polygon data and country values
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  
  # Remove Antarctica
  total <- subset(total, region != "Antarctica")
  
  # Compute decile breaks (non-NA values only)
  break1 <- unname(quantile((total$val[!is.na(total$val)]),
                            probs = c(0, seq(0.1, 0.9, by = 0.1), 1)))
  
  # Cut into labeled bins for plotting
  total <- total %>%
    mutate(
      val2 = cut(
        val,
        breaks = break1,
        labels = c(
          paste('< ', round(break1[2]), sep = ""),
          paste(round(break1[2]), " to < ", round(break1[3]), sep = ""),
          paste(round(break1[3]), " to < ", round(break1[4]), sep = ""),
          paste(round(break1[4]), " to < ", round(break1[5]), sep = ""),
          paste(round(break1[5]), " to < ", round(break1[6]), sep = ""),
          paste(round(break1[6]), " to < ", round(break1[7]), sep = ""),
          paste(round(break1[7]), " to < ", round(break1[8]), sep = ""),
          paste(round(break1[8]), " to < ", round(break1[9]), sep = ""),
          paste(round(break1[9]), " to < ", round(break1[10]), sep = ""),
          paste('≥ ', round(break1[10]), sep = "")
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  return(total)
}

# -----------------------------------------------------------------------------
# mapmain_8: same as mapmain_10 but with 8 bins (used in other panels)
# -----------------------------------------------------------------------------
mapmain_8 <- function(data) {
  ASR <- data
  colnames(ASR) <- c("val", "location")
  worldData <- map_data('world')
  country_asr <- ASR
  country_asr$location <- as.character(country_asr$location)
  
  # Harmonize names (same mapping as above)
  country_asr$location[country_asr$location == 'United States of America'] <- 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] <- 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] <- 'UK'
  country_asr$location[country_asr$location == 'Congo'] <- 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] <- 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] <- 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] <- 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] <- 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] <- 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] <- 'Ivory Coast'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] <- 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] <- 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] <- 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] <- 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] <- 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] <- 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] <- 'Micronesia'
  country_asr$location[country_asr$location == "Macedonia"] <- 'North Macedonia'
  country_asr$location[country_asr$location == "Cabo Verde"] <- 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] <- 'Virgin Islands'
  country_asr$location[country_asr$location == "Eswatini"] <- 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] <- 'Brunei'
  
  # Split multi-island names
  original_row <- country_asr[country_asr$location == "Trinidad and Tobago", ]
  Trinidad_row <- original_row; Trinidad_row$location <- "Trinidad"
  Tobago_row <- original_row; Tobago_row$location <- "Tobago"
  country_asr <- country_asr[country_asr$location != "Trinidad and Tobago", ]
  country_asr <- rbind(country_asr, Trinidad_row, Tobago_row)
  
  original_row <- country_asr[country_asr$location == "Antigua and Barbuda", ]
  antigua_row <- original_row; antigua_row$location <- "Antigua"
  barbuda_row <- original_row; barbuda_row$location <- "Barbuda"
  country_asr <- country_asr[country_asr$location != "Antigua and Barbuda", ]
  country_asr <- rbind(country_asr, antigua_row, barbuda_row)
  
  original_row <- country_asr[country_asr$location == "Saint Kitts and Nevis", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Kitts"
  Nevis_row <- original_row; Nevis_row$location <- "Nevis"
  country_asr <- country_asr[country_asr$location != "Saint Kitts and Nevis", ]
  country_asr <- rbind(country_asr, Saint_row, Nevis_row)
  
  original_row <- country_asr[country_asr$location == "Saint Vincent and the Grenadines", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Vincent"
  Grenadines_row <- original_row; Grenadines_row$location <- "Grenadines"
  country_asr <- country_asr[country_asr$location != "Saint Vincent and the Grenadines", ]
  country_asr <- rbind(country_asr, Saint_row, Grenadines_row)
  
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  total <- subset(total, region != "Antarctica")
  
  # Compute octile breaks for map categorization
  break1 <- unname(quantile((total$val[!is.na(total$val)]),
                            probs = c(0, seq(0.125, 0.875, by = 0.125), 1)))
  
  total <- total %>%
    mutate(
      val2 = cut(
        val,
        breaks = break1,
        labels = c(
          paste('<', round(break1[2]), sep = ""),
          paste(round(break1[2]), " to < ", round(break1[3]), sep = ""),
          paste(round(break1[3]), " to < ", round(break1[4]), sep = ""),
          paste(round(break1[4]), " to < ", round(break1[5]), sep = ""),
          paste(round(break1[5]), " to < ", round(break1[6]), sep = ""),
          paste(round(break1[6]), " to < ", round(break1[7]), sep = ""),
          paste(round(break1[7]), " to < ", round(break1[8]), sep = ""),
          paste('≥', round(break1[8]), sep = "")
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  return(total)
}

# -----------------------------------------------------------------------------
# Global map function for choropleth panels
# -----------------------------------------------------------------------------
mapworld <- function(a, rei) {
  p <- ggplot()
  p2 <- p +
    geom_polygon(
      data = a,
      aes(x = long, y = lat, group = group, fill = val2),
      colour = "black",
      size = .2
    ) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    theme(aspect.ratio = 1 / 3) + xlim(-200, 200) + ylim(-150, 100) +
    theme_void() +
    labs(x = "", y = "") +
    guides(fill = guide_legend(
      title = paste('Macroeconomic burden \nattributable to ', rei, '\n, 2020-2050', sep = ''),
      ncol = 2, byrow = TRUE, keywidth = 0.32, keyheight = 0.08, default.unit = "inch"
    )) +
    theme(
      legend.position = c(0.1, 0.4), legend.justification = c(0, 0),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 14)
    )
  return(p2)
}

# -----------------------------------------------------------------------------
# Subplot functions for regional insets (caribbean, Persian Gulf, Balkans, SE Asia)
# -----------------------------------------------------------------------------
sub1 <- function(data) {
  p <- ggplot()
  
  caribbean <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(-90, -60), ylim = c(8, 26)) +
    theme(aspect.ratio = 3 / 4) + theme_void() +
    labs(x = "", y = "") +
    theme(legend.position = 'none') +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  Persian <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(47, 55.5), ylim = c(21, 31)) +
    theme_void() + labs(x = "", y = "") +
    theme(legend.position = 'none') +
    theme(aspect.ratio = 1 / 1) +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  Balkan <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(14, 31), ylim = c(35.5, 49)) +
    theme_void() + labs(x = "", y = "") +
    theme(legend.position = 'none') +
    theme(aspect.ratio = 1 / 1.25) +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  Southeast <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(94, 117.5), ylim = c(-9.5, 8.5)) +
    theme(aspect.ratio = 1 / 1.4) +
    theme_void() + labs(x = "", y = "") +
    theme(legend.position = 'none') +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  sub1 <- caribbean | Persian | Balkan | Southeast
  return(sub1)
}

sub2 <- function(data) {
  p <- ggplot()
  westafrica <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(-17, -10), ylim = c(7, 15)) +
    theme(aspect.ratio = 1 / 1) + theme_void() +
    labs(x = "", y = "") + theme(legend.position = 'none') +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  EasternMediterranean <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(31, 38), ylim = c(29, 35.5)) +
    theme(aspect.ratio = 1 / 1) + theme_void() +
    labs(x = "", y = "") + theme(legend.position = 'none') +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  sub2 <- westafrica | EasternMediterranean
  return(sub2)
}

sub3 <- function(data) {
  p <- ggplot()
  NorthernEurope <- p +
    geom_polygon(data, mapping = aes(x = long, y = lat, group = group, fill = val2),
                 colour = "black", size = .2) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    coord_sf(xlim = c(3, 28), ylim = c(49.5, 58.5)) +
    theme(aspect.ratio = 1 / 1.5) + theme_void() +
    labs(x = "", y = "") + theme(legend.position = 'none') +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
  
  sub3 <- NorthernEurope
  return(sub3)
}

# -----------------------------------------------------------------------------
# Load burden results and harmonize for mapping
# -----------------------------------------------------------------------------
air_rei <- read.csv("/dssg/home/economic2/outcome/rural outcome/air_rei.csv") # in million$
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(2,4)) %>% rename(country = location_id)

# Select total air pollution burden and convert to billion
air_rei_n <- air_rei %>%
  subset(rei_name == 'Air pollution') %>%
  select('country', 'burden') %>%
  merge(country_n, by = 'country') %>%
  select(-country) %>%
  rename(country = location_name) %>%
  mutate(burden = burden / 1000) # convert million -> billion


# Create map-ready objects and plot (global + insets)
air_ASR_2019 <- mapmain_10(air_rei_n)
air_tot_2019 <- mapworld(air_ASR_2019, 'air pollution')
air_tot2019_sub1 <- sub1(air_ASR_2019)
air_tot2019_sub2 <- sub2(air_ASR_2019)
air_tot2019_sub3 <- sub3(air_ASR_2019)

ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot.pdf',
       air_tot_2019, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub1.pdf',
       air_tot2019_sub1, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub2.pdf',
       air_tot2019_sub2, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub3.pdf',
       air_tot2019_sub3, width = 18, height = 10)


# -----------------------------------------------------------------------------
# Percent of GDP: prepare map where burden is expressed as percent of GDP
# -----------------------------------------------------------------------------
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(2,3)) %>% rename(WBcode = Country.Code, country = location_name)
gdp <- read.csv('/dssg/home/economic2/outcome/rural outcome/all/gdp_161950_n.csv')
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  merge(country_n, by = 'WBcode', all.y = TRUE) %>%
  group_by(WBcode) %>%
  mutate(val.gdp = sum(val.gdp) / 1e9) %>%  # total GDP in billion$
  select(-year) %>%
  distinct(WBcode, country, .keep_all = TRUE)

air_percent <- air_rei_n %>% merge(gdp_country, by = 'country') %>%
  mutate(gdp = burden / val.gdp * 100 * 1e6) %>% select('country', 'gdp')

# The multiplication by 1e6 above scales percent values to a plotting-friendly range.
# After plotting, adjust the legend label back to true percent values.

# mapmain2: build percent-of-GDP map (labels scaled to billions in the legend)
mapmain2 <- function(data) {
  ASR <- data
  colnames(ASR) <- c("location", "val")
  worldData <- map_data('world')
  country_asr <- ASR
  country_asr$location <- as.character(country_asr$location)
  
  # Harmonize names (same mapping as earlier)
  country_asr$location[country_asr$location == 'United States of America'] <- 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] <- 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] <- 'UK'
  country_asr$location[country_asr$location == 'Congo'] <- 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] <- 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] <- 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] <- 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] <- 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] <- 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] <- 'Ivory Coast'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] <- 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] <- 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] <- 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] <- 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] <- 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] <- 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] <- 'Micronesia'
  country_asr$location[country_asr$location == "Macedonia"] <- 'North Macedonia'
  country_asr$location[country_asr$location == "Cabo Verde"] <- 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] <- 'Virgin Islands'
  country_asr$location[country_asr$location == "Eswatini"] <- 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] <- 'Brunei'
  
  # Split multi-island names
  original_row <- country_asr[country_asr$location == "Trinidad and Tobago", ]
  Trinidad_row <- original_row; Trinidad_row$location <- "Trinidad"
  Tobago_row <- original_row; Tobago_row$location <- "Tobago"
  country_asr <- country_asr[country_asr$location != "Trinidad and Tobago", ]
  country_asr <- rbind(country_asr, Trinidad_row, Tobago_row)
  
  original_row <- country_asr[country_asr$location == "Antigua and Barbuda", ]
  antigua_row <- original_row; antigua_row$location <- "Antigua"
  barbuda_row <- original_row; barbuda_row$location <- "Barbuda"
  country_asr <- country_asr[country_asr$location != "Antigua and Barbuda", ]
  country_asr <- rbind(country_asr, antigua_row, barbuda_row)
  
  original_row <- country_asr[country_asr$location == "Saint Kitts and Nevis", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Kitts"
  Nevis_row <- original_row; Nevis_row$location <- "Nevis"
  country_asr <- country_asr[country_asr$location != "Saint Kitts and Nevis", ]
  country_asr <- rbind(country_asr, Saint_row, Nevis_row)
  
  original_row <- country_asr[country_asr$location == "Saint Vincent and the Grenadines", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Vincent"
  Grenadines_row <- original_row; Grenadines_row$location <- "Grenadines"
  country_asr <- country_asr[country_asr$location != "Saint Vincent and the Grenadines", ]
  country_asr <- rbind(country_asr, Saint_row, Grenadines_row)
  
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  total <- subset(total, region != "Antarctica")
  
  # Compute decile breaks on original ASR$val for percent-of-GDP plotting
  break1 <- unname(quantile((ASR$val[!is.na(ASR$val)]),
                            probs = c(0, seq(0.1, 0.9, by = 0.1), 1)))
  
  total <- total %>%
    mutate(
      val2 = cut(
        val,
        breaks = break1,
        labels = c(
          paste("< ", round(break1[2] / 1e6, 2), sep = ""),
          paste(round(break1[2] / 1e6, 2), " to < ", round(break1[3] / 1e6, 2), sep = ""),
          paste(round(break1[3] / 1e6, 2), " to < ", round(break1[4] / 1e6, 2), sep = ""),
          paste(round(break1[4] / 1e6, 2), " to < ", round(break1[5] / 1e6, 2), sep = ""),
          paste(round(break1[5] / 1e6, 2), " to < ", round(break1[6] / 1e6, 2), sep = ""),
          paste(round(break1[6] / 1e6, 2), " to < ", round(break1[7] / 1e6, 2), sep = ""),
          paste(round(break1[7] / 1e6, 2), " to < ", round(break1[8] / 1e6, 2), sep = ""),
          paste(round(break1[8] / 1e6, 2), " to < ", round(break1[9] / 1e6, 2), sep = ""),
          paste(round(break1[9] / 1e6, 2), " to < ", round(break1[10] / 1e6, 2), sep = ""),
          paste("≥ ", round(break1[10] / 1e6, 2))
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  return(total)
}

mapworld2 <- function(total, rei) {
  p <- ggplot()
  p2 <- p +
    geom_polygon(
      data = total,
      aes(x = long, y = lat, group = group, fill = val2),
      colour = "black", size = .2
    ) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1) +
    theme(aspect.ratio = 1 / 3) + xlim(-200, 200) + ylim(-150, 100) +
    theme_void() +
    labs(x = "", y = "") +
    guides(fill = guide_legend(
      title = paste('Macroeconomic burden \nattributable to ', rei, '\n as a percent of GDP\n, 2020-2050', sep = ''),
      ncol = 2, byrow = TRUE, keywidth = 0.32, keyheight = 0.08, default.unit = "inch"
    )) +
    theme(
      legend.position = c(0.1, 0.4), legend.justification = c(0, 0),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 14)
    )
  return(p2)
}

air_ASR_2019_2 <- mapmain2(air_percent)
air_tot_2019_2 <- mapworld2(air_ASR_2019_2, 'air pollution')
air_tot2019_sub1_2 <- sub1(air_ASR_2019_2)
air_tot2019_sub2_2 <- sub2(air_ASR_2019_2)
air_tot2019_sub3_2 <- sub3(air_ASR_2019_2)

ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_gdp.pdf',
       air_tot_2019_2, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub1_gdp.pdf',
       air_tot2019_sub1_2, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub2_gdp.pdf',
       air_tot2019_sub2_2, width = 18, height = 10)
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_tot_sub3_gdp.pdf',
       air_tot2019_sub3_2, width = 18, height = 10)

# -----------------------------------------------------------------------------
# Risk-type and top-cause maps: identify top contributor per country
# -----------------------------------------------------------------------------
country <- read.csv("/dssg/home/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(2,4)) %>% rename(country = location_id)

# mapmain3: categorize by custom breaks and labels (used for categorical maps)
mapmain3 <- function(data, data_break, data_labels) {
  ASR <- data
  colnames(ASR) <- c("location", "val")
  worldData <- map_data('world')
  country_asr <- ASR
  country_asr$location <- as.character(country_asr$location)
  
  # Harmonize names (re-used mapping)
  country_asr$location[country_asr$location == 'United States of America'] <- 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] <- 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] <- 'UK'
  country_asr$location[country_asr$location == 'Congo'] <- 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] <- 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] <- 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] <- 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] <- 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] <- 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] <- 'Ivory Coast'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] <- 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] <- 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] <- 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] <- 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] <- 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] <- 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] <- 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] <- 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] <- 'Micronesia'
  country_asr$location[country_asr$location == "Macedonia"] <- 'North Macedonia'
  country_asr$location[country_asr$location == "Cabo Verde"] <- 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] <- 'Virgin Islands'
  country_asr$location[country_asr$location == "Eswatini"] <- 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] <- 'Brunei'
  
  # Split multi-island names
  original_row <- country_asr[country_asr$location == "Trinidad and Tobago", ]
  Trinidad_row <- original_row; Trinidad_row$location <- "Trinidad"
  Tobago_row <- original_row; Tobago_row$location <- "Tobago"
  country_asr <- country_asr[country_asr$location != "Trinidad and Tobago", ]
  country_asr <- rbind(country_asr, Trinidad_row, Tobago_row)
  
  original_row <- country_asr[country_asr$location == "Antigua and Barbuda", ]
  antigua_row <- original_row; antigua_row$location <- "Antigua"
  barbuda_row <- original_row; barbuda_row$location <- "Barbuda"
  country_asr <- country_asr[country_asr$location != "Antigua and Barbuda", ]
  country_asr <- rbind(country_asr, antigua_row, barbuda_row)
  
  original_row <- country_asr[country_asr$location == "Saint Kitts and Nevis", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Kitts"
  Nevis_row <- original_row; Nevis_row$location <- "Nevis"
  country_asr <- country_asr[country_asr$location != "Saint Kitts and Nevis", ]
  country_asr <- rbind(country_asr, Saint_row, Nevis_row)
  
  original_row <- country_asr[country_asr$location == "Saint Vincent and the Grenadines", ]
  Saint_row <- original_row; Saint_row$location <- "Saint Vincent"
  Grenadines_row <- original_row; Grenadines_row$location <- "Grenadines"
  country_asr <- country_asr[country_asr$location != "Saint Vincent and the Grenadines", ]
  country_asr <- rbind(country_asr, Saint_row, Grenadines_row)
  
  total <- full_join(worldData, country_asr, by = c('region' = 'location'))
  total <- subset(total, region != "Antarctica")
  
  # Categorize using supplied breaks and labels
  total <- total %>%
    mutate(
      val2 = cut(val, breaks = data_break, labels = data_labels, include.lowest = FALSE, right = TRUE)
    )
  
  return(total)
}

mapworld3 <- function(total, map_title) {
  p <- ggplot()
  p2 <- p +
    geom_polygon(
      data = total,
      aes(x = long, y = lat, group = group, fill = val2),
      colour = "black", size = .2
    ) +
    scale_fill_brewer(palette = "Set3", direction = -1) +
    theme(aspect.ratio = 1 / 3) + xlim(-200, 200) + ylim(-150, 100) +
    theme_void() +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = map_title, ncol = 1, byrow = TRUE,
                               keywidth = 0.32, keyheight = 0.08, default.unit = "inch")) +
    theme(
      legend.position = c(0.1, 0.4), legend.justification = c(0, 0),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 14)
    )
  return(p2)
}

# -----------------------------------------------------------------------------
# Identify top disease (by burden) per country and produce categorical maps
# -----------------------------------------------------------------------------
cause_top <- function(data) {
  data_n <- data %>%
    merge(country_n, by = 'country') %>%
    select(-country) %>%
    rename(country = location_name) %>%
    select('cause_name', 'country', 'burden') %>%
    arrange(country, cause_name) %>%
    group_by(country) %>%
    mutate(rank = rank(desc(burden), ties.method = 'first')) %>%
    subset(rank == 1) %>%
    group_by(cause_name) %>%
    mutate(grp = cur_group_id())
  
  cause_type <- data_n %>% distinct(cause_name, grp, .keep_all = TRUE)
  print(cause_type)
  data_n2 <- data_n %>% ungroup() %>% select('country', 'grp')
  return(data_n2)
}

# Fixed color mapping for top-cause maps (example palette)
disease_colors <- brewer.pal(n = 5, name = "Set3")
names(disease_colors) <- c(
  'Diabetes mellitus type 2', 'Ischemic heart disease',
  'Lower respiratory infections', 'Stroke', 'Chronic obstructive pulmonary disease'
)

# Repeat for air pollution causes and REI-specific disease types
air_allcause <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_allcause.csv')
air_apm <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_apm.csv')
air_hap <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_hap.csv')
air_aop <- read.csv('/dssg/home/economic2/outcome/rural outcome/air_aop.csv')

air_allcause_type <- cause_top(air_allcause)
air_allcause_map <- mapmain3(air_allcause_type, c(0, 1.5, 2.5, 3.5, 4.5),
                             c('Diabetes mellitus type 2', 'Ischemic heart disease', 'Lower respiratory infections', 'Stroke'))
air_allcause_map2 <- mapworld3(air_allcause_map, 'Disease associated with air pollution')
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_allcause_type.pdf',
       air_allcause_map2, width = 18, height = 10)

air_apm_type <- cause_top(air_apm)
air_apm_map <- mapmain3(air_apm_type, c(0, 1.5, 2.5, 3.5),
                        c('Diabetes mellitus type 2', 'Ischemic heart disease', 'Lower respiratory infections'))
air_apm_map2 <- mapworld3(air_apm_map, 'Disease associated with ambient particulate matter pollution')
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_apm_type.pdf',
       air_apm_map2, width = 18, height = 10)

air_hap_type <- cause_top(air_hap)
air_hap_map <- mapmain3(air_hap_type, c(0, 1.5, 2.5, 3.5, 4.5),
                        c('Diabetes mellitus type 2', 'Ischemic heart disease', 'Lower respiratory infections', 'Stroke'))
air_hap_map2 <- mapworld3(air_hap_map, 'Disease associated with household air pollution from solid fuels')
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_hap_type.pdf',
       air_hap_map2, width = 18, height = 10)

air_aop_type <- cause_top(air_aop)
air_aop_map <- mapmain3(air_aop_type, c(0, 1.5), c('Chronic obstructive pulmonary disease'))
air_aop_map2 <- mapworld3(air_aop_map, 'Disease associated with ambient ozone pollution')
ggsave(file = '/dssg/home/economic2/outcome/figure/fig_country/air_aop_type.pdf',
       air_aop_map2, width = 18, height = 10)