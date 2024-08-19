rm(list = ls())
library(tidyverse)
library(here)
library(janitor)

wb_indicators <- read_csv(here("data", "wb_indicators.csv"), na = c("..", ""))
wb_metadata <- read.csv(here("data", "wb_indicators_metadata.csv"))

wb_indicators_long <- wb_indicators|>
  tidyr::pivot_longer(cols = `2001 [YR2001]`:`2020 [YR2020]`,
                      names_to = "year",
                      values_to = "indicator_value")

wb_data_clean <- wb_indicators_long |>
  tidyr::separate(col = year, into = c("year", "year_chr"), sep = " ") |>
  dplyr::select(-year_chr, -`Country Code`, -`Series Code`)
head(wb_data_clean)

wb_data_tidy <-  wb_data_clean |>
  tidyr::drop_na('Series Name') |>
  tidyr::pivot_wider(names_from = `Series Name`,
                     values_from = indicator_value)

names(wb_data_tidy) <- c("country",
                         "year",
                         "access_clean_fuels_pp",
                         "access_electricty_pp",
                         "co2_emissions_kit",
                         "fossil_fuel_con_pt",
                         "water_stress")
us_wb <- wb_data_tidy |>
  dplyr::filter(country == "United States")

nicaragua_co2 <- wb_data_tidy |>
  dplyr::filter(country == "Nicaragua") |>
  select(year, co2_emissions_kit)

wb_subset <- wb_data_tidy |>
  select(-c(water_stress, access_electricty_pp))

wb_newnames <- wb_data_tidy |>
  rename(elec = access_electricty_pp, co2 = co2_emissions_kit)

class(wb_data_tidy$year)
wb_data_tidy$year <- as.numeric(wb_data_tidy$year)

#reassigning as numeric
wb_data_tidy <- wb_data_tidy |>
  mutate(year = as.numeric(year))

 class(wb_data_tidy)

 wb_co2_tons <- wb_data_tidy |>
   mutate(co2_tons = co2_emissions_kit *1000)
 View(wb_co2_tons)

 #group by ----
co2_total <- wb_data_tidy |>
   group_by(country) |>
   summarize(total_co2_kt = sum(co2_emissions_kit, na.rm = TRUE))

us_ca_data <- wb_data_tidy |>
  filter(country %in% c("United States", "Canada"))

data_2020 <- wb_data_tidy |>
  filter(year == 2020)

co2_annual <- wb_data_tidy |>
  group_by(year) |>
  summarize(annual_total_co2_kt = sum(co2_emissions_kit, na.rm = TRUE))

ggplot(data = co2_annual, aes(x = year, y = annual_total_co2_kt, color = annual_total_co2_kt)) +
  geom_line(size = 2) +
  labs(title = "Global total of Annual CO2 by Year")
