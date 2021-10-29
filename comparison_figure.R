library(tidyverse)

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date,
         all_vax = people_vaccinated_per_hundred,
         fully_vax = people_fully_vaccinated_per_hundred) %>%
  filter((location == "Australia" | location == "Brazil" |
            location == "Canada" | location == "China" |
            location == "France" | location == "Germany" |
            location == "India" | location == "Italy" |
            location == "Japan" | location == "Mexico" |
            location == "South Africa" | location == "South Korea" |
            location == "Spain" | location == "United Kingdom" |
            location == "United States")) %>% 
  drop_na(fully_vax) %>%
  group_by(location) %>%
  slice_max(date, n=1) %>%
  ungroup() %>%
  select(-date)
