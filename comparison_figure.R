library(tidyverse)

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(country = location,
         date,
         all_vax = people_vaccinated_per_hundred,
         fully_vax = people_fully_vaccinated_per_hundred) %>%
  drop_na(fully_vax) %>%
  group_by(country) %>%
  slice_max(date, n=1) %>%
  ungroup() %>%
  select(-date)

ipsos <- read_csv("august_october_2020.csv") %>%
  rename(country = X.1,
         percent_august = "Total Agree - August 2020",
         percent_october = "Total Agree - October 2020")
  
ipsos_owid <- inner_join(owid, ipsos, by="country")
