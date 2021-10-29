library(tidyverse)

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date,
         all_vax = people_vaccinated_per_hundred,
         fully_vax = people_fully_vaccinated_per_hundred)
