library(tidyverse)

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(country = location,
         date,
         all_vax = people_vaccinated_per_hundred,
         fully_vax = people_fully_vaccinated_per_hundred) %>%
  filter(date < "2021-11-01") %>%
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

#scatter
library(ggrepel)

ipsos_owid %>%
  ggplot(aes(x=percent_october, y=fully_vax, label=country))+
  geom_point() +
  geom_abline() +
  geom_text_repel()

# dotplot
ipsos_owid %>%
  mutate(diff = fully_vax - percent_october,
         country = fct_reorder(country, -diff)) %>%
  ggplot(aes(x=diff, y=country)) +
  geom_point() +
  geom_vline(xintercept=0)

#slope plot
ipsos_owid %>%
  select(country, actual=fully_vax, intended =percent_october) %>%
  pivot_longer(cols=c(actual, intended), names_to="status", values_to="percent") %>%
  mutate(status = factor(status, levels=c("intended", "actual"))) %>%
  ggplot(aes(x=status, y=percent, group=country)) +
  geom_line()

#dumbbell chart / barbell chart
ipsos_owid %>%
  select(country, actual=fully_vax, intended =percent_october) %>%
  pivot_longer(cols=c(actual, intended), names_to="status", values_to="percent") %>%
  mutate(status = factor(status, levels=c("intended", "actual"))) %>%
  ggplot(aes(x=percent, y=country, group=country, color=status)) +
  geom_path(arrow=arrow(ends="first", angle=20, length=unit(0.1, "inches"), type="closed"),
            color="black")# +
  #geom_point()
#