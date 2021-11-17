library(tidyverse)
library(patchwork)
library(ggtext)
library(showtext)

font_add_google(family="patua-one", "Patua One")
font_add_google(family="montserrat", "Montserrat")

showtext_auto()

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
  
ipsos_owid <- inner_join(owid, ipsos, by="country") %>%
  mutate(diff = fully_vax - percent_october,
         country = fct_reorder(country, -diff),
         country = recode(country,
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                          "South Korea" = "S. Korea",
                          "South Africa" = "S. Africa")) 
  

# dotplot
dot <- ipsos_owid %>%
  ggplot(aes(x=diff, y=country)) +
  geom_hline(aes(yintercept=country), color="gray", size=0.25) +
  geom_point() +
  geom_vline(xintercept=0)  +
  scale_x_continuous(limits=c(NA, 20), breaks = seq(-100, 20, 20)) +
  labs(x="Difference between actual\nand intended vaccination rate",
       y=NULL) +
  theme_classic() +
  theme(
    text = element_text(family="montserrat"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=10)
    )


#dumbbell chart / barbell chart
dumbbell <- ipsos_owid %>%
  select(country, actual=fully_vax, intended =percent_october) %>%
  pivot_longer(cols=c(actual, intended), names_to="status", values_to="percent") %>%
  mutate(status = factor(status, levels=c("intended", "actual"))) %>%
  ggplot(aes(x=percent, y=country, group=country, color=status)) +
  geom_hline(aes(yintercept=country), color="gray", size=0.25) +
  geom_path(arrow=arrow(ends="first", angle=20, length=unit(0.1, "inches"), type="closed"),
            color="black") +
  labs(x="2020 intended and 2021\nactual vaccination rates",
       y=NULL) +
  theme_classic() +
  theme(
    text = element_text(family="montserrat"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_text(size=10)
  
  )

dot + dumbbell +
  plot_annotation(
    title="Countries are not meeting their people's stated desire to receive the COVID-19 vaccine",
    theme=theme(
      plot.title=element_textbox_simple(size=20, face="bold", family="patua-one")
    )
    ) +
  plot_layout(widths = c(2, 3))

ggsave("comparison_figure.tiff", width=6, height=4)
