library(tidyverse)
library(showtext)

font_add_google(family="patua-one", "Patua One")
font_add_google(family="roboto", "Roboto")

showtext_auto()

data <- read_csv("august_october_2020.csv") %>%
  rename(country = X.1,
         august = "Total Agree - August 2020",
         october = "Total Agree - October 2020") %>%
	filter(country != "Total") %>%
  mutate(country = recode(country,
                          "South Korea" = "S. Korea",
                          "South Africa" = "S. Africa",
                          "United Kingdom" = "UK",
                          "United States" = "USA")) %>%
  pivot_longer(cols =c(august, october), names_to="month", values_to="percent")

data %>%
  ggplot(aes(x=month, y=percent, group=country, color=country)) +
  geom_line()

ggsave("august_october_2020_slope.tiff", width=5, height=5)
