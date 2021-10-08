library(tidyverse)
library(showtext)
library(ggtext)

font_add_google(family="patua-one", "Patua One")
font_add_google(family="montserrat", "Montserrat")

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
  pivot_longer(c(august, october), names_to="month", values_to="percent")

data %>%
  ggplot(aes(x=month, y=percent, group=country, color=country)) +
  geom_line() +
  labs(title = "COVID-19 vaccination intent is decreasing globally",
       subtitle = NULL,
       caption = "<i>Base: 18,526 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos",
       tag = NULL, 
       x = NULL,
       y= "Percent willing to receive vaccine",
       color = NULL) +
  scale_x_discrete(breaks = c("august", "october"),
                   labels = c("August '20", "October '20"),
                   expand = c(0.1,0.1)) +
  scale_y_continuous(limits = c(50, 100),
                     breaks = seq(50, 100, by=10),
                     minor_breaks = NULL,
                     expand = c(0.03, 0.03)) +
  # ylim(60, 90) +
  # coord_cartesian(ylim = c(60, 90)) +
  theme(
    text = element_text(family = "montserrat"),
    plot.title = element_textbox_simple(family = "patua-one", size=28,
                                        lineheight = 1,
                                        margin = margin(b=10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0, color="gray",
                                    margin= margin(t=10)),
    plot.caption.position = "plot"
  )

ggsave("august_october_2020_slope.tiff", width=5, height=5)
