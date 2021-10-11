library(tidyverse)
library(showtext)
library(ggtext)
library(glue)

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
  pivot_longer(c(august, october), names_to="month", values_to="percent") %>%
  mutate(highlight = (country == "USA"),
         country = fct_reorder(country, highlight))

highlight_data <- data %>%
  group_by(country) %>%
  summarize(diff = diff(percent),
            direction = if_else(diff < 0, "dropped", "increased"),
            diff = abs(diff)) %>%
  filter(country== "USA")
  
title <- glue("Between August and October of 2020, people's intention to receive the COVID-19 vaccine <span style='color: #0000FF'>{highlight_data$direction} by {highlight_data$diff}% in the {highlight_data$country}</span>")

data %>%
  ggplot(aes(x=month, y=percent, group=country, color=highlight,
             size = highlight)) +
  geom_line(lineend="round",
            show.legend=FALSE) +
  labs(title = title,
       subtitle = NULL,
       caption = "<i>Base: 18,526 online adults aged 16-74 across 15 countries. Each line represents a different country</i><br>Source: Ipsos",
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
  scale_color_manual(breaks = c(F, T),
                     values = c("#AAAAAA", "#0000FF")) +
  scale_size_manual(breaks = c(F, T),
                    values = c(0.5, 2)) +
  # ylim(60, 90) +
  # coord_cartesian(ylim = c(60, 90)) +
  theme(
    text = element_text(family = "montserrat", color = "#AAAAAA"),
    plot.title = element_textbox_simple(family = "patua-one", size=20,
                                        lineheight = 1,
                                        color = "#000000",
                                        margin = margin(b=10)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0,
                                    margin= margin(t=10)),
    plot.caption.position = "plot",
    axis.text = element_text(color="#AAAAAA"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid = element_blank()
  )

ggsave("august_october_2020_slope.tiff", width=4, height=5)
