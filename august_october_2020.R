library(tidyverse)
library(showtext)
library(ggtext)
library(ggrepel)

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
                          "United States" = "USA"),
         change = october - august,
         country = fct_reorder(country, -change))

legend <- tibble(x = c(2.75, -3),
                 y = c(14, 14),
                 label = c("Increasing\nintention", "Decreasing\nintention"))

data %>%
  ggplot(aes(x=change, y=country)) +
  # geom_abline(slope=1, intercept = 0, color="#AAAAAA", size =0.25) + 
  geom_vline(xintercept = 0, size=0.25, color="#AAAAAA") +
  geom_hline(aes(yintercept = country), size=0.05, color="#AAAAAA") +
  geom_point() +
  coord_cartesian(xlim=c(-15, 5)) +
  labs(title = "COVID-19 vaccination intent is decreasing globally",
       subtitle = NULL,
       caption = "<i>Base: 18,526 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos",
       tag = NULL, 
       x = "Percentage point change in intention to receive<br>COVID-19 vaccine between August and October 2020",
       y= NULL,
       color = NULL) +
  theme(
    text = element_text(family = "montserrat"),
    plot.title = element_textbox_simple(family = "patua-one", size=28,
                                        lineheight = 1,
                                        margin = margin(b=10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0, color="gray",
                                    margin= margin(t=10)),
    plot.caption.position = "plot",
    axis.title.x = element_markdown(),
    axis.ticks = element_blank(),
    axis.line = element_line(),
    panel.background = element_rect(fill="#FFFFFF")
  ) +
  geom_label(data =  legend,
            mapping = aes(x = x, y=y, label=label),
            color = "#AAAAAA",
            label.padding = unit(2, "pt"),
            label.size = 0,
            hjust = c(1, 0),
            lineheight = 1,
            family = "montserrat",
            size = 2,
            inherit.aes = FALSE)

ggsave("august_october_2020.tiff", width=5, height=5)

