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
                          "United States" = "USA"))

legend <- tibble(x = c(90, 100),
                 y = c(100, 95),
                 label = c("Increasing\nintention", "Decreasing\nintention"))

data %>%
  ggplot(aes(x=august, y=october, label=country)) +
  geom_abline(slope=1, intercept = 0, color="#AAAAAA", size =0.25) + 
  geom_point() +
  geom_label_repel(min.segment.length = 0,
                   max.overlaps = Inf,
                   label.size = 0,
                   label.padding = 0.1,
                   label.r = 0,
                   size=3.5,
                   family = "montserrat") +
  coord_fixed(xlim=c(50, 100), ylim=c(50, 100), clip="off") +
  labs(title = "COVID-19 vaccination intent is decreasing globally",
       subtitle = NULL,
       caption = "<i>Base: 18,526 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos",
       tag = NULL, 
       x = "Percent willing to receive<br>vaccine in August 2020",
       y= "Percent willing to receive<br>vaccine in October 2020",
       color = NULL) +
  theme(
    text = element_text(family = "montserrat"),
    plot.title = element_textbox_simple(family = "patua-one", size=25,
                                        lineheight = 1,
                                        margin = margin(b=20, t=10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0, color="gray",
                                    margin= margin(t=10)),
    plot.caption.position = "plot",
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    axis.ticks = element_blank(),
    axis.line = element_line(),
    panel.background = element_rect(fill="#FFFFFF")
  ) +
  geom_text(data =  legend,
            mapping = aes(x = x, y=y, label=label),
            color = "#AAAAAA",
            hjust = 0,
            lineheight = 1,
            family = "montserrat",
            size = 2,
            inherit.aes = FALSE)

ggsave("august_october_2020.tiff", width=5, height=5)

