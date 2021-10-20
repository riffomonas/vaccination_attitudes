library(tidyverse)
# library(showtext)
library(ggtext)
library(plotly)
library(htmlwidgets)

# font_add_google(family="patua-one", "Patua One")
# font_add_google(family="montserrat", "Montserrat")

#showtext_auto()

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
  group_by(country) %>%
  summarize(x = seq(0, 1, 0.1),
            percent = percent[1] + x * diff(percent),
            .groups="drop")

data %>%
  highlight_key(., ~country) %>%
  ggplot(aes(x=x, y=percent, group=country, text = country)) +
  geom_line(color="#888888") +
  labs(title = "COVID-19 vaccination intent is<br>decreasing globally",
       subtitle = NULL,
       caption = "<i>Base: 18,526 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos",
       tag = NULL, 
       x = NULL,
       y= "Percent willing to receive vaccine",
       color = NULL) +
  scale_x_continuous(breaks = c(0, 1),
                   labels = c("August '20", "October '20"),
                   expand = c(0.1,0.1)) +
  scale_y_continuous(limits = c(50, 100),
                     breaks = seq(50, 100, by=10),
                     minor_breaks = NULL,
                     expand = c(0.03, 0.03)) +
  theme(
    text = element_text(family = "Montserrat", size=20, color="#888888"),
    plot.title = element_textbox_simple(family = "Patua One", size=35,
                                        lineheight = 1,
                                        margin = margin(b=10)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0, color="gray",
                                    margin= margin(t=10)),
    plot.caption.position = "plot",
    panel.background = element_rect(fill="#FFFFFF"),
    axis.ticks = element_blank(),
    axis.text  = element_text(color="#888888")
  )

s <- attrs_selected(
  line = list(color="#0000FF",
              width=5)
)


ggplotly(tooltip = "text", width=700, height=800) %>%
  config(displayModeBar = FALSE) %>%
  highlight(on="plotly_hover",
            opacityDim = 1,
            selected = s) %>%
  layout(title = list(
      x=0,
      xanchor="left",
      xref="container",
      y=1,
      yanchor="top",
      pad = list(t=50, l=10)
    ),
    hoverlabel = list(bgcolor="#0000FF",
                      font=list(color="#FFFFFF",
                                size=20,
                                family="Montserrat")
    ),
    margin = list(t=150, b=150),
    annotations = list(x = 0,
                       y = -0.2,
                       text = "<i>Base: 18,526 online adults aged 16-74 across 15 countries. Each line represents a<br>different country</i><br>Source: Ipsos",
                       showarrow = F,
                       xref='paper',
                       yref='paper',
                       xanchor='left',
                       align='left',
                       xshift=-60, yshift=0,
                       font=list(size=15, fontfacet="italic")
    )
  ) %>%
  saveWidget("august_october_2020_slope.html")
