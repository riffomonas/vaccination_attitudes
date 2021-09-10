library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

font_add_google(family="patua-one", "Patua One")
font_add_google(family="roboto", "Roboto")

showtext_auto()

data <- read_csv("august_october_2020.csv") %>%
  rename(country = X.1,
         percent_august = "Total Agree - August 2020",
         percent_october = "Total Agree - October 2020") %>%
  mutate(bump_august = case_when(percent_august <= percent_october ~
                                   percent_august - 2,
                                 percent_august > percent_october ~
                                   percent_august + 2),
         bump_october = case_when(percent_august <= percent_october ~
                                    percent_october + 2,
                                  percent_august > percent_october ~
                                    percent_october - 2),
         y_position = rev(1:nrow(.))) %>%
  mutate(country = recode(country,
                          "South Korea" = "S. Korea",
                          "South Africa" = "S. Africa",
                          "United Kingdom" = "UK",
                          "United States" = "USA")) %>%
  filter(country != "Total")

strip_data <- data %>%
  select(country, y_position) %>%
  mutate(xmin = 50, xmax=100,
         ymin = y_position - 0.5,
         ymax = y_position + 0.5,
         fill = c(rep(c("a", "b"), length.out=nrow(.)))) %>%
  pivot_longer(cols=c(xmin, xmax), values_to="x", names_to="xmin_xmax") %>%
  select(-xmin_xmax)
 
data %>%
  pivot_longer(cols = -c(country, y_position),
               names_to=c(".value", "month"),
               names_sep = "_") %>%
  drop_na() %>%
  ggplot(aes(x=percent, y=y_position, color=month, group=y_position)) +
  geom_ribbon(data = strip_data,
              aes(x = x, ymin=ymin, ymax = ymax, group=y_position, fill=fill),
              inherit.aes = FALSE,
              show.legend=FALSE) +
  geom_line(color="#888888", size=1, show.legend = FALSE) +
  geom_point(size=3, show.legend = TRUE) +
  geom_text(aes(label=glue("{percent}%"), x=bump), 
            size=2,
            color="#686868", 
            family="roboto",
            show.legend = FALSE) +
  scale_color_manual(name="If a vaccine for COVID-19 were\navailable, I agree I would get it...",
                     breaks=c("august", "october"),
                     values=c("#ABB3FF", "#263AFF"),
                     labels=c("August '20", "October '20"),
                     guide = guide_legend(override.aes = list(size=4))) +
  scale_fill_manual(name=NULL, 
                    breaks=c("a", "b"),
                    values=c("#F8F8F8", "#FFFFFF"),
                    labels=c("a", "b")) +
  scale_x_continuous(limits=c(50, 100),
                     breaks=NULL,
                     labels=NULL,
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = data$y_position,
                     labels = data$country,
                     expand = c(0, 0),
                     limits=c(0.5, 16.5)) +
  labs(x=NULL, y=NULL,
       title="COVID-19 vaccination intent\nis decreasing globally",
       caption="<i>Base: 18,526 online adults aged 16-74 across 15 countries</i><br>Source: Ipsos")+
  theme(
    text = element_text(family = "roboto"),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", size=28,
                              color="#000000", family = "patua-one",
                              margin = margin(t=10, b=20)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill="#FFFFFF"),
    plot.margin = margin(l=5, r=15),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="#686868", size=6),
    axis.text.y = element_text(face="bold"),
    legend.position = c(0, 1.0),
    legend.margin = margin(l=0),
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.background = element_blank(),
    legend.title = element_text(size=9, lineheight = 1.3, margin = margin(r=35)),
    legend.key = element_blank(),
    legend.key.width = unit(3, "pt"),
    legend.text = element_markdown(margin = margin(r=10))
    ) +
  guides(fill="none") +
  coord_cartesian(clip="off")
  

ggsave("august_october_2020_makeover.tiff", width=5, height=5)

