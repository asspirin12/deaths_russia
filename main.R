library(tidyverse)
library(bbplot2)
library(grDevices)

deaths <- readxl::read_excel("raw_data/Число зарегистрированных умерших (оперативные данные).xls")

deaths_lng <- deaths %>% gather(key = 'year', value = 'quant', -month)

deaths_lng$year <-
  factor(
    deaths_lng$year,
    levels = c(
      "2006",
      "2007",
      "2008",
      "2009",
      "2010",
      "2011",
      "2012",
      "2013",
      "2014",
      "2015",
      "2016",
      "2017",
      "2018",
      "2019",
      "2020",
      "2021"
    )
  )

deaths_lng$month <-
  factor(
    deaths_lng$month,
    levels = c(
      "Янв",
      "Фев",
      "Мар",
      "Апр",
      "Май",
      "Июн",
      "Июл" ,
      "Авг",
      "Сен",
      "Окт",
      "Ноя",
      "Дек"
    )
  )

deaths_lng$year_group <- ""

for (i in 1:length(deaths_lng$year)) {
  if (deaths_lng$year[i] == "2020") {
    deaths_lng$year_group[i] <- "2020"
  } else if (deaths_lng$year[i] == "2021") {
    deaths_lng$year_group[i] <- "2021"
  } else {
    deaths_lng$year_group[i] <- "2006 — 2019"
  }
}

#write_csv(deaths_lng, "clean_data/deaths_long.csv")

russia_plot <- 
  deaths_lng %>% 
  ggplot(aes(x = month, y = quant, fill = year_group, group = year)) +
  geom_bar(position = "dodge", stat = "identity") +
  bbc_style() +
  scale_fill_manual(
    values = c('#EFDADA', '#D46666', "#5B0101"), #CFE1E5
    breaks = c("2006 — 2019", "2020", 2021)
  ) +
  labs(
    title = "Пик смертности в России пришелся\nна декабрь",
    subtitle = "Тысяч умерших в месяц, по годам"
  ) +
  geom_hline(yintercept = 0, size = 1, color = "#333333") +
  theme(
    axis.ticks.x = element_line(color = "#333333"),
    axis.ticks.length = unit(0.26, "cm")
  ) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0, 50000, 100000, 150000, 200000, 250000),
                     labels = c(0, 50, 100, 150, 200, 250),
                     limits = c(0, 250000))

russia_plot 

finalise_plot(
  russia_plot,
  source_name = "Источник: Росстат",
  save_filepath = "russia_plot-nc.png",
  width_pixels = 640,
)

cairo_pdf("russia_plot.pdf", width = 670/72, height = 480/72)
print(russia_plot)
dev.off()
