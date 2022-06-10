library(dplyr)
library(readr)

p_mean <- read.csv("data/Timeseries.FBI.Payout.mean.csv")
province_district <- read.csv("data/province district.csv")%>%
  rename(Province =1)

forecast_mean=p_mean%>%
  left_join(y=province_district)%>%
  select(Year,Quantile,Province,Area,mean_observed_SPI6,mean_mean_Forecasted_SPI6_belowQ)
head(forecast_mean)

aggregate(mean_observed_SPI6 ~ Province, forecast_mean, mean)


### Summary stats Mean SPI per province for each year
library("dplyr")
forecast_2020= forecast_mean %>%
  filter(Year == 2021) %>%
  group_by(Province,Quantile ) %>%
  summarise(mean_observed_SPI6 = median(mean_observed_SPI6, na.rm = TRUE),mean_mean_Forecasted_SPI6_belowQ = median(mean_mean_Forecasted_SPI6_belowQ, na.rm = TRUE))

######### SPEI for each Plot Quantile for each district
#  Exclude one area
# library("ggplot2")
# ggplot(subset(aa, Province != "Maputo"),
#        aes(x = Quantile, y = mean_mean_Forecasted_SPI6_belowQ, color = Province)) +
#   geom_line(lwd = 1, show.legend = FALSE) + facet_wrap(~ Province) +
#   theme_bw() + theme(strip.text = element_text(size = rel(1.1)))

library("ggplot2")
ggplot(subset(forecast_2020, Province != "NA"),
       aes(x = Quantile, y = mean_mean_Forecasted_SPI6_belowQ, color = Province)) +
  geom_line(lwd = 1, show.legend = FALSE) + facet_wrap(~ Province) +
  theme_bw() + theme(strip.text = element_text(size = rel(1.1)))#+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))

forecast_2020_plot <- ggplot(data = forecast_2020, mapping = aes(x = Quantile, y = mean_mean_Forecasted_SPI6_belowQ, color=Province)) +
  geom_line() + facet_wrap( ~ Province) +
  labs(
    x = "Quantile",              # x axis title
    y = "SPI",   # y axis title
    title = "SPI for province and quantile",      # main title of figure
    color = "Province"      # title of legend
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = "output/png/spei_quantile_Province.png", plot = forecast_2020_plot, width = 12, height = 10, dpi = 300, units = "cm")

