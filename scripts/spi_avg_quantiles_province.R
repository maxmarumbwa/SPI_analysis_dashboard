
library(dplyr)
library(explore)
library(readr)

p_mean <- read.csv("SPI_forecast_results_all/Timeseries.FBI.Payout.mean.csv")
province_district <- read.csv("SPI_forecast_results_all/province district.csv")%>%
  rename(Province =1)

forecast_mean=p_mean%>%
  left_join(y=province_district)%>%
  select(Year,Quantile,Province,Area,mean_observed_SPI6,mean_mean_Forecasted_SPI6_belowQ)
head(forecast_mean)


aa=aggregate(mean_observed_SPI6 ~ Province, forecast_mean, median)


### Summary stats Mean SPI per province for each yes
library("dplyr")
aa= forecast_mean %>%
  filter(Year == 2019) %>%
  group_by(Province,Quantile ) %>%
  summarise(mean_observed_SPI6 = median(mean_observed_SPI6, na.rm = TRUE),mean_mean_Forecasted_SPI6_belowQ = median(mean_mean_Forecasted_SPI6_belowQ, na.rm = TRUE))

######### SPEI for each Plot Quantile for each district
library("ggplot2")
ggplot(subset(aa, Province != "Manica"),
       aes(x = Quantile, y = mean_mean_Forecasted_SPI6_belowQ, color = Province)) +
  geom_line(lwd = 1, show.legend = FALSE) + facet_wrap(~ Province) +
  scale_color_manual(values = country_colors) +
  theme_bw() + theme(strip.text = element_text(size = rel(1.1)))


