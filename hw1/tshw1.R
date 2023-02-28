install.packages("fpp3")
install.packages("USgas")
library(fpp3)
library(tsibble)
library(tsibbledata)
library(USgas)
library(vctrs)
library(dplyr)
library(ggfortify)
library(ggplot2)
#2.10
#4b
my_data <- as_tibble(us_total,index=year,key=state)
#4c
New_England=list("Maine","Vermont","New Hampshire",
                 "Massachusetts","Connecticut","Rhode Island")
NEgas <- my_data |>
  filter(state %in% New_England) |>
  group_by(year,state)|>
  summarise(gas = sum(y))
NEgas |>
  ggplot(aes(x = year, y =gas, colour = state)) +
  geom_line()+
  ggtitle("Consumption of States in New England")
#8
set.seed(12)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
myseries <- as_tsibble(myseries)

autoplot(myseries,Turnover)+
  labs(y = "Turnover",
       title = "Australian retail data")

gg_season(myseries,Turnover) +
  labs(y = "Turnover",
       title = "Australian retail data")

myseries |>
  gg_subseries(Turnover) +
  labs(y = "Turnover",
       title = "Australian retail data")

myseries |>
  gg_lag(Turnover, geom = "point") +
  labs(x = "lag(Turnover, k)")

myseries |>
  ACF(Turnover) |>
  autoplot() + labs(title="Australian retail data")
#11
pigsla <- aus_livestock |>
  filter(Animal == "Pigs",
         State=="Victoria",
         year(Month) >= 1990,
         year(Month) <= 1995)

autoplot(pigsla,Count)+
  labs(y = "Count",
       title = "pig slaughters in Victoria between 1990 and 1995")
pigsla |>
  ACF(Count) |>
  autoplot() + labs(title="pig slaughters in Victoria between 1990 and 1995(acf)")

aus_livestock |>
  filter(Animal == "Pigs",
         State=="Victoria",
         year(Month)>= 1990)|>
  ACF(Count) |>
  autoplot() + labs(title="pig slaughters in Victoria")

#12
dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close))

autoplot(dgoog,diff)+
  labs(y = "diff",
       title = "daily changes in Google closing stock prices.")

dgoog|>
  ACF(diff) |>
  autoplot() + labs(title="daily changes in Google closing stock prices(acf)")
#3.7
#10a
autoplot(canadian_gas,Volume)+
  labs(y = "Volume",
       title = "Canadian gas production")

canadian_gas |>
  gg_subseries(Volume) +
  labs(y = "Volume",
       title = "Canadian gas production")

canadian_gas |>
  gg_season(Volume) +
  labs(y = "Volume",
       title = "Canadian gas production")
#10b
canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()
#10c
canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  gg_season(season_adjust)

#10d
library(stats)

# Generate a time series data with seasonal component
set.seed(123)
tsdata <- ts(rnorm(100, mean = 100, sd = 10), start = c(2010, 1), frequency = 12) + 
  cos(2 * pi * (1:100)/12) * 20

# Perform seasonal adjustment using stl
adj_tsdata <- stl(tsdata, s.window = "periodic")$time.series[, "seasonal"]

# Plot the original and adjusted time series
par(mfrow = c(2, 1))
plot(tsdata, main = "Original Time Series")
plot(adj_tsdata, main = "Seasonally Adjusted Time Series")