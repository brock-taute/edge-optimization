library(CVXR)
library(tidyverse)
library(lubridate)

source('optimization_functions.R')
source('simulation_functions.R')

# 2018 electricity pricing and solar production data (hourly)
data <- read_csv('sample-data/data.csv')

# Example 1. Remote Net Metering Solar + Storage ----
# Simulate a remote net metering solar + storage project with the NY Value Stack
# tariff for the whole 2018 calendar year.

# Define the system

charge_efficiency <- .95
discharge_efficiency <- .95
storage_power <- 5000 # kW
storage_energy <- 15000 / discharge_efficiency # Nominal kWh
soc_start <- storage_energy
poi_limit <- 5000

# Set up and run simulation

# Defaults of function are for a NYSERDA, 1-Year simulation where
# Day Ahead pricing is released daily at 11AM
fs <- get_forecast_sequences()

rs <- simulate_remote_net_meter(fs$forecast_sequence, fs$forecast_lengths,
                                data$price, data$solar, storage_power,
                                storage_energy, poi_limit, charge_efficiency,
                                discharge_efficiency, soc_start)

df <- mutate(data,
             storage = rs$storage,
             soc = rs$soc,
             dispatch = solar - storage,
             value = price * dispatch)

# Analyze Results

cat(paste0('Predicted annual return:  $', round(sum(df$value), 0)), '\n',
    'Number of Storage Cycles per Year: ',
    round(-sum(df$storage[df$storage < 0]) / storage_energy, 1))

# Apply a scaling factor to second y-axis to see pricing
price_scaling_factor <- storage_energy * .5

# Look at average hourly dispatch each month
df %>%
  mutate(price = price * price_scaling_factor,
         month = month(datetime),
         hour = hour(datetime)) %>%
  group_by(month, hour) %>%
  summarise_all(mean) %>%
  gather('key', 'value', price, solar, storage, soc, dispatch) %>%
  ggplot(aes(x = hour, y = value, color = key)) +
  geom_path() +
  facet_wrap(~month) +
  scale_y_continuous(sec.axis = sec_axis(~./price_scaling_factor,
                                         name = 'Electricity Price ($/kWh)')) +
  scale_color_brewer(palette = 'Dark2') +
  scale_linetype_manual(values = c('solid', 'dotted', rep('solid', 3))) +
  labs(x = 'Datetime', y = 'kW', color = 'Key', linetype = 'Key',
       title = 'Average Hourly Dispatch Each Month') +
  theme_bw()

# Zoom in on time series performance
df %>%
  mutate(price = price * price_scaling_factor) %>%
  filter(datetime > ymd('2018/08/15'), datetime < ymd('2018/08/18')) %>%
  gather('key', 'value', solar, storage, soc, dispatch, price) %>%
  ggplot(aes(x = datetime, y = value, color = key)) +
  geom_path() +
  scale_y_continuous(sec.axis = sec_axis(~./price_scaling_factor,
                                         name = 'Electricity Price ($/kWh)')) +
  scale_color_brewer(palette = 'Dark2') +
  scale_linetype_manual(values = c('solid', 'dotted', rep('solid', 3))) +
  labs(x = 'Datetime', y = 'kW', color = 'Key', linetype = 'Key',
       title = 'Time Series Dispatch Mid-August') +
  theme_bw()


# Example 2 Stand Alone Storage System ----
# Find the optimal dispatch of a standalone storage system with full visibility
# into a single month's data.  The results of this can be used as an input to
# a forecasted dispatch model (for example setting the baseline for a demand
# threshold limit that gets updated throughout the month).

# Add in ConEd utility tariff to data since storage will consume grid energy.

summer_months <- c(6, 7, 8, 9)
weekend <- c('Saturday', 'Sunday')
workweek <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

full_data <- data %>%
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime),
         weekday = weekdays(datetime),
         # Constant monthly peak usage rate
         peakcost_monthly1 = 7.57,
         # Different Daily Peak rates based on day of week and season
         peakcost_daily1 = if_else(
           # If a weekday, 8am-10pm
           (hour >= 8) & (hour <= 22) & (weekday %in% workweek),
           if_else(month %in% summer_months, # And if in summer
                   1.0241, # then summer peak price
                   .7847, # else winter peak price
           ),
           # Else zero when not between 8am and 10pm (any time of year)
           0
         ),
         peakcost_daily2 = if_else(
           # If a summer weekday, 8am-6pm
           (month %in% summer_months) & (hour >= 8) & (hour <= 18) &
             (weekday %in% workweek),
           0.5298,
           0))

tax_kWh <- .024066
fees_kWh <- .005578

# Now prepare the data to model the optimal dispatch of June

model_month <- 6

# Filter out the unused months, then pivot the table wider twice, once for
# the monthly peaks and once for the daily peaks.  Each time use the abbreviated
# month name in the new column title.

test_data <- full_data %>%
  filter(month == model_month) %>%
  mutate(month = month.abb[month]) %>%
  pivot_wider(names_from = c(month, day), values_from = c(peakcost_daily1,
                                                          peakcost_daily2),
              values_fill = list(peakcost_daily1 = 0, peakcost_daily2 = 0)) %>%
  mutate(month = month.abb[month(datetime)]) %>%
  pivot_wider(names_from = 'month', values_from = peakcost_monthly1,
              values_fill = list(peakcost_monthly1 = 0),
              names_prefix = 'peakcost_monthly1_')

june_dispatch <- optimize_dispatch(storage_power, storage_energy, poi_limit,
                                   charge_efficiency, discharge_efficiency,
                                   soc_start, fees_kWh, tax_kWh,
                                   charge_solar_only = FALSE,
                                   price_kWh = test_data$price,
                                   peakcost_monthly1 =
                                     as.matrix(select(test_data,
                                                      matches('monthly1_Jun'))),
                                   peakcost_monthly2 = NULL,
                                   peakcost_daily1 =
                                     as.matrix(select(test_data,
                                                      matches('daily1_Jun'))),
                                   peakcost_daily2 =
                                     as.matrix(select(test_data,
                                                      matches('daily2_Jun'))))

# Calculate SOC using DC Side of storage dispatch

dc_storage <- june_dispatch

dc_storage[dc_storage > 0] <- dc_storage[dc_storage > 0] *
  charge_efficiency

dc_storage[dc_storage < 0] <- dc_storage[dc_storage < 0] /
  discharge_efficiency

soc <- cumsum(dc_storage) + soc_start

# Visualize results by zooming in on specific time series period
full_data %>%
  filter(month == 6) %>%
  mutate(storage = june_dispatch[,1],
         soc = soc,
         price = price * price_scaling_factor,
         peakcost_daily1 = peakcost_daily1 * price_scaling_factor,
         peakcost_daily2 = peakcost_daily2 * price_scaling_factor) %>%
  gather('key', 'value',
         soc, storage, price, peakcost_daily1, peakcost_daily2) %>%
  filter(datetime > ymd('2018/06/23'), datetime < ymd('2018/06/28')) %>%
  ggplot(aes(x = datetime, y = value, color = key)) +
  geom_path() +
  scale_y_continuous(sec.axis = sec_axis(~./price_scaling_factor,
                                         name = 'Electricity Price ($/kWh)')) +
  scale_color_brewer(palette = 'Dark2')+
  scale_linetype_manual(values = c('solid', 'dotted', rep('solid', 3))) +
  labs(x = 'Datetime', y = 'kW', color = 'Key', linetype = 'Key') +
  theme_bw()

cat(paste0('Number of Storage Cycles in June: ',
           round(-sum(june_dispatch[june_dispatch < 0]) / storage_energy, 1)))

