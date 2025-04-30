# Prerequisites (Packages & Libraries)

library(tidyverse)
library(ggrepel)
library(nycflights13)
library(ggthemes)
library(ggplot2)
library(rlang)
library(forcats)
library(gt)
library(png)
library(grid)
library(patchwork)
library(lahman)
library(janitor)
## 18.3.4 Exercises

#1. Can you find any relationship between the carrier and the rows that appear to be missing from planes?

# Yes, as we can see in the table, the airline carriers MQ and AA have most of their aircrafts' tail numbers missing from the planes data-set, apart from few other carriers that have a small percentage of their data missing.

car_vec = flights |>
  distinct(tailnum, carrier) |>
  anti_join(planes) |>
  distinct(carrier) |>
  as_vector() |>
  unname()

total_tails = flights |>
  filter(carrier %in% car_vec) |>
  group_by(carrier) |>
  summarize(
    total_aircrafts = n_distinct(tailnum)
  )

flights |>
  distinct(tailnum, carrier) |>
  anti_join(planes) |>
  count(carrier, name = "missing_tailnums") |>
  full_join(total_tails) |>
  mutate(percentage_missing = missing_tailnums/total_aircrafts) |>
  arrange(desc(percentage_missing))

## 19.2.4 Exercises

#1. We forgot to draw the relationship between weather and airports in Figure 19.1. What is the relationship and how should it appear in the diagram?

# - The primary key will be airports$faa .

# - It corresponds to a compound secondary key, weather$origin and weather$time_hour.

#2. weather only contains information for the three origin airports in NYC. If it contained weather records for all airports in the USA, what additional connection would it make to flights?

# If weather contained the weather records for all airports in the USA, it would have made an additional connection to the variable dest in the flights dataset.

#3. The year, month, day, hour, and origin variables almost form a compound key for weather, but there’s one hour that has duplicate observations. Can you figure out what’s special about that hour?

# As we can see in the table below , on November 3, 2013 at 1 am, we have a duplicate weather record. This means that the combination of year, month, day, hour, and origin variables does not form a compound key for weather , since some observations are not unique.

# This happens because the daylight savings time clock changed on November 3, 2013 in New York City as follows: --

# - Start of DST in 2013: Sunday, March 10, 2013 -- 1 hour forward - 1 hour is skipped.

# - End of DST in 2013: Sunday, November 3, 2013 -- 1 hour backward at 1 am.

weather |>
  group_by(year, month, day, hour, origin) |>
  count() |>
  filter(n > 1) |>
  ungroup()

#4. We know that some days of the year are special and fewer people than usual fly on them (e.g., Christmas eve and Christmas day). How might you represent that data as a data frame? What would be the primary key? How would it connect to the existing data frames?

# We can create a data frame or a tibble, as shown in the code below, named holidays to represent holidays and the pre-holiday days.

# The primary key would be a compound key of year , month and day. It would connect to the existing data frames using a secondary compound key of of year , month and day.

holidays <- tibble(
  year = 2013,
  month = c(1, 2, 5, 7, 9, 10, 11, 12),
  day = c(1, 14, 27, 4, 2, 31, 28, 25),
  holiday_name = c(
    "New Year's Day",
    "Valentine's Day",
    "Memorial Day",
    "Independence Day",
    "Labor Day",
    "Halloween",
    "Thanksgiving",
    "Christmas Day"
  ),
  holiday_type = "Holiday"
)

# Computing the pre-holiday date and adding it to holidays
holidays <- bind_rows(
  # Exisitng tibble of holidays
  holidays,
  # A new tibble of holiday eves
  holidays |>
    mutate(
      day = day-1,
      holiday_name = str_c(holiday_name, " Eve"),
      holiday_type = "Pre-Holiday"
    ) |>
    slice(2:8)
) |>
  mutate(flight_date = make_date(year, month, day))

holidays

# Now, we can use this new tibble, join it with our existing data sets and try to figure out whether there is any difference in number of flights on holidays, and pre-holidays, vs. the rest of the days. The results are in table below.

# A tibble on the number of flights each day, along with whether each day 
# is holiday or not; and if yes, which holiday
nos_flights <- flights |>
  mutate(flight_date = make_date(year, month, day)) |>
  left_join(holidays) |>
  group_by(flight_date, holiday_type, holiday_name) |>
  count()

nos_flights |>
  group_by(holiday_type) |>
  summarize(avg_flights = mean(n)) |>
  mutate(holiday_type = if_else(is.na(holiday_type),
                                "Other Days",
                                holiday_type)) |>
  ggplot(aes(x = avg_flights,
             y = reorder(holiday_type, avg_flights))) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(y = NULL, x = "Average Number of flights (per day)",
       title = "Holidays / pre-holiday have lower number of flights, on average") +
  theme(plot.title.position = "plot")

# The number of flights on various holidays and pre-holiday days is shown below.

nos_flights |>
  group_by(holiday_name) |>
  summarize(avg_flights = mean(n)) |>
  mutate(holiday_name = if_else(is.na(holiday_name),
                                "Other Days",
                                holiday_name)) |>
  mutate(col_var = holiday_name == "Other Days") |>
  ggplot(aes(x = avg_flights,
             y = reorder(holiday_name, avg_flights),
             fill = col_var,
             label = round(avg_flights, 0))) +
  geom_bar(stat = "identity") +
  geom_text(nudge_x = 20, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        legend.position = "none") +
  labs(y = NULL, x = "Number of flights (per day)") +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(xlim = c(500, 1050))

#5. Draw a diagram illustrating the connections between the Batting, People, and Salaries data frames in the Lahman package. Draw another diagram that shows the relationship between People, Managers, AwardsManagers. How would you characterize the relationship between the Batting, Pitching, and Fielding data frames?

# The data-frames are shown below, alongwith the check that playerID is a key: --

# In Batting , the variables playerID , yearID and stint form a compound key.

library(Lahman)
Batting |> as_tibble() |>
  group_by(playerID, yearID, stint) |>
  count() |>
  filter(n > 1)
head(Batting)

# In People, the variable playerID is unique for each observation, and hence a primary key.

People |> 
  as_tibble() |>
  group_by(playerID) |>
  count() |>
  filter(n > 1)

head(People)

# In Salaries the variables playerID , yearID and stint form a compound key.

Salaries |> 
  as_tibble() |>
  group_by(playerID, yearID, teamID) |>
  count() |>
  filter(n > 1)

head(Salaries)

# Now, we show another diagram that shows the relationship between People, Managers, AwardsManagers.

# For Managers, the key is a compound key of playerID, yearID and inseason

head(Managers)
Managers |>
  as_tibble() |>
  group_by(playerID, yearID, inseason) |>
  count() |>
  filter(n > 1)

head(Managers)

# For AwardsManagers , the primary key is a compound key of playerID , awardID and yearID .

head(AwardsManagers)

AwardsManagers |>
  as_tibble() |>
  group_by(playerID, awardID, yearID) |>
  count() |>
  filter(n > 1)

head(AwardsManagers)

# Now, let's try to characterize the relationship between Batting , Pitching and Fielding.

Pitching |> as_tibble() |>
  group_by(playerID, yearID, stint) |>
  count() |>
  filter(n > 1)

head(Pitching)

# In the Fielding dataset, the primary key is a compound key comprised of playerID , yearID , stint and POS.

Fielding |> as_tibble() |>
  group_by(playerID, yearID, stint, POS) |>
  count() |>
  filter(n > 1)

head(Fielding)

## 19.3.4 Exercises

#1. Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

# First, we find out the 48 hours (over the course of the whole year) that have the worst delays. As we can see in below, these are quite similar across the 3 origin airports, for which we have the weather data.

# Create a dataframe of 48 hours with highestaverage delays 

# (for each of the 3 origin airports)
delayhours = flights |>
  group_by(origin, time_hour) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_delay), .by_group = TRUE) |>
  slice_head(n = 48) |>
  arrange(time_hour)

delayhours |>
  ggplot(aes(y = time_hour, x = avg_delay)) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~origin, dir = "h") +
  theme_minimal() +
  labs(x = "Average delay during the hour (in mins.)", y = NULL,
       title = "The worst 48 hours for departure delays are similar across 3 airports")

# The figure below depicts that across the three airports, the 48 hours with worst delays consistently have much higher rainfall (precipitation in inches) and poorer visibility (lower visibility in miles and higher dew-point in degrees F).

var_labels = c("Temperature (F)", "Dewpoint (F)", 
               "Relative Humidity %", "Precipitation (inches)", 
               "Visibility (miles)")
names(var_labels) = c("temp", "dewp", "humid", "precip", "visib")

g1 = weather |>
  filter(origin == "EWR") |>
  left_join(delayhours) |>
  mutate(
    del_hrs = if_else(is.na(avg_delay),
                      "Other hours",
                      "Hours with max delays"),
    precip = precip * 25.4
  ) |>
  pivot_longer(
    cols = c(temp, dewp, humid, precip, visib),
    names_to = "variable",
    values_to = "values"
  ) |>
  group_by(origin, del_hrs, variable) |>
  summarise(means = mean(values, na.rm = TRUE)) |>
  ggplot(aes(x = del_hrs, y = means, fill = del_hrs)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ variable, scales = "free", ncol = 5,
              labeller = labeller(variable = var_labels)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom") +
  labs(subtitle = "Weather Patterns for Newark Airport (EWR)",
       fill = "")

g2 = weather |>
  filter(origin == "JFK") |>
  left_join(delayhours) |>
  mutate(
    del_hrs = if_else(is.na(avg_delay),
                      "Other hours",
                      "Hours with max delays"),
    precip = precip * 25.4
  ) |>
  pivot_longer(
    cols = c(temp, dewp, humid, precip, visib),
    names_to = "variable",
    values_to = "values"
  ) |>
  group_by(origin, del_hrs, variable) |>
  summarise(means = mean(values, na.rm = TRUE)) |>
  ggplot(aes(x = del_hrs, y = means, fill = del_hrs)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ variable, scales = "free", ncol = 5,
              labeller = labeller(variable = var_labels)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")  +
  labs(subtitle = "Weather Patterns for John F Kennedy Airport (JFK)",
       fill = "")

g3 = weather |>
  filter(origin == "LGA") |>
  left_join(delayhours) |>
  mutate(
    del_hrs = if_else(is.na(avg_delay),
                      "Other hours",
                      "Hours with max delays"),
    precip = precip * 25.4
  ) |>
  pivot_longer(
    cols = c(temp, dewp, humid, precip, visib),
    names_to = "variable",
    values_to = "values"
  ) |>
  group_by(origin, del_hrs, variable) |>
  summarise(means = mean(values, na.rm = TRUE)) |>
  ggplot(aes(x = del_hrs, y = means, fill = del_hrs)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ variable, scales = "free", ncol = 5,
              labeller = labeller(variable = var_labels)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")  +
  labs(subtitle = "Weather Patterns for La Guardia Airport (LGA)",
       fill = "") 

g1 / g2 / g3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

#2. Imagine you’ve found the top 10 most popular destinations using this code:

flights2 <- flights
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)

# How can you find all flights to those destinations?

flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
top_dest <- flights2 |>   
  count(dest, sort = TRUE) |>   
  head(10)
top_dest_vec <- top_dest |> select(dest) |> as_vector()
flights |>
  filter(dest %in% top_dest_vec) 

#3. Does every departing flight have corresponding weather data for that hour?

# No, as we can see from the code below, every departing flight DOES NOT have corresponding weather data for that hour. 1556 flights do not have associated weather data; and these correspond to 38 different hours during the year.

# Number of flights that do not have associated weather data
flights |>
  anti_join(weather) |>
  nrow()

# Number of distinct time_hours that do not have such data
flights |>
  anti_join(weather) |>
  distinct(time_hour)

# A check to confirm our results
flights |>
  select(year, month, day, origin, dest, time_hour) |>
  left_join(weather) |>
  summarise(
    missing_temp_or_windspeed = mean(is.na(temp) & is.na(wind_speed)),
    missing_dewp = mean(is.na(dewp))
  )
(as.numeric(flights |> anti_join(weather) |> nrow())) / nrow(flights)

#4. What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)

# The tail numbers that don't have a matching record in planes mostly belong the a select few airline carriers, i.e., AA and MQ . The variable carrier explains most of the problems in missing data, as shown in below.

flights2 <- flights |>
  mutate(id = row_number(), .before = 1)

ids_no_record = flights2 |>
  anti_join(planes, by = join_by(tailnum)) |>
  select(id) |>
  as_vector() |> unname()

flights2 = flights2 |>
  mutate(
    missing_record = id %in% ids_no_record
  )

label_vec = c("Flights with missing tailnum in planes", "Other flights")
names(label_vec) = c(FALSE, TRUE)

flights2 |>
  group_by(missing_record) |>
  count(carrier) |>
  mutate(col_var = carrier %in% c("MQ", "AA")) |>
  ggplot(aes(x = n, y = carrier, fill = col_var)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ missing_record, 
             scales = "free_x", 
             labeller = labeller(missing_record = label_vec)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Number of flights",  y = "Carrier",
       title = "Flights with missing tailnum in planes belong to a select few carriers") + 
  scale_fill_brewer(palette = "Set2")

#5. Add a column to planes that lists every carrier that has flown that plane. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned in previous chapters.

# Using the code below, we confirm that there are 17 such different airplanes (identified by tailnum) that have been flown by two carriers. These are shown below .

# Displaying tail numbers which have been used by more than one carriers
flights |>
  group_by(tailnum) |>
  summarise(number_of_carriers = n_distinct(carrier)) |>
  filter(number_of_carriers > 1) |>
  drop_na()

# The following code adds a column to planes that lists every carrier that has flown that plane.

# A tibble that lists all carriers a tailnum has flown
all_carrs = flights |>
  group_by(tailnum) |>
  distinct(carrier) |>
  summarise(carriers = paste0(carrier, collapse = ", ")) |>
  arrange(desc(str_length(carriers)))
# Display the tibble
slice_head(all_carrs, n= 30)

# Merge with planes
planes |>
  left_join(all_carrs)

#6. Add the latitude and the longitude of the origin and destination airport to flights. Is it easier to rename the columns before or after the join?

flights |>
  left_join(airports, by = join_by(dest == faa)) |>
  rename(
    "dest_lat" = lat,
    "dest_lon" = lon
  ) |>
  left_join(airports, by = join_by(origin == faa)) |>
  rename(
    "origin_lat" = lat,
    "origin_lon" = lon
  ) |>
  relocate(origin, origin_lat, origin_lon,
           dest, dest_lat, dest_lon,
           .before = 1)

#7. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:

airports |>
  semi_join(flights, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# You might want to use the size or color of the points to display the average delay for each airport.

# The following code and the resulting plot displays the result. I would like tt use size as an aesthetic, as it is easy to compare on a continuous scale, and leads to visually tough comparison.

# Create a dataframe of 1 row for origin airports
or_apts = airports |>
  filter(faa %in% c("EWR", "JFK", "LGA")) |>
  select(-c(alt, tz, dst, tzone)) |>
  rename(dest = faa) |>
  mutate(type = "New York City",
         avg_delay = 0)

# Start with the flights data-set
flights |>
  
  # Compute average delay for each location
  group_by(dest) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
  
  # Add the latitude and longitude data
  left_join(airports, join_by(dest == faa)) |>
  select(-c(alt, tz, dst, tzone)) |>
  mutate(type = "Destinations") |>
  
  # Add a row for origin airports data
  bind_rows(or_apts) |>
  
  # Plot the map and points
  ggplot(aes(x = lon, y = lat, 
             col = avg_delay, 
             shape = type,
             label = name)) +     
  borders("state", colour = "white", fill = "lightgrey") +     
  geom_point(size = 2) +     
  coord_quickmap(xlim = c(-130, -65),
                 ylim = c(23, 50)) +
  scale_color_viridis_c(option = "C") +
  labs(col = "Average Delay at Arrival (mins.)", shape = "") +
  
  # Themes and Customization
  theme_void() +
  theme(legend.position = "bottom")

#8. What happened on June 13 2013? Draw a map of the delays, and then use Google to cross-reference with the weather.

## 19.5.5 Exercises

#1. Can you explain what’s happening with the keys in this equi join? Why are they different?

# x |> full_join(y, join_by(key == key))

# x |> full_join(y, join_by(key == key), keep = TRUE)

# Yes, the key column names in the output are different because when we use the option keep = TRUE in the full_join() function, the execution by dplyr retains both the keys and names them as key.x and key.y for ease of recognition.

#2. When finding if any party period overlapped with another party period we used q < q in the join_by()? Why? What happens if you remove this inequality?

# The default syntax for function inner_join is inner_join(x, y, by = NULL, ...) . The default for by = argument is NULL, where the default *_join()⁠ will perform a natural join, using all variables in common across x and y.

# Thus, when we skip q < q , the inner_join finds that the variables q , start and end are common. The start and end variables are taken care of by the helper function overlaps() . But q remains. Since q is common in parties and parties all observations get matched. To prevent observations from matching on q we can keep a condition q < q , and thus each observation and match is repeated only once, leading to correct results.

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)

# Using the correct code in textbook
parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |>
  select(start.x, end.x, start.y, end.y)

# Removing the "q < q" in the join_by()
parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end))) |>
  select(start.x, end.x, start.y, end.y)