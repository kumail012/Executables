lapply(c("tidyverse", "ggrepel", "ggthemes", "ggplot2", "rlang", "forcats", "gt", "naniar", "gtExtras")
       
       , library, character.only = TRUE)





library(tidyverse)

library(ggrepel)

library(nycflights13)

library(ggthemes)

library(ggplot2)

library(rlang)

library(forcats)

library(gt)



## 12.2.4 Exercises

#1. How does dplyr::near() work? Type near to see the source code. Is sqrt(2)^2 near 2?



dplyr::near

# The near() function compares two numbers with a small tolerance to handle floating-point inaccuracies.



sqrt(2)^2 == 2

near(sqrt(2)^2, 2)

# Yes, sqrt(2)^2 is near(2).



#2. Use mutate(), is.na(), and count() together to describe how the missing values in dep_time, sched_dep_time and dep_delay are connected.



#First, let us try to compute the number of rows where, as it should be, `sched_dep_time - dep_time == dep_delay`. As we see below, the results is `NA` since `NAs` are contagious in addition, the result returns `NA` .



flights |>
  
  select(dep_time, sched_dep_time, dep_delay) |>
  
  mutate(check1 = sched_dep_time - dep_time == dep_delay) |>
  
  summarise(
    
    n = n(),
    
    equal = sum(check1)
    
  )





# Now, let us rework the maths with using `is.na()` to remove missing values of departure time, i.e. cancelled flights. We can use `filter(!is.na(dep_time))` . The results indicate that 69.% of flights, the departure delay is equal to difference between departure time and scheduled departure time.



flights |>
  
  select(dep_time, sched_dep_time, dep_delay) |>
  
  filter(!is.na(dep_time)) |>
  
  mutate(check1 = dep_time - sched_dep_time == dep_delay) |>
  
  summarise(
    
    total = n(),
    
    equal = sum(check1)
    
  ) |>
  
  mutate(perc_equal = (equal*100)/total)



# Now, onto checking the relation between missing values. We observe that none of the Scheduled Departure Time values are missing. There are 8,255 missing Departure Time values, which indicate a cancelled flight. There are also 8,255 missing Departure Delay values, and we show below that these are the exact same flights for which the departure time is missing. Thus, the missing values in `dep_delay` and `dep_time` are connected and exaclty occurring for same rows.



# Number of row with missing Scheduled Departure Time

flights |>
  
  filter(is.na(sched_dep_time)) |>
  
  count() |>
  
  as.numeric()



# The number of rows with missing Departure Time

flights |>
  
  filter(is.na(dep_time)) |>
  
  count() |>
  
  as.numeric()



# The number of rows with missing Departure Delay

flights |>
  
  filter(is.na(dep_delay)) |>
  
  count() |>
  
  as.numeric()



# Checking whether the exact same rows have missing values

# for Departure Time and Departure Delay

sum(which(is.na(flights$dep_time)) != which(is.na(flights$dep_delay)))



## 12.4.4 Exercises

#1. What will sum(is.na(x)) tell you? How about mean(is.na(x))?

# The expression `sum(is.na(x))` tells us the number of missing values in the vector `x`. The expression `mean(is.na(x))` tells us the proportion of missing values in the vector `x` . 



#2. What does prod() return when applied to a logical vector? What logical summary function is it equivalent to? What does min() return when applied to a logical vector? What logical summary function is it equivalent to? Read the documentation and perform a few experiments.



# A logical vector with random TRUE and FALSE

random <- sample(c(TRUE, FALSE),
                 
                 size = 10,
                 
                 replace = TRUE)

random



# A logical vector with all TRUE

all_true <- rep(TRUE, 10)

all_true



# A logical vector with all FALSE

all_false <- rep(FALSE, 10)

all_false



prod(random)

prod(all_true)

prod(all_false)



min(random)

min(all_true)

min(all_false)



# In R, when we apply the `prod()` function to a logical vector, it treats `TRUE` as 1 and `FALSE` as 0, and then computes the product of the elements in the vector. Essentially, it multiplies all the elements together. This can be useful when we want to check if all elements in a logical vector are `TRUE`, as the product will be 1 if all are `TRUE` and 0 if any of them is `FALSE`.



# This is equivalent to using the `all()` function, which checks if all elements in a logical vector are `TRUE`. The `all()` function returns `TRUE` if all elements are `TRUE` and `FALSE` otherwise.



# Now, when we apply the `min()` function to a logical vector, it also treats `TRUE` as 1 and `FALSE` as 0, and then computes the minimum value. Since 0 represents `FALSE` and 1 represents `TRUE`, the minimum value in a logical vector is `FALSE` (0). Therefore, when we use `min()` on a logical vector with even one value `FALSE`, it will return `FALSE`.



# In summary, `prod()` and `min()` applied to logical vectors have specific behavior related to the interpretation of `TRUE` and `FALSE`, and they are equivalent to the `all()`.



##12.5.4 Exercises

#1. A number is even if it’s divisible by two, which in R you can find out with x %% 2 == 0. Use this fact and if_else() to determine whether each number between 0 and 20 is even or odd.



x = 0:20

if_else(x %% 2 == 0,
        
        true = "even",
        
        false = "odd")



#2. Given a vector of days like x <- c("Monday", "Saturday", "Wednesday"), use an if_else() statement to label them as weekends or weekdays.



days = c("Monday", "Tuesday", "Wednesday", "Thursday",
         
         "Friday", "Saturday", "Sunday")

weeknd = c("Saturday", "Sunday")



x = sample(days, size = 10, replace = TRUE)



cbind(x,
      
      if_else(x %in% weeknd,
              
              "Weekends",
              
              "Weekdays", 
              
              "NA")) |>
  
  as_tibble() |> gt()



#3. Use if_else() to compute the absolute value of a numeric vector called x.



x = sample(x = -10:10,
           
           replace = TRUE,
           
           size = 100)



tibble(
  
  x = x,
  
  abs_x = if_else(x < 0,
                  
                  true = -x,
                  
                  false = x,
                  
                  missing = 0)
  
) |>
  
  gt() |>
  
  opt_interactive(use_pagination = TRUE,
                  
                  pagination_type = "simple")



#4. Write a case_when() statement that uses the month and day columns from flights to label a selection of important US holidays (e.g., New Years Day, 4th of July, Thanksgiving, and Christmas). First create a logical column that is either TRUE or FALSE, and then create a character column that either gives the name of the holiday or is NA.



flights |>
  
  mutate(holiday = case_when(
    
    month == 1 & day == 1   ~ "New Year’s Day",
    
    month == 6 & day == 19  ~ "Juneteenth National Independence Day",
    
    month == 7 & day == 4   ~ "Independence Day",
    
    month == 11 & day == 11 ~ "Veterans’ Day",
    
    month == 12 & day == 25 ~ "Christmas Day",
    
    .default = NA
    
  ),
  
  .keep = "used") |>
  
  mutate(is_holiday = if_else(!is.na(holiday),
                              
                              true = TRUE,
                              
                              false = FALSE))



## 13.3.1 Exercises

#1. How can you use count() to count the number of rows with a missing value for a given variable?



flights |>
  
  group_by(month) |>
  
  summarise(total = n(),
            
            missing = sum(is.na(dep_time))) |>
  
  gt()





flights |>
  
  group_by(month) |>
  
  count(wt = is.na(dep_time)) |>
  
  ungroup() |>
  
  gt()



#2. Expand the following calls to count() to instead use group_by(), summarize(), and arrange():



flights |>
  
  group_by(dest) |>
  
  summarise(n = n()) |>
  
  arrange(desc(n))





flights |>
  
  group_by(tailnum) |>
  
  summarise(n = sum(distance))



## 13.4.8 Exercises



#1. Explain in words what each line of the code used to generate Figure 13.1 does.



# Load in the data-set flights from package nycflights13

flights |> 
  
  # Create a variable hour, which is the quotient of the division of
  
  # sched_dep_time by 100. Further, group the dataset by this newly 
  
  # created variable "hour" to get data into 24 groups - one for each
  
  # hour.
  
  group_by(hour = sched_dep_time %/% 100) |> 
  
  
  
  # For each gropu, i.e. all flights scheduled to depart in that
  
  # hour, compute the NAs, i.e. cancelled flights, then compute their
  
  # mean, i.e. proportion of cancelled flights; and also create a 
  
  # variable n, which is the total number of flights
  
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  
  
  
  # Remove the flights departing between 12 midnight and 1 am, since
  
  # these are very very few, and all are cancelled leading to a highly
  
  # skewed and uninformative graph
  
  filter(hour > 1) |> 
  
  
  
  # Start a ggplot call, plotting the hour on the x-axis, and 
  
  # proportion of cancelled flights on the y-axis
  
  ggplot(aes(x = hour, y = prop_cancelled)) +
  
  
  
  # Create a line graph,which joins the proportion of cancelled 
  
  # flights for each hour. Also, put in in dark grey colour
  
  geom_line(color = "grey50") + 
  
  
  
  # Add points for each hour, whose size varies with the total number
  
  # of flights in that hour
  
  geom_point(aes(size = n))



#2. What trigonometric functions does R provide? Guess some names and look up the documentation. Do they use degrees or radians?



# In R, trigonometric functions like `sin`, `cos`, and `tan` expect angles to be in radians by default. However, we can convert between degrees and radians using the `deg2rad` and `rad2deg` functions. For example, to compute the sine of an angle in degrees, you can use `sin(deg2rad(angle))`.



#3. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. You can see the basic problem by running the code below: there’s a gap between each hour.



flights |>    
  
  filter(month == 1, day == 1) |>    
  
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +   
  
  geom_point()



# Convert them to a more truthful representation of time (either fractional hours or minutes since midnight).



gridExtra::grid.arrange(
  
  flights |>    
    
    filter(month == 1, day == 1) |>    
    
    ggplot(aes(x = sched_dep_time, y = dep_delay)) +   
    
    geom_point(size = 0.5) +
    
    labs(subtitle = "Incorrect scheduled departure time"),
  
  
  
  flights |>
    
    mutate(
      
      hour_dep = sched_dep_time %/% 100,
      
      min_dep  = sched_dep_time %%  100,
      
      time_dep = hour_dep + (min_dep/60)
      
    ) |>
    
    filter(month == 1, day == 1) |>    
    
    ggplot(aes(x = time_dep, y = dep_delay)) +   
    
    geom_point(size = 0.5) +
    
    labs(subtitle = "Improved and accurate scheduled departure time",
         
         x = "Scheduled Departure Time (in hrs)") +
    
    scale_x_continuous(breaks = seq(0,24,4)),
  
  
  
  ncol = 2)



#4. Round dep_time and arr_time to the nearest five minutes.



attach(flights)

flights |>
  
  slice_head(n = 50) |>
  
  mutate(
    
    dep_time_5 = round(dep_time/5) * 5,
    
    arr_time_5 = round(arr_time/5) * 5,
    
    .keep = "used"
    
  ) |>
  
  gt() |>
  
  opt_interactive()



##13.5.4 Exercises

#1. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().



flights |>
  
  select(sched_dep_time, dep_time, dep_delay, tailnum, dest, carrier) |>
  
  mutate(rank_delay = min_rank(desc(dep_delay))) |>
  
  arrange(rank_delay) |>
  
  slice_head(n = 10) |>
  
  gt()



#2. Which plane (tailnum) has the worst on-time record?



# The flight tailnum with the highest average delay

flights |>
  
  group_by(tailnum) |>
  
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            
            n = n()) |>
  
  arrange(desc(mean_delay)) |>
  
  slice_head(n = 5)



# The flight tailnum with the highest average delay amongst 

# the flights that flew atleast 5 times or more

flights |>
  
  group_by(tailnum) |>
  
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            
            nos_of_flights = n()) |>
  
  filter(nos_of_flights >= 5) |>
  
  arrange(desc(mean_delay)) |>
  
  slice_head(n = 5)



#3. What time of day should you fly if you want to avoid delays as much as possible?



flights |>
  
  mutate(sched_dep_hour = sched_dep_time %/% 100) |>
  
  group_by(sched_dep_hour) |>
  
  summarize(
    
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    
    nos_of_flights = n()
    
  ) |>
  
  drop_na() |>
  
  ggplot(aes(x = sched_dep_hour,
             
             y = mean_dep_delay)) +
  
  geom_line() +
  
  geom_point(aes(size = nos_of_flights), 
             
             alpha = 0.5) +
  
  theme_light() +
  
  labs(x = "Depature Hour", y = "Average departure delay (min)",
       
       title = "Early morning flights have the least delay",
       
       size = "Number of flights") +
  
  scale_x_continuous(breaks = seq(5, 24, 2)) +
  
  theme(legend.position = "bottom")



#4. What does flights |> group_by(dest) |> filter(row_number() < 4) do? What does flights |> group_by(dest) |> filter(row_number(dep_delay) < 4) do?



flights |> 
  
  # reducing the number of columns for easy display
  
  select(carrier, dest, sched_dep_time, month, day) |>
  
  group_by(dest) |> 
  
  # arrange(dest, sched_dep_time) |>
  
  filter(row_number() < 4)



flights |> 
  
  # reducing the number of columns for easy display
  
  select(carrier, dest, sched_dep_time, month, day, dep_delay) |>
  
  group_by(dest) |> 
  
  filter(row_number(dep_delay) < 4)



#5. For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.



flights |>
  
  group_by(dest) |>
  
  mutate(
    
    total_delay = sum(dep_delay, na.rm = TRUE),
    
    prop_delay = dep_delay / total_delay,
    
    .keep = "used"
    
  )



#6. Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag(), explore how the average flight delay for an hour is related to the average delay for the previous hour.



flights |>    
  
  # Generate hour of departure
  
  mutate(hour = dep_time %/% 100) |>    
  
  
  
  # Creating groups by each hour of depature for different days
  
  group_by(year, month, day, hour) |>  
  
  
  
  # Remove 213 flights with NA as hour (i.e., cancelled flights)
  
  # to improve the subsequent plotting
  
  filter(!is.na(hour)) |>
  
  
  
  # Average delay and the number of flights in each hour
  
  summarize(     
    
    dep_delay = mean(dep_delay, na.rm = TRUE),     
    
    n = n(),     
    
    .groups = "drop"   
    
  ) |>    
  
  
  
  # Removing hours in which there were less than 5 flights departing
  
  filter(n > 5) |>
  
  
  
  # Grouping to prevent using 11 pm hour delays as previous delays of the
  
  # next day's 5 am flights
  
  group_by(year, month, day) |>
  
  
  
  # A new variabe to show previous hour's average average departure delay
  
  mutate(
    
    prev_hour_delay = lag(dep_delay),
    
    morning_flights = hour == 5
    
  ) |>
  
  
  
  # Plotting to see correlation
  
  ggplot(aes(x = dep_delay,
             
             y = prev_hour_delay,
             
             col = morning_flights)) +
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(se = FALSE) +
  
  geom_abline(slope = 1, intercept = 0, col = "grey") +
  
  theme_light() +
  
  coord_fixed() +
  
  scale_x_continuous(breaks = seq(0,300,60)) +
  
  scale_y_continuous(breaks = seq(0,300,60)) +
  
  scale_color_discrete(labels = c("Other flights",
                                  
                                  "Early Morning flights (5 am to 6 am)")) +
  
  labs(x = "Average Departure Delay in an hour (min)",
       
       y = "Average Departure Delay in the previous hour (min)",
       
       col = NULL,
       
       title = "Departure Delay correlates with delay in previous hour",
       
       subtitle = "The average departure delay in any hour is worse than previous hours's average delay. \nFurther, the early morning flights' delay doesn't depend on previous nights' delay.") +
  
  theme(legend.position = "bottom")



#7. Look at each destination. Can you find flights that are suspiciously fast (i.e. flights that represent a potential data entry error)? Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?



flights |>
  
  select(month, day, dest, tailnum, dep_time, arr_time, air_time) |>
  
  group_by(dest) |>
  
  mutate(
    
    min_air_time = min(air_time, na.rm = TRUE),
    
    rel_ratio = air_time/min_air_time
    
  ) |>
  
  relocate(air_time, min_air_time, rel_ratio) |>
  
  ungroup() |>
  
  arrange(desc(rel_ratio)) |>
  
  slice_head(n = 100) |>
  
  gt() |>
  
  fmt_number(columns = c(min_air_time, rel_ratio),
             
             decimals = 2) |>
  
  opt_interactive()



#8. Find all destinations that are flown by at least two carriers. Use those destinations to come up with a relative ranking of the carriers based on their performance for the same destination. 



df1 = flights |>
  
  group_by(dest) |>
  
  summarize(no_of_carriers = n_distinct(carrier)) |>
  
  filter(no_of_carriers > 1)



dest2 = df1 |>
  
  select(dest) |>
  
  as_vector() |>
  
  unname()



df2 = flights |>
  
  filter(dest %in% dest2) |>
  
  group_by(dest, carrier) |>
  
  summarise(
    
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    
    prop_cancelled = mean(is.na(dep_time)),
    
    mean_air_time  = mean(air_time, na.rm = TRUE),
    
    nos_of_flights = n()
    
  ) |>
  
  drop_na()



df3 = df2 |>
  
  mutate(
    
    score_delay = (mean_dep_delay - min(mean_dep_delay))/(max(mean_dep_delay) - min(mean_dep_delay)),
    
    score_cancel = (prop_cancelled - min(prop_cancelled)) / (max(prop_cancelled) - min(prop_cancelled)),
    
    score_at = (mean_air_time - min(mean_air_time)) / (max(mean_air_time) - min(mean_air_time)),
    
    total_score = score_delay + score_cancel + score_at,
    
    rank_carrier = min_rank(total_score)
    
  ) |>
  
  drop_na()



df3 |>
  
  arrange(dest, rank_carrier) |>
  
  select(dest, carrier, rank_carrier) |>
  
  ungroup() |>
  
  pivot_wider(names_from = rank_carrier,
              
              values_from = carrier) |>
  
  t() |>
  
  as_tibble() |>
  
  janitor::row_to_names(row_number = 1) |>
  
  gt() |>
  
  sub_missing(missing_text = "") |>
  
  gtExtras::gt_theme_538()



## 13.6.7 Exercises

#1. Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. When is mean() useful? When is median() useful? When might you want to use something else? Should you use arrival delay or departure delay? Why might you want to use data from planes?



# 1. Mean Delay

# - The average delay across all flights.

# - Useful when delays are symmetrically distributed and there are no extreme outliers.



# 2. Median Delay

# - The middle value of delay when sorted.

# - Useful when data is skewed or contains outliers; gives a better "typical" delay.



# 3. Mode of Delays

# - The most frequently occurring delay time.

# - Useful to understand the most common experience for passengers.



# 4. Percentiles (e.g., 75th percentile)

# - Shows the value below which a certain percentage of delays fall.

# - Useful to understand the delay range for most passengers and identify "normal worst-case" scenarios.



# 5. Standard Deviation or IQR

# - Measures the variability or spread of delay times.

# - Useful when evaluating consistency or reliability of flight times.



# Mean vs. Median



# - Mean is useful when the data is evenly distributed and you're okay with the influence of outliers.

# - Median is better when data is skewed or contains extreme values (e.g., a few very long delays).



# Arrival Delay vs. Departure Delay



# - Arrival delay is generally preferred when assessing passenger experience and schedule impact.

# - It includes all delay factors, including air traffic, weather en route, and connection delays.

# - Departure delay is useful for analyzing airport/ground performance or gate issues.



# Using Plane Data



# - Helps control for aircraft-specific factors like maintenance issues or plane utilization.

# - Useful for tracking repeated delays tied to the same aircraft.

# - Can help assess airline operations, turnaround efficiency, and scheduling patterns.





#2. Which destinations show the greatest variation in air speed?



flights |>
  
  mutate(speed = 60 * distance/air_time) |>
  
  group_by(dest) |>
  
  summarise(
    
    nos = n(),
    
    mean = mean(speed, na.rm = TRUE),
    
    sd   = sd(speed, na.rm = TRUE),
    
    CV   = sd/mean) |>
  
  arrange(desc(CV)) |>
  
  slice_head(n = 5) |>
  
  gt() |>
  
  fmt_number(columns = -nos, 
             
             decimals = 2) |>
  
  fmt_percent(columns = "CV") |>
  
  cols_label(
    
    dest = "Destination",
    
    nos = "Number of flights",
    
    mean = "Mean Air Speed (mph)",
    
    sd = "Std. Dev. Air Speed",
    
    CV = "Coeff. of Variation"
    
  ) |>
  
  gtExtras::gt_theme_538()



#3. Create a plot to further explore the adventures of EGE. Can you find any evidence that the airport moved locations? Can you find another variable that might explain the difference?



flights |>
  
  filter(dest == "EGE") |>
  
  group_by(distance) |>
  
  count() |>
  
  ungroup() |>
  
  gt() |>
  
  cols_label(
    
    distance = "Distance (in miles)",
    
    n = "Number of flights in 2013"
    
  ) |>
  
  gtExtras::gt_theme_538()



flights |>
  
  filter(dest == "EGE") |>
  
  ggplot(aes(x = distance, y = 1)) +
  
  geom_jitter(width = 0, 
              
              height = 0.1,
              
              alpha = 0.3) +
  
  theme_minimal() +
  
  coord_cartesian(xlim = c(1500, 1800),
                  
                  ylim = c(0.8, 1.2)) +
  
  scale_x_continuous(breaks = c(1500, 1600, 1700, 1725, 1745, 1800)) +
  
  labs(title = "Flight distances to EGE cluster into two groups around 1725 and 1746 miles", 
       
       x = "Flight Distance (miles)", y = NULL) +
  
  theme(panel.grid.minor = element_blank(),
        
        panel.grid.major.y = element_blank(),
        
        axis.text.y = element_blank(),
        
        axis.ticks.y = element_blank())







# Now, upon exploring further, we find another variable, i.e., `origin` of the flights might explain the difference since `EWR` and `JFK` are slightly away from each other, even within New York City metropolitan area. The @tbl-q3b-ex6 explains this, and we conclude that after all, `EGE` airport may not have shifted after all.



flights |>
  
  filter(dest == "EGE") |>
  
  group_by(origin) |>
  
  summarise(
    
    mean_of_distance = mean(distance, na.rm = TRUE),
    
    std_dev_of_distance = sd(distance, na.rm = TRUE),
    
    proportion_of_cancelled_flights = mean(is.na(dep_time)),
    
    number_of_flights = n()
    
  ) |>
  
  gt() |>
  
  fmt_percent(columns = proportion_of_cancelled_flights) |>
  
  fmt_number(decimals = 2,
             
             columns = c(mean_of_distance, std_dev_of_distance)) |>
  
  cols_label_with(fn = ~ janitor::make_clean_names(., case = "title")) |>
  
  gtExtras::gt_theme_pff()