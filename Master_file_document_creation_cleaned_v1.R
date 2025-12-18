#needed packages 
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(skimr)


#-------------------------DATASET CREATION------------------------------------


#load data 
rides   <- read_csv("Rides_Data.csv", show_col_types = FALSE)
drivers <- read_csv("Drivers_Data.csv", show_col_types = FALSE)

#get column names and make everything lower case
colnames(rides)   <- str_to_lower(colnames(rides))
colnames(drivers) <- str_to_lower(colnames(drivers))

#map states of cities
city_to_state <- function(city) {
  case_when(
    is.na(city) | str_trim(city) == "" ~ NA_character_,
    
    str_to_lower(city) == "chicago"       ~ "illinois",
    str_to_lower(city) == "miami"         ~ "florida",
    str_to_lower(city) == "new york"      ~ "new york",
    str_to_lower(city) == "los angeles"   ~ "california",
    str_to_lower(city) == "san francisco" ~ "california",
    
    TRUE ~ NA_character_
  )
}

#create dataset merged and add columns
merged <- rides %>%
  rename(discount_name = promo_code, gross_fare_usd = fare) %>% #increase clarity of col names 
  mutate(
    date = mdy(date), #format date as date
    
    average_speed_kmh = distance_km / (duration_min / 60), #calculate speed in km/h
    
    ride_category = case_when(  #new column ride category 
      distance_km < 17.1 ~ "short_range",
      distance_km >= 17.1 & distance_km <= 33.4 ~ "mid_range",
      distance_km > 33.4 ~ "long_range",
      TRUE ~ NA_character_
    ),
    
    discount_name = if_else( #add "no discount" for regular trips in discount column
      is.na(discount_name) | str_trim(discount_name) == "",
      "NO DISCOUNT",
      discount_name
    ),
    #new column with numeric discount
    discount_digits = str_extract(discount_name, "\\d{1,2}$"), 
    discount_percentage = if_else(
      discount_name == "NO DISCOUNT" | is.na(discount_digits),
      0,
      as.numeric(discount_digits) / 100
    ),
    net_fare_usd = gross_fare_usd * (1 - discount_percentage), #calc net fare
    year        = year(date), #add year in date format
    quarter     = quarter(date), #add quarter in date format
    month       = month(date), #add month in date format
    month_name  = str_to_lower(as.character(month(date, label = TRUE, abbr = FALSE))), #add month lower case
    week        = isoweek(date), #add week
    day         = day(date), #add day
    day_of_week = str_to_lower(as.character(wday(date, label = TRUE, abbr = FALSE))), #add day as varchar
    is_weekend  = day_of_week %in% c("saturday", "sunday"), #add boolean for weekend vs weekdays
    
    start_date = if_else(  #as all discounts were used during the whole month we assume a start date of 
      #latest Oct 31. 
      discount_name == "NO DISCOUNT",
      as.Date(NA),
      dmy("31.10.2024")
    ),
    end_date = as.Date(NA) #end date stays empty until campaign is terminated 
  ) %>%
  left_join(drivers, by = "driver_id", suffix = c("_ride", "_driver")) %>% #left join drivers 
  mutate( 
    name = str_trim(
      str_remove(name, "\\s+(MD|PhD)$") #remove titles 
    ), 
    experience_level = case_when( #classify the experience level
      ntile(experience_years, 3) == 1 ~ "junior",
      ntile(experience_years, 3) == 2 ~ "mid",
      ntile(experience_years, 3) == 3 ~ "senior",
      TRUE ~ NA_character_), #
    
    state_driver_home_city = city_to_state(city_driver), #add home state of driver
    state_ride             = city_to_state(city_ride) #add state where ride took place 
  ) %>%
  select(-discount_digits) %>% 
  rename(
    age_years = age, #add unit for age 
    employment_status = active_status #increase clarity 
  )
  
glimpse(merged) 
#write_csv(merged, "merged_output_v7.csv")


#------------------------EXPLORATORY DATA ANALYSIS-----------------------------
#------------------------general analyses-------------------------------------
#summary statistics
summary(merged)
skim(merged)

#correlation matrix
num_vars <- merged %>% select(where(is.numeric))
cor(num_vars, use = "complete.obs") 

# number of drivers 
merged %>%
  summarise(n_drivers = n_distinct(driver_id))

#number of rides
merged %>%
  summarise (n_rides = n_distinct(ride_id))

# rides per city
merged %>%
  count(city_ride, sort = TRUE) #most rides in LA

#total net revenue
merged %>%
  summarise(total_revenue = sum(net_fare_usd))

#comparison average rides per day - week by week
weekly_avg_rides <- merged %>%
  group_by(year, week, date) %>%
  summarise(
    rides_per_day = n(),
    .groups = "drop"
  ) %>%
  group_by(year, week) %>%
  summarise(
    avg_rides_per_day = mean(rides_per_day),
    .groups = "drop"
  )
print(weekly_avg_rides)

#revenue comparison week by week
daily_revenue <- merged %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarise(
    revenue_per_day = sum(net_fare_usd, na.rm = TRUE),
    .groups = "drop"
  )

weekly_avg_revenue <- daily_revenue %>%
  mutate(
    year = lubridate::year(date),
    week = lubridate::isoweek(date)
  ) %>%
  group_by(year, week) %>%
  summarise(
    avg_revenue_per_day = mean(revenue_per_day),
    .groups = "drop"
  )
weekly_avg_revenue


#--------------trip duration------------------------------

#average duration
mean(merged$duration_min, na.rm = TRUE)

#min duration
merged %>%
  slice_min(duration_min, n = 1, with_ties = TRUE)

#nr. of rides with only 2min duration
merged %>%
  count(duration_min == 2) #1.9% of rides with only 2 Min

# number of rides duration smaller/ equal 5 min
merged %>%
  count(duration_min <= 5) #1.9% of rides with only 2 Min  #5.2% 

#max duration
merged %>% #two trips 121 min
  slice_max(duration_min, n= 1, with_ties = TRUE)

# fare per minute
merged %>%
  summarise(
    min_fare_per_min  = min(fare_usd / duration_min, na.rm = TRUE),
    max_fare_per_min  = max(fare_usd / duration_min, na.rm = TRUE),
    mean_fare_per_min = mean(fare_usd / duration_min, na.rm = TRUE)
  )

#fares per km
merged %>%
  summarise(
    min_fare_per_km = min(fare_usd / distance_km, na.rm = TRUE),
    max_fare_per_km  = max(fare_usd / distance_km, na.rm = TRUE),
    mean_fare_per_km = mean(fare_usd / distance_km, na.rm = TRUE)
  )

# fares vs duration
ggplot(merged, aes(x = duration_min, y = fare_usd, color = ride_category)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Ride Duration and Fare",
    x = "Duration (minutes)",
    y = "Fare (USD)",
    color = "Distance Category"
  ) + 
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() # t


# fares vs distance
ggplot(merged, aes(x = distance_km, y = fare_usd)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Distance of ride and Fare",
    x = "Distance (km)",
    y = "Fare (USD)"
  )

#fares vs duration with distance (bad practice)

ggplot(merged, aes(
  x = duration_min,
  y = fare_usd,
  size = distance_km
)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(0.5, 3))
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fare vs Duration (size = Distance)",
    x = "Duration (minutes)",
    y = "Fare (USD)",
    size = "Distance (km)"
  )

#fares std long_range

merged %>%
  filter(ride_category == "long_range") %>%
  summarise(
    variance_net_fare = var(net_fare_usd, na.rm = TRUE),
    sd_net_fare       = sd(net_fare_usd, na.rm = TRUE)
  )
#comparison std fare by ride category
merged %>%
  group_by(ride_category) %>%
  summarise(
    variance_net_fare = var(net_fare_usd, na.rm = TRUE),
    sd_net_fare       = sd(net_fare_usd, na.rm = TRUE)
  )

#comparisons std distances by ride category 
merged %>%
  group_by(ride_category) %>%
  summarise(
    variance_distance_km = var(distance_km, na.rm = TRUE),
    sd_distance_km    = sd(distance_km, na.rm = TRUE)
  )


# relationship speed*duration and fare 
ggplot(merged, aes(x = average_speed_kmh*duration_min, y = fare_usd)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Average Speed times the Duration of the ride and the fare",
    x = "Speed * duration",
    y = "Fare (USD)"
    )
  
  
#average duration per city

merged %>%
  group_by(city_ride) %>%
  summarise(
    avg_duration = mean(duration_min, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_duration))   # optional: sort by duration


#average km per city
merged %>%
  group_by(city_ride) %>%
  summarise(
    avg_distance = mean(distance_km, na.rm = TRUE)
  )

#max km 
merged %>%
  summarise(
    avg_distance = mean(distance_km, na.rm = TRUE)
  )
merged %>%
  summarise(
    max_distance = max(distance_km, na.rm = TRUE)
  )
# correlation speed - duration 
ggplot(merged, aes(x = duration_min, y = average_speed_kmh, color = ride_category)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Average Speed and Duration of the trip ",
    x = "Duration (min)",
    y = "Speed (km/h)",
    color = "Ride Category"
  )  +
  scale_color_brewer(palette = "Set1") +  
  theme_minimal() # those super long trips have a super low speed 


#------------customer rating---------------------------
#number of customers who left a review
merged %>%
  summarise(
    n_rated = sum(!is.na(rating) & str_trim(rating) != "")
  ) #all customers left a review 

# rating and driver's experience
ggplot(merged, aes(x = experience_years, y = rating)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Customer Rating and Experience of the Driver ",
    x = "Experience Driver (Years) ",
    y = "Customer Rating (1-5)") #no correlation

# rating weekend vs non weekend
merged %>%
  group_by(is_weekend) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating)) # no significant difference of rating on weekends vs during the week

#development of rating over time
merged %>%
  group_by(week) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating))
#ranking differences across cities 
merged %>%
  group_by(city_ride) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    median_rating = median(rating, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating)) # chic

#rating of drivers in their hometown vs in foreign towns 
merged <- merged %>%
  mutate(
    local_ride = if_else(city_ride == city_driver, "local", "non_local")
  )

merged %>%
  group_by(local_ride) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    median_rating = median(rating, na.rm = TRUE),
    n_rides   = n()
  )

#------------discount-------------------------------------------------
#average rating based on discount percentage
merged %>%
  group_by(discount_percentage) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    median_rating = median(rating, na.rm = TRUE),
    n_rides   = n()
  ) # slightly higher rating with discount 

# are gross fares lower for rides with discount?
merged %>%
  group_by(discount_percentage) %>%
  summarise(
    avg_gross_fare = mean(gross_fare_usd, na.rm = TRUE),
    n_rides   = n()
  ) #fares not generally lower with discount applied

#discount usage in different cities
merged %>%
  group_by(city_ride) %>%
  summarise(
    discount_percentage = mean(discount_percentage, na.rm = TRUE),
    n_discounts   = n()
)

merged %>%
  group_by(city_ride) %>%
  summarise(
    total_rides          = n(),
    discounted_rides     = sum(discount_percentage > 0, na.rm = TRUE),
    pct_discounted_rides = discounted_rides / total_rides * 100
  )


#------------riders------------------------
#active status 
drivers %>%
  count(active_status, name = "n_drivers") %>%
  arrange(desc(n_drivers))

#rides per rider 
rides_per_driver <- merged %>%
  group_by(driver_id) %>%
  summarise(
    rides_per_driver = n(),
    .groups = "drop"
  )

rides_per_driver %>%
  summarise(
    avg_rides_per_driver = mean(rides_per_driver),
    min_rides_per_driver = min(rides_per_driver),
    max_rides_per_driver = max(rides_per_driver)
  )

#rides per driver per day
rides_per_driver_day <- merged %>%
  group_by(driver_id, date) %>%
  summarise(
    rides_per_day = n(),
    .groups = "drop"
  )

rides_per_driver_day %>%
  summarise(
    avg_rides_per_day = mean(rides_per_day),
    min_rides_per_day = min(rides_per_day),
    max_rides_per_day = max(rides_per_day)
  )

# driving days per driver 

active_days_per_driver <- merged %>%
  group_by(driver_id) %>%
  summarise(
    active_days = n_distinct(date),
    .groups = "drop"
  )

active_days_per_driver %>%
  summarise(
    avg_active_days = mean(active_days),
    min_active_days = min(active_days),
    max_active_days = max(active_days)
  )


#driver age and rating
library(ggplot2)

ggplot(merged, aes(x = age_years, y = rating)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship between Driver Age and Customer Rating",
    x = "Driver Age (years)",
    y = "Customer Rating"
  ) +
  theme_minimal() # not relevant 


#riders that ride outside their hometown
merged %>%
  group_by(driver_id) %>%
  summarise(
    rides_non_local = sum(city_ride != city_driver)
  ) %>%
  filter(rides_non_local == 0) %>%
  summarise(n_only_local = n()) # there are no riders that ride only in their
# hometown

#riders that don't ride in their hometown
merged %>%
  group_by(driver_id) %>%
  summarise(
    rides_local = sum(city_ride == city_driver)
  ) %>%
  filter(rides_local == 0) %>%
  summarise(n_never_local = n()) # 14 riders never ride in their hometown

#in how many different cities do riders ride?
merged %>%
  group_by(driver_id) %>%
  summarise(n_cities = n_distinct(city_ride)) %>%       # keep only 1–5 cities
  count(n_cities)                 # counts how many drivers per number of cities







#experience levels thresholds
quantile(merged$experience_years, probs = c(1/3, 2/3), na.rm = TRUE)

#ride_category thresholds
quantile(merged$distance_km, probs = c(1/3, 2/3, 1), na.rm = TRUE)



