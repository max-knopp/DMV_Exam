

CREATE OR REPLACE VIEW public.dim_city
 AS
 WITH all_cities AS (
         SELECT city_rides_table.city_driver AS city_name,
            city_rides_table.state_driver_home_city AS state
           FROM city_rides_table
        UNION
         SELECT city_rides_table.city_ride AS city_name,
            city_rides_table.state_ride AS state
           FROM city_rides_table
        )
 SELECT dense_rank() OVER (ORDER BY state, city_name) AS location_id,
    city_name,
    state
   FROM ( SELECT DISTINCT all_cities.city_name,
            all_cities.state
           FROM all_cities) s;

ALTER TABLE public.dim_city
    OWNER TO postgres;




CREATE OR REPLACE VIEW public.dim_date
 AS
 WITH d AS (
         SELECT DISTINCT city_rides_table.date AS full_date,
            city_rides_table.year,
            city_rides_table.quarter,
            city_rides_table.month,
            city_rides_table.month_name,
            city_rides_table.week,
            city_rides_table.day,
            city_rides_table.day_of_week,
            city_rides_table.is_weekend
           FROM city_rides_table
        )
 SELECT dense_rank() OVER (ORDER BY full_date) AS date_id,
    full_date,
    year,
    quarter,
    month,
    month_name,
    week,
    day,
    day_of_week,
    is_weekend
   FROM d;

ALTER TABLE public.dim_date
    OWNER TO postgres;




CREATE OR REPLACE VIEW public.dim_discount
 AS
 WITH d AS (
         SELECT DISTINCT city_rides_table.discount_name,
            city_rides_table.discount_percentage,
            city_rides_table.start_date,
            city_rides_table.end_date
           FROM city_rides_table
        )
 SELECT dense_rank() OVER (ORDER BY discount_name, discount_percentage) AS discount_id,
    discount_name,
    discount_percentage,
    start_date,
    end_date
   FROM d;

ALTER TABLE public.dim_discount
    OWNER TO postgres;




CREATE OR REPLACE VIEW public.dim_driver
 AS
 SELECT DISTINCT m.driver_id,
    m.name,
    m.age_years,
    c.location_id,
    m.experience_years,
    m.experience_level AS average_rating,
    m.employment_status
   FROM city_rides_table m
     JOIN dim_city c ON c.city_name = m.city_driver AND c.state = m.state_driver_home_city;

ALTER TABLE public.dim_driver
    OWNER TO postgres;




CREATE OR REPLACE VIEW public.dim_ride_category
 AS
 WITH c AS (
         SELECT DISTINCT city_rides_table.ride_category AS category_name
           FROM city_rides_table
        )
 SELECT dense_rank() OVER (ORDER BY category_name) AS category_id,
    category_name
   FROM c;

ALTER TABLE public.dim_ride_category
    OWNER TO postgres;



CREATE OR REPLACE VIEW public.fact_ride
 AS
 SELECT m.ride_id,
    m.driver_id,
    ride_city.location_id,
    disc.discount_id,
    cat.category_id,
    dt.date_id,
    m.distance_km,
    m.duration_min,
    m.gross_fare_usd,
    m.net_fare_usd,
    m.rating,
    m.average_speed_kmh
   FROM city_rides_table m
     JOIN dim_city ride_city ON ride_city.city_name = m.city_ride AND ride_city.state = m.state_ride
     JOIN dim_discount disc ON disc.discount_name = m.discount_name AND disc.discount_percentage = m.discount_percentage
     JOIN dim_ride_category cat ON cat.category_name = m.ride_category
     JOIN dim_date dt ON dt.full_date = m.date;

ALTER TABLE public.fact_ride
    OWNER TO postgres;






