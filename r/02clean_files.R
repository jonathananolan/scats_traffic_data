library(fst)
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(absmapsdata)


public_holidays <- tribble(~date, 
"20200101",
"20200127",
"20200309",
"20200410",
"20200411",
"20200412",
"20200413",
"20200425",
"20200608",
"20201103",
"20201225",
"20201226",
"20201228") %>% mutate(date = as.POSIXct(ymd(date)))



#Import a map of Melbourne so we can filter out non-Melbournian data points
mel_area <- gcc2016 %>% filter(gcc_name_2016 == "Greater Melbourne")

#Import a list of all the sensor locations
sensor_locations <- read_sf("scats_location_data/898c57e6-4c28-4ae4-9334-403c1ac040822020329-1-1b450pc.nk4u.shp") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  rename(nb_scats_site = site_no) %>% 
  mutate(geometry = st_transform(geometry,st_crs(mel_area$geometry)))

#Import the scats traffic count data
scats_data_raw <- read_fst("processing_files/scats_data.fst") %>% 
              clean_names() %>% 
              select(qt_volume_24hour,
                     nb_scats_site,
                     nb_detector,
                     qt_interval_count)

#clean the scats data, and add variables for the month, week and day. 
scats_data <- scats_data_raw %>% 
              mutate(date = coalesce(ymd_hms(qt_interval_count),
                                     dmy_hms(qt_interval_count,truncated = Inf)),
                     week_day = wday(date,label = TRUE),
                     month_day = day(date),
                     month_week = ceiling(month_day / 7),
                     month = month(date)) %>% 
  filter(!(date %in% public_holidays$date))

#get a traffic count for the second Tuesday in April

pre_lockdown <- scats_data %>% filter(month == 2 ) %>% 
  group_by(nb_scats_site,date) %>% 
  # Each intersection has lots of sensors - often one for every lane. 
  # We're not interested in left turning cars or right turning cars. 
  # The easiest filter is to just find whichever sensor is the busiest, 
  # and use that as a marker of the whole intersection.
  filter(qt_volume_24hour == max(qt_volume_24hour)) %>% 
  rename(count_pre = qt_volume_24hour) %>% 
  filter(count_pre>100) %>% # FILTER OUT TINY SITES!
  group_by(nb_scats_site,week_day) %>% 
  summarise(count_pre_crisis = median(count_pre))


scats_sites <- scats_data %>% 
distinct(nb_scats_site) %>% 
left_join(sensor_locations) %>% 
  mutate(lat = st_coordinates(geometry)[,2],
         lon = st_coordinates(geometry)[,1]) %>%
  filter(!is.na(lat)) %>% #Some of the intersections didn't have a lat lon. This is weird - but we have enough data to make our point. 
  ungroup() %>% 
  filter(st_intersects(x = .$geometry, 
                       y = mel_area$geometry, 
                       sparse = FALSE))
  

#Create bins so that the scale is nice when we make the final graph

post_data <- scats_data %>% 
  filter(nb_scats_site %in% scats_sites$nb_scats_site) %>% 
  group_by(date,nb_scats_site) %>% 
  filter(qt_volume_24hour == max(qt_volume_24hour)) %>% 
  inner_join(scats_sites) %>% 
  left_join(pre_lockdown) %>% 
  mutate(change = qt_volume_24hour / count_pre_crisis) %>% 
  mutate(change_bin = case_when(change<.2 ~.2,
                            change>1.2 ~ 1.2,
                            TRUE ~ change)) %>% 
  filter(change>.1, #IF THE CHANGE IS TOO MUCH WE SAY IT IS AN ERROR
         change<10)

post_data_for_shiny <- post_data %>% select(date,change_bin,nb_scats_site,lat,lon)

write_fst(post_data_for_shiny,"traffic_map/pre_post.fst")

post_data %>% group_by(date) %>% 
  filter(!(week_day %in% c("Sat", "Sun"))) %>% 
  summarise(change = median(change)) %>% 
  ggplot(aes(x = date, y = change))+
  geom_line(stat = "identity")
