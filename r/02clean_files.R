library(fst)
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(absmapsdata)

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
  filter(!week_day %in% c("Sun","Sat"))

#get a traffic count for the second Tuesday in April

pre_lockdown <- scats_data %>% filter(week_day == "Tue",
                                      month_week %in% c(2),
                                      month == 4 ) %>% 
  group_by(nb_scats_site,date) %>% 
  # Each intersection has lots of sensors - often one for every lane. 
  # We're not interested in left turning cars or right turning cars. 
  # The easiest filter is to just find whichever sensor is the busiest, 
  # and use that as a marker of the whole intersection.
  filter(qt_volume_24hour == max(qt_volume_24hour)) %>% 
  rename(count_pre = qt_volume_24hour) %>% 
  filter(count_pre>100) # FILTER OUT TINY SITES! 

#get a traffic count for the third Tuesday in July
post_lockdown <- scats_data %>% filter(week_day == "Tue",
                                       month_week %in% c(3),
                                       month == 7) %>%
select(nb_detector, 
       nb_scats_site,
       count_post = qt_volume_24hour)

#Join them together
lockdown_comparison <- inner_join(pre_lockdown, 
                                  post_lockdown) %>% 
                       mutate(change = count_post / count_pre) %>% 
                       left_join(sensor_locations) %>% 
                       mutate(lat = st_coordinates(geometry)[,2],
                              lon = st_coordinates(geometry)[,1]) %>%
  filter(!is.na(lat)) %>% #Some of the intersections didn't have a lat lon. This is weird - but we have enough data to make our point. 
  ungroup() %>% 
  filter(st_intersects(x = .$geometry, 
                       y = mel_area$geometry, 
                       sparse = FALSE)) %>% 
  #Create bins so that the scale is nice when we make the final graph
  mutate(change_bin = case_when(change<.8 ~.8,
                            change>1.2 ~ 1.2,
                            TRUE ~ change)) %>% 
  filter(change>.1, #IF THE CHANGE IS TOO MUCH WE SAY IT IS AN ERROR
         change<10)


#Create leaflet

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = c(.8,1.2))


leaflet(lockdown_comparison) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(change_bin),
    stroke = FALSE, fillOpacity = 0.5
  )  %>% 
  addLegend(pal = pal, values = ~change_bin, opacity = 1)


