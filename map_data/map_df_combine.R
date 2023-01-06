library(dplyr)

df_combine <- function(name1, name2) {
  raw1 <- read.csv(name1, encoding = 'UTF-8')
  raw2 <- read.csv(name2, encoding = 'UTF-8')
  df1 <- raw1 %>%
    select(placeVisit_location_name, placeVisit_location_address,
           placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp,
           placeVisit_location_latitudeE7, placeVisit_location_longitudeE7
           # i inne potrzebne kolumny...
           )
  df2 <- raw2 %>%
    select(placeVisit_location_name, placeVisit_location_address,
           placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp,
           placeVisit_location_latitudeE7, placeVisit_location_longitudeE7
           # i inne potrzebne kolumny...
    )
  df <- rbind(df1, df2)
  return(df)
}

df_raw_t <- df_combine("raw_data/december_data_t", "raw_data/january_data_t")
df_raw_w <- df_combine("raw_data/december_data_w", "raw_data/january_data_w")

df_t <- df_raw_t %>%
  mutate(person = "Tymek") %>%
  mutate(color = 'blue')

df_w <- df_raw_w %>%
  mutate(person = "Wojtek") %>%
  mutate(color = "yellow")

df_raw = rbind(df_t, df_w)

map_df <- df_raw %>%
  filter(placeVisit_location_address != "") %>%
  mutate(date = substr(placeVisit_duration_startTimestamp, 1, 10)) %>%
  mutate(lat = placeVisit_location_latitudeE7 / 10000000) %>%
  mutate(lng = placeVisit_location_longitudeE7 / 10000000)

write.csv(map_df, file = "map_df.csv", row.names = FALSE)
