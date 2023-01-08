library("rjson")
library("dplyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("shiny")



# decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_w", encoding = "UTF-8"))
# janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_w", encoding = "UTF-8"))
# 
# decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_t", encoding = "UTF-8"))
# janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_t", encoding = "UTF-8"))

decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_c", encoding = "UTF-8"))
janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_c", encoding = "UTF-8"))



View(decCsv)
View(janCsv)

filterDecCsv <- decCsv %>% 
  filter(placeVisit_location_name != "")

filterJanCsv <- janCsv %>% 
  filter(placeVisit_location_name != "")

filterData<-bind_rows(filterDecCsv, filterJanCsv) %>% 
  select(placeVisit_duration_startTimestamp, placeVisit_duration_endTimestamp, placeVisit_location_name) %>% 
  mutate(placeVisit_location_name = tolower(placeVisit_location_name))
names(filterData) <- c("timeStart", "timeEnd", "name")
x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
filterData$name <- gsub("[[:punct:]]", " ", filterData$name) 

filterData$timeStart <- paste(substring(filterData$timeStart, 1, 10), substring(filterData$timeStart, 12, 16))
filterData$timeEnd <- paste(substring(filterData$timeEnd, 1, 10), substring(filterData$timeEnd, 12, 16))

filterData$timeStart <- strptime(filterData$timeStart, '%Y-%m-%d %H:%M')
filterData$timeEnd <- strptime(filterData$timeEnd, '%Y-%m-%d %H:%M')
filterData <- filterData %>%
  mutate(minutes = as.numeric(difftime(timeEnd, timeStart))) %>% 
  mutate(week = format(filterData$timeStart, format="%Y-%U")) %>% 
  mutate(weekday = wday(timeStart))

#stringi Wojtek
# home <- c("mikrus", "cieplewo", "łęgowo")
# uni <- c("university", "akademik", "lincoln", "politechnika", "faculty", "central")
# fun <- c("unii", "fryzjer", "rostock", "museum", "suntago", "royal", "church", "polny", "game", "frankfurt", "hamburg")

# #stringi Tymek
# home <- c("konstancin", "home")
# uni <- c("wydział", "pw")
# fun <- c("ramen", "asia", "boisko", "green", "garden", "mcdonald's", "momencik", "park", "soto", "sphinx", "cafe",
#          "piaseczno", "handsome", "dworzec", "kabaty", "stańczyka",
#          "tarasy", "gołków")
# work <- c("arkadia", "magazyn")
# other <- c("biedronka", "stara", "lidl")
# 
# #stringi Czarek
home <- c("kazimierów", "willa", "repkowska", "sokołowska")
uni <- c("gmach", "university")
fun <- c("cybermachina", "gato", "kredens", "kuchnia", "manekin", "muzeum", "cafe", "restauracja")


homes <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(home %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "home")

unis <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(uni %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "uni")

funs <- filterData[sapply(strsplit(filterData$name, split=" "), function(str) any(fun %in% str)), ] %>% 
  select("timeStart", "timeEnd") %>% 
  mutate(type = "fun")


filterData <- left_join(filterData, homes, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- left_join(filterData, unis, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- left_join(filterData, funs, by=c('timeStart'='timeStart', 'timeEnd'='timeEnd'))
filterData <- filterData %>%
  mutate(type = case_when(
    !is.na(type.x)~"home",
    !is.na(type.y)~"uni",
    !is.na(type)~"fun",
    TRUE~"other"
  )) %>% select(-type.x, -type.y)

View(filterData)

#dir.create("ramkiW")
#saveRDS(filterData, file = "ramkiW/dataW.rds")
# saveRDS(filterData, file = "ramkiW/dataT.rds")
# saveRDS(filterData, file = "ramkiW/dataC.rds")


baseFrame <- data.frame(weekday = c(1:7), hours = integer(7))

graphData <- filterData %>% 
  filter(week == "2023-01" & type == "fun") %>% 
  select(week, weekday, minutes) %>% 
  group_by(weekday) %>% 
  summarise(hours = sum(minutes)/60) %>% 
  data.frame()

graphData <- graphData %>% 
  full_join(baseFrame, by = "weekday") %>% 
  mutate(hours = coalesce(hours.x, hours.y)) %>% 
  select(-c(hours.x, hours.y))

plot <- ggplot(data = graphData, aes(x=weekday, y=hours)) +
  geom_line() + 
  geom_point() +
  theme_bw()+
  scale_x_continuous("weekday", labels = graphData$weekday, breaks = graphData$weekday)
plot

View(graphData)

