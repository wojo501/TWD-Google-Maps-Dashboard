library("rjson")
library("dplyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("shiny")
library("tidyr")



decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_w", encoding = "UTF-8"))
janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_w", encoding = "UTF-8"))
# 
# decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_t", encoding = "UTF-8"))
# janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_t", encoding = "UTF-8"))
# 
# decCsv <- as.data.frame(read.csv(file = "raw_data/december_data_c", encoding = "UTF-8"))
# janCsv <- as.data.frame(read.csv(file = "raw_data/january_data_c", encoding = "UTF-8"))



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
  mutate(time_diff = as.integer(difftime(timeEnd, timeStart, units = "days")))

while(filterData %>% filter(time_diff != 0) %>% summarise(n()) %>% first() > 0){
  diffDay <- filterData %>%  
    filter(time_diff != 0) %>% 
    bind_rows(as.data.frame(.) %>% select(name, timeStart, timeEnd, time_diff) %>% 
                mutate(timeStart = timeStart, timeEnd = as.Date(timeStart) + 1, time_diff = as.integer(difftime(timeEnd, timeStart, units = "days")), part = "new"), as.data.frame(.) %>% 
                select(name, timeStart, timeEnd, time_diff) %>% 
                mutate(timeStart = as.Date(timeStart)+1, timeEnd = timeEnd, time_diff = as.integer(difftime(timeEnd, timeStart, units = "days")), part = "new")) %>% 
    filter(part == "new") %>% 
    select(-part)
  
  filterData <- filterData %>% 
    filter(time_diff == 0) %>% 
    bind_rows(diffDay)
}


View(filterData %>% filter(time_diff = 0))

filterData <- filterData %>%
  mutate(minutes = as.numeric(difftime(timeEnd, timeStart))) %>% 
  mutate(week = format(filterData$timeStart, format="%Y-%U")) %>% 
  mutate(weekday = wday(timeStart))


#stringi Wojtek
home <- c("mikrus", "cieplewo", "łęgowo")
uni <- c("university", "akademik", "lincoln", "politechnika", "faculty", "central")
fun <- c("unii", "fryzjer", "rostock", "museum", "suntago", "royal", "church", "polny", "game", "frankfurt", "hamburg")

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
# home <- c("kazimierów", "willa", "repkowska", "sokołowska")
# uni <- c("gmach", "university")
# fun <- c("cybermachina", "gato", "kredens", "kuchnia", "manekin", "muzeum", "cafe", "restauracja")


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

filterDataW <- readRDS(file = "ramkiW/dataW.rds")
filterDataT <- readRDS(file = "ramkiW/dataT.rds")
filterDataC <- readRDS(file = "ramkiW/dataC.rds")
filterDataW <- filterDataW %>% 
  mutate(person = "W")
filterDataT <- filterDataT %>% 
  mutate(person = "T")
filterDataC <- filterDataC %>% 
  mutate(person = "C")
filterData <- rbind(filterDataW, filterDataT, filterDataC)
saveRDS(filterData, file = "ramkiW/data.rds")

View(filterData)
View(df_merged)

person <- c("W", "T", "C")
baseFrame <- data.frame(weekday = c(1:7), hours = integer(7)) %>% 
  expand(weekday, hours, person)
saveRDS(baseFrame, file = "ramkiW/baseFrame.rds")

View(baseFrame)
                        
graphData <- filterData %>% 
  filter(week == "2023-01" & type == "fun" & person %in% c("W", "T")) %>% 
  select(week, weekday, minutes, person) %>% 
  group_by(weekday, person) %>% 
  summarise(hours = sum(minutes)/60) %>% 
  data.frame()
View(graphData)
View(graphDataW)

graphData <- graphData %>% 
  full_join(baseFrame, by = c("weekday", "person")) %>% 
  mutate(hours = coalesce(hours.x, hours.y)) %>% 
  select(-c(hours.x, hours.y)) %>% 
  filter(person %in% c("W", "T"))

plot <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, colour = person)) +
  geom_line() + 
  geom_point() +
  theme_bw()+
  scale_x_continuous("weekday", labels = graphData$weekday, breaks = graphData$weekday)
plot

View(graphData)

