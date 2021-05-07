# Loading the packages
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm) # For loading text data as corpus
library(plotly) # For bubble chart
library(viridis)


# Reading the CSV file

air_bnb <- read_csv("AB_NYC_2019.csv", locale = locale(encoding = "UTF-8"))

# Getting to know about the data
str(air_bnb)
summarise_if(air_bnb, is.numeric, mean)

# Checking for missing values wrt to each column (prints the no.of missing values for each column present in the df)
colSums(is.na(air_bnb))

#m <- air_bnb %>%
#  select_all() %>%
#  filter_all(any_vars(is.na(.)))

# Removing the columns with NA values
air_bnb$host_name <- NULL
air_bnb$last_review <- NULL
air_bnb$reviews_per_month <- NULL

# Converting chr columns to factors
air_bnb <- mutate_if(air_bnb, is.character, as.factor)


## Checking & Understanding the data

#most_booked_neighbourhood_group <- air_bnb %>%
#  select(neighbourhood_group, neighbourhood) %>%
#  count(neighbourhood_group)            #(Not necessary we can simply plot this by bar plot)

ggplot(air_bnb, aes(x = reorder(neighbourhood_group, neighbourhood_group,
                                function(x) - length(x)))) +  # Arranging the plots in decreasing order.
  geom_bar(fill = "tomato3") +
  labs(title = "Booking patterns in the neighbourhood group") +
  xlab("neighbourhood_group")

# Checking for high price area as per neighbourhood_group
costliest_area <- air_bnb %>%
  group_by(neighbourhood_group) %>%
  summarise(avg_price = mean(price))

ggplot(costliest_area, aes(x = reorder(neighbourhood_group, -avg_price), y = avg_price)) +
  geom_bar(stat = 'identity', fill = "tomato3") +
  labs(title ="Average price in the neighbourhood group") +
  xlab("neighbourhood_group")

# Checking for high price neighbourhood
costliest_neighbourhood <- air_bnb %>%
  group_by(neighbourhood) %>%
  summarise(avg_room_price = mean(price)) 

most_costly_neighbourhood <- costliest_neighbourhood %>%
  arrange(desc(avg_room_price)) %>%
  filter(avg_room_price >= 250)
  
ggplot(most_costly_neighbourhood, aes(x = reorder(neighbourhood, -avg_room_price),y = avg_room_price)) +
  geom_bar(stat = 'identity', fill = "tomato3") +
  coord_flip() +
  labs(title="Average Price of Rooms in each Neighbourhoods", 
        subtitle = "Top 15 Costliest Neighbourhoods") +
  xlab("neighbourhood")


# Checking the price distribution 
ggplot(air_bnb) +
  geom_density(aes(x = price, group = room_type, fill = room_type), adjust = 500, alpha = .4) +
  theme_ipsum() +
  labs(title = "Price distribution for different room types")

# Most preferred neighbourhood & visualizing it through word cloud
preferred_neighbourhood <- air_bnb %>%
  select(neighbourhood) %>%
  add_count(neighbourhood)

text <- preferred_neighbourhood$neighbourhood # creating a vector containing only text
docs <- Corpus(VectorSource(text)) # Creating a corpus from the text data

# What we want to do as a next step is to have a dataframe containing each word 
#in the first column and their frequency in the second column.
#This can be done by creating a document term matrix with the TermDocumentMatrix 
#function from the tm package

dtm <- TermDocumentMatrix(docs) # It represents the words in the text as a table of numbers.
matrix <- as.matrix(dtm) # matrix representation of vectors with same data type.
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Visualizing through word cloud
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200, random.order=FALSE, 
          rot.per=0.35,colors=brewer.pal(8, "Dark2"))


# Booking patterns in the neighbourhood
neighbourhood <- air_bnb %>%
  group_by(neighbourhood) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Most preferred neighbourhood
preferred_neighbourhood <- neighbourhood %>%
  filter(Count >= 900)
# Visualizing the top 15 preferred neighbourhood
ggplot(preferred_neighbourhood, aes(x = reorder(neighbourhood, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'tomato3') +
  labs(title = "Booking patterns in the neighbourhood", subtitle = "Top 15 preferred Neighbourhood") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5), 
        axis.text.x = element_text(angle = 90)) + xlab("Neighbourhood")

# Understanding the most preferred room_type
preferred_room_type <- air_bnb %>%
  select(neighbourhood, room_type:availability_365)

room_analysis <- preferred_room_type %>%
  group_by(room_type) %>%
  summarise(count = n(), avg_price = mean(price), max_price = max(price), min_price = min(price))
ggplot(room_analysis, aes(x = reorder(room_type, -count), y = count)) +
  geom_bar(stat = 'identity', fill = "tomato3") +
  labs(title = "Most preferred room type") + xlab("room type")

# A look of the reviews for a particular room type
preferred_room_type %>%
  group_by(room_type) %>%
  summarise(count = n(), total_reviews = sum(number_of_reviews))

#-----------------------------------------------------------------------------
# What we can learn about different hosts and areas
preferred_room_type %>%
  group_by(neighbourhood) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  arrange(desc(total_hosts))

# For Manhattan neighbourhood_group
manhattan_neighbourhood <- air_bnb %>%
  select(neighbourhood_group:availability_365) %>%
  group_by(neighbourhood_group, neighbourhood, latitude, longitude, room_type) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  filter(neighbourhood_group == "Manhattan") %>%
  arrange(desc(total_hosts))

min(manhattan_neighbourhood$latitude)
max(manhattan_neighbourhood$latitude)
max(manhattan_neighbourhood$longitude)
min(manhattan_neighbourhood$longitude)

# Hosts in Manhattan neighbourhood_group
manhattan_neighbourhood %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = manhattan_neighbourhood %>% filter(total_hosts > 150), color="tomato3", aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  labs(title = "Hosts in Manhattan area", 
subtitle = "Areas with a minimum of one fifty hosts to a maximum of three hundred and twenty seven hosts")


# For Brooklyn neighbourhood_group
brooklyn_neighbourhood <- air_bnb %>%
  select(neighbourhood_group:availability_365) %>%
  group_by(neighbourhood_group, neighbourhood, latitude, longitude, room_type) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  filter(neighbourhood_group == "Brooklyn") %>%
  arrange(desc(total_hosts))

# Hosts in brooklyn neighbouhood_group
brooklyn_neighbourhood %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = brooklyn_neighbourhood %>% filter(total_hosts >= 15), color="tomato3", 
             aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  labs(title = "Hosts in Brooklyn area", 
       subtitle = "Areas with a minimum of fifteen hosts to a maximum of two hundred and thirty two hosts") 

# For Staten Island neighbourhood_group
staten_island_neighbourhood <- air_bnb %>%
  select(neighbourhood_group:availability_365) %>%
  group_by(neighbourhood_group, neighbourhood, latitude, longitude, room_type) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  filter(neighbourhood_group == "Staten Island") %>%
  arrange(desc(total_hosts))

# Hosts in staten island neighbourhood_group
staten_island_neighbourhood %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = staten_island_neighbourhood %>% filter(total_hosts >= 2), color="tomato3", aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  labs(title = "Hosts in staten island area", 
       subtitle = "Areas with a minimum of two hosts to a maximum of eight hosts") 


# For Queens neighbourhood_group
queens_neighbourhood <- air_bnb %>%
  select(neighbourhood_group:availability_365) %>%
  group_by(neighbourhood_group, neighbourhood, latitude, longitude, room_type) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  filter(neighbourhood_group == "Queens") %>%
  arrange(desc(total_hosts))

# Hosts in queens neighbourhood_group
queens_neighbourhood %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = queens_neighbourhood %>% filter(total_hosts >= 43), color="tomato3", aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  labs(title = "Hosts in queens area", 
       subtitle = "Areas with a minimum of fourty three hosts to a maximum of one hundred and three hosts")

# For Bronx neighbourhood_group
bronx_neighbourhood <- air_bnb %>%
  select(neighbourhood_group:availability_365) %>%
  group_by(neighbourhood_group, neighbourhood, latitude, longitude, room_type) %>%
  summarise(number_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            total_reviews = sum(number_of_reviews), avg_stay = mean(minimum_nights)) %>%
  filter(neighbourhood_group == "Bronx") %>%
  arrange(desc(total_hosts))

# Hosts in Bronx area
bronx_neighbourhood %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = bronx_neighbourhood %>% filter(total_hosts >= 5), color="tomato3", aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  labs(title = "Hosts in Bronx area", 
       subtitle = "Areas with a minimum of five hosts to a maximum of thirty seven hosts") 

# Hosts around New York city

new_york <- rbind(manhattan_neighbourhood, brooklyn_neighbourhood, staten_island_neighbourhood,
                  queens_neighbourhood, bronx_neighbourhood)


new_york %>%
  ggplot( aes(y = latitude, x = longitude)) +
  geom_point(color="grey", size=2) +
  geom_point(data = new_york %>% filter(neighbourhood_group == "Manhattan" & total_hosts > 150), 
             color="tomato3", alpha = .1, aes(size = total_hosts)) +
  geom_point(data = new_york %>% filter(neighbourhood_group == "Brooklyn" & total_hosts >= 15), 
             color= "blue", 
             aes(size = total_hosts)) +
  geom_point(data = new_york %>% filter(neighbourhood_group == "Staten Island" & total_hosts >= 2), 
             color="gray1", aes(size = total_hosts)) +
  geom_point(data = new_york %>% filter(neighbourhood_group == "Queens" & total_hosts >= 43), 
             color="orangered2", aes(size = total_hosts)) +
  geom_point(data = new_york %>% filter(neighbourhood_group == "Bronx" & total_hosts >= 5), 
             color="maroon", aes(size = total_hosts)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) 

#-------------------------------------------------------------------------
# Which hosts are busy & why they are busy
preferred_room_type %>%
  group_by(room_type) %>%
  summarise(no_of_rooms = n(), total_hosts = sum(calculated_host_listings_count),
            max_stay = (max(minimum_nights)), 
            avg_occupancy_rate = (sum(availability_365)/no_of_rooms)*100)

# Shared room analysis
share_room <- preferred_room_type %>%
  select(room_type, availability_365) %>%
  group_by(room_type) %>%
  filter(room_type == "Shared room" & availability_365 == 365) %>%
  summarise(Available_shared_room = n())

# Entire home/apt analysis
entire_home_apt <- preferred_room_type %>%
  select(room_type, availability_365) %>%
  group_by(room_type) %>%
  filter(room_type == "Entire home/apt" & availability_365 == 365) %>%
  summarise(Available_Entire_home_apt = n())

share_room <- rename(share_room, availability = Available_shared_room)
entire_home_apt <- rename(entire_home_apt, availability = Available_Entire_home_apt)
room_availability <- rbind(share_room, entire_home_apt)

ggplot(room_availability, aes(x = room_type, y = availability)) +
  geom_bar(stat = 'identity', color = 'black', fill = 'tomato3') +
  labs(title = "Number of vacant rooms throughout the year") 
# Hence hosts with shared room types are the busisest.













































































