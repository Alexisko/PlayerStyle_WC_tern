library(tidyverse)
library(StatsBombR)
library(here)

#get matches dataframe
matches <- FreeCompetitions() %>%
  filter(competition_id == 43) %>%
  FreeMatches()

#read and clean all the wc data
wc_data <- StatsBombFreeEvents(matches)
wc_data <- wc_data %>% allclean()

#select interesting columns : player, event type and location and filter for events we want
events <- wc_data %>% select(player.name, type.name, location.x, location.y) %>%
  filter(type.name %in% c("Pass", "Shot", "Dribble"))

#we need to keep the event dataframe for heatmap and create a summary for tern plots
events_sum <- events %>% group_by(player.name) %>%
  mutate(number = n()) %>%
  filter(number >= 40) %>%
  group_by(player.name, number, type.name) %>%
  summarise(nb = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = type.name, values_from = nb) %>%
  replace_na(list(Dribble = 0, Shot = 0, Pass = 0)) %>%
  mutate(prop.Shot = Shot/number,
         prop.Pass = Pass/number,
         prop.Dribble = Dribble/number)

#create player profile for filtering defenders and gk
profile <- wc_data %>% select(player.name, position.name, team.name) %>%
  group_by(player.name, team.name, position.name) %>%
  summarise(n = n()) %>%
  filter(n == max(n)) %>%
  select(!n)


#join profile and events_sum
events_sum <- events_sum %>% left_join(profile)


#filter by position
events_sum <- events_sum %>% filter(! position.name %in% c("Goalkeeper",
                                                           "Center Back",
                                                           "Left Back",
                                                           "Left Center Back",
                                                           "Left Wing Back",
                                                           "Right Back",
                                                           "Right Center Back",
                                                           "Right Wing Back"))

#Finding the best number of clusters
library(NbClust)
nb <- NbClust(events_sum %>% select(prop.Dribble, prop.Pass, prop.Shot),
              method = "kmeans")

#Calculate cluster and join
clusters <- events_sum[6:8] %>% kmeans(6)
events_sum$clusters <- clusters[[1]]

#to set cluster numbers to not change when we re-run code we arrange them by descending proportion of pass
set_cluster_nb <- events_sum %>% group_by(clusters) %>% 
  summarise(prop.Pass = mean(prop.Pass)) %>% 
  mutate(rk = rank(-prop.Pass)) %>%
  select(-prop.Pass)

events_sum <- events_sum %>% left_join(set_cluster_nb) %>%
  select(-clusters) %>%
  rename("cluster"  = rk)


write.csv(events_sum, here("data/events_sum.csv"))


#we join cluster to events for heatmap
events <- events %>% left_join(events_sum %>% select(player.name, cluster)) %>% 
  filter(!is.na(cluster))

write.csv(events, here("data/events.csv"))
