library(tidyverse)

library(ggtern)

#read data
profile <- read_csv(here::here("data/events_sum.csv"))
profile$cluster <- as.factor(profile$cluster)

#plot all clusters
all_clusters <- ggtern(data = profile, aes(x = prop.Pass, y = prop.Shot, z = prop.Dribble,
                                           fill = cluster)) +
  geom_point(size = 2,
             shape = 21) +
  tern_limits(T= 0.3, L = 1, R = 0.3) +
  labs(xarrow = "Passes",
       yarrow = "Tirs",
       zarrow = "Dribbles") +
  theme_bw() +
  theme_arrowdefault() +
  theme(tern.axis.title.show = FALSE) +
  scale_fill_brewer(palette = "Dark2")

all_clusters

#define function for making the cluster plots
plot_cluster <- function(data, cluster_nb) {
  
  data <- data %>% mutate(highlight = ifelse(cluster == cluster_nb, TRUE, FALSE))
  
  plot <- ggtern(data = data, aes(x = prop.Pass, y = prop.Shot, z = prop.Dribble,
                             fill = highlight)) +
    scale_fill_manual(values = c("grey70", RColorBrewer::brewer.pal(6, "Dark2")[[cluster_nb]]),
                      guide = FALSE) +
    geom_point(size = 2,
               shape = 21) +
    tern_limits(T= 0.3, L = 1, R = 0.3) +
    labs(xarrow = "Passes",
         yarrow = "Tirs",
         zarrow = "Dribbles",
         title = element_blank()) +
    theme_bw() +
    theme_arrowdefault() +
    theme(tern.axis.title.show = FALSE)
  
  return(plot)
}

#create the plot
plots_tern <- 1:6 %>%
  map(~plot_cluster(profile, .x))


#save the plots
for (i in 1:length(plots_tern)) {
  ggsave(plots_tern[[i]], file = paste0(here::here("plots/tern/cluster_"), i, ".png"),
         width = 7)
}

ggsave(all_clusters, file = here::here("plots/tern/cluster_all.png"),
       width = 7)


#info for slides
profile %>% select(prop.Pass,
                   prop.Shot,
                   prop.Dribble) %>%
  split(profile$ cluster) %>%
  map(summary)

profile$position.name_2 <- profile$position.name %>% fct_collapse(`Milieu central` = c("Left Center Midfield",
                                                                                       "Right Center Midfield",
                                                                                       "Center Midfield"),
                                                                  `Milieu offensif` = c("Center Attacking Midfield",
                                                                                        "Right Attacking Midfield",
                                                                                        "Left Attacking Midfield"),
                                                                  `Milieu défensif` = c("Center Defensive Midfield",
                                                                                        "Right Defensive Midfield",
                                                                                        "Left Defensive Midfield"),
                                                                  `Attaquant` = c("Center Forward",
                                                                                  "Right Center Forward",
                                                                                  "Left Center Forward"),
                                                                  `Ailier` = c("Right Wing",
                                                                               "Left Wing"),
                                                                  `Milieu latéral` = c("Right Midfield",
                                                                                       "Left Midfield"))

profile %>% select(position.name_2) %>%
  split(profile$cluster) %>%
  map(summary)


#plot teams

team <- "England"

ggtern(profile, aes(x = prop.Pass, y = prop.Shot, z = prop.Dribble)) +
  geom_point(size = 2,
             color = "grey70") +
  geom_point(data = profile %>% filter(team.name == team),
             aes(fill = cluster),
             shape = 21,
             size = 2) +
  geom_text(data = profile %>% filter(team.name == team),
            aes(label = player.name)) +
  tern_limits(T= 0.3, L = 1, R = 0.3) +
  labs(xarrow = "Passes",
       yarrow = "Tirs",
       zarrow = "Dribbles") +
  theme_bw() +
  theme_arrowdefault() +
  theme(tern.axis.title.show = FALSE) +
  scale_fill_brewer(palette = "Dark2")



ggtern(data = profile %>% filter(team.name == team), aes(x = prop.Pass, y = prop.Shot, z = prop.Dribble)) +
  geom_point(aes(fill = cluster),
             shape = 21,
             size = 2) +
  geom_text(aes(label = player.name)) +
  tern_limits(T= 0.3, L = 1, R = 0.3) +
  labs(xarrow = "Passes",
       yarrow = "Tirs",
       zarrow = "Dribbles") +
  theme_bw() +
  theme_arrowdefault() +
  theme(tern.axis.title.show = FALSE) +
  scale_fill_brewer(palette = "Dark2")
