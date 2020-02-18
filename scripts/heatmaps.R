library(tidyverse)

#read data
events <- read_csv(here::here("data/events.csv"))

#define circle function
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#create pitch
pen_spot_1 <- c(12,40)
pen_spot_2 <- c(108,40)
center <- c(60,40)
#### create leftD arc ####
Dleft <- circleFun(pen_spot_1, diameter = 20, 1000)
Dleft <- Dleft[which(Dleft$x > 18),]

points <- rbind(pen_spot_1, pen_spot_2, center)

#### create rightD arc ####
Dright <- circleFun(pen_spot_2, diameter = 20, 1000)
Dright <- Dright[which(Dright$x < 102),]

### create center ###
center_circle <- circleFun(center, diameter = 20, 1000)

#define function for plotting heatmaps
plot_heatmaps <- function(data, cluster_nb) {
  
  data <- data %>% filter(cluster == cluster_nb)
  
  ggplot(data, aes(x = location.x, y = location.y)) +
    geom_bin2d(binwidth = c(10,10)) +
    scale_fill_distiller(palette = "RdYlGn", guide = FALSE) +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
             fill = "transparent",
             color = "black") + #terrain
    annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
             fill = "transparent",
             color = "black") + #surface
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
             fill = "transparent",
             color = "black") + #6m
    annotate("rect", xmin = 0, xmax = -2, ymin = 36, ymax = 44,
             fill = "transparent",
             color = "black") + #but
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
             fill = "transparent",
             color = "black") + #surface
    annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
             fill = "transparent",
             color = "black") + #6m
    annotate("rect", xmin = 120, xmax = 122, ymin = 36, ymax = 44,
             fill = "transparent",
             color = "black") + #but
    annotate("segment", x = 60, xend = 60, y = 0, yend = 80) +
    annotate("path", x = Dleft$x, y = Dleft$y) + #Dleft
    annotate("path", x = Dright$x, y = Dright$y) + #Dright
    annotate("path", x = center_circle$x, y = center_circle$y) + #Center circle +
    annotate("point", x = points[,1], y = points[,2]) +
    coord_flip() +
    theme_void() +
    theme(aspect.ratio = 3/2)
}

#plot heatmaps
plots_heatmap <- 1:6 %>%
  map(~plot_heatmaps(events, .x))

#save plots
for (i in 1:length(plots_heatmap)) {
  ggsave(plots_heatmap[[i]], file = paste0(here::here("plots/heatmap/cluster_"), i, ".png"),
         width = 5)
}
