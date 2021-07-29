#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Load packages
library(ggplot2)
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(41)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Generate some data, following general conventions for the plot
data <-
  data.frame(
    days = rep(-12:132, 3),
    group = as.factor(rep(
    1:3, each = 145)),
    stringsAsFactors = F
  ) %>%
  mutate(change = ifelse(group == 1, runif(345,-0.1,0.1),1),
         change = ifelse(group == 2, runif(345,-0.3,+0.3),change),
         change = ifelse(group == 3, runif(345,4,8), change),
         change = ifelse(group == 3 & days %in% 15:60, runif(345,7,9),change)) %>%

  mutate(high = ifelse(group == 1, change+0.1,1),
         high = ifelse(group == 2, change+0.2,high),
         high = ifelse(group == 3, change+1, high)) %>%

  mutate(low = ifelse(group == 1, change-0.1,1),
         low = ifelse(group == 2, change-0.2,low),
         low = ifelse(group == 3, change-1, low))

p <- ggplot(data, aes(x = days, y = change)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymax = high,ymin = low, fill = group), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(
    limits = c(-13, 140),
    breaks = c(seq(-10, 130, by = 10), 134),
    labels = c("", seq(0, 130, by = 10), ""),
    name = "Days since first symptoms",
    expand = expansion(mult = c(0.005, .05))
  ) +
  scale_y_continuous(
    limits = c(-4, 13),
    breaks = seq(-4, 12, by = 2),
    labels = stringr::str_pad(seq(-4, 12, by = 2),2,"left"),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(title = "Change in resting heart rate, beats per minute") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_blank(),
    # Sort over-arching thematic decisions
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.border = element_blank(),

    # Remove y-axis tick marks and move y-axis text to be over plot
    axis.ticks.length.y.right = unit(0, "cm"),
    axis.text.y = element_text(vjust = -.6),
    axis.text.y.right = element_text(margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = -8
    ), colour = "black"),
    axis.line.x.bottom = element_line(colour = "black"),

    legend.position = "hide",

    plot.title = element_text(size =10, face = "bold")
  ) +

# Save plot to file
ggsave("2021_07_17/2021_07_17_Heart_Covid_Attempt.png",
       width = 8,
       height = 8,
       units = "in")
