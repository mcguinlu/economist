#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Load packages
library(ggplot2)
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(41)

# Load helper function to create nice dates
# Taken from
# https://stackoverflow.com/questions/40039903/r-add-th-rd-and-nd-to-dates

append_date_suffix <- function(dates) {
  dayy <- day(dates)
  suff <- case_when(
    dayy %in% c(11, 12, 13) ~ "th",
    dayy %% 10 == 1 ~ 'st',
    dayy %% 10 == 2 ~ 'nd',
    dayy %% 10 == 3 ~ 'rd',
    TRUE ~ "th"
  )

  dayysuff <- paste0(dayy, suff)

  # Add case so that first date in each month contains month
  # Note this means the function is widely reusable
  case_when(
    dayysuff == "12th" ~ "12th\nOctober",
    dayysuff == "2nd" ~ "2nd\nNovember",
    TRUE ~ dayysuff
  )
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Generate some data, following general conventions for the plot
data <-
  data.frame(
    date = as.Date(rep(seq(
      ymd('2021-10-12'), ymd('2021-11-30'), by = 'weeks'
    ), 5)),
    count = runif(40, min = -1.5, max = 1.5),
    group = as.factor(rep(
      c("1st (bottom 20%)", "2nd", "3rd", "4th", "5th (top 20%)"), each = 8
    )),
    stringsAsFactors = F
  ) %>%
  mutate(
    count = case_when(
      date == as.Date("2021-11-23") & group == "5th (top 20%)" ~ 4.2,
      date == as.Date("2021-11-30") &
        group == "5th (top 20%)" ~ 3.75,
      date == as.Date("2021-11-23") &
        group == "2nd" ~ -3.1,
      date == as.Date("2021-11-30") &
        group == "2nd" ~ -3.7,
      date == as.Date("2021-11-23") &
        group == "1st (bottom 20%)" ~ -1.2,
      date == as.Date("2021-11-30") &
        group == "1st (bottom 20%)" ~ 0.1,
      T ~ count
    )
  )

# Combine with fixed dataset so all groups have count = 0 on Nov 3rd
data <- rbind(data,
              data.frame(
                date = as.Date(rep("2021-11-03", 5)),
                count = rep(0, 5),
                group = as.factor(c(
                  "1st (bottom 20%)", "2nd", "3rd", "4th", "5th (top 20%)"
                )),
                stringsAsFactors = F
              ))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Start plot
p <- ggplot() +

  # Add line plot
  geom_line(data = data,
            aes(x = date, y = count, color = group),
            size = 1) +

  # Add cross-hairs at 3rd Nov and 0
  # Note for vertical line,  allow for space for "Election day" at top
  geom_line(aes(y = c(-4, 4.2), x = as.Date(c(
    "2021-11-03", "2021-11-03"
  )))) +
  geom_hline(yintercept = 0) +

  # Add center point, with white outline
  geom_point(aes(x = as.Date("2021-11-03"), y = 0), size = 3, color = "black") +
  geom_point(
    aes(x = as.Date("2021-11-03"), y = 0),
    size = 3,
    shape = 1,
    color = "white"
  ) +

  # Add top of plot annotations
  geom_text(aes(
    y = 4.5,
    x = as.Date("2021-11-03"),
    label = "Election day"
  ),
  size = 3,
  fontface = 2) +
  geom_text(
    aes(
      y = 4.5,
      x = as.Date("2021-11-30"),
      label = "Increased rate of covid-19 cases \u2191"
    ),
    size = 3,
    hjust = .8
  ) +

  # Add Lowest annotation
  geom_label(
    aes(
      y = -2.1,
      x = as.Date("2021-11-05"),
      label = "Counties with the lowest\nin-person voter turnout"
    ),
    size = 3,
    hjust = 0,
    fontface = 2,
    color = "#a0522d",
    fill = "white",
    label.size = NA
  ) +
  geom_label(
    aes(
      y = -2.5,
      x = as.Date("2021-11-05"),
      label = "Eg, Philadelphia, PA and\nMiami-Dade, FL"
    ),
    size = 3,
    hjust = 0,
    color = "#a0522d",
    fill = "white",
    label.size = NA
  ) +
  geom_curve(
    data = data.frame(
      x = as.Date("2021-11-14"),
      y = -1.9,
      xend = as.Date("2021-11-22"),
      yend = -.8
    ),
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    angle = 90L,
    colour = "#a0522d",
    curvature = -0.4,
    arrow = NULL,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +

  # Add Highest annotation
  geom_label(
    aes(
      y = 3.5,
      x = as.Date("2021-11-05"),
      label = "Counties with the highest\nin-person voter turnout"
    ),
    size = 3,
    hjust = 0,
    fontface = 2,
    color = "#c10534",
    fill = "white",
    label.size = NA
  ) +
  geom_label(
    aes(
      y = 3.1,
      x = as.Date("2021-11-05"),
      label = "Eg, Sumter, FL (The Village retirment\ncommunity) and Navajo, AZ"
    ),
    size = 3,
    hjust = 0,
    color = "#c10534",
    fill = "white",
    label.size = NA
  ) +
  geom_curve(
    data = data.frame(
      x = as.Date("2021-11-17"),
      y = 3,
      xend = as.Date("2021-11-21"),
      yend = 2.8
    ),
    mapping = aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    angle = 90L,
    colour = "#c10534",
    curvature = 0.26,
    arrow = NULL,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +

  # Deal with scales
  scale_y_continuous(
    limits = c(-4, 5),
    breaks = -4:4,
    labels = c("-4", "-3", "-2", "-1", " 0", " 1", " 2", " 3", " 4"),
    # Need this as otherwise the ones without a minus are misaligned
    minor_breaks = NULL,
    position = "right",
    expand = c(0, 0)
  ) +
  scale_x_date(
    limits = as.Date(c("2021-10-12", "2021-11-30")),
    breaks = seq(ymd('2021-10-12'), ymd('2021-11-30'), by = 'weeks'),
    labels = append_date_suffix(seq(
      ymd('2021-10-12'), ymd('2021-11-30'), by = 'weeks'
    )),
    # Get expansion of x-axis correct (small bit of space on left side/lots on right)
    expand = expansion(mult = c(0.005, .05))
  ) +
  scale_color_manual(values = rev(c(
    "#c10534", "#e37e00", "#7b92a8", "#ffd200", "#a0522d" # Taken from Economist theme from {ggthemes}
  )),
  guide = guide_legend(reverse = TRUE),
  name = "Population-based quintiles\nof in-person voting rates") +

  labs(title = "Change in county’s covid-19 cases per 100,000 people compared with election day 2020",
       subtitle = "By share of population voting in person on election day, in 20 states with available data, seven-day moving average") +

  # Add theme, starting with simple black and white theme
  theme_bw() +
  theme(
    # Sort over-arching thematic decisions
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.border = element_blank(),

    #Align x-axis text
    axis.text.x = element_text(hjust = 0),

    # Remove y-axis tick marks and move y-axis text to be over plot
    axis.ticks.length.y.right = unit(0, "cm"),
    axis.text.y = element_text(vjust = -1),
    axis.text.y.right = element_text(margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = -8
    ), colour = "black"),
    axis.line.x.bottom = element_line(colour = "black"),

    # Remove both axis titles
    axis.title = element_blank(),

    # Fix title sizing
    plot.title = element_text(size =10, face = "bold"),
    plot.subtitle = element_text(size =10),


    # Style and position legende
    legend.position = c(.25, .75),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10)
  ) +

  # Save plot to file
  ggsave("2021_07_10/2021_07_10_Elections_Pandemic_Attempt.png",
         width = 8,
         height = 8,
         units = "in")
