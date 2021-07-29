#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(packcircles)


# Set seed for reproducibility
set.seed(41)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Generate some data, following general conventions for the plot
data <-
  data.frame(x = rep(1:20,20:1),
             y = rep(sample(1:105,105,F),2),
             size = runif(210,0.01,10),
    stringsAsFactors = F)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Start plot
ggplot(data, aes(x=x, y=y)) +
  geom_point(color = "black", shape = 1, fill = "white") +
  scale_size_continuous(range = c(.1, 10), name="")

# Save plot to file
ggsave("2021_06_05/2021_06_05_Algorithmic_Discrimination_Attempt.png",
       width = 8,
       height = 8,
       units = "in")
