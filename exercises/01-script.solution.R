library(readr)
library(ggplot2)
library(rlang)

# Identify the file we're going to load, relative to project root
filepath <- here::here("exercises/safety_data.csv")

# Load CSV data
safety <- read_csv(filepath, comment = "#")

# Identify a specific column to examine
column <- "Cyp11a1"

# Print a basic summary
summary(safety[[column]])

# Plot a histogram of the column in question
ggplot(safety, aes(!!sym(column), fill = Class)) +
  geom_histogram(bins = 30) +
  facet_grid(Class ~ .)

# Choose a second column to plot against the first
column2 <- "Star"

# Plot a scatter plot of column vs. column2
ggplot(safety, aes(!!sym(column), !!sym(column2), color = Class)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(se = FALSE)

# Calculate correlation
cor(safety[[column]], safety[[column2]], use = "complete.obs")
