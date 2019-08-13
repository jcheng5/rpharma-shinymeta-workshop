library(readr)
library(ggplot2)
library(rlang)

# 
filepath <- here::here("exercises/safety_data.csv")

# 
safety <- read_csv(filepath, comment = "#")

# 
column <- "Cyp11a1"

# 
summary(safety[[column]])

# 
ggplot(safety, aes(!!sym(column), fill = Class)) +
  geom_histogram(bins = 30) +
  facet_grid(Class ~ .)

# 
column2 <- "Star"

# 
ggplot(safety, aes(!!sym(column), !!sym(column2), color = Class)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(se = FALSE)

# 
cor(safety[[column]], safety[[column2]], use = "complete.obs")
