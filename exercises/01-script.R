# 
filepath <- here::here("exercises/IC50_data.csv")

# 
assays <- read.csv(filepath, stringsAsFactors = FALSE)

# 
target <- "A"

# 
assays_target <- assays[assays$Target == target,]

# 
model <- lm(Dose_Response ~ Single_Point, assays_target)

# 
plot(model, which = 2)
