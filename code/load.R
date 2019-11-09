################### LOAD DATA ###################
wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here("data", "input", "wine-colors.csv"))
dim(wine_data)

# Add color
wine_data_with_color <- merge(wine_data, colors, by = "variety")

nrow(wine_data)
#[1] 129833
nrow(wine_data_with_color)
#[1] 126407
nrow(wine_data) - nrow(wine_data_with_color)
# 3426 records missing

# List of records i am not getting merged by ID
setdiff(wine_data$X, wine_data_with_color$X)




### Probably irrelevant because we are looking to model
#seq(0, 3000, by = 25)
stats <-
  wine_data %>% group_by(pr = cut(
    price,
    breaks = c(0, 10, 25, 50, 100, 200, 500, 1000, 3500),
    dig.lab = 5
  )) %>% summarize(
    count = n(),
    min = min(points),
    max = max(points),
    avg = mean(points),
    sd = sd(points)
  )
stats
