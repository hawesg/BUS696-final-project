### Probably irrelevant because we are looking to model
#seq(0, 3000, by = 25)
# stats <-
#   wine_data %>% group_by(pr = cut(
#     price,
#     breaks = c(0, 10, 25, 50, 100, 200, 500, 1000, 3500),
#     dig.lab = 5
#   )) %>% summarize(
#     count = n(),
#     min = min(points),
#     max = max(points),
#     avg = mean(points),
#     sd = sd(points)
#   )
# stats
# 
# 
#install.packages("summarytools")
library(summarytools)
summarytools::descr(wine_data_clean)
# Descriptive Statistics  
# wine_data_clean  
# N: 91500  
# 
# ID     points      price   taster_avg_points   taster_review_count
# ----------------- ---------- ---------- ---------- ------------------- ---------------------
# Mean   45750.50      88.61      35.29               88.61              11256.58
# Std.Dev   26413.92       2.97      43.29                0.95               5946.77
# Min       1.00      80.00       4.00               86.61                  6.00
# Q1   22875.50      87.00      17.00               88.54               6237.00
# Median   45750.50      88.00      25.00               88.63               9507.00
# Q3   68625.50      91.00      42.00               89.09              14944.00
# Max   91500.00     100.00    3300.00               90.61              20172.00
# MAD   33914.47       2.97      14.83                0.68               7911.15
# IQR   45749.50       4.00      25.00                0.55               8707.00
# CV       0.58       0.03       1.23                0.01                  0.53
# Skewness       0.00      -0.01      19.23               -0.45                  0.24
# SE.Skewness       0.01       0.01       0.01                0.01                  0.01
# Kurtosis      -1.20      -0.24     872.76                0.04                 -1.14
# N.Valid   91500.00   91500.00   91500.00            91500.00              91500.00
# Pct.Valid     100.00     100.00     100.00              100.00                100.00
# 
# Table: Table continues below
# 
# 
# 
# title_length
# ----------------- --------------
#   Mean          52.75
# Std.Dev          13.74
# Min          12.00
# Q1          43.00
# Median          52.00
# Q3          61.00
# Max         136.00
# MAD          13.34
# IQR          18.00
# CV           0.26
# Skewness           0.54
# SE.Skewness           0.01
# Kurtosis           0.56
# N.Valid       91500.00
# Pct.Valid         100.00



summarytools::descr(wine_data_clean, transpose = TRUE)

dfSummary(wine_data_clean)
