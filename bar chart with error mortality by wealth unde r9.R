# Load necessary packages
library(ggplot2)
library(dplyr)

# Create the dataframe
data <- data.frame(
  R = c(38.85, 39.23, 78.08, 102.7, 172.76, 40.99, 35.95, 76.94, 99.3, 168.6,
        41.49, 27.2, 68.68, 64.21, 128.48, 31.93, 21.38, 53.31, 34.69, 86.15,
        27.7, 11.93, 39.62, 14.32, 53.38),
  N = c(16190, 15673, 16290, 14180, 15272, 15088, 14434, 15030, 12976, 14108,
        13929, 13330, 13845, 12044, 12950, 11870, 11408, 11758, 10377, 11026,
        9084, 8719, 8986, 7864, 8293),
  WN = c(15275, 14781, 15373, 13397, 14434, 15030, 14341, 14980, 12843, 13993,
         13558, 12922, 13459, 11650, 12553, 12031, 11558, 11919, 10503, 11163,
         10423, 10015, 10298, 8985, 9454),
  Period = rep("0-9", 25),
  Class = c("poorest", "poorest", "poorest", "poorest", "poorest", 
            "poorer", "poorer", "poorer", "poorer", "poorer", 
            "middle", "middle", "middle", "middle", "middle", 
            "richer", "richer", "richer", "richer", "richer", 
            "richest", "richest", "richest", "richest", "richest")
)

# Convert Class to a factor with specified levels
data$Class <- factor(data$Class, levels = c("poorest", "poorer", "middle", "richer", "richest"))

# Calculate mean and standard error for R by Class
summary_data <- data %>%
  group_by(Class) %>%
  summarise(
    mean_R = mean(R),
    se_R = sd(R) / sqrt(n()),  # Standard error
    .groups = 'drop'
  )
# Create the bar chart with error bars
ggplot(summary_data, aes(x = Class, y = mean_R)) +
  geom_col(fill = "blue") +  # Bar for mean
  geom_errorbar(aes(ymin = mean_R - se_R, ymax = mean_R + se_R), width = 0.2) +  # Error bars
  labs(title = "Child Mortality Rates by Wealth Class 95 CI",
       x = "Wealth Class",
       y = "Mean Child Mortality Rate per 1000") +
  )




