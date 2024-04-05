install.packages("ggplot2")

library(ggplot2)

earthquake <- read.table(file = "earthquake.txt", header = TRUE, sep = "", dec = ".")


# Visualizing the data
ggplot(earthquake, aes(x=body, y=surface, color=type, shape=type)) +
  geom_point(size=3) +
  theme_minimal() +
  labs(title = "Body-wave Magnitude vs. Surface-wave Magnitude",
       x = "Body-wave Magnitude (mb)",
       y = "Surface-wave Magnitude (Ms)",
       color = "Type",
       shape = "Type") +
  scale_color_manual(values = c("equake" = "blue", "explosn" = "red")) +
  geom_smooth(method = "lm", se = FALSE, aes(group=type), color="black") # Optional: Add linear regression lines
