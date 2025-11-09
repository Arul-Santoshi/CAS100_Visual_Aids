# Union Membership Percentage Plot
# This script creates a plot showing union membership as a percentage of total workers
# Data source: Current Population Survey (1983-2024)

library(ggplot2)

data <- read.csv("C:/Users/asant/OneDrive - The Pennsylvania State University/CAS100A/union_data.csv")

# ============================================================================
# Create the union membership percentage plot
# ============================================================================

p <- ggplot(data, aes(x = Year, y = Union_Percentage)) +
  geom_line(color = "#A23B72", linewidth = 1.3) +
  geom_point(color = "#A23B72", size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#E63946", linetype = "dashed", 
              alpha = 0.3, se = FALSE) +
  labs(title = "Union Membership as Percentage of Total Workers (1983-2024)",
       subtitle = "Over 50% Decrease in Union membership",
       x = "Year",
       y = "Union Members (% of Total Workers)",
       caption = "Source: US Bureau of Labor Statistics") +
  scale_y_continuous(limits = c(10, 22)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        panel.grid.major = element_line(color = "gray90"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)

