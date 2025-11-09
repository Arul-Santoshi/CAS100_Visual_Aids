library(tidyverse)
library(ggplot2)

# Read the data
wages <- read.csv("C:/Users/asant/OneDrive - The Pennsylvania State University/CAS100A/state_minimum_wages.csv")

# Create the plot
ggplot(wages, aes(x = Year, y = Minimum_Wage, color = State, size = State)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  
  # Highlight Pennsylvania with thicker line and different appearance
  scale_size_manual(values = c("PA" = 1.5, "NJ" = 0.8, "NY" = 0.8, "DE" = 0.8, "WV" = 0.8),
                    guide = "none") +
  
  scale_color_manual(values = c("PA" = "#E63946", "NJ" = "#457B9D", "NY" = "#1D3557", 
                                 "DE" = "#A8DADC", "WV" = "#808080"),
                     name = "State") +
  
  labs(
    title = "State Minimum Wage Trends (2013-2024)",
    subtitle = "Pennsylvania highlighted in red",
    x = "Year",
    y = "Minimum Wage ($)",
    caption = "Data source: U.S. Department of Labor\nNote: PA has remained at $7.25 since 2009"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray", size = 0.3),
    panel.grid.minor = element_blank()
  ) +
  
  scale_y_continuous(breaks = seq(7, 16, by = 1)) +
  scale_x_continuous(breaks = seq(2009, 2024, by = 2))

# Save the plot
ggsave("minimum_wage_trends.png", width = 12, height = 8, dpi = 300)
print("Plot saved as minimum_wage_trends.png")

