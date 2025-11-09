library(tidyverse)
library(ggplot2)

# Read the data
wages <- read.csv("C:/Users/asant/OneDrive - The Pennsylvania State University/CAS100A/state_minimum_wages.csv")

wages_2013_plus <- wages %>%
  filter(Year >= 2013)

# Calculate average rate of change for each state (2013-2024)
rate_of_change <- wages_2013_plus %>%
  group_by(State) %>%
  summarise(
    min_year = min(Year),
    max_year = max(Year),
    min_wage_start = first(Minimum_Wage),
    min_wage_end = last(Minimum_Wage),
    years = max_year - min_year,
    avg_annual_change = (min_wage_end - min_wage_start) / years
  )

print("Average Annual Rate of Change (2013-2024):")
print(rate_of_change)

# Create data for 2025-2028 predictions using the average rate of change
future_years <- data.frame(Year = 2025:2028)

predictions <- future_years %>%
  crossing(rate_of_change %>% select(State, avg_annual_change, min_wage_end)) %>%
  mutate(
    years_from_2024 = Year - 2024,
    Minimum_Wage = min_wage_end + (avg_annual_change * years_from_2024)
  ) %>%
  select(State, Year, Minimum_Wage)

print("\nPredicted Minimum Wages (2025-2028):")
print(predictions)

# Create Pennsylvania proposal data for 2025-2027
pa_proposal <- data.frame(
  State = "PA",
  Year = c(2025, 2026, 2027),
  Minimum_Wage = c(12, 13.5, 15)
)

# Get historical data from 2018 onwards
historical_data <- wages %>%
  filter(Year >= 2018, State %in% c("NJ", "NY", "DE", "WV", "PA")) %>%
  mutate(Type = "Historical", Linetype = "solid")

# Add projections for other states
projections_other <- predictions %>%
  filter(State %in% c("NJ", "NY", "DE", "WV"), Year <= 2027) %>%
  mutate(Type = "Projected", Linetype = "dashed")

# Create connector rows for 2024 to connect historical to projections
connector_2024 <- wages %>%
  filter(Year == 2024, State %in% c("NJ", "NY", "DE", "WV")) %>%
  mutate(Type = "Projected", Linetype = "dashed")

# Add PA proposal (only through 2027)
pa_proposal_with_type <- pa_proposal %>%
  mutate(Type = "Proposed", Linetype = "dashed")

# Create connector row for PA 2024 to connect historical to proposal
pa_connector_2024 <- wages %>%
  filter(Year == 2024, State == "PA") %>%
  mutate(Type = "Proposed", Linetype = "dashed")

# Combine all data
plot_data <- bind_rows(
  historical_data,
  connector_2024,
  projections_other,
  pa_connector_2024,
  pa_proposal_with_type
)

# Create the comprehensive plot
ggplot(plot_data, aes(x = Year, y = Minimum_Wage, color = State, linetype = Linetype)) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(shape = State), size = 2.5) +
  
  scale_shape_manual(
    values = c("PA" = 17, "NJ" = 16, "NY" = 16, "DE" = 16, "WV" = 16),
    guide = "none"
  ) +
  
  scale_linetype_manual(
    values = c("solid" = "solid", "dashed" = "dashed"),
    labels = c("solid" = "Historical", "dashed" = "Projection/Proposal"),
    name = "Data Type"
  ) +
  
  scale_color_manual(
    values = c(
      "PA" = "#E63946",
      "NJ" = "#457B9D",
      "NY" = "#1D3557",
      "DE" = "#A8DADC",
      "WV" = "#808080"
    ),
    name = "State"
  ) +
  
  labs(
    title = "State Minimum Wage: Historical Data and Projections (2018-2027)",
    subtitle = "Solid lines = Historical data | Dashed lines = Projections/Proposal\nPA proposal: $12 (2025) → $13.50 (2026) → $15 (2027)",
    x = "Year",
    y = "Minimum Wage ($)",
    caption = "Data source: U.S. Department of Labor\nProjections based on average annual rate of change (2013-2024)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  
  scale_y_continuous(
    limits = c(7, 18),
    breaks = seq(7, 18, by = 1),
    labels = scales::dollar_format(prefix = "$")
  ) +
  scale_x_continuous(breaks = 2018:2027, limits = c(2018, 2027))

# Save the plot
print("\nPlot saved as minimum_wage_projections.png")

