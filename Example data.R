# ==========================================
# Title: Kenya Maternal Health Analysis
# Goal: Showcase Reproducibility & Collaboration
# ==========================================

# 1. SETUP: Create necessary project folders
# This ensures the script never crashes due to missing directories
folders <- c("data", "scripts", "output")
for (f in folders) {
  if (!dir.exists(f)) {
    dir.create(f)
  }
}

# 2. LOAD LIBRARIES
# If these aren't installed, use: install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# 3. DATA SIMULATION (Reproducibility Step)
set.seed(254) # Ensuring the "random" data is the same for everyone

counties <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Kiambu", 
              "Uasin Gishu", "Turkana", "Mandera", "Machakos", "Kilifi")

kenya_health_data <- data.frame(
  county = rep(counties, each = 5),
  year = rep(2019:2023, times = 10),
  skilled_attendants_pct = runif(50, min = 40, max = 95),
  mmr = rnorm(50, mean = 400, sd = 50) 
)

# Apply a trend: as attendance increases, mortality decreases
kenya_health_data$mmr <- kenya_health_data$mmr - (kenya_health_data$skilled_attendants_pct * 2.5)

# Save the raw data
write.csv(kenya_health_data, "data/kenya_maternal_health.csv", row.names = FALSE)

View(kenya_health_data)






# 4. ANALYSIS & VISUALIZATION

# Plot A: County-wise Distribution (Boxplot)
county_dist_plot <- ggplot(kenya_health_data, aes(x = reorder(county, mmr), y = mmr, fill = county)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(title = "Maternal Mortality Distribution by County (2019-2023)",
       subtitle = "Comparing simulated healthcare outcomes across counties",
       x = "County", y = "MMR (per 100,000)") +
  theme_minimal() + 
  theme(legend.position = "none")

# Plot B: Time-Series Trend (Line graph with fixed linewidth)
trend_plot <- kenya_health_data %>%
  group_by(year) %>%
  summarise(avg_mmr = mean(mmr)) %>%
  ggplot(aes(x = year, y = avg_mmr)) +
  geom_line(linewidth = 1.2, color = "#008080") + # Corrected aesthetic
  geom_point(size = 3, color = "#008080") +
  labs(title = "National Average Maternal Mortality Trend",
       subtitle = "Simulated annual progress (2019-2023)",
       x = "Year", y = "Average MMR") +
  theme_light()

# 5. PREDICTION MODELING
# Simple linear regression: MMR explained by skilled attendance
model <- lm(mmr ~ skilled_attendants_pct, data = kenya_health_data)

# Predict MMR for 100% skilled attendance coverage
future_scenario <- data.frame(skilled_attendants_pct = 100)
predicted_val <- predict(model, newdata = future_scenario)

# Print results to Console
cat("\n--- Prediction Analysis ---\n")
print(paste("Predicted MMR if Skilled Attendance is 100%:", round(predicted_val, 2)))

# 6. EXPORTING OUTPUTS
ggsave("output/county_distribution.png", plot = county_dist_plot, width = 8, height = 6)
ggsave("output/national_trend.png", plot = trend_plot, width = 8, height = 6)


print("Analysis complete. All files saved to 'data/' and 'output/' folders.")
# 7. RISK CATEGORIZATION (Feature Work)
# We define 'High Risk' as any county with MMR above the national median

national_median <- median(kenya_health_data$mmr)

kenya_health_data <- kenya_health_data %>%
  mutate(risk_level = ifelse(mmr > national_median, "High Risk", "Standard Risk"))

# Plot C: Density Plot of MMR
# This shows the "shape" of health outcomes in the country
density_plot <- ggplot(kenya_health_data, aes(x = mmr, fill = risk_level)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = national_median), linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Maternal Mortality Risk",
       subtitle = paste("Dashed line represents the National Median:", round(national_median, 2)),
       x = "Maternal Mortality Rate", y = "Density") +
  scale_fill_manual(values = c("High Risk" = "#d95f02", "Standard Risk" = "#1b9e77")) +
  theme_minimal()

# Save this new specific analysis
ggsave("output/risk_distribution.png", plot = density_plot)

# 8. SUMMARY TABLE (For the presentation)
risk_summary <- kenya_health_data %>%
  group_by(risk_level) %>%
  summarise(
    avg_skilled_attendance = mean(skilled_attendants_pct),
    count = n()
  )

print(risk_summary)

