incident_count <- df %>%
  group_by(Season) %>%
  summarise(incident_count = n())

ggplot(incident_count, aes(x = forcats::fct_reorder(Season, -incident_count), y = incident_count, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Number of Incidents",
    title = "Number of Incidents by Season",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

table(df$Year)


#Is there significant association between the season and the number of shooting incidents
df$Season <- factor(df$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Create a contingency table
contingency_table <- table(df$Season)

# Perform the chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Print the result
print(chi_squared_result)

#reject null based on chi squared


# Convert the 'Season' column to a factor
df$Season <- factor(df$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Create a contingency table
contingency_table <- table(df$Season)

# Extract observed counts
observed_counts <- contingency_table

# Perform pairwise comparisons
for (i in 1:(length(observed_counts) - 1)) {
  for (j in (i + 1):length(observed_counts)) {
    pair_data <- df[df$Season %in% levels(df$Season)[c(i, j)], ]
    pair_contingency_table <- table(pair_data$Season)
    
    # Perform chi-squared test
    chi_squared_result <- chisq.test(pair_contingency_table)
    
    # Print the result
    cat("Comparison between", names(observed_counts)[i], "and", names(observed_counts)[j], ":\n")
    print(chi_squared_result)
    cat("\n")
  }
}


# Convert the 'Season' column to a factor
df$Season <- factor(df$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Load the dplyr package
library(dplyr)

# Group by Year and Season, and summarize the total number of shootings
seasonal_summary <- df %>%
  group_by(Year, Season) %>%
  summarize(Total_Shootings = n())

write.csv(seasonal_summary, "seasonal_summary.csv", row.names = FALSE)

# Calculate the mean number of shootings per season
mean_shootings_per_season <- seasonal_summary %>%
  group_by(Season) %>%
  summarize(Mean_Shootings = mean(Total_Shootings))

# Print the result
print(mean_shootings_per_season)

library(ggplot2)

# Reorder the Season factor based on Mean_Shootings
mean_shootings_per_season$Season <- forcats::fct_reorder(mean_shootings_per_season$Season, -mean_shootings_per_season$Mean_Shootings)

# Create a bar plot
ggplot(mean_shootings_per_season, aes(x = Season, y = Mean_Shootings, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Mean Number of Shootings",
    title = "Mean Shootings by Season",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

# Create a grouped bar plot
ggplot(seasonal_summary, aes(x = Year, y = Total_Shootings, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Year",
    y = "Number of Incidents",
    title = "Number of Incidents by Season and Year",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Create a faceted line plot
ggplot(seasonal_summary, aes(x = Year, y = Total_Shootings, group = Season, color = Season)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Number of Incidents",
    title = "Number of Incidents by Season and Year",
    color = "Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~Season, scales = "free_y", ncol = 2)

# Create a faceted grouped bar plot
ggplot(seasonal_summary, aes(x = Year, y = Total_Shootings, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Number of Incidents",
    title = "Number of Incidents by Season and Year",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~Season, scales = "free_y", ncol = 2)



# Calculate the percentage change relative to the first year for each season
seasonal_summary2 <- seasonal_summary %>%
  arrange(Season, Year) %>%
  group_by(Season) %>%
  mutate(PercentageChange = (Total_Shootings / first(Total_Shootings) - 1) * 100)

# Create a line plot with points for the percentage change
ggplot(seasonal_summary2, aes(x = Year, y = PercentageChange, group = Season, color = Season, linetype = Season)) +
  geom_line() +
  geom_point(size = 2, alpha = 0.7) +  # Add points for better visibility
  labs(
    x = "Year",
    y = "Percentage Change in Number of Incidents",
    title = "Percentage Change in Number of Incidents by Season and Year"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"))



# Calculate the percentage change relative to the previous year for each season
seasonal_summary2 <- seasonal_summary %>%
  arrange(Season, Year) %>%
  group_by(Season) %>%
  mutate(PercentageChange = (Total_Shootings / lag(Total_Shootings) - 1) * 100)

# Create a line plot with points for the percentage change
ggplot(seasonal_summary2, aes(x = Year, y = PercentageChange, group = Season, color = Season, linetype = Season)) +
  geom_line() +
  geom_point(size = 2, alpha = 0.7) +  # Add points for better visibility
  labs(
    x = "Year",
    y = "Percentage Change in Number of Incidents",
    title = "Percentage Change in Number of Incidents by Season and Year"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"))

# Calculate the percentage change relative to the previous year for each season
seasonal_summary2 <- seasonal_summary %>%
  arrange(Season, Year) %>%
  group_by(Season) %>%
  mutate(PercentageChange = (Total_Shootings / lag(Total_Shootings) - 1) * 100)

# Create a bar plot with labels
ggplot(seasonal_summary2, aes(x = Year, y = PercentageChange, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", PercentageChange)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(
    x = "Year",
    y = "Percentage Change in Number of Incidents",
    title = "Percentage Change in Number of Incidents by Season and Year"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1")

# Calculate the percentage change relative to the previous year for each season
seasonal_summary2 <- seasonal_summary %>%
  arrange(Season, Year) %>%
  group_by(Season) %>%
  mutate(PercentageChange = (Total_Shootings / lag(Total_Shootings) - 1) * 100)

# Create a line plot with labels
ggplot(seasonal_summary2, aes(x = Year, y = PercentageChange, group = Season, color = Season)) +
  geom_line() +
  geom_text(aes(label = sprintf("%.1f%%", PercentageChange)), hjust = 1.1, vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(
    x = "Year",
    y = "Percentage Change in Number of Incidents",
    title = "Percentage Change in Number of Incidents by Season and Year"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1")



library(ggrepel)
# Calculate the percentage change relative to the previous year for each season
seasonal_summary2 <- seasonal_summary %>%
  arrange(Season, Year) %>%
  group_by(Season) %>%
  mutate(PercentageChange = (Total_Shootings / lag(Total_Shootings) - 1) * 100)

# Create a line plot with labels for every 5 years
ggplot(seasonal_summary2, aes(x = Year, y = PercentageChange, group = Season, color = Season)) +
  geom_line(size = 1.5) +  # Adjust line thickness
  geom_text_repel(data = seasonal_summary %>% filter(Year %% 5 == 0), 
                  aes(label = sprintf("%.1f%%", PercentageChange)),
                  position = position_dodge(width = 0.9),
                  box.padding = 0.5,  # Adjust box padding for labels
                  max.overlaps = Inf,  # Allow unlimited overlaps
                  segment.size = 0.2) +  # Adjust segment size for label repelling
  labs(
    x = "Year",
    y = "Percentage Change in Number of Incidents",
    title = "Percentage Change in Number of Incidents by Season and Year"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(min(seasonal_summary$Year), max(seasonal_summary$Year), by = 1))


# Create a boxplot to visualize the distribution of shootings across seasons
ggplot(seasonal_summary, aes(x = Season, y = Total_Shootings, fill = Season)) +
  geom_boxplot() +
  labs(
    x = "Season",
    y = "Number of Shootings",
    title = "Distribution of Shootings Across Seasons"
  ) +
  theme_minimal()

# Perform ANOVA test
anova_result <- aov(Total_Shootings ~ Season, data = seasonal_summary)

# Print ANOVA summary
summary(anova_result)

# Perform Tukey HSD test
tukey_result <- TukeyHSD(anova_result)

# Print Tukey HSD results
print(tukey_result)


#Assumption violations: (they're reasonable enough esp with 27k lines)
# Load the car package
library(car)

# Perform Levene's test
levene_test_result <- leveneTest(Total_Shootings ~ Season, data = seasonal_summary)

# Print the result
print(levene_test_result)

# Check normality of residuals using a residual plot
residuals <- residuals(anova_result)
fitted_values <- fitted(anova_result)

plot(fitted_values, residuals)
abline(h = 0, col = "red", lty = 2)

# Perform Shapiro-Wilk test
shapiro_test_result <- shapiro.test(residuals)

# Print the result
print(shapiro_test_result)


# Check normality of residuals using a Q-Q plot
qqnorm(residuals)
qqline(residuals)
