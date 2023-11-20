library(readr)
NYPD_Shooting_Incident_Data_Historic_20231107 <- read_csv("NYPD_Shooting_Incident_Data__Historic__20231107.csv")
View(NYPD_Shooting_Incident_Data_Historic_20231107)

df = NYPD_Shooting_Incident_Data_Historic_20231107

library(ggplot2)
library(dplyr)  

# Group the data by BORO and count the number of incidents in each group
incident_count <- df %>%
  group_by(BORO) %>%
  summarise(incident_count = n())

# Create a bar plot
ggplot(incident_count, aes(x = forcats::fct_reorder(BORO, -incident_count), y = incident_count, fill = BORO)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Borough",
    y = "Number of Incidents",
    title = "Number of Incidents by Borough",
    fill = "Borough"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

#########################
incident_count_by_year <- df %>%
  group_by(Year) %>%
  summarise(incident_count = n())

# Sort the data in ascending order by year
incident_count_by_year <- incident_count_by_year %>%
  arrange(Year)

# Create a line plot for the number of incidents by year
ggplot(incident_count_by_year, aes(x = Year, y = incident_count)) +
  geom_line() +
  scale_x_continuous(breaks = unique(incident_count_by_year$Year)) +  # Show every year on the x-axis
  labs(
    x = "Year",
    y = "Number of Incidents",
    title = "Number of Incidents by Year"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)
  )


####################
population = c(1379946, 2590516, 1596273, 2278029, 491133)
incident_count = cbind(incident_count, population)


# Calculate the incident rate per 100,000 people
incident_count$incident_rate_per_100k <- (incident_count$incident_count / incident_count$population) * 100000

# Sort the data by incident rate (descending order)
incident_count <- incident_count %>%
  arrange(desc(incident_rate_per_100k))

# Create a bar plot for the incident rate per 100,000 people
ggplot(incident_count, aes(x = forcats::fct_reorder(BORO, -incident_rate_per_100k), y = incident_rate_per_100k, fill = BORO)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Borough",
    y = "Incident Rate per 100,000 People",
    title = "Incident Rate by Borough (per 100,000 People)",
    fill = "Borough"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

df = NYPD_Shooting_Incident_Data_Historic_20231107
df$Season = "0"
df[df$Month == "6" | df$Month == "7" | df$Month == "8", ]$Season = "Summer"
df[df$Month == "9" | df$Month == "10" | df$Month == "11", ]$Season = "Fall"
df[df$Month == "12" | df$Month == "1" | df$Month == "2", ]$Season = "Winter"
df[df$Month == "3" | df$Month == "4" | df$Month == "5", ]$Season = "Spring"

table(is.na(df$Season))

write.csv(df, "data.csv", row.names = FALSE)
