# MUST RUN WORLD DATA FIRST


library(dplyr)
library(ggplot2)

# Load up to date data
load("covid19/rdas/covid19.rda")

UK_cum_cases <- 0
UK_cum_deaths <- 0

# Subset the UK data
plot_subset <- world_data %>% filter(geoId %in% c("UK"))
UK_total_cases <- sum(plot_subset$cases)
UK_total_deaths <- sum(plot_subset$deaths)

# Calculate cumulative cases and deaths counts
UK_days <- plot_subset %>% group_by(dateRep) %>% summarize(cases_by_day = sum(cases), deaths_by_day = sum(deaths))
UK_cum_cases_vector <- vector(mode = "numeric", length = date_count)
UK_cum_deaths_vector <- vector(mode = "numeric", length = date_count)

for (i in 1:date_count){
  this_day <- filter(UK_days, (dateRep == min(dateRep)+3600*24*(i-1)))
  UK_cum_cases <- UK_cum_cases + this_day$cases_by_day
  UK_cum_deaths <- UK_cum_deaths + this_day$deaths_by_day
  UK_cum_cases_vector[i] <- UK_cum_cases
  UK_cum_deaths_vector[i] <- UK_cum_deaths
}

# Make tibble of cumulative counts
UK_summary <- tibble(DateRep = date_vector, Cum_Cases = UK_cum_cases_vector, Cum_Deaths = UK_cum_deaths_vector)

# Plot UK cumulative figures
labs <- data.frame(Type = c("Cases", "Deaths"), labx = c(as.Date("20-03-20", format = "%d-%m-%y"), as.Date("20-03-20", format = "%d-%m-%y")), laby = c(10^4, 10^3))
lab2 <- data.frame(Type1 = c(UK_total_cases, UK_total_deaths), labx = c(as.Date("20-03-20", format = "%d-%m-%y"), as.Date("20-03-20", format = "%d-%m-%y")), laby = c(10^4+1000, 10^3+1000))
ggplot(UK_summary, aes(DateRep)) +
  geom_line(aes(y = Cum_Cases), color = "Blue") +
  geom_line(aes(y = Cum_Deaths), color = "Red") +
  geom_text(data = labs, aes(labx, laby, label = Type), size = 4) +
  geom_text(data = lab2, aes(labx, laby, label = Type1), size = 4) +
  ylab("Cumulative Count") +
  xlab("Date Reported") +
  ggtitle("UK Cumulative Figures for Covid19 as at", as.Date(max(summary$DateRep)))

# Plot line chart for New Cases
ggplot(plot_subset, aes(x = dateRep), show.legend = TRUE) + 
  geom_line(aes(y = cases), color = "Blue", show.legend = TRUE) +
  geom_text(aes(y = cases, label = cases), color = "Black", nudge_y = 30, check_overlap = TRUE, size = 2.5) +
  xlab("Date Reported") +
  ggtitle ("Covid-19 New Cases for UK as at", as.Date(max(plot_subset$dateRep)))


# Plot line chart for Cases and Deaths
ggplot(plot_subset, aes(x = dateRep), show.legend = TRUE) + 
  geom_line(aes(y = cases), color = "Blue", show.legend = TRUE) +
  geom_text(aes(y = cases, label = cases), color = "Black", nudge_y = 30, check_overlap = TRUE, size = 2.5) +
  geom_line(aes(y = deaths), color = "Red", show.legend = TRUE) +
  geom_text(aes(y = deaths, label = deaths), color = "Black", nudge_y = 5, check_overlap = TRUE, size = 2.5) +
  xlab("Date Reported") +
  ggtitle ("Covid-19 New Cases and Deaths for UK as at", as.Date(max(plot_subset$dateRep)))

# Plot bar chart for New Cases
ggplot(plot_subset, aes(x = dateRep, y = cases, label = cases)) +
  geom_bar(fill = "Red", stat = "identity") +
  geom_text(color = "Black", nudge_y = 40, check_overlap = TRUE, size = 2.5) +
  xlab("Date Reported") +
  ggtitle ("Covid-19 New Cases for UK as at", as.Date(max(plot_subset$dateRep)))

# Plot line chart for New Deaths
ggplot(plot_subset, aes(x = dateRep, y = deaths, label = deaths)) + 
  geom_line(color = "Blue") +
  geom_text(color = "Black", nudge_y = 1, check_overlap = TRUE, size = 2.5) +
  xlab("Date Reported") +
  ggtitle ("Covid-19 New Deaths for UK as at", as.Date(max(plot_subset$dateRep)))

# Plot bar chart for New Deaths
ggplot(plot_subset, aes(x = dateRep, y = deaths, label = deaths)) +
  geom_bar(fill = "Red", stat = "identity") +
  geom_text(color = "Black", nudge_y = 4, check_overlap = TRUE, size = 2.5) +
  xlab("Date Reported") +
  ggtitle ("Covid-19 New Deaths for UK as at", as.Date(max(plot_subset$dateRep)))
