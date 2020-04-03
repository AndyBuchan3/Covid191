# Calculate cumulative figures for the World and plot

library(dplyr)
library(ggplot2)

# Load up to date data
load("rdas/covid19.rda")

names(world_data) <- c("DateRep", "Day", "Month", "Year", "Cases", "Deaths", "Country", "GeoId", "Country Code", "Population") 

total_cases <- sum(world_data$Cases)
total_deaths <- sum(world_data$Deaths)
dates <- distinct_at(world_data, vars(DateRep))

days <- world_data %>% group_by(DateRep) %>% summarize(cases_by_day = sum(Cases), deaths_by_day = sum(Deaths))

date_vector <- as.Date(days$DateRep)
date_count <- length(date_vector)

# Calculate cumulative cases and deaths counts
cum_cases_vector <- vector(mode = "numeric", length = date_count)
cum_deaths_vector <- vector(mode = "numeric", length = date_count)
cum_cases <- 0
cum_deaths <- 0

for (i in 1:date_count){
  this_day <- filter(days, (DateRep == min(as.Date(DateRep))+3600*24*(i-1)))
  cum_cases <- cum_cases + this_day$cases_by_day
  cum_deaths <- cum_deaths + this_day$deaths_by_day
  cum_cases_vector[i] <- cum_cases
  cum_deaths_vector[i] <- cum_deaths
}

# Make tibble of cumulative counts
summary <- tibble(DateRep = date_vector, Cum_Cases = cum_cases_vector, Cum_Deaths = cum_deaths_vector)

# Calculate Case and Death Rates per 100000 people
# Plot cumulative figures
labs <- data.frame(Type = c("Cases", "Deaths"), labx = c(as.Date("01-03-20", format = "%d-%m-%y"), as.Date("01-03-20", format = "%d-%m-%y")), laby = c(10^5, 10^4))
lab2 <- data.frame(Type1 = c(total_cases, total_deaths), labx = c(as.Date("01-03-20", format = "%d-%m-%y"), as.Date("01-03-20", format = "%d-%m-%y")), laby = c(10^5+25000, 10^4+25000))
ggplot(summary, aes(DateRep)) +
  geom_line(aes(y = Cum_Cases), color = "Blue") +
  geom_line(aes(y = Cum_Deaths), color = "Red") +
  geom_text(data = labs, aes(labx, laby, label = Type), size = 4) +
  geom_text(data = lab2, aes(labx, laby, label = Type1), size = 4) +
  ylab("Cumulative Count") +
  xlab("Date Reported") +
  ggtitle("Global Cumulative Figures for Covid19 as at", as.Date(max(summary$DateRep)))

# Plot Case Rate by Country
Rates <- world_data %>% filter(!is.na(Population)) %>% group_by(GeoId, Population) %>% summarize(Tot_Cases = sum(Cases), Tot_Deaths = sum(Deaths))
Rates <- Rates %>% group_by(GeoId, Population) %>% summarize(Tot_Cases = Tot_Cases, Tot_Deaths = Tot_Deaths, Case_Rate = Tot_Cases/Population*10^5, Death_Rate = Tot_Deaths/Population*10^5) %>% ungroup()
Rates <- Rates %>% filter(Case_Rate > 0) %>% mutate(GeoId = reorder(GeoId, Case_Rate))
ggplot(Rates, aes(GeoId, log10(Case_Rate)), fill = GeoId) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(hjust = 1, size = 5)) +
  coord_flip() +
  #scale_y_continuous(trans = "log10") +
  ggtitle("Rate of New Cases per 100,000 people") +
  ylab("Cases per 100,000 population (log scale)") +
  xlab("Country")

# Plot Death Rate by Country
Rates <- world_data %>% filter(!is.na(Population)) %>% group_by(GeoId, Population) %>% summarize(Tot_Cases = sum(Cases), Tot_Deaths = sum(Deaths))
Rates <- Rates %>% group_by(GeoId, Population) %>% summarize(Tot_Cases = Tot_Cases, Tot_Deaths = Tot_Deaths, Case_Rate = Tot_Cases/Population*10^5, Death_Rate = Tot_Deaths/Population*10^5) %>% ungroup()
Rates <- Rates %>% filter(Death_Rate > 0) %>% mutate(GeoId = reorder(GeoId, Death_Rate))
ggplot(Rates, aes(GeoId, log10(Death_Rate)), col = GeoId) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(hjust = 1, size = 5)) +
  coord_flip() +
  #scale_y_continuous(trans = "log10") +
  ggtitle("Rate of Deaths per 100,000 people") +
  ylab("Deaths per 100,000 population (log scale)") +
  xlab("Country")

