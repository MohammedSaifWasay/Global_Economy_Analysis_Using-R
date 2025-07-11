#Group-3 
#Guancheng Lu 
#Saif Wasay Mohammed NUID: 002815958
#Ganavi Kudukuli Chandrashekar 
#Vedansh Modwel

#ALY6015 Intermediate Analytics
#Final Project: Initial Analysis 
# EDA (Exploratory Data Analysis)

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(summarytools)) install.packages("summarytools")
library(readr)
library(dplyr)
library(summarytools)
library(ggplot2)
data <- read.csv("C:/Users/Mohammed Saif Wasay/Documents/code/data/Global Economy Indicators.csv")
str(data)
summary(data)
head(data)
missing_data <- is.na(data)

#a. How has the Gross Domestic Product (GDP) changed over time for specific countries like China and India?
colnames(data) <- trimws(colnames(data))
data$Country <- trimws(data$Country)
unique(data$Country)
china_gdp_data <- data %>%
  filter(Country == "China")
ggplot(china_gdp_data, aes(x = Year, y = Gross.Domestic.Product..GDP.)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "GDP of China Over Time",
    x = "Year",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()

india_gdp_data <- data %>%
  filter(Country == "India")
ggplot(india_gdp_data, aes(x = Year, y = Gross.Domestic.Product..GDP.)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "GDP of India Over Time",
    x = "Year",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()

#b. Is there a correlation between exchange rates and GDP for India?
correlation_gdp <- cor(india_gdp_data$AMA.exchange.rate, india_gdp_data$Gross.Domestic.Product..GDP., use = "complete.obs")
ggplot(india_gdp_data, aes(x = AMA.exchange.rate, y = Gross.Domestic.Product..GDP.)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Exchange Rate vs. GDP for India",
    x = "Exchange Rate",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()

#c. How does population size correlate with per capita GNI across countries?
correlation_population_gni <- cor(data$Population, data$Per.capita.GNI, use = "complete.obs")
ggplot(data, aes(x = Population, y = Per.capita.GNI)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Correlation Between Population Size and Per Capita GNI",
    x = "Population Size",
    y = "Per Capita GNI"
  ) +
  theme_minimal() +
  scale_x_log10(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)

#d. How do imports and exports of goods and services affect the GDP?
correlation_exports_gdp <- cor(data$Exports.of.goods.and.services, data$Gross.Domestic.Product..GDP., use = "complete.obs")
correlation_imports_gdp <- cor(data$Imports.of.goods.and.services, data$Gross.Domestic.Product..GDP., use = "complete.obs")
#Exports vs. GDP
ggplot(data, aes(x = Exports.of.goods.and.services, y = Gross.Domestic.Product..GDP.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Exports vs. GDP",
    x = "Exports of Goods and Services",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()
#Imports vs. GDP
ggplot(data, aes(x = Imports.of.goods.and.services, y = Gross.Domestic.Product..GDP.)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Imports vs. GDP",
    x = "Imports of Goods and Services",
    y = "Gross Domestic Product (GDP)"
  ) 

#e. How does government final consumption expenditure vary across countries, and how does it relate to GDP?
#Summary statistics for government expenditure across countries
gov_exp_summary <- data %>%
  group_by(Country) %>%
  summarise(
    GovExp_Mean = mean(General.government.final.consumption.expenditure, na.rm = TRUE),
    GovExp_Median = median(General.government.final.consumption.expenditure, na.rm = TRUE),
    GovExp_Std = sd(General.government.final.consumption.expenditure, na.rm = TRUE),
    GDP_Mean = mean(Gross.Domestic.Product..GDP., na.rm = TRUE)
  )

# Correlation between government expenditure and GDP
correlation_gov_gdp <- cor(
  data$General.government.final.consumption.expenditure, 
  data$Gross.Domestic.Product..GDP., 
  use = "complete.obs"
)

# Visualization: Scatter plot of government expenditure vs GDP
ggplot(data, aes(x = General.government.final.consumption.expenditure, y = Gross.Domestic.Product..GDP.)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Government Expenditure vs GDP",
    x = "Government Final Consumption Expenditure",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()

# Visualization: Distribution of government expenditure across countries
ggplot(data, aes(x = General.government.final.consumption.expenditure)) +
  geom_histogram(binwidth = 1e10, fill = "blue", alpha = 0.7, color = "black") +
  labs(
    title = "Distribution of Government Final Consumption Expenditure",
    x = "Government Expenditure",
    y = "Frequency"
  ) +
  theme_minimal()

#f. Which countries have the highest share of government expenditure in relation to their GDP?
# Calculating the ratio of government expenditure to GDP
data <- data %>%
  mutate(GovExp_to_GDP_Ratio = (General.government.final.consumption.expenditure / Gross.Domestic.Product..GDP.) * 100)

# Top 10 countries by average government expenditure to GDP ratio
top_countries_gov_exp <- data %>%
  group_by(Country) %>%
  summarise(GovExp_to_GDP_Ratio_Mean = mean(GovExp_to_GDP_Ratio, na.rm = TRUE)) %>%
  arrange(desc(GovExp_to_GDP_Ratio_Mean)) %>%
  head(10)

# Bottom 10 countries by average government expenditure to GDP ratio
bottom_countries_gov_exp <- data %>%
  group_by(Country) %>%
  summarise(GovExp_to_GDP_Ratio_Mean = mean(GovExp_to_GDP_Ratio, na.rm = TRUE)) %>%
  arrange(GovExp_to_GDP_Ratio_Mean) %>%
  head(10)

# Visualization: Top 10 countries by government expenditure ratio
ggplot(top_countries_gov_exp, aes(x = reorder(Country, GovExp_to_GDP_Ratio_Mean), y = GovExp_to_GDP_Ratio_Mean)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Government Expenditure to GDP Ratio",
    x = "Country",
    y = "Government Expenditure to GDP Ratio (%)"
  ) +
  theme_minimal()

# Visualization: Bottom 10 countries by government expenditure ratio
ggplot(bottom_countries_gov_exp, aes(x = reorder(Country, -GovExp_to_GDP_Ratio_Mean), y = GovExp_to_GDP_Ratio_Mean)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Bottom 10 Countries by Government Expenditure to GDP Ratio",
    x = "Country",
    y = "Government Expenditure to GDP Ratio (%)"
  ) +
  theme_minimal()
#g. What are the top and bottom countries in terms of GDP growth over the years?
# Calculate GDP growth rate for each country
data <- data %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(GDP_Growth_Rate = (Gross.Domestic.Product..GDP. - lag(Gross.Domestic.Product..GDP.)) / lag(Gross.Domestic.Product..GDP.) * 100)

# Average GDP growth rate by country
gdp_growth_summary <- data %>%
  group_by(Country) %>%
  summarise(Avg_GDP_Growth_Rate = mean(GDP_Growth_Rate, na.rm = TRUE)) %>%
  arrange(desc(Avg_GDP_Growth_Rate))

# Top 10 countries by GDP growth rate
top_countries_gdp_growth <- gdp_growth_summary %>%
  head(10)

# Bottom 10 countries by GDP growth rate
bottom_countries_gdp_growth <- gdp_growth_summary %>%
  tail(10)

# Visualization: Top 10 countries by GDP growth rate
ggplot(top_countries_gdp_growth, aes(x = reorder(Country, Avg_GDP_Growth_Rate), y = Avg_GDP_Growth_Rate)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Average GDP Growth Rate",
    x = "Country",
    y = "Average GDP Growth Rate (%)"
  ) +
  theme_minimal()

# Visualization: Bottom 10 countries by GDP growth rate
ggplot(bottom_countries_gdp_growth, aes(x = reorder(Country, -Avg_GDP_Growth_Rate), y = Avg_GDP_Growth_Rate)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Bottom 10 Countries by Average GDP Growth Rate",
    x = "Country",
    y = "Average GDP Growth Rate (%)"
  ) +
  theme_minimal()

#h. Do countries with larger populations have higher or lower GDP on average?
# Calculate correlation between population size and GDP
correlation_population_gdp <- cor(data$Population, data$Gross.Domestic.Product..GDP., use = "complete.obs")

# Visualization: Scatter plot of Population Size vs GDP
ggplot(data, aes(x = Population, y = Gross.Domestic.Product..GDP.)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Population Size vs GDP",
    x = "Population Size",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal() +
  scale_x_log10(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)

# Visualization: Distribution of GDP by population size bins
data <- data %>%
  mutate(Population_Bin = cut(Population, breaks = quantile(Population, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE))

ggplot(data, aes(x = Population_Bin, y = Gross.Domestic.Product..GDP.)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  labs(
    title = "GDP Distribution by Population Size Bins",
    x = "Population Size Bins",
    y = "Gross Domestic Product (GDP)"
  ) +
  theme_minimal()
