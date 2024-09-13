# demographic analysis  
library(ggplot2)
library(tidyverse)
data <- read.csv("cleaned_data.csv")
# Distribution of age group
data$AGE_GROUP <- factor(data$AGE_GROUP,
                         levels = c("0", "21", "35", "55", "70"),
                         labels = c("<18", "18-24", "25-44", "45-64", "65+"))

ggplot(data, aes(x=AGE_GROUP)) +
  geom_bar(fill="skyblue") +
  labs(x="Age Group", y="Number Count", title="Distribution of Age Groups")
# Distribution of sex
sex_counts <- table(data$PERP_SEX)
sex_percentages <- round(prop.table(sex_counts) * 100, 2)
ggplot(data, aes(x="", fill=PERP_SEX)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=c("F"="#FF9999", "M"="#99CCFF", "U"="#99FFCC")) +
  labs(x="", y="", fill="Sex", title="Distribution of Sex") +
  coord_polar(theta="y") +
  geom_text(data = subset(data, PERP_SEX != "U"), aes(label = paste0(sex_percentages[PERP_SEX], "%")), stat="count", position = position_stack(vjust = 0.5)) +
  theme_void()
# Distribution of race
race_mapping <- c('1' = 'AMERICAN INDIAN/ALASKAN NATIVE', '2' = 'ASIAN / PACIFIC ISLANDER', '3' = 'BLACK HISPANIC', '4' = 'BLACK', '5' = 'WHITE HISPANIC', '6'='WHITE', '7'='UNKNOWN')
data$PERP_RACE <- factor(data$PERP_RACE, levels=names(race_mapping), labels=race_mapping)
race_counts <- table(data$PERP_RACE)
race_percentages <- prop.table(race_counts) * 100
ggplot(data, aes(x="", fill=PERP_RACE)) + 
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=c(
    "AMERICAN INDIAN/ALASKAN NATIVE"="#D2B48C",
    "ASIAN / PACIFIC ISLANDER"="#FFA500",
    "BLACK HISPANIC"="#FFFF00",
    "BLACK"="#8B4513",
    "WHITE HISPANIC"="#FFD700", 
    "WHITE"="#F5DEB3",  
    "UNKNOWN"="#808080")) +  
  labs(x="", y="", fill="Race", title="Distribution of Race") + 
  coord_polar(theta="y") +  
  geom_text(data = subset(data, PERP_RACE %in% c('BLACK', 'WHITE HISPANIC', 'BLACK HISPANIC', 'WHITE', 'ASIAN / PACIFIC ISLANDER')), aes(label = paste0(round(race_percentages[PERP_RACE], 2), "%")), stat="count", position = position_stack(vjust = 0.7)) +
  theme_void()


# geographic analysis
install.packages('factoextra')
install.packages('leaflet')
library(tidyverse) # Easily Install and Load the 'Tidyverse'
df <- read.csv("cleaned_data.csv") #Read data set
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(dplyr)
dfclu <- df |>
  select(LAW_CAT_CD,JURISDICTION_CODE,PD_CD,KY_CD) #Select partial variable
sapply(df[, c("LAW_CAT_CD", "JURISDICTION_CODE", "PD_CD", "KY_CD")], unique)
set.seed(123) #Set random seeds
n <- nrow(dfclu)
trainIndex = sample(1:n, size = n*0.01)   #Some samples are randomly selected for clustering
dfclu2 <- dfclu[trainIndex,] #Some samples are randomly selected for clustering
#Using kmeans for cluster analysis, using the average contour width to evaluate cluster number, the larger the contour coefficient value, the better the clustering effect
fviz_nbclust(dfclu2, kmeans, method = "silhouette") #According to the results, the optimal cluster number is 8
set.seed(123)#Set random seeds
dfkmeans<-kmeans(dfclu,8,nstart = 25) #The data set is divided into 8 categories by means clustering
df$Class <- dfkmeans$cluster#Get the clustering result label

# Categorical variables are mapped to colors
colorize_factor <- function(x) {
  # Sets the number of color gradients
  scales::col_factor(palette = "plasma", domain = unique(x))(x)
}
df1 <- transform(df,colors = colorize_factor(Class))
# check different clustering information
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Get the most representative values of "LAW_CAT_CD", "JURISDICTION_CODE", "PD_CD", "KY_CD" for each class
# Get cluster centroids and cluster assignments
cluster_centroids <- data.frame(cluster = 1:8, dfkmeans$centers)
# Function to find the most representative value for each attribute within each cluster
find_representative_values <- function(data, centroids, attributes) {
  representative_values <- lapply(1:8, function(cluster) {
    cluster_data <- data[data$Class == cluster, ]
    centroid_vals <- centroids[cluster, attributes]
    representative_attrs <- sapply(attributes, function(attr) {
      Mode(cluster_data[[attr]])
    })
    return(representative_attrs)
  })
  return(representative_values)
}
# Find the most representative values for each attribute within each cluster
attributes <- c("LAW_CAT_CD", "JURISDICTION_CODE", "PD_CD", "KY_CD")
representative_values <- find_representative_values(df, cluster_centroids, attributes)
# Print out the most representative values for each attribute within each cluster
for (i in 1:8) {
  cat("Cluster:", i, "\n")
  for (j in 1:length(representative_values[[i]])) {
    cat("Attribute:", attributes[j], "Value:", representative_values[[i]][j], "\n")
  }
  cat("\n")
}
# Cluster: 1
# Attribute: LAW_CAT_CD Value: 1
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 779
# Attribute: KY_CD Value: 126

# Cluster: 2
# Attribute: LAW_CAT_CD Value: 2
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 705 
# Attribute: KY_CD Value: 359

# Cluster: 3
# Attribute: LAW_CAT_CD Value: 2
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 101
# Attribute: KY_CD Value: 344

# Cluster: 4
# Attribute: LAW_CAT_CD Value: 2
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 339
# Attribute: KY_CD Value: 341

# Cluster: 5
# Attribute: LAW_CAT_CD Value: 1
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 109
# Attribute: KY_CD Value: 106

# Cluster: 6
# Attribute: LAW_CAT_CD Value: 1
# Attribute: JURISDICTION_CODE Value: 0 
# Attribute: PD_CD Value: 397
# Attribute: KY_CD Value: 105

# Cluster: 7
# Attribute: LAW_CAT_CD Value: 2
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 922
# Attribute: KY_CD Value: 348

# Cluster: 8
# Attribute: LAW_CAT_CD Value: 2
# Attribute: JURISDICTION_CODE Value: 0
# Attribute: PD_CD Value: 511
# Attribute: KY_CD Value: 235
# Display the unique colors associated with each class
unique_colors <- unique(df1[c("Class", "colors")])
print(unique_colors)
#     Class  colors
# 1       6 #F48849 Light Salmon
# 2       5 #DB5C68 Blush
# 14      3 #8B0AA5 Electric Purple
# 20      1 #0D0887 Indigo
# 30      4 #B93289 Mulberry
# 73      8 #F0F921 Lemon Chiffon
# 107     7 #FEBC2A Selective Yellow
# 110     2 #5402A3 Royal Purple
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet'
dft = data.frame(Lat = df1$Latitude, Lon = df1$Longitude,
                 colors = df1$colors)
m <- leaflet(data = dft) %>%
  addTiles() %>%
  setView(lng = -74.006, lat = 40.7128, zoom = 15)
addCircles(map = m, lng = ~Lon, lat = ~Lat, color = ~colors, radius = 25, stroke = FALSE, fillOpacity = 0.8)


# Time series analysis
library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
library(scales)
library(tsibble)
library(fable)

data = read.csv("cleaned_data.csv")
str(data)
# cover from 2023-01-01 to 2023-12-31
data$ARREST_DATE <- as.Date(data$ARREST_DATE, "%Y-%m-%d")
# monthly arrest Plot
month_arrest <- data %>%
  group_by(MONTH) %>%
  summarise(number_arrest = n())
month_arrest
ggplot(month_arrest, aes(x=MONTH, y=number_arrest)) +
  geom_line() +
  ggtitle("number of incidents by month") +
  scale_x_continuous(breaks=seq(0,12,by=1))
# p-value = 0.5245 > 0.05, which means that month has no significant influence on number of arrest
reg=lm(month_arrest$number_arrest~month_arrest$MONTH)
summary(reg)
# daily arrest
date_arrest = data %>%
  group_by(ARREST_DATE) %>%
  summarise(number_arrest = n())
date_arrest
# daily arrest plots
# arrest activity reach the pick in May, and reach minimum in February
ggplot(date_arrest, aes(x=ARREST_DATE, y=number_arrest)) +
  geom_line() +
  ggtitle("number of incidents by date") +
  scale_x_continuous(breaks=seq(0,12,by=1))
plot(number_arrest ~ ARREST_DATE, date_arrest, xaxt = "n", type = "l")
axis(1, date_arrest$ARREST_DATE, format(date_arrest$ARREST_DATE, "%b"), cex.axis = 1.2)
# no big influence
reg=lm(date_arrest$number_arrest~date_arrest$ARREST_DATE)
summary(reg)
# Autocorrelation Function (ACF)
#There is a significant spike at lag 1, which means there's a strong positive correlation 
# between each observation and its immediately preceding observation.
acf(date_arrest$number_arrest, main="ACF for Daily Arrests")
# Partial Autocorrelation Function (PACF)
#The PACF cuts off after the first lag, meaning that the partial correlation after lag 1 
#is not significant when the influence of the intervening lags is removed.
pacf(date_arrest$number_arrest, main="PACF for Daily Arrests")
# Fit an ARIMA model
fit <- auto.arima(date_arrest$number_arrest)
summary(fit)
# Forecast the next 12 periods (e.g., months)
forecast_fit <- forecast(fit, h=12)
# Create a sequence of dates from January 2, 2024 to January 13, 2024
forecast_dates <- as.Date("2024-01-02") + 0:11
forecast_df <- data.frame(
  Time = time(forecast_fit$mean),
  Forecast = as.numeric(forecast_fit$mean),
  Lo80 = as.numeric(forecast_fit$lower[,1]),
  Hi80 = as.numeric(forecast_fit$upper[,1]),
  Lo95 = as.numeric(forecast_fit$lower[,2]),
  Hi95 = as.numeric(forecast_fit$upper[,2])
)
# Replace the Time column with these dates in the forecast dataframe
forecast_df$Time <- forecast_dates
# Now plot the forecast using ggplot with dates on the x-axis
ggplot(forecast_df, aes(x=Time, y=Forecast)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin=Lo80, ymax=Hi80), fill="blue", alpha=0.2) + # 80% CI
  geom_ribbon(aes(ymin=Lo95, ymax=Hi95), fill="blue", alpha=0.1) + # 95% CI
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") + # Set x-axis breaks and labels
  xlab("Date") + ylab("Number of Arrests") +
  ggtitle("Point Forecast with 80% and 95% Prediction Intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability
## holiday arrest Plot
holiday = data %>%
  filter(HOLIDAY == 1) %>%
  select(ARREST_DATE, MONTH, OFNS_DESC) 
holiday

holiday_arrest = holiday %>%
  group_by(MONTH) %>%
  summarise(number_arrest = n())
holiday_arrest
# number of arrest reaches its pick in January
# probable reason: 1. more crime activity in January 2. more polices effort in January
# number of arrest on Thanksgiving was pretty low
ggplot(holiday_arrest, aes(x=MONTH, y=number_arrest)) +
  geom_line() +
  ggtitle("number of incidents on holidays") +
  scale_x_continuous(breaks=seq(0,12,by=1))
# p-value = 0.1136 > 0.05
# holidays would not have statistically significant influence on number of arrest
reg=lm(number_arrest~MONTH, data = holiday_arrest)
summary(reg)
