##Module 2 

##Crop Datset PART 01

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stats)

# Load the dataset
crop_data <- read.csv("C:/Users/ravin/Downloads/crop_data.csv")

# View the structure of the dataset
str(crop_data)

summary(crop_data)  # Summary statistics

# Step 2: Check for missing values
missing_vals <- colSums(is.na(crop_data))
cat("Missing Values:\n")
print(missing_vals[missing_vals > 0])

# Step 3: Descriptive Analysis
describe(crop_data)  # Descriptive statistics for all columns

# Step 4: Visualizations
# Histogram of yield
ggplot(crop_data, aes(yield)) +
  geom_histogram(fill = "lavender", bins = 20, color = "black", alpha = 0.7) +
  labs(title = "Histogram of Crop Yield", x = "Yield", y = "Frequency")

# Bar plots for density and fertilizer
ggplot(crop_data, aes(x = as.factor(density), fill = as.factor(density))) +
  geom_bar() +
  labs(title = "Density Distribution", x = "Density", y = "Count") +
  theme_minimal()

ggplot(crop_data, aes(x = as.factor(fertilizer), fill = as.factor(fertilizer))) +
  geom_bar() +
  labs(title = "Fertilizer Distribution", x = "Fertilizer", y = "Count") +
  theme_minimal()

# Check normality using Shapiro-Wilk test
shapiro.test(crop_data$yield)

# Check homogeneity of variance using Levene's test
leveneTest(yield ~ as.factor(fertilizer), data = crop_data)

# Perform one-way ANOVA
anova_result <- aov(yield ~ as.factor(fertilizer), data = crop_data)

# Display the ANOVA table
summary(anova_result)

# Calculate degrees of freedom
k <- length(unique(crop_data$fertilizer))  # Number of fertilizer types
N <- nrow(crop_data)  # Total number of observations
df1 <- k - 1
df2 <- N - k

# Find the critical F-value
critical_f <- qf(0.95, df1, df2)
print(paste("Critical F-value:", critical_f))

# Perform Tukey HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

ggplot(crop_data, aes(x = as.factor(fertilizer), y = yield, fill = as.factor(fertilizer))) +
  geom_boxplot() +
  labs(title = "Boxplot of Yield by Fertilizer Type",
       x = "Fertilizer Type",
       y = "Yield") +
  theme_minimal()

plot(anova_result, which = 1)  # Residuals vs Fitted


#QUESTION



# Cell means
cell_means <- data.frame(
  Grow_light = rep(c("Grow-light 1", "Grow-light 2"), each = 2),
  Plant_food = rep(c("Plant food A", "Plant food B"), 2),
  Growth = c(9.17, 7.6, 8.87, 6.3) # Calculated means for each cell
)

# Load ggplot2 library
library(ggplot2)

# Create the interaction plot
ggplot(cell_means, aes(x = Grow_light, y = Growth, group = Plant_food, color = Plant_food)) +
  geom_line(size = 1) +           # Line connecting points
  geom_point(size = 3) +          # Points for means
  labs(
    title = "Interaction Plot: Grow-light vs Plant Food",
    x = "Grow-light",
    y = "Mean Growth (inches)",
    color = "Plant Food"
  ) +
  theme_minimal(base_size = 14) + # Minimal theme for clean visualization
  theme(legend.position = "top")


#part:3

# Load necessary libraries
library(tidyverse)

# Import the dataset
df <- read_csv("C:\\Users\\ravin\\Downloads\\baseball.csv")

# Display basic information about the dataset
glimpse(df)

##EDA
# Descriptive statistics
summary(df)

# Plotting
# Distribution of Wins
ggplot(df, aes(x = W)) +
  geom_histogram(bins = 20, fill = "lightsteelblue", alpha = 0.7) +
  labs(title = "Distribution of Wins", x = "Number of Wins", y = "Frequency")

# Boxplot of Wins by League
ggplot(df, aes(x = League, y = W)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot of Wins by League", x = "League", y = "Number of Wins")

# Trend of Wins Over Years
# Load necessary library
library(ggplot2)

# Plot the trend of wins over the years
ggplot(df, aes(x = Year, y = W)) +
  geom_line(color = "red") +
  labs(title = "Trend of Wins Over Years", x = "Year", y = "Number of Wins")

# Correlation heatmap
# Install the reshape2 package 
install.packages("reshape2")

# Load the reshape2 package
library(reshape2)

# Create the correlation heatmap
correlation_matrix <- cor(df %>% select_if(is.numeric))
melted_correlation_matrix <- melt(correlation_matrix)

ggplot(melted_correlation_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "")

##Chi Square

# Create a new column 'Decade' to categorize the years into decades
df <- df %>%
  mutate(Decade = floor(Year / 10) * 10)

# Group by 'Decade' and sum the wins for each decade
wins_by_decade <- df %>%
  group_by(Decade) %>%
  summarise(Total_Wins = sum(W))

# Perform Chi-Square Goodness-of-Fit test
observed_frequencies <- wins_by_decade$Total_Wins
expected_frequencies <- rep(mean(observed_frequencies), length(observed_frequencies))

chi_square_test <- chisq.test(observed_frequencies, p = expected_frequencies / sum(expected_frequencies))

# Print the results
chi_square_test
