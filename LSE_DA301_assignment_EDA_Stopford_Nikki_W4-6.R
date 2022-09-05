# Week 4
#Business question 4: What impact does each product have on sales?

#Install and import the Tidyverse library
install.packages('tidyverse')
library(tidyverse)

# Import the data set (wages_demo.csv), with first row as header.
turtle_sales <- read.csv("turtle_sales.csv", header=TRUE) 

# Print/return the data frame.
turtle_sales

# View the data frame.
head(turtle_sales)
View(turtle_sales) 

# Explore the data
dim(turtle_sales)
glimpse(turtle_sales)
summary(turtle_sales)

# Subset the data by dropping redundant columns and save as a new data frame
sales <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# Explore the data for missing values (output shows no missing values)
sum(is.na(sales))

# View and explore the subset dataframe
View(sales)
head(sales)
summary(sales)
as_tibble(sales)

# Generate a downloadable HTML file containing summary stats of the data subset
DataExplorer::create_report(sales)

# Create plots to review and determine insights into the sales data set.
# Create a scatterplots
qplot(NA_Sales, EU_Sales, data=sales, col=Platform)
qplot(NA_Sales, Global_Sales, data=sales, col=Platform)
qplot(Global_Sales, EU_Sales, data=sales, col=Platform)
qplot(y=NA_Sales, data=sales)
qplot(y=EU_Sales, data=sales)
qplot(y=Global_Sales, data=sales)

# Create histograms
qplot(NA_Sales, bins=10, data=sales)
qplot(EU_Sales, bins=10, data=sales)
qplot(Global_Sales, bins=10, data=sales)

# Create boxplots
qplot(NA_Sales, data=sales, colour=I('red'), geom='boxplot')
qplot(EU_Sales, data=sales, colour=I('red'), geom='boxplot')
qplot(Global_Sales, data=sales, colour=I('red'), geom='boxplot')

# Determine the impact on sales per product_id
# Use the group_by and aggregate functions to sum the values grouped by product.
sales_sum <- sales %>% group_by(Product) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

# View the results.
sales_sum

# Create a summary of the new data frame.
summary(sales_sum)

# Create plots to review and determine insights into the sales data set.
# Create a scatterplots
qplot(NA_Sales_sum, EU_Sales_sum, data=sales_sum, colour=I('red'))
qplot(NA_Sales_sum, Global_Sales_sum, data=sales_sum, colour=I('purple'))
qplot(Global_Sales_sum, EU_Sales_sum, data=sales_sum, colour=I('blue'))

# Create histograms
qplot(NA_Sales_sum, bins=10, data=sales_sum)
qplot(EU_Sales_sum, bins=10, data=sales_sum)
qplot(Global_Sales_sum, bins=10, data=sales_sum)

# Create boxplots
qplot(NA_Sales_sum, data=sales_sum, colour=I('red'), geom='boxplot')
qplot(EU_Sales_sum, data=sales_sum, colour=I('red'), geom='boxplot')
qplot(Global_Sales_sum, data=sales_sum, colour=I('red'), geom='boxplot')

# Note your observations and diagrams that could be used to provide insights into the business.
## Observations and charts noted in assignment report for Business question 4.

###################################################################################

# Week 5
# Business question 4: What impact does each product have on sales?  
# Business question 5: How reliable is the data (e.g. normal distribution, skewness or kurtosis)?

# Load and explore the subset sales data from week 4. 
# View the data frame to sense-check the data set.
View(sales_sum)
head(sales_sum)
summary(sales_sum)
as_tibble(sales_sum)
  
# Determine the min, max and mean values of all the sales data (three columns).
# North American sales mean, median min and max
mean(sales_sum$NA_Sales_sum)
median(sales_sum$NA_Sales_sum)
min(sales_sum$NA_Sales_sum)
max(sales_sum$NA_Sales_sum)

# EU sales mean, median min and max
mean(sales_sum$EU_Sales_sum)
median(sales_sum$EU_Sales_sum)
min(sales_sum$EU_Sales_sum)
max(sales_sum$EU_Sales_sum)

# Global sales mean, median min and max
mean(sales_sum$Global_Sales_sum)
median(sales_sum$Global_Sales_sum)
min(sales_sum$Global_Sales_sum)
max(sales_sum$Global_Sales_sum)

# Create a summary of the data frame.
summary(sales_sum)

# As also done in Wk4: Determine the impact on sales per product_id
# Use the group_by, apply(), and/or aggregate functions to sum the values grouped by product. 
sales_sum <- sales %>% group_by(Product) %>%
  summarise(NA_Sales_sum=sum(NA_Sales),
            EU_Sales_sum=sum(EU_Sales),
            Global_Sales_sum=sum(Global_Sales),
            .groups='drop')

# View the results.
sales_sum

# Create a summary of the data frame.
summary(sales_sum)

# Create plots explore normality in the data set.
# Create a historgram plot.
hist(sales_sum$Global_Sales_sum) +
  geom_smooth(se=FALSE)
  
# Create a boxplot.
boxplot(sales_sum$Global_Sales_sum) +
  title("Boxplot of global product sales")
  theme_classic()
  
# Create a quantile-quantile plot with qqline
qqnorm(sales_sum$Global_Sales_sum) 
qqline(sales_sum$Global_Sales_sum) 

# Determine the normality of the data set (sales data).
# Perform a Shapiro-Wilk test on all the sales data.
shapiro.test(sales_sum$NA_Sales_sum)
shapiro.test(sales_sum$EU_Sales_sum)
shapiro.test(sales_sum$Global_Sales_sum)

# Determine the Skewness and Kurtosis of all the sales data.
# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
# North American sales skewness and kurtosis
skewness(sales_sum$NA_Sales_sum) 
kurtosis(sales_sum$NA_Sales_sum)

# EU Sales skewness and kurtosis
skewness(sales_sum$EU_Sales_sum) 
kurtosis(sales_sum$EU_Sales_sum)

# Global sales skewness and kurtosis
skewness(sales_sum$Global_Sales_sum) 
kurtosis(sales_sum$Global_Sales_sum)

# Determine if there is any correlation between the sales data columns.
cor(sales_sum$Global_Sales_sum, sales_sum$NA_Sales_sum)
cor(sales_sum$Global_Sales_sum, sales_sum$EU_Sales_sum)
cor(sales_sum$EU_Sales_sum, sales_sum$NA_Sales_sum)

# Our correlation coefficients of 0.92, 0.85 and 0.62 
# This suggests a strong positive correlation.

# Create plots to gain insights into the sales data.
# Install and import the ggplot2 package.
install.packages('ggplot2')
library(ggplot2)

# Create a scatterplot to demonstrate relationship between NA sales and EU sales
# Exclude standard error
ggplot(sales, aes(x=NA_Sales, y=EU_Sales,)) + 
  geom_point() +
  geom_smooth(se=FALSE) +
  ggtitle("Relationship between product sales in North America and Europe") +
  theme_classic()
  
################################################################################

# Week 6

# Business question 6: What are the relationships, if any, between North American, European and global sales?

# Load and explore the week 5 data frame 

# Create a simple linear regression model.
  # Determine the correlation between the sales columns.
  # View the output.
  # Create plots to view the linear regression.

# Create a multiple linear regression model.
  # Select only the numeric columns.
  # Determine the correlation between the sales columns.
  # View the output.

# Predict global sales based on provided values. Compare your prediction to the observed value(s).
  # NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
  # NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
  # NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
  # NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
  # NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

# Summarise (150–200 words) any insights you’ve discovered as well as anything you would like to explore further.
