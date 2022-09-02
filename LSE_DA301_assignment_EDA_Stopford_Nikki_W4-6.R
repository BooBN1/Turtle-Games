#Business question 4: What impact does each product have on sales?

#Install and import the Tidyverse library
install.packages('tidyverse')
library(tidyverse)

# Import the data set (wages_demo.csv), with first row as header.
turtle_sales <- read.csv("turtle_sales.csv", header=TRUE) 

# Print/return the data frame.
turtle_sales

# View the data frame.
View(turtle_sales) 

# Explore the data
glimpse(turtle_sales)
summary(turtle_sales)

# Subset the data by dropping redundant columns
sales <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# View and explore the subset dataframe
View(sales)
head(sales)
summary(sales)
as_tibble(sales)

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
sales_sum <- sales %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# Create a summary of the new data frame.
head(as_tibble(sales_sum))
View(sales_sum)
glimpse(sales_sum)
summary(sales_sum)

# Create plots to review and determine insights into the sales data set.
# Create a scatterplots
qplot(NA_Sales_sum, EU_Sales_sum, data=sales_sum)
qplot(NA_Sales_sum, Global_Sales_sum, data=sales_sum)
qplot(Global_Sales_sum, EU_Sales_sum, data=sales_sum, colour=I('red'))

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






