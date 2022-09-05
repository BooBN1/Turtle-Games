# Turtle-Games

CO2 LSE DA 301 Advanced Analytics for Organisational Impact. Assignment: Predicting future outcomes through the analysis of data sets to help Turtle Games, a games manufacturer and retailer, to improve overall sales performance

Business question 1: How do customers accumulate loyalty points? The 2000 Turtle Games customers have a mean average of 1578 loyalty points. This ranges from a minimum of 25 to a maximum of 6847. To explore drivers of loyalty points we have correlated this against customer spend, income and age. Multiple linear regression analysis looking at accumulation of loyalty points shows that 83% of variation can be explained by customer spending and income. In other words, customers will have more loyalty points if they spend more and have higher incomes. Breaking this down using simple linear regression analysis using the OLS method shows the amount spent by customers accounts for 45% of the variation in loyalty points and customer income accounts for a further 38%. Age has minimal impact on how customers accumulate loyalty points.

Business question 2: How can groups within the customer base be used to target specific market segments? Using the k=5 model, cluster 1 (middle income/middle spend) has most data points, followed by cluster 0 (high income/high spend) and then cluster 2 (high income/low spend). These three customer segments could be used by the marketing team to target future advertising. A particularly interesting target market could be cluster 2, these customers are high earners but have lower spending scores so the ambition could be to move them into the high income/high spend segment.

Business question 3: How can social data (e.g. customer reviews) be used to inform marketing campaigns? The sentiment analysis of textual social data comments shows a leaning towards positive comments but there is no extreme sentiment in either direction. The most common words left in reviews tend to be positive - as can be seen in the wordcloud visualisation and the plot of the 15 most common words used in customer product reviews. Keywords like ‘five stars’, ‘great’, ‘fun’, ‘love’, ‘good’ feature strongly. Taking a closer look at the top 20 positive reviews and top 20 negative reviews received from customers gives a strong indication of what customers did and didn’t like. A theme coming from the more negative reviews showed that customers found products bought difficult or complicated to use. For example, one dissatisfied customer wrote: “Booo, unless you are patient, know how to measure, I didn’t have the patience neither did my daughter. Boring unless you are a craft person, which i am not”. Conversely, more positive customer experiences revealed much higher satisfaction with the products bought. As one very satisfied customer wrote: “Excellent activity for teaching self-management skills.” As an additional piece of analysis it would be useful for the team to interrogate whether there are any trends in the products bought - so, are some products more likely to elicit negative feedback than others.

Business question 4: What impact does each product have on sales?
The sales data has a total of 352 rows of data for products sold globally. Product lines are separated by the different types of gaming platforms bought for. Product data has been aggregated to explore trends in the data by product id. This aggregated data covers a total of 175 product types. Looking at the sales revenue by product, Product 107 has generated the highest revenue in sales for Turtle - both across North America and Europe - generating a total of £67.85m in sales globally. Typically, there is a positive correlation between product sales across North America and in Europe. The scatterplot in chart 9 illustrates the tendency for products to generate similar revenue levels across regions. There are some outliers where products have higher sales revenues in one region. An example is product 123 which generated £26.64m in revenue in North America but only £4.01m in Europe. Chart 10 illustrates histogram plots in ‘bins’ of 10 the product revenues in North America and EU. Both plots show a similar shape to the data across regions - with products sales across North America and Europe being positively skewed. This shows that most products generate sales of less than £10m. Examining the descriptive statistics more closely for global sales grouped by product we can see that the median average of sales across the 175 products is £8.09m. The lowest global sales achieved for a product was £4.2m. This is illustrated in the boxplot in chart 11, which further visualises the right-skewed nature of the data, and the outliers of some products that have performed particularly well. The middle 50% of product sales (the interquartile range) fall between £5.52m and £12.79m.

Business question 5: How reliable is the data (e.g. normal distribution, skewness or kurtosis)?
The histogram for global sales (see chart 12) shows the right skewedness of the data. This means the data does not follow a normal distribution. The boxplot for global sales, chart above, shows visible outliers in the data. Chart 13 also indicates that the data isn’t following a normal distribution because the data points on the Q-Q Plot are not following the straight reference line. A Shapiro-Wilk normality test was conducted to confirm that the results do not follow a normal distribution. The results across the sales data - NA, EU and global - show a p-value that is very close to zero (for example, for global sales the p-value < 2.2e-16), which confirms the data is not normally distributed. The skewness test confirms as we’ve seen visually that the data is positively (or right) skewed. We saw evidence of this from the results of the mean and the median, where the mean is larger than the median. Data is considered to be normal if the skewness is less than -1 and greater than 1. Skewness results for all sales data is above 2. A normal distribution will have a kurtosis coefficient of 3. The values for the sales data are much higher than this (for example, 17.8 for global sales), which indicates that a leptokurtic (or heavy-tailed) distribution. This confirms that we are seeing more extreme outliers than you would in a normal distribution. The Pearson’s correlation coefficient (r) test was run to determine whether there is a correlation between the sales data columns. This test is typically only used when on data with a normal distribution. However, the correlation coefficients of the sales data columns suggest a strong positive correlation - so, for instance, higher sales of a product in Europe typically correlates with higher sales of a product in North America. We can see this clearly in chart 14.


Business question 6: What are the relationships, if any, between North American, European and global sales?
