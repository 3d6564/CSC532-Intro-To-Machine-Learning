# Managing Data with R

## Saving/Loading/Removing data in R
x = c(1, 2, 3)
y = c(4, 5, 6)
z = c(7, 8, 9)

### Saving data to a permanent RData file from memory
save(x, y, z, file = "2_exploratory_analysis/test.RData")
rm(x, y, z)

### Load previously saved RData file
load("2_exploratory_analysis/test.RData")

### List and remove all objects
ls()
rm(list = ls())

### Read in a csv. File has header and default option works. Read.table() has 
### more features versus other methods to customize.
pt_data <- read.csv("0_data/pt_data.csv", stringsAsFactors = FALSE)
pt_data_table <- read.table("0_data/pt_data.csv", quote="\"", sep=",", 
                            header=TRUE)

### Write to a file using write.csv or write.table.Include row.names=FALSE
### if you want to exclude row numbers as the leading column.
write.table(pt_data_table, "2_exploratory_analysis/testwrite.csv", sep=",",
            row.names=FALSE)


## Data exploration
usedcars <- read.csv("0_data/usedcars.csv", stringsAsFactors = TRUE)
str(usedcars)
summary(usedcars$year)
summary(usedcars)

### Measuring central tendency (mean and median)
mean(usedcars$mileage)
median(usedcars$mileage)

### Span between min and max
range(usedcars$mileage)
range(usedcars$price)
diff(range(usedcars$mileage))
diff(range(usedcars$price))

### IQR - difference between Q1 and Q3. This can help determine outliers.
IQR(usedcars$mileage)

### Quartiles
quantile(usedcars$mileage)
quantile(usedcars$mileage, seq(from = 0, to = 1, by = .20))

### Identify outliers using IQR. It may be determined to drop any records that
### are outliers for multiple variables, such as below. After examining price 
### and mileage it is determined that records 149 and 150 may need to be 
### dropped.

### Used Car Price outliers
IQRprice <- IQR(usedcars$price)
Q1price <- quantile(usedcars$price, .25)
Q3price <- quantile(usedcars$price, .75)
leftprice <- Q1price-1.5*IQRprice
rightprice <- Q3price+1.5*IQRprice
usedcars[usedcars$price<leftprice | usedcars$price>rightprice,]

### Used Car Mileage outliers
IQRmile <- IQR(usedcars$mileage)
Q1mile <- quantile(usedcars$mileage, .25)
Q3mile <- quantile(usedcars$mileage, .75)
leftmile <- Q1mile-1.5*IQRmile
rightmile <- Q3mile+1.5*IQRmile
usedcars[usedcars$mileage<leftmile | usedcars$mileage>rightmile,]

### Boxplots
boxplot(usedcars$mileage, main = "Used Car Mileage", ylab = "Miles")
boxplot(usedcars$price, main = "Boxplot of Car Price", ylab = "Price ($)")

### Histogram can illustrate the distribution and mode. Both appear to be 
### unimodal distribution, but not normal. Price has the closest to normal.
hist(usedcars$mileage, main = "Used Car Mileage", xlab = "Miles")
hist(usedcars$price, main = "Used Car Price", xlab = "Price ($)")

### Measuring spread with variance and standard deviation. A larger variance
### indicates that the data is spread more widely. The standard deviation can be
### used to determine how far from the mean a typical value might be.
var(usedcars$mileage)
sd(usedcars$mileage)


## Exploring Categorical Data - Bivariate Analysis
### Examining the maximum from table() will be the mode of the data.
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

### Mode
t = table(usedcars$price)
names(t[t == max(t)])

### Table proportions for categorical data. This displays how the data is 
### distributed across the categories as a percentage of total. 
model_table <- table(usedcars$model)
round(prop.table(model_table), digit=3)

### Same output as prop.table but formatted
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

### Two-way cross-tabulations
install.packages("gmodels")
library(gmodels)

### Create dummy variable for binary indicator. %in% returns TRUE or FALSE for
### each value in the vector of the left side of the operator if it is found
### in the right side vector. The term conservative is used based on the plain
### colors selected as a filter.
usedcars$conservative <- usedcars$color %in% 
  c("Black", "Gray", "Silver", "White")

### The crosstable shows the distribution of the usedcar models across the 
### conservative colors.
table(usedcars$conservative, model)
CrossTable(x = usedcars$model, y = usedcars$conservative)
CrossTable(x = usedcars$color %in% c("Black","Gold"), y = usedcars$model,
           chisq = TRUE)
chisq.test(table(color %in% c("Black","Gold"), model))
### P-value was ~40 and is much GREATER than significance level of 1-5% then we
### say we cannot reject null.. We cannot say that black or gold are associated
### with the model of the car.


## Exploring Numeric Data - Bivariate Analysis
### Relationships between variables with scatterplots. 
### Price is negatively correlated to mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Price vs Mileage",
     xlab = "Mileage",
     ylab = "Price ($)")
### Year is positively correlated to price. Year is negatively correlated to 
### mileage.
plot(usedcars[,c("year","price","mileage")])

### Correlation
cor(usedcars$price, usedcars$mileage)
cor(usedcars[,c("year","price","mileage")])

### Pearson hypthesis testing with confidence interval. Provides a confidence
### interval for the correlation value calculated previously. We are 95 percent
### confident the correlation of price and mileage is between -.85 and -.74
cor.test(usedcars$price, usedcars$mileage)


## Exploring Numeric vs Categorical Data
### Side by Side boxplot
plot(usedcars$price~usedcars$model, col="red")
plot(usedcars$price~usedcars$transmission, col="red")

### Two sample T-test in R. P-Value=.13 and is greater than .05, thus we cannot
### reject null or say there is a significant difference.
t.test(usedcars$price~usedcars$transmission, data=usedcars)

### One-Way ANOVA for Model. We can say there is a statistically significant
### variation between the means for model compared to price. 
oneway.test(usedcars$price~usedcars$model, data=usedcars)


## Additional Tools to consider
#### Data editor
pt_data1 <- edit(pt_data)
fix(pt_data1)


## Empirical Rule in R
### Find the amount of observations that fall within 1SD, 2SD, and 3SD of mean
attach(usedcars)
m = mean(price)
s = sd(price)
length(which(price > m-s & price < m+s))/length(price)
length(which(price > m-2*s & price < m+2*s))/length(price)
length(which(price > m-3*s & price < m+3*s))/length(price)
### The percentages approximately match the Normal Distribution, we can say that 
### the distribution of price is approximately normal.

### Q-Q Plot to determine theoretical distribution
qqnorm(usedcars$price)
qqnorm(usedcars$mileage)
