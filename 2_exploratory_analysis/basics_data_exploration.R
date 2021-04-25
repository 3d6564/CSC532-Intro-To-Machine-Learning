# Introduction to Statistical Learning - ISLR
# Lab: Introduction to R - Basics
x <- c(1,3,2,5)
x

x = c(1,6,2)
y = c(1,4,3)

# x and y are the same length and can be added together. Length of object can 
# be verified with length().
x + y

# List all objects
ls()
# Remove objects
rm(x, y)

# Functions on a matrix returns for each element of vector or matrix.
x = matrix(c(1:4), nrow=2)
sqrt(x)
x^2
rm(x)

# Create object of standard normal random variables with a mean of 0 and
# standard deviation of 1. Create an object of standard normal random variables
# with a mean of 50 and a standard deviation of .1.
x = rnorm(50)
y = x + rnorm(50, mean=50, sd=.1)
cor(x,y)

# Set seed to perform calculations and get a repeatable set of random variables.
set.seed(3)

y = rnorm(100)

# mean(y) = .0110
mean(y)

# var(y) = .7329
var(y)

# Sqrt(var(y)) = sd(y)
# sqrt(var(y)) = .8561
sqrt(var(y))

# sd(y) = .8561
sd(y)

# Graphics and plotting of basic statistics
x = rnorm(100)
y = rnorm(100)
plot(x,y)

# Adjust labels on plot and sending to pdf
pdf("2_exploratory_analysis/figure.pdf")
plot(x,y, xlab="X-Axis", ylab="Y-Axis", main="Plot of X and Y")

# Contour function to create three-dimensional plots. nlevels will not allow
# sending plot to a pdf.
x = seq(-pi, pi, length=50)
y = x
f = outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f, nlevels=45, add=T)
fa=(f-t(f))/2
contour(x,y,fa, nlevels=15)

# Image function to create a heatmap
image(x,y,f, nlevels=45, add=T)
image(x,y,fa, nlevels=15)
dev.off()

# Indexing data in a matrix.
A = matrix(1:16, 4, 4)
A

# Select single item. This example shows the item in row 2 from column 3.
A[2,3]

# Select multiple items. This example shows rows 1-3 from column 4.
A[c(1:3), 4]


# Loading data from a file. Check data if it has a header row or other issues.
auto = read.table("0_data/Auto.data", header=T, na.strings="?")
fix(auto) # view data in spreadsheet window of Rstudio
dim(auto)

auto[1:4,]

# Remove rows with na values. In this dataset, 5 records have na values.
auto = na.omit(auto)
dim(auto)

# Graphical and Numerical Summaries of data. Attach() will allow referencing
# of variables by name, without mentioning data frame.
plot(auto$cylinders, auto$mpg)
attach(auto)
plot(cylinders, mpg)

# Adjust cylinders from a quantitative to qualitative variable. This changes
# the plot to a boxplot and provides more meaning.
cylinders = as.factor(cylinders)
plot(cylinders, mpg)

# Adjust parameters for a boxplot. Varwidth=T is proportionate box width.
plot(cylinders, mpg, col="red", varwidth=T, xlab="Cylinders", ylab="MPG")

# Histograms
hist(mpg, col=2, breaks=15)

# Scatterplot for pairs of variables in data frame. 
pairs(auto) # all pairs
pairs(~ mpg + displacement + horsepower + weight + acceleration, auto)

# Identify can produce an interactive plot
plot(horsepower, mpg)
identify(horsepower, mpg, name)

# Summary statistics for variables
summary(auto)
