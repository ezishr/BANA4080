
# Determing data type -------------------------------------------------------------

num <- 2.2
char <- "hello world"
logi <- TRUE

# return the class of the data type
class(num)
class(char)
class(logi)

# explicitly check for a data type
is.numeric(num)
is.character(num)
is.character(char)
is.logical(logi)


# Type coercion -------------------------------------------------------------------

class(num)
num_char <-  as.character(num)
num_char

class(num_char)
class(char)
char_num <- as.numeric(char)

bool <- c(TRUE, TRUE, FALSE)
bool
as.numeric(bool)
sum(bool)


# Your Turn! ----------------------------------------------------------------------

# 1. Check out the built-in object pi. What class is this object?

# 2. What happens when you coerce this object to a character?

# 3. What happens when you coerce it to a logical?

# 4. Is there a coercion function that could convert this to an integer? What happens
#    when you do this?



# Matrices ------------------------------------------------------------------------

# numeric matrix
set.seed(123)
v1 <- sample(1:10, 25, replace = TRUE)
m1 <- matrix(v1, nrow = 5)
m1

# character matrix
m2 <- matrix(letters[1:9], nrow = 3)
m2


# Matrix applications -------------------------------------------------------------

# correlation matrix
cor(mtcars)[, 1:5]

# matrix multiplication
m3 <- matrix(1:6, nrow = 2)
m4 <- matrix(6:11, ncol = 2)
m3 %*% m4


# Matrix indexing -----------------------------------------------------------------

m5 <- matrix(1:15, nrow = 3)

# first element, third colum
m5[1, 3]

# first two rows for all columns
m5[1:2, ]

# first two columns for all rows
m5[, 1:2]

## nhap--------------------------
letters
matrix(letters[1:9], ncol = 3)
matrix(letters, ncol=5)

sample(1:9, 25, replace=TRUE)
m <- matrix(sample(1:9, 25, replace=TRUE), ncol = 5)
m[2,2:3]
m[2,c(2,4,5)]
m[2,]

# Matrix operators ----------------------------------------------------------------

# mean of all elements
mean(m1)

# standard deviation of all elements
sd(m1)

# compute the mean of each column
colMeans(m1)


# Your Turn! ----------------------------------------------------------------------

# 1. Check out the built-in VADeaths data matrix?
?VADeaths
# 2.Subset this matrix for only male death rates.
subset_male <- VADeaths[,c(1,3)]
subset_male2 <- VADeaths[,c('Rural Male','Urban Male')]
# 3. Subset for males death rates over the age of 60.
subset_male_60 <- VADeaths[3:nrow(VADeaths),c('Rural Male','Urban Male')]
# 4. Calculate averages for each column and row.
rowMeans(subset_male_60)
colMeans(subset_male_60)

# Data frames ---------------------------------------------------------------------

# ggplot provides the following mpg data frame
library(ggplot2)
mpg


# Data frame indexing -------------------------------------------------------------

# first 10 rows for 3 columns
mpg[1:10, c("manufacturer", "model", "cty")]

# preserve output as data frame
mpg["cty"]

# simplify output as a vector
mpg[["cty"]]
mpg$cty

mean(mpg$cty)
colMeans(mpg[c('cty','hwy')])

# Your turn! ----------------------------------------------------------------------

# 1. Check out the built-in mtcars data set. What type of object is this?
mtcars
typeof(mtcars)
# 2. Apply the head() and summary() functions to mtcars, what do these functions return?
head(mtcars, 5)
summary(mtcars)
nrow(mtcars)
# 3. Index for just the ‘mpg’ column in mtcars using three different approaches:
#    - single brackets [,
#    - double brackets [[, and
#    - dollar sign $.
#    How do the results differ?
mtcars['mpg']
mtcars[['mpg']]
mtcars$mpg
mean(mtcars$mpg)




# Lists ---------------------------------------------------------------------------

l1 <- list(item1 = 1:10, item2 = m1, item3 = mpg)
l1


# List indexing -------------------------------------------------------------------

# preserve output as a list
a <- l1["item1"]
is.list(a)
is.atomic(a)
a

# simplify output as a vector
b <- l1[["item1"]]
is.list(b)
is.atomic(b)
b

# simplify output as a vector
c <- l1$item1
is.list(c)
is.atomic(c)
c

# a linear model's results are in a list
model <- lm(mpg ~ ., data = mtcars)
str(model)

# get the first ten residuals
model[["residuals"]][1:10]

# get the coefficient for the wt variable
model$coefficients["wt"]




# In-class ----------------------------------------------------------------
library(readr)
library(here)
path <- 'Module 2/student data'
system.time(products <- read_csv(here('Module 2/student data','products.csv')))
## read.csv is slow and might manipulates our dataset

library(readxl)
## read sheets in a workbook
excel_sheets(path = here('Module 2/student data', 'products.xlsx'))

products_xl <- read_excel(here('Module 2/student data', 'products.xlsx'),sheet = 'products data')

transactions <- read_csv(here(path, 'transactions.csv'))
glimpse(transactions)
names(transactions)
sum(is.na(transactions))
View(transactions)


