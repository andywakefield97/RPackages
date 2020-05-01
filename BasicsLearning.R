library(datasets)
head(iris)

#BASIC PLOTS

plot(iris$Species)
plot(iris$Petal.Length)
plot(iris$Species, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width)
plot(iris)


plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",    #SPECIFY COLOUR OF POINTS
     pch = 19,     #USE SOLID CIRCLES FOR POINTS
     main = "Iris: Petal Length vs. Petal Width",   #TITLE OF PLOT
     xlab = "Petal Length",    #X AXIS LABEL
     ylab = "Petal Width")    #Y AXIS LABEL

#BAR CHARTS

head(mtcars)
cylinders <- table(mtcars$cyl)
barplot(cylinders)

#SCATTER GRAPHS

#HISTOGRAMS TO CHECK FOR UNIVARIATE DISTRIBUTION
hist(mtcars$wt) 
hist(mtcars$mpg)

plot(mtcars$wt, mtcars$mpg,
     col ="#cc0000",
     pch=10,
     cex=1.5,
     main ="MPG compared to Weight in cars")
 
#OVERLAY PLOTS

head(lynx)
?lynx

hist(lynx)

hist(lynx,
     breaks = 14,
     freq = FALSE,
     col = 'thistle1',
     main = "Lynx Histogram",
     xlab= "Number pf lynx trapped")

curve(dnorm(x, mean = mean(lynx), sd=sd(lynx)),
      col = 'thistle4',
      lwd =2,    #LINE WIDTH OF PIXELS
      add = TRUE)   #ADD TO THE PREVIOUS PLOT

#KERNAL DENSITY ESTIMATORS
lines(density(lynx), col ='blue', lwd=2)
lines(density(lynx, adjust=3), col='purple', lwd=2)

#RUG PLOT
rug(lynx, lwd=2, col='orange')


#SUMMARY

summary(iris$Species) #CATEGORICAL
summary(iris$Sepal.Length) #QUANTITATIVE
summary(iris) #ENTIRE DATAFRAME

#DESCRIBE

pacman::p_load(psych)

pacman::p_help(psych,web=F)

describe(iris$Sepal.Length)
describe(iris)

#SELECTING CASES

summary(iris$Species)
 
#ONLY SELECTING A SINGLE SPECIES  
hist(iris$Petal.Length[iris$Species == 'virginica'],
     main = 'Petal Length: Virginica')
hist(iris$Petal.Length[iris$Species == 'versicolor'],
     main = 'Petal Length: Versicolor')
hist(iris$Petal.Length[iris$Species == 'setosa'],
     main = 'Petal Length: Setosa')

#SELECT PETALS LESS THAN 2
hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = 'Petal Length less than 2')

#MULTIPLE SELECTORS
hist(iris$Petal.Length[iris$Species == 'virginica' & iris$Petal.Length < 5.5],
     main = 'Short Length: Virginica')

#CREATING A SAMPLE USING SELECTORS
i.setosa <- iris[iris$Species == 'setosa', ]
head(i.setosa)
summary(i.setosa$Petal.Length)

#DATA FORMATS

#VECTOR = 1+ NUMBERS IN A 1D ARRAY; ALL SAME DATA TYPE, R BASIC DATA OBJECT
#MATRIX = 2 DIMENSIONAL, SAME LENGTH, SAME DATA CLASS, COLUMNS NOT NAMED
#DATAFRAME = CAN HAVE VECTORS OF MULTIPLE TYPES, ALL SAME LENGTH, CLOSEST R GETS TO A SPREADSHEET(SPREADSHEETS = DF IN R)
#LIST = MOST FLEXIBLE, ORDERED COLLECTION OF ELEMENTS, ANY CLASS, LSNGTH OR STRUCTURE, CAN INCLUDE LISTS

#COERCION = CHANGING A DATA OBJECT FROM ONE TYPE TO ANOTHER

#DATA TYPES
n1 <- 15
n1
typeof(n1) 

n2 <- 1.5
n2
typeof(n2)

c1 <- 'c'
c1
typeof(c1)

c2 <- 'a string of text'
c2
typeof(c2)

l1 <- TRUE
l1
typeof(l1)

l2 <- F
l2
typeof(F)

#DATA STRUCTURES
#VECTORS
v1 <- c(1,2,3,4,5)
v1
is.vector(v1)

v2 <- c('a', 'b', 'c')
v2
is.vector(v2)

#MATRICES
m1 <- matrix(c('a', 'b', 'c', 'd'), nrow=2, byrow= TRUE)
m1

#ARRAY (DATA GIVEN THEN (ROWS, COLUMNS, TABLES))
a1 <- array(c(1:24), c(4,3,2))
a1

#DATA FRAME
vNumeric <- c(1,2,3)
vCharacter <- c('a', 'b', 'c')
vLogical <- c(T, F, T)

dfa <- cbind(vNumeric, vLogical, vCharacter)
dfa

df <- as.data.frame(cbind(vNumeric, vLogical, vCharacter))
df

#OBJECTS IN LISTS
o1 <- c(1,2,3)
o2 <- c('a', 'b', 'c')
o3 <- c(T, F, T)

list1 <- list(o1,o2,o3)
list1

#COERCION
(coerce1 <- c(1, 'b', T))
typeof(coerce1)

coerce2 <- 5
typeof(coerce2)

coerce3 <- as.integer(5)
typeof(coerce3)

#MATRIX TO DF
coerce4 <- matrix(1:9, nrow=3)
is.matrix(coerce4)

coerce5 <- as.data.frame(coerce4)
is.data.frame(coerce5)

#FACTORS

x1 <- 1:3
y <- 1:9

df1 <- cbind.data.frame(x1, y)
typeof(df1$y)
str(df1)

x2 <- as.factor(c(1:3))
df2 <- cbind.data.frame(x2, y)
typeof(df2$x2)
str(df2)

#CHANGE DF1 TO FACTORS WITH LABELS
df1$x1 <- factor(df1$x1, levels = c(1,2,3), labels = c('Mac', 'Windows', 'Linux'))
df1

#IMPORTING DATA INTO R
#RIO PACKAGE IMPORTS ALL DATA TYPES WITH CONSISTENT SYNTAX

#pacman :: p_load(rio)

rio_csv <- import("/Desktop/avocado.csv")
head(rio_csv)

rio_text <- import("/Desktop/avocado.txt")
head(rio_text)

rio_xlsx <- import("/Desktop/avocado.xlsx")
head(rio_xlsx)

View(rio_csv)

#REGRESSION

head(USJudgeRatings)
data <- USJudgeRatings

#DEFINE VARIABLE GROUPS
#EVERYTHING BUT 12TH COLUMN
x <- as.matrix(data[-12])

#ONLY USE 12TH COLUMN
y <- data[, 12]

reg1 <- lm(y~x)
reg1
summary(reg1)
anova(reg1)
coef(reg1)
