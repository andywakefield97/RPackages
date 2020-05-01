avocado <- read.csv("avocado.csv", header = TRUE)

plot(avocado$AveragePrice)
plot(avocado$Total.Volume)
plot(avocado$AveragePrice, avocado$Date)

summary(avocado)

plot(avocado$AveragePrice, avocado$Total.Volume,
     col = 'red',
     main = "Volume vs. Average price",
     pch =19)

curve(dnorm(x, mean = mean(avocado$AveragePrice), sd=sd(avocado$AveragePrice)),
      col = 'thistle4',
      lwd =2,    
      add = TRUE) 


curve(dnorm(x, mean = mean(avocado$Total.Volume), sd=sd(avocado$AveragePrice)),
      col = 'thistle4',
      lwd =2,    
      add = TRUE) 

library(ggplot2)

ggplot(avocado, aes(x=type, y=AveragePrice)) + geom_col()

ggplot(avocado, aes(x=year, y=AveragePrice)) + geom_col()

?ggplot2

View(avocado)

qplot(aes(x=type, y=AveragePrice), data=avocado) 

format(avocado$Date, format='%Y')
format(avocado$year, format='%Y')

qplot(year, AveragePrice, data=avocado)
qplot(type, AveragePrice, data=avocado)


qplot(type, AveragePrice, data=avocado, color=year)

qplot(type, AveragePrice, data=avocado, geom=c("point", "smooth"))

qplot(Date, AveragePrice, data=avocado, color=type)

qplot(type, data=avocado, color=year)

qplot(type, AveragePrice, data=avocado, facets = .~year, color=year)

qplot(region == 'SanFrancisco', Total.Volume, data=avocado, color = year)
