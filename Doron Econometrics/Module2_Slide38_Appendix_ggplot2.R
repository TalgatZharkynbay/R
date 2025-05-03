#install.packages("ggplot2")
require(ggplot2)



### Get data from ggplot2 package for future analyses
data(mpg)
# cty = miles per galon city driving 
# hwy = miles in galon highway driving 
# class = type of car 
# displ =  the engine displeacement in liters 
unique(mpg$manufacturer)
head(mpg)



#### Example 1:
# Calling ggplot() alone just creates a blank plot
ggplot()

# Telling ggplot what data to use
ggplot(mpg, aes(displ, hwy))

# Adding points 
ggplot(mpg, aes(displ, hwy)) + geom_point()

# Adding colour, size, shape, and other aes attributes as well as points
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point()

# Adding points as well as a line
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() + geom_line()

# Adding points as well as a smooth line
ggplot(mpg, aes(displ, hwy))+ geom_point()+ geom_smooth()

# Adding points as well as a smooth line with span = 0.2
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span = 0.2)



#### Example 2:
### Ploting histograms using ggplot
ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(cty)) + geom_histogram(binwidth = 2.5)