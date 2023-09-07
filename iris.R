# Install Required Packages
install.packages("tidyverse")
install.packages("GGally")

# Load IRIS Dataset
library(datasets)
iris <- datasets::iris

View(iris)

head(iris, 5)
tail(iris, 5)

# Summary descriptive Stats
summary(iris)

# To check for missing values
sum(is.na(iris))


# Statistics grouped by Species
library(dplyr)

iris %>% 
  group_by(Species) %>% summarize(
    mean(Sepal.Length), sd(Sepal.Length), min(Sepal.Length), max(Sepal.Length)
    )

iris %>% 
  group_by(Species) %>% summarize(
    mean(Sepal.Width), sd(Sepal.Width), min(Sepal.Width), max(Sepal.Width)
    )

iris %>% 
  group_by(Species) %>% summarize(
    mean(Petal.Length), sd(Petal.Length), min(Petal.Length), max(Petal.Length)
  )

iris %>% 
  group_by(Species) %>% summarize(
    mean(Petal.Width), sd(Petal.Width), min(Petal.Width), max(Petal.Width)
  )

################
# Data Visualization

plot(iris, col = "blue")

library(ggplot2)

# Scatter Plot
ggplot(iris) +
  geom_point(aes(x=Sepal.Width, y=Sepal.Length), color="red")

ggplot(iris)+
  geom_point(aes(x=Petal.Width, y=Petal.Length), color="blue")

# Histogram
ggplot(iris) + geom_histogram(aes(x=Sepal.Width), fill = "purple")

ggplot(iris) + geom_histogram(aes(x=Sepal.Length), fill = "orange")

ggplot(iris) + geom_histogram(aes(x=Petal.Width), fill = "cyan")

ggplot(iris) + geom_histogram(aes(x=Petal.Length), fill = "green")

# Relationship Sepal and Species
ggplot(iris) +
  geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +
  facet_wrap(~Species)

# Relationship Petal and Species
ggplot(iris) +
  geom_point(aes(x=Petal.Width, y=Petal.Length, color=Species)) +
  facet_wrap(~Species)
