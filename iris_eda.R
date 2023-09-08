# Load libraries
library(dplyr)
library(GGally)
library(ggplot2)
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

# Scatter Plot
ggplot(iris) +
  geom_point(aes(x=Sepal.Width, y=Sepal.Length), color="red") +
  labs(title = "Sepal Width vs Sepal Length")

ggplot(iris)+
  geom_point(aes(x=Petal.Width, y=Petal.Length), color="blue") +
  labs(title = "Petal Width vs Petal Length")

# Histogram
ggplot(iris) +
  geom_histogram(aes(x=Sepal.Width), fill = "purple") +
  labs(title = "Sepal Width")

ggplot(iris) +
  geom_histogram(aes(x=Sepal.Length), fill = "orange") +
  labs(title = "Sepal Length")

ggplot(iris) +
  geom_histogram(aes(x=Petal.Width), fill = "cyan") +
  labs(title = "Petal Width")

ggplot(iris) +
  geom_histogram(aes(x=Petal.Length), fill = "green") +
  labs(title = "Petal Length")

# Relationship Sepal and Species
ggplot(iris) +
  geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +
  facet_wrap(~Species) +
  labs(title = "Relationship Sepal and Species")

# Relationship Petal and Species
ggplot(iris) +
  geom_point(aes(x=Petal.Width, y=Petal.Length, color=Species)) +
  facet_wrap(~Species) +
  labs(title = "Relationship Petal and Species")

# Pair Plot
ggpairs(iris,
        columns = 1:4,
        aes(color = Species),
        upper = list(continuous = "points", combo = "box_no_facet"),
        legend = 2,
        title = "Relationship Between Features By Species"
        )