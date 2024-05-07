library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(ggridges)
library(factoextra)
library(GGally)
library(ISLR)
library(knitr)

baseball <- read.csv("datasets/500hits.csv")

baseball %>% glimpse()

# data cleaning 

baseball_clean <- 
  baseball %>% 
  select(-HR, -HOF)

baseball_clean %>% glimpse()
baseball_clean %>% summary()

# linear regression

model <- lm(R ~ YRS + G + AB + H + X2B + X3B + RBI + BB + SO + SB + CS + BA, data = baseball_clean)
summary(model)
# YRS, G, AB, X3B, RBI, BB, SO, SB, CS are most relevant

# scatterplot
ggplot(baseball_clean, aes(x = R, y = RBI)) +
  geom_point() +
  ggtitle("Runs vs RBI") +
  xlab("Runs") +
  ylab("RBIs")+
  theme_minimal()


# boxplot
ggplot(baseball_clean, aes(x = R, y = X3B)) + 
  geom_boxplot(fill = "blue", color = "red") + 
  ggtitle("Boxplot Runs vs Triples") + 
  xlab("Runs") + 
  ylab("Triples") + 
  theme_minimal()
