
## Factor Analysis in R
## Exercise R script

## Import data:

setwd("~/Desktop/factor_analysis_R_workshop/")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lavaan, vtable, 
               psych, scales, corrplot, 
               ggthemes, ggcharts, patchwork)

groups <- read_csv("data/fa_exercise_dataset.csv", show_col_types = F)

## Examine variables:
vtable(groups)

## EFA:

## Omit NA or missing values:
groups <- na.omit(groups)

## Examine correlation matrix:
mtx01 <- cor(groups[, c(1:6)])
mtx01

## Visualize correlation matrix:

corrplot(mtx01, 
         method = "number", 
         number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))


## Bartlett's test of sphericity:
cortest.bartlett(groups)

## Kaiser-Meyer-Olkin factor adequacy test
KMO(r=cor(groups))

parallel <- fa.parallel(groups)

## Based on the scree plot, factor analysis with two factors is the most appropriate. 
## We will proceed with promax rotation, which assumes that the items are inter-correlated 
## (that is, not independent from each other).

## Two factor model:
fit02 <- factanal(groups, 2, rotation="promax")
fit02

## Three factor model:
fit03 <- factanal(groups, 3, rotation="promax")
fit03

## Four factor model:
fit04 <- factanal(groups, 4, rotation="promax")
fit04

## Often times, real world data can be messy! 

## How would you interpret these FA results? How many factors should we retain?

## To do:

## After you justify the number of factors to retain, examine your CFA output. 




