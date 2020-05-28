install.packages("tree")
install.packages("ISLR")
install.packages("randomForest")
library(tree)
library(ISLR)
library(randomForest)
library(readr)

#get dataframe

compiled <- read_csv("~/NYCDSA/compiled_features.csv")
View(compiled)

compiled = compiled %>%
  select(-"BsmtScore", -"garagetype_NA", -"garagefinish_NA", -"X1")

#train/test split (80/20)

set.seed(0)
train1 = sample(1:nrow(compiled), 8 * nrow(compiled)/10)

tree.compiled = tree(compiled$LogSalePrice ~ ., data = compiled, subset = train1)
summary(tree.compiled)





