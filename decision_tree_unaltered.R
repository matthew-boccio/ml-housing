install.packages("tree")
install.packages("ISLR")
install.packages("randomForest")
library(tree)
library(ISLR)
library(randomForest)
library(readr)

#get data frame

train_df = read_csv("~/NYCDSA/ml_project_train.csv")

train_df = train_df %>%
  mutate(LogSalePrice = log(SalePrice)) %>%
  select(-"SalePrice")

#train test split (80/20)

train2 = sample(1:nrow(train_df), 8 * nrow(train_df)/10)

tree.train = tree(LogSalePrice ~ ., data = train_df, subset = train2)
summary(tree.compiled)
