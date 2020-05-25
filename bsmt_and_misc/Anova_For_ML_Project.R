ml_project_train

all_basement = ml_project_train %>% 
  select(contains("Bsmt"), "SalePrice") %>%
  replace_na(replace = list(BsmtQual = "None", 
                            BsmtCond = "None", 
                            BsmtExposure = "None", 
                            BsmtFinType1 = "None", 
                            BsmtFinType2 = "None")) %>%
  mutate(SalePrice = log(SalePrice)) %>%
 # mutate(BsmtFinSF1 = log(BsmtFinSF1)) %>%
  rename("LogSalePrice" = "SalePrice") %>%
  mutate(TotalBsmtBath = BsmtFullBath + BsmtHalfBath) %>%
  mutate(BsmtHasBath = ifelse(TotalBsmtBath > 0, "Yes", "No")) %>%
  filter(BsmtFinType2 != "None") 
 # filter(BsmtFinType1 != "GLQ", BsmtFinType1 != "None", BsmtFinType1 != "Unf")

all_basement_tester = all_basement %>%
  mutate(LogSalePrice = e^LogSalePrice) %>%
  rename(SalePriceTest = LogSalePrice) %>%
  group_by(BsmtCond) %>%
  summarize(mean(SalePriceTest), 
            median(SalePriceTest), sd(SalePriceTest))

boxplot(all_basement$LogSalePrice ~ all_basement$BsmtCond)

summary(aov(all_basement$LogSalePrice ~ all_basement$BsmtCond))

test = ggplot(all_basement, aes(x = TotalBsmtSF, y = LogSalePrice)) + 
  geom_point()
test

test_model = lm(all_basement$LogSalePrice ~ all_basement$TotalBsmtSF)
test_model
summary(test_model)

summary(all_basement$BsmtFinType2)

all_basement_numerics = all_basement %>%
  select(BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, LogSalePrice)

cor(all_basement_numerics)

#BsmtQual matters on Log Sale Price, they're all pretty different
#BsmtCond matters, they're all pretty different
#BsmtExposure matters, they're all pretty similar except for "none"
#but if you take "None" out the Anova still holds
#BsmtFinType1 matters, they're all pretty different
#BsmtFinType2 matters, they're all pretty similar except for "none"
#and when you take out "none" there's no significant difference.
#Maybe that variable becomes an "all or none" type situation
#BsmtFullBath matters, but similar to BsmtFinType2 - it's all or none
#take out 0, Anova crumbles. Maybe another "all or none"
#BsmtHalfBath does not matter. Maybe combine with Basement Fullbath
#Very low correlation between SalePrice and BsmtFinSF1 (R^2 = .2)
#Remove or combine that one with Total SF
#Even lower with BsmtFinSF2 (R^2 = .04). Combine with BsmtFinSF1 for sure
#Lower on BsmtUnfSF (R^2 = .03). Thankfully there's a "total" variable
#Pretty low on TotalBsmtSF (R^2 = .37). Pass it along to total SF

#TotalBsmtBath matters, but as an all or one
#BsmtHasBath solves this 

all_misc = ml_project_train %>%
  select('MiscFeature', "MiscVal", 
         "MoSold", "YrSold", 
         "SaleType", "SaleCondition", 
         "SalePrice") %>%
  replace_na(replace = list(MiscFeature = "None")) %>%
  mutate(SalePrice = log(SalePrice)) %>%
  rename("LogSalePrice" = "SalePrice") %>%
  mutate(SaleNormal = ifelse(SaleType == "WD", "Yes", "No")) %>%
  mutate(ConditionNormal = ifelse(SaleCondition == "Normal", "Yes", "No")) %>%
  mutate(WeirdDate = YrSold*MoSold)

boxplot(all_misc$LogSalePrice ~ all_misc$WeirdDate)

summary(aov(all_misc$LogSalePrice ~ all_misc$MiscFeature))

all_misc_tester = all_misc %>%
  mutate(LogSalePrice = e^LogSalePrice) %>%
  rename(SalePriceTest = LogSalePrice) %>%
  group_by(MiscFeature) %>%
  summarize(mean(SalePriceTest), 
            median(SalePriceTest), sd(SalePriceTest))
dim(all_misc_tester)
