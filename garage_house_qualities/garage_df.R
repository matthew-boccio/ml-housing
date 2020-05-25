library(VIM)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(MASS)
library(mice)
library(fastDummies)
mice::md.pattern(data)

data= read.csv('train.csv')

names(data)

names(data) = tolower(names(data))

garage = data %>% dplyr::select(id,garagetype,garageyrblt,garagefinish,garagecars,garagearea,garagequal,garagecond,
                                yearbuilt,yearremodadd,saleprice)

#very little variance in garagecond or garagequal, garage finish has variance, using that as variable to keep
plot(garage$garagecond)
plot(garage$garagequal)
plot(garage$garagefinish)

garage = garage %>% mutate(yeardelta = garageyrblt-yearbuilt)

# creating saturated model

model.saturated = lm(log(saleprice) ~., data = garage)
summary(model.saturated)

# creating final garage dataframe

garage_final = garage %>% dplyr::select(id,garagetype,garagefinish,garagecars)

#dummify variables

head(garage_final)

garage_final = dummy_cols(garage_final,select_columns = c('garagetype','garagefinish'))

garage_final = subset(garage_final, select = -c(garagetype,garagefinish))

head(garage_final)

write.csv(garage_final,'\\garage.final.csv')
