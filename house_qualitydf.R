library(VIM)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(MASS)
library(mice)
library(knitr)
library(fastDummies)
mice::md.pattern(data)

data= read.csv('train.csv')

head(data)

# turn column names to lower case
names(data) = tolower(names(data))
names(data)

# create house_qualities dataframe 
house_qualities = data %>% dplyr::select(id,x1stflrsf,x2ndflrsf,lowqualfinsf,grlivarea,bsmtfullbath,bsmthalfbath,
                                         fullbath,halfbath,bedroomabvgr,kitchenabvgr,kitchenqual,totrmsabvgrd,functional,
                                         fireplaces,fireplacequ,yearbuilt,saleprice)

# creating square feet column

house_qualities = house_qualities %>% dplyr::mutate(house_sf = x1stflrsf+x2ndflrsf)
house_qualities = house_qualities %>% dplyr::mutate(grlivarea_check = house_sf-grlivarea)
head(house_qualities)

house_qualities[order(house_qualities$grlivarea_check),]

# determined that grlivearea includes sf of house that are not "lowqual"- most houses do not have lowqual

# creating total half bath and full bath columns

house_qualities = house_qualities %>% dplyr::mutate(tot_halfbath = halfbath + bsmthalfbath, tot_fullbath = 
                                                      fullbath + bsmtfullbath)

# creating extra rooms column

house_qualities = house_qualities %>% dplyr::mutate(extra_rooms = totrmsabvgrd 
                                                       - kitchenabvgr - bedroomabvgr)

house_qualities[order(house_qualities$extra_rooms),]

#fireplacequ investigation- fair and good only have $800 difference in regression, dropping variable- run T-test
plot(data$fireplacequ)

model.saturated = lm(log(saleprice) ~., data = house_qualities)
summary(model.saturated)
t.test()

model.saturated$coefficients

# fireplace variable not significant in above, turning to 1,0 variable

house_qualities = house_qualities %>% dplyr::mutate(fireplace_binary = ifelse(fireplaces>0,1,0))

# creating dataframe to be combined into main dataframe

house_qualities_final = house_qualities %>% dplyr::select(id,lowqualfinsf,grlivarea,tot_halfbath,tot_fullbath,extra_rooms,
                                                          fireplace_binary,functional,kitchenabvgr,kitchenqual)


# dummify variables
house_qualities_final = dummy_cols(house_qualities_final,select_columns = c('kitchenqual','functional'))

head(house_qualities_final)

# get rid of extraneaous non-dummified columns
house_qualities_final = subset(house_qualities_final,select = -c(functional,kitchenqual))

names(house_qualities_final)

nrow(house_qualities_final)

write.csv(house_qualities_final,'\\house_qualities.final.csv')
