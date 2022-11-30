library(tidyverse)
Prices <- read_csv(file = "https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv", sep)
# there's no commas in this 'CSV' file so it doesn't work
# read.csv("", sep = "\t")
data <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")
# read.table 
 


# dat %>%
  # mutate(Ddate = as.Date(dat$DocumentDate))

cor(data$BldgGrade, data$SalePrice)
plot(data$BldgGrade, data$SalePrice)

hist(data$ZipCode, data$SalePrice)

#to do : find mean sale price for each zip code

cor(data$AdjSalePrice, data$SalePrice)
A <- mean(data$SalePrice)

Zip <- as.factor(data$ZipCode)
plot(Zip, data$SalePrice)


### wk 9 tutorial step through ###

install.packages("stargazer")
library(stargazer)



data %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

zip_group <- data %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- data %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + as.factor(BldgGrade) + as.factor(ZipGroup), data = dat)
summary(mod4)

sresid <- rstandard(mod4) # the rstandard() function extracts standardised residuals
index <- order(sresid) # make an index of standardised residuals
dat[index[1:5], c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", "Bedrooms", "BldgGrade")]


### for year built ;

lastconstruction <- replace(dat$YrRenovated, dat$YrRenovated == 0, dat$YrBuilt)
view(lastconstruction)

data1 <- cbind(dat, lastconstruction)


data1 %>%
  group_by(lastconstruction) %>%
  mutate(av_price = mean(AdjSalePrice)) %>%
  arrange(lastconstruction) %>%
  ggplot(aes(lastconstruction, av_price)) +
  geom_line()

mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + as.factor(BldgGrade) + as.factor(ZipGroup) + as.factor(lastconstruction),  data = data1)

summary(mod5)
           


