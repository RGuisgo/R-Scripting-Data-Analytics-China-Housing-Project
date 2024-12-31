library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(VIM)
library(moments)
install.packages("stargazer")
library(stargazer)
library(openxlsx)
#Importing the dataset
Housing_data_original <-read_excel("Housing_data_original.xlsx")
Housing_data_original <- data.frame(Housing_data_original)
View(Housing_data_original)



Housing_data_cleaned <-Housing_data_original[,c(1:15)] #Created a dataframe with the columns we need

# For this analysis, we're going to work with 6 variables: TotalPrice, Price, Square, BuildingType, BuildingStructure and District. 
# First, we check the variables we are going to use.

str(Housing_data_cleaned)
summary(Housing_data_cleaned$TotalPrice)
summary(Housing_data_cleaned$Square)
table(Housing_data_cleaned$BuildingType) #Checked for missing and unusable values, found 2
table(Housing_data_cleaned$RenovationCondition) #Checked for missing and unusable values, found none
table(Housing_data_cleaned$BuildingStructure) #Checked for missing and unusable values, found none

#Checking and Imputing the missing values

md.pattern(Housing_data_cleaned)
aggr_plot <- aggr(Housing_data_cleaned, col = c('navyblue', 'skyblue'),
                  numbers = T,
                  sortVars = T,
                  labels = names(Housing_data_cleaned),
                  cex.axis = .7,
                  gap = 3,
                  ylab = c("Histogram of missing data", "Pattern")) #Percentage of missing data is about 1%, so it is acceptable to impute data



#Converting BuildingType to a factor variable
Housing_data_cleaned_1 <- Housing_data_cleaned %>%
  mutate( BuildingType = as.factor(BuildingType) )

## imputing missing data using random forest decision tree to predict random values

Housing_data_imputed <- mice(Housing_data_cleaned_1, m= 5, method = "rf")
summary(Housing_data_imputed)
Housing_data_imputed$imp$BuildingType

# selection of  the dataset to work with after imputing

Housing_data_imputed_1 <- complete(Housing_data_imputed, 3)
table(Housing_data_imputed_1$BuildingType)

#Creating z-scores for the variables to check for outliers

Housing_data_imputed_1$TotalPrice_z <- ((Housing_data_cleaned_1$TotalPrice-mean(Housing_data_imputed_1$TotalPrice))/sd(Housing_data_imputed_1$TotalPrice))
boxplot(Housing_data_imputed_1$TotalPrice_z)
hist(Housing_data_imputed_1$TotalPrice_z) #Both boxplot and histogram show that there are outliers


Housing_data_imputed_1$Square_z <- ((Housing_data_cleaned_1$Square-mean(Housing_data_imputed_1$Square))/sd(Housing_data_imputed_1$Square))
boxplot(Housing_data_imputed_1$Square_z)
hist(Housing_data_imputed_1$Square_z) #Both boxplot and histogram show that there are outliers


##creating the outliers

Housing_data_imputed_1$outlier_Price <- ifelse(Housing_data_imputed_1$TotalPrice_z>3,1,0)
Housing_data_imputed_1$outlier_Square <- ifelse(Housing_data_imputed_1$Square_z>3,1,0)


## removing outliers


Housing_data_cleaned_2 <- Housing_data_imputed_1[Housing_data_imputed_1$outlier_Price == 0,]
Housing_data_cleaned_final <- Housing_data_cleaned_2[Housing_data_cleaned_2$outlier_Square == 0,]

setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/data analysis")
write.xlsx(Housing_data_cleaned_final, "Housing_Prices_China.xlsx")


boxplot(Housing_data_cleaned_final$TotalPrice_z)
hist(Housing_data_cleaned_final$TotalPrice_z)
summary(Housing_data_cleaned_final$TotalPrice)
boxplot(Housing_data_cleaned_final$TotalPrice)$stats[c(1, 5), ]

boxplot(Housing_data_cleaned_final$Square)
hist(Housing_data_cleaned_final$Square_z)
summary(Housing_data_cleaned_final$Square)
boxplot(Housing_data_cleaned_final$Square)$stats[c(1, 5), ]

#To remove outliers in the boxplot

Final0_data <- Housing_data_cleaned_final[Housing_data_cleaned_final$Price<102971, ]
boxplot(Final0_data$Price)
summary(Final0_data$Price)
hist(Final0_data$Price)

Final_data <- Final0_data[Final0_data$Square<144.74, ]
boxplot(Final_data$Square)
summary(Final_data$Square)
hist(Final_data$Square_z)



Housing_data_cleaned_final$TotalPrice = ifelse(Housing_data_cleaned_final$TotalPrice> 794 ,794 ,Housing_data_cleaned_final$TotalPrice) 
Housing_data_cleaned_final$Square = ifelse(Housing_data_cleaned_final$Square>144.73,144.73,Housing_data_cleaned_final$Square)
summary(Housing_data_cleaned_final$TotalPrice)
boxplot(Housing_data_cleaned_final$TotalPrice)
summary(Housing_data_cleaned_final$Square)
boxplot(Housing_data_cleaned_final$Square)

#Housing_data_cleaned_final is our final dataset. Missing values have been imputed and outliers have been removed.

#Descriptive statistics

lapply(Housing_data_cleaned_final[,c(2,3,4)],mean) #Mean
lapply(Housing_data_cleaned_final[,c(2,3,4)],median) #Median
lapply(Housing_data_cleaned_final[,c(2,3,4)],quantile) #Quartile
quantile(Housing_data_cleaned_final$TotalPrice, prob = c(0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) #Decile of TotalPrice
quantile(Housing_data_cleaned_final$Price, prob = c(0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) #Decile of Price
quantile(Housing_data_cleaned_final$Square, prob = c(0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) #Decile of Square
lapply(Housing_data_cleaned_final[,c(2,3,4)],sd) #Standard deviation
lapply(Housing_data_cleaned_final[,c(2,3,4)],moments::skewness) #skewness
lapply(Housing_data_cleaned_final[,c(2,3,4)],moments::kurtosis) #kurtosis


#Stratifying the data

summary(Housing_data_cleaned_final$TotalPrice)

Housing_data_cleaned_final$LowTPriced <- ifelse(Housing_data_cleaned_final$TotalPrice < 324.1, 1, 0)
Housing_data_cleaned_final$MidTPriced <- ifelse(Housing_data_cleaned_final$TotalPrice >= 324.1 & 
                                                  Housing_data_cleaned_final$TotalPrice <= 518.5, 1, 0)
Housing_data_cleaned_final$HighTPriced <- ifelse(Housing_data_cleaned_final$TotalPrice > 518.5, 1, 0)

summary(Housing_data_cleaned_final$Price)

Housing_data_cleaned_final$LowPriced <- ifelse(Housing_data_cleaned_final$Price < 42629, 1, 0)
Housing_data_cleaned_final$MidPriced <- ifelse(Housing_data_cleaned_final$Price >= 42629 & Housing_data_cleaned_final$Price <= 70320, 1, 0)
Housing_data_cleaned_final$HighPriced <- ifelse(Housing_data_cleaned_final$Price > 70320, 1, 0)

summary(Housing_data_cleaned_final$Square)
Housing_data_cleaned_final$SmallSized <- ifelse(Housing_data_cleaned_final$Square < 57.26, 1, 0)
Housing_data_cleaned_final$ModerateSized <- ifelse(Housing_data_cleaned_final$Square >= 57.26 & Housing_data_cleaned_final$Square <= 94.61, 1, 0)
Housing_data_cleaned_final$LargeSized <- ifelse(Housing_data_cleaned_final$Square > 94.61, 1, 0)

#(Stratification) Frequency Table for TotalPrice and Square

d1=ifelse(Housing_data_cleaned_final$TotalPrice < 324.1 & 
            Housing_data_cleaned_final$Square  <57.26, 1, 0)
table(d1)

d2=ifelse(Housing_data_cleaned_final$TotalPrice < 324.1 & 
            Housing_data_cleaned_final$Square>= 57.26 & 
            Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(d2)

d3=ifelse(Housing_data_cleaned_final$TotalPrice < 324.1 & 
            Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(d3)

d11=ifelse(Housing_data_cleaned_final$TotalPrice >= 324.1 & 
             Housing_data_cleaned_final$TotalPrice <= 518.5 & 
             Housing_data_cleaned_final$Square<57.26,1,0)
table(d11)

d22=ifelse(Housing_data_cleaned_final$TotalPrice >= 324.1 & 
             Housing_data_cleaned_final$TotalPrice <= 518.5 & 
             Housing_data_cleaned_final$Square>= 57.26 & 
             Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(d22)

d33=ifelse(Housing_data_cleaned_final$TotalPrice >= 324.1 & 
             Housing_data_cleaned_final$TotalPrice <= 518.5 &
             Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(d33)

d111=ifelse(Housing_data_cleaned_final$TotalPrice > 518.5 & 
              Housing_data_cleaned_final$Square < 57.26,1,0)
table(d111)

d222=ifelse(Housing_data_cleaned_final$TotalPrice > 518.5 & 
              Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(d222)

d333=ifelse(Housing_data_cleaned_final$TotalPrice > 518.5 & 
              Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(d333)


M <- as.table(rbind(c(27, 26, 1), c(24, 58, 26), c(4,27,27)))
dimnames(M) <- list(Price = c("Lowpriced", "MidPriced","HighPriced"),
                    Square = c("Small","Moderate", "Large"))
M

#(Stratification) Frequency table for Price and Square

t1=ifelse(Housing_data_cleaned_final$Price < 42629 & 
            Housing_data_cleaned_final$Square  <57.26, 1, 0)
table(t1)

t2=ifelse(Housing_data_cleaned_final$Price < 42629 & 
            Housing_data_cleaned_final$Square>= 57.26 & 
            Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(t2)

t3=ifelse(Housing_data_cleaned_final$Price < 42629 & 
            Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(t3)

t11=ifelse(Housing_data_cleaned_final$Price >= 42629 & 
             Housing_data_cleaned_final$Price <= 70320 & 
             Housing_data_cleaned_final$Square<57.26,1,0)
table(t11)

t22=ifelse(Housing_data_cleaned_final$Price >= 42629 & 
             Housing_data_cleaned_final$Price <= 70320 & 
             Housing_data_cleaned_final$Square>= 57.26 & 
             Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(t22)

t33=ifelse(Housing_data_cleaned_final$Price >= 42629 & 
             Housing_data_cleaned_final$Price <= 70320 &
             Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(t33)

t111=ifelse(Housing_data_cleaned_final$Price > 70320 & 
              Housing_data_cleaned_final$Square < 57.26,1,0)
table(t111)

t222=ifelse(Housing_data_cleaned_final$Price > 70320 & 
              Housing_data_cleaned_final$Square>= 57.26 & 
              Housing_data_cleaned_final$Square <= 94.61, 1, 0)
table(t222)

t333=ifelse(Housing_data_cleaned_final$Price > 70320 & 
              Housing_data_cleaned_final$Square > 94.61, 1, 0)
table(t333)


T <- as.table(rbind(c(8, 29, 17), c(23, 55, 29), c(24,23,0)))
dimnames(T) <- list(Price = c("Lowpriced", "MidPriced","HighPriced"),
                    Square = c("Small","Moderate", "Large"))
T

# (Stratification) Price, Square and Total Price by District, BuildingType and BuildingStructure


one=by(Housing_data_cleaned_final[,c(2,3,4)],Housing_data_cleaned_final$District, summary)

#str(one)

stargazer(as.data.frame(one[1:27]), summary=FALSE, rownames=FALSE)

by(Housing_data_cleaned_final[,c(2,3,4)],Housing_data_cleaned_final$BuildingType,summary)
by(Housing_data_cleaned_final[,c(2,3,4)],Housing_data_cleaned_final$BuildingStructure,summary)

#Graphs for stratification 

df1 <- Housing_data_cleaned_final %>% mutate(PriceRanges =
                                               case_when(Housing_data_cleaned_final$TotalPrice < 324.1 ~ "LowPriced", 
                                                         Housing_data_cleaned_final$TotalPrice >= 324.1 & 
                                                           Housing_data_cleaned_final$TotalPrice <= 518.5 ~ "MidPriced",
                                                         Housing_data_cleaned_final$TotalPrice > 518.5 ~ "HighPriced")
)

df2 <- df1 %>% mutate(SquareRanges = case_when(Housing_data_cleaned_final$Square < 57.26 ~ "SmallSized", 
                                               Housing_data_cleaned_final$Square >= 57.26 & 
                                                 Housing_data_cleaned_final$Square <= 94.61 ~ "ModerateSized",
                                               Housing_data_cleaned_final$Square > 94.61 ~ "LargeSized"))

df2 %>% 
  ggplot(aes(PriceRanges), col = District)+
  geom_bar()+
  facet_wrap(~ SquareRanges)+
  theme_bw()+
  labs(title = "Frequency chart of Price ranges by Squared area ranges",
       x= "Price ranges",
       y= "Frequency")

df2 %>% 
  ggplot(aes(Square, TotalPrice))+
  geom_point()+
  geom_smooth(method= lm, se= T)+
  facet_wrap(~ District)+
  theme_bw()+
  labs(title = "Scatter plot of Area by Price (stratified by districts)",
       x= "Area",
       y= "Price")

df2 %>% 
  ggplot(aes(Square, TotalPrice))+
  geom_point()+
  geom_smooth(method= lm, se= T)+
  facet_wrap(~ BuildingType)+
  theme_bw()+
  labs(title = "Scatter plot of Area by Price (stratified by BuildingType)",
       x= "Area",
       y= "Price")

df2 %>% 
  ggplot(aes(Square, TotalPrice))+
  geom_point()+
  geom_smooth(method= lm, se= T)+
  facet_wrap(~ BuildingStructure)+
  theme_bw()
labs(title = "Scatter plot of Area by Price (stratified by BuildindStructure)",
     x= "Area",
     y= "Price")

#ggplots

ggplot(Housing_data_cleaned_final,aes(BuildingType, TotalPrice))+
  geom_boxplot()+
  geom_point()+
  theme_bw()+
  labs(title = "TotalPrice by BuildingType (with data points)")

ggplot(Housing_data_cleaned_final,aes(BuildingStructure, TotalPrice))+
  geom_boxplot()+
  geom_point()+
  theme_bw()+
  labs(title = "TotalPrice by BuildingStructure (with data points)")

ggplot(Housing_data_cleaned_final,aes(District, TotalPrice))+
  geom_boxplot()+
  geom_point()+
  theme_bw()+
  labs(title = "TotalPrice by District (with data points)")

ggplot(data = Housing_data_cleaned_final, mapping = aes(x = Square, y = log(Price))) +
  geom_point()+
  geom_smooth(method = lm)

#Figure 1
ggplot(data = Housing_data_cleaned_final, mapping = aes(x = Square, y = Price)) +
  geom_col(alpha = 0.7)+
  theme_bw()+
  labs(title = "Column chart: Price by Square Area",
       caption = "Figure 1")

#Figure 2
Housing_data_cleaned_final %>% 
  ggplot(aes(log(Price), fill = BuildingType))+
  geom_histogram(binwidth = 830, alpha = 0.7, color = "black")+
  theme_bw()+
  facet_wrap(~ BuildingType)+
  labs(title = "Histogram: Log(Price) by building type",
       y = "Frequency",
       caption = "Figure 2")

Housing_data_cleaned_final %>% 
  ggplot(aes(log(Price), fill = District))+
  geom_histogram(binwidth = 830, alpha = 0.7, color = "black")+
  theme_bw()+
  facet_wrap(~ District)+
  labs(title = "Histogram: Log(Price) by Locality",
       y = "Frequency",
       caption = "Figure 2")

#Figure 3
Housing_data_cleaned_final %>% 
  ggplot(aes(x = Square, y = log(Price), color = BuildingType)) +
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  geom_line()+
  facet_wrap(~ BuildingType)+
  theme_minimal()+
  theme(panel.grid.major = element_blank())+
  labs(title = "Effect of Square Area on Price",
       caption = "Figure 3")

#Figure 4
Housing_data_cleaned_final %>% 
  ggplot(aes(District, log(Price)))+
  geom_col(aes(col = District))+
  theme_bw()

#Figure 5
Housing_data_cleaned_final %>% 
  ggplot(aes(BuildingType, log(Price)))+
  geom_col(aes(col = BuildingType))+
  theme_bw()
table(Housing_data_cleaned_final$District)


# confidence interval estimation (sample number larger than 30, use z test)
# 95% CI of housing total price
samplemean_totprice <- mean(Housing_data_cleaned_final$TotalPrice)
margin_of_error_totprice <- qnorm(0.975)*sd(Housing_data_cleaned_final$TotalPrice)/sqrt(216)
intlow_totprice <- samplemean_totprice - margin_of_error_totprice
intup_totprice <-samplemean_totprice + margin_of_error_totprice
c(intlow_totprice,intup_totprice)

# 95% CI of square meter of the house 
samplemean_square <- mean(Housing_data_cleaned_final$Square)
margin_of_error_square <- qnorm(0.975)*sd(Housing_data_cleaned_final$Square)/sqrt(216)
intlow_square <- samplemean_square - margin_of_error_square
intup_square <-samplemean_square + margin_of_error_square
c(intlow_square,intup_square)

library (gmodels) 
# crosstabs of building type and building structure (qualitative variables)
CrossTable (Housing_data_cleaned_final$BuildingType, Housing_data_cleaned_final$BuildingStructure, 
            chisq = T, sresid = T, 
            fisher = T, 
            format = "SPSS") 


# ANOVA between District and Price
library(ggpubr)
ggboxplot(Housing_data_cleaned_final, x = "District", y = "TotalPrice",color = "District",
          ylab = "TotalPrice", xlab = "District", main = "Total Price and District")
anova <- aov (Housing_data_cleaned_final$Price ~ Housing_data_cleaned_final$District) 
summary (anova) 

# remove all columns with same values (to avoid NA values in correlation)
remove_kitchen <- Housing_data_cleaned_final[,-7]
remove_proverty_elevator <- remove_kitchen[,-(12:13)]
remove_last_4 <- remove_proverty_elevator[,-(13:16)]
remove_time <- remove_last_4[,-9]
# remove id -- no meaning of Id so we cannot add this variable into the analysis
remove <-remove_time[,-1]
# correlation and covariance
library(ggcorrplot)
num <- sapply (remove, is.numeric) 
cov(Housing_data_cleaned_final$TotalPrice,Housing_data_cleaned_final$Square)
cor(Housing_data_cleaned_final$TotalPrice,Housing_data_cleaned_final$Square)
# correlation between total price and square
pricetot <-Housing_data_cleaned_final$TotalPrice
area <- Housing_data_cleaned_final$Square

ggplot(data=Housing_data_cleaned_final, aes(x=area, y=pricetot)) +
  geom_point()+stat_smooth(method="lm") +
  labs(title = "Correlation between Total Price and Area")


# correlation between all numeric variables
cor (remove [, num ]) 
ggcorrplot (cor(remove [, num]) , 
            method = "square", 
            lab = T, 
            hc.order = T,
            title = "Correlation between Variables") 

# rational hypothesis 

# 1. Test the statement that the average housing price of Beijing in 2018 is smaller than 59868 yuan
# H0: the average housing price of Beijing in 2018 is larger than or equal 59868 yuan 
# H1: the average housing price of Beijing in 2018 is less than 59868 yuan 
price <- Housing_data_cleaned_final$TotalPrice
t.test(price, alternative = "less", mu = 59868)
# p value is lower than 0.05 , which means we can reject H0 and accept H1

# 2. Building type and total price 
table(df2$BuildingType)
df2$BTCombination <- ifelse(df2$BuildingType == "Combination of Plate and Tower", 1,0)
df2$BTPlate <- ifelse(df2$BuildingType == "Plate", 1,0)
df2$BTTower<- ifelse(df2$BuildingType == "Tower", 1,0)

tot_combina<- t.test ( df2$TotalPrice ~ df2$BTCombination , data = df2 , 
                       var.equal = F, alternative = "less" )
tot_combina

tot_plate <- t.test ( df2$TotalPrice ~ df2$BTPlate , data = df2 , 
                            var.equal = F, alternative = "less" )
tot_plate

tot_tower <- t.test ( df2$TotalPrice ~ df2$BTTower , data = df2 , 
                      var.equal = F, alternative = "less" )
tot_tower

# 3. price square meter and locality
# chi test to test the relationship between price and district
# price range
Housing_data_cleaned_final$price_level[Housing_data_cleaned_final$Price<50000]='low price'
Housing_data_cleaned_final$price_level[Housing_data_cleaned_final$Price>=50000 & Housing_data_cleaned_final$Price<100000]='middle price'
Housing_data_cleaned_final$price_level[Housing_data_cleaned_final$Price>100000]='high price'

table1 <- table(Housing_data_cleaned_final$District, Housing_data_cleaned_final$price_level)
# chi-test
chisq<-chisq.test(table1)
chisq # price do have a relationship with district 
# check strength of the relationship
assocstats(table1) 
# Contingency Coeff.: 0.591
# Cramer's V        : 0.518
# the above 2 values are high, which means the relationship is strong

# check detailed relation
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

