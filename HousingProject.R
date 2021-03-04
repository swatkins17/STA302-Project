

############ Mini Project Pt. 2 ############

# STA302; Winter 2021
# Sierra Watkins; 1005473685


############ Initial Setup
# load dataset housing.csv
prod <- read.csv(file.choose(), header=T)

# import ggmap
install.packages("mapproj")
library(mapproj)

# take sample of n = 250
set.seed(1005473685)
rows <- sample(1:nrow(prod), 250, replace=FALSE) 
housing <- prod[rows,]

# create new variables
housing$roomsperhouse <- housing$total_rooms / housing$households
housing$bedroomsperhouse <- housing$total_bedrooms / housing$households
housing$peopleperhouse <- housing$population / housing$households

# inspect data
summary(housing)

pairs(housing[,2:10], lower.panel=NULL)


############ map
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
map("state", xlim=c(-130,-110),ylim=c(30,60), col="gray90", fill=TRUE) 
points(housing$longitude, housing$latitude, pch=10, col="red", cex=0.2)  #plot my sample sites

############ age and value
modelAge <- lm(median_house_value ~ housing_median_age, data = housing)
summary(modelAge)
plot(housing$median_house_value ~ housing$housing_median_age, 
     main="Housing Price vs Median Age", 
    xlab="Median Age", ylab="Median Housing Price")
abline(a = modelAge$coefficients[1], 
       b = modelAge$coefficients[2], col="red")

############ rooms/household and value
modelRooms <- lm(median_house_value ~ roomsperhouse, data = housing)
summary(modelRooms)
plot(housing$median_house_value ~ housing$roomsperhouse, 
     main="Housing Price vs Total Rooms / Household", 
     xlab="Rooms / Household", ylab="Median Housing Price")
abline(a = modelRooms$coefficients[1], 
       b = modelRooms$coefficients[2], col="red")

############ bedrooms/household and value
modelBR <- lm(median_house_value ~ bedroomsperhouse, data = housing)
summary(modelBR)
plot(housing$median_house_value ~ housing$bedroomsperhouse, 
     main="Housing Price vs Total Bedrooms / Household", 
     xlab="Bedrooms per Household", ylab="Median Housing Price")
abline(a = modelBR$coefficients[1], 
       b = modelBR$coefficients[2], col="red")

############ members/house and value
modelPeoplePerHouse <- lm(median_house_value ~ peopleperhouse, data = housing)
summary(modelPeoplePerHouse)
plot(housing$median_house_value ~ housing$peopleperhouse, 
     main="Housing Price vs People Per Household", 
     xlab="People Per Household", ylab="Median Housing Price")
abline(a = modelPeoplePerHouse$coefficients[1], 
       b = modelPeoplePerHouse$coefficients[2], col="red")

############ households and value
modelHouseholds <- lm(median_house_value ~ households, data = housing)
summary(modelHouseholds)
plot(housing$median_house_value ~ housing$households, 
     main="Housing Price vs Total Households", 
     xlab="Total Households", ylab="Median Housing Price")
abline(a = modelHouseholds$coefficients[1], 
       b = modelHouseholds$coefficients[2], col="red")

############ income and value
modelIncome <- lm(median_house_value ~ median_income, data = housing)
summary(modelIncome)
plot(housing$median_house_value ~ housing$median_income, 
     main="Housing Price vs Median Income", 
     xlab="Median Income", ylab="Median Housing Price")
abline(a = modelIncome$coefficients[1], 
       b = modelIncome$coefficients[2], col="red")
lines(lowess(housing$median_income, housing$median_house_value), col="blue")

############ near bay and value
par(mfrow=c(1,2))
values <- c(housing$median_house_value)
area <- c(rep("Median House Value", length(housing$median_house_value)))
near_bay <- c(housing$near_bay)
boxplot(values[which(near_bay==1)]~area[which(near_bay==1)], 
         ylab="House Value")
boxplot(values[which(near_bay==0)]~area[which(near_bay==0)], 
        ylab="House Value")

############ near ocean and value

par(mfrow=c(1,2))
near_ocean <- c(housing$near_ocean)
boxplot(values[which(near_ocean==1)]~area[which(near_ocean==1)], 
        ylab="House Value")
boxplot(values[which(near_ocean==0)]~area[which(near_ocean==0)], 
        ylab="House Value")

############ 1h drive to ocean and value

par(mfrow=c(1,2))
oneh_ocean <- c(housing$oneh_ocean)
boxplot(values[which(oneh_ocean==1)]~area[which(oneh_ocean==1)], 
        ylab="House Value")
boxplot(values[which(oneh_ocean==0)]~area[which(oneh_ocean==0)], 
        ylab="House Value")

############ inland and value

par(mfrow=c(1,2))
inland <- c(housing$inland)
boxplot(values[which(inland==1)]~area[which(inland==1)], 
        ylab="House Value")
boxplot(values[which(inland==0)]~area[which(inland==0)], 
        ylab="House Value")

par(mfrow=c(1,4))
boxplot(median_house_value ~ oneh_ocean, data=housing,
        xlab = "Within 1H from Ocean", ylab = "Median House Value", 
        col=c("slategray2","snow3"))
boxplot(median_house_value ~ near_bay, data=housing,
        xlab = "Near a Bay", ylab = "Median House Value", 
        col=c("slategray2","snow3"))
boxplot(median_house_value ~ near_ocean, data=housing,
        xlab = "Near an Ocean", ylab = "Median House Value", 
        col=c("slategray2","snow3"))
boxplot(median_house_value ~ inland, data=housing,
        xlab = "Inland Location", ylab = "Median House Value", 
        col=c("slategray2","snow3"))

sum(housing$inland)
sum(housing$near_ocean)
sum(housing$near_bay)
sum(housing$oneh_ocean)

############ location / income and value

dataInland <-housing[which(housing$inland == 1),]
modelInland <- lm(median_house_value ~ median_income, data = dataInland)
dataWater <-housing[which(housing$oneh_ocean == 1 | 
                                housing$near_bay == 1 | housing$near_ocean == 1),]
modelWater <- lm(median_house_value ~ median_income, data = dataWater)

summary(modelWater)
summary(modelInland)

par(mfrow=c(1,1))
plot(housing$median_house_value ~ housing$median_income, 
     main = "Median House Value vs Median Income",
     xlab = "Median Income of Area ($10k)", ylab="Median House Value of Area")
abline(a = modelInland$coefficients[1], 
       b = modelInland$coefficients[2], col="red")
abline(a = modelWater$coefficients[1], 
       b = modelWater$coefficients[2], col="blue")
legend("topleft", legend=c("Inland", "Near Water"), 
       col=c("red", "blue"), lty=1)

confint(modelWater, level=0.95)
confint(modelInland, level=0.95)

anova(modelWater)
anove(modelInland)

############ Model Diagnostics

# check assumptions for inland data:
# residuals versus predictor
plot(modelInland$residuals ~ dataInland$median_income, main="Residuals vs Income: Inland Data", 
     xlab="Median Income", ylab="Residuals")
abline(h = c(-2, 2), lty=2)
# standardized residuals vs predictor
plot(rstandard(modelInland) ~ dataInland$median_income, main="Std. Residuals v Decor", 
     xlab="Med Income ($10k)", ylab="Std. Residuals")
abline(h = c(-2, 2), lty=2)

# standardized residuals vs fitted
plot(rstandard(modelInland) ~ modelInland$fitted.values, 
     main="Std. Residuals v Fitted", 
     xlab="Fitted", ylab="Std. Residuals")
# normal QQ plot
qqnorm(residuals(modelInland))
qqline(residuals(modelInland))

# leverage
hii_inland <- hatvalues(modelInland)
cutoff_inland <- 4/nrow(dataInland)
which(hii_inland > cutoff_inland)

# locate on residual plot
plot(rstandard(modelInland) ~ dataInland$median_income, main="Std. Residuals v Income: Inland", 
     xlab="Median Income ($10k)", ylab="Std. Residuals")
points(dataInland$median_income[which(hii_inland>cutoff_inland)], 
       rstandard(modelInland)[which(hii_inland>cutoff_inland)], col="blue", pch=20)

# outliers
ri_inland <- rstandard(modelInland)
which(ri_inland > 2 | ri_inland < -2)

plot(rstandard(modelInland) ~ dataInland$median_income, main="Std. Residuals v Income: Inland", 
     xlab="Median Income ($10k)", ylab="Std. Residuals")
points(dataInland$median_income[which(ri_inland > 2 | ri_inland < -2)], 
       rstandard(modelInland)[which(ri_inland > 2 | ri_inland < -2)], col="red", pch=20)
points(dataInland$median_income[which(hii_inland>cutoff_inland)], 
       rstandard(modelInland)[which(hii_inland>cutoff_inland)], col="blue", pch=20)
points(dataInland$median_income[which(hii_inland>cutoff_inland & (ri_inland > 2 | ri_inland < -2))], 
       rstandard(modelInland)[which(hii_inland>cutoff_inland & (ri_inland > 2 | ri_inland < -2))], 
       col="green", pch=20)



# check assumptions for inland data:
# standardized residuals vs fitted
plot(rstandard(modelCoastal) ~ modelCoastal$fitted.values, 
     main="Std. Residuals v Fitted", 
     xlab="Fitted", ylab="Std. Residuals")
# normal QQ plot
qqnorm(residuals(modelCoastal))
qqline(residuals(modelCoastal))

# leverage
hii_coastal <- hatvalues(modelCoastal)
cutoff_coastal <- 4/nrow(dataCoastal)
which(hii_coastal > cutoff_coastal)

# outliers
ri_coastal <- rstandard(modelCoastal)
which(ri_coastal > 2 | ri_coastal < -2)

plot(rstandard(modelCoastal) ~ dataCoastal$median_income, main="Std. Residuals v Income: Coastal", 
     xlab="Median Income ($10k)", ylab="Std. Residuals")
points(dataCoastal$median_income[which(ri_coastal > 2 | ri_coastal < -2)], 
       rstandard(modelCoastal)[which(ri_coastal > 2 | ri_coastal < -2)], col="red", pch=20)
points(dataCoastal$median_income[which(hii_coastal>cutoff_coastal)], 
       rstandard(modelCoastal)[which(hii_coastal>cutoff_coastal)], col="blue", pch=20)
points(dataCoastal$median_income[which(hii_coastal>cutoff_coastal & (ri_coastal > 2 | ri_coastal < -2))], 
       rstandard(modelCoastal)[which(hii_coastal>cutoff_coastal & (ri_coastal > 2 | ri_coastal < -2))], 
       col="green", pch=20)


############ model adjustments


housing$ynew_income <- log10(housing$median_income)

xnewCoastal_income <- (dataCoastal$median_income)
ynewCoastal_income <- log10(dataCoastal$median_house_value)
xnewInland_income <- (dataInland$median_income)
ynewInland_income <- log10(dataInland$median_house_value)

newmod_inland <- lm(ynewCoastal_income~xnewCoastal_income)
newmod_coastal <- lm(ynewInland_income~xnewInland_income)

summary(newmod_inland)
summary(newmod_coastal)

par(mfrow=c(1,2))
qqnorm(residuals(modelInland), main = "First Model: Inland")
qqline(residuals(modelInland))
qqnorm(rstandard(newmod_inland), main="New Model: Inland")
qqline(rstandard(newmod_inland))

par(mfrow=c(1,2))
qqnorm(residuals(modelCoastal), main = "First Model: Coastal")
qqline(residuals(modelCoastal))
qqnorm(rstandard(newmod_coastal), main="New Model: Coastal")
qqline(rstandard(newmod_coastal))

confint(newmod_coastal, level=0.95)
confint(newmod_inland, level=0.95)

par(mfrow=c(1,1))
plot(housing$ynew_income ~ housing$median_income, 
     main = "Median House Value vs Median Income",
     xlab = "Median Income of Area ($10k)", ylab="Median House Value of Area")
abline(a = newmod_inland$coefficients[1], 
       b = newmod_inland$coefficients[2], col="red")
abline(a = newmod_coastal$coefficients[1], 
       b = newmod_coastal$coefficients[2], col="blue")
legend("topleft", legend=c("Inland", "Near Water"), 
       col=c("red", "blue"), lty=1)

