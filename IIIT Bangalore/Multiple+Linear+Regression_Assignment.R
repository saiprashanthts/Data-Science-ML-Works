# Calling the required Packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(sqldf)
library(car)
# Reading the Data Set
carprice<-read.csv('CarPrice_Assignment.csv',stringsAsFactors = F)

# Checking the structure of data frame
str(carprice)

# Checking if any na values are present
sum(is.na(carprice))

# Checking if any duplicates are present
sum(duplicated(carprice))

# Checking the number of columns and Rows in Data Frame
ncol(carprice)
nrow(carprice)

# Checking the column names of uber_dataset_original data frame
colnames(carprice)

# Checking the Summary of uber_dataset_original Data Frame
summary(carprice)

# Checking the class of uber_dataset_original Data frame
class(carprice)

# Cleaning of data 

# Cleaning car_ID as this is nothing to feature
carprice <- carprice[,-which(names(carprice)=='car_ID')]

# Cleaning of carname and converting it into company name
carprice$CarName <- str_to_upper(carprice$CarName)
carprice$companyname <- word(carprice$CarName)
sqldf("
      SELECT distinct companyname from carprice
      ")
companyname_clean <- sqldf("
      SELECT CASE WHEN companyname in ('VOKSWAGEN','VW') then 'VOLKSWAGEN'
        WHEN companyname ='TOYOUTA' then 'TOYOTA'
        WHEN companyname ='PORCSHCE' then 'PORSCHE'
        WHEN companyname = 'MAXDA' then 'MAZDA'
        ELSE companyname END AS companyname_clean 
        FROM carprice
      ")
cardata <- cbind(carprice,companyname_clean)
cardata <- cardata[,-which(names(cardata)=='companyname')]

# To Check duplicates and na values
sum(duplicated(cardata))
sum(is.na(cardata))

# to check for outliers
quantile(cardata$wheelbase,seq(0,1,0.01))
cardata$wheelbase[which(cardata$wheelbase>115.544)]<-115.544
quantile(cardata$carlength,seq(0,1,0.01))
quantile(cardata$carwidth,seq(0,1,0.01))
quantile(cardata$carheight,seq(0,1,0.01))
quantile(cardata$curbweight,seq(0,1,0.01))
cardata$curbweight[which(cardata$curbweight<1819.72)]<-1819.72
quantile(cardata$enginesize,seq(0,1,0.01))
cardata$enginesize[which(cardata$enginesize<90.00)]<-90.00
cardata$enginesize[which(cardata$enginesize>209.00)]<-209.00
quantile(cardata$boreratio,seq(0,1,0.01))
quantile(cardata$stroke,seq(0,1,0.01))
quantile(cardata$compressionratio,seq(0,1,0.01))
quantile(cardata$horsepower,seq(0,1,0.01))
cardata$horsepower[which(cardata$horsepower>207.00)]<-207.00
quantile(cardata$peakrpm,seq(0,1,0.01))
quantile(cardata$citympg,seq(0,1,0.01))
quantile(cardata$highwaympg,seq(0,1,0.01))
         
# Creating UDF Variable which contains themes and can be reused in all plots been drawn
# Theme is been Created for Title and Axis for all Graphs
UDF_Theme_with_legend <-
  theme_bw() +
  theme(
    plot.title = element_text(
      size = 20,
      lineheight = .8,
      face = "bold",
      color = 'Red'
    ),
    legend.background = element_rect(fill = "Orange", size = 0.5),
    axis.title.x = element_text(
      face = "bold",
      colour = 'blue',
      size = 18
    ),
    axis.text.x  = element_text(
      angle = 0,
      face="bold",
      vjust = 0.5,
      size = 12
    ),
    axis.title.y = element_text(
      face = "bold",
      colour = "blue",
      size = 18
    ),
    axis.text.y  = element_text(
      angle = 90,
      face="bold",
      vjust = 0.5,
      size = 12
    )
  )

UDF_Theme_without_legend <-
  theme_bw() +
  theme(
    plot.title = element_text(
      size = 18,
      lineheight = .8,
      face = "bold",
      color = 'Red'
    ),
    axis.title.x = element_text(
      face = "bold",
      colour = 'blue',
      size = 18
    ),
    axis.text.x  = element_text(
      angle = 0,
      face="bold",
      vjust = 0.5,
      size = 12
    ),
    axis.title.y = element_text(
      face = "bold",
      colour = "blue",
      size = 18
    ),
    axis.text.y  = element_text(
      angle = 90,
      face="bold",
      vjust = 0.5,
      size = 12
    ),
    legend.position = "none"
  )

# EDA
# Plot between Symboling and count
ggplot(data = cardata,aes(x=cardata$symboling)) + geom_bar() + UDF_Theme_without_legend

# Plot between company name and count
ggplot(data = cardata,aes(x=cardata$companyname_clean)) + geom_bar() + UDF_Theme_without_legend

# Plot between fuel type and count
ggplot(data=cardata,aes(x=cardata$fueltype)) + geom_bar() + UDF_Theme_without_legend

# Plot between aspiration and count
ggplot(data = cardata,aes(x=cardata$aspiration)) + geom_bar() + UDF_Theme_without_legend

# Plot between number of cars and count
ggplot(data = cardata,aes(x=cardata$doornumber)) + geom_bar() + UDF_Theme_without_legend

# Plot between Car Body and count
ggplot(data = cardata,aes(x=cardata$carbody)) + geom_bar() + UDF_Theme_without_legend

# Plot between drive wheel and count
ggplot(data = cardata,aes(x=cardata$drivewheel)) + geom_bar() + UDF_Theme_without_legend

# Plot between engine location and count
ggplot(data = cardata,aes(x=cardata$enginelocation)) + geom_bar() + UDF_Theme_without_legend

# Plot between engine type and count
ggplot(data = cardata,aes(x=cardata$enginetype)) + geom_bar() + UDF_Theme_without_legend

# Plot between cylinder number and count
ggplot(data = cardata,aes(x=cardata$cylindernumber)) + geom_bar() + UDF_Theme_without_legend

# plot between fuelsystem and count
ggplot(data = cardata,aes(x=cardata$fuelsystem)) + geom_bar() + UDF_Theme_without_legend

# Plot between citympg and highway mpg
ggplot(data = cardata,aes(x=cardata$citympg,y=cardata$highwaympg)) + geom_point() + UDF_Theme_without_legend

# Plot between price and highwaympg
ggplot(data = cardata,aes(x=cardata$highwaympg,y=cardata$price)) + geom_point() + UDF_Theme_without_legend

# Plot betwwen price and citympg
ggplot(data = cardata,aes(x=cardata$citympg,y=cardata$price)) + geom_point() + UDF_Theme_without_legend

# Plot between price and peakrpm 
ggplot(data = cardata,aes(x=cardata$peakrpm,y=cardata$price)) + geom_point() + UDF_Theme_without_legend

# Plot between compression ratio and price
ggplot(data = cardata,aes(x=cardata$compressionratio,y=cardata$price)) + geom_point() + UDF_Theme_without_legend

# Plot between car features and price
ggplot(data = cardata,aes(x=cardata$wheelbase,y=cardata$price)) + geom_point() + UDF_Theme_without_legend
ggplot(data = cardata,aes(x=cardata$carlength,y=cardata$price)) + geom_point() + UDF_Theme_without_legend
ggplot(data = cardata,aes(x=cardata$carwidth,y=cardata$price)) + geom_point() + UDF_Theme_without_legend
ggplot(data = cardata,aes(x=cardata$carheight,y=cardata$price)) + geom_point() + UDF_Theme_without_legend
ggplot(data = cardata,aes(x=cardata$enginesize,y=cardata$price)) + geom_point() + UDF_Theme_without_legend

# Derived Metrics

cardata$mpg_per_hp <- cardata$citympg /  cardata$horsepower

# Creation of Dummies
# Create the dummy variable for symboling variable
cardata$symboling <- as.character(cardata$symboling)
dummy_symboling <- data.frame(model.matrix( ~symboling, data = cardata))
# View(dummy_symboling)
dummy_symboling <- dummy_symboling[,-1]

# Create the dummy variable for companyname_clean variable
cardata$companyname_clean <- as.character(cardata$companyname_clean)
dummy_companyname_clean <- data.frame(model.matrix( ~companyname_clean, data = cardata))
# View(dummy_companyname_clean)
dummy_companyname_clean <- dummy_companyname_clean[,-1]

# Create the dummy variable for fueltype variable
cardata$fueltype <- as.factor(cardata$fueltype)
levels(cardata$fueltype)<-c(1,0)
cardata$fueltype <- as.numeric(levels(cardata$fueltype))[cardata$fueltype]

# Create the dummy variable for aspiration variable
cardata$aspiration <- as.factor(cardata$aspiration)
levels(cardata$aspiration)<-c(1,0)
cardata$aspiration <- as.numeric(levels(cardata$aspiration))[cardata$aspiration]

# Create the dummy variable for doornumber variable
cardata$doornumber <- as.factor(cardata$doornumber)
levels(cardata$doornumber)<-c(1,0)
cardata$doornumber <- as.numeric(levels(cardata$doornumber))[cardata$doornumber]

# Create the dummy variable for carbody variable
cardata$carbody <- as.character(cardata$carbody)
dummy_carbody <- data.frame(model.matrix( ~carbody, data = cardata))
# View(dummy_carbody)
dummy_carbody <- dummy_carbody[,-1]

# Create the dummy variable for drivewheel variable
cardata$drivewheel <- as.character(cardata$drivewheel)
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = cardata))
# View(dummy_drivewheel)
dummy_drivewheel <- dummy_drivewheel[,-1]

# Create the dummy variable for enginelocation variable
cardata$enginelocation <- as.factor(cardata$enginelocation)
levels(cardata$enginelocation)<-c(1,0)
cardata$enginelocation <- as.numeric(levels(cardata$enginelocation))[cardata$enginelocation]

# Create the dummy variable for enginetype variable
cardata$enginetype <- as.character(cardata$enginetype)
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = cardata))
# View(dummy_enginetype)
dummy_enginetype <- dummy_enginetype[,-1]

# Create the dummy variable for cylindernumber variable
cardata$cylindernumber <- as.character(cardata$cylindernumber)
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = cardata))
# View(dummy_cylindernumber)
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Create the dummy variable for fuelsystem variable
cardata$fuelsystem <- as.character(cardata$fuelsystem)
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = cardata))
# View(dummy_fuelsystem)
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combining original data and dummies created
cardata_final <-
  cbind(cardata,dummy_carbody,dummy_companyname_clean,dummy_cylindernumber,dummy_drivewheel,dummy_enginetype,dummy_fuelsystem,dummy_symboling)

# Removing the variables
cardata_final <- cardata_final[,-which(names(cardata_final)=='symboling')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='companyname_clean')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='carbody')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='drivewheel')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='enginetype')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='cylindernumber')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='fuelsystem')]
cardata_final <- cardata_final[,-which(names(cardata_final)=='CarName')]
str(cardata_final)

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cardata_final), 0.7*nrow(cardata_final))
train = cardata_final[trainindices,]
test = cardata_final[-trainindices,]

cor(cardata_final,cardata_final$price)

str(train)
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#######

# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 


# Lets load the library in which stepAIC function exists
library(MASS)

#Now lets run the code. 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Let's execute this model here, 
model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + mpg_per_hp + carbodyhatchback + 
                carbodysedan + carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd + enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

# summary(model_2)
# sort(vif(model_2))

## Let us check for multicollinearity 
# If the VIF is above  5 as the business goal says, you would remove 
# the variables if they are statistically insignificant


# fueltype removed

model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + mpg_per_hp + carbodyhatchback + 
                carbodysedan + carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd + enginetypeohc + fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_3) 

sort(vif(model_3))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + citympg + mpg_per_hp + carbodyhatchback + 
                carbodysedan + carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_4) 

sort(vif(model_4))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# citympg removed

model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + mpg_per_hp + carbodyhatchback + 
                carbodysedan + carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_5) 

sort(vif(model_5))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# car width removed

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase  + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + mpg_per_hp + carbodyhatchback + 
                carbodysedan + carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_6) 

sort(vif(model_6))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# carbody sedan removed

model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase  + curbweight + enginesize + stroke + 
                compressionratio + peakrpm + mpg_per_hp + carbodyhatchback + 
                carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_7) 

sort(vif(model_7))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

#  compression ratio

model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase  + curbweight + enginesize + stroke + 
                 peakrpm + mpg_per_hp + carbodyhatchback + 
                carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_8) 

sort(vif(model_8))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

#  carbody hactch back

model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase  + curbweight + enginesize + stroke + 
                peakrpm + mpg_per_hp + 
                carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_9) 

sort(vif(model_9))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# curb weight

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase  + enginesize + stroke + 
                peakrpm + mpg_per_hp + 
                carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                drivewheelfwd +  fuelsystem2bbl + fuelsystemmpfi, 
              data = train)

summary(model_10) 

sort(vif(model_10))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

#  drivewheelfwd


model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 peakrpm + mpg_per_hp + 
                 carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT + companyname_cleanSAAB + companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_11) 

sort(vif(model_11))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanSAAB removed


model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 peakrpm + mpg_per_hp + 
                 carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE + companyname_cleanHONDA + 
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_12) 

sort(vif(model_12))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanHONDA removed

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 peakrpm + mpg_per_hp + 
                 carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_13) 

sort(vif(model_13))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# peakrpm removed

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 mpg_per_hp + 
                 carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_14) 

sort(vif(model_14))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# mpgperhp

model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 carbodywagon + companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_15) 

sort(vif(model_15))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# carbody wagon removed
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_16) 

sort(vif(model_16))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed


# cylindernumberfour removed
model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumbersix + 
                 fuelsystem2bbl + fuelsystemmpfi, 
               data = train)

summary(model_17) 

sort(vif(model_17))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# fuelsystem2bbl removed
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive + cylindernumbersix + 
                  fuelsystemmpfi, 
               data = train)

summary(model_18) 

sort(vif(model_18))



## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# cylindernumbersix
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanCHEVROLET + companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR + companyname_cleanMAZDA + 
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_19) 

sort(vif(model_19))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanMAZDA removed
model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                  companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA + companyname_cleanVOLKSWAGEN + companyname_cleanVOLVO + 
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_20) 

sort(vif(model_20))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanVOLKSWAGEN removed
model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanRENAULT +  companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +  companyname_cleanVOLVO + 
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_21) 

sort(vif(model_21))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanRENAULT removed
model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                 companyname_cleanMERCURY + companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                   companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +  companyname_cleanVOLVO + 
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_22) 

sort(vif(model_22))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanMERCURY removed
model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                  companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +  companyname_cleanVOLVO + 
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_23) 

sort(vif(model_23))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanVOLVO removed
model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                 companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 cylindernumberfive +  
                 fuelsystemmpfi, 
               data = train)

summary(model_24) 

sort(vif(model_24))



## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed


# cylindernumberfive removed
model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanISUZU + companyname_cleanJAGUAR +  
                 companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                   
                 fuelsystemmpfi, 
               data = train)

summary(model_25) 

sort(vif(model_25))



## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanISUZU removed
model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                  companyname_cleanJAGUAR +  
                 companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT + companyname_cleanPLYMOUTH + 
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_26) 

sort(vif(model_26))


## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanPLYMOUTH removed
model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanJAGUAR +  
                 companyname_cleanMITSUBISHI + 
                 companyname_cleanNISSAN + companyname_cleanPEUGEOT +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_27) 

sort(vif(model_27))



## Let us check for multicollinearity 
# If the VIF is above 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
# enginetypeohc  removed

# companyname_cleanNISSAN removed
model_28 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanJAGUAR +  
                 companyname_cleanMITSUBISHI + 
                  companyname_cleanPEUGEOT +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_28) 

sort(vif(model_28))

# Although, all variables have a p value below 0.05, the number of variables is still too large.

# companyname_cleanMITSUBISHI removed
model_29 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanDODGE +  
                 companyname_cleanJAGUAR +  
                 companyname_cleanPEUGEOT +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_29) 

sort(vif(model_29))

# Although, all variables have a p value below 0.05, the number of variables is still too large.

# companyname_cleanDODGE removed
model_30 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanJAGUAR +  
                 companyname_cleanPEUGEOT +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_30) 

sort(vif(model_30))

# Although, all variables have a p value below 0.05, the number of variables is still too large.

# companyname_cleanPEUGEOT removed
model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase  + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanJAGUAR +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_31) 

sort(vif(model_31))

# Although, all variables have a p value below 0.05, the number of variables is still too large.

# wheelbase removed
model_32 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanJAGUAR +  
                 companyname_cleanSUBARU + 
                 companyname_cleanTOYOTA +   
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_32) 

sort(vif(model_32))


# Although, all variables have a p value below 0.05, the number of variables is still too large.

# companyname_cleanTOYOTA removed
model_33 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke + 
                 companyname_cleanBMW + companyname_cleanBUICK + 
                 companyname_cleanJAGUAR +  
                 companyname_cleanSUBARU + 
                 
                 
                 fuelsystemmpfi, 
               data = train)

summary(model_33) 

sort(vif(model_33))




# predicting the results in test dataset
Predict_1 <- predict(model_33,test[,-which(names(test)=='price')])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

# Train --> 90
# Test --> 81