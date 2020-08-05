#read csv
projdata<-read.csv("Acidification_Moorings CSV.csv")

#add Site column
GBRWIS<-rep("Heron Islands",length(which(projdata$site_code == "GBRWIS")))
NRSMAI<-rep("Maria Island",length(which(projdata$site_code == "NRSMAI")))
NRSKAI<-rep("Kangaroo Island",length(which(projdata$site_code == "NRSKAI")))
Site<-append(GBRWIS,NRSMAI)
Site<-append(Site, NRSKAI)
projdata<-cbind(projdata,Site)
projdata <- projdata[, c(1, 2, 28, 3:27)]

#split TIME column into date and time (requires package "chron")
projdata$date <- substr(projdata$TIME, 1, 10)
projdata$time <- substr(projdata$TIME, 12, 19)
projdata$TIME <- chron(dates = projdata$date, times = projdata$time, format=c('y-m-d','h:m:s'))
projdata <- projdata[, c(1:28)]

#remove irrelevant columns
relprojdata <- projdata[, -which(names(projdata) %in% c("FID", "site_code", "deployment_code", "file_id", "measurement_id", "LATITUDE", "LONGITUDE", "SUBFLAG", "geom"))]

#remove fugacity-related columns
relprojdata <- relprojdata[, -which(names(relprojdata) %in% c("fCO2SW_UATM", "DfCO2", "fCO2SW_UATM_quality_control", "DfCO2_quality_control"))]

#remove AtmPress-related columns
relprojdata <- relprojdata[, -which(names(relprojdata) %in% c("Press_ATM", "Press_ATM_quality_control"))]

#remove 02Conc-related columns
relprojdata <- relprojdata[, -which(names(relprojdata) %in% c("DOX1", "DOX1_quality_control"))]

#remove erroneous rows
finprojdata <- relprojdata[!(relprojdata$TIME_quality_control > 2), ]
finprojdata <- finprojdata[!(finprojdata$TEMP_quality_control > 2),]
finprojdata <- finprojdata[!(finprojdata$PSAL_quality_control > 2),]
finprojdata <- finprojdata[!(finprojdata$xCO2EQ_PPM_quality_control > 2),]
finprojdata <- finprojdata[!(finprojdata$xCO2ATM_PPM_quality_control > 2),]

#remove quality control columns (requires package "data.table")
finprojdata <- finprojdata[, -which(names(finprojdata) %like% "quality_control")]

#visualizing data trends between variables
attach(finprojdata)
par(mfrow=c(1,2))
plot(xCO2EQ_PPM, PSAL, col = factor(Site), xlab = "Concentration of CO2 in water (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2EQ_PPM), col="blue")
cor(xCO2EQ_PPM, PSAL)
plot(xCO2ATM_PPM, PSAL, col = factor(Site), xlab = "Concentration of CO2 in air (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2ATM_PPM), col="blue")
cor(xCO2ATM_PPM, PSAL)
legend("bottomright", legend=levels(Site), pch = 1, col = unique(Site))

cor(finprojdata[,2:6])
detach(finprojdata)

#heron only
HeronData <- finprojdata[finprojdata$Site == "Heron Islands",]
attach(HeronData)
plot(xCO2EQ_PPM, PSAL, xlab = "Concentration of CO2 in water (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2EQ_PPM), col="blue")
cor(xCO2EQ_PPM, PSAL)
plot(xCO2ATM_PPM, PSAL, xlab = "Concentration of CO2 in air (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2ATM_PPM), col="blue")
cor(xCO2ATM_PPM, PSAL)
detach(HeronData)

#kangaroo only
KangarooData <- finprojdata[finprojdata$Site == "Kangaroo Island",]
attach(KangarooData)
plot(xCO2EQ_PPM, PSAL, xlab = "Concentration of CO2 in water (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2EQ_PPM), col="blue")
cor(xCO2EQ_PPM, PSAL)
plot(xCO2ATM_PPM, PSAL, xlab = "Concentration of CO2 in air (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2ATM_PPM), col="blue")
cor(xCO2ATM_PPM, PSAL)
detach(KangarooData)

#maria only
MariaData <- finprojdata[finprojdata$Site == "Maria Island",]
attach(MariaData)
plot(xCO2EQ_PPM, PSAL, xlab = "Concentration of CO2 in water (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2EQ_PPM), col="blue")
cor(xCO2EQ_PPM, PSAL)
plot(xCO2ATM_PPM, PSAL, xlab = "Concentration of CO2 in air (PPM)", ylab = "Salinity of seawater (PSU)")
abline(lm(PSAL~xCO2ATM_PPM), col="blue")
cor(xCO2ATM_PPM, PSAL)
detach(MariaData)

g <- ggplot(projdata, aes(xCO2EQ_PPM, TEMP))

g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(y="TEMP", 
       x="CO2", 
       title="TEMP vs CO2 (Water)")

g2 <- ggplot(projdata, aes(xCO2ATM_PPM, TEMP))

g2 + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(y="TEMP", 
       x="CO2", 
       title="TEMP vs CO2 (Air)")


#correlation between 2 variables 
#(close to 1 - strong +ve correlation, 
#close to -1 - strong -ve correlation, 0 = no correlation)
res <- cor(projdata$xCO2ATM_PPM , projdata$TEMP)
res # -0.2480993
res2 <- cor(projdata$xCO2EQ_PPM , projdata$TEMP)
res2 # 0.5508517

#salinity box plots
par(mfrow=c(1,4))
boxplot(finprojdata$PSAL, main="All Data")
boxplot(HeronData$PSAL, main="Heron Island Only")
boxplot(MariaData$PSAL, main="Maria Island Only")
boxplot(KangarooData$PSAL, main="Kangaroo Island Only")

#############################################
#
# Relationships between Co2 in water and Air Vs Temperature and Salinity
#
##############################################


# load ggplot2
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)

# A basic scatterplot with color depending on site
plot_1 <- ggplot(projdata, aes(x=xCO2ATM_PPM, y=TEMP, color=Site)) + 
  geom_point(size=1) +
  geom_smooth() +
  theme_ipsum() +
  ggtitle("Co2 in Air vs Sea Water Temperature")

ggsave(filename="Co2_air vs Temp.png", 
       plot = plot_1, device="png",height=8, 
       width=12, units="in", dpi=500)

###
plot_2 <- ggplot(projdata, aes(x=fCO2SW_UATM, y=PSAL, color=Site)) + 
  geom_point(size=1) +
  geom_smooth() +
  theme_ipsum() +
  ggtitle("Fugacity of co2 (at 100% humidity) in water
          vs Salinity in Sea Water")

ggsave(filename="Co2water vs sal.png", 
       plot = plot_2, device="png",height=8, 
       width=12, units="in", dpi=500)

####
plot_3 <- ggplot(projdata, aes(x=xCO2ATM_PPM, y=PSAL, color=Site)) + 
  geom_point(size=1) +
  geom_smooth() +
  theme_ipsum() +
  ggtitle("Co2 in air vs Salinity in sea water")

ggsave(filename="Co2_air vs sal.png", 
       plot = plot_3, device="png",height=8, 
       width=12, units="in", dpi=500)
####
plot_4 <- ggplot(projdata, aes(x=fCO2SW_UATM, y=TEMP, color=Site)) + 
  geom_point(size=1) +
  geom_smooth() +
  theme_ipsum() +
  ggtitle("Fugacity of co2 (at 100% humidity) in water vs Sea Water Temperature")

ggsave(filename="Co2_water vs Temp.png", 
       plot = plot_4, device="png",height=8, 
       width=12, units="in", dpi=500)

#Trends between station and over time
projdata<-read.csv("") # read CSV for plotting perhaps cleaning of data a little different
projdata<-projdata[-which(projdata$xCO2EQ_PPM_quality_control == 4),]
projdata<-projdata[-which(projdata$PSAL_quality_control == 4),]
GBRWIS<-rep("Heron Islands",length(which(projdata$site_code == "GBRWIS")))
NRSMAI<-rep("Maria Island",length(which(projdata$site_code == "NRSMAI")))
NRSKAI<-rep("Kangaroo Island",length(which(projdata$site_code == "NRSKAI")))
Site<-append(GBRWIS,NRSMAI)
Site<-append(Site, NRSKAI)
projdata<-cbind(projdata,Site)
Heron<-projdata[which(projdata$Site == "Heron Islands"),]
Maria<-projdata[which(projdata$Site == "Maria Island"),]
Kangaroo<-projdata[which(projdata$Site == "Kangaroo Island"),]
Heron$TIME<-as.POSIXct(Heron[,2]) 
Kangaroo$TIME<-as.POSIXct(Kangaroo[,2]) 
Maria$TIME<-as.POSIXct(Maria[,2]) 

#Temperature vs Time
temp_plot<-ggplot(data = Heron[,2:3], mapping = aes(x=TIME,y=TEMP,group=1))
temp_plot + 
  geom_line(alpha = 0.5 , color = 'blue')+
  geom_line(data = Maria[,2:3], mapping = aes(x=TIME,y=TEMP,group=1), alpha=0.5, color="red")+
  geom_line(data = Kangaroo[,2:3], mapping = aes(x=TIME,y=TEMP,group=1), alpha=0.5, color="black")+
  ggtitle("Temperature Change") + 
  ylab("Temperature (ºC)")+
  xlab("Time")+  
  scale_x_datetime(limits=c(as.POSIXct('2017/01/01'), as.POSIXct('2018/12/31')))
mean(Heron[,3])
sd(Heron[,3])
mean(Maria[,3])
sd(Maria[,3])
mean(Kangaroo[,3])
sd(Kangaroo[,3])

#Atmospheric CO2 vs Time
co2ATM_plot<-ggplot(data = Heron[,c(2,7)], mapping = aes(x=TIME,y=xCO2ATM_PPM,group=1))
co2ATM_plot+
  geom_line(alpha = 0.4 , color = 'blue')+
  geom_line(data = Maria[,c(2,7)], mapping = aes(x=TIME,y=xCO2ATM_PPM,group=1), alpha=0.4, color="Red")+
  geom_line(data = Kangaroo[,c(2,7)], mapping = aes(x=TIME,y=xCO2ATM_PPM,group=1), alpha=0.4, color="black")+
  ggtitle("Atmospheric C02 Change") + 
  ylab("Atmospheric C02 (ppm)")+
  xlab("Time")+
  scale_x_datetime(limits=c(as.POSIXct('2017/01/01'), as.POSIXct('2018/12/31')))
mean(Heron[,7])
sd(Heron[,7])
mean(Maria[,7])
sd(Maria[,7])
mean(Kangaroo[,7])
sd(Kangaroo[,7])

#Salinty vs Time
psal_plot<-ggplot(data = Heron[,c(2,4)], mapping = aes(x=TIME,y=PSAL,group=1))
psal_plot+
  geom_line(alpha = 0.4 , color = 'blue')+
  geom_line(data = Maria[,c(2,4)], mapping = aes(x=TIME,y=PSAL,group=1), alpha=0.4, color="Red")+
  geom_line(data = Kangaroo[,c(2,4)], mapping = aes(x=TIME,y=PSAL,group=1), alpha=0.4, color="black")+
  ggtitle("Salinty Change") + 
  ylab("Salinty (PSU)")+
  xlab("Time")+
  scale_x_datetime(limits=c(as.POSIXct('2017/01/01'), as.POSIXct('2018/12/31')))
mean(Heron[,4])
sd(Heron[,4])
mean(Maria[,4])
sd(Maria[,4])
mean(Kangaroo[,4])
sd(Kangaroo[,4])

#Aqueous CO2 vs Time
water_co2_plot<-ggplot(data = Heron[,c(2,6)], mapping = aes(x=TIME,y=xCO2EQ_PPM,group=1))
water_co2_plot+
  geom_line(alpha = 0.4 , color = 'blue')+
  geom_line(data = Maria[,c(2,6)], mapping = aes(x=TIME,y=xCO2EQ_PPM,group=1), alpha=0.4, color="Red")+
  geom_line(data = Kangaroo[,c(2,6)], mapping = aes(x=TIME,y=xCO2EQ_PPM,group=1), alpha=0.4, color="black")+
  ggtitle("C02 Water Sample Change") + 
  ylab("C02 Concentration (ppm)")+
  xlab("Time")+
  scale_x_datetime(limits=c(as.POSIXct('2017/01/01'), as.POSIXct('2018/12/31')))
mean(Heron[,6])
sd(Heron[,6])
mean(Maria[,6])
sd(Maria[,6])
mean(Kangaroo[,6])
sd(Kangaroo[,6])

#Model Testing
#stepwise regression
summary(pd.fit <- lm(xCO2ATM_PPM ~ TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DOX1 + DfCO2, data = pd.train))
step(pd.fit, data = pd.train)

regsub <- regsubsets(xCO2ATM_PPM ~ TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DOX1 + DfCO2, data = pd.train)
summary(regsub)
summary(regsub)$cp
with(summary(regsub), which[which.min(cp), ])
pd.fit2 <- lm(xCO2ATM_PPM ~ (TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DfCO2)^2,
              data = pd.train)

step(pd.fit2, data = swiss)
summary(pd.fit2)
#predict on test set
CO2pred_2 <- predict(pd.fit2, pd.test)
#actual values of CO2 in air vs predicted values
actuals_preds_2 <- data.frame(cbind(actuals = pd.test$xCO2ATM_PPM, predicteds = CO2pred_2))
head(actuals_preds)
# accuracy of the predictions/model
min_max_accuracy_2 <- mean(apply(actuals_preds_2, 1, min) / apply(actuals_preds_2, 1, max))
min_max_accuracy_2 # 99.99313%

# mean absolute percentage error in predictions 
mape_2 <- mean(abs((actuals_preds_2$predicteds - actuals_preds_2$actuals))/actuals_preds_2$actuals)
mape_2 # 6.870855e-05

#MODEL
#splitting data into training and test set
set <- sample(1:nrow(projdata),nrow(projdata)/2,
              replace=FALSE)
pd.train=projdata[set, ] # Create training set
pd.test=projdata[-set, ] # Create test set

#fitted model on training set (R^2 = 0.9979)
mod <- lm(xCO2ATM_PPM ~ TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DfCO2, data = pd.train)
summary(mod)

#predict on test set
CO2pred <- predict(mod, pd.test)

#actual values of CO2 in air vs predicted values
actuals_preds <- data.frame(cbind(actuals = pd.test$xCO2ATM_PPM, predicteds = CO2pred))
head(actuals_preds)

# accuracy of the predictions/model
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy # 99.97355%

# mean absolute percentage error in predictions 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape # 0.0002645813 or 0.02645813%

#plot
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

#model with cross-validation (R^2 (0.998) is almost equal to R^2(0.9979) of simple linear regression but its more costly/slow,
# so we went with the simple linear regression model with data split into 2 halves)
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model2 <- train(xCO2ATM_PPM ~ TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DfCO2, data = projdata, method = "lm",
                trControl = train.control)
print(model2)
summary(model2)

#regression tree
library(rpart)
rt <- rpart(xCO2ATM_PPM ~ TEMP + PSAL + Press_ATM + xCO2EQ_PPM + fCO2SW_UATM + DOX1 + DfCO2, data = pd.train)
rtree <- predict(rt,pd.test)
summary(rt)

actuals_preds_rtee <- data.frame(cbind(actuals = pd.test$xCO2ATM_PPM, predicteds = rtree))
head(actuals_preds_rtee)

min_max_accuracy3 <- mean(apply(actuals_preds_rtee, 1, min) / apply(actuals_preds_rtee, 1, max))
min_max_accuracy3