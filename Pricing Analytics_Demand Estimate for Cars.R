rm(list = ls())
library("lfe")
library("data.table")


setwd("/Users/siqijiang/Desktop/2020 Spring A/MKT440 Pricing Analytics/project 1")

#Load data
cardata=fread("cars.csv",stringsAsFactors = F)
production_cost = fread("iron_ore.csv",stringsAsFactors = F)

#Merge two tables by year 
#install.packages("dplyr")
library("dplyr")

production_cost$ye <- substr(production_cost$year, 3,4)
cardata$ye<-as.character(cardata$ye)
Merge_Data<- left_join(cardata, production_cost, by = NULL, copy=False)
#due to different unit in we (kg) and unit price (t), we take weight multiply by 0.001 to match the unit of "unit_value_98."
Merge_Data$cost_iron <- 0.001*Merge_Data$we*Merge_Data$unit_value_98

#Initial regression
reg=felm(log(qu)~log(eurpr),data=cardata)
summary(reg)
#It is inelastic because the coefficient in absolute value is less than 1. This means that the customers are not price sensitive. However, this estimate is not reasonable based on common sense. It does not make sense that consumers do not care if the seller increases the price of cars, since cars are usually very expensive, an one percent increase in price (euro) will cost consumers hundreds of euros more to buy a car.   

###########################
###Add control variables###
###########################
#Example of adding controls: run a log-log regression with year as fixed effect and "li" as a control variable
reg2=felm(log(qu)~log(eurpr)+li|factor(ye),data=cardata)
summary(reg2)
###########################
#####THINKING PROCESS######
###########################
# find variations in Xs.
#First, we think that cy, li, hp, sp, ac might be the variations in prices.
# check their correlation with each other
cor(Merge_Data$cy,Merge_Data$hp, method='spearman', use='pairwise.complete.obs') #0.93
cor(Merge_Data$sp,Merge_Data$ac, method='spearman', use='pairwise.complete.obs') #-0.9108706
cor(Merge_Data$li,Merge_Data$ac, method='spearman', use='pairwise.complete.obs') #-0.4368312

#Because some of them are highly correlated, we only include li and ac as the control variables


########################
###Find fixed effects###
########################

#####THINKING PROCESS######

# Then, we try to find fixed effects
# We try to add only one fixed effect but the result shows that for our data environment, fixing only one dimension is not enough
felm(log(qu)~ log(eurpr)+li+ac|factor(ye),data=cardata)#improvement needed
felm(log(qu)~ log(eurpr)+li+ac|factor(co),data=cardata)##improvement needed
felm(log(qu)~ log(eurpr)+li+ac|factor(ma),data=cardata)##improvement needed

# Then, we try to add interacted fixed effects (this thinking process is in the report)


summary(felm(log(qu)~ log(eurpr)+li+ac|factor(co)+factor(ye):factor(ma), data=Merge_Data)) #chosen, beta1:-1.650158

#●	li: we believe that fuel efficiency is a key component to the quantity sold because consumers usually care about how fuel-consuming the car is.  This is because higher fuel efficiency leads to less spending on fuel in the future and more environmentally-friendly. 

#●	cy: cylinder volume is related to horsepower; higher cylinder volume will lead to higher horsepower, which is a key feature that people care about when buying a car. cylinder volume sometimes indicates how good the car is that influences demand. 

#●	hp: higher horsepower means better driving experience with a shorter time to acceleration; based on common sense, people usually like cars with higher horsepower and driving fast when they don’t take money into account. Thus, we believe that horsepower is a key to the demand.

#●	sp: Cars with a higher maximum speed are generally more preferred by consumers. 

#●	ac: Consumers prefer a better performance of automotive acceleration that is represented here by time to accelerate (in seconds from 0 to 100km/h. 
                                                                                                                       
#Some variables that we did not add in our regression:
#brd: Brand code is not considered as we already include fixed effect co(model code),  which serves as the third dimension of panel and provides more information of the car model. 

#●	do/le/wi/he: Doors, length, width, and height are features that largely depend on car design and are not directly related to demand. 

#●	pop/rgdp: We do not add population and GDP as control variables since the variation caused by these two is already taken away by including the fixed effect of ma(market) and ye(year). Otherwise there’ll exist a perfect correlation. 


#################################
###Find Instrumental variables###
#################################

#Possible IVs: hp; xexr; avcpr; avppr; avdppr; avdcpr; costs_iron (from irone_ore data frame)

#check if they are sufficiently correlated with log(price)
IV_check1 = lm(formula = log(eurpr)~hp+xexr+avcpr+avppr+avdppr+avdcpr+cost_iron, data = Merge_Data)
summary(IV_check1)
#As a result, avcpr, avdppr, and avppr are not potential IVs because they are not correlated with price which violates the fundamental requirements of being an IV.The reason behind this is that we might overthink the effects from producer price index on the demand. In reality, PPI might not have that much effect on cost as we expected between 1970 to 2000, thus, PPI does not influence the price that much. #

# exclude avcpr, avppr, and avdppr that are insignificant
# test our IVs' effects on the casual relationship
summary(felm(log(qu)~1|0|(log(eurpr)~hp+xexr+avdcpr+cost_iron), data=Merge_Data))#-0.50613 is better than -0.29

#add the Ivs' effect to the model
#The final model 
final_model<-felm(log(qu)~li+ac|factor(co)+factor(ye):factor(ma)|(log(eurpr)~hp+xexr+avdcpr+cost_iron), data=Merge_Data)
summary(final_model)
#Last, added these IVs to the regression with control variables and fixed effects that we picked in part 4 to see the total effect changes on beta 1. Consequently, beta 1 is -2.34 after adding all the control and effects, which matches our first assumption that the price elasticity should be elastic because car consumers are usually price-sensitive based on common sense. 
final_model$coefficients[3,] #-2.341181 

#find the intercept
fe<-getfe(final_model)
tail(fe)
idc <- match('90',fe$idx)
fe90=fe[idc,1]
fe90
# intercept :31.55348

#6

beta1 = final_model$coefficients[3,]
rival_price<-subset(Merge_Data, ye=='95')
rival_price<-subset(rival_price, ma=='1')
rival_price$cost <- (beta1-1/beta1)*rival_price$avgurprrival

plot( rival_price$avgurprrival, rival_price$qu, main="Rival Price vs Sales",
      xlab="Rival Price", ylab="Sales")


#7
# in order to get log-average rival price, I add it to the regression.

#####THINKING PROCESS#####

# I first run a log-log regression between log qu and log-average rival price without any changes.
summary(felm(log(qu)~log(avgurprrival)+li+ac|factor(co)+factor(ye):factor(ma)|(log(eurpr)~xexr+avdcpr+hp+cost_iron), data=Merge_Data)) # -2.858522  -16.12806️
# However, the result is not what we expected (see explainations in the report)
# Then, we alter the fixed effects which eliminates the car model effects, which the result is also not what we expected.
summary(felm(log(qu)~log(avgurprrival)+li+ac|factor(ye):factor(ma)|(log(eurpr)~xexr+avdcpr+hp+cost_iron), data=Merge_Data)) #-4.041 -1.584e+02 
#Last 
# Because avgurprrival is avg price of rival cars in the same ye & ma combination, then we only control car model as the fixed effect

summary(felm(log(qu)~log(avgurprrival)+li+ac|factor(co)|(log(eurpr)~xexr+avdcpr+hp+cost_iron), data=Merge_Data)) 

#log-average rival price's coeffecient is 2.037329 and our price's coeffecient is -1.802232.

