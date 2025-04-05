getwd()
setwd("/Users/prashannaraj/Documents/SIUE/STAT561")
list.files()
pat_sat=read.table("pat_stat.txt",header=T)

str(pat_sat)
################## Q.N. 1 (a) ####################
################## Plot Histogram ################
predictor=colnames(pat_sat)
par(mfrow=c(2,2))  # Set layout to display multiple plots
for (col in predictor[2:length(predictor)]) {
  hist(pat_sat[[col]], main=paste("Histogram of", col), xlab=col, col="pink", border="black")
}

################ Box Plot #######################
par(mfrow=c(2,2))  
for(col in predictor[2:length(predictor)]){
  boxplot(pat_sat[[col]],main=paste("Boxplot of", col),xlab=col, col="orange",border="black")
}
################ Summary ######################
summary(pat_sat["pat_age"])
summary(pat_sat["severity"])
summary(pat_sat["anxiety"])

################### (b) #######################
# Create scatter plot matrix for numeric predictors
numeric_cols = sapply(pat_sat, is.numeric) 
numeric_data = pat_sat[, numeric_cols]  
pairs(numeric_data, main="Scatter Plot Matrix of Predictors",
      col="brown")
# Corelation matrix
corelation_matrix=cor(pat_sat)
print(corelation_matrix)

################# (c) and (d) #######################
# Multiple Linear Regression Model
multi_reg_model=lm(pat_sat ~ pat_age + severity + anxiety, data = pat_sat)
summary(multi_reg_model)

################# (e) ###############################
confint(multi_reg_model,level = 0.9)

################# (g) ###############################
# Predicting sample data
attach(pat_sat)
new_data <- data.frame(pat_age = 35, severity = 45, anxiety = 2.2)
predicted_value <- predict(multi_reg_model, newdata = new_data, interval = "prediction")
print(predicted_value)

################ (h) ###############################
###### Forward selection
null_model<-lm(pat_sat~1,data = pat_sat)
summary(null_model)

full_model = lm(pat_sat ~ ., data=pat_sat)
forward = step(null_model, direction='forward', 
               scope=formula(full_model), trace=1)
forward$coefficients

####### Backward selection
full_model = lm(pat_sat ~ ., data=pat_sat)
backward = step(full_model, direction='backward', 
                scope=formula(full_model), trace=1)

backward$coefficients

################ Q.N. 2 #######################
# Muscle mass data
muscle_mass=read.table("muscle_mass.txt",header = T)
View(muscle_mass)
cor(muscle_mass)

############ First order model
first_order_model<-lm(mmass~age,data = muscle_mass)
s=summary(first_order_model)
s$coefficients
str(first_order_model)
pf(174.1,1,58, lower.tail=F) # p(F>174.1)

plot(muscle_mass$age, muscle_mass$mmass, main="Muscle mass VS age", xlab="Age",
     ylab="Muscle Mass",
     pch=19, col="blue")
abline(first_order_model,col="red")

########### Second order model
second_order_model <- lm(mmass ~ age + I(age^2), data = muscle_mass)
summary(second_order_model)

plot(muscle_mass$age, muscle_mass$mmass, 
     main="Muscle Mass vs Age", 
     xlab="Age", ylab="Muscle Mass", 
     pch=19, col="blue")

# Add first-order regression line (linear)
abline(first_order_model, col="red", lwd=2)  # Red line for linear model

# Add second-order regression curve (quadratic)
age_seq <- seq(min(muscle_mass$age), max(muscle_mass$age), length.out=100)  # Smooth age values
mmass_pred_quadratic <- predict(second_order_model, newdata = data.frame(age = age_seq))
lines(age_seq, mmass_pred_quadratic, col="green", lwd=2)  # Green line for quadratic model

legend("topright", legend=c("Linear Fit", "Quadratic Fit"), 
       col=c("red", "green"), lty=1, lwd=2)

##### Third order model #######
third_order_model<-lm(mmass~age+I(age^2)+I(age^3),data = muscle_mass)
summary(third_order_model)

################ Q.N 3. #####################
cdi=read.table("cdi.txt",header = TRUE)
cdi=cdi[,-1]
View(cdi)
str(cdi)
unique(cdi$geographic_region)
# Since we have 4 grographical regions. Let’s assume geographic_region is coded as:
# 1 = Northeast | 2 = Midwest | 3 = South | 4 = West
#  Northeast is used as the baseline. The remaining categories (Midwest, South, and West) will be represented as dummy variables.
library(dplyr)
# Convert geographic region to factor
cdi$geographic_region <- as.factor(cdi$geographic_region)
model <- lm(number_active_physicians ~ total_population + total_personal_income_millions + geographic_region, data = cdi)
#summary(model)

names(model$coefficients) <- gsub("geographic_region2", "X3", names(model$coefficients))
names(model$coefficients) <- gsub("geographic_region3", "X4", names(model$coefficients))
names(model$coefficients) <- gsub("geographic_region4", "X5", names(model$coefficients))

summary(model)

