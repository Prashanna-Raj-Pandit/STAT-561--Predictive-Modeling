library(dplyr)
library(readxl)
library(ggplot2)
library(corrplot)
library(nnet)
absent= read_excel("Absenteeism_at_work.xls")
names(absent) #column names
View(absent)

#recode absenteeism
absent <- absent %>%
  mutate(absenteeism = case_when(
    Absenteeism_time_in_hours >= 0 & Absenteeism_time_in_hours <= 20 ~ "Low",
    Absenteeism_time_in_hours > 20 & Absenteeism_time_in_hours <= 40 ~ "Moderate",
    Absenteeism_time_in_hours > 40 ~ "High"
  ))
View(absent)
# Convert relevant variables to factors BEFORE modeling
absent$absenteeism <- factor(absent$absenteeism, levels = c("Low", "Moderate", "High"))

absent$`Day_of_the_week` <- factor(absent$`Day_of_the_week`,
                                   levels = 2:6,
                                   labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
absent$Seasons <- factor(absent$Seasons,
                         levels = 1:4,
                         labels = c("Summer", "Autumn", "Winter", "Spring"))
absent$Education <- factor(absent$Education,
                           levels = 1:4,
                           labels = c("High School", "Graduate", "Postgraduate", "Master/Doctor"))

#### check the structure
str(absent[c("Month_of_absence", "Day_of_the_week", "Seasons", "Education")])


############# a
# Histogram
hist(absent$Absenteeism_time_in_hours, main = "Distribution of Absenteeism Time",
     xlab = "Hours", col = "orange")
summary(absent['Absenteeism_time_in_hours'])
boxplot(absent$Absenteeism_time_in_hours, main = "Absenteeism Hours",col="orange")

table(absent$Absenteeism_time_in_hours)
table(absent$Month_of_absence)
table(absent$Service_time)
table(absent$Absenteeism_time_in_hours)


############# b
# Histogram
hist(absent$Age, main = "Age Distribution", xlab = "Age",col="orange")


########### c
# Scatter plot
plot(absent$Distance_from_Residence_to_Work, 
     absent$Absenteeism_time_in_hours,
     main = "Distance vs. Absenteeism",
     xlab = "Distance from Residence to Work (km)",
     ylab = "Absenteeism Time (hours)",
     pch = 19, col = "black")

# Add linear regression line
abline(lm(Absenteeism_time_in_hours ~ Distance_from_Residence_to_Work, data = absent), 
       col = "red", lwd = 2)

# Correlation
cor(absent$`Distance_from_Residence_to_Work`, absent$`Absenteeism_time_in_hours`, use = "complete.obs")


######### d
# Scatter plot
boxplot(Work_load_Average_in_days ~ absenteeism, data = absent,
        main = "Work Load vs. Absenteeism Category",
        xlab = "Absenteeism Category", ylab = "Average Workload per Day",
        col = c("lightgreen", "orange", "tomato"))

aggregate(Work_load_Average_in_days ~ absenteeism, data = absent, FUN = mean)


######### e
# Boxplot
boxplot(Absenteeism_time_in_hours ~ Education, 
        data = absent,
        col = "green",
        main = "Absenteeism by Education Level",
        xlab = "Education Level",
        ylab = "Absenteeism Time (hours)")
# Create a contingency table
edu_abs_table <- table(absent$Education, absent$absenteeism)

# Make side-by-side barplot
barplot(edu_abs_table, 
        beside = TRUE, 
        col = c("lightblue", "orange", "tomato"),
        legend.text = TRUE,
        args.legend = list(title = "Absenteeism", x = "topright"),
        main = "Absenteeism Category by Education Level",
        xlab = "Education Level",
        ylab = "Frequency")


############# f
# Correlation matrix (numeric variables only)
num_vars <- absent %>% select_if(is.numeric)
cor_matrix <- cor(num_vars, use = "complete.obs")
# Extract the correlations with absenteeism time
cor_with_absence <- cor_matrix[, "Absenteeism_time_in_hours"]
cor_with_absence_sorted <- sort(abs(cor_with_absence), decreasing = TRUE)

# Print sorted correlations
cor_with_absence_sorted

###### g


####### h. service time
absent$Service_bin <- cut(absent$Service_time, breaks = c(0, 5, 10, 15, 20, 25), right = FALSE)
boxplot(Absenteeism_time_in_hours ~ Service_bin, data = absent,
        main = "Absenteeism Time by Service Time",
        xlab = "Service Time Group (Years)",
        ylab = "Absenteeism Time (Hours)",
        col = "skyblue")

plot(absent$Service_time, absent$Absenteeism_time_in_hours,
     main = "Service Time vs. Absenteeism Time",
     xlab = "Service Time (Years)",
     ylab = "Absenteeism Time (Hours)",
     col = "steelblue", pch = 16)
abline(lm(Absenteeism_time_in_hours ~ Service_time, data = absent), col = "red", lwd = 2)


####### i day of the week
# Bar Graph
#Count absences by day
table(absent$Day_of_the_week)

# Cross-tab of Day vs. Absenteeism Level
day_abs <- table(absent$Day_of_the_week, absent$absenteeism)

# Stacked barplot
barplot(t(day_abs),
        main = "Absenteeism Type by Day of the Week",
        col = c("green", "orange", "red"),
        xlab = "Day of the Week",
        ylab = "Number of Absences",
        legend = TRUE,
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri"))
table(absent$`Day_of_the_week`, absent$absenteeism)

######### j
# Check all numeric columns
numeric_vars <- absent[sapply(absent, is.numeric)]

# Summary to spot possible outliers
summary(numeric_vars)

# Basic boxplots for each numeric column
par(mfrow = c(3, 5))  # layout for multiple plots
for (var in names(numeric_vars)) {
  boxplot(numeric_vars[[var]], main = var, col = "lightblue")
}
par(mfrow = c(1, 1))  # reset layout

levels(absent$absenteeism)
########### Fit multinomial logistic regression
absent$absenteeism <- relevel(absent$absenteeism, ref = "Low")
logistic_model <- multinom(absenteeism ~ . -ID -Absenteeism_time_in_hours,  # Exclude ID and original target
  data = absent,
  trace = FALSE
)


summary(logistic_model)

# Interpret Son and Weight
exp(coef(logistic_model)[, "Son"])
exp(coef(logistic_model)[, "Weight"])

# Backward selection
step_model <- step(logistic_model, direction = "backward", trace = FALSE)

# Final model summary
summary(step_model)

############## 2. Flu Shot Data
flu_data=read.table("flu shot.txt",header=TRUE)
flu_data$sex=factor(flu_data$sex)
flu_data$flu_shot=factor(flu_data$flu_shot)

#color vector based on the y variable
pairs(flu_data,  
      col = flu_data$flu_shot,
      pch = 19,  
      main = "Pairs Plot for Flu Data")

# Fit the logistic regression model
flu_model <- glm(flu_shot ~ age + awareness + sex, 
                      data = flu_data, 
                      family = binomial)

# View the model summary
summary(flu_model)

##### e.
#Predict the probability of receiving a flu shot for a specific case
new_data <- data.frame(age = 55, awareness = 60, sex = factor(1, levels = levels(flu_data$sex)))
predicted_probability <- predict(flu_model, newdata = new_data, type = "response")
predicted_probability

# f
#Wald test for variable significance
# This gives the z-values (Wald statistics)
wald_test <- summary(flu_model)$coefficients[, "Estimate"] / summary(flu_model)$coefficients[, "Std. Error"]

# Calculate p-values from the Wald statistics
p_values <- 2 * (1 - pnorm(abs(wald_test)))
p_values


#  g

# Forward selection for predictor variables
model_null <- glm(flu_shot ~ 1, family = binomial(link = "logit"), data = flu_data)
model_forward <- step(model_null, scope = list(lower = model_null, upper = flu_model), direction = "forward")
summary(model_forward)
###### h
# Backward selection for predictor variables
model_backward <- step(flu_model, direction = "backward")
summary(model_backward)
