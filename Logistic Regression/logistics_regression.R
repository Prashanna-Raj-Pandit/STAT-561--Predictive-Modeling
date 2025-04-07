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

############# 1.a
# Histogram
ggplot(absent, aes(x = `Absenteeism_time_in_hours`)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Distribution of Absenteeism Time", x = "Hours", y = "Count")
summary(absent['Absenteeism_time_in_hours'])
# Boxplot
ggplot(absent, aes(y = `Absenteeism_time_in_hours`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Absenteeism Time", y = "Hours")

boxplot(absent$Absenteeism_time_in_hours, main = "Absenteeism Hours")





############# 1. b
# Histogram
hist(absent$Age, main = "Age Distribution", xlab = "Age",col="orange")

#ggplot(absent, aes(x = Age)) +
 # geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  #labs(title = "Distribution of Age", x = "Age", y = "Count")

########### 1.c
# Scatter plot
ggplot(absent, aes(x = `Distance_from_Residence_to_Work`, y = `Absenteeism_time_in_hours`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Distance vs. Absenteeism", x = "Distance (km)", y = "Hours")


# Correlation
cor(absent$`Distance_from_Residence_to_Work`, absent$`Absenteeism_time_in_hours`, use = "complete.obs")


######### 1.d
# Scatter plot
cor(absent$`Work_load_Average_in_days`, absent$Absenteeism_time_in_hours, use = "complete.obs")

ggplot(absent, aes(x = `Work_load_Average_in_days`, y = `Absenteeism_time_in_hours`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Work Load vs. Absenteeism", x = "Work Load", y = "Hours")

######### 1.e
# Boxplot
ggplot(absent, aes(x = Education, y = `Absenteeism_time_in_hours`)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Absenteeism by Education", x = "Education Level", y = "Hours")
ggplot(absent, aes(x = Education, fill = absenteeism)) + geom_bar(position = "dodge")


############# 1.f
# Correlation matrix (numeric variables only)
num_vars <- absent %>% select_if(is.numeric)
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle")


numerics <- select(absent, where(is.numeric))
cor_matrix <- cor(numerics, use = "complete.obs")
corrplot(cor_matrix, method = "color")

###### 1.g


####### 1.h. service time
boxplot(absent$Service_time, main = "Service Time")

####### 1.i day of the week
# Bar plot
ggplot(absent, aes(x = `Day_of_the_week`, y = `Absenteeism_time_in_hours`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "orange") +
  labs(title = "Average Absenteeism by Day", x = "Day", y = "Mean Hours")

table(absent$`Day_of_the_week`, absent$absenteeism)


########### Fit multinomial logistic regression
# Convert relevant variables to factors BEFORE modeling
absent$`Month_of_absence` <- as.factor(absent$`Month_of_absence`)
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

#### Visualize the category
values<-unique(absent$Day_of_the_week)
print(values)

logistic_model <- multinom(absenteeism ~ ., data = absent)
summary(logistic_model)

# Interpret Son and Weight
exp(coef(logistic_model)[, "Son"])
exp(coef(logistic_model)[, "Weight"])

# Backward selection
step_model <- step(model_full, direction = "backward", trace = FALSE)

# Final model summary
summary(step_model)

