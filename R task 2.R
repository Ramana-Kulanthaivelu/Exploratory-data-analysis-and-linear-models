#Task 2 (Individual).  Exploratory Data Analysis and Linear Models (MLO2)

library(tidyverse)

data = read.csv("C:/Users/Admin/Pictures/Saved Pictures/Pima_Indians_Diabetes_Dataset1.csv")
# To take stratified random samples
set.seed(733)
strata_sample = data %>% 
  group_by(Target) %>% 
  slice_sample(n=200) %>% 
  ungroup()
# The data sample has been taken and stored in the folder for group and individual tasks.
View(strata_sample)

#install.packages("tidyverse")
#install.packages("factoextra")
#install.packages("psych")
#install.packages("GGally")
#install.packages("olsrr")
#Task 2 Exploratory Data Analysis and Linear Models (MLO2)
# Task 2.1
library(GGally)
# plotting a scatter matrix using ggpairs
str(strata_sample)
strata_sample$Target<-as_factor(strata_sample$Target)
ggpairs(strata_sample,columns = 1:9,aes(color=Target)) +
  ggtitle ("Matrix of Scattered Data Points Along with Correlation Coefficients")

# Create a linear model using the appropriate variable name.
x<-strata_sample$`Body mass index`
y<-strata_sample$`Diabetes pedigree function`
dpf_lm<- lm(y~x) 
summary(dpf_lm)

# predicting the best predictor
correlation_btw_Diabetes_pedigree_function <- cor(strata_sample[, 1:6], strata_sample$`Diabetes pedigree function`)
best_predictor <- rownames(correlation_btw_Diabetes_pedigree_function)[which.max(abs(correlation_btw_Diabetes_pedigree_function))]
cat(" The Best Predictor for the Diabetes Pedigree Function is", best_predictor, "\n")


#Task 2.2(b) compare and accessing the given models
# Model #1
model1 <- lm(formula = `Diabetes pedigree function` ~ `Diastolic blood pressure` + `Body mass index`,strata_sample)
summary(model1)
# Model #2 (best two predictor linear model)
model2 <- lm(formula = `Diabetes pedigree function` ~ `Body mass index`+`Triceps skinfold thickness` ,strata_sample)  
summary(model2)
# Model #3 (best four-predictor liner model)
model3 <- lm(formula = `Diabetes pedigree function` ~+`Body mass index`+`Triceps skinfold thickness`+`Plasma glucose concentration`+`2-Hour serum insulin` ,strata_sample)  
summary(model3)
# By using AIC comparing the models
AIC_1<-AIC(model1)
AIC_2<-AIC(model2)
AIC_3<-AIC(model3)
AIC_vector<- c(AIC_1, AIC_2, AIC_3)
model_names <- c("Model#1", "Model#2", "Model#3")

# Creating a summary table
summarized <- data.frame(Model = model_names, AIC = AIC_vector)
print(summarized)

#predicting the best model
print(best_model<- summarized[which.min(summarized$AIC), ])

# Load the ggplot2 library
library(ggplot2)


# Create a violin plot using ggplot2
ggplot(data = summarized, aes(x = Model, y = AIC)) +
  geom_boxplot(fill = "Red", color = "green") +
  labs(title = "Comparing the models through AIC", x = "Models", y = "AIC values")



