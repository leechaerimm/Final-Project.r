library(tidyverse)
library(ggplot2)
install.packages("AICcmodavg")
library(AICcmodavg)
install.packages("broom")
library(broom)
library(dplyr)
install.packages("ggcorrplot")
library(ggcorrplot)

#Question 1 Test of means 
#Read data sets and make data frame
mat <- read.csv("student-mat.csv",sep=";",header=TRUE)
por <- read.csv("student-por.csv",sep=";",header=TRUE)

merged <- merge(mat,por,by=c("school","sex","age","address","famsize",
                             "Pstatus","Medu","Fedu","Mjob","Fjob",
                             "reason","nursery","internet"))
print(nrow(merged)) # 382 students
write.csv(merged, "uci_student.csv")

#1-1 Perform the appropriate t-test to determine if the means for G1 grade and G2 
#grades are the same for the Portuguese language data set

#qqplot to test normality of the data set
qqplot(por$G1, por$G2, xlab="G1", ylab="G2", 
       main = 'Por Q-Q Plot - Normality Test')
qqnorm(por$G1, main ="G1 Q-Q Plot")
qqline(por$G1, main ="G1 Q-Q Plot")
qqnorm(por$G2, main = "G2 Q-Q Plot")
qqline(por$G2, main = "G2 Q-Q Plot")

#Data visualization
ggplot(data= por,mapping = aes(x = G1, y= G2)) + 
  geom_line()+labs(x = "G1", y = "G2", 
                   title = "Line Graph for G1 grade and G2 grade")
ggplot(data= por,mapping = aes(x = G1)) + 
  geom_histogram()+labs(x = "G1", y = "Grade", 
                        title = "Histogram for G1 grade")
ggplot(data= por,mapping = aes(x = G2)) + 
  geom_histogram()+labs(x = "G2", y = "Grade", 
                        title = "Histogram for G2 grade")

#Perform Welch's Two Sample t-test
t.test(por$G1, por$G2, var.equal = FALSE) 

#1-2 Perform the appropriate t-test to determine if the means for G1 grade and 
#G2 grades are the same for the mathematics data set

#qqplot to test normality of the data set
qqplot(mat$G1, mat$G2, xlab="G1", ylab="G2", main = 'Mat Q-Q Plot - Normality Test')
qqnorm(mat$G1, main ="G1 Q-Q Plot")
qqline(mat$G1, main ="G1 Q-Q Plot")
qqnorm(mat$G2, main = "G2 Q-Q Plot")
qqline(mat$G2, main = "G2 Q-Q Plot")

#Perform Welch's Two Sample t-test
t.test(mat$G1, mat$G2, var.equal = FALSE)

#1-3 Perform the appropriate t-test to determine if mean of G3 grades for Mathematics 
#are the same as the G3 grades for Portuguese.

#Test normality
qqplot(mat$G3, por$G3, xlab="mat G3", ylab="por G3", 
       main = 'Mat&Por Q-Q Plot - Normality Test')
qqnorm(mat$G3, main ="Mat G3 Q-Q Plot")
qqline(mat$G3, main ="Mat G3 Q-Q Plot")
qqnorm(por$G3, main = "Por G3 Q-Q Plot")
qqline(por$G3, main = "Por G3 Q-Q Plot")

#Data visualization
ggplot(data= datasample,mapping = aes(x = col1, y= col2), na.rm =TRUE) + geom_line()+
  labs(x = "Por G3", y = "Mat G3", title = "Line Graph of G3 grade of Por and Mat")
ggplot(data= por,mapping = aes(x = G3)) + geom_histogram()+
  labs(x = "G3 Grade", y = "", title = "Histogram for G3 Por grade")
ggplot(data= mat,mapping = aes(x = G3)) + geom_histogram()+
  labs(x = "G3 Grade", y = "", title = "Histogram for G3 Mat grade")

#Perform Welch's Two Sample t-test
t.test(mat$G3, por$G3, var.equal = FALSE)

#1-4 Perform the appropriate t-test to determine if the means of G3 grades are 
#the same for both Portuguese and Mathematics for students appearing in
#both data sets.

#Test normality
qqplot(mat$G3, por$G3, xlab="mat G3", ylab="por G3", 
       main = 'Mat&Por Q-Q Plot - Normality Test')
qqnorm(mat$G3, main ="Mat G3 Q-Q Plot")
qqline(mat$G3, main ="Mat G3 Q-Q Plot")
qqnorm(por$G3, main = "Por G3 Q-Q Plot")
qqline(por$G3, main = "Por G3 Q-Q Plot")

#Data visualization
ggplot(data= datasample,mapping = aes(x = col1, y= col2), na.rm =TRUE) + geom_line()+
  labs(x = "Por G3", y = "Mat G3", title = "Line Graph of G3 grade of Por and Mat")
ggplot(data= por,mapping = aes(x = G3)) + geom_histogram()+
  labs(x = "G3 Grade", y = "", title = "Histogram for G3 Por grade")
ggplot(data= mat,mapping = aes(x = G3)) + geom_histogram()+
  labs(x = "G3 Grade", y = "", title = "Histogram for G3 Mat grade")

#create data frame to match the length of variables
#Finding maximum length
max_ln <-max(c(length(por$G3), length(mat$G3)))
datasample <- data.frame (col1 =c(por$G3, rep(NA, max_ln - length(por$G3))),
                          col2 =c(mat$G3, rep(NA, max_ln - length(mat$G3))))

#checking if the 'datasample' data frame is a data frame
is.data.frame((datasample))

#Create sample of 100 from each data set (Portuguese and Mathematics)  
porsample <- sample(x=por$G3, size = 100)
matsample <-sample(x=mat$G3, size = 100)

#Perform Paired t-test
t.test(porsample, matsample,data = datasample, alternative = "two.sided",
       na.rm =TRUE, paired=TRUE)

#simple plot of differences
difference = porsample - matsample

#Simple 1-to-1 plot of values
plot(porsample, matsample, xlab="Por", ylab="Mat")
abline(0,2, col="black", lwd =2)

#Checking assumptions of the model
hist(difference,col ="gray", main="Histogram of differences",
     xlab="Differences")

#Question2 ANOVA
#2-1 Perform a 2-way ANOVA examining the effect of weekday and weekend alcohol
#consumption on G3 grades for the Mathematics data set

#Test Normality
qqnorm(mat$Walc, xlab ="Mathematics", 
       main ="Q-Q Plot Weekend Alcohol consumption")
qqline(mat$Walc, xlab ="Mathematics",
       main ="Q-Q Plot Weekend Alcohol consumption")
qqnorm(mat$Dalc, xlab ="Mathematics",
       main ="Q-Q Plot Weekday Alcohol consumption")
qqline(mat$Dalc, xlab ="Mathematics",
       main ="Q-Q Plot Weekday Alcohol consumption")

#plot
boxplot(mat$G3 ~ mat$Dalc * mat$Walc, data = mat, frame = FALSE,
        main = "Week & Weekend Alcohol consumption on G3 Grade",
        col = c("#00AFBB","#E7B800"), ylab = "Final grade")

#Perform Two-way ANOVA test to examine the effect of weekday and weekend alcohol consumption
matadditive <-aov(mat$G3 ~mat$Dalc + mat$Walc, data = mat)
summary(matadditive)

matinteraction <-aov(mat$G3 ~mat$Dalc *mat$Walc, data=mat)
summary(matinteraction)

#Create model set of two ANOVA models
models <- list(matadditive, matinteraction)

#Name the models by Additive and Interaction
modelnames <- c('MatAdditive','MatInteraction')

#perform AIC model to determine the best fit
aictab(models, modelnames)

#2-2 Perform a 2-way ANOVA examining the effect of weekday and weekend alcohol
#consumption on G3 grades for the Portuguese data set

#Test Normality
por$Walc <- as.numeric(por$Walc)
por$Dalc <- as.numeric(por$Dalc)
qqnorm(por$Walc, xlab ="Portuguese",
       main ="Q-Q Plot Weekend Alcohol consumption")
qqline(por$Walc, xlab ="Portuguese",
       main ="Q-Q Plot Weekend Alcohol consumption")
qqnorm(por$Dalc, xlab ="Portuguese",
       main ="Q-Q Plot Weekday Alcohol consumption")
qqline(por$Dalc, xlab ="Portuguese",
       main ="Q-Q Plot Weekday Alcohol consumption")

#data visualization
boxplot(por$G3 ~ por$Dalc * por$Walc, data = por, frame = FALSE,
        main = "Week & Weekend Alcohol consumption on G3 Grade",
        col = c("#00AFBB","#E7B800"), ylab = "Final grade")

#Change the type of variables from numeric to factor
por$Walc <-as.factor(por$Walc)
por$Dalc <-as.factor(por$Dalc)

#Perform Two-way ANOVA test to examine the effect of weekday and weekend alcohol consumption
poradditive <-aov(por$G3 ~por$Dalc + por$Walc, data = por)
summary(poradditive)

porinteraction <-aov(por$G3 ~ por$Dalc *por$Walc, data=por)
summary(porinteraction)

#Create a model set for Two-way ANOVA models
models1 <- list(poradditive, porinteraction)

#Name the models by Additive and Interaction
modelnames1 <- c('PorAdditive','PorInteraction')

#perform AIC model to determine the best fit
aictab(models1, modelnames1)

#Perform Tukey's test
TukeyHSD(poradditive)

#2-3 Are there any interesting patterns in the data?
#Answered in separate word doc

#Question3
#3-1 Is age correlated to G3 grade for Mathematics?
#Calculating
#Correlation coefficient
#Using cor() method
mresult <- cor(mat$age, mat$G3, method ="pearson")
cat("Pearson correlation coefficient is: ", mresult)

#Using cor.test() method 
mresulttest <- cor.test(mat$age, mat$G3, method = "pearson")
print(mresulttest)

#creating the plot
plot(mat$age, mat$G3, pch =19, col ="lightblue")

#Regression line
abline(lm(mat$G3 ~ mat$age), col ="red", lwd =3)

#Test normality 
#G3 grade of Mat data
ggplot(data= mat,mapping = aes(x = G3)) + 
  geom_histogram()+labs(x = "G3", y = "", 
                        title = "Histogram for G3 Grade of Mat")
#Age of Mat data 
ggplot(data= mat,mapping = aes(x = age)) + 
  geom_histogram()+labs(x = "Age", y = "", 
                        title = "Histogram for Age of Mat")

#3-2 Is age correlated to G3 grade for Portuguese language?
#Calculating
#Correlation coefficient
#Using cor() method
presult  <- cor(por$age, por$G3, method ="pearson")
cat("Pearson correlation coefficient is:" , presult)

#Using cor.test() method
presulttest <- cor.test(por$age, por$G3, method ="pearson")
print(presulttest)

#creating the plot
plot(por$age, por$G3, pch =19, col ="darkblue")

#Regression line
a <- abline(lm(por$G3 ~ por$age), col ="red", lwd =3)

#Test normality
#G3 grade of Por data 
ggplot(data= por,mapping = aes(x = G3)) + 
  geom_histogram()+labs(x = "G1", y = "Grade", 
                        title = "Histogram for G3 grade")
#Age of Por data
ggplot(data= por,mapping = aes(x = age)) + 
  geom_histogram()+labs(x = "G1", y = "Grade", 
                        title = "Histogram for age")

#create data frame
porage <-por$age
gradepor <-por$G3

#3-3 For the students appearing in both data sets, does the grade in Portuguese
#language correlate to the grade in Mathematics?
#Create data frame of samples from Por G3 and Mat G3 with equal number of variables
porG3 = data.frame(porG3 = as.numeric(rpois(100,10)))
matG3 = data.frame(matG3 = as.numeric(rpois(100,10)))

#create data frame of porG3 and matG3
aa <- data.frame(porG3, matG3)
#calculating
#Correlation Coefficient
#Using cor() method
mpresult <- cor(porG3, matG3, method = "pearson")
cat("Pearson correlation coefficient is: ", mpresult)

#Using cor.test() method
aa <- cor.test(aa$porG3, aa$matG3, method = "pearson")
print(aa)

#creating the plot
plot(aa$porG3, aa$matG3, col ="black", drop.unused.levels = TRUE)

#Regression line
abline(lm(aa$porG3 ~aa$matG3), col ="red", lwd =3, na.rm =TRUE)

#Question4 Linear Regression Analysis
#4-1 Choose one of the correlational analyses you ran above to use for a linear
#regression analysis. What is your predictor variable and what is your 
#response variable?

#Test Normality: QQ plot of residuals
qqnorm(G3grademat)
qqline(G3grademat)
qqnorm(matage)
qqline(matage)

#Linear Regression
matmodel <-lm(G3grademat ~matage, data =mat)
summary(matmodel)

#Create a model
model_diag <-augment(matmodel)
head(model_diag)

#Data visualization & Assumptions (written in word doc)
par(mfrow = c(2,2))
plot(matmodel)

#Question5 Choose one or two categorical variable that you think might influence
#academic performance and perform either an ANOVA or a regression model for
#qualitative variables. Report your results

#Test Normality
qqnorm(mat$famrel, xlab = "Family relationship", main = "Q-Q Plot")
qqline(mat$famrel, xlab = "Family relationship", main = "Q-Q Plot")
qqnorm(mat$health, xlab = "Current Health Status", main = "Q-Q Plot")
qqline(mat$health , xlab = "Current Health Status", main = "Q-Q Plot")

#Two way ANOVA test for additive and interaction model
additive5 <-aov(mat$G3 ~mat$famrel + mat$health, data = mat)
summary(additive5)

interaction5 <-aov(mat$G3 ~mat$famrel *mat$health, data=mat)
summary(interaction5)

#Data visualization
boxplot(mat$G3 ~ mat$famrel * mat$health, data = mat, frame = FALSE,
        main = "Family relationship and Current health condition on Mat Final Grade",
        col = c("#00AFBB","#E7B800"), ylab = "Final grade")

#create a model set for Two-way ANOVA models
model5 <- list(additive5,interaction5)

#Create model set names for aictab
model4name <- c("Additive","Interaction")

#perform AIC model to determine the best fit
aictab(model4,model4name)
