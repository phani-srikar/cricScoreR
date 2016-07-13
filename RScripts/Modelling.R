#Packages Required.
install.packages("AER")
install.packages("pscl")
library(pscl)
library(AER)

#To read data file into frame.
OverallDataFrame = read.csv("C:\\Users\\admin\\Downloads\\Use_Case_Dhruv\\BindIPLData.csv")

FinalDataFrame = OverallDataFrame[1:100000,]

#Compare statistic values for train and overall data .
summary(OverallDataFrame$RunsScored)
summary(FinalDataFrame$RunsScored)

#Number of unique batsmen.
length(unique(OverallDataFrame$StrikeBatsman))
length(unique(FinalDataFrame$StrikeBatsman))

#Number of unique bowlers.
length(unique(OverallDataFrame$Bowler))
length(unique(FinalDataFrame$Bowler))

#Number of unique teams
length(unique(OverallDataFrame$Team))
length(unique(FinalDataFrame$Team))

#Number of innings
length(unique(OverallDataFrame$Inning))
length(unique(FinalDataFrame$Inning))


#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl <- glm(RunsScored~StrikeBatsman,family=poisson,data=FinalDataFrame)

#Plot of Coefficients of Strike Batsman and Strike Batsman.
plot(unique(FinalDataFrame$StrikeBatsman),exp(glmipl$coefficients),xlab = "StrikeBatsman",ylab = "Coefficients",asp =0.1,main = "Coefficients v/s StrikeBatsman")

#To know the number of unique batsmen.
length(glmipl$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl$weights)
WeightCoefficients =data.frame(unique(glmipl$weights),glmipl$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl$linear.predictors),glmipl$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#Highest predicted values 
head(sort(predict(glmipl),decreasing = TRUE))

#For summary of model.
SummaryModel1 = summary(glmipl)
SummaryModel1

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel1$deviance)/(SummaryModel1$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl,trafo =1)
print(DispersionTest)



#Zero inflated model to diminish over dispersion.

zeroinfl <- zeroinfl(RunsScored~StrikeBatsman,data=FinalDataFrame)

#For summary of zero inflated model.
SummaryModel21 = summary(zeroinfl)
SummaryModel21



#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl2 <- glm(RunsScored~Bowler,family=poisson,data=FinalDataFrame)


#To know the number of unique bowler.
length(glmipl2$coefficients)

#To check coefficients of the model.

class(glmipl2$coefficients)
CoefficientResults<-sort(glmipl2$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl2$weights)
WeightCoefficients =data.frame(unique(glmipl2$weights),glmipl2$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl2$linear.predictors),glmipl2$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel2 = summary(glmipl2)
SummaryModel2

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel2$deviance)/(SummaryModel2$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl2,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.

zeroinfl2 <- zeroinfl(RunsScored~Bowler,data=FinalDataFrame)

#For summary of model.
SummaryModel22 = summary(zeroinfl2)
SummaryModel22


View(head(FinalDataFrame))

#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman and Bowler as predictors.
glmipl3 <- glm(RunsScored~Bowler+StrikeBatsman,family=poisson,data=FinalDataFrame)

#TO know the number of unique bowler.
length(glmipl3$coefficients)

#To know the coefficients.
CoefficientResults<-sort(glmipl3$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl3$weights)
WeightCoefficients =data.frame(unique(glmipl3$weights),glmipl3$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl3$linear.predictors),glmipl3$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl3)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel3 = summary(glmipl3)
SummaryModel3

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel3$deviance)/(SummaryModel3$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl3,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.

zeroinfl3 <- zeroinfl(RunsScored~StrikeBatsman+Bowler,data=FinalDataFrame)

#For summary of model.
SummaryModel23 = summary(zeroinfl3)
SummaryModel23


View(head(FinalDataFrame))

#Applied Poisson Regression with Runs Scored as dependent and Team as predictor.
glmipl4 <- glm(RunsScored~Team,family=poisson,data=FinalDataFrame)

#To know the number of unique entries.
length(glmipl4$coefficients)

#To know the coefficients of model.
CoefficientResults<-sort(glmipl4$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl4$weights)
WeightCoefficients =data.frame(unique(glmipl4$weights),glmipl4$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl4$linear.predictors),glmipl4$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl4)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel4 = summary(glmipl4)
SummaryModel4

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel4$deviance)/(SummaryModel4$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl4,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.

zeroinfl4 <- zeroinfl(RunsScored~Team,data=FinalDataFrame)

#For summary of model.
SummaryModel24 = summary(zeroinfl4)
SummaryModel24



View(head(FinalDataFrame))

#Make Inning a factor variable
FinalDataFrame$Inning = factor(FinalDataFrame$Inning)
levels(FinalDataFrame$Inning)

#Applied Poisson Regression with Runs Scored as dependent and Inning as independent variable.
glmipl5 <- glm(RunsScored~Inning,family=poisson,data=FinalDataFrame)

#To know the number of unique factors.
length(glmipl5$coefficients)



#To check coefficients of the model.
CoefficientResults<-sort(glmipl5$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)



#To check for other properties of model.
length(glmipl5$weights)
WeightCoefficients =data.frame(unique(glmipl5$weights),glmipl5$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl5$linear.predictors),glmipl5$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl5)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel5 = summary(glmipl5)
SummaryModel5

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel5$deviance)/(SummaryModel5$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl5,trafo =1)
print(DispersionTest)


#Zero inflated model to diminish over dispersion.

zeroinfl5 <- zeroinfl(RunsScored~Inning,data=FinalDataFrame)

#For summary of model.
SummaryModel25 = summary(zeroinfl5)
SummaryModel25


View(head(FinalDataFrame))

#Applied Poisson Regression with Extras as dependent and Extras as independent variable.
glmipl6 <- glm(RunsScored~Extras,family=poisson,data=FinalDataFrame)

#To check coefficients of the model.
CoefficientResults<-sort(glmipl6$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To know the number of unique extras.
length(glmipl6$coefficients)

#To check for other properties of model.
length(glmipl6$weights)
WeightCoefficients =unique(glmipl6$weights)
WeightCoefficients

PredictorCoefficients =(unique(glmipl6$linear.predictors))
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl6)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel6 = summary(glmipl6)
SummaryModel6

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel6$deviance)/(SummaryModel6$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl6,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.

zeroinfl6 <- zeroinfl(RunsScored~Extras,data=FinalDataFrame)

#For summary of model.
SummaryModel26 = summary(zeroinfl6)
SummaryModel26


#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl7 <- glm(RunsScored~NonStrikeBatsman,family=poisson,data=FinalDataFrame)

#To know the number of unique batsmen.
length(glmipl7$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl7$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl7$weights)
WeightCoefficients =data.frame(unique(glmipl7$weights),glmipl7$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl7$linear.predictors),glmipl7$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl7)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel7 = summary(glmipl7)
SummaryModel7

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel7$deviance)/(SummaryModel7$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl7,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.

zeroinfl7 <- zeroinfl(RunsScored~NonStrikeBatsman,data=FinalDataFrame)

#For summary of model.
SummaryModel27 = summary(zeroinfl7)
SummaryModel27



View(head(FinalDataFrame))

#To remove the ball number in Over after decimal.
FinalDataFrame$Over = trunc(FinalDataFrame$Over)

#To convert Over column to factor .
FinalDataFrame$Over = factor(FinalDataFrame$Over)
levels(FinalDataFrame$Over)

#Applied Poisson Regression with Runs Scored as dependent and Over as independent variable.
glmipl8 <- glm(RunsScored~Over,family=poisson,data=FinalDataFrame)

#To know the number of unique over.
length(glmipl8$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl8$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl8$weights)
WeightCoefficients =data.frame(unique(glmipl8$weights),glmipl8$coefficients)
WeightCoefficients

PredictorCoefficients =data.frame(unique(glmipl8$linear.predictors),glmipl8$coefficients)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl8)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel8 = summary(glmipl8)
SummaryModel8

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel8$deviance)/(SummaryModel8$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl8,trafo =1)
print(DispersionTest)

#Zero inflated model to diminish over dispersion.
zeroinfl8 <- zeroinfl(RunsScored~Over,data=FinalDataFrame)

#For summary of model.
SummaryModel28 = summary(zeroinfl8)
SummaryModel28


#Make the Inning column to be a factor variable.
FinalDataFrame$Inning = factor(FinalDataFrame$Inning)

#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl9 <- glm(RunsScored~StrikeBatsman+Bowler+Team+Inning,family=poisson,data=FinalDataFrame)

#To know the number of unique factors.
length(glmipl9$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl9$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl9$weights)
WeightCoefficients =(unique(glmipl9$weights))
WeightCoefficients

PredictorCoefficients =(unique(glmipl9$linear.predictors))
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl9)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#For summary of model.
SummaryModel9 = summary(glmipl9)
SummaryModel9

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel9$deviance)/(SummaryModel9$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl9,trafo =1)
print(DispersionTest)


#Zero inflated model to diminish over dispersion.
zeroinfl9 <- zeroinfl(RunsScored~StrikeBatsman+Bowler+Team+Inning,data=FinalDataFrame)

#For summary of model.
SummaryModel29 = summary(zeroinfl9)
SummaryModel29


#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl10 <- glm(RunsScored~StrikeBatsman+Team,family=poisson,data=FinalDataFrame)

#To know the number of unique batsmen.
length(glmipl10$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl10$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl10$weights)
WeightCoefficients =unique(glmipl10$weights)
WeightCoefficients

PredictorCoefficients =unique(glmipl10$linear.predictors)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl10)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#Highest predicted values 
head(sort(predict(glmipl10),decreasing = TRUE))

#For summary of model.
SummaryModel10 = summary(glmipl10)
SummaryModel10

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel10$deviance)/(SummaryModel10$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl10,trafo =1)
print(DispersionTest)



#Zero inflated model to diminish over dispersion.

zeroinfl10 <- zeroinfl(RunsScored~StrikeBatsman+Team,data=FinalDataFrame)

#For summary of zero inflated model.
SummaryModel210 = summary(zeroinfl10)
SummaryModel210


#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl11 <- glm(RunsScored~StrikeBatsman+Inning,family=poisson,data=FinalDataFrame)

#To know the number of unique batsmen.
length(glmipl11$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl11$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl11$weights)
WeightCoefficients =unique(glmipl11$weights)
WeightCoefficients

PredictorCoefficients =unique(glmipl11$linear.predictors)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl11)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#Highest predicted values 
head(sort(predict(glmipl11),decreasing = TRUE))

#For summary of model.
SummaryModel11 = summary(glmipl11)
SummaryModel11

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel11$deviance)/(SummaryModel11$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl11,trafo =1)
print(DispersionTest)



#Zero inflated model to diminish over dispersion.

zeroinfl11 <- zeroinfl(RunsScored~StrikeBatsman+Inning,data=FinalDataFrame)

#For summary of zero inflated model.
SummaryModel211 = summary(zeroinfl11)
SummaryModel211


#Applied Poisson Regression with Runs Scored as dependent and StrikeBatsman as independent variable.
glmipl12 <- glm(RunsScored~NonStrikeBatsman+Team,family=poisson,data=FinalDataFrame)

#To know the number of unique batsmen.
length(glmipl12$coefficients)

#To check coefficients of the model.

CoefficientResults<-sort(glmipl12$coefficients,decreasing = TRUE)
CoefficientResults
head(CoefficientResults)

#To check for other properties of model.
length(glmipl12$weights)
WeightCoefficients =unique(glmipl12$weights)
WeightCoefficients

PredictorCoefficients =unique(glmipl12$linear.predictors)
PredictorCoefficients

#Predictions for test data only.Default argument for new data is NULL
Predictions = predict(glmipl12)
class(Predictions)
head(sort(Predictions,decreasing = TRUE))

#Highest predicted values 
head(sort(predict(glmipl12),decreasing = TRUE))

#For summary of model.
SummaryModel12 = summary(glmipl12)
SummaryModel12

#Testing for Overdispersion of data.
OverDispersionRatio = (SummaryModel12$deviance)/(SummaryModel12$df.residual)
print(OverDispersionRatio)

#Overdispersion testing using Library AER and dispersiontest function.
DispersionTest = dispersiontest(glmipl12,trafo =1)
print(DispersionTest)



#Zero inflated model to diminish over dispersion.

zeroinfl12 <- zeroinfl(RunsScored~StrikeBatsman+NonStrikeBatsman,data=FinalDataFrame)

#For summary of zero inflated model.
SummaryModel212 = summary(zeroinfl12)
SummaryModel212



