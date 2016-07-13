require(lattice)
#To read data file into frame.
FinalDataFrame = read.csv("C:\\Users\\admin\\Downloads\\Use_Case_Dhruv\\BindIPLData.csv")

#Summary of RunsScored column of FinalData Frame.
Summary1 = summary(FinalDataFrame$RunsScored)
Summary1
#Standard deviation of RunsScored column of FinalDataFrame.
sd1 = sd(FinalDataFrame$RunsScored)
sd1
#Summary of Extras column.
Summary2 = summary(FinalDataFrame$Extras)
Summary2
#Standard deviation of Extras column.
sd2 = sd(FinalDataFrame$Extras)
sd2
FinalDataFrame$Over = factor(trunc(FinalDataFrame$Over))


#Histogram of RunsScored with Over as a factor variable.
histogram(~RunsScored | Over,data = FinalDataFrame,type = "count")

#Histogram of Runs Scored with Team as a factor variable.
histogram(~RunsScored | Team,data = FinalDataFrame,type = "percent")

#Histogram of Runs Scored with StrikeBatsman as a factor variable.
histogram(~RunsScored | StrikeBatsman,data = FinalDataFrame,type = "count")

#Histogram of Runs Scored with Bowler as a factor variable.
histogram(~RunsScored | Bowler,data = FinalDataFrame,type = "count")

FinalDataFrame$RunsScored = factor(FinalDataFrame$RunsScored)
histogram(~RunsScored,data = FinalDataFrame,type = "count")
histogram(~RunsScored,data = FinalDataFrame,type = "percent")

#To find correalation between Extras and Runs Scored.
correalation = cov(FinalDataFrame$RunsScored,FinalDataFrame$Extras)/(var(FinalDataFrame$RunsScored)*var(FinalDataFrame$Extras))
correalation
