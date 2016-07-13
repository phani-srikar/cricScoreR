#To list down the various models present in an environment.
env = globalenv()
ls(env)

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and glmipl2(RunsScored~Bowler)
test1 = anova(glmipl,glmipl2,test = "Chisq")
test1

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and glmipl4(RunsScored~Team)
test2 = anova(glmipl,glmipl4,test = "Chisq")
test2

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and glmipl5(RunsScored~Inning)
test3 = anova(glmipl,glmipl5,test = "Chisq")
test3

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and glmipl6(RunsScored~Extras)
test4 = anova(glmipl,glmipl6,test = "Chisq")
test4

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and glmipl6(RunsScored~NonStrikeBatsman)
test5 = anova(glmipl,glmipl7,test = "Chisq")
test5

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and all models
test6 = anova(glmipl,glmipl2,glmipl4,glmipl5,glmipl6,glmipl7,glmipl8,test = "Chisq")
test6

#ChiSquaretest between glmipl(RunsScored~StrikeBatsman)and left out models
test7 = anova(glmipl,glmipl6,glmipl7,glmipl8,test = "Chisq")
test7

#ChiSquare test between glmipl(RunsScored~StrikeBatsman) and glmipl8(RunsScored~Over)
test8 =anova(glmipl,glmipl8,test = "Chisq")
test8



#Vuong test for comparing closeness hypothesis.
#Null Hypothesis = Both models are equally close to data generating process.
#Alternate Hypothesis = One model is closer than other model.
#Test would be applied between Poisson Regression based model and Zero Inflated Model.

#Test between model 1 with (RunsScored ~ StrikeBatsman)
vtest1 = vuong(glmipl,zeroinfl)

#Test between model 2 with (RunsScored ~ Bowler)
vtest2 = vuong(glmipl2,zeroinfl2)

#Test between model 3 with (RunsScored ~ StrikeBatsman+Bowler)
vtest3 = vuong(glmipl3,zeroinfl3)

#Test between model 4 with (RunsScored ~ Team)
vtest4 = vuong(glmipl4,zeroinfl4)

#Test between model 5 with (RunsScored ~ Inning)
vtest5 = vuong(glmipl5,zeroinfl5)

#Test between model 6 with (RunsScored ~ Extras)
vtest6 = vuong(glmipl6,zeroinfl6)

#Test between model 7 with (RunsScored ~ NonStrikeBatsman)
vtest7 = vuong(glmipl7,zeroinfl7)

#Test between model 8 with (RunsScored ~ Over)
vtest8 = vuong(glmipl8,zeroinfl8)

#Test between model 9 with ( Runs Scored ~ StrikeBatsman+Bowler+Team+Inning)
vtest9 = vuong(glmipl9,zeroinfl9)

#Accuracy of model 1
Accuracy1 = 1-pchisq(185933,99618)
print(Accuracy1)

#Accuracy of model 2
Accuracy2 = 1-pchisq(186751,99714)
print(Accuracy2)

#Accuracy of model 1
Accuracy3 = 1-pchisq(185933,99618)
print(Accuracy3)

#Accuracy of model 1
Accuracy4 = 1-pchisq(188212,99989)
print(Accuracy4)

#Accuracy of model 1
Accuracy5 = 1-pchisq(188291,99996)
print(Accuracy5)


#Accuracy of model 1
Accuracy6 = 1-pchisq(178688,99998)
print(Accuracy6)

#Accuracy of model 1
Accuracy7 = 1-pchisq(186735,99619)
print(Accuracy7)

#Accuracy of model 1
Accuracy8 = 1-pchisq(186001,99980)
print(Accuracy8)

