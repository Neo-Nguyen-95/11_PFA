file.choose()
[1] "/Ken/Teaching/ ITS Courses/PSLC Summer 06/Data Mining/Excel:DataShop example/Geometry Area 96-97/GeoArea96-97-decomp.csv"

# Make sure the file doesn't have any "#" in it, like "Opportunity #".  These cause read errors like "more columns than column names"
Area = read.table(file.choose(), header=T, sep=",")
attach(Area)
summary(Area)

N=length(Anon.Student.Id)

## To do items
# Check BIC (and other) values of AFM models are consistent with DataShop.
# Is there significant student variability in successes on compose-by-addition that is not predicted by proficiency?  Does this variability predict transfer to decompose?  That is, do students who have more than expected prior success on compose-by-addition do better on transfer?  Most specifically, do prior-corrects on compose-by-addition yield higher performance on decompose? [add a Decompose-Prep column that has only prior-corrects on compose-by-addition (and perhaps the same for circle area?)]
# Is there a residual bias in the PFM model for low opportunities?

### Typical AFM model
afm = glm(Success~Anon.Student.Id+KC11+KC11:Opp11-1, family=binomial(), data=Area); summary(afm);
length(coef(afm));
(summary(afm)$aic-2*length(coef(afm)))/-2;
summary(afm)$aic+length(coef(afm))*(log(N)-2)
# Parameters = 80
# Liklihood = -2462.061
# AIC = 5084.1
# BIC = 5607.145

# See file GeoArea96-97-decomp.xls for summary of fitted parameter values of these models.

### Phil's model, but with student intercept
crt.decomp = glm(Success~Anon.Student.Id+KC11+KC11:PriorCorrects11+KC11:PriorErrors11-1, family=binomial(), data=Area); summary(crt.decomp);
length(coef(crt.decomp));
(summary(crt.decomp)$aic-2*length(coef(crt.decomp)))/-2;
summary(crt.decomp)$aic+length(coef(crt.decomp))*(log(N)-2)
# Parameters = 91
# Liklihood = -2444.293 vs. -2462.061
# AIC = 5070.6 vs. 5084.1
# BIC = 5665.523 vs. 5607.145

### Phil's model, but without student intercept
crt.decomp.noS = glm(Success~KC11+KC11:PriorCorrects11+KC11:PriorErrors11-1, family=binomial(), data=Area); summary(crt.decomp.noS);
length(coef(crt.decomp.noS));
(summary(crt.decomp.noS)$aic-2*length(coef(crt.decomp.noS)))/-2;
summary(crt.decomp.noS)$aic+length(coef(crt.decomp.noS))*(log(N)-2)
# Parameters = 33
# Liklihood = -2549.926 vs. -2444.293 vs. -2462.061
# AIC = 5165.9 vs. 5070.6 vs. 5084.1
# BIC = 5381.598 vs. 5665.523 vs. 5607.145  BEST of the three

#### Now the same 3 models with KC12 rather than KC11
### Typical AFM model
afm = glm(Success~Anon.Student.Id+KC12+KC12:Opp12-1, family=binomial(), data=Area); summary(afm);
length(coef(afm));
(summary(afm)$aic-2*length(coef(afm)))/-2;
summary(afm)$aic+length(coef(afm))*(log(N)-2)
# Parameters = 82
# Liklihood = -2444.636 vs. -2462.061
# AIC = 5053.3
# BIC = 5589.371 vs. 5607.145 Beats K11 (as per DataShop)

### Phil's model, but with student intercept
crt.decomp = glm(Success~Anon.Student.Id+KC12+KC12:PriorCorrects12+KC12:PriorErrors12-1, family=binomial(), data=Area); summary(crt.decomp);
length(coef(crt.decomp));
(summary(crt.decomp)$aic-2*length(coef(crt.decomp)))/-2;
summary(crt.decomp)$aic+length(coef(crt.decomp))*(log(N)-2)
# Parameters = 94
# Liklihood = -2424.21
# BIC = 5650.971 

### Phil's model, but without student intercept
crt.decomp.noS = glm(Success~KC12+KC12:PriorCorrects12+KC12:PriorErrors12-1, family=binomial(), data=Area); summary(crt.decomp.noS);
length(coef(crt.decomp.noS));
(summary(crt.decomp.noS)$aic-2*length(coef(crt.decomp.noS)))/-2;
summary(crt.decomp.noS)$aic+length(coef(crt.decomp.noS))*(log(N)-2)
# Parameters = 36
# Liklihood = -2530.696
# BIC = 5368.753 Again, BEST of the three

## Phil's model, but with student intercept & rho is general (not KC specific)
crt.decomp.genRho = glm(Success~Anon.Student.Id+KC12+KC12:PriorCorrects12+PriorErrors12-1, family=binomial(), data=Area); summary(crt.decomp.genRho);
length(coef(crt.decomp.genRho));
(summary(crt.decomp.genRho)$aic-2*length(coef(crt.decomp.genRho)))/-2;
summary(crt.decomp.genRho)$aic+length(coef(crt.decomp.genRho))*(log(N)-2)
# Parameters = 83
# Liklihood = -2444.754
# AIC = 5055.5
# BIC = 5598.145  better than with KC specific Rho!

## Phil's model, but with student intercept & no rho
crt.decomp.noRho = glm(Success~Anon.Student.Id+KC12+KC12:PriorCorrects12-1, family=binomial(), data=Area); summary(crt.decomp.noRho);
length(coef(crt.decomp.noRho));
(summary(crt.decomp.noRho)$aic-2*length(coef(crt.decomp.noRho)))/-2;
summary(crt.decomp.noRho)$aic+length(coef(crt.decomp.noRho))*(log(N)-2)
# Parameters = 82
# Liklihood = -2445.134
# AIC = 5054.3
# BIC = 5590.365  better than with KC specific Rho!

## Phil's model, but with student intercept & rho only
crt.decomp.RhoOnly = glm(Success~Anon.Student.Id+KC12+KC12: PriorErrors12-1, family=binomial(), data=Area); summary(crt.decomp.RhoOnly);
length(coef(crt.decomp.RhoOnly));
(summary(crt.decomp.RhoOnly)$aic-2*length(coef(crt.decomp.RhoOnly)))/-2;
summary(crt.decomp.RhoOnly)$aic+length(coef(crt.decomp.RhoOnly))*(log(N)-2)
# Parameters = 82
# Liklihood = -2481.181 vs. -2445.134
# AIC = 5126.4
# BIC = 5662.46 vs. 5590.365  worse than no Rho

# How models with slope compare with those without it?
afm.dfa = glm(Success~Anon.Student.Id+KC12-1, family=binomial(), data=Area); summary(afm.dfa);
length(coef(afm.dfa));
(summary(afm.dfa)$aic-2*length(coef(afm.dfa)))/-2;
summary(afm.dfa)$aic+length(coef(afm.dfa))*(log(N)-2)
# Parameters = 70 vs. 82
# Liklihood = -2543.043 vs. -2424.21
# BIC = 5683.73 vs. 5650.971 

# Is there significant student variability in successes on compose-by-addition that is not predicted by proficiency?  Does this variability predict transfer to decompose?  That is, do students who have more than expected prior success on compose-by-addition do better on transfer?  Most specifically, do prior-corrects on compose-by-addition yield higher performance on decompose? 
#How to implment: Add a Decompose-Prep column that has only prior-corrects on compose-by-addition (and perhaps the same for circle area?) Then need to put these values in rows corresponding with decompose -- they are not in such rows now!

crt.decomp.trans = glm(Success~Anon.Student.Id+KC12+KC12:PriorCorrects12+DecomposePrep-1, family=binomial(), data=Area); summary(crt.decomp.trans);
length(coef(crt.decomp.trans));
(summary(crt.decomp.trans)$aic-2*length(coef(crt.decomp.trans)))/-2;
summary(crt.decomp.trans)$aic+length(coef(crt.decomp.trans))*(log(N)-2)
# Parameters = 83
# Liklihood = -2444.75
# AIC = 5055.5
# BIC = 5598.136

DecomposePrep                                         0.07755    0.08878   0.873 0.382427    
KC12compose-by-addition:PriorCorrects12               0.15646    0.12607   1.241 0.214568    
KC12decompose:PriorCorrects12                         0.12805    0.05305   2.414 0.015786 *  

# Need to clean up compose-by-addition. Get rid of segment additions. IT DOESN'T HAVE ANY! Also, compose-by-multiplication may be relevant to future decomposes.
# Does decompose generalize between addition and multiplication?


#####
# Try 1 parameter IRT (with student as fixed effect)
# First try DFA model where items are differentiated into KC categories
afm.dfa = glm(Success~Anon.Student.Id+KC12-1, family=binomial(), data=Area); summary(afm.dfa);
length(coef(afm.dfa));
(summary(afm.dfa)$aic-2*length(coef(afm.dfa)))/-2;
summary(afm.dfa)$aic+length(coef(afm.dfa))*(log(N)-2)

# Parameters = 70 vs. 82
# Liklihood = -2543.043 vs. -2444.636
# BIC = 5683.73 vs. 5589.371 worse than afm model using KC12

# First try DFA model where items are differentiated into KC categories
area.irt = glm(Success~Anon.Student.Id+KC12-1, family=binomial(), data=Area); summary(afm.dfa);
length(coef(afm.dfa));
(summary(afm.dfa)$aic-2*length(coef(afm.dfa)))/-2;
summary(afm.dfa)$aic+length(coef(afm.dfa))*(log(N)-2)
# Parameters = 70 vs. 82
# Liklihood = -2543.043 vs. -2444.636
# BIC = 5683.73 vs. 5589.371 worse than afm model using KC12

## Now try IRT model 
# Need to create an item variable, Unique.Step, by concatenating (in Excel) the Problem variable
# and the Step variable.  There are 139 items, that is, levels (unique values) of Unique.Step

area.irt = glm(Success~Anon.Student.Id+Unique.Step-1, family=binomial(), data=Area); summary(area.irt);
length(coef(area.irt));
(summary(area.irt)$aic-2*length(coef(area.irt)))/-2;
summary(area.irt)$aic+length(coef(area.irt))*(log(N)-2)
# Parameters = 197 vs. 70 vs. 82
# Liklihood = -2328.972 vs. -2543.043 vs. -2444.636
# BIC = 6339.886 vs. 5683.73 vs. 5589.371 much worse than dfa and afm model using KC12



# Try applying 2 parameter IRT to this data set.
library(ltm)
# First need to create format that LTM requies.
file.choose()
[1] "/Ken/Teaching/ ITS Courses/PSLC Summer 06/Data Mining/Excel:DataShop example/Geometry Area 96-97/GeoArea96-97-decomp-irt.csv"
area.irt.data = read.table(file.choose(), header=T, sep=",")
# NOTE: In Excel, I inserted a space value (" ") into all the blank cells ("") in the final column
# of the file so that Excel does not truncate the number of commas it inserts for rows with
# incomplete values.  
summary(area.irt.data)
descript(area.irt.data)
#Error in chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]])) : 
#  at least one entry of 'x' must be positive
#I believe I'm getting the above because the data includes,in the 1st column,the student id's.
area.irt.data2 = area.irt.data[,2:140]
descript(area.irt.data2)
#Error in chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]])) : 
#  at least one entry of 'x' must be positive
# Nope--maybe it is items with all 0's.
# I deleted the variables (columns 138 & 139) that had only 0 values.
#> descript(area.irt.data)
#Error in chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]])) : 
#  at least one entry of 'x' must be positive
# Still geting problem -- get rid of student column:
area.irt.data2 = area.irt.data[,2:138]
descript(area.irt.data2)
# Same error -- perhaps it is the students (rows) that are 0. Just one of them.
# deleting in csv file and reloading
# stillgetting error -- maybe itis lack ofexplicit NA?
# created a new, simpler file:
file.choose()
[1] "/Ken/Teaching/ ITS Courses/PSLC Summer 06/Data Mining/Excel:DataShop example/Geometry Area 96-97/GeoArea96-97-decomp-irt2.csv"
descript(area.irt.data)
# That worked!  Maybe it is deleting the studentcolumn in Excel. 
# Nope. But, perhaps better with fewer missing data points
# I'm trying the 39 students with more than 20 (item) responses and all items with more 
# than 10 (students).
# Still having problem. But new error message:  
#> descript(area.irt.data)
#Error in chisq.test(table(X[, ind[i, 1]], X[, ind[i, 2]])) : 
#  'x' must at least have 2 elements
# Should incrementally move toward irt2 file by deleting columns or rows.
# 
# S=38;  I=107 not working
# S=38;I=52 not working


> rasch-fit=rasch(area.irt.data)
Error in rasch - fit = rasch(area.irt.data) : 
  could not find function "-<-"


# How about an IRT model where student proficiency improves for each item performed?
# Or improves for each item correct?
# The point is what if we did knowledge tracing as adaptive testing is done?  This doesn't
# make so much sense perhaps if there high DIF items are being included or not accounted for.







#############
# Stuff below here is from prior file.



### Testing for condition effect ###
cond.model = glm(Partial.Credit.Crt~Student+Skills6+LnOpp6:Skills6+LnOpp6:Condition-1, family=binomial(), data=Excel); summary(cond.model)
#                              Estimate Std. Error z value Pr(>|z|)    
# LnOpp6:ConditionIN           0.077973   0.107563   0.725 0.468510    
cond.model2 = glm(Partial.Credit.Crt~Condition+Skills6+LnOpp6:Skills6+LnOpp6:Condition-1, family=binomial(), data=Excel); summary(cond.model2)
#                             Estimate Std. Error z value Pr(>|z|)    
# ConditionEX                 -0.903693   0.212451  -4.254 2.10e-05 ***
# ConditionIN                 -0.237238   0.212764  -1.115 0.264840    
# ConditionIN:LnOpp6           0.084410   0.083703   1.008 0.313237    
cond.model3 = glm(Partial.Credit.Crt~Skills6+LnOpp6:Skills6+LnOpp6:Condition-1, family=binomial(), data=Excel); summary(cond.model3)
#                              Estimate Std. Error z value Pr(>|z|)    
# LnOpp6:ConditionIN           0.36584    0.03931   9.307  < 2e-16 ***
    
### Computing BIC
N=length(Student)
aic=summary(cond.model)$aic
k=length(coef(cond.model))
log.like=(aic-2*k)/-2
bic.check=-2*log.like+k*log(N)
bic=summary(cond.model)$aic+length(coef(cond.model))*(log(N)-2)
# 4723.495
summary(cond.model2)$aic+length(coef(cond.model2))*(log(N)-2)
# 5316.89
summary(cond.model3)$aic+length(coef(cond.model3))*(log(N)-2)
# 5333.397

### Which skill model is best OVERALL ###
Item = factor(ProbNum)
skill.model30 = glm(Assist.Prob~Student+Item+LnOpp30:Item-1, family=binomial(), data=Excel); summary(skill.model30) ;
length(coef(skill.model30));
(summary(skill.model30)$aic-2*length(coef(skill.model30)))/-2;
summary(skill.model30)$aic+length(coef(skill.model30))*(log(N)-2)
# Parameters = 108
# Liklihood = -2070
# AIC = 4356.5
# BIC = 5061.887

skill.model6 = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=Excel); summary(skill.model6)
length(coef(skill.model6));
(summary(skill.model6)$aic-2*length(coef(skill.model6)))/-2;
summary(skill.model6)$aic+length(coef(skill.model6))*(log(N)-2)
# Parameters = 60
# Liklihood = -2097.242
# AIC = 4314.5
# BIC = 4706.362

skill.model3 = glm(Assist.Prob~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill.model3)
length(coef(skill.model3));
(summary(skill.model3)$aic-2*length(coef(skill.model3)))/-2;
summary(skill.model3)$aic+length(coef(skill.model3))*(log(N)-2)
# Parameters = 54
# Liklihood = -2102.903
# AIC = 4313.8
# BIC = 4666.496

skill.model1 = glm(Assist.Prob~Student+LnOpp1-1, family=binomial(), data=Excel); summary(skill.model1)
length(coef(skill.model1));
(summary(skill.model1)$aic-2*length(coef(skill.model1)))/-2;
summary(skill.model1)$aic+length(coef(skill.model1))*(log(N)-2)
# Parameters = 50
# Liklihood = -2447.223
# AIC = 4994.4
# BIC = 5321.011

### Which skill model is best OVERALL ###
skill.model6 = glm(Partial.Credit.Crt~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=Excel); summary(skill.model6)
# AIC: 4323.9
bic=summary(skill.model6)$aic+length(coef(skill.model6))*(log(N)-2)
# BIC: 4715.823

skill.model5 = glm(Partial.Credit.Crt~Student+Skills5+LnOpp5:Skills5-1, family=binomial(), data=Excel); summary(skill.model5)
# AIC: 4320.8
bic=summary(skill.model5)$aic+length(coef(skill.model5))*(log(N)-2)
# BIC: 4699.612

skill.model4 = glm(Partial.Credit.Crt~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=Excel); summary(skill.model4)
# AIC: 4323.7
summary(skill.model4)$aic+length(coef(skill.model4))*(log(N)-2)
# BIC: 4689.432

skill.model3 = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill.model3)
# AIC: 4323.5
summary(skill.model3)$aic+length(coef(skill.model3))*(log(N)-2)
# BIC: 4676.178

skill.model2 = glm(Partial.Credit.Crt~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=Excel); summary(skill.model2)
# AIC: 4347.5
summary(skill.model2)$aic+length(coef(skill.model2))*(log(N)-2)
# BIC: 4687.167

## So far skill3 is best on BIC (4676) with 2 and 4 close behind (4687 and 4689).
## Try the 1 skill model!
## On AIC, skill5 (4321) and skill3 (4324) are best.

### Fit skill models separately for EX and IN groups.
EX.data = Excel[Excel$Condition=="EX",]
detach(Excel); attach(EX.data)
EX.N = length(Student)

## Which skill model is best for EX? ##
EX.skill.model6 = glm(Partial.Credit.Crt~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=EX.data); summary(EX.skill.model6)
# AIC: 2330.2 [Skill6 is best on AIC for EX]
summary(EX.skill.model6)$aic+length(coef(EX.skill.model6))*(log(EX.N)-2)
# BIC: 2530.147

EX.skill.model5 = glm(Partial.Credit.Crt~Student+Skills5+LnOpp5:Skills5-1, family=binomial(), data=EX.data); summary(EX.skill.model5)
# AIC: 2332.6
summary(EX.skill.model5)$aic+length(coef(EX.skill.model5))*(log(EX.N)-2)
# BIC: 2521.155

EX.skill.model4 = glm(Partial.Credit.Crt~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=EX.data); summary(EX.skill.model4)
# AIC: 2333.1
summary(EX.skill.model4)$aic+length(coef(EX.skill.model4))*(log(EX.N)-2)
# BIC: 2510.258

EX.skill.model3 = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=EX.data); summary(EX.skill.model3)
# AIC: 2332.7
summary(EX.skill.model3)$aic+length(coef(EX.skill.model3))*(log(EX.N)-2)
# BIC: 2498.427 [Skill3 is best on BIC for EX]

EX.skill.model2 = glm(Partial.Credit.Crt~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=EX.data); summary(EX.skill.model2)
# AIC: 2365.3
summary(EX.skill.model2)$aic+length(coef(EX.skill.model2))*(log(EX.N)-2)
# BIC: 2519.521

## Which skill model is best for IN? ##
IN.data = Excel[Excel$Condition=="IN",]
detach(EX.data); attach(IN.data)
IN.N = length(Student)

IN.skill.model6 = glm(Partial.Credit.Crt~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=IN.data); summary(IN.skill.model6)
# AIC: 1965.9
summary(IN.skill.model6)$aic+length(coef(IN.skill.model6))*(log(IN.N)-2)
# BIC: 2180.092

IN.skill.model5 = glm(Partial.Credit.Crt~Student+Skills5+LnOpp5:Skills5-1, family=binomial(), data=IN.data); summary(IN.skill.model5)
# AIC: 1965.0
summary(IN.skill.model5)$aic+length(coef(IN.skill.model5))*(log(IN.N)-2)
# BIC: 2167.231

IN.skill.model4 = glm(Partial.Credit.Crt~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=IN.data); summary(IN.skill.model4)
# AIC: 1963.8
summary(IN.skill.model4)$aic+length(coef(IN.skill.model4))*(log(IN.N)-2)
# BIC: 2154.192

IN.skill.model3 = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=IN.data); summary(IN.skill.model3)
# AIC: 1960.7 [Best AIC for IN]
summary(IN.skill.model3)$aic+length(coef(IN.skill.model3))*(log(IN.N)-2)
# BIC: 2139.189 [Best BIC for IN]

IN.skill.model2 = glm(Partial.Credit.Crt~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=IN.data); summary(IN.skill.model2)
# AIC: 1982.4
summary(IN.skill.model2)$aic+length(coef(IN.skill.model2))*(log(IN.N)-2)
# BIC: 2148.976

## Using Partial.Credit.Crt as dependent:
## EX condition:
## Skill3 is best on BIC (2498) with 4 next (2510).
## On AIC, skill6 (2330) is best, with skill5 (2332.7) and skill3 (2332.7) next.
## IN condition:
## Skill3 is best on BIC (2139) with 2 next (2149).
## On AIC, skill3 (1961) is best, with skill4 (1964) and skill5 (1965) next.

### Repeat EX vs. IN comparison for Assist.Prob as dependent
detach(IN.data); attach(EX.data)

## Which skill model is best for EX? ##
EX.skill.model6 = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=EX.data); summary(EX.skill.model6)
# AIC: 2324.1 [Better than with Partial.Credit.Crt: 2330.2] [Still best AIC]
summary(EX.skill.model6)$aic+length(coef(EX.skill.model6))*(log(EX.N)-2)
# BIC: 2524.115 < 2530.147

EX.skill.model5 = glm(Assist.Prob~Student+Skills5+LnOpp5:Skills5-1, family=binomial(), data=EX.data); summary(EX.skill.model5)
# AIC: 2324.4 < 2332.6
summary(EX.skill.model5)$aic+length(coef(EX.skill.model5))*(log(EX.N)-2)
# BIC: 2512.923 < 2521.155

EX.skill.model4 = glm(Assist.Prob~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=EX.data); summary(EX.skill.model4)
# AIC: 2326.1 < 2333.1
summary(EX.skill.model4)$aic+length(coef(EX.skill.model4))*(log(EX.N)-2)
# BIC: 2503.242 < 2510.258

EX.skill.model3 = glm(Assist.Prob~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=EX.data); summary(EX.skill.model3)
# AIC: 2325.2 < 2332.7
summary(EX.skill.model3)$aic+length(coef(EX.skill.model3))*(log(EX.N)-2)
# BIC: 2490.838 < 2498.427 [Skill3 is best on BIC for EX]

EX.skill.model2 = glm(Assist.Prob~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=EX.data); summary(EX.skill.model2)
# AIC: 2359.2 < 2365.3
summary(EX.skill.model2)$aic+length(coef(EX.skill.model2))*(log(EX.N)-2)
# BIC: 2513.461 < 2519.521

## Which skill model is best for IN? ##
detach(EX.data); attach(IN.data)

IN.skill.model6 = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=IN.data); summary(IN.skill.model6)
# AIC: 1962.4 < 1965.9
summary(IN.skill.model6)$aic+length(coef(IN.skill.model6))*(log(IN.N)-2)
# BIC: 2176.586 < 2180.092

IN.skill.model5 = glm(Assist.Prob~Student+Skills5+LnOpp5:Skills5-1, family=binomial(), data=IN.data); summary(IN.skill.model5)
# AIC: 1961.3 < 1965.0
summary(IN.skill.model5)$aic+length(coef(IN.skill.model5))*(log(IN.N)-2)
# BIC: 2163.607 < 2167.231

IN.skill.model4 = glm(Assist.Prob~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=IN.data); summary(IN.skill.model4)
# AIC: 1959.7 < 1963.8
summary(IN.skill.model4)$aic+length(coef(IN.skill.model4))*(log(IN.N)-2)
# BIC: 2150.101 < 2154.192

IN.skill.model3 = glm(Assist.Prob~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=IN.data); summary(IN.skill.model3)
# AIC: 1956.6 < 1960.7 [Best AIC for IN]
summary(IN.skill.model3)$aic+length(coef(IN.skill.model3))*(log(IN.N)-2)
# BIC: 2135.050 < 2139.189 [Best BIC for IN]

IN.skill.model2 = glm(Assist.Prob~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=IN.data); summary(IN.skill.model2)
# AIC: 1978.3 < 1982.4
summary(IN.skill.model2)$aic+length(coef(IN.skill.model2))*(log(IN.N)-2)
# BIC: 2144.851 < 2148.976

## Using Assist.Prob as dependent:
## EX condition:
## Skill3 is still best on BIC (2491) with 4 next (2503).
## On AIC, skill6 (2324.1) is still best, with skill5 (2324.4) and skill3 (2325) next.
## IN condition:
## Skill3 is best on BIC (2135) with 2 next (2145).
## On AIC, skill3 (1957) is best, with skill4 (1960) and skill5 (1961) next.

detach(IN.data); attach(Excel)

#### Does adding condition help (lower AIC or BIC)?  In which model?  Try Skill3
cond.model.skill3 = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3)
# AIC: 4294.9 [Best overall model so far, but for Skill5? But beat by using Assist.Prob as dependent.]
summary(cond.model.skill3)$aic+length(coef(cond.model.skill3))*(log(N)-2)
# BIC: 4667.19 [Was best overall model so far, but beat by using Assist.Prob as dependent.]
#                                     Estimate Std. Error z value Pr(>|z|)    
# Skills3one ref:LnOpp3:ConditionIN    0.21159    0.10387   2.037 0.041638 *  
# Skills3two refs:LnOpp3:ConditionIN  -0.06640    0.10430  -0.637 0.524331    
# Skills3zero refs:LnOpp3:ConditionIN  0.02106    0.14479   0.145 0.884344    

assist.cond.model.skill3 = glm(Assist.Prob~Student+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(assist.cond.model.skill3)
# AIC: 4286.0 vs. 4294.9 [Best overall model so far, but Skill5?]
summary(assist.cond.model.skill3)$aic+length(coef(assist.cond.model.skill3))*(log(N)-2)
# BIC: 4658.327 vs. 4667.19 [Was best overall model so far, but now it is absref.model.first.comp.cond at 4648.]
#                                     Estimate Std. Error z value Pr(>|z|)    
# Skills3one ref:LnOpp3:ConditionIN    0.2140302  0.1030308   2.077 0.037770 *  
# Skills3two refs:LnOpp3:ConditionIN  -0.0623836  0.1035630  -0.602 0.546926    
# Skills3zero refs:LnOpp3:ConditionIN  0.0008309  0.1438416   0.006 0.995391    

assist.cond.model.skill3.session = glm(Assist.Prob~Student+Session+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(assist.cond.model.skill3.session)
# AIC: 4287.5 vs. 4286.0 [Slighlty worse]
summary(assist.cond.model.skill3.session)$aic+length(coef(assist.cond.model.skill3.session))*(log(N)-2)
# BIC: 4666.354 vs. 4658.327 [Significantly worse!]
# Session                             -0.076886   0.112293  -0.685 0.493535    

Session.Factor = factor(Session)

assist.cond.model.skill3.sessionf = glm(Assist.Prob~Student+Session.Factor+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(assist.cond.model.skill3.sessionf)
# AIC: 4289.7 vs. 4287.5 vs. 4286.0 [Still slightly worse]
# Session.Factor2                     -0.016515   0.179228  -0.092 0.926582    
# Session.Factor3                     -0.116739   0.240110  -0.486 0.626833    

# Skill is robust to forgetting over a one week period.

# Is there a difference between conditions?
assist.cond2.model.skill3.sessionf = glm(Assist.Prob~Student+Session.Factor:Condition+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.skill3.sessionf)
# AIC: 4287.9 [Still slightly worse]
# Session.Factor1:ConditionEX         -0.210203   0.307702  -0.683 0.494520    
# Session.Factor2:ConditionEX         -0.091216   0.162799  -0.560 0.575276    
# Session.Factor3:ConditionEX                NA         NA      NA       NA    
# Session.Factor1:ConditionIN          0.614180   0.381205   1.611 0.107146    
# Session.Factor2:ConditionIN          0.377706   0.193456   1.952 0.050889 .  
# Session.Factor3:ConditionIN                NA         NA      NA       NA    

# No difference between conditions, but looks like there may be an interaction.

# Is there an interaction with Condition?
assist.cond2.model.skill3.session = glm(Assist.Prob~Student+Session+Session:Condition+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.skill3.session)
Session                              0.09891    0.14463   0.684 0.494048    
Session:ConditionIN                 -0.43983    0.22817  -1.928 0.053894 .  
AIC: 4284.0 vs. 4287.5 vs. 4286.0 
# Yes strangely, seems to be a bit more forgetting with the IN condition!
# I wonder whether this has to do with Conddition intercept differences (which, in the model above, get captured in the Student parameters.)
# Should really just distinguish session 3 (after a week) from 1 and 2 (which only have 1 day difference)

# Is there a difference between skill models?  i.e. does skill6 do worse?
assist.cond2.model.skill6.session = glm(Assist.Prob~Student+Session+Session:Condition+Skills6+LnOpp6:Skills6 +LnOpp6:Skills6:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.skill6.session)
AIC: 4286.3 
Session                                  0.08182    0.15065   0.543 0.587060
Session:ConditionIN                     -0.45152    0.23838  -1.894 0.058209 .

# Or better yet, item level, performance decline?
Item = factor(ProbNum)
assist.cond2.model.Item.session = glm(Assist.Prob~Student+Item+Session+Session:Condition+LnOpp6:Item +LnOpp6:Item:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.Item.session)
# AIC: 4351.7 [Worse AIC!]
summary(assist.cond2.model.Item.session)$aic+length(coef(assist.cond2.model.Item.session))*(log(N)-2)
# BIC: 5266.084 [Much worse BIC!]
# Session                    0.1015199  0.1654308   0.614 0.539434    
# Session:ConditionIN       -0.5199869  0.2490261  -2.088 0.036790 *  

assist.cond2.model.Item.session.noS = glm(Assist.Prob~Condition+Item+Session+Session:Condition+LnOpp6:Item +LnOpp6:Item:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.Item.session.noS)
# AIC: 5100.5 vs. 4351.7 [Worse AIC!]
# ConditionEX                1.85632    0.36084   5.144 2.68e-07 ***
# ConditionIN                3.07679    0.37579   8.187 2.67e-16 ***
# Session                   -0.63019    0.09919  -6.353 2.11e-10 ***
# ConditionIN:Session       -0.72159    0.16507  -4.371 1.24e-05 ***

# Now there seems to be even more forgetting across session.
# And a greater interaction.
# With the item model, the intercept does capture some of the learning effect.

# Assume no difference except as accounted for by MATH
assist.cond2.model.Item.session.noS.math = glm(Assist.Prob~MATH+Item+Session+Session:Condition+LnOpp6:Item +LnOpp6:Item:Condition-1, family=binomial(), data=Excel); summary(assist.cond2.model.Item.session.noS.math)
# AIC: 5021.7
# Session                   -0.432288   0.101777  -4.247 2.16e-05 ***
# Session:ConditionIN       -0.235106   0.125290  -1.876 0.060588 .  
# Now item differences are still capturing some of the learning.
# Now still seeing forgetting overall -- less of an interaction though.

## How about without the Student parameters?
cond.model.skill3.noS = glm(Partial.Credit.Crt~Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.noS)
# AIC: 5219.3
summary(cond.model.skill3.noS)$aic+length(coef(cond.model.skill3.noS))*(log(N)-2)
# BIC: 5278.108 
# Skills3one ref:LnOpp3:ConditionIN    0.41352    0.04567   9.054  < 2e-16 ***
# Skills3two refs:LnOpp3:ConditionIN   0.17486    0.04523   3.866 0.000111 ***
# Skills3zero refs:LnOpp3:ConditionIN  0.22956    0.10129   2.266 0.023427 *  

## Student is sucking up an initial difference between conditions, which is perhaps
## due to using Partial.Credit.Crt. 
# See whether the initial Student parameter estimates are indeed in favor of IN.
StudIDs=unique(StudID)
StudID.pos=vector(mode = "numeric", length = length(StudIDs))
for (i in 1:length(StudIDs)) {StudID.pos[StudIDs[i]]=i}
IN.StudIDs=unique(StudID[Condition=="IN"])
IN.StudID.pos=vector(mode = "numeric", length = length(IN.StudIDs))
for (i in 1:length(IN.StudIDs)) {IN.StudID.pos[i]=StudID.pos[IN.StudIDs[i]]}
EX.StudIDs=unique(StudID[Condition=="EX"])
EX.StudID.pos=vector(mode = "numeric", length = length(EX.StudIDs))
for (i in 1:length(EX.StudIDs)) {EX.StudID.pos[i]=StudID.pos[EX.StudIDs[i]]}

mean(coef(cond.model.skill3)[IN.StudID.pos])
# 0.6751986
mean(coef(cond.model.skill3)[EX.StudID.pos])
# -0.1609770
# Yes, indeed the mean IN Student intercept is 0.675 whereas the mean EX Student 
# intercept is only -0.161.

# Check that the condition diff at start (with Partial.Credit.Crt) is significant.
cond.model.skill3.noS.cond = glm(Partial.Credit.Crt~Skills3+Condition+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.noS.cond)
# AIC: 5203.9
# ConditionIN                          0.65171    0.20750   3.141 0.001685 ** 
# Indeed, a signficant difference at start.

cond.model.skill3.cond = glm(Partial.Credit.Crt~Skills3+Condition+Student+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.cond)
# AIC: 4294.9
# ConditionIN                         -0.18781    0.40342  -0.466 0.641537    
# But no sig Condition intercept diff when Student parameters are included. (We
# need a hierarchical model.)

# See whether there is still a condition difference at 1st opp for Success.
success.cond.model.skill3 = glm(Success~Student+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(success.cond.model.skill3)
mean(coef(success.cond.model.skill3)[IN.StudID.pos])
# -0.7050036
mean(coef(success.cond.model.skill3)[EX.StudID.pos])
# -1.71329
success.cond.model.skill3.noS.cond = glm(Success~Skills3+Condition+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(success.cond.model.skill3.noS.cond)
# ConditionIN                          1.02235    0.21125   4.839 1.30e-06 ***

## SURPRISE: Yes, the difference is still significant. Two possible reasons:
# 1) Because of possible higher transfer with Skill3 for the IN group,
# how about for Skill2?
success.cond.model.skill2 = glm(Success~Student+Skills2+LnOpp2:Skills2 +LnOpp2:Skills2:Condition-1, family=binomial(), data=Excel); summary(success.cond.model.skill2)
mean(coef(success.cond.model.skill2)[IN.StudID.pos])
# -1.394755
mean(coef(success.cond.model.skill2)[EX.StudID.pos])
# -2.438917
success.cond.model.skill2.noS.cond = glm(Success~Skills2+Condition+LnOpp2:Skills2 +LnOpp2:Skills2:Condition-1, family=binomial(), data=Excel); summary(success.cond.model.skill2.noS.cond)
# ConditionIN                         1.06286    0.23623   4.499 6.82e-06 ***

# 2) Because of incoming differences, perhaps predicted by the Math covariate?
# Go back to Partial.Credit.Crt (since success does not change things)

cond.model.skill3.math = glm(Partial.Credit.Crt~MATH+Student+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.math)
# AIC: 4294.9 [SAme as without MATH, which was 4294.9]
summary(cond.model.skill3.math)$aic+length(coef(cond.model.skill3.math))*(log(N)-2)
# BIC: 4673.721 [Worse than without MATH, which was 4667.19]
#                                     Estimate Std. Error z value Pr(>|z|)    
# Skills3one ref:LnOpp3:ConditionIN    0.21159    0.10387   2.037 0.041638 *
# Skills3two refs:LnOpp3:ConditionIN  -0.06640    0.10430  -0.637 0.524331
# Skills3zero refs:LnOpp3:ConditionIN  0.02106    0.14479   0.145 0.884344

t.test(MATH~Condition, data=subset(Excel, ProbNum==1 & Opp2==1, select=c(Condition, MATH)))
# t = -1.4086, df = 46.103, p-value = 0.1657
# mean in group EX mean in group IN 
#             3.0              3.6 
# So, the MATH aptitude score is not significantly different, though there is a trend.

# 3) It is also perhaps because of the quick improvement on the second opportunity 
# for the IN group that may not be captured because the learning curve function
# is not sharp enough.  Is there a significant difference just looking at Opp=1?
# (With or without MATH.)

cond.model.skill3.noS.1stOpp = glm(Partial.Credit.Crt~MATH+Skills3+Condition-1, family=binomial(), data=subset(Excel, Opp3==1)); summary(cond.model.skill3.noS.1stOpp)
#                  Estimate Std. Error z value Pr(>|z|)   
# MATH              0.42224    0.13654   3.092  0.00199 **
# Skills3one ref   -1.28816    0.54709  -2.355  0.01854 * 
# Skills3two refs  -1.79801    0.57143  -3.147  0.00165 **
# Skills3zero refs  0.72638    0.57523   1.263  0.20667   
# ConditionIN      -0.07775    0.39178  -0.198  0.84268   
# AIC: 172.94

cond.model.skill3.noS.1stOpp.noMath = glm(Partial.Credit.Crt~Skills3+Condition-1, family=binomial(), data=subset(Excel, Opp3==1)); summary(cond.model.skill3.noS.1stOpp.noMath)
# ConditionIN        0.1668     0.3693   0.452    0.652    
# AIC: 185.96
# Still no condition effect.  (Better overall fit with math.)

# How about Opp2?  Note MATH is a predictor for Opp1, but will it continue to be? 
cond.model.skill3.noS.2ndOpp = glm(Partial.Credit.Crt~MATH+Skills3+Condition-1, family=binomial(), data=subset(Excel, Opp3==2)); summary(cond.model.skill3.noS.2ndOpp)

                 Estimate Std. Error z value Pr(>|z|)   
MATH               0.3923     0.1393   2.817  0.00485 **
Skills3one ref    -1.0246     0.5448  -1.881  0.06001 . 
Skills3two refs   -1.7670     0.5831  -3.031  0.00244 **
Skills3zero refs   0.9480     0.6072   1.561  0.11844   
ConditionIN        0.1472     0.3995   0.368  0.71263   
AIC: 173.11

cond.model.skill3.noS.2ndOpp.Inter = glm(Partial.Credit.Crt~MATH+Skills3+Condition+Skills3:Condition-1, family=binomial(), data=subset(Excel, Opp3==2)); summary(cond.model.skill3.noS.2ndOpp.Inter)
                             Estimate Std. Error z value Pr(>|z|)   
MATH                           0.3942     0.1398   2.820  0.00480 **
Skills3one ref                -0.7966     0.5888  -1.353  0.17610   
Skills3two refs               -1.7061     0.6284  -2.715  0.00662 **
Skills3zero refs               0.3955     0.6364   0.621  0.53428   
ConditionIN                   -0.3137     0.6098  -0.514  0.60695   
Skills3two refs:ConditionIN    0.3303     0.8563   0.386  0.69970   
Skills3zero refs:ConditionIN   2.5396     1.6453   1.544  0.12270   
AIC: 172.5

cond.model.skill3.noS.2ndOpp.Inter.noMath = glm(Partial.Credit.Crt~Skills3+Condition+Skills3:Condition-1, family=binomial(), data=subset(Excel, Opp3==2)); summary(cond.model.skill3.noS.2ndOpp.Inter.noMath)
ConditionIN                  -0.06377    0.57920  -0.110  0.91233   
Skills3two refs:ConditionIN   0.33360    0.82485   0.404  0.68589   
Skills3zero refs:ConditionIN  2.48925    1.62772   1.529  0.12619   

# Perhaps skill6 will show as there are more data points (more Opp=6)
cond.model.skill6.noS.2ndOpp.Inter = glm(Partial.Credit.Crt~MATH+Skills6+Condition+Skills6:Condition-1, family=binomial(), data=subset(Excel, Opp6==2)); summary(cond.model.skill6.noS.2ndOpp.Inter)
ConditionIN                        0.1470     0.6205   0.237 0.812778    
Skills6matrix:ConditionIN          0.4937     0.8788   0.562 0.574237    
Skills6one ref col:ConditionIN     0.3635     0.9088   0.400 0.689150    
Skills6one ref row:ConditionIN     0.5628     0.8900   0.632 0.527160    
Skills6zero refs col:ConditionIN   0.8200     1.2115   0.677 0.498498    
Skills6zero refs row:ConditionIN   0.6418     1.1672   0.550 0.582445    
# Too many condition terms soaking up variance!

cond.model.skill6.noS.2ndOpp.noMath = glm(Partial.Credit.Crt~Skills6+Condition-1, family=binomial(), data=subset(Excel, Opp6==2)); summary(cond.model.skill6.noS.2ndOpp.noMath)
ConditionIN            0.8082     0.2754   2.935 0.003334 ** 
AIC: 390.88

cond.model.skill6.noS.2ndOpp = glm(Partial.Credit.Crt~MATH+Skills6+Condition-1, family=binomial(), data=subset(Excel, Opp6==2)); summary(cond.model.skill6.noS.2ndOpp)
MATH                   0.4805     0.1015   4.736 2.18e-06 ***
ConditionIN            0.5635     0.2914   1.934 0.053137 .  
AIC: 353.37
# Survives at ~.05 with addition of MATH


### Side question: Does LnOpp (power law) fit better than Opp (exponential)?
cond.model.skill3.math.noLn = glm(Partial.Credit.Crt~MATH+Student+Skills3+Opp3:Skills3 +Opp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.math.noLn)
# AIC: 4351.6 [Worse than LnOpp of 4294.9!]
summary(cond.model.skill3.math.noLn)$aic+length(coef(cond.model.skill3.math.noLn))*(log(N)-2)
# BIC: 4730.455 [Worse than LnOpp of 4673.721]
# Yes, LnOpp is significantly better!

### Are there wide student differences in learning rate?
model.skill3.Sslope = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Student-1, family=binomial(), data=Excel); summary(model.skill3.Sslope)
# AIC: 4332.3
summary(model.skill3.Sslope)$aic+length(coef(model.skill3.Sslope))*(log(N)-2)
# BIC: 4985.448

model.skill3.SSslope = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Student+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(model.skill3.SSslope)
# AIC: 4317.3
summary(model.skill3.SSslope)$aic+length(coef(model.skill3.SSslope))*(log(N)-2)
# BIC: 4983.518

model.skill3.Sslope.Math.noInter = glm(Partial.Credit.Crt~MATH+Skills3+LnOpp3:Student-1, family=binomial(), data=Excel); summary(model.skill3.Sslope.Math.noInter)
# AIC: 4334.6
summary(model.skill3.Sslope.Math.noInter)$aic+length(coef(model.skill3.Sslope.Math.noInter))*(log(N)-2)
# BIC: 4680.806

# Yes, there are significant differences (at least for some models). See the Excel
# file LogData-AFM-model.xls


#### Can we beat the best overall model: 1) by adding math, 2) by adding student slope
cond.model.skill3.math.noStud = glm(Partial.Credit.Crt~MATH+Skills3+LnOpp3:Skills3 +LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.math.noStud)
# AIC: 4973.6 [Worse than best of 4294.9]
summary(cond.model.skill3.math.noStud)$aic+length(coef(cond.model.skill3.math.noStud))*(log(N)-2)
# BIC: 5038.902 [Worse than best of 4667.19]

## 2) Add a student slope.
cond.model.skill3.math.Sslope = glm(Partial.Credit.Crt~MATH+Skills3+LnOpp3:Skills3+LnOpp3:Student+LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(cond.model.skill3.math.Sslope)
# AIC: 4293.6 [Beats previous best of 4294.9]
summary(cond.model.skill3.math.Sslope)$aic+length(coef(cond.model.skill3.math.Sslope))*(log(N)-2)
# BIC: 4672.452 [Worse than best of 4667.19]


### Comparing EX and IN on just oneref problems for Skill3 & Skill6

detach(IN.data); attach(EX.data)

## Which skill model is best for EX? ##
EX.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=subset(EX.data, Skills3=="one ref")); summary(EX.skill.model6.oneref)
# AIC: 987.17 [2324.1 better than with Partial.Credit.Crt: 2330.2] [Still best AIC]
summary(EX.skill.model6.oneref)$aic+length(coef(EX.skill.model6.oneref))*(log(EX.N)-2)
# BIC: 1141.426 2524.115 < 2530.147

EX.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data= subset(EX.data, Skills3=="one ref")); summary(EX.skill.model3.oneref)
# AIC: 989.13 [2325.2 < 2332.7]
summary(EX.skill.model3.oneref)$aic+length(coef(EX.skill.model3.oneref))*(log(EX.N)-2)
# BIC: 1131.963 2490.838 < 2498.427 [Skill3 is best on BIC for EX]

## Which skill model is best for IN? ##
detach(EX.data); attach(IN.data)

IN.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data= subset(IN.data, Skills3=="one ref")); summary(IN.skill.model6.oneref)
# AIC: 771.68  1962.4 < 1965.9
summary(IN.skill.model6.oneref)$aic+length(coef(IN.skill.model6.oneref))*(log(IN.N)-2)
# BIC: 938.2499  2176.586 < 2180.092

IN.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data=subset(IN.data, Skills3=="one ref")); summary(IN.skill.model3.oneref)
# AIC: 770.08 1956.6 < 1960.7 [Best AIC for IN]
summary(IN.skill.model3.oneref)$aic+length(coef(IN.skill.model3.oneref))*(log(IN.N)-2)
# BIC: 924.7566 2135.050 < 2139.189 [Best BIC for IN]

# Actually the contrast is weaker if anything.  What about restricting to first 10 of
# Opp6

detach(IN.data); attach(EX.data)

## Which skill model is best for EX? ##
EX.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=subset(EX.data, Skills3=="one ref" & Opp6<11)); summary(EX.skill.model6.oneref)
# AIC: 570.55 [no longer best AIC!]

EX.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data= subset(EX.data, Skills3=="one ref" & Opp6<11)); summary(EX.skill.model3.oneref)
# AIC: 568.23 

## Which skill model is best for IN? ##
detach(EX.data); attach(IN.data)

IN.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data= subset(IN.data, Skills3=="one ref" & Opp6<11)); summary(IN.skill.model6.oneref)
# AIC: 486  1962.4 < 1965.9

IN.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data=subset(IN.data, Skills3=="one ref" & Opp6<11)); summary(IN.skill.model3.oneref)
# AIC: 482.2 1956.6 < 1960.7 [Best AIC for IN]

detach(IN.data); attach(Excel)

## How about elapsed time
## Which skill model is best for IN? ##
detach(EX.data); attach(IN.data)

IN.skill.model6.oneref = glm(ElapsedTime+Skills6+LnOpp6:Skills6-1, family=binomial(), data=IN.data); summary(IN.skill.model6.oneref)
# AIC: 771.68  1962.4 < 1965.9
summary(IN.skill.model6.oneref)$aic+length(coef(IN.skill.model6.oneref))*(log(IN.N)-2)
# BIC: 938.2499  2176.586 < 2180.092

IN.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data=subset(IN.data, Skills3=="one ref")); summary(IN.skill.model3.oneref)
# AIC: 770.08 1956.6 < 1960.7 [Best AIC for IN]
summary(IN.skill.model3.oneref)$aic+length(coef(IN.skill.model3.oneref))*(log(IN.N)-2)
# BIC: 924.7566 2135.050 < 2139.189 [Best BIC for IN]

# Actually the contrast is weaker if anything.  What about restricting to first 10 of
# Opp6

detach(IN.data); attach(EX.data)

## Which skill model is best for EX? ##
EX.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data=subset(EX.data, Skills3=="one ref" & Opp6<11)); summary(EX.skill.model6.oneref)
# AIC: 570.55 [no longer best AIC!]

EX.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data= subset(EX.data, Skills3=="one ref" & Opp6<11)); summary(EX.skill.model3.oneref)
# AIC: 568.23 

## Which skill model is best for IN? ##
detach(EX.data); attach(IN.data)

IN.skill.model6.oneref = glm(Assist.Prob~Student+Skills6+LnOpp6:Skills6-1, family=binomial(), data= subset(IN.data, Skills3=="one ref" & Opp6<11)); summary(IN.skill.model6.oneref)
# AIC: 486  1962.4 < 1965.9

IN.skill.model3.oneref = glm(Assist.Prob~Student+LnOpp3-1, family=binomial(), data=subset(IN.data, Skills3=="one ref" & Opp6<11)); summary(IN.skill.model3.oneref)
# AIC: 482.2 1956.6 < 1960.7 [Best AIC for IN]

detach(IN.data); attach(Excel)


### Time based models

skill.model3.time = glm(Assist.Prob~Student+Skills3+Time3:Skills3-1, family=binomial(), data=Excel); summary(skill.model3.time)
length(coef(skill.model3.time));
(summary(skill.model3.time)$aic-2*length(coef(skill.model3.time)))/-2;
summary(skill.model3.time)$aic+length(coef(skill.model3.time))*(log(N)-2)
# Parameters = 54
# Liklihood = -2128.861 vs. -2102.903
# AIC = 4365.7 vs. 4313.8
# BIC = 4718.412 vs. 4666.496

skill.model3.time.cond = glm(Assist.Prob~Student+Skills3+Time3:Skills3+Time3:Skills3:Condition-1, family=binomial(), data=Excel); summary(skill.model3.time.cond)
length(coef(skill.model3.time.cond));
summary(skill.model3.time.cond)$aic+length(coef(skill.model3.time.cond))*(log(N)-2)
# Parameters = 57
# AIC = 4330.4 vs. 4313.8
# BIC = 4702.726 vs. 4658 for assist.cond.model.skill3

skill.model3.time.cond.noS.math = glm(Assist.Prob~MATH+Skills3+Time3:Skills3+Time3:Skills3:Condition-1, family=binomial(), data=Excel); summary(skill.model3.time.cond.noS.math)
length(coef(skill.model3.time.cond.noS.math));
summary(skill.model3.time.cond.noS.math)$aic+length(coef(skill.model3.time.cond.noS.math))*(log(N)-2)
# Parameters = 10
# AIC = 5197.4 vs. 4313.8
# BIC = 5207.361 vs. 4666.496
                                    Estimate Std. Error z value Pr(>|z|)    
MATH                                0.543858   0.029163  18.649  < 2e-16 ***
Skills3one ref                     -1.054377   0.164937  -6.393 1.63e-10 ***
Skills3two refs                    -1.810007   0.178250 -10.154  < 2e-16 ***
Skills3zero refs                    0.966756   0.234637   4.120 3.79e-05 ***
Skills3one ref:Time3               -0.007665   0.005678  -1.350   0.1771    
Skills3two refs:Time3               0.033125   0.004855   6.822 8.96e-12 ***
Skills3zero refs:Time3             -0.001105   0.017569  -0.063   0.9498    
Skills3one ref:Time3:ConditionIN    0.045713   0.005975   7.651 1.99e-14 ***
Skills3two refs:Time3:ConditionIN   0.007556   0.004115   1.836   0.0663 .  
Skills3zero refs:Time3:ConditionIN  0.005687   0.016881   0.337   0.7362    

skill.model6.time.cond = glm(Assist.Prob~Student+Skills6+Time6:Skills6+Time6:Skills6:Condition-1, family=binomial(), data=Excel); summary(skill.model6.time.cond)
length(coef(skill.model6.time.cond));
summary(skill.model6.time.cond)$aic+length(coef(skill.model6.time.cond))*(log(N)-2)
# Parameters = 66
# AIC = 4355.6 vs. 4313.8
# BIC = 4786.666 vs. 4666.496

skill.model6.time.cond.noS.math = glm(Assist.Prob~MATH+Skills6+Time6:Skills6+Time6:Skills6:Condition-1, family=binomial(), data=Excel); summary(skill.model6.time.cond.noS.math)
length(coef(skill.model6.time.cond.noS.math));
summary(skill.model6.time.cond.noS.math)$aic+length(coef(skill.model6.time.cond.noS.math))*(log(N)-2)
# Parameters = 19
# AIC = 5225 vs. 4313.8
# BIC = 5349.13 vs. 4666.496
MATH                                    0.5400727  0.0290644  18.582  < 2e-16 ***
Skills6double abs                      -1.6097257  0.2070877  -7.773 7.66e-15 ***
Skills6matrix                          -1.7038713  0.2108469  -8.081 6.42e-16 ***
Skills6one ref col                     -0.9270221  0.1920646  -4.827 1.39e-06 ***
Skills6one ref row                     -1.1117439  0.2010914  -5.529 3.23e-08 ***
Skills6zero refs col                    0.8967484  0.2870101   3.124  0.00178 ** 
Skills6zero refs row                    1.1956579  0.3030989   3.945 7.99e-05 ***
Skills6double abs:Time6                 0.0624038  0.0133085   4.689 2.75e-06 ***
Skills6matrix:Time6                     0.0530241  0.0126416   4.194 2.74e-05 ***
Skills6one ref col:Time6               -0.0289027  0.0161859  -1.786  0.07415 .  
Skills6one ref row:Time6               -0.0058830  0.0142694  -0.412  0.68013    
Skills6zero refs col:Time6             -0.0014683  0.0391220  -0.038  0.97006    
Skills6zero refs row:Time6             -0.0194604  0.0537160  -0.362  0.71714    
Skills6double abs:Time6:ConditionIN     0.0154839  0.0123579   1.253  0.21022    
Skills6matrix:Time6:ConditionIN         0.0158950  0.0110958   1.433  0.15199    
Skills6one ref col:Time6:ConditionIN    0.0910741  0.0179167   5.083 3.71e-07 ***
Skills6one ref row:Time6:ConditionIN    0.0896796  0.0161391   5.557 2.75e-08 ***
Skills6zero refs col:Time6:ConditionIN  0.0037786  0.0390494   0.097  0.92291    
Skills6zero refs row:Time6:ConditionIN  0.0009743  0.0572541   0.017  0.98642    



### How about a two skill model where the double ref problems are treated as two
### instances of the absolute ref skill?

absref.model = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRef+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model)
length(coef(absref.model));
summary(absref.model)$aic+length(coef(absref.model))*(log(N)-2)
# Parameters = 52
# AIC = 4434.7 vs. 4313.8 [Not best]
# BIC = 4774.348 vs. 4666.496 [Does not beat assist.cond.model.skill3 at 4658.327]
AbsRefSkill -0.34749    0.08582  -4.049 5.14e-05 ***
LnOppAbsRef  0.28938    0.04066   7.117 1.10e-12 ***
LnOppRelRef  1.03335    0.08667  11.922  < 2e-16 ***

absref.model.cond = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRef+LnOppRelRef+LnOppAbsRef:Condition+LnOppRelRef:Condition-1, family=binomial(), data=Excel); summary(absref.model.cond)
length(coef(absref.model.cond));
summary(absref.model.cond)$aic+length(coef(absref.model.cond))*(log(N)-2)
# Parameters = 54
# AIC = 4436.7 vs. 4286.0
# BIC = 4789.38 vs. 4658.327

absref.model.cond.noS.math = glm(Assist.Prob~MATH+AbsRefSkill+LnOppAbsRef+LnOppRelRef+LnOppAbsRef:Condition+LnOppRelRef:Condition-1, family=binomial(), data=Excel); summary(absref.model.cond.noS.math)
length(coef(absref.model.cond.noS.math));
summary(absref.model.cond.noS.math)$aic+length(coef(absref.model.cond.noS.math))
# Parameters = 7
# AIC = 5282.2 
# BIC = 5289.188
                         Estimate Std. Error z value Pr(>|z|)    
MATH                     0.30634    0.02662  11.506   <2e-16 ***
AbsRefSkill             -0.62312    0.07073  -8.809   <2e-16 ***
LnOppAbsRef              0.39585    0.03640  10.875   <2e-16 ***
LnOppRelRef              0.66749    0.07348   9.084   <2e-16 ***
LnOppAbsRef:ConditionEX -0.19013    0.02306  -8.247   <2e-16 ***
LnOppRelRef:ConditionIN  0.14388    0.10254   1.403    0.161    

## What if we count the double ref problems as only exercising the absolute ref
## skill only once (that is the first time)?

absref.model.first = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first)
length(coef(absref.model.first));
summary(absref.model.first)$aic+length(coef(absref.model.first))*(log(N)-2)
# Parameters = 52
# AIC = 4432.7 vs. 4434.7 [Slightly better]
# BIC = 4772.325 vs. 4774.348 vs. 4666.496 [Does not beat assist.cond.model.skill3 at 4658.327]
AbsRefSkill      -0.35079    0.08588  -4.084 4.42e-05 ***
LnOppAbsRefFirst  0.31928    0.04434   7.201 5.99e-13 ***
LnOppRelRef       1.02927    0.08615  11.947  < 2e-16 ***

## And what about a composition effect (with and without learning) of the absolute skills
## on the two ref problems.

absref.model.first.comp = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppRelRef+AbsCompSkill+LnOppAbsComp-1, family=binomial(), data=Excel); summary(absref.model.first.comp)
length(coef(absref.model.first.comp));
summary(absref.model.first.comp)$aic+length(coef(absref.model.first.comp))*(log(N)-2)
# Parameters = 54
# AIC = 4311.3 vs. 4434.7 [Slightly better]
# BIC = 4664.028 vs. 4774.348 vs. 4666.496 [Does not quite beat assist.cond.model.skill3 at 4658.327]
AbsRefSkill      -1.67563    0.31998  -5.237 1.64e-07 ***
LnOppAbsRefFirst  0.26984    0.05825   4.633 3.61e-06 ***
LnOppRelRef       0.50010    0.11700   4.275 1.92e-05 ***
AbsCompSkill      0.51391    0.45039   1.141 0.253854    
LnOppAbsComp      0.43315    0.08432   5.137 2.79e-07 ***


# Perhaps the slope LnOppAbsComp is not needed.
absref.model.first.comp2 = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppRelRef+AbsCompSkill-1, family=binomial(), data=Excel); summary(absref.model.first.comp2)
length(coef(absref.model.first.comp2));
summary(absref.model.first.comp2)$aic+length(coef(absref.model.first.comp2))*(log(N)-2)
# Parameters = 53
# AIC = 4342.9 vs. 4311.3 [worse]
# BIC = 4689.074 vs. 4664.028 [worse]

# Or adding condition might help?
absref.model.first.comp.cond = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppAbsRefFirst:Condition+LnOppRelRef+AbsCompSkill+LnOppAbsComp+LnOppAbsComp:Condition-1, family=binomial(), data=Excel); summary(absref.model.first.comp.cond)
length(coef(absref.model.first.comp.cond));
summary(absref.model.first.comp.cond)$aic+length(coef(absref.model.first.comp.cond))*(log(N)-2)
# Parameters = 56
# AIC = 4282.9 vs. 4434.7 [Better]
# BIC = 4648.659 vs. 4664.028 [Was BEST so far before absref.model.first.comp.cond.mod. Beats assist.cond.model.skill3 at 4658.327]
AbsRefSkill                  -1.68709    0.31997  -5.273 1.34e-07 ***
LnOppAbsRefFirst              0.20418    0.06509   3.137 0.001708 ** 
LnOppRelRef                   0.50066    0.11696   4.281 1.86e-05 ***
AbsCompSkill                  0.54738    0.45034   1.215 0.224183    
LnOppAbsComp                  0.53983    0.08956   6.027 1.67e-09 ***
LnOppAbsRefFirst:ConditionIN  0.17595    0.07203   2.443 0.014581 *  
ConditionIN:LnOppAbsComp     -0.27378    0.07138  -3.835 0.000125 ***

## Maybe don't need AbsCompSkill intercept?!
absref.model.first.comp.cond.noabscompinit = glm(Assist.Prob~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppAbsRefFirst:Condition+LnOppRelRef+LnOppAbsComp+LnOppAbsComp:Condition-1, family=binomial(), data=Excel); summary(absref.model.first.comp.cond.noabscompinit)
length(coef(absref.model.first.comp.cond.noabscompinit));
summary(absref.model.first.comp.cond.noabscompinit)$aic+length(coef(absref.model.first.comp.cond.noabscompinit))*(log(N)-2)
# Parameters = 56
# AIC = 4285.9 vs. 4282.9 [Worse]
# BIC = 4645.076 vs. 4648.659 [Better, but not by more than 6.]
AbsRefSkill                  -1.34829    0.15211  -8.864  < 2e-16 ***
LnOppAbsRefFirst              0.16159    0.05470   2.954 0.003136 ** 
LnOppRelRef                   0.57209    0.10030   5.704 1.17e-08 ***
LnOppAbsComp                  0.61036    0.06866   8.890  < 2e-16 ***
LnOppAbsRefFirst:ConditionIN  0.17389    0.07175   2.424 0.015366 *  
ConditionIN:LnOppAbsComp     -0.27211    0.07139  -3.811 0.000138 ***


### The Expert tutor was more strict about students producing optimally correct answers
### than was the IN tutor.
### What if we grade both based on the IN tutor's less stringent criteria?
absref.model.first.comp.cond.mod = glm(AssistProbMod~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppAbsRefFirst:Condition+LnOppRelRef+AbsCompSkill+LnOppAbsComp+LnOppAbsComp:Condition-1, family=binomial(), data=Excel); summary(absref.model.first.comp.cond.mod)
length(coef(absref.model.first.comp.cond.mod));
summary(absref.model.first.comp.cond.mod)$aic+length(coef(absref.model.first.comp.cond.mod))*(log(N)-2)
# Parameters = 56
# AIC = 4006.7 vs. 4434.7 [Was best so far.]
# BIC = 4372.486 vs. 4664.028 [Was best so far. Beats absref.model.first.comp.cond. Beat by absref.model.first.comp.mod]
AbsRefSkill                  -1.84211    0.35903  -5.131 2.89e-07 ***
LnOppAbsRefFirst              0.35727    0.07180   4.976 6.49e-07 ***
LnOppRelRef                   0.55130    0.14032   3.929 8.54e-05 ***
AbsCompSkill                  0.35315    0.48284   0.731 0.464532    
LnOppAbsComp                  0.38485    0.09292   4.142 3.45e-05 ***
LnOppAbsRefFirst:ConditionIN -0.01755    0.08182  -0.215 0.830128    
ConditionIN:LnOppAbsComp     -0.01295    0.07556  -0.171 0.863959    

# Note condition factors on slope do not differ here (and trend in the wrong direction) 
## Are the student intercepts different between the groups?
##(See above for how to compare student parameter estimates between groups.)

## Deleting cond may improve fit.
absref.model.first.comp.mod = glm(AssistProbMod~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppRelRef+AbsCompSkill+LnOppAbsComp-1, family=binomial(), data=Excel); summary(absref.model.first.comp.mod)
length(coef(absref.model.first.comp.mod));
summary(absref.model.first.comp.mod)$aic+length(coef(absref.model.first.comp.mod))*(log(N)-2)
# Parameters = 54
# AIC = 4003.8 vs. 4006.7 [Was best.  Beat by absref.model.first.comp.mod.bothS]
# BIC = 4356.473 vs. 4372.486 [Was best. Beats absref.model.first.comp.cond.mod. Beaten by ]
AbsRefSkill      -1.84014    0.35894  -5.127 2.95e-07 ***
LnOppAbsRefFirst  0.34932    0.06223   5.613 1.99e-08 ***
LnOppRelRef       0.55160    0.14034   3.930 8.48e-05 ***
AbsCompSkill      0.35306    0.48263   0.732 0.464448    
LnOppAbsComp      0.37830    0.08677   4.360 1.30e-05 ***

# Still seeing a "reverse composition effect" in that AbsCompSkill is positive (0.35306) suggesting
# composed problems somewhat easier than predicted by "doubled" difficulty of AbsRefSkill (2 * 
# -1.84014).  AbsCompSkill is not significant.  The slope (LnOppAbsComp) is significant


## Do we get a condition slope difference when there is no Student parameter.
absref.model.first.comp.cond.mod.noS = glm(AssistProbMod~MATH+AbsRefSkill+LnOppAbsRefFirst+LnOppAbsRefFirst:Condition+LnOppRelRef+AbsCompSkill+LnOppAbsComp+LnOppAbsComp:Condition-1, family=binomial(), data=Excel); summary(absref.model.first.comp.cond.mod.noS)
length(coef(absref.model.first.comp.cond.mod.noS));
summary(absref.model.first.comp.cond.mod.noS)$aic+length(coef(absref.model.first.comp.cond.mod.noS))*(log(N)-2)
# Parameters = 9
# AIC = 4652.4 vs. 4006.7 
# BIC = 4711.211 vs. 4372.486
MATH                          0.39721    0.03011  13.190  < 2e-16 ***
AbsRefSkill                  -1.19467    0.19231  -6.212 5.22e-10 ***
LnOppAbsRefFirst              0.47928    0.05963   8.038 9.15e-16 ***
LnOppRelRef                   0.84242    0.07559  11.145  < 2e-16 ***
AbsCompSkill                 -0.15998    0.35509  -0.451  0.65233    
LnOppAbsComp                  0.35677    0.08619   4.139 3.49e-05 ***
LnOppAbsRefFirst:ConditionEX -0.11885    0.04222  -2.815  0.00488 ** 
LnOppAbsRefFirst:ConditionIN       NA         NA      NA       NA    
ConditionIN:LnOppAbsComp     -0.01225    0.07047  -0.174  0.86199    

# GOOD NEWS: Slope on EX for AbsRef is significantly worse!!


### Have I looked at allowing individual student slopes to vary?
# Start with simple model:
skill3.mod = glm(AssistProbMod~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill3.mod)
length(coef(skill3.mod));
summary(skill3.mod)$aic+length(coef(skill3.mod))*(log(N)-2)
# Parameters = 54
# AIC = 4008.9 
# BIC = 4361.567 vs. 4356.473 [Not significantly worse than best of absref.model.first.comp.mod]

# Substitue student slope for skill slope:
skill3.mod.Sslope = glm(AssistProbMod~Student+Skills3+LnOpp3:Student-1, family=binomial(), data=Excel); summary(skill3.mod.Sslope)
L=length(coef(skill3.mod.Sslope)); L;
summary(skill3.mod.Sslope)$aic+L*(log(N)-2)
# Parameters = 100
# AIC = 4033.8 
# BIC = 4686.886 vs. 4361.567 
# Adding a student slope does not significantly improve BIC (or AIC).

# Include both, & add math
skill3.mod.bothS.math = glm(AssistProbMod~MATH+Student+LnOpp3:Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill3.mod.bothS.math)
L=length(coef(skill3.mod.bothS.math)); L;
summary(skill3.mod.bothS.math)$aic+L*(log(N)-2)
# Parameters = 103
# AIC = 4020 
# BIC = 4692.735 vs. 4686.204 [worse than without math: skill3.mod.bothS]

# Include both
skill3.mod.bothS = glm(AssistProbMod~Student+LnOpp3:Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill3.mod.bothS)
L=length(coef(skill3.mod.bothS)); L;
summary(skill3.mod.bothS)$aic+L*(log(N)-2)
# Parameters = 102
# AIC = 4020 
# BIC = 4686.204 vs. 4686.886 [no difference from skill3.mod.Sslope]
                         Estimate Std. Error z value Pr(>|z|)    
StudentS1                1.038957   0.781087   1.330  0.18347    
StudentS10               0.729480   0.981897   0.743  0.45752    
StudentS11               0.189690   0.832822   0.228  0.81983    
StudentS12              -0.778900   0.795758  -0.979  0.32767    
StudentS13               0.909307   0.950915   0.956  0.33895    
StudentS14               1.736628   0.999834   1.737  0.08240 .  
StudentS15               0.555989   0.836646   0.665  0.50634    
StudentS16              -0.438344   1.070278  -0.410  0.68213    
StudentS17               1.436367   1.036398   1.386  0.16577    
StudentS18              -0.739575   0.786177  -0.941  0.34685    
StudentS19               0.687582   0.819519   0.839  0.40147    
StudentS2               -1.414109   0.917819  -1.541  0.12338    
StudentS20              -0.506268   0.813893  -0.622  0.53392    
StudentS21               0.390485   0.899282   0.434  0.66413    
StudentS22               2.372155   1.290636   1.838  0.06607 .  
StudentS23               4.418486   2.318923   1.905  0.05673 .  
StudentS24               3.328862   1.375816   2.420  0.01554 *  
StudentS25               2.931583   1.301360   2.253  0.02428 *  
StudentS26               0.037141   0.744681   0.050  0.96022    
StudentS27              -0.468428   0.856505  -0.547  0.58444    
StudentS28               4.061028   1.971820   2.060  0.03944 *  
StudentS3               -0.207999   1.014407  -0.205  0.83754    
StudentS31               0.165769   0.988167   0.168  0.86678    
StudentS32              -0.638208   1.138427  -0.561  0.57507    
StudentS33              -1.652279   1.146713  -1.441  0.14962    
StudentS34              -0.625696   1.268653  -0.493  0.62187    
StudentS35              -0.885287   1.131894  -0.782  0.43414    
StudentS36              -0.280349   0.940528  -0.298  0.76564    
StudentS37              -0.784186   0.864445  -0.907  0.36432    
StudentS39               1.269303   0.872594   1.455  0.14577    
StudentS40               0.090098   0.824835   0.109  0.91302    
StudentS41               0.738104   1.078478   0.684  0.49373    
StudentS42              -0.301172   0.867024  -0.347  0.72832    
StudentS43               0.506031   0.720575   0.702  0.48252    
StudentS44              -1.017396   0.892475  -1.140  0.25430    
StudentS45               0.162280   0.856362   0.189  0.84970    
StudentS46              -1.508019   1.034432  -1.458  0.14489    
StudentS47               2.565831   1.382875   1.855  0.06353 .  
StudentS48               2.144362   0.923401   2.322  0.02022 *  
StudentS49              -0.917468   1.004227  -0.914  0.36092    
StudentS5               -0.219672   0.835611  -0.263  0.79264    
StudentS50               0.352316   0.783229   0.450  0.65284    
StudentS51               0.040349   0.958703   0.042  0.96643    
StudentS52               1.578890   0.803132   1.966  0.04931 *  
StudentS53               0.880738   0.680432   1.294  0.19553    
StudentS54              -0.808029   1.025373  -0.788  0.43068    
StudentS6                1.128493   1.004288   1.124  0.26115    
StudentS7                0.563903   0.675176   0.835  0.40361    
StudentS8               -1.079027   0.915159  -1.179  0.23837    
Skills3two refs         -1.244841   0.264504  -4.706 2.52e-06 ***
Skills3zero refs         1.816411   0.360029   5.045 4.53e-07 ***
StudentS1:LnOpp3        -0.107184   0.278961  -0.384  0.70081    
StudentS10:LnOpp3        1.201715   0.447536   2.685  0.00725 ** 
StudentS11:LnOpp3        0.098662   0.344428   0.286  0.77453    
StudentS12:LnOpp3        0.690014   0.308279   2.238  0.02520 *  
StudentS13:LnOpp3        0.173331   0.402403   0.431  0.66666    
StudentS14:LnOpp3        0.315678   0.372271   0.848  0.39645    
StudentS15:LnOpp3        0.370214   0.330758   1.119  0.26302    
StudentS16:LnOpp3        0.024779   0.684952   0.036  0.97114    
StudentS17:LnOpp3        0.862659   0.408156   2.114  0.03455 *  
StudentS18:LnOpp3        0.302345   0.296255   1.021  0.30746    
StudentS19:LnOpp3        0.739308   0.285753   2.587  0.00968 ** 
StudentS2:LnOpp3         0.654078   0.404494   1.617  0.10587    
StudentS20:LnOpp3        0.524124   0.326757   1.604  0.10871    
StudentS21:LnOpp3        0.002137   0.423203   0.005  0.99597    
StudentS22:LnOpp3        0.564903   0.486646   1.161  0.24572    
StudentS23:LnOpp3       -0.048413   0.734228  -0.066  0.94743    
StudentS24:LnOpp3        0.027910   0.404728   0.069  0.94502    
StudentS25:LnOpp3        0.076714   0.428464   0.179  0.85790    
StudentS26:LnOpp3        0.282422   0.266585   1.059  0.28941    
StudentS27:LnOpp3        0.815961   0.369163   2.210  0.02708 *  
StudentS28:LnOpp3        0.012647   0.607697   0.021  0.98340    
StudentS3:LnOpp3         1.660897   0.597553   2.779  0.00544 ** 
StudentS31:LnOpp3        0.122650   0.549956   0.223  0.82352    
StudentS32:LnOpp3        0.975606   0.881570   1.107  0.26844    
StudentS33:LnOpp3        0.812852   0.720492   1.128  0.25924    
StudentS34:LnOpp3       -0.953414   1.236333  -0.771  0.44061    
StudentS35:LnOpp3        0.971510   0.797893   1.218  0.22338    
StudentS36:LnOpp3        0.814280   0.473924   1.718  0.08577 .  
StudentS37:LnOpp3        1.009790   0.376778   2.680  0.00736 ** 
StudentS39:LnOpp3        0.314857   0.313714   1.004  0.31555    
StudentS40:LnOpp3        0.728852   0.326985   2.229  0.02581 *  
StudentS41:LnOpp3        0.136147   0.677374   0.201  0.84070    
StudentS42:LnOpp3        1.057652   0.377710   2.800  0.00511 ** 
StudentS43:LnOpp3        0.422095   0.233396   1.808  0.07053 .  
StudentS44:LnOpp3        0.591866   0.394731   1.499  0.13377    
StudentS45:LnOpp3        0.636725   0.359964   1.769  0.07692 .  
StudentS46:LnOpp3        0.945109   0.551120   1.715  0.08637 .  
StudentS47:LnOpp3        0.565251   0.519660   1.088  0.27671    
StudentS48:LnOpp3       -0.325379   0.323649  -1.005  0.31473    
StudentS49:LnOpp3        0.416042   0.539839   0.771  0.44090    
StudentS5:LnOpp3         0.568737   0.347635   1.636  0.10184    
StudentS50:LnOpp3        0.459165   0.287045   1.600  0.10968    
StudentS51:LnOpp3        0.395828   0.502605   0.788  0.43096    
StudentS52:LnOpp3        0.255474   0.237986   1.073  0.28305    
StudentS53:LnOpp3        0.119658   0.206510   0.579  0.56230    
StudentS54:LnOpp3       -0.124826   0.562696  -0.222  0.82444    
StudentS6:LnOpp3         0.783053   0.438958   1.784  0.07444 .  
StudentS7:LnOpp3        -0.109177   0.216463  -0.504  0.61400    
StudentS8:LnOpp3         0.665166   0.418453   1.590  0.11193    
LnOpp3:Skills3two refs   0.307069   0.098720   3.110  0.00187 ** 
LnOpp3:Skills3zero refs  0.138728   0.156416   0.887  0.37512    

# See Excel spreadsheet "/Ken/Talks:Conf/_Conferences:Workshops/AIED&ITS/ITS 2004/Log File Workshop/
# Excel Learning Curve/LogData-AFM-model.xls" for an analysis.  See tab Student2.

skill3.mod.bothS.cond = glm(AssistProbMod~Student+LnOpp3:Student+Skills3+LnOpp3:Skills3+LnOpp3:Skills3:Condition-1, family=binomial(), data=Excel); summary(skill3.mod.bothS.cond)
L=length(coef(skill3.mod.bothS.cond)); L;
summary(skill3.mod.bothS.cond)$aic+L*(log(N)-2)
# Parameters = 105
# AIC = 4019.6 
# BIC = 4705.348 vs. 4686.204 [worse]
LnOpp3:Skills3two refs               0.31536    0.10322   3.055  0.00225 ** 
LnOpp3:Skills3zero refs              0.08167    0.16438   0.497  0.61932    
LnOpp3:Skills3one ref:ConditionIN   -0.14622    0.15062  -0.971  0.33164    
LnOpp3:Skills3two refs:ConditionIN  -0.16632    0.15063  -1.104  0.26952    
LnOpp3:Skills3zero refs:ConditionIN       NA         NA      NA       NA    
# IN brings the slope down!!

## Excel analysis of student intercepts and slopes indicates a possible greater transfer from 
## one-ref to two-ref.  Try student analysis again with AbsRef model.

absref.model.first.comp.mod.bothS = glm(AssistProbMod~Student+AbsRefSkill+LnOppAbsRefFirst+LnOppRelRef+AbsCompSkill+LnOppAbsComp+Student:LnOppAbsRefFirst-1, family=binomial(), data=Excel); summary(absref.model.first.comp.mod.bothS)
L=length(coef(absref.model.first.comp.mod.bothS)); L;
summary(absref.model.first.comp.mod.bothS)$aic+L*(log(N)-2)
# Parameters = 102
# AIC = 3988.7 vs. 4003.8 [BEST so far.  Beats absref.model.first.comp.mod]
# BIC = 4654.887 vs. 4356.47 vs. 4686.204  [Not as good as best.  Better than skill3.mod.bothS.]
StudentS1                    3.52598    0.93982   3.752 0.000176 ***
StudentS10                   3.07427    0.95458   3.221 0.001280 ** 
StudentS11                   1.60946    0.74811   2.151 0.031446 *  
StudentS12                   1.64902    0.70644   2.334 0.019581 *  
StudentS13                   3.20574    1.05398   3.042 0.002354 ** 
StudentS14                   3.44104    1.02521   3.356 0.000790 ***
StudentS15                   2.70839    0.87680   3.089 0.002009 ** 
StudentS16                   1.08557    0.82069   1.323 0.185919    
StudentS17                   3.93275    1.16409   3.378 0.000729 ***
StudentS18                   0.62968    0.58986   1.068 0.285742    
StudentS19                   2.68028    0.80168   3.343 0.000828 ***
StudentS2                    0.68227    0.62926   1.084 0.278258    
StudentS20                   2.06082    0.78634   2.621 0.008773 ** 
StudentS21                   2.33732    0.90198   2.591 0.009561 ** 
StudentS22                   4.71912    1.50638   3.133 0.001732 ** 
StudentS23                   6.87744    2.72307   2.526 0.011549 *  
StudentS24                   4.72309    1.29545   3.646 0.000266 ***
StudentS25                   5.48224    1.58904   3.450 0.000561 ***
StudentS26                   1.84661    0.70857   2.606 0.009158 ** 
StudentS27                   1.32046    0.68026   1.941 0.052246 .  
StudentS28                   6.48665    2.30370   2.816 0.004866 ** 
StudentS3                    2.46131    0.89893   2.738 0.006180 ** 
StudentS31                   2.67619    1.05454   2.538 0.011156 *  
StudentS32                   1.36116    0.89627   1.519 0.128841    
StudentS33                   0.79236    0.77569   1.021 0.307021    
StudentS34                  -0.17937    0.88282  -0.203 0.838993    
StudentS35                   1.39283    0.91436   1.523 0.127689    
StudentS36                   2.14424    0.87241   2.458 0.013978 *  
StudentS37                   1.72395    0.73684   2.340 0.019302 *  
StudentS39                   3.58976    0.99639   3.603 0.000315 ***
StudentS40                   2.29800    0.79919   2.875 0.004035 ** 
StudentS41                   2.44135    1.05006   2.325 0.020074 *  
StudentS42                   2.26858    0.81544   2.782 0.005402 ** 
StudentS43                   2.28543    0.72071   3.171 0.001519 ** 
StudentS44                   0.80852    0.64715   1.249 0.211535    
StudentS45                   1.55413    0.72006   2.158 0.030902 *  
StudentS46                   0.90343    0.70888   1.274 0.202503    
StudentS47                   3.69720    1.19742   3.088 0.002017 ** 
StudentS48                   4.32201    1.08110   3.998 6.39e-05 ***
StudentS49                   1.93590    0.89891   2.154 0.031270 *  
StudentS5                    2.24990    0.83010   2.710 0.006721 ** 
StudentS50                   2.49779    0.80188   3.115 0.001840 ** 
StudentS51                   1.63712    0.81568   2.007 0.044742 *  
StudentS52                   2.74258    0.76258   3.596 0.000323 ***
StudentS53                   3.36408    0.80866   4.160 3.18e-05 ***
StudentS54                  -0.39770    0.61371  -0.648 0.516973    
StudentS6                    3.34257    1.06142   3.149 0.001637 ** 
StudentS7                    2.61536    0.75201   3.478 0.000505 ***
StudentS8                    0.85295    0.66065   1.291 0.196673    
AbsRefSkill                 -2.15012    0.39793  -5.403 6.54e-08 ***
LnOppAbsRefFirst            -0.19819    0.25454  -0.779 0.436209    
LnOppRelRef                  0.50322    0.15010   3.353 0.000801 ***
AbsCompSkill                 0.59133    0.50530   1.170 0.241905    
LnOppAbsComp                 0.40145    0.08856   4.533 5.82e-06 ***
StudentS10:LnOppAbsRefFirst  1.10848    0.41337   2.682 0.007328 ** 
StudentS11:LnOppAbsRefFirst  0.53371    0.34483   1.548 0.121684    
StudentS12:LnOppAbsRefFirst  0.67017    0.32319   2.074 0.038115 *  
StudentS13:LnOppAbsRefFirst  0.28588    0.41838   0.683 0.494412    
StudentS14:LnOppAbsRefFirst  0.60621    0.39411   1.538 0.124006    
StudentS15:LnOppAbsRefFirst  0.49659    0.36121   1.375 0.169200    
StudentS16:LnOppAbsRefFirst  0.53915    0.51071   1.056 0.291111    
StudentS17:LnOppAbsRefFirst  0.78214    0.42144   1.856 0.063475 .  
StudentS18:LnOppAbsRefFirst  0.70910    0.30176   2.350 0.018778 *  
StudentS19:LnOppAbsRefFirst  0.86875    0.33073   2.627 0.008620 ** 
StudentS2:LnOppAbsRefFirst   0.76650    0.33449   2.292 0.021933 *  
StudentS20:LnOppAbsRefFirst  0.47476    0.34370   1.381 0.167181    
StudentS21:LnOppAbsRefFirst  0.25818    0.41695   0.619 0.535778    
StudentS22:LnOppAbsRefFirst  0.59394    0.50591   1.174 0.240399    
StudentS23:LnOppAbsRefFirst  0.07307    0.74879   0.098 0.922261    
StudentS24:LnOppAbsRefFirst  0.41958    0.40998   1.023 0.306112    
StudentS25:LnOppAbsRefFirst  0.14751    0.48357   0.305 0.760342    
StudentS26:LnOppAbsRefFirst  0.53080    0.31361   1.693 0.090542 .  
StudentS27:LnOppAbsRefFirst  1.02753    0.34219   3.003 0.002675 ** 
StudentS28:LnOppAbsRefFirst  0.13236    0.63316   0.209 0.834409    
StudentS3:LnOppAbsRefFirst   1.26534    0.45130   2.804 0.005051 ** 
StudentS31:LnOppAbsRefFirst  0.09460    0.50991   0.186 0.852819    
StudentS32:LnOppAbsRefFirst  1.09915    0.62103   1.770 0.076746 .  
StudentS33:LnOppAbsRefFirst  0.69522    0.52657   1.320 0.186737    
StudentS34:LnOppAbsRefFirst  1.50384    1.21747   1.235 0.216751    
StudentS35:LnOppAbsRefFirst  0.87610    0.55267   1.585 0.112918    
StudentS36:LnOppAbsRefFirst  0.72662    0.41286   1.760 0.078415 .  
StudentS37:LnOppAbsRefFirst  0.89352    0.34871   2.562 0.010398 *  
StudentS39:LnOppAbsRefFirst  0.40209    0.36909   1.089 0.275975    
StudentS40:LnOppAbsRefFirst  0.77594    0.34597   2.243 0.024910 *  
StudentS41:LnOppAbsRefFirst  0.54837    0.55947   0.980 0.327003    
StudentS42:LnOppAbsRefFirst  0.90209    0.36275   2.487 0.012888 *  
StudentS43:LnOppAbsRefFirst  0.65416    0.30466   2.147 0.031780 *  
StudentS44:LnOppAbsRefFirst  0.82146    0.33839   2.428 0.015200 *  
StudentS45:LnOppAbsRefFirst  1.02205    0.35049   2.916 0.003545 ** 
StudentS46:LnOppAbsRefFirst  0.85743    0.39694   2.160 0.030762 *  
StudentS47:LnOppAbsRefFirst  1.06125    0.50348   2.108 0.035046 *  
StudentS48:LnOppAbsRefFirst -0.07784    0.38412  -0.203 0.839416    
StudentS49:LnOppAbsRefFirst  0.12430    0.46193   0.269 0.787854    
StudentS5:LnOppAbsRefFirst   0.54176    0.35870   1.510 0.130957    
StudentS50:LnOppAbsRefFirst  0.57456    0.33313   1.725 0.084575 .  
StudentS51:LnOppAbsRefFirst  0.77496    0.42112   1.840 0.065733 .  
StudentS52:LnOppAbsRefFirst  0.67699    0.30673   2.207 0.027303 *  
StudentS53:LnOppAbsRefFirst  0.20100    0.30780   0.653 0.513746    
StudentS54:LnOppAbsRefFirst  1.05452    0.39095   2.697 0.006990 ** 
StudentS6:LnOppAbsRefFirst   0.80559    0.43255   1.862 0.062543 .  
StudentS7:LnOppAbsRefFirst   0.11077    0.30566   0.362 0.717059    
StudentS8:LnOppAbsRefFirst   0.84293    0.34760   2.425 0.015308 *  

# See Excel spreadsheet "/Ken/Talks:Conf/_Conferences:Workshops/AIED&ITS/ITS 2004/Log File Workshop/
# Excel Learning Curve/LogData-AFM-model.xls" for an analysis.  See tab Student3.

## Had forgotten to put the 2 absolute skills into the slope
absref.model.first.comp.mod2 = glm(AssistProbMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef+AbsCompSkill+LnOppAbsComp-1, family=binomial(), data=Excel); summary(absref.model.first.comp.mod2)
L=length(coef(absref.model.first.comp.mod2)); L;
summary(absref.model.first.comp.mod2)$aic+L*(log(N)-2)
# Parameters = 54
# AIC = 4002.6 vs. 4003.8 [Slighlty better than absref.model.first.comp.mod]
# BIC = 4355.29 vs. 4356.473 [Slighlty better than absref.model.first.comp.mod]
AbsRefSkill                  -1.84526    0.35850  -5.147 2.64e-07 ***
LnOppRelRef                   0.54967    0.14041   3.915 9.05e-05 ***
AbsCompSkill                  0.02682    0.45007   0.060 0.952480    
LnOppAbsComp                  0.05798    0.12731   0.455 0.648777    
AbsRefSkill:LnOppAbsRefFirst  0.34913    0.06135   5.691 1.26e-08 ***

# Now perhaps Comp is not needed!
absref.model.first.mod2 = glm(AssistProbMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.mod2)
L=length(coef(absref.model.first.mod2)); L;
summary(absref.model.first.mod2)$aic+L*(log(N)-2)
# Parameters = 52
# AIC = 4001.0 vs. 4003.8 [Slighlty better than absref.model.first.comp.mod.  Does not beat absref.model.first.comp.mod.bothS]
# BIC = 4340.675 vs. 4356.473 [Was best. absref.model.first.succmod is now better! Beats absref.model.first.comp.mod]
AbsRefSkill                  -1.77905    0.14343 -12.403  < 2e-16 ***
LnOppRelRef                   0.60023    0.08749   6.861 6.84e-12 ***
AbsRefSkill:LnOppAbsRefFirst  0.37406    0.03371  11.096  < 2e-16 ***

# Should use this one (absref.model.first.mod2) for the student analysis. 
absref.model.first.mod2.bothS = glm(AssistProbMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+Student:AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.mod2.bothS)
L=length(coef(absref.model.first.mod2.bothS)); L;
summary(absref.model.first.mod2.bothS)$aic+L*(log(N)-2)
# Parameters = 100
# AIC = 4006.2 vs. 3988.7 [Worse than absref.model.first.comp.mod.bothS]
# BIC = 4659.379 vs. 4654.887 [Worse than absref.model.first.comp.mod.bothS]

# Accidentally did the following which is better.  Not clear why?
absref.model.first.mod.bothS = glm(AssistProbMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+Student:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.mod.bothS)
L=length(coef(absref.model.first.mod.bothS)); L;
summary(absref.model.first.mod.bothS)$aic+L*(log(N)-2)
# Parameters = 100
# AIC = 3999.5 vs. 3988.7 [Does not beat absref.model.first.comp.mod.bothS]
# BIC = 4652.673 vs. 4654.887 [Slightly better than absref.model.first.comp.mod.bothS]

# Not clear this model makes sense given the contradiction between the treatment of the slope
# with the skills (AbsRefSkill is included) and the students (AbsRefSkill is excluded).

# How about adding back Condition?
absref.model.first.mod2.cond = glm(AssistProbMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+AbsRefSkill:LnOppAbsRefFirst:Condition+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.mod2.cond)
L=length(coef(absref.model.first.mod2.cond)); L;
summary(absref.model.first.mod2.cond)$aic+L*(log(N)-2)
# Parameters = 53
# AIC = 4002.0 vs. 4001.0 [Worse than absref.model.first.mod2]
# BIC = 4348.203 vs. 4340.675 [Worse than absref.model.first.mod2]

## Are there Student by ProbType (Skill6) interactions?
skill6.full = glm(AssistProbMod~Student+Student:Skills6+Student:Skills6:LnOpp6-1, family=binomial(), data=Excel); summary(skill6.full)
L=length(coef(skill6.full)); L;
summary(skill6.full)$aic+L*(log(N)-2)
# Parameters = 588
# AIC = 4433.9 vs. 3988.7 [Not even close to best]
# BIC = 8274.298 [Worst so far!]

skill6 = glm(AssistProbMod~Student+Skills6+Skills6:LnOpp6-1, family=binomial(), data=Excel); summary(skill6)
L=length(coef(skill6)); L;
summary(skill6)$aic+L*(log(N)-2)
# Parameters = 60
# AIC = 4010.2 vs. 4433.9 [Much better than skill6.full.]
# BIC = 4402.055 vs. 8274.298 [Much better than skill6.full.]

#How about just interaction term?
skill6.SS = glm(AssistProbMod~Student:Skills6+Skills6:LnOpp6-1, family=binomial(), data=Excel); summary(skill6.SS)
L=length(coef(skill6.SS)); L;
summary(skill6.SS)$aic+L*(log(N)-2)
# Parameters = 300
# AIC = 4187.8 vs. 4010.2 vs [Worse than skill6.]
# BIC = 6147.14 vs. 4402.055 [Worse than skill6.]

#For the simpler 3 skill model?
skill3.SS = glm(AssistProbMod~Student:Skills3+Skills3:LnOpp3-1, family=binomial(), data=Excel); summary(skill3.SS)
L=length(coef(skill3.SS)); L;
summary(skill3.SS)$aic+L*(log(N)-2)
# Parameters = 150
# AIC = 4064.1 vs. 4187.8 [Little better than skill6.SS]
# BIC = 5043.835 vs. 6147.14 [Little better than skill6.SS.]

# Is Assistance Score (AssistProbMod = 1/(1 + AssistanceScore)) better than Success?
# What about with SuccessMod?
absref.model.first.succmod = glm(SuccessMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.succmod)
L=length(coef(absref.model.first.succmod)); L;
summary(absref.model.first.succmod)$aic+L*(log(N)-2)
# Parameters = 52
# AIC = 3483.6 vs. 3751.7 vs. 4001.0 vs. 3988.7 [Best, tied with absref.model.first.succmod.cond! Even etter than both absref.model.first.comp.mod and previous best of absref.model.first.comp.mod.bothS]
# BIC = 3823.271 vs. 4340.675 vs. 4356.473 [Wow! BEST so far, by a lot! Beats absref.model.first.mod2!]
AbsRefSkill                  -2.30962    0.14619 -15.799  < 2e-16 ***
LnOppRelRef                   0.73636    0.07774   9.472  < 2e-16 ***
AbsRefSkill:LnOppAbsRefFirst  0.47591    0.03360  14.165  < 2e-16 ***

# Wow!  Using the less continuous measure of Success beats the more continuous metric of AssistProbMod.
# Why is that?  Could it be that likelihood tends to be higher when the data has more extreme values,
# that is, closer to 0 and 1?

# What about adding Condition back?
absref.model.first.succmod.cond = glm(SuccessMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+AbsRefSkill:LnOppAbsRefFirst:Condition+LnOppRelRef-1, family=binomial(), data=Excel); summary(absref.model.first.succmod.cond)
L=length(coef(absref.model.first.succmod.cond)); L;
summary(absref.model.first.succmod.cond)$aic+L*(log(N)-2)
# Parameters = 52
# AIC = 3482.0 vs. 3483.6 [Best, tied with absref.model.first.succmod]
# BIC = 3828.198 vs. 3823.271 [Slightly worse than absref.model.first.succmod]
AbsRefSkill                              -2.32347    0.14682 -15.825  < 2e-16 ***
LnOppRelRef                               0.73527    0.07750   9.488  < 2e-16 ***
AbsRefSkill:LnOppAbsRefFirst              0.50767    0.03777  13.440  < 2e-16 ***
AbsRefSkill:LnOppAbsRefFirst:ConditionIN -0.06507    0.03435  -1.894  0.05822 .  

# Marginally significant, but in the wrong direction!!

# Check the slope without student
absref.model.first.succmod.cond.math = glm(SuccessMod~MATH+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+AbsRefSkill:LnOppAbsRefFirst:Condition+LnOppRelRef+LnOppRelRef:Condition-1, family=binomial(), data=Excel); summary(absref.model.first.succmod.cond.math)
L=length(coef(absref.model.first.succmod.cond.math)); L;
summary(absref.model.first.succmod.cond.math)$aic+L*(log(N)-2)
# Parameters = 7
# AIC = 4225.5 [How does this compare to other math, no student models?]
# BIC = 4271.234 [How does this compare to other math, no student models?]
MATH                                      0.40209    0.02236  17.980  < 2e-16 ***
AbsRefSkill                              -2.05546    0.10537 -19.507  < 2e-16 ***
LnOppRelRef                               0.76532    0.09061   8.446  < 2e-16 ***
AbsRefSkill:LnOppAbsRefFirst              0.42213    0.02786  15.151  < 2e-16 ***
ConditionEX:LnOppRelRef                  -0.24194    0.10692  -2.263   0.0236 *  
ConditionIN:LnOppRelRef                        NA         NA      NA       NA    
AbsRefSkill:LnOppAbsRefFirst:ConditionIN  0.07015    0.01466   4.787 1.70e-06 ***

# Important: Good, there is a condition slope difference in the right direction!


absref.succ.cond.2ndOpp = glm(SuccessMod~MATH+AbsRefSkill+Condition, family=binomial(), data=subset(Excel, Opp6==2)); summary(absref.succ.cond.2ndOpp)
L=length(coef(absref.succ.cond.2ndOpp)); L;
summary(absref.succ.cond.2ndOpp)$aic+L*(log(N)-2)
# Parameters = 4
# AIC = 298.51 vs. 353.37 [Better than cond.model.skill6.noS.2ndOpp]
# BIC = 324.6357
(Intercept)  0.02596    0.37364   0.069    0.945    
MATH         0.59657    0.10800   5.524 3.32e-08 ***
AbsRefSkill -1.52976    0.20571  -7.436 1.03e-13 ***
ConditionIN  0.19325    0.29371   0.658    0.511    

# Condition is not significant, but in the right direction.  Not enough data perhaps.
# How about for Opps 2-4
absref.succ.cond.Opps2to4 = glm(SuccessMod~MATH+AbsRefSkill+Condition, family=binomial(), data=subset(Excel, Opp6>1&Opp6<5)); summary(absref.succ.cond.Opps2to4)
L=length(coef(absref.succ.cond.Opps2to4)); L;
summary(absref.succ.cond.Opps2to4)$aic+L*(log(N)-2)
# Parameters = 4
# AIC = 298.51 vs. 353.37 [Better than cond.model.skill6.noS.2ndOpp]
# BIC = 324.6357
(Intercept)  0.02596    0.37364   0.069    0.945    
MATH         0.59657    0.10800   5.524 3.32e-08 ***
AbsRefSkill -1.52976    0.20571  -7.436 1.03e-13 ***
ConditionIN  0.19325    0.29371   0.658    0.511    


# Check the fit with the conditions.
# EX
absref.model.first.succmod.EX = glm(SuccessMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=subset(Excel, Condition=="EX")); summary(absref.model.first.succmod.EX)
L=length(coef(absref.model.first.succmod.EX)); L;
summary(absref.model.first.succmod.EX)$aic+L*(log(N)-2)
# Parameters = 27
# AIC = 1943.1
# BIC = 2119.414

# IN
absref.model.first.succmod.IN = glm(SuccessMod~Student+AbsRefSkill+AbsRefSkill:LnOppAbsRefFirst+LnOppRelRef-1, family=binomial(), data=subset(Excel, Condition=="IN")); summary(absref.model.first.succmod.IN)
L=length(coef(absref.model.first.succmod.IN)); L;
summary(absref.model.first.succmod.IN)$aic+L*(log(N)-2)
# Parameters = 28
# AIC = 1533.2 vs. 1943.1 [Better than absref.model.first.succmod.EX]
# BIC = 1716.044 vs. 2119.414 [Better than absref.model.first.succmod.EX]

# IN is better than EX for 2 skill conjunctive model

## How about EX vs. IN for skill6? 
skill6.succmod.EX = glm(SuccessMod~Student+Skills6+Skills6:LnOpp6-1, family=binomial(), data=subset(Excel, Condition=="EX")); summary(skill6.succmod.EX)
L=length(coef(skill6.succmod.EX)); L;
summary(skill6.succmod.EX)$aic+L*(log(N)-2)
# Parameters = 35
# AIC = 1941.2 vs. 1943.1  [Slighlty better than absref.model.first.succmod.EX]
# BIC = 2169.781 vs. 2119.414 [Much worse than absref.model.first.succmod.EX]
# Against hypothesis: So the more specific skill6 is not a better model for EX than the 2 skill.  

skill6.succmod.IN = glm(SuccessMod~ Student+Skills6+Skills6:LnOpp6-1, family=binomial(), data=subset(Excel, Condition=="IN")); summary(skill6.succmod.IN)
L=length(coef(skill6.succmod.IN)); L;
summary(skill6.succmod.IN)$aic+L*(log(N)-2)
# Parameters = 36
# AIC = 1532 vs. 1533.2 [Slighlty better than absref.model.first.succmod.IN]
# BIC = 1767.085 vs. 1716.044 vs. 2119.414 [Much worse than absref.model.first.succmod.IN]
# For hypothesis: skill6 is worse than skill2-conj

## How about EX vs. IN for skill3? 
skill3.succmod.EX = glm(SuccessMod~Student+Skills3+Skills3:LnOpp3-1, family=binomial(), data=subset(Excel, Condition=="EX")); summary(skill3.succmod.EX)
L=length(coef(skill3.succmod.EX)); L;
summary(skill3.succmod.EX)$aic+L*(log(N)-2)
# Parameters = 29
# AIC = 1942.9 vs. 1941.2 vs. 1943.1  [in between]
# BIC = 2132.266 vs. 2169.781 vs. 2119.414 [in between]
# Against hypothesis: More general models are working better for EX too.  


## Can we fit skill models to individual students?   Does the more specific model work for any of them?
## Get less data per student, so may need to go to AIC?  
## And will AssistProbMod work better than SuccessMod for students?

skill3.succmod.S1 = glm(SuccessMod~Skills3+Skills3:LnOpp3-1, family=binomial(), data=subset(Excel, Snum==1)); summary(skill3.succmod.S1)
L=length(coef(skill3.succmod.S1)); L;
summary(skill3.succmod.S1)$aic+L*(log(N)-2)
# Parameters = 6
# AIC = 105.46
# BIC = 144.6445
skill6.succmod.S1 = glm(SuccessMod~Skills6+Skills6:LnOpp6-1, family=binomial(), data=subset(Excel, Snum==1)); summary(skill6.succmod.S1)
L=length(coef(skill6.succmod.S1)); L;
summary(skill6.succmod.S1)$aic+L*(log(N)-2)
# Parameters = 12
# AIC = 112.57
# BIC = 190.9411

for(i in 1:49){
skill3.succmod.S = glm(SuccessMod~Skills3+Skills3:LnOpp3-1, family=binomial(), data=subset(Excel, Snum==i)); 
skill6.succmod.S = glm(SuccessMod~Skills6+Skills6:LnOpp6-1, family=binomial(), data=subset(Excel, Snum==i)); 
print(i);
if(summary(skill3.succmod.S)$aic < summary(skill6.succmod.S)$aic) 
{print("AIC: Sk3")} 
else {print("AIC: Sk6")}
if(summary(skill3.succmod.S)$aic+length(coef(skill3.succmod.S))*(log(N)-2) < summary(skill6.succmod.S)$aic+ length(coef(skill6.succmod.S))*(log(N)-2))
{print("BIC: Sk3")} 
else {print("AIC: Sk6")}
	}

# I need to learn how to print on a single line!
# Result though is Skill3 always wins on BIC.
# AIC wins on all but a few.
[1] 1
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 2
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 3
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 4
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 5
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 6
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 7
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 8
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 9
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 10
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 11
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 12
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 13
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 14
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 15
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 16
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 17
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 18
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 19
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 20
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 21
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 22
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 23
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 24
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 25
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 26
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 27
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 28
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 29
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 30
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 31
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 32
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 33
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 34
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 35
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 36
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 37
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 38
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 39
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 40
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 41
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 42
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 43
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 44
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 45
[1] "AIC: Sk6"
[1] "BIC: Sk3"
[1] 46
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 47
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 48
[1] "AIC: Sk3"
[1] "BIC: Sk3"
[1] 49
[1] "AIC: Sk3"
[1] "BIC: Sk3"

for(i in 1:49){
skill3.succmod.S = glm(SuccessMod~Skills3+Skills3:LnOpp3-1, family=binomial(), data=subset(Excel, Snum==i)); 
skill6.succmod.S = glm(SuccessMod~Skills6+Skills6:LnOpp6-1, family=binomial(), data=subset(Excel, Snum==i)); 
if(summary(skill3.succmod.S)$aic > summary(skill6.succmod.S)$aic) {print(i); print(summary(Condition[Snum==i]))} 
	} 
#AIC is better for Sk6 than Sk3	:
[1] 13
EX IN 
91  0 
[1] 19
EX IN 
51  0 
[1] 22
 EX  IN 
  0 226 
[1] 31
EX IN 
23  0 
[1] 32
EX IN 
 0 54 
[1] 33
EX IN 
84  0 
[1] 34
 EX  IN 
  0 128 
[1] 35
 EX  IN 
  0 110 
[1] 38
 EX  IN 
184   0 
[1] 39
EX IN 
60  0 
[1] 43
 EX  IN 
105   0 
[1] 45
 EX  IN 
  0 119 

# EX: 13,19,23,33,38,60,43 -- 7 students
# IN: 22,32,34,35,45 -- 5 students

## Repeat other prior analysis using AssistProbMod or SuccessMod



#### How many individual students in each group are best fit by which model?
### Are a greater proportion of IN students fit by a more compact model?
## Can I write a loop?  Need to be doing a lot of selection.

for(i in 1:141){
	error.rate.ans[i] = 1- mean(Success[adj.count==i & Condition=="Answer"])
	}

skill.model6 = glm(Partial.Credit.Crt[Student=="1"]~Skills6[Student=="1"]+LnOpp6[Student=="1"]:Skills6[Student=="1"]-1, family=binomial(), data=Excel)
best.aic[1]=summary(skill.model6)$aic
best.aic.model[1]="skill6"
best.bic[1]=best.aic[1]+length(coef(skill.model6))*(log(length(Student[Student=="1"]))-2) 
best.bic.model[1]="skill6"

skill.model5 = glm(Partial.Credit.Crt[Student=="1"]~Skills5[Student=="1"]+LnOpp5[Student=="1"]:Skills5[Student=="1"]-1, family=binomial(), data=Excel)
# AIC: 4320.8
new-bic=summary(skill.model5)$aic+length(coef(skill.model5))*(log(N)-2)
# BIC: 4699.612

skill.model4 = glm(Partial.Credit.Crt~Student+Skills4+LnOpp4:Skills4-1, family=binomial(), data=Excel); summary(skill.model4)
# AIC: 4323.7
summary(skill.model4)$aic+length(coef(skill.model4))*(log(N)-2)
# BIC: 4689.432

skill.model3 = glm(Partial.Credit.Crt~Student+Skills3+LnOpp3:Skills3-1, family=binomial(), data=Excel); summary(skill.model3)
# AIC: 4323.5
summary(skill.model3)$aic+length(coef(skill.model3))*(log(N)-2)
# BIC: 4676.178

skill.model2 = glm(Partial.Credit.Crt~Student+Skills2+LnOpp2:Skills2-1, family=binomial(), data=Excel); summary(skill.model2)