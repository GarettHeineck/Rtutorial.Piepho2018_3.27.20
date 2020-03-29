#*#
#*#
#*#
#### INTRODUCTION ####
## Turfgrass lab meeting R tutorial
## Based on:
##  Piepho, H.P. and Edmondson, R.N., 2018. 
##  A tutorial on the statistical analysis of factorial experiments with qualitative and quantitative treatment factor levels. 
##  Journal of Agronomy and Crop Science, 204(5), pp.429-455.
## Author of changes:
##  Garett Heineck
##  gch.analysis.solutions@gmail.com 
##  https://docs.google.com/forms/d/e/1FAIpQLSfcf3kgeZ7_ToxD0b1lpzLbexpNOm5Ri5mcwmMa2jBBWJ6hFA/viewform?usp=sf_link
##  3.27.2020
#
#
## Remeber to UPDATE R!
R.Version()[c('version.string','nickname')]
## Hey! Is your version 3.6.3 "Holding the Windsock?"
#  
#
#
## Required packages:
if(!require(agriTutorial)){
  install.packages("agriTutorial")
  require(agriTutorial)
}
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(agridat)){
  install.packages("agridat")
  require(agridat)
}
if(!require(desplot)){
  install.packages("desplot")
  require(desplot)
}
if(!require(lmer)){
  install.packages("lmer")
  require(lmer)
}
if(!require(lmerTest)){
  install.packages("lmerTest")
  require(lmerTest)
}
if(!require(nlme)){
  install.packages("nlme")
  require(nlme)
}
#
#
## Custom theme to be used thoughout the script
Turf_theme1<- function(base_size = 18) {
  theme_minimal(base_size = base_size) %+replace%
    theme(strip.background = element_rect(fill = "grey85", color = "black", linetype = 1),
          legend.background =  element_rect(fill = "white", linetype = 0),
          legend.position = "bottom",
          panel.grid.major.x = element_line(linetype = "dotted", color = "grey40", size = .1),
          panel.grid.major.y = element_line(linetype = "dotted", color = "black", size = .3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = alpha("white", 0), color = "grey85"),
          axis.line = element_line(size = 1.5, colour = "grey40"),
          complete = TRUE)
}
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### PRACTICE PLOTTING ####
## Using the stock 'iris' data set
## Plotting using 'ggplot2'
head(iris)
#
#
ggplot(iris, aes(y=Sepal.Length,
                 x=Petal.Width))+
  geom_point()+
  Turf_theme1(base_size = 3)
#
#
ggplot(iris, aes(y=Sepal.Length,
                 x=Petal.Width))+
  geom_point(size=2)+ #change size
  Turf_theme1(base_size = 15) #change size***
#
#
ggplot(iris, aes(y=Sepal.Length,
                 x=Petal.Width,
                 color=Species))+ #add species***
  geom_point(size=2)+
  Turf_theme1(base_size = 15)
#
#
ggplot(iris, aes(y=Sepal.Length,
                 x=Petal.Width,
                 color=Species))+
  geom_point(size=2)+
  stat_smooth(method = 'lm',
              formula = y ~ x)+ #add a regression***
  Turf_theme1(base_size = 15)
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### 3 EXAMPLE 1—FACTORIAL INTERACTIONS ####
## Based on:
##  Gomez & Gomez (1984, p. 143) (download book: https://scholar.google.com/scholar?hl=en&as_sdt=0%2C24&q=Statistical+procedures+for+agricultural+research&btnG=)
##  A rice experiment with three management practices 
##  3 management practices (minimum, optimum, intensive)
##  5 different amounts of nitrogen (N) fertilizer (0, 50, 80, 110, 140 kg/ha)
##  3 varieties (V1, V2, V3)
#
#
## Look at a summary of the data
summary(rice)
#
#
## Look at plot design
##  NOTICE: I am using the dataset from 'Agridat'
##  Agridat data is the same it just has row/column designators
gomez.splitsplit$nf <- factor(gomez.splitsplit$nitro)
ggdesplot(nf ~ col*row, 
          data=gomez.splitsplit,
          out1=rep, col=management, text=gen, 
          cex=1.5)+
  scale_fill_manual(values = c("grey60","grey70","grey80","grey90","grey100"))+
  scale_color_manual(values = c("red4", "red1", "orange2"))+
  labs(fill  ="Nitrogen rate",
       color ="Managment type",
       title ="Example 1 - Rice")
## THINGS TO NOTE:
##  There is not complete randomization of 'management' and 'nitrogen' treatments
##  Variety is completely randomized
#
#
## Fit a linear model ('lm') for a FACTORIAL design 
rice.lm1.1 = lm(yield ~ Replicate + management * variety * nitrogen +
                  (Replicate:nitrogen)+(Replicate:nitrogen:management), 
                data=rice)
anova(rice.lm1.1)
## THINGS TO NOTE:
##  Compare output with Table 1 in Piepho & Edmondson (2018)
##  There is only one error term
##  The MS values are identical
##  The F values are mostly incorrect 
#
#
## Fit a linear model ('aov') for a nested design
rice.lm1.2 = aov(yield ~ Replicate + management * variety * nitrogen +
                    Error((Replicate:nitrogen)+(Replicate:nitrogen:management)), #Error() informs F tests
                  data=rice)
summary(rice.lm1.2)
## THINGS TO NOTE:
##  Compare output with Table 1 in Piepho & Edmondson (2018)
##  There are three error terms
##  MS and F values are correct
##  Variety and Nitrogen interact with each other
#
#
## Calculating estimated marginal means (averages) from model
rice.emmean1.1<- emmeans(rice.lm1.2, ~ variety * nitrogen) #warning messages are ok***
rice.data1.1<- as.data.frame(rice.emmean1.1) #converting to a data frame
rice.emmean1.1
## THINGS TO NOTE:
##  Compare with Table 2 in Piepho & Edmondson (2018)
##  Variety and Nitrogen treatment are included in the same emmean
##  Managment treatments can be observed independantly
#
#
## Plotting means as a dot plot
ggplot(rice.data1.1, aes(y=emmean,
                         x=nitrogen,
                         color=variety,
                         shape=variety))+
  geom_point(size=3)+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety")+
  scale_color_manual(values = c("green4","red3","blue3"))+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
## THINGS TO NOTE:
##  Compare with Figure 1 in Piepho & Edmondson (2018)
##  The dots have the correct patterns
##  Regression lines are absent
#
#
## Plotting means as a bar plot
ggplot(rice.date1.1, aes(y=emmean,
                         x=nitrogen,
                         fill=variety))+
  geom_bar(stat="identity",
           position=position_dodge())+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety",
       linetype="Variety")+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
#
#
## Calculating standard error of the mean (SEM) and contrasts
nitro.by.var1<- emmeans::emmeans(rice.lm1.2, ~ nitrogen|variety) #hold variety constant***
contrast(nitro.by.var1, alpha = 0.05, method = "pairwise")
var.by.nitro1 = emmeans::emmeans(rice.lm1.2, ~ variety|nitrogen) #hold nitrogen constant***
contrast(var.by.nitro1, alpha = 0.05, method = "pairwise")
## THINGS TO NOTE:
##  Compare with footers in Table 2 Piepho & Edmondson (2018)
##  SEM are slightly different
##  Table 2 used REML not Sums of Squares to calculate SEM 
##  This results in different values
##  Output tests for differences between treatment levels
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### 4 MIXED MODEL ANALYSIS OF EXAMPLE 1 BY REML ####
## Based on:
##  Gomez & Gomez (1984, p. 143) (download book: https://scholar.google.com/scholar?hl=en&as_sdt=0%2C24&q=Statistical+procedures+for+agricultural+research&btnG=)
##  A rice experiment with three management practices 
##  3 management practices (minimum, optimum, intensive)
##  5 different amounts of nitrogen (N) fertilizer (0, 50, 80, 110, 140 kg/ha)
##  3 varieties (V1, V2, V3)
## NOTE the rice data set is still being used, different statistics are being applied
#
#
## Fit a linear mixed model ('lmer') for a nested design 
rice.lmer1.1<- lmer(yield ~ Replicate + nitrogen * management * variety + 
                      (1|Replicate:nitrogen) + #random effect***
                      (1|Replicate:nitrogen:management), #random effect***
                data=rice)
summary(rice.lmer1.1)
anova(rice.lmer1.1, ddf = "Kenward-Roger", type = 1)
anova(rice.lmer1.1, ddf = "Satterthwaite", type = 1)
## THINGS TO NOTE:
##  Compare with Table 3 in Piepho & Edmondson (2018)
##  ANOVA output has two columns for degrees of freedom (df)
##  'DenDf' (denominator) is analagous to Error df in Table 1
##  Mixed models have different ways to calculated df and result in slightly different answers
##  Mixed models have three ways to approximate 'types of Sums of Squares' (type=1,2,3)
##  Linear models also have three different ways to conduct 'types of Sums of Squares' this is not covered here
#
#
## Calculating estimated marginal means (averages) from mixed model
rice.emmean1.1<- emmeans(rice.lmer1.1, ~ variety * nitrogen) #warning messages are ok***
rice.emmean1.1
## THINGS TO NOTE:
##  Compare with Table 2 in Piepho & Edmondson (2018)
##  Values are correct and are the same as the linear model
#
#
## Calculating standard error of the mean (SEM) and contrasts
nitro.by.var2<- emmeans(rice.lmer1.1, ~ nitrogen|variety) #hold variety constant***
contrast(nitro.by.var2, alpha = 0.05, method = "pairwise")
var.by.nitro2 = emmeans(rice.lmer1.1, ~ variety|nitrogen) #hold nitrogen constant***
contrast(var.by.nitro2, alpha = 0.05, method = "pairwise")
## THINGS TO NOTE:
##  Compare with footers in Table 2 Piepho & Edmondson (2018)
##  Values are very similar to the table
##  Values from SEM from mixed model are different than linear model
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### 5 EXAMPLE 2—LACK OF FIT AND MARGINALITY FOR A SINGLE QUANTITATIVE TREATMENT FACTOR ####
## Based on:
##  Petersen, R.G., 1994, p. 125
##  Agricultural field experiments: design and analysis. CRC Press.
##  5 different quantities of N-fertiliser (0, 35, 70, 105 and 140 kg N/ha)
##  Response is yield of root dry matter
## NOTE that nitrogen can be either considered a factor, as before, or numeric (continuous)
#
#
## Observe data
summary(beet)
#
#
## Plotting the data with simple linear regression
ggplot(beet, aes(y=yield,
                 x=nrate))+
  geom_point(shape=1,
             size=2)+
  stat_summary(fun = "mean", 
               color = "black", 
               size = 2, 
               geom = "point")+
  stat_smooth(method = 'lm',
              formula = y ~ x, #linear term only***
              se=F,
              color="black")+
  labs(x="Amount of nitrogen (kg)",
       y="Yield (kg/ha)")+
  Turf_theme1(base_size = 20)
## THINGS TO NOTE:
##  Compare to Figure 2 in Piepho & Edmondson (2018)
##  Simple 'lm' applied to the 'stat_smooth' function
#
#
## Method 1 (Agritutorial package): fitting a polynomial
##  Calculate non-orthogonal polynomials up to a quartic level
##  Bind new data columns to original data from
##  use anova and summary functions to explore best fit
N<- poly((beet$nrate), degree = 4, raw = TRUE)
colnames(N)<- c("Linear_N", "Quadratic_N", "Cubic_N", "Quartic_N")
beet<- cbind(beet, N)
anova(lm(yield ~   0+Replicate + Linear_N + Quadratic_N + Cubic_N + Quartic_N, data = beet)) #the '0' was added to exclude the intercept***
summary(lm(yield ~ 0+Replicate + Linear_N + Quadratic_N + Cubic_N + Quartic_N, data = beet)) #marginal fit***
summary(lm(yield ~ 0+Replicate + Linear_N + Quadratic_N + Cubic_N, data = beet)) #remove quartic***
summary(lm(yield ~ 0+Replicate + Linear_N + Quadratic_N, data = beet)) #remove cubic***
## THINGS TO NOTE:
##  Loosly compares to tables 4 and 5 in Piepho & Edmondson (2018)
##  'anova' fits sequentially in the order added (unless there are interactions added)
##  'summary' fits marginally, which fits everything then one of the terms
##  'summary' it is obvious by the summary output that quadratic, cubic and quartic terme are colinear
#
#
## Method 1 (used by most R users): fitting a polynomial
##  No need to create new columns
##  Same results as poly(degree=4, raw=F)
##  raw=T give orthogonal polynomials which are harder to interperet
anova(lm(yield ~   0+Replicate + nrate + I(nrate^2) + I(nrate^3) + I(nrate^4), data = beet))
summary(lm(yield ~ 0+Replicate + nrate + I(nrate^2) + I(nrate^3) + I(nrate^4), data = beet))
summary(lm(yield ~ 0+Replicate + nrate + I(nrate^2) + I(nrate^3), data = beet))
summary(lm(yield ~ 0+Replicate + nrate + I(nrate^2), data = beet))
#
#
## Calculating estimates from the best fit model (quadratic)
beet$Replicate<- factor(beet$Replicate, levels = c("3","2","1"))
beet.lm1.1<- lm(yield ~ Replicate + nrate + I(nrate^2), data = beet)
beet.summary1.1<- summary(beet.lm1.1)[[4]][,1:2]
beet.confidence1.1<- confint(beet.lm1.1, level = 0.95)
round(cbind(beet.summary1.1,beet.confidence1.1), 2) #rows 1-6 in Table 6***
beet.ref1.1<- ref_grid(beet.lm1.1, at = list(nrate = 0))
emmeans(beet.ref1.1, ~ nrate) #this is an easy way to get the last row of Table 6***
## THINGS TO NOTE:
##  Loosly compares to table 6 in Piepho & Edmondson (2018)
##  There was really no reason for the author to have the 4th row in the table
##  The last row in the table are average of blocks at the y-intercept
#
#
## Plotting the optimal model
ggplot(beet, aes(y=yield,
                 x=nrate))+
  geom_point(shape=1,
             size=2)+
  stat_summary(fun = "mean", 
               color = "black", 
               size = 2, 
               geom = "point")+
  stat_smooth(method = 'lm',
              formula = y ~ x + I(x^2), #quadratic term I(x^2) is similar to the model***
              se=F,
              color="black")+
  labs(x="Amount of nitrogen (kg)",
       y="Yield (kg/ha)")+
  Turf_theme1(base_size = 20)
## THINGS TO NOTE:
##  Compare to Figure 3 in Piepho & Edmondson (2018)
##  The optimal model was added into the 'stat_smooth' function
#
#
## Apply the delta method for optimal nitrogen
##  Plot that on the last figure 
##  Maximum point on a quadratic is often useful
B1<- summary(beet.lm1.1)[[4]][4,1] #B1 (equation 5 Piepho & Edmondson (2018))
B2<- summary(beet.lm1.1)[[4]][5,1] #B2 (equation 5 Piepho & Edmondson (2018))
max.nitrogen<- -B1/(2*B2)
#
#
ggplot(beet, aes(y=yield,
                 x=nrate))+
  geom_point(shape=1,
             size=2)+
  stat_summary(fun = "mean", 
               color = "black", 
               size = 2, 
               geom = "point")+
  stat_smooth(method = 'lm',
              formula = y ~ x + I(x^2),
              se=F,
              color="black")+
  geom_vline(xintercept = max.nitrogen, #adding the point of max nitrogen***
             linetype="dashed",
             color="red")+
  labs(x="Amount of nitrogen (kg)",
       y="Yield (kg/ha)")+
  Turf_theme1(base_size = 20)
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### 6 EXAMPLE 1 (CONTINUED)—COMPARING SEVERAL REGRESSIONS IN AN EXPERIMENT WITH QUALITATIVE AND QUANTITATIVE TREATMENT FACTORS ####
## Based on:
##  Gomez & Gomez, 1984, p. 143)
##  A rice experiment with three management practices 
##  3 management practices (minimum, optimum, intensive)
##  5 different amounts of nitrogen (N) fertilizer (0, 50, 80, 110, 140 kg/ha)
##  3 varieties (V1, V2, V3)
## NOTE the rice data set is still being used, however nitrogen will be considered NUMERIC (was a factor before)
## In classical experimental analysis this would be called analysis of covariance (ANCOVA)
##  Often statisticians do notnuse this term
##  I am not sure why this is, but I notice some folks really dislike that phrase
#
#
summary(rice) #instead of 'nitrogen' this section uses 'nrate'
#
#
## Mixed effects model with nitrogen as a continuous variable
rice.lmer2.1<- lmer(yield ~ Replicate + management + variety * (nrate + I(nrate^2) + I(nrate^3) + I(nrate^4)) + #fixed effects***
                                  (1|Replicate:nrate) + (1|Replicate:nrate:management), #random effects***
                   data = rice)
anova(rice.lmer2.1, ddf = "Kenward-Roger", type = 1)
#
#
rice.lmer2.2<- lmer(yield ~ Replicate + management + variety * (nrate + I(nrate^2)) + #fixed effects***
                      (1|Replicate:nrate) + (1|Replicate:nrate:management), #random effects***
                    data = rice)
anova(rice.lmer2.2, ddf = "Kenward-Roger", type = 1)
#
#
## Making a comparable linear model 
rice.aov2.1<- aov(yield ~ Replicate + management + variety * (nrate + I(nrate^2)) + #fixed effects***
                      Error((Replicate:nrate) + (Replicate:nrate:management)), #fixed effects***
                    data = rice)
summary(rice.aov2.1, ddf = "Kenward-Roger", type = 1)
## THINGS TO NOTE:
##  Compare model 2.1 to Table 7 in Piepho & Edmondson (2018)
##  The mixed model and linear model give similar results
##  If there was missing data a linear model would be difficult to use
#
#
## Calculating useful coefficients
rice.ref2.1<- ref_grid(rice.lmer2.2, 
                       at = list(nrate = 0)) #yield at the y intercept
emmeans(rice.ref2.1, ~ variety)
## THINGS TO NOTE:
##  Compare to Table 8 first column in Piepho & Edmondson (2018)
##  Table 8 is not incredible useful so additional plotting is provided below
#
#
## Plotting results from mixed model
##  Instead of plotting using the stat_smooth emmip from emmeans package was employed
rice.ref2.2<- ref_grid(rice.lmer2.2, 
                       at = list(nrate = seq(0,140, by=10)))
emmip(rice.ref2.2, variety ~ nrate , cov.reduce = T)+
  geom_point()+
  geom_line()+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety",
       linetype="Variety")+
  scale_color_manual(values = c("green4","red3","blue3"))+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
## THINGS TO NOTE:
##  Compare to Figure 1 first column in Piepho & Edmondson (2018)
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###
#### 8 EXAMPLE 4—AN EXPERIMENT WITH ONE QUALITATIVE TREATMENT FACTOR AND REPEATED MEASURES IN TIME ####
## Based on:
##  Milliken & Johnson (1992, p. 429)
##  4 sorghum varieties
##  5 replicate blocks
##  5 consecutive measures starting two weeks after emergence
##  Response was leaf area index
#
#
summary(sorghum)
#
#
sorghum$rawWeeks = poly(sorghum$varweek, degree = 4, raw = TRUE)
sorghum$polWeeks = poly(sorghum$varweek, degree = 4, raw = FALSE)
sorghum$polBlocks = poly(sorghum$varblock, degree = 4, raw = FALSE)
sorghum$factblock = factor(sorghum$varblock)
#
#
AIC = NULL
logLik = NULL
Model = c("ID", "CS", "AR(1)", "AR(1) + nugget", "UN")
#
#
full_indy = gls(y ~ factweek * (Replicate + variety), sorghum)
anova(full_indy)
AIC = c(AIC, AIC(full_indy))
logLik = c(logLik, logLik(full_indy))
#
#
corCompSymm = gls(y ~ factweek * (Replicate + variety),
                  corr = corCompSymm(form = ~ varweek|factplot), sorghum)
anova(corCompSymm)
AIC = c(AIC, AIC(corCompSymm))
logLik = c(logLik, logLik(corCompSymm))
Variogram(corCompSymm)
#
#
corExp = gls(y ~ factweek * (Replicate + variety),
             corr = corExp(form = ~ varweek|factplot), sorghum)
anova(corExp)
AIC = c(AIC, AIC(corExp))
logLik = c(logLik, logLik(corExp))
Variogram(corExp)
#
#
corExp_nugget = gls(y ~ factweek * (Replicate + variety),
                    corr = corExp(form = ~ varweek|factplot, nugget = TRUE), sorghum)
anova(corExp_nugget)
AIC = c(AIC, AIC(corExp_nugget))
logLik = c(logLik, logLik(corExp_nugget))
Variogram(corExp)
#
#
corSymm = gls(y ~ factweek * (Replicate + variety), corr = corSymm(form = ~ 1|factplot),
              weights = varIdent(form = ~ 1|varweek), sorghum)
anova(corSymm)
AIC = c(AIC, AIC(corSymm))
logLik = c(logLik, logLik(corSymm))
Variogram(corSymm)
#
#
dAIC = AIC - AIC[4]
logLik = -2 * logLik
dlogLik = logLik - logLik[4]
AICtable = data.frame(Model, round(logLik, 2), round(dlogLik, 2), round(AIC, 2), round(dAIC, 2))
colnames(AICtable) = c("Covar_Model", "-2logLr", "-diff2logLr", "AIC", "diffAIC")
AICtable
#
#
pol_Wald =
  gls(y ~ (factblock+variety) * (rawWeeks[,1] + rawWeeks[,2] + polWeeks[,3] + polWeeks[,4]),
      corr = corExp(form = ~ varweek | factplot, nugget = TRUE), sorghum)
anova(pol_Wald)
range=coef(pol_Wald$modelStruct$corStruct,unconstrained=FALSE)[1]
nugget=coef(pol_Wald$modelStruct$corStruct,unconstrained=FALSE)[2]
rho=(1-nugget)*exp(-1/range)
cat("Range =", range, "\n")
cat("Nugget =", nugget, "\n")
cat("Correlation =", rho, "\n")
ACF(pol_Wald)
plot(pol_Wald,sub.caption = NA, main = "Residuals from full polynomial weeks model")
#
#
quad_Wald = gls(y ~ polBlocks + variety + rawWeeks[,1] + rawWeeks[,2] +
                  polBlocks:(rawWeeks[,1] + rawWeeks[,2]+ polWeeks[,3] + polWeeks[,4]) +
                  variety:(rawWeeks[,1] + rawWeeks[,2]),
                corr = corExp(form = ~ varweek | factplot, nugget=TRUE), sorghum)
anova(quad_Wald)
summary(quad_Wald)$tTable
vcov(quad_Wald)
range=coef(quad_Wald$modelStruct$corStruct,unconstrained=FALSE)[1]
nugget=coef(quad_Wald$modelStruct$corStruct,unconstrained=FALSE)[2]
rho=(1-nugget)*exp(-1/range)
cat("Range =", range, "\n")
cat("Nugget =", nugget, "\n")
cat("Correlation =", rho, "\n")
plot(quad_Wald,sub.caption = NA, main = "Residuals from quadratic regression model")
#
#
pol_Wald =
  gls(y ~ (factblock+variety) * (rawWeeks[,1] + rawWeeks[,2] + polWeeks[,3] + polWeeks[,4]),
      corr = corExp(form = ~ varweek | factplot, nugget = TRUE), sorghum)

summary(sorghum)
sorghum.gls1.1<- gls(y ~ (factblock+variety) * (varweek + I(varweek^2)),
                     corr = corExp(form = ~ varweek | factplot, nugget = TRUE),
                     data = sorghum)
anova(sorghum.gls1.1)
sorghum.refgrd1.1<- ref_grid(sorghum.gls1.1,at = list(varweek = seq(0,5, by=1)))
emmip(sorghum.refgrd1.1, variety ~ varweek)
### 
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
#*#
###