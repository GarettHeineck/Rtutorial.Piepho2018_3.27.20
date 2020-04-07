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
##  3.27.2020 and 4.07.2020
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
#### 1 PRACTICE PLOTTING ####
## Using the stock 'iris' data set
## Summarizing the iris data
summary(iris)
## THINGS TO NOTE:
##  The 'iris' data set contains information on species of iris
##  Sepal length 
##  Petal width
##  iris Species
#
#
## Using 'ggplot2' to plot the 'iris' data
ggplot(iris,                      #calling data frame
       aes(y=Petal.Width,        #calling sepal column***
           x=Sepal.Length))+       #calling petal column***
  geom_point()+                   #organizing data into geometric(geom) point plot (dot plot)***
  Turf_theme1(base_size = 3)      #adding the fancy turfgrass theme***
#
#
ggplot(iris, aes(y=Petal.Width,
                 x=Sepal.Length))+
  geom_point(size=2)+ #change dot size
  Turf_theme1(base_size = 15) #increase font size***
#
#
ggplot(iris, aes(y=Petal.Width,
                 x=Sepal.Length,
                 color=Species))+ #add species designation***
  geom_point(size=2)+
  Turf_theme1(base_size = 15)
#
#
ggplot(iris, aes(y=Petal.Width,
                 x=Sepal.Length,
                 color=Species))+ 
  geom_smooth(method = 'lm')+
  labs(title = "Practice Example: Iris data")+ #add title to plot
  geom_point(size=2)+
  Turf_theme1(base_size = 15)
#
#
ggplot(iris, aes(y=Petal.Width,
                 x=Sepal.Length,
                 color=Species))+
  labs(title = "Practice example - Iris data")+
  geom_point(size=2)+
  stat_smooth(method = 'lm',
              formula = y ~ x)+ #add a regression for each species***
  Turf_theme1(base_size = 15)
#
#
ggplot(iris, aes(Petal.Width))+
  geom_histogram(binwidth=.2)+  #designating a histogram for the geometric object*** 
  Turf_theme1(base_size = 15)
ggplot(iris, aes(Sepal.Length))+
  geom_histogram(binwidth=.2)+  #designating a histogram for the geometric object*** 
  Turf_theme1(base_size = 15)
ggplot(iris, aes(y=Petal.Width,
                 x=Species))+
  geom_boxplot()+  #designating a boxplot for the geometric object*** 
  Turf_theme1(base_size = 15)
#
#
ggplot(iris,                     
       aes(y=Petal.Width,        
           x=Sepal.Length))+
  geom_point()+
  geom_smooth(method='lm',
              formula = y ~ x,
              se=F)
## THINGS TO NOTE:
##  Generally, as petal width increases so does sepal width
##  Different species may have different widths
##  The linear relationship between width and length may change based on species
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
#### 2 PRACTICE MODEL FITTING ####
## Using the stock 'iris' data set
## Summarizing the iris data
summary(iris)
#
#
## Reject or fail to reject the hypothesis that:
##  Ho: no correlation exists 
##  Ha: petal ad petal width are correlated among species
##  Using a linear model with lm() or aov() 
##  aov() is a wrapper function for lm()
iris.lm.1<- lm(Petal.Width~ #y variable (response)***
                 Sepal.Length,#x variable (explanatory)***
               data=iris) #calling data***
summary(iris.lm.1) #summary output***
## THINGS TO NOTE:
##  Reject the null (Ho) in favor of alternative (Ha)
##  width and length are correlated
#
#
## Reject or fail to reject the hypothesis that:
##  Ho: species have the same petal width
##  Ha: species have different petal width
iris.lm.2<- lm(Petal.Width~ Species, #adding the factor species***
               data=iris) 
anova(iris.lm.2) 
## THINGS TO NOTE:
##  Reject the null (Ho) in favor of alternative (Ha)
##  Species have different petal widths
#
#
## Reject or fail to reject the hypothesis that:
##  Ho: the relationship (slope) of each species is the same for width and length
##  Ha: The relatioship is species dependant
iris.lm.3<- lm((Petal.Width)~ Species * Sepal.Length, # '*' adds and interaction term between variables***, 
               data=iris) 
anova(iris.lm.3)
summary(iris.lm.3)
## THINGS TO NOTE:
##  FAIL to reject the null (Ho)
##  The relationship is the same
#
#
lm3.assumptions<- data.frame(residuals=residuals(iris.lm.3),
                             fitted=fitted(iris.lm.3))
ggplot(data = lm3.assumptions, aes(y=residuals,
                                   x=fitted))+ 
  geom_point() +
  geom_smooth(se=F)
## THINGS TO NOTE:
##  Some uneven distribution around the line
##  This indicates assumptions are not being met
##  Try a square root transformation on petal width
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
##  Arranged in a split-split plot design
##  3 management practices (minimum, optimum, intensive)
##  5 different amounts of nitrogen (N) fertilizer (0, 50, 80, 110, 140 kg/ha)
##  3 varieties (V1, V2, V3)
#
#
## Summarizing rice data
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
##  Often split-plot (nested) designs are used for logistic reasons
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
rice.emmean1.1<- emmeans(rice.lm1.2, ~ variety * nitrogen) #warning messages are ok here***
rice.data1.1<- as.data.frame(rice.emmean1.1) #converting to a data frame
rice.emmean1.1
## THINGS TO NOTE:
##  Compare with Table 2 in Piepho & Edmondson (2018)
##  Variety and Nitrogen treatment are included in the same emmean
##  Managment treatments can be observed independantly
#
#
## Calculating standard error of the mean (SEM) and contrasts
nitro.by.var1<- emmeans(rice.lm1.2, ~ nitrogen|variety) #hold variety constant***
contrast(nitro.by.var1, alpha = 0.05, method = "pairwise")
var.by.nitro1 = emmeans(rice.lm1.2, ~ variety|nitrogen) #hold nitrogen constant***
contrast(var.by.nitro1, alpha = 0.05, method = "pairwise")
## THINGS TO NOTE:
##  Compare with footers in Table 2 Piepho & Edmondson (2018)
##  SEM are slightly different
##  Table 2 used REML not Sums of Squares to calculate SEM 
##  This results in different values
##  Output tests for differences between treatment levels
##  Here is an interesting tutorial on how to hand calculated SEM: http://www.fao.org/3/X6831E/X6831E09.htm#TopOfPage 
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
       shape="Variety",
       title = "Example 1 - rice data")+
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
## Plotting means as a bar plot and compact letter display
rice.cld.1.1<- data.frame(CLD(rice.emmean1.1, 
                              Letters = c("abcdefghijh"))) #designating letter grouping***
#
#
ggplot(rice.data1.1, aes(y=emmean,
                         x=nitrogen,
                         fill=variety))+
  geom_bar(stat="identity",
           position=position_dodge())+
  geom_text(rice.cld.1.1, mapping = aes(y=emmean+.5, #adding compact letter display***
                                        x=nitrogen,
                                        label=.group),
            position = position_dodge(width = .8))+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety",
       linetype="Variety",
       title = "Example 1 - rice data (linear model)")+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
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
rice.emmean2.1<- emmeans(rice.lmer1.1, ~ variety * nitrogen) #warning messages are ok***
rice.emmean2.1
## THINGS TO NOTE:
##  Compare with Table 2 in Piepho & Edmondson (2018)
##  Values are correct and are the same as the linear model
#
#
## Calculating standard error of the mean (SEM) and contrasts
nitro.by.var2<- emmeans(rice.lmer1.1, ~ nitrogen|variety) #hold variety constant***
contrast(nitro.by.var2, alpha = 0.05, method = "pairwise")
CLD(nitro.by.var2, Letters = c(letters))
var.by.nitro2 = emmeans(rice.lmer1.1, ~ variety|nitrogen) #hold nitrogen constant***
contrast(var.by.nitro2, alpha = 0.05, method = "pairwise")
CLD(var.by.nitro2, Letters = c(letters))
## THINGS TO NOTE:
##  Compare with footers in Table 2 Piepho & Edmondson (2018)
##  Values are very similar to the table
##  Values from SEM from mixed model are different than linear model
#
#
## Plotting means as a bar plot and compact letter display
rice.cld.2.1<- data.frame(CLD(rice.emmean2.1, 
                              Letters = c("abcdefghijh"))) #designating letter grouping***
#
#
ggplot(rice.cld.2.1, aes(y=emmean,
                         x=nitrogen,
                         fill=variety))+
  geom_bar(stat="identity",
           position=position_dodge())+
  geom_text(rice.cld.2.1, mapping = aes(y=emmean+.5, #adding compact letter display***
                                        x=nitrogen,
                                        label=.group),
            position = position_dodge(width = .8))+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety",
       linetype="Variety",
       title = "Example 1 - rice data (mixed model)")+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
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
       y="Yield (kg/ha)",
       title = "EXAMPLE 2 - beet data")+
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
anova(lm(yield ~   Replicate + Linear_N + Quadratic_N + Cubic_N + Quartic_N, data = beet)) #the '0' was added to exclude the intercept***
summary(lm(yield ~ Replicate + Linear_N + Quadratic_N + Cubic_N + Quartic_N, data = beet)) #marginal fit***
summary(lm(yield ~ Replicate + Linear_N + Quadratic_N + Cubic_N, data = beet)) #remove quartic***
anova(lm(yield ~   Replicate + Linear_N + Quadratic_N, data = beet)) #remove cubic***
## THINGS TO NOTE:
##  Loosly compares to tables 4 and 5 in Piepho & Edmondson (2018)
##  'anova' fits sequentially in the order added (unless there are interactions added)
##  'summary' fits marginally, which fits everything then one of the terms
##  'summary' it is obvious by the summary output that quadratic, cubic and quartic terme are colinear
#
#
## Method 2 (used by most R users): fitting a polynomial
##  No need to create new columns
##  Same results as poly(degree=4, raw=F)
##  raw=T give orthogonal polynomials which are harder to interperet
anova(lm(yield ~ Replicate + nrate + I(nrate^2) + I(nrate^3) + I(nrate^4), data = beet))
anova(lm(yield ~ Replicate + nrate + I(nrate^2) + I(nrate^3), data = beet))
anova(lm(yield ~ Replicate + nrate + I(nrate^2), data = beet))
anova(lm(yield ~Replicate + nrate + I(nrate^2), data = beet))
#
#
## Calculating estimates from the best fit model (quadratic)
beet$Replicate<- factor(beet$Replicate, levels = c("3","2","1"))
beet.lm1.1<- lm(yield ~ Replicate + nrate + I(nrate^2), data = beet)
beet.summary1.1<- summary(beet.lm1.1)[[4]][,1:2]
beet.confidence1.1<- confint(beet.lm1.1, level = 0.95)
round(cbind(beet.summary1.1,beet.confidence1.1), 3) #rows 1-6 in Table 6***
beet.ref1.1<- ref_grid(beet.lm1.1, at = list(nrate = 0))
beet.ref30.80<- ref_grid(beet.lm1.1, at = list(nrate = c(30,80,102)))
emmeans(beet.ref1.1, ~ nrate) #this is an easy way to get the last row of Table 6***
emmeans(beet.ref30.80, ~ nrate) #exmaple from demo slides
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
       y="Yield (kg/ha)",
       title = "EXAMPLE 2 - beet data")+
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
       y="Yield (kg/ha)",
       title = "Example 2 - beet data")+
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
##  Often statisticians do not use this term
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
anova(rice.lmer2.1, type = 1)
## THINGS TO NOTE:
##  This is the full polynomial model described in the previous section
##  Compare to Table 7 in Piepho & Edmondson (2018)
##  Cubic and quartic terms are at most maringally significant
##  The author opted for a more simplistic quadratic model 
#
#
rice.lmer2.2<- lmer(yield ~ Replicate + management + variety * nrate + I(nrate^2) + 
                      (1|Replicate:nrate) + (1|Replicate:nrate:management), 
                    data = rice)
summary(rice.lmer2.2)
anova(rice.lmer2.2, type = 1)
## THINGS TO NOTE:
##  In this example only those terms that are significant at alpha 0.05 or those that are marginal were included
##  This expcluded all cubic and quartic terms
##  This excluded variety by quadratic interaction
##  This model can be used to calculate estimates for Table 8 in Piepho & Edmondson (2018)
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
rice.ref2.2<- ref_grid(rice.lmer2.2, 
                       at = list(nrate = seq(0,140, by=1)))
rice.emm2.2<- data.frame(emmeans(rice.ref2.2, ~ variety*nrate))
#
#
ggplot(rice.emm2.2, aes(y=emmean,
                         x=nrate,
                         color=variety,
                         shape=variety))+
  geom_line(size=.75)+
  stat_summary(rice, mapping=aes(y=yield,
                               x=nrate,
                               color=variety,
                               shape=variety),
               size=.5)+
  labs(x="Nitrogen (kg/ha)",
       y="Yield (t/ha)",
       color="Variety",
       shape="Variety",
       title = "Example 1 - rice data (mixed model ANCOVA)")+
  scale_color_manual(values = c("green4","red3","blue3"))+
  scale_y_continuous(limits = c(0,11),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
## THINGS TO NOTE:
##  Compare to Figure 1 in Piepho & Edmondson (2018)
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
## Split-plot in time
sorghum.aov1.1<- aov(y~Replicate+variety*factweek + Error(Replicate:variety), 
                     data = sorghum)
summary(sorghum.aov1.1)
## THINGS TO NOTE:
##  Compare this to the split-split plot output
##  Variety has fewer df than week in the error term
#
#
##  Means comparisons using the split-plot in time model
sorghum.emm1.1<- emmeans(sorghum.aov1.1, ~ factweek|variety)
sorghum.cld1.1<- data.frame(CLD(sorghum.emm1.1, Letters = c(LETTERS)))
## THINGS TO NOTE:
##  Tukey contrasts for time points within variety
#
#
## Plotting split-plot in time analysis
ggplot(sorghum.cld1.1, aes(y=emmean,
                           x=factweek,
                           fill=variety))+
  geom_bar(stat="identity",
           position=position_dodge())+
  facet_wrap(~variety)+                                #creating a panel for each variety***
  geom_text(sorghum.cld1.1, mapping = aes(y=emmean+.5, #adding compact letter display***
                                        x=factweek,
                                        label=.group),
            position = position_dodge(width = 1))+
  labs(x="Weeks",
       y="Leaf area index",
       fill="Variety",
       title = "Example 4 - sorghum data (lm - split-plot in time)")+
  scale_y_continuous(limits = c(0,6),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
#
#
## Setting up polynomials for advanced tecnique
sorghum$rawWeeks = poly(sorghum$varweek, degree = 4, raw = TRUE)
sorghum$polWeeks = poly(sorghum$varweek, degree = 4, raw = FALSE)
sorghum$polBlocks = poly(sorghum$varblock, degree = 4, raw = FALSE)
sorghum$factblock = factor(sorghum$varblock)
## THINGS TO NOTE:
##  Source code: https://cran.r-project.org/web/packages/agriTutorial/agriTutorial.pdf
##  Each poly() add four columns to the data set
#
# 
## Setting up model output for autocorrelation testing
AIC = NULL
logLik = NULL
Model = c("ID", "CS", "AR(1)", "AR(1) + nugget", "UN")
#
#
## Independent uncorrelated random plots
full_indy = gls(y ~ factweek * (Replicate + variety), sorghum)
anova(full_indy)
AIC = c(AIC, AIC(full_indy))
logLik = c(logLik, logLik(full_indy))
#
#
## CorCompSymm compound symmetry
corCompSymm = gls(y ~ factweek * (Replicate + variety),
                  corr = corCompSymm(form = ~ varweek|factplot), sorghum)
anova(corCompSymm)
AIC = c(AIC, AIC(corCompSymm))
logLik = c(logLik, logLik(corCompSymm))
Variogram(corCompSymm)
#
#
## CorExp without nugget
corExp = gls(y ~ factweek * (Replicate + variety),
             corr = corExp(form = ~ varweek|factplot), sorghum)
anova(corExp)
AIC = c(AIC, AIC(corExp))
logLik = c(logLik, logLik(corExp))
Variogram(corExp)
#
#
## CorExp with nugget
corExp_nugget = gls(y ~ factweek * (Replicate + variety),
                    corr = corExp(form = ~ varweek|factplot, nugget = TRUE), sorghum)
anova(corExp_nugget)
AIC = c(AIC, AIC(corExp_nugget))
logLik = c(logLik, logLik(corExp_nugget))
Variogram(corExp)
#
#
## CorSymm unstructured
corSymm = gls(y ~ factweek * (Replicate + variety), corr = corSymm(form = ~ 1|factplot),
              weights = varIdent(form = ~ 1|varweek), sorghum)
anova(corSymm)
AIC = c(AIC, AIC(corSymm))
logLik = c(logLik, logLik(corSymm))
Variogram(corSymm)
#
#
## Log likelihood and AIC statistics for different correlation structures
dAIC = AIC - AIC[4]
logLik = -2 * logLik
dlogLik = logLik - logLik[4]
AICtable = data.frame(Model, round(logLik, 2), round(dlogLik, 2), round(AIC, 2), round(dAIC, 2))
colnames(AICtable) = c("Covar_Model", "-2logLr", "-diff2logLr", "AIC", "diffAIC")
AICtable
## THINGS TO NOTE:
##  Source code: https://cran.r-project.org/web/packages/agriTutorial/agriTutorial.pdf
##  Compare with Table 11
#
#
## Fitting the model with the best autocorrelation structure
sorghum.lme1.1<- lme(y ~ factblock + variety  + varweek + I(varweek^2) +
                     factblock:(varweek+I(varweek^2)) +
                     variety:(varweek+I(varweek^2)), 
                     random=~1|factplot,
                     corr = corExp(form = ~ varweek|factplot, nugget = TRUE),
                     data=sorghum)
anova(sorghum.lme1.1, type = "sequential")
#
#
sorghum.ref2.1<- ref_grid(sorghum.lme1.1, 
                       at = list(varweek = seq(1,5, by=1)))
sorghum.ref2.2<- ref_grid(sorghum.lme1.1, 
                          at = list(varweek = seq(1,5, by=.1)))
#
#
sorghum.emm2.1<- emmeans(sorghum.ref2.1, ~ varweek|variety)
sorghum.emm2.2<- data.frame(emmeans(sorghum.ref2.2, ~ varweek|variety))
#
#
sorghum.cld2.1<- data.frame(CLD(sorghum.emm2.1, Letters = c(LETTERS)))
#
#
## Plotting for comparison with split-plot in time
ggplot(sorghum.emm2.2, aes(y=emmean,
                           x=varweek,
                           color=variety))+
  geom_line()+
  facet_wrap(~variety)+                       
  geom_point(sorghum.cld2.1, mapping = aes(y=emmean,
                                            x=varweek,
                                            color=variety),
             shape=1)+
  geom_text(sorghum.cld2.1, mapping = aes(y=emmean+.4,
                                          x=varweek,
                                          color=variety,
                                          label=.group))+
  labs(x="Weeks",
       y="Leaf area index",
       fill="Variety",
       title = "Example 4 - sorghum data (mixed model - autocorrelation)")+
  scale_y_continuous(limits = c(0,6),
                     breaks = c(seq(0,11, by=2)))+
  Turf_theme1(base_size = 20)
#
#
## Recreate figure 5
ggplot(sorghum.emm2.2, aes(y=emmean,
                           x=varweek,
                           color=variety))+
  geom_line(size=.75)+
  geom_point(sorghum.cld2.1, mapping = aes(y=emmean,
                                           x=varweek,
                                           color=variety),
             shape=1,
             size=3)+
  labs(x="Week",
       y="Leaf area index",
       fill="Variety",
       title = "Example 4 - sorghum data (Figure 5)")+
  scale_y_continuous(limits = c(0,6),
                     breaks = c(seq(0,11, by=2)))+
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
#*#
#*#
#*#
##### END #####
#*#
#*#
#*#