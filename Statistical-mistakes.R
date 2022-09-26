# Common Mistakes in Statistics

#~ by Dr. Wolfgang Forstmeier
#~ 2022 09 27

# General Introduction ----
#~ Statistical testing is used to distinguish "significant" patterns from
#~ random noise. P-values are supposed to tell us how often a pattern of a
#~ certain strength or magnitude (i.e. effect size for a given N) arises by 
#~ chance alone. However, in practice, there are many mistakes that one can make 
#~ during data analysis, and many of them will result in anti-conservative 
#~ p-values. Hence, we often believe that we have discovered a real effect, 
#~ but in fact this was just a matter of chance (i.e. we got a false-positive 
#~ finding). According to my experience, anti-conservative p-values (or likewise 
#~ confidence intervals that are too narrow) are a major reason why many 
#~ research findings cannot be replicated between studies. Accordingly, meta 
#~ analyses show high levels of heterogeneity in effect sizes between studies.

#~ Here we will work through the most common and most serious mistakes. If you 
#~ learn to avoid these mistakes, you will less often be fooled by the data, and
#~ will less often waste your time on follow-up studies of phenomena that do not
#~ exist. If you also learn to recognize these mistakes in published studies,
#~ you will also less often get misled by the mistakes of others. More details
#~ can be found here (Forstmeier et al. 2016).

# 0. Set-up -----

#~ Before we can start, we need to install and load some R packages
#~ If the packages are not installed yet, use the 'install.packages'-line by
#~ removing the # in front of it, and then run the line (Run button in RStudio)

# install.packages(c("lme4", "lmerTest", "ggplot2","pedigreemm", "data.table", "magrittr" ))

library(lme4)
library(lmerTest)
library(ggplot2)
library(pedigreemm)
library(data.table)
library(magrittr)


# 1. The issue of multiple testing -----

#~ Introduction to multiple testing ----

#~ Multiple hypothesis testing (and failing to realize that one did numerous
#~ tests before finding something significant) is a major source of false-
#~ positive findings. If you conduct 20 tests, one of them should be significant
#~ at p<0.05 anyway. Hence, it is essential to keep track of how many tests were
#~ conducted, which includes many variants of "giving the data a second chance",
#~ such as adding more data, adding covariates, categorizing variables, 
#~ transforming variables, removing outliers, removing a treatment category etc.
#~ Such "playing with data until reaching significance" is known as p-hacking, 
#~ and this is so well-known (and frowned upon) that we do not need to cover
#~ this here. 

#~ Yet what is sometimes overlooked, is what we called "cryptic multiple
#~ hypothesis testing" during model selection (Forstmeier & Schielzeth 2011). 
#~ Below, you will experience a drastic example, which works so well because the 
#~ initial full model is massively over-fitted (i.e. too many predictors for 
#~ just a few data points). Such over-fitting can be considered a statistical
#~ crime. Yet, it may be "the perfect crime" that leaves no traces, if you do not
#~ honestly report how you arrived at your minimal model. Often people have just
#~ presented the minimal model and have claimed that these results confirm their
#~ initial hypotheses.

#~ Simulations of model selection ----

#~ Let us assume we have 30 observations and 6 predictors

N_OBS = 30
N_MAIN_EFFECTS = 6

# First, we set a seed to make randomly generated data reproducible

set.seed(7)

#~ Now we randomly generate a data.table with N_OBS rows and N_MAIN_EFFECTS + 1
#~ columns. All values are from a normal distribution.

mydata = matrix( data = rnorm( (N_OBS *(N_MAIN_EFFECTS + 1)) ), 
                 nrow = N_OBS, ncol = (N_MAIN_EFFECTS + 1) ) %>% data.table

#~ The first column is the dependent variable Y, the remaining columns are the 
#~ predictors A, B, C...

colnames (mydata) = c("Y", LETTERS[1:N_MAIN_EFFECTS])
mydata

#~ Now we fit a full model that tries to explain Y by the 6 main effects (A-F)
#~ and by the 15 (two-way) interactions.
#~ We then automatically simplify this "full model" by always removing the least
#~ significant term until we get a "minimal model"

source("Fun_Model_Simplifier.R")
Model_Simplifier()

#~ The high significance of the minimal model (p=0.005551) and of its terms 
#~ (up to p=0.000613) is remarkable. 
#~ Let's try this again with different seeds

Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 8)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 9)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 10)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 11)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 12)


#~ Lesson 1 ------
#~ Automatic model simplification comes with a considerable burden of multiple 
#~ testing, especially if the initial full model was over-fitted (N<3k, 
#~ k = number of parameters). Minimal models often look convincing, but the 
#~ problematic history of getting there is often forgotten.
#~ Exploratory testing of many interaction terms is generally discouraged. 
#~ Realistically, one can argue that interaction require 16x more data than main 
#~ effects (https://statmodeling.stat.columbia.edu/2018/03/15/need-16-times-sample-size-estimate-interaction-estimate-main-effect/).
#~ If N is small (e.g. N<30), it is better to test each predictor singly. 


# 2. Pseudoreplication -----

#~ Introduction to pseudoreplication ----
#~ In my opinion, pseudoreplication is the most underestimated problem in the
#~ reproducibility crisis. P-values are calculated under the assumption that 
#~ the data points are independent of each other (unless you specify existing
#~ dependencies in your model). Yet, in reality there are many sources of non-
#~ independence: repeated measures from the same individual, individuals that
#~ are genetically related, samples that are geographically structured, 
#~ observations that are related to each other in time. Failing to account for
#~ such dependencies may mean to overestimate the true sample size and hence 
#~ confidence in the findings. 

# 2A. Getting the basic idea -----

#~ Let's create two samples (groups A and B) of 10 human height measurements 
#~ that are drawn from the same (!) normal distribution of mean 165 and sd 10

set.seed(20220721)

sample1 <- data.frame(
  Group = rep("A", 10),
  IndID = 1:10,
  Height = rnorm(n = 10, mean = 165, sd = 10))
 
sample1 
  
sample2 <- data.frame(
  Group = rep("B", 10),
  IndID = 11:20,
  Height = rnorm(n = 10, mean = 165, sd = 10))

sample2

dataset <- rbind(sample1, sample2)
dataset

boxplot(dataset$Height ~ dataset$Group)

#~ Due to sampling noise, the two groups A and B will never be identical.
#~ Let's test whether the average height differs significantly between A and B

model1 <- lm(Height ~ Group, data = dataset)
summary(model1) 

#~ In the model output, the intercept represents Group A (on average 168.13 cm) 
#~ and group B differs from that by -4.565 cm (being shorter).
#~ Yet, the difference is clearly non-significant (p=0.357).
#~ Now, the researchers decide that there is too much noise in the data, that
#~ prevents them from seeing the truth that B is in fact shorter.
#~ To increase precision, the researchers hence measure each person 10 times.
#~ For now, let's assume that the 10 repeated measures turn out exactly the same.
#~ So we essentially copy the same data 10 times into a large table that 
#~ contains all 20 x 10 = 200 measurements.

dataset10 <- rbind(dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset)
### proper way to write this: data10 <- do.call("rbind", replicate(10, dataset, simplify = FALSE))

#~ Now we test again whether the average height is different between the groups,
#~ with each individual measured 10 times (identical values!).

model2 <- lm(Height ~ Group, data = dataset10)
summary(model2) 

#~ Note that the parameter estimates are still the same (168.13 and -4.565)
#~ but the Standard Errors have become much smaller (higher confidence) 
#~ and hence the same difference of 4.565 cm is judged as highly significant 
#~ (p=0.002). This is because the model assumes that there are N=100 independent
#~ samples in each group, and with such high N, a difference by 4.565 cm would 
#~ not easily arise by chance.

#~ Lesson 2A -----
#~ Note the meaning of the term "pseudoreplication", in contrast to having truly
#~ independent samples only.
#~ The assumption of independence of data points is being violated.
#~ With pseudoreplication, p-values become simply incorrect.
#~ SEs (and CIs) are too small, confidence in the difference is too high.

# 2B. A realistic example with measurement error (replicates are not identical) -----

#~ This time we add a little bit of measurement error to each measurement 
#~ (with a mean of 0 and SD of 0.5 cm)

dataset10$SmallNoise <- rnorm(200, 0, 0.5)
dataset10$HeightWithSmallNoise <- dataset10$Height + dataset10$SmallNoise
View(dataset10)

#~ Again we test whether the average height differs between the groups, 
#~ where each individual was measured 10 times with some error

model3 <- lm(HeightWithSmallNoise ~ Group, data = dataset10)
summary(model3) 

#~ Note that the estimates have changed slightly (compared to model 2) due to
#~ the added noise, but the conclusions are the same (p=0.00199)

#~ Now, in such realistic data sets, we can start specifying how the data points
#~ are non-independent. We specify individual identity (IndID) as a random 
#~ effect in a mixed effect model, thereby allowing each individual to have its
#~ own average phenotype ("random intercepts")

model4 <- lmer(HeightWithSmallNoise ~ Group + (1|IndID), data = dataset10)
summary(model4) 

#~ Note that this gets us back to the correct p-value (p=0.357) 
#~ very close to the one we had before adding replicates. 
#~ Note from the output on random effects that nearly all Variance is explained 
#~ by IndID (116.7704) compared to the Residual (0.2427) which is the 
#~ measurement error that we added to each value of height.

#~ Lesson 2B ----- 
#~ Fitting individual ID as a random intercept takes care of the 
#~ repeated measurements per individual and restores the correct p-value


# 2C. A real example: Is egg mass affected by treatment? -----

#~ This is about fitting random intercepts vs. random slopes. -----
#~ For this we load and inspect some data on egg mass in relation to treatment


d45 <- read.table("dataFig45.txt", sep='\t', header=T)

#~ Female ID and Trt are numeric values that should be considered as factors
d45$Female_ID <-as.factor(d45$Female_ID)
d45$Trt <- as.factor(d45$Trt)

ggplot(data=d45, aes(x=Female_ID, y=Egg_mass, group=Trt, colour=Trt))  +
  geom_point(size=2)+
  labs(x="Female ID", y="Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.text = element_text (size=10), axis.title = element_text (face="bold",size=13)) + 
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")

View(d45)

#~ The plot illustrates that we have 5 eggs per female and two times 6 females
#~ belonging to the two treatment groups.

#~ We want to test whether the treatment had an effect on egg mass.
#~ If we forget that the data is pseudoreplicated, and that we do not have 30
#~ independent eggs of each type, but rather 6 females of each type, the 
#~ following incorrect model shows a significant treatment effect

mod_egg1 <- lm(Egg_mass ~ Trt, data = d45)
summary(mod_egg1)

#~ When we specify Female_ID as a random intercept effect, the significance is
#~ gone. 

mod_egg2 <- lmer(Egg_mass ~ Trt + (1|Female_ID), data = d45)
summary(mod_egg2)

#~ Note the random effects structure of the model. About half of the variance 
#~ is explained by female identity

#~ Maybe we failed to see a treatment effect on egg mass because we ignored 
#~ information about the order in which the 5 eggs of each female were laid.
#~ So let's plot egg size in relation to laying order.

ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Trt, colour = Trt))  +
  geom_point(size=2)+
  geom_smooth(size = 2, method='lm', se = FALSE) +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")

#~ The plot suggests that egg mass was fairly constant over the laying sequence
#~ in the "Reduced" treatment group, while egg mass increased over the sequence
#~ in the "Enhanced" group. Let's test whether the interaction between treatment
#~ and laying sequence is significant!

mod_egg3 <- lmer(Egg_mass ~ Trt*Laying_order + (1|Female_ID), data = d45)
summary(mod_egg3) 

#~ The model suggests that the slopes are significantly different (p=0.0479).
#~ Note the random effects structure: we have accounted for the fact that 
#~ females have different intercepts (i.e. lay eggs of different mean size), but 
#~ is this sufficient, if our interest lies in a slope of change of egg mass 
#~ over the laying sequence?
#~ Let's plot what each of the 12 females did in this respect!

ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Trt, colour = Trt))  +
  geom_point(size=2)+
  geom_smooth(aes(group = Female_ID), method = "lm", se = FALSE) +
  geom_smooth(size = 3, method='lm', se = FALSE) +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")

#~ Seeing it this way, you may wonder whether the 6 slopes in orange are in fact
#~ steeper than the 6 slopes in blue.
#~ Hence, let's extract the 12 regression slopes manually.

#~ We create a data set with 1 row per female, with their appropriate treatment

d45_F <- unique(d45[,c('Female_ID', 'Trt')]) # one line per female 
d45_F$slope <- NA
d45_F

#~ We then calculate the slope for female 1 

mod_F1 <- lm(Egg_mass ~ Laying_order, data = d45[d45$Female_ID == "1",])
summary(mod_F1)
summary(mod_F1)$coeff[2,1]

#~ And we add her value to the data set

d45_F$slope[1] <- summary(mod_F1)$coeff[2,1]
d45_F

#~ Using a loop function, we now do this for each female

for (i in 1:12){
  j <- as.character(i)
  
  mod_F <- lm(Egg_mass ~ Laying_order, data = d45[d45$Female_ID == j,])

  d45_F$slope[i] <- summary(mod_F)$coeff[2,1]
  
}

d45_F

#~ Now, let's compare the female slopes between treatment groups

summary(lm(slope ~ Trt, data = d45_F)) 

#~ This is like doing a t-test of 6 vs 6 slopes, and it is clearly non-
#~ significant (p=0.248) compared to the p=0.0479 we had before.
#~ What was wrong with that random intercept model? 

mod_egg3 <- lmer(Egg_mass ~ Trt*Laying_order + (1|Female_ID), data = d45)
summary(mod_egg3) 

#~ Note that our interest lies in egg mass slopes over the laying sequence 
#~ (whether they increase or decrease), and this is not the same as variation
#~ in intercepts (generally large or small eggs). Given this interest in 
#~ whether the treatment affected those slopes (interaction term), we need to
#~ allow each female to have a random slope of change (as this may be a 
#~ physiological peculiarity that just varies widely between females and that 
#~ is not affected by the treatment).
#~ So let's specify a random slope model! 
#~ While (1|Female_ID) gives each female a random intercept, 
#~ (Laying_order|Female_ID) gives each female a random intercept and a random 
#~ slope over the laying order.

mod_egg4 <- lmer(Egg_mass ~ Trt*Laying_order + (Laying_order|Female_ID), data = d45)
summary(mod_egg4) 

#~ This model shows that the above t-test (p=0.248) was correct, and that the
#~ treatment did not affect the female slopes. 

#~ Lesson 2C: Sometimes pseudoreplication lies in slopes rather than intercepts
#~ i.e. individuals differ not in their mean value of the dependent variable (y)
#~ but rather in how the dependent variable (y) changes in response to another 
#~ variable (X). This variable x may be continuous or just consist of two 
#~ conditions (environment 1 vs. 2). A random slope model accounts for the fact 
#~ that individuals differ in their response to x and this may be necessary to 
#~ consider as soon as you have 3 or more values per individual. More generally,
#~ failing to see the true structure of dependencies yields much spurious
#~ significance. There is a large body of literature reporting significant
#~ treatment by laying order interactions - most likely a huge collection of
#~ false-positive findings.

## 2D. Genetic relatedness -----

#~ Here is an example that shows that controlling for pseudoreplication can be
#~ quite a challenge.
#~ Let's begin by reading and plotting the data

d7 <- read.table("dataFig7.txt", sep='\t', header=T)
head(d7)
tail(d7)

ggplot(d7, aes(x=ESR_10, y=Courtship_rate)) +
  geom_jitter(width=0.25, col="dodgerblue4") +
  xlab("Number of copies of allele ESR1_10")+  ylab("Residual courtship rate")+ 
  theme_classic() + 
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + theme(axis.text = element_text (size=10)) + 

  geom_smooth(method='lm',color = "red") +
  scale_x_continuous(breaks=c(0,1,2), labels=c("0\n\n", "1\nheterozygous", "2\nhomozygous")) 

#~ Here we look at an association between genotype (at the locus of the estrogen
#~ receptor) and phenotype (courtship behaviour) of male zebra finches (N=1556).
#~ Males that have one or two copies of a certain allele sing at a higher rate.
#~ Given the large number of males that have been measured, the effect appears
#~ highly significant.

mod_song1 <- glm (Courtship_rate ~ ESR_10, data = d7)
summary(mod_song1)

#~ However, all these males come from the same captive population (comprising 8
#~ generations), and families in which the ESR1_10 allele is more common may 
#~ just happen to have (other) genes that lead to a high courtship rate.
#~ Let's see whether adding Family_ID as a random effect makes a difference.

mod_song2 <-	lmer (Courtship_rate ~ ESR_10 + (1|Family_ID), data = d7)
summary(mod_song2)

#~ In this model, Family_ID is defined as the joint identities of the parents. 
#~ This random effect has 632 levels, and hence it groups together on average
#~ 2-3 brothers in a family, and indeed this explains some variance (0.7041).
#~ Accordingly, the effect is now estimated to be somewhat smaller and with
#~ larger uncertainty (Std. Error), leading to a larger p-value than before. 

#~ Still, this does not take into account the resemblance between father and 
#~ son, or other forms of relatedness. To get this right, we need to fit the 
#~ entire 8-generations pedigree of this population (comprising 3404 birds).

ped <- read.table("data_ped3404_Seewiesen.txt", header=TRUE, sep="\t", na.strings="NA") 
tail(ped)

#~ Now we use the pedigreemm package to link the pedigree information to the 
#~ data table 

ped_mm <- pedigree(sire = as.character(ped$MID), dam  = as.character(ped$FID), label = as.character(ped$Animal))
all.data <- merge(d7, ped, by.x="Animal", by.y="Animal")
all.data$Animal <- factor(all.data$Animal)

#~ Now we can run the pedigreemm model
#~ Depending on your computer this might take time (3 min on my laptop)

mod_song3 = pedigreemm(Courtship_rate ~ ESR_10 + (1|Animal), pedigree=list(Animal=ped_mm)
                       , data=all.data, verbose=TRUE, na.action=na.exclude
                       , control=lmerControl(check.nobs.vs.nlev="ignore"
                       , check.nobs.vs.nRE="ignore", check.nobs.vs.rankZ = "ignore"))
summary(mod_song3)

#~ You can see that the estimated effect (per copy of ESR1_10) has decreased
#~ further (0.25852, previously 0.32662, initially 0.39830), suggesting that 
#~ more of the variance is attributed to chance differences between families,
#~ and less variance to the ESR1 gene. The standard Error did not increase
#~ further since the previous model, but the t-value has decreased (2.896, 
#~ previously 3.64, initially 4.703), and now corresponds to a p-value of 

2*(1-pnorm(2.896))

#~ about 0.004, which is far less compelling than initially. 
#~ Note the useful 'pnorm'-function that allows you to calculate p-values from 
#~ t-values (assuming infinite degrees of freedom, which is somewhat anti-
#~ conservative). If your N is small, you should instead use something like this

2*(pt(abs(2.896),lower.tail=FALSE,df=20))

#~ A cool side effect of pedigreemm is that you can estimate the heritability
#~ of the trait (courtship rate).

varcorsmod = c(VarCorr(mod_song3)$Animal, attr(VarCorr(mod_song3), "sc")^2)
h2.mod = varcorsmod/sum(varcorsmod)
h2.mod

#~ Hence, we estimate heritability from how much of the total variance can be 
#~ explained by relatedness in the pedigree. Here, this is 36.4%, the remaining
#~ 63.6% is residual variance.

#~ Lesson 2D: Non-independence of data points may sometimes be hard to account 
#~ for completely. Besides relatedness of individuals (causing non-independence 
#~ in heritable traits), there is often spatial or temporal autocorrelation in 
#~ the data. All these dependencies can be modeled, but this is challenging and 
#~ rarely done perfectly. Note that, in the wild, we normally would not have
#~ such pedigree information. Hence p-values often remain anti-conservative, and
#~ this explains in part the difficulties in replicating findings


# 3. Dealing with non-Gaussian data -----

#~ Finally, one should know about the risks of modelling count data.
#~ Good news first: if your data is not normally distributed you can still 
#~ continue as usual (assuming normally distributed, "Gaussian", errors) and be 
#~ pretty safe (see Knief & Forstmeier 2021).
#~ However, if you instead try to model count data with a Poisson error 
#~ distribution you should be aware that this is very risky.

#~ Again we start by reading and plotting some data

d8 <- read.table("dataFig8.txt", sep='\t', header=T)
head(d8)

ggplot(data=d8, aes(x=Exploration_score, y=Latency_min))  +
  geom_point(size=2, col="dodgerblue4")+ ylim(0,600)+ scale_x_continuous(breaks=seq(-1.5,1.5, 0.5))+
  geom_smooth(method='lm', color='red')+
  xlab("Exploration score")+ ylab("Latency to return (min)")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) 

#~ In this example a researcher measured the latency of birds to return to their
#~ nest after a disturbance, in relation to some measure of their exploratory 
#~ behaviour (i.e. a measure of how they behave in some standardized test where 
#~ they explore a new environment). Although the regression line looks very 
#~ flat, the relationship was said to be highly significant. A look into the 
#~ Methods revealed that the latency was modeled as a count of minutes, assuming 
#~ a Poisson error distribution.

mod_latency1 <- glm (Latency_min ~ Exploration_score, data = d8, family = "poisson")
summary(mod_latency1)

#~ Indeed, the negative slope of the regression line appears to be 10.83 SEs 
#~ away from zero (z = -10.83 = -0.16112 / 0.01488)!
#~ This is against all intuition about how significant effects look like.
#~ But wait, I can get this even more significant!
#~ Let's model this as a count of seconds by multiplying the minutes with 60

d8$Latency_seconds <- d8$Latency_min * 60
head(d8)
tail(d8)

mod_latency1s <- glm (Latency_seconds ~ Exploration_score, data = d8, family = "poisson")
summary(mod_latency1s)

#~ Now we are even 83.86 SEs away from zero! What a great tool for producing
#~ significant results!
#~ But let's try to be realistic. We log-transform the dependent variable to 
#~ approach a normal distribution and then use a model with Gaussian errors.

hist(log(d8$Latency_min))

mod_latency2 <- glm (log(Latency_min) ~ Exploration_score, data = d8, family = "gaussian") 
summary(mod_latency2)

#~ The p-value of 0.257 fits much better to the shallow regression line.

#~ The problem with count data is that these usually do not meet the criteria of
#~ a Poisson process. Instead, counts are typically over-dispersed (i.e. with 
#~ more extreme values than expected). Under a Poisson distribution, with a mean
#~ of about 100 min, the data should range from about 75 to 130 min. Cases like
#~ 559 min (top left of the plot) should not occur, and hence are given an 
#~ extreme weight (in terms of evidence), and this gets worse if we take the 
#~ even larger counts in seconds.

#~ To account for the over-dispersion in the count data, there are two ways.
#~ If you have no random effects (hence a glm) you can just write "quasipoisson"
#~ instead of "poisson".

mod_latency3 <- glm (Latency_min ~ Exploration_score, data = d8, family = "quasipoisson")
summary(mod_latency3)

#~ This gives essentially the same p-value (p=0.292) as the Gaussian model above

#~ If you have random effects (hence using glmer) you can just add another 
#~ random effect that has as many level as you have rows in your data sheet.
#~ This is called an "observation-level random effect" (OLRE).
#~ So we add such a column to our data sheet.

d8$ObsvID <- 1:nrow(d8)
head(d8)
tail(d8)

#~ And then we fit this column (ObsvID) as a random effect in our poisson model

mod_latency4 <- glmer (Latency_min ~ Exploration_score + (1|ObsvID), data = d8, family = "poisson")
summary(mod_latency4)

#~ Again, we get a reasonable p-value (p=0.232)

#~ Lesson 3: The use of non-Gaussian models (especially Poisson models) should 
#~ always ring an alarm bell! Has the author accounted for over-dispersion?
#~ Does a Gaussian model (which is fail safe and equally powerful; see Knief and
#~ Forstmeier 2021) yield the same conclusion? 
#~ If not, better don't trust the Poisson model!

# References

#~ Knief, U. and Forstmeier, W. (2021) Violating the normality assumption may be the lesser of two evils. Behavior Research Methods 53: 2576-2590.
#~ Forstmeier, W., Wagenmakers, E.-J. and Parker, T. H. (2017) Detecting and avoiding likely false-positive findings – A practical guide. Biological Reviews 92, 1941-1968.
#~ Forstmeier, W. and Schielzeth, H. (2011) Cryptic multiple hypotheses testing in linear models: overestimated effect sizes and the winner’s curse. Behavioral Ecology Sociobiology 65, 47-55.
#~ Schielzeth, H. and Forstmeier, W. (2009) Conclusions beyond support: Over-confident estimates in mixed-models. Behavioral Ecology 20, 416-420. 

