# Common Mistakes in Statistics

## General Introduction ----
## Statistical testing 

# 0. Set-up -----

##### install.packages(c("lme4", "lmerTest", "ggplot2","pedigreemm", "data.table", "magrittr" ))

library(lme4)
library(lmerTest)
library(ggplot2)
library(pedigreemm)
library(data.table)
library(magrittr)
#source()


# 1. Multiple Testing -----

# Let us assume we have 30 observations and 6 predictors (this can be changed here to play)
N_OBS = 30
N_MAIN_EFFECTS = 6

# First we set a seed to make randomly generated data reproducible
set.seed(7)

# Now we randomly generate a data.table with N_OBS rows and N_MAIN_EFFECTS +1 columns, all values are from a normal distribution
mydata = matrix( data = rnorm( (N_OBS *(N_MAIN_EFFECTS + 1)) ), 
                 nrow = N_OBS, ncol = (N_MAIN_EFFECTS + 1) ) %>% data.table
# The first column is the dependent variable Y, the remaining columns are the predictors A, B, C...
colnames (mydata) = c("Y", LETTERS[1:N_MAIN_EFFECTS])
mydata

# Now we fit a full model that tries to explain Y by the 6 main effects (A-F) and the 15 (two-way) interactions
# and the we automatically simplify this by always removing the least significant term until we get a minimal model

source("Fun_Model_Simplifier.R")
Model_Simplifier()


# Let's try this again with different seeds------

Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 8)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 9)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 10)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 11)
Model_Simplifier(N_OBS = 30, N_MAIN_EFFECTS = 6, MYSEED = 12)

## Lesson 1: Automatic model simplification comes with a considerable burden of multiple testing ------
## especially if the initial full model was overfitted (N<3k, k = number of parameters)
## Minimal models often look convincing, but the problematic history of getting there is often forgotten
## Exploratory testing for interaction terms is generally discouraged 
## Realistically, interaction require 16x more data than main effects
## If N<8k, often better to test each predictor singly 


# 2. Pseudoreplication -----


## A. Get the basic idea -----

### create two samples (group A and B) of 10 human height measurements drawn from a normal distribution of mean 165 and sd 10
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

boxplot(dataset$Height ~ dataset$Group) ### plot with all data points, mean, visualize noise

### Test whether the average height differs significantly between groups A and B
model1 <- lm(Height ~ Group, data = dataset)
summary(model1) # the difference is clearly non-significant (p=0.357)

### copy the data 10 times (i.e. exact replicates)
dataset10 <- rbind(dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset)
#### proper way to write this: data10 <- do.call("rbind", replicate(10, dataset, simplify = FALSE))

### Test whether the average height is now different between the groups
### with each individual measured 10 times (identical values!)
model2 <- lm(Height ~ Group, data = dataset10)
summary(model2) # the difference appears highly significant (p=0.002)

## Lesson 2: With pseudoreplication, p-values become incorrect
## (assumption of independence of data points is being violated)
## SEs (and CIs) are too small, confidence in the difference is too high

## B. A more realistic situation: there is a small measurement error (i.e. replicates are not exact) -----

### create a small normally distributed measurement error
dataset10$SmallNoise <- rnorm(200, 0, 0.5)
dataset10$HeightWithSmallNoise <- dataset10$Height + dataset10$SmallNoise
View(dataset10)

### Test whether the average height differs between the groups, with the same data replicated 10 times with small noise
model3 <- lm(HeightWithSmallNoise ~ Group, data = dataset10)
summary(model3) # the difference again appears highly significant (p=0.002); practically identical to model2

### Correct model -> nest value within individuals (= add IndID as a random effect (intercept) in a mixed-effect model)
model4 <- lmer(HeightWithSmallNoise ~ Group + (1|IndID), data = dataset10)
summary(model4) # correct p-value, the IndID explains most of the variance

## Lesson 3: Fitting individual ID as a random intercept takes care of the repeated measurements per individual
## and restores the p-value we had before (p=0.357)


## C. A real example: Egg Mass -----

### i. Random intercept: Feeding treatment effect on egg mass  -----

d45 <- read.table("dataFig45.txt", sep='\t', header=T)
View(d45)

### Female ID and Trt are numeric values that should be considered as factors (i.e. categories) not as numbers
d45$Female_ID <-as.factor(d45$Female_ID)
d45$Trt <- as.factor(d45$Trt)

### plot of the mass of each of the 5 eggs of each female 
ggplot(data=d45, aes(x=Female_ID, y=Egg_mass, group=Trt, colour=Trt))  +
  geom_point(size=2)+
  labs(x="Female ID", y="Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.text = element_text (size=10), axis.title = element_text (face="bold",size=13)) + 
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")

### Test whether feeding treatment has an effect on egg mass
#### pseudoreplicated
mod_egg1 <- lm(Egg_mass ~ Trt, data = d45)
summary(mod_egg1)

#### correct
mod_egg2 <- lmer(Egg_mass ~ Trt + (1|Female_ID), data = d45)
summary(mod_egg2)



### ii. Random slopes: Laying order effect on egg mass ------

### Plots with raw data, 1 color per Trt, 1 slope per female + 1 large average slope per Trt
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

### Model on raw data (1 line per egg) with random intercept only
mod_egg3 <- lmer(Egg_mass ~ Trt*Laying_order + (1|Female_ID), data = d45)
summary(mod_egg3) # significant interaction (i.e. effect of trt on slope 'egg mass over laying order' significant)


### Plots with raw data, 1 color per Trt, 1 slope per female + 1 large average slope per Trt
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


### Compare the slopes of the females between treatment groups
#### warning: not something to actually do as you lose variance from the raw data (each individual egg value)
#### create a dataset with 1 row per female, with their appropriate treatment

d45_F <- unique(d45[,c('Female_ID', 'Trt')]) # one line per female 
d45_F$slope <- NA
d45_F

##### get the slope for female 1
mod_F1 <- lm(Egg_mass ~ Laying_order, data = d45[d45$Female_ID == "1",])
summary(mod_F1)
summary(mod_F1)$coeff[2,1]

d45_F$slope[1] <- summary(mod_F1)$coeff[2,1]
d45_F

#### get the slope for each female
for (i in 1:12){
  j <- as.character(i)
  
  mod_F <- lm(Egg_mass ~ Laying_order, data = d45[d45$Female_ID == j,])

  d45_F$slope[i] <- summary(mod_F)$coeff[2,1]
  
}

d45_F

#### compare the female slopes between treatment groups
summary(lm(slope ~ Trt, data = d45_F)) # Trt effect on slope not significant



### Model on raw data (1 line per egg) with random intercept only
mod_egg3 <- lmer(Egg_mass ~ Trt*Laying_order + (1|Female_ID), data = d45)
summary(mod_egg3) # significant interaction (i.e. effect of trt on slope 'egg mass over laying order' significant)


### Model with random intercept and random slope (each female may have different slope of egg mass along laying order)
mod_egg4 <- lmer(Egg_mass ~ Trt*Laying_order + (Laying_order|Female_ID), data = d45)
summary(mod_egg4) # non-significant interaction, equivalent to the t test (lm) above

## Lesson 4: Sometimes pseudoreplication lies in slopes rather than intercepts
## i.e. individuals differ not in their mean value of the dependent variable (y)
## but rather in how the dependent variable (y) changes in response to another variable (X)
## This variable x may be continuous or just consist of two classes (treatment A vs B)
## A random slope model accounts for the fact that individuals differ in their response to x
## and this may be necessary as soon as you have 3 or more values per individual


## D. Genetic relatedness -----

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


### incorrect model
mod_song1 <- glm (Courtship_rate ~ ESR_10, data = d7)
summary(mod_song1)

### deceptive model
mod_song2 <-	lmer (Courtship_rate ~ ESR_10 + (1|Family_ID), data = d7)
summary(mod_song2)

### correct model : include pedigree
#### get pedigrees
ped <- read.table("data_ped3404_Seewiesen.txt", header=TRUE, sep="\t", na.strings="NA") 

#### get phenotypes
phenos <- read.table("dataFig7.txt", header=TRUE, sep="\t")

#### prepare data 
##### calculate FPed via pedigreemm
ped_mm <- pedigree(sire = as.character(ped$MID), dam  = as.character(ped$FID), label = as.character(ped$Animal))
FPed <- inbreeding(ped_mm)
ped$FPed <- FPed
all.data <- merge(phenos, ped, by.x="Animal", by.y="Animal")

##### some basic formatting
all.data$Animal <- factor(all.data$Animal)


##### model

mod_song3 = pedigreemm(Courtship_rate ~ ESR_10 + (1|Animal), pedigree=list(Animal=ped_mm)
                       , data=all.data, verbose=TRUE, na.action=na.exclude
                       , control=lmerControl(check.nobs.vs.nlev="ignore"
                       , check.nobs.vs.nRE="ignore", check.nobs.vs.rankZ = "ignore"))
summary(mod_song3)
varcorsmod = c(VarCorr(mod_song3)$Animal, attr(VarCorr(mod_song3), "sc")^2)
h2.mod = varcorsmod/sum(varcorsmod)
h2.mod

## Lesson 5: Non-independence of data points may sometimes be hard to account for completely
## Besides relatedness of individuals (causing non-independence in heritable traits)
## there is often spatial or temporal autocorrelation in the data
## All these dependencies can be modeled, but this is challenging and rarely done perfectly
## Hence p-values often remain anti-conservative, and this explains in part the difficulties in replicating findings


# 3. Dealing with non-Gaussian data -----

#d8 <- read.table("C://C//R_files//dataFig8.txt", sep='\t', header=T)
d8 <- read.table("dataFig8.txt", sep='\t', header=T)
head(d8)

ggplot(data=d8, aes(x=Exploration_score, y=Latency_min))  +
  geom_point(size=2, col="dodgerblue4")+xlim(c(-1.65,1.73))+ ylim(0,600)+scale_x_continuous(breaks=seq(-1.5,1.5, 0.5))+
  geom_smooth(method='lm', color='red')+
  xlab("Exploration score")+ ylab("Latency to return (min)")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) 


# consider number of minutes as a count of minutes, i.e. a Poisson distribution
mod_latency1 <- glm (Latency_min ~ Exploration_score, data = d8, family = "poisson")
summary(mod_latency1)

## same model in seconds!
d8$Latency_seconds <- d8$Latency_min * 60
head(d8)
tail(d8)

mod_latency1sec <- glm (Latency_seconds ~ Exploration_score, data = d8, family = "poisson")
summary(mod_latency1sec)


# transform latency to approximate a Gaussian distribution
mod_latency2 <- glm (log(Latency_min) ~ Exploration_score, data = d8, family = "gaussian") # log in R = Ln
summary(mod_latency2)

# BTW, this is equal to:
mod_latency2bis <- lm (log(Latency_min) ~ Exploration_score, data = d8)
summary(mod_latency2bis)


# correct: use a quasi poisson distribution
mod_latency3 <- glm (Latency_min ~ Exploration_score, data = d8, family = "quasipoisson")
summary(mod_latency3)


# also correct: add an overdispersion parameter (observation-level random effect) to a poisson model

d8$ObsvID <- 1:nrow(d8)
head(d8)
tail(d8)

mod_latency4 <- glmer (Latency_min ~ Exploration_score + (1|ObsvID), data = d8, family = "poisson")
summary(mod_latency4)

## Lesson 6: The use of non-Gaussian models (especially Poisson models) should always ring an alarm bell!
## Has the author accounted for overdispersion?
## Does a Gaussian model (fail safe and equally powerful) yield the same conclusion?
## If not, better don't trust the Poisson model!



