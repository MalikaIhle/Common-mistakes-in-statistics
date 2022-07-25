# Common Mistakes in Statistics


# 0. Set-up -----

##### install.packages("lme4")
##### install.packages("lmerTest")
##### install.packages("ggplot2")
##### install.packages("pedigreemm")
library(lme4)
library(lmerTest)
library(ggplot2)
library(pedigreemm)


# 1. Multiple Testing -----



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

### Test whether the average height in each group is significantly different
model1 <- lm(Height ~ Group, data = dataset)
summary(model1)

### copy the data 10 times (i.e. exact replicates)
dataset10 <- rbind(dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset, dataset)
#### proper way to write this: data10 <- do.call("rbind", replicate(10, dataset, simplify = FALSE))

### Test whether the average height in each group - with the exact same data replicated 10 times - is significantly different
model2 <- lm(Height ~ Group, data = dataset10)
summary(model2) # the result appear significant!



## B. A more realistic situation: there is a small measurement error (i.e. replicates are not exact) -----

### create a small normally distributed measurement error
dataset10$SmallNoise <- rnorm(200, 0, 0.5)
dataset10$HeightWithSmallNoise <- dataset10$Height + dataset10$SmallNoise
View(dataset10)

### Test whether the average height in each group, with the same data replicated 10 times with small noise, is significantly different
model3 <- lm(HeightWithSmallNoise ~ Group, data = dataset10)
summary(model3) # pseudoreplicated model

### Correct model -> nest value within individuals (= add IndID as a random effet (intercept) in a mixed-effect model)
model4 <- lmer(HeightWithSmallNoise ~ Group + (1|IndID), data = dataset10)
summary(model4) # correct p-value, the IndID explains all the variance



## C. Even more realistic: the measure has a low repeatability (e.g. behavioural measure instead of physical trait) -----

### create two samples (group A and B) of 10 bird aggressiveness measurements drawn from a normal distribution of mean 20 and sd 3
set.seed(20220721)

sample3 <- data.frame(
  Group = rep("A", 10),
  IndID = 1:10,
  Aggressiveness = rnorm(n = 10, mean = 20, sd = 3))

sample3

sample4 <- data.frame(
  Group = rep("B", 10),
  IndID = 11:20,
  Aggressiveness = rnorm(n = 10, mean = 20, sd = 3))

sample4 

dataset_Aggr <- rbind(sample3, sample4)
dataset_Aggr


### Test whether the average height in each group is significantly different
model1_Aggr <- lm(Aggressiveness ~ Group, data = dataset_Aggr)
summary(model1_Aggr)

### copy the data 10 times (i.e. exact replicates)
dataset_Aggr10 <- rbind(dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr, dataset_Aggr)
#### proper way to write this: dataset_Aggr10 <- do.call("rbind", replicate(10, dataset_Aggr, simplify = FALSE))

### Test whether the average height in each group, with the exact same data replicated 10 times, is significantly different
model2_Aggr <- lm(Aggressiveness ~ Group, data = dataset_Aggr10)
summary(model2_Aggr) # the result is significant

### create a large normally distributed measurement error
dataset_Aggr10$LargeNoise <- rnorm(200, 0, 6)
dataset_Aggr10$AggressivenessWithLargeNoise <- dataset_Aggr10$Aggressiveness + dataset_Aggr10$LargeNoise
View(dataset_Aggr10)

### Test whether the average height in each group, with the same data replicated 10 times with small noise, is significantly different
model3_Aggr <- lm(AggressivenessWithLargeNoise ~ Group, data = dataset_Aggr10)
summary(model3_Aggr) # pseudoreplicated model

### Correct model -> nest value within individuals (= add IndID as a random effet (intercept) in a mixed-effect model)
model4_Aggr <- lmer(AggressivenessWithLargeNoise ~ Group + (1|IndID), data = dataset_Aggr10)
summary(model4_Aggr) # correct p-value, the IndID explains all the variance



## D. A real example: Egg Mass -----

### i. Random intercept: Feeding treatment effect on egg mass  -----

##### d45 <- read.table("C://C//R_files//dataFig45.txt", sep='\t', header=T)
d45 <- read.table("dataFig45.txt", sep='\t', header=T)
View(d45)

### Female ID and Trt are numeric values that should be considered as factors (i.e. categories) not as numbers
d45$Female_ID <-as.factor(d45$Female_ID)
d45$Trt <- as.factor(d45$Trt)

### plot of the mass of each of the ~5 eggs of each female 
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

### Plots with raw data, 1 color per Female
ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Female_ID, colour = Female_ID))  +
  geom_point(size=2)+
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "deepskyblue",
                                 "2" = "dodgerblue", 
                                 "3" = "blue", 
                                 "4" = "blue4", 
                                 "5" = "darkslateblue", 
                                 "6" = "steelblue", 
                                 "7" = "orange", 
                                 "8" = "darkorange", 
                                 "9" = "darkorange1", 
                                 "10" = "darkorange2", 
                                 "11" = "darkorange3", 
                                 "12" = "darkorange4"), name="Female_ID")


### Plots with raw data, 1 color per Female, 1 slope per female
ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Female_ID, colour = Female_ID))  +
  geom_point(size=2)+
  geom_smooth(aes(group = Female_ID), method = "lm", se = FALSE) +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "deepskyblue",
                                 "2" = "dodgerblue", 
                                 "3" = "blue", 
                                 "4" = "blue4", 
                                 "5" = "darkslateblue", 
                                 "6" = "steelblue", 
                                 "7" = "orange", 
                                 "8" = "darkorange", 
                                 "9" = "darkorange1", 
                                 "10" = "darkorange2", 
                                 "11" = "darkorange3", 
                                 "12" = "darkorange4"), name="Female_ID")

### Plots with raw data, 1 color per Trt, 1 slope per female
ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Trt, colour = Trt))  +
  geom_point(size=2)+
  geom_smooth(aes(group = Female_ID), method = "lm", se = FALSE) +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")


### Plots with raw data, 1 color per Trt, 1 slope per female + 1 large average slope per Trt
ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = Trt, colour = Trt))  +
  geom_point(size=2)+
  geom_smooth(aes(group = Female_ID), method = "lm", se = FALSE) +
  geom_smooth(size = 5, method='lm', se = FALSE) +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")


### Plots with raw data, 1 color per Trt, average slope per Trt and SE
ggplot(data=d45, aes(x = Laying_order,  y = Egg_mass, group = c(Trt), colour = c(Trt)))  +
  geom_point(size=2)+
  geom_smooth(method='lm') +
  xlab("Laying order")+ ylab("Egg mass")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) +
  scale_colour_manual(values = c("1" = "blue","2" = "orange"),labels=c("Reduced", "Enhanced"), name="Treatment")



### Compare the slopes of the females between treatment groups
#### warning: not something to actually do as you loose variance from the raw data (each individual egg value)
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




## E. Genetic relatedness -----

##### d7 <- read.table("C://C//R_files//dataFig7.txt", sep='\t', header=T)
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


# incorrect model
mod_song1 <- glm (Courtship_rate ~ ESR_10, data = d7)
summary(mod_song1)

# deceptive model
####mod_song2 <-	lmer (Courtship_rate ~ ESR_10 + (1|family_ID))
####summary(mod_song2)

# correct model
####mod_song3 <-pedigreemm (Courtship_rate ~ ESR_10 + (1|animal))




# 3. Dealing with non-Gaussian data -----

#d8 <- read.table("C://C//R_files//dataFig8.txt", sep='\t', header=T)
d8 <- read.table("dataFig8.txt", sep='\t', header=T)
head(d8)

ggplot(data=d8, aes(x=Exploration_score, y=Latency))  +
  geom_point(size=2, col="dodgerblue4")+xlim(c(-1.65,1.73))+ ylim(0,600)+scale_x_continuous(breaks=seq(-1.5,1.5, 0.5))+
  geom_smooth(method='lm', color='red')+
  xlab("Exploration score")+ ylab("Latency to return (min)")+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  theme(axis.title = element_text (face="bold",size=13)) + 
  theme(axis.text = element_text (size=10)) 


# consider number of seconds as a count of seconds, i.e. a Poisson distribution
mod_latency1 <- glm (Latency ~ Exploration_score, data = d8, family = "poisson")
summary(mod_latency1)


# transform latency to approximate a Gaussian distribution
mod_latency2 <- glm (log(Latency) ~ Exploration_score, data = d8, family = "gaussian")
summary(mod_latency2)

# BTW, this is equal to:
mod_latency2bis <- lm (log(Latency) ~ Exploration_score, data = d8)
summary(mod_latency2bis)


# correct: use a quasi poisson distribution
mod_latency3 <- glm (Latency ~ Exploration_score, data = d8, family = "quasipoisson")
summary(mod_latency3)


# correct: add an overdispersion parameter (observation-level random effect) to a poisson model

d8$IndID <- 1:nrow(d8)
head(d8)
tail(d8)

mod_latency4 <- glmer (Latency ~ Exploration_score + (1|IndID), data = d8, family = "poisson")
summary(mod_latency4)






