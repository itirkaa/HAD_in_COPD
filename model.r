# AUTHOR: Aakriti Sharma
# DATE: 28 May 2020


# 1. Research Question: ----

# 1.1 Population of interest: ----
## People living with Chronic Obsrtuctive Pulmonary Disease (COPD)

# 1.2 Outcomes/Goal: ----
## To find out Hospital Anxiety and Depression Scale (HAD)
## in the given population

# 1.3 Purpose: ----
## To evaluate the effects of smoking and 
## understanding the effects of various other characteristics on our target value

# 1.4 Candidate variables for Inclusion in the model: ----
## Various candidate variables are
## AGE, PackHistory, COPDSEVERITY,
## FEV1(Forced expiratory volume), FVC (Forced vital Capacity), CAT,
## SGRQ(Quality of life), AGEquartiles, copd, 
## gender, smoking, Diabetes, muscular, hypertension, AtrialFib, IHD

# 1.5 Considered variables for Inclusion: ----
## AQEquartiles, Gender, smoking, PackHistory, SGRQ, lung funtion
## copdSeverity, copd, and other comorbidities, hypertension


# 2. Importing Libraries ----
library(Hmisc)
library(ggplot2)
library(gmodels)
library(missForest)


# 3. Importing dataset ----
copdData <- read.csv('dataset/COPD_student_dataset.csv')
head(copdData)
colnames(copdData)
dim(copdData)

# 4. Inspecting Data ----
describe(copdData)

# 4.1 Class of candidate attributes and missing values ----
class(copdData$HAD) # numeric
sum(is.na(copdData$HAD))
summary(copdData$HAD)

class(copdData$AGE) # integer
sum(is.na(copdData$AGE))
summary(copdData$AGE)

class(copdData$gender) # integer
copdData$gender <- as.factor(copdData$gender) # changing to factor
sum(is.na(copdData$gender))
summary(copdData$gender)

class(copdData$smoking) # integer
copdData$smoking <- as.factor(copdData$smoking)
sum(is.na(copdData$smoking))
summary(copdData$smoking)

class(copdData$PackHistory) # numeric
sum(is.na(copdData$PackHistory))
summary(copdData$PackHistory)

class(copdData$copd) # integer
sum(is.na(copdData$copd))
copdData$copd <- as.factor(copdData$copd)
summary(copdData$copd)

class(copdData$COPDSEVERITY) # factor
sum(is.na(copdData$COPDSEVERITY))
copdData$COPDSEVERITY <- as.factor(copdData$COPDSEVERITY)
summary(copdData$COPDSEVERITY)

class(copdData$SGRQ) # numeric
sum(is.na(copdData$SGRQ))
summary(copdData$SGRQ)

class(copdData$CAT) # integer
sum(is.na(copdData$CAT))
summary(copdData$CAT) # value should not be more than 40

class(copdData$FEV1) # numeric
sum(is.na(copdData$FEV1))
summary(copdData$FEV1)

class(copdData$MWT1Best)
sum(is.na(copdData$MWT1Best))
summary(copdData$FEV1)

class(copdData$hypertension) # integer
sum(is.na(copdData$hypertension))
copdData$hypertension <- as.factor(copdData$hypertension)
summary(copdData$hypertension)

# 4.2 Removing and Imputing erroneous values ----
## Note HAD scale goes up to 21 only but max value is 56
## Replacing values greater that 21 with NA
copdData$HAD[copdData$HAD > 21] <- NA
# Imputing values greater than 21 and less than 30 with 21
# copdData$HAD[copdData$HAD > 21] <- 21
# Summary after replacement
summary(copdData$HAD)

## Similar to HAD the CAT Score doesn't go above 40
## So, we'll replace values above 40 with NA
copdData$CAT[copdData$CAT > 40] <- NA
summary(copdData$CAT)
hist(copdData$CAT)
copdData$CAT <- as.numeric(impute(copdData$CAT))
summary(copdData$CAT)

## Missing values in MWT1Best (I missing value)
summary(copdData$MWT1Best)
hist(copdData$MWT1Best)
copdData$MWT1Best <- as.numeric(impute(copdData$MWT1Best))
summary(copdData$MWT1Best)

summary(copdData$MWT1)
hist(copdData$MWT1)
copdData$MWT1 <- as.numeric(impute(copdData$MWT1))
summary(copdData$MWT1Best)

summary(copdData$MWT2)
hist(copdData$MWT2)
copdData$MWT2 <- as.numeric(impute(copdData$MWT2))
summary(copdData$MWT2)


# 4.3 Histograms ----
ggplot() + geom_histogram(data = copdData, aes(x=HAD),
                          fill = 'pink', col = 'grey') +
    theme_minimal()

ggplot() + geom_histogram(data = copdData, aes(x=AGE), bins = 20,
                          fill = 'pink', col = 'grey') +
    theme_minimal()

ggplot() + geom_histogram(data = copdData, aes(x=MWT1Best), bins = 20,
                          fill = 'pink', col = 'grey') +
    theme_minimal()

ggplot() + geom_histogram(data = copdData, aes(x=CAT), bins = 20,
                          fill = 'pink', col = 'grey') +
    theme_minimal() # Not normally distributed 

ggplot() + geom_histogram(data = copdData, aes(x=SGRQ),
                          fill = 'pink', col = 'grey') +
    theme_minimal()

ggplot() + geom_histogram(data = copdData, aes(x=FEV1),
                          fill = 'pink', col = 'grey') +
    theme_minimal()


# 4.4 Examining Categorical values ----
CrossTable(copdData$copd) # Narrow distributions for level 4, so we'll merge it with level 3 group
copdData$copd[copdData$copd == '4'] <- '3'
#CrossTable(copdData$COPDSEVERITY) #
CrossTable(copdData$gender)
CrossTable(copdData$smoking)

# 5. Examining relationship between candidate predictor variables ----
## Making new attribute called comorbidity
comorbid <- length(copdData$Diabetes)

comorbid[copdData$Diabetes == 1 | copdData$muscular == 1 |
             copdData$hypertension == 1 | copdData$AtrialFib == 1 |
             copdData$IHD == 1] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- as.factor(comorbid)
copdData$comorbid <- comorbid

# 5.1 Pearson's correlation test ----
## Selecting candidate columns
candidateData <- copdData[, c("AGE", "PackHistory", "FEV1", 
                              "FVC", "CAT", "SGRQ", "MWT1Best")]
cor(candidateData)
## Plotting pair plot
pairs(candidateData)

## We can observe FEV1 and FVC are highly cor-related and,
## CAT and SGRQ are highly correlated
## Also MWT1Best is correlated to CAT, SGRQ, FVC and FEV, so we won't consider it

# 5.2 Coorelation between Categorical values ----
CrossTable(copdData$smoking, copdData$comorbid)
## We can observe that people who smoke are also showing presence of comorbidities
chisq.test(copdData$comorbid, copdData$smoking)
## The test has p-value of 0.22 which is not significant 
## So, we can say that the relationship between smoking and comorbidities does not exist

CrossTable(copdData$copd, copdData$smoking) # There seems to be very little relationship between the two
chisq.test(copdData$copd, copdData$smoking)


# 6. Comparing Predictors with target value ----
had_age <- lm(HAD~AGE, data = copdData)
summary(had_age) # Multiple R-squared:  0.02817,	Adjusted R-squared:  0.01713 

had_smoking <- lm(HAD~smoking, data = copdData) 
summary(had_smoking) # Multiple R-squared:  0.01232,	Adjusted R-squared:  0.0011 

had_comorbid <- lm(HAD~comorbid, data = copdData)
summary(had_comorbid) # Multiple R-squared:  0.04316,	Adjusted R-squared:  0.03229 
## Note: comorbid is able to explain more variance than smoking

had_copd <- lm(HAD~copd, data = copdData)
summary(had_copd) # Multiple R-squared:  0.08766,	Adjusted R-squared:  0.06669 

had_ph <- lm(HAD~PackHistory, data = copdData)
summary(had_ph) # Multiple R-squared:  0.0002534,	Adjusted R-squared:  -0.01111 
## Very low R square value

had_fev1 <- lm(HAD~FEV1, data = copdData)
summary(had_fev1) # Multiple R-squared:  0.04667,	Adjusted R-squared:  0.03584 

had_fvc <- lm(HAD~FVC, data = copdData)
summary(had_fvc) # Multiple R-squared:  0.01139,	Adjusted R-squared:  0.0001582 
## FEV1 explains more variance than fvc

had_cat <- lm(HAD~CAT, data = copdData)
summary(had_cat) # Multiple R-squared:  0.3095,	Adjusted R-squared:  0.3017 
## R square is high
## Checking correlation now
plot(copdData$CAT, copdData$HAD)
cor.test(copdData$CAT, copdData$HAD)
# We can say the two attributes are linearly co-related(moderate)

had_sgrq <- lm(HAD~SGRQ, data = copdData)
summary(had_sgrq) # Multiple R-squared:  0.3035,	Adjusted R-squared:  0.2956 
plot(copdData$SGRQ, copdData$HAD)
cor.test(copdData$SGRQ, copdData$HAD)
# SINCE SGRQ AND CAT ARE CO-RELATED, THUS WE'LL CHOSE 
# CAT FOR FINAL MODEL BASED OF R VALUE

had_mwt1best <- lm(HAD~MWT1Best, data = copdData)
summary(had_mwt1best) # Multiple R-squared:  0.1159,	Adjusted R-squared:  0.1059 
plot(copdData$MWT1Best, copdData$HAD)
cor.test(copdData$MWT1Best, copdData$HAD) # Negative co-relation
# MWT1BEST IS CO-RELATED WITH CAT AND SGRQ
# THUS WE'LL DROP IT

# 7. MULTIVARIATE REGRESSION ----
m1 <- lm(HAD~AGE+smoking+(AGE*smoking), data = copdData)
summary(m1) # Multiple R-squared:  0.06185,	Adjusted R-squared:  0.02912 

m2 <- lm(HAD~CAT+comorbid, data = copdData)
summary(m2) # Multiple R-squared:  0.3271,	Adjusted R-squared:  0.3116 

m3 <- lm(HAD~CAT+AGE+copd+(AGE*copd)+smoking+(PackHistory*smoking)+PackHistory, data = copdData)
summary(m3)

final_model <- m3

