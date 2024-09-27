library(ggplot2)
library(mgcv) 

# Exploratory Data Analysis ####################################################################################################

ReferendumResults <- read.csv("/Users/wongzhiyuan/Downloads/ReferendumResults.csv", stringsAsFactors=TRUE)
View(ReferendumResults)

# We replace missing values in the 'Leave' column that are indicated by 
# -1 with NAs
ReferendumResults["Leave"][ReferendumResults["Leave"] == -1] <- NA

# As our model will predict the proportion of leave votes, we create a new
# variable in the dataset, LeaveProp, that represents the proportion of
# leave votes
LeaveProp <- ReferendumResults$Leave/ReferendumResults$NVotes
ReferendumResults <- cbind(ReferendumResults, LeaveProp)

# We create 3 new variables to represent the proportion of 'young' voters
# (voters aged 18-29), the proportion of 'working age' voters (age 30-64)
# and the proportion of 'retirement age' voters (age 65+)
Young <- ReferendumResults$Age_18to19 + ReferendumResults$Age_20to24 +
  ReferendumResults$Age_25to29
WorkingAge <- ReferendumResults$Age_30to44 + ReferendumResults$Age_45to59 +
  ReferendumResults$Age_60to64
RetirementAge <- ReferendumResults$Age_65to74 + ReferendumResults$Age_75to84 +
  ReferendumResults$Age_85to89 + ReferendumResults$Age_90plus
ReferendumResults <- cbind(ReferendumResults, Young, WorkingAge, RetirementAge)

# We create a new variables to represent the % of households renting by adding
# the two renting-related covariates together. We also create another variable
# representing the % of households owning their accommodation with a mortgage
# by substracting OwnedOutright from Owned
Rent <- (ReferendumResults$SocialRent+ReferendumResults$PrivateRent)
OwnedM <- (ReferendumResults$Owned-ReferendumResults$OwnedOutright)
ReferendumResults <- cbind(ReferendumResults, Rent, OwnedM)

# We create 2 new variables to represent the % of households in social grade
# C2 by subtracting DE from C2DE. We also create another variable representing
# the % of households in social grade C1 by subtracting C2DE from C1C2DE
C2 <- ReferendumResults$C2DE - ReferendumResults$DE
C1 <- ReferendumResults$C1C2DE - ReferendumResults$C2DE
ReferendumResults <- cbind(ReferendumResults, C1, C2)

ReferendumResults <- ReferendumResults[order(ReferendumResults$ID),]

# We divide the wards into their respective groups, Group 1 and Group 2. 
# Group 1 consists of the wards with the number of leave votes
# known.
RefGr1 <- ReferendumResults[!is.na(ReferendumResults$Leave),]

# As we will be using the proportion of 'Leave' votes for Group 1
# frequently, we give it a name
LeavePGr1 <- RefGr1$LeaveProp

# As we will be repeatedly plotting covariates against the proportion of
# leave votes, we create a function to achieve this
LeavePlot <- function(covariates)
  for (i in covariates) {
    
    # covariates: a list of the names of the covariates corresponding to the 
    # Referendum Results dataset that we would like to plot against the 
    # proportion of 'Leave' votes
    
    xcovariate <- RefGr1[,i]
    plot(xcovariate, y= RefGr1$LeaveProp, xlab= paste("% of", i, "Residents"), 
         ylab= "Proportion of 'Leave' Votes", pch= 20)
  }

# We calculate the summary statistics and plot a boxplot of the proportion
# of leave votes where the blue point represents the mean proportion of 'Leave'
# votes
cat("The summary statistics for the proportion of 'Leave' votes is\n")
print(summary(LeavePGr1))
cat("====================\n")

par(mfrow=c(1,1))
boxplot(LeavePGr1, main= "Proportion of 'Leave' Votes")
points(mean(LeavePGr1), col= "blue", pch= 20)

# We plot the education-related covariates against the proportion of
# leave votes
par(mfrow=c(2,2))
educovariates <- c("NoQuals", "L1Quals", "L4Quals_plus") 
LeavePlot(educovariates)

# We will be plotting related covariates against each other, as well as 
# calculating their correlation. This is so we're aware of how correlated
# certain covariates are with each other.
CorrPlot <- function(corrcovariates){
  
  # CorrPlot is a function that plots the given list of covariates from
  # the ReferendumResults dataset against each other
  # against each other 
  
  # corrcovariates: a list of the names of the covariates corresponding to the 
  # ReferendumResults dataset that we would like to plot against each other
  
  refcovariate <- subset(ReferendumResults, select= corrcovariates)
  plot(refcovariate, pch= 20)
  
}

CorrStatement <- function(covariate1, covariate2){
  
  # CorrStatement calculates the correlation between the two given covariates
  # from the ReferendumResults dataset and prints out a statement stating this
  # correlation 
  
  # covariate1 and covariate2 represents the two covariates corresponding to the
  # referendum dataset we would like to find the correlation of
  
  refcov1 <- subset(ReferendumResults, select= covariate1)
  refcov2 <- subset(ReferendumResults, select= covariate2)
  
  cat("The correlation between", covariate1, "and", 
      covariate2, "is", round(cor(refcov1, refcov2), 3),"\n")
  
}

# We plot the education-related covariates against each other to observe the
# correlation between covariates. In addition to this, we calculate the 
# correlation between education related covariates 
CorrPlot(educovariates)
cat("Correlation between education-related covariates\n")

CorrStatement("L4Quals_plus", "L1Quals")
CorrStatement("L4Quals_plus", "NoQuals")
CorrStatement("L1Quals", "NoQuals")
cat("====================\n")

# We plot the ethnicity-related covariates against the proportion of 'Leave'
# votes
par(mfrow=c(3,2))
par(mar=c(4,4,4,4))
ethnicitycov <- c("White", "Black", "Asian", "Indian", "Pakistani") 
LeavePlot(ethnicitycov)

# We create a function that transforms covariates and plots them against
# the proportion of 'Leave' votes. The transformations used in the function
# are the most commonly used transformations that we expect to use
TransformPlot <- function(transformcov){
  par(mfrow=c(2,2))
  xcov <- RefGr1[,transformcov]
  plot(x= log(xcov + 1), y=RefGr1$LeaveProp, ylab= "Proportion of Leave Votes", 
       xlab= paste("% of log(",transformcov," + 1) Residents"), pch= 20)
  plot(x= sqrt(xcov), y=RefGr1$LeaveProp, ylab= "Proportion of Leave Votes", 
       xlab= paste("% of sqrt(",transformcov," Residents"), pch= 20)
  plot(x= 1/(xcov + 1), y=RefGr1$LeaveProp, ylab= "Proportion of Leave Votes", 
       xlab= paste("% of 1/(",transformcov," + 1) Residents"), pch= 20)
  plot(x= (xcov)^2, y=RefGr1$LeaveProp, ylab= "Proportion of Leave Votes", 
       xlab= paste("% of (",transformcov,")^2 Residents"), pch= 20)
}

# We transform the ethnicity covariates to break up the cluster of points in 
# their plots against the response variable. We also transform the ethnicity
# covariates in an attempt to make the relationship with the response variable 
# more linear 
par(mar= c(5.1, 4.1, 4.1, 2.1))
TransformPlot("White")
TransformPlot("Black")
TransformPlot("Asian")
TransformPlot("Indian")
TransformPlot("Pakistani")

# We plot the ethnicity-related covariates against each other to observe their
# relationship with each other. We also calculate the correlation between
# these covariates. In this case, we will only consider the 'White', 'Black' 
# and 'Asian' covariates because we consider 'Asian', 'Indian', and 'Pakistani'
# as similar covariates.
ethnicitycov3 <- c("White", "Black", "Asian")
CorrPlot(ethnicitycov3)

cat("Correlation between ethnicity-related covariates:\n")
CorrStatement("White", "Black")
CorrStatement("Black", "Asian")
CorrStatement("White", "Asian")
cat("====================\n")

# We plot each age group and the proportion of 'Leave' votes, We also plot
# the AdultMeanAge against the proportion of 'Leave' votes
par(mfrow=c(2,2))
agecov <- c("Young", "WorkingAge", "RetirementAge") 
LeavePlot(agecov)
plot(RefGr1$AdultMeanAge, LeavePGr1, pch=20, xlab="Adult Mean Age",
     ylab= "Proportion of 'Leave' votes")

# We plot the age group covariates each other and calculate their correlation
CorrPlot(agecov)

cat("Correlation between age-related covariates:\n")
CorrStatement("Young", "WorkingAge")
CorrStatement("Young", "RetirementAge")
CorrStatement("WorkingAge", "RetirementAge")
cat("====================\n")

# We transform the ''WorkingAge' and 'Young' covariates to break the cluster
# of points that exist in these plots. We also transform these covariates in 
# attempt to observe a more linear relationship with the proportion of 'Leave'
# votes
TransformPlot("Young")
TransformPlot("WorkingAge")

# We plot the employment-related covariates against the proportion of 'Leave'
# votes
par(mfrow=c(3,2))
par(mar=c(4,4,4,4))
employmentcov <- c("Students", "Unemp", "UnempRate_EA", "HigherOccup",
                   "RoutineOccupOrLTU") 
LeavePlot(employmentcov)

# We transform the Students covariate in attempt to break the cluster of points 
# that exists in its plot or show a more linear relationship with the proportion
# of 'Leave' votes. Applying the log, sqrt and recipricol transformation on 
# Students achieves a stronger relationship with the proportion of 'Leave' votes.
par(mar= c(5.1, 4.1, 4.1, 2.1))
TransformPlot("Students")

# We plot the employment status related covariates against each other to 
# observe their relationship with each other. We also calculate the correlation
# between covariates. In this case, we discount the Unemp covariate since it
# is essentially the same as the UnempRate_EA covariate. We also discount
# the students covariate because it is unlikely it will have a relationship
# with the remaining covariates
employmentcov4 <- c("UnempRate_EA", "HigherOccup", "RoutineOccupOrLTU") 
CorrPlot(employmentcov4)

# RoutineOccupOrLTU appears to be directly correlated with the HigherOccup. So
# we would recommend only using either HigherOccup or RoutineOccupOrLTU.
cat("Correlation between employment-related covariates:\n")
CorrStatement("HigherOccup", "UnempRate_EA")
CorrStatement("HigherOccup", "RoutineOccupOrLTU")
CorrStatement("UnempRate_EA", "RoutineOccupOrLTU")
cat("====================\n")

# We plot the social grade covariates against the proportion of 'Leave' votes.
# Generally there appears to be a positive correlation between the social grade
# covariates and the proportion of 'Leave' votes (except for C1)
par(mfrow=c(3,2))
socialcov <- c("C1", "C2", "DE", "C1C2DE", "C2DE") 
par(mar= c(4, 4, 4, 4))
LeavePlot(socialcov)

# We plot the social grade covariates, C1, C2 and DE, against each other to observe
# their relationship with each other, and calculate the correlation between 
# them
socialcov3 <- c("C1", "C2", "DE") 
par(mar= c(5.1, 4.1, 4.1, 2.1))
CorrPlot(socialcov3)

cat("Correlation between social grade covariates:\n")
CorrStatement("C1", "C2")
CorrStatement("DE", "C2")
CorrStatement("C1", "DE")
cat("====================\n")

# We plot the covariates related to deprivation against the proportion of 
# 'Leave' votes. 
par(mfrow=c(1,2))
deprivedcov <- c("Deprived", "MultiDepriv") 
LeavePlot(deprivedcov)

# Deprived and MultiDepriv essentially provide the same 
# information so we would expect them to be highly correlated.
par(mfrow=c(1,1))
CorrPlot(deprivedcov)

cat("Correlation between Deprived covariates:\n")
CorrStatement("Deprived", "MultiDepriv")
cat("====================\n")

# We plot the accomodation-ownership covariates against the proportion of
# 'Leave' votes. We observe a moderate negative correlation between PrivateRent
# and the proportion of 'Leave' votes
par(mfrow=c(2,2))
housingcov <- c("Owned", "OwnedOutright", "SocialRent", "PrivateRent")
LeavePlot(housingcov)

# We produce Boxplots for the proportion of 'Leave' votes according to whether
# the ward included postal votes or not. We observe that on average, wards with 
# postal votes included were more likely to vote 'Remain'
par(mfrow=c(1,1))
boxplot(LeaveProp ~ Postals, data=RefGr1, xlab= "Were Postal Votes Included?",
        ylab= "Proportion of Leave Votes", names=c("No", "Yes"))

# We create boxplots for the proportion of 'Leave' votes according to the
# AreaType the ward belongs in. We observe that on average, area E09, was more
# likely to vote Remain.
par(mfrow=c(1,1))
boxplot(LeaveProp ~ AreaType, data= RefGr1, ylab= "Proportion of Leave Votes")

# Since the dataset has 8 regions, we want to reduce the levels of this 
# categorical covariate using hierarchical clustering. We group the regions
# into 3 groups based on similarities with covariates we believe are related
# to the proportion of 'Leave' votes. 
# 'leavecovs' represents the covariates we believe are related based on our
# analysis so far and Rosenbaum's article
leavecovs <- c("AdultMeanAge", "White", "Black", "Asian", "Indian", "Pakistani",
               "PrivateRent", "NoQuals", "L1Quals", "L4Quals_plus", "Students",
               "Unemp", "UnempRate_EA", "HigherOccup", "RoutineOccupOrLTU",
               "Deprived", "MultiDepriv", "DE", "C1", "C2", "Young", "WorkingAge",
               "RetirementAge")
RegionMeans <- aggregate(RefGr1[,leavecovs], by=list(RefGr1$RegionName), FUN=mean)
rownames(RegionMeans) <- RegionMeans[,1]
RegionMeans <- scale(RegionMeans[,-1])
Distances <- dist(RegionMeans)
ClusTree <- hclust(Distances, method="complete")
par(mar=c(3,3,3,1), mgp=c(2,0.75,0))
plot(ClusTree, xlab="Region name", ylab="Separation", cex.main=0.8)

RegionGroups <- cutree(ClusTree, k=3)

cat("We categorise the regions into the following 3 groups:", "\n")
print(RegionGroups, width=90)

ReferendumResults <- merge(ReferendumResults, data.frame(RegionName=names(RegionGroups),
                                                         RegionGroup=RegionGroups))
ReferendumResults <- ReferendumResults[order(ReferendumResults$ID),]

RefGr1 <- merge(RefGr1, data.frame(RegionName=names(RegionGroups),
                                   RegionGroup=RegionGroups))
RefGr1 <- RefGr1[order(RefGr1$ID),]

# We produce boxplots of the proportion of 'Leave' votes according to the 
# region groups we created. We see that RegionGroup 1 was more likely to 
# vote 'Leave', followed by group 2 and then group 3.
par(mfrow=c(1,1))
boxplot(LeaveProp ~ RegionGroup, data=RefGr1, ylab= "Proportion of 'Leave' Votes")

# Interactions

# First, we investigate the interactions between the numeric covariates and the
# AreaType with the proportion of 'Leave' votes. We create a function because
# we expect to do this for several covariates
AreaInteract <- function(areacov)
  for (i in areacov) {
    
    # areacov: a list of the covariates we would like to plot against the 
    # proportion of 'Leave' votes according to the AreaType
    
    plot_list = list()
    xcovinteract <- RefGr1[,i]
    areaplot = qplot(x = xcovinteract, y = LeaveProp, data = RefGr1
                     , color = AreaType) +
      labs(x= paste("% of", i, "Residents"), y= "Proportion of 'Leave' Votes") +
      geom_smooth(method = "lm")
    plot_list[[i]] = areaplot
    
    print(areaplot)
  }

# We produce the interaction plots between the numeric covariates and the 
# proportion of 'Leave' votes according to the AreaType
AreaInteract(educovariates)
AreaInteract(ethnicitycov)
AreaInteract(agecov)
AreaInteract(employmentcov)
AreaInteract(socialcov)
AreaInteract(housingcov)
AreaInteract(deprivedcov)

# We produce the interaction plots between the numeric covariates and the 
# proportion of 'Leave' votes according to the RegionGroup
RegionInteract <- function(regioncov)
  for (i in regioncov) {
    
    # regioncov: a list of the covariates we would like to plot against the 
    # proportion of 'Leave' votes according to the AreaType
    
    plot_list = list()
    covinteract <- RefGr1[,i]
    regionplot = qplot(x = covinteract, y = LeaveProp, data = RefGr1
                       , color = as.factor(RegionGroup)) +
      labs(x= paste("% of", i, "Residents"), y= "Proportion of 'Leave' Votes") +
      geom_smooth(method = "lm")
    plot_list[[i]] = regionplot
    
    print(regionplot)
  }

RegionInteract(educovariates)
RegionInteract(ethnicitycov)
RegionInteract(agecov)
RegionInteract(employmentcov)
RegionInteract(socialcov)
RegionInteract(housingcov)
RegionInteract(deprivedcov)

# Rosenbaum also suggested that voters with lower qualification were more
# likely to back Leave than the better qualified, even when they’re in the
# same social or occupational class. We plot the interaction between
# NoQuals and the different occupational classes
NoQualsCut <- cut(RefGr1$NoQuals, pretty(RefGr1$NoQuals))
qplot(x = C1, y = LeaveProp, data = RefGr1
      , color = NoQualsCut) +
  labs(x= "Social Grade C1", y= "Proportion of 'Leave' Votes", color= "NoQuals") +
  geom_smooth(method = "lm")

qplot(x = C2, y = LeaveProp, data = RefGr1
      , color = NoQualsCut) +
  labs(x= "Social Grade C2", y= "Proportion of 'Leave' Votes", color= "NoQuals") +
  geom_smooth(method = "lm")

qplot(x = DE, y = LeaveProp, data = RefGr1
      , color = NoQualsCut) +
  labs(x= "Social Grade DE", y= "Proportion of 'Leave' Votes", "NoQuals") +
  geom_smooth(method = "lm")

# Testing assumptions

# To test whether certain assumptions would be appropriate, we use a simple
# linear modelconsisting of the education covariates to test these assumptions
# This is because Rosenbaum's article states that education has the largest
# influence on the proportion of 'Leave' votes and the education-related 
# covariates appear to have a linear relationship with the proportion of 
# 'Leave' votes

# We create a linear model consisting only of the education covariates to predict
# the proportion of leave votes
education.model <- lm(LeaveProp ~ NoQuals + L1Quals + L4Quals_plus, data= RefGr1)

# We plot the diagnostics for this model. Generally, we observe a random scatter
# of points forming a horizontal band in the Residuals vs Fitted plot. This 
# suggests that the homoscedasticity assumption is satisfied. The Normal QQ-plot
# has many points lying on the y=x line, however, there are deviations at the 
# lower tail. This may improve with a more complex model. 
par(mfrow=c(2,2))
plot(education.model)

# We calculate the standardised residuals of the education model. Generally, this
# shows a random scatter of points around 0, with most standardised residuals 
# lying between 2 and -2. Further suggesting that the homoscedasticity assumption
# is satisfied. 
residedu <- rstandard(education.model)
par(mfrow=c(1,1))
plot(fitted(education.model), residedu, pch= 16, xlab= "Fitted Values", 
     ylab= "Standardised Residuals")
abline(0,0, col= "red")

plot(RefGr1$AreaType, rstandard(education.model))

# Model Building ####################################################################################################

# Linear Model 
# To begin the model building process, we first consider a linear model.

# Our initial model consists of all the relevant predictors suggested 
# by the EDA and information from relevant preliminary readings.
leave.lm1a <- lm(LeaveProp ~ as.factor(RegionGroup) + as.factor(Postals)
                 + NoQuals + L1Quals + L4Quals_plus + AdultMeanAge + RetirementAge 
                 + I(1/(Young + 1)) + White + I(log(Black+1)) + I(log(Asian+1))
                 + UnempRate_EA + HigherOccup + C2 + PrivateRent + MultiDepriv
                 + I(log(Students+1)),
                 data=RefGr1)

# We create a function that provides the summary and AIC of a model. It also
# produces the Residuals vs Fitted plot, Normal Q-Q plot, Scale-Location plot, and
# Residuals vs Leverage plot for the given model. These plots will allow us to 
# check whether the given model satisfies the assumptions of a linear model.
lmsummary <- function(lm.model){
  
  # model: this is the model we would like to find the summary and AIC of.
  
  cat(as.character(substitute(lm.model)), "summary:\n")
  print(summary(lm.model))
  cat("The AIC of", as.character(substitute(lm.model)), "is", AIC(lm.model), "\n")
  cat("====================\n")
  
  par(mfrow=c(2,2))
  plot(lm.model, pch= 20)
  
}

fsummary <- function(model1, model2) {
  
  # A function that performs an F-test between two models and prints the
  # result
  
  # model1 and model2 represents the two models we would like to perform an
  # F-test on
  
  cat("F-test between", as.character(substitute(model1)), "and", 
      as.character(substitute(model2)), "\n")
  print(anova(model1, model2, test= "F"))
  cat("====================\n")
  
}

# We find the model summary, AIC and plot of leave.lm1a. From the plot of 
# leave.lm1a it seems that the model satisfies the assumptions of a linear model
lmsummary(leave.lm1a)

# We remove the log(Students+1) covariate based on its high p-value. We also
# perform an ANOVA test to compare model leave.lm1a and leave.lm1b
leave.lm1b <- update(leave.lm1a, . ~ . - I(log(Students+1)))
lmsummary(leave.lm1b)
fsummary(leave.lm1b, leave.lm1a)

# We remove the 1/(Young + 1) covariate based on its high p-value
leave.lm1c <- update(leave.lm1b, . ~ . - I(1/(Young + 1)))
lmsummary(leave.lm1c)
fsummary(leave.lm1c, leave.lm1b)

# We remove the AdultMeanAge covariate based on its high p-value.
leave.lm1d <- update(leave.lm1c, . ~ . - AdultMeanAge)
lmsummary(leave.lm1d)
fsummary(leave.lm1d, leave.lm1c)

# We suspect that MultiDepriv and UnempRate_EA are highly correlated. We create
# a plot of these two covariates against each other and calculate their 
# correlation. We remove UnempRate_EA based on it having a relatively higher 
# p-value. Although, the AIC increases when we remove UnempRate_EA and the
# ANOVA test suggests we should keep this covariate, we do not 
# keep UnempRate_EA because of the correlation between UnempRate_EA and 
# MultiDepriv
par(mfrow=c(1,1))
plot(RefGr1$UnempRate_EA, RefGr1$MultiDepriv, pch=20,
     xlab= "% of Unemployed Residents (Economically Active)",
     ylab= "% of Household Deprived in 2+ Dimensions")
CorrStatement("UnempRate_EA", "MultiDepriv")
cat("====================\n")
leave.lm1e <- update(leave.lm1d, . ~ . - UnempRate_EA)
lmsummary(leave.lm1e)
fsummary(leave.lm1e, leave.lm1d)

# From our EDA, we saw that the education covariates are highly correlated with
# each other. To counteract this, we remove the L1Quals covariate from our model.
# The AIC increases and the RSS decreases. However, we still remove the L1Quals
# covariate because we believe it essentially provides the same information as
# the NoQuals covariate
leave.lm1f <- update(leave.lm1e, . ~ . - L1Quals)
lmsummary(leave.lm1f)
fsummary(leave.lm1f, leave.lm1e)

# The PrivateRent covariate now has a very high p-value. We remove the 
# PrivateRent covariate on this basis.
leave.lm1g <- update(leave.lm1f, . ~ . - PrivateRent)
lmsummary(leave.lm1g)
fsummary(leave.lm1g, leave.lm1f)

# Interactions
# Rosenbaum's article stated that ethnic minorities were generally more likely
# to vote 'Remain', however, in certain regions they were more likely to vote 
# 'Leave'. Therefore, we include an interaction between ethnicity and 
# RegionGroup. We observe a large decrease in the AIC value and an increase in
# multiple R squared. Although the terms with RegionGroup 2 are insignificant,
# we keep these terms in our model.
leave.lm2a <- update(leave.lm1g, . ~ . + as.factor(RegionGroup)*I(log(Black + 1))
                     + as.factor(RegionGroup)*I(log(Asian + 1)) + 
                       as.factor(RegionGroup)*White)
lmsummary(leave.lm2a)
fsummary(leave.lm1g, leave.lm2a)

# Based on the model summary, we remove the interaction between RegionGroup and
# log(Asian+1). We also perform an anova test for leave.lm2b and leave.lm2a
# which reinforces the idea that we should remove this interaction.
leave.lm2b <- update(leave.lm1g, . ~ . + as.factor(RegionGroup)*I(log(Black + 1))
                     + as.factor(RegionGroup)*White)
lmsummary(leave.lm2b)
fsummary(leave.lm2b, leave.lm2a)

# Rosenbaum's article suggested that wards with lower education qualifications
# produced high 'Remain' votes. These wards turned out to have a higher ethnic 
# minority population. Therefore, we investigate the interaction between NoQuals
# and ethnicity
leave.lm2c <- update(leave.lm2b, . ~ . + NoQuals*White + NoQuals*I(log(Black + 1))
                     + I(log(Asian + 1))*NoQuals)
lmsummary(leave.lm2c)
fsummary(leave.lm2b, leave.lm2c)

# We include an interaction between the ethnicity covariates and the deprived
# covariates
leave.lm2d <- update(leave.lm2c, . ~ . + MultiDepriv*White +
                       MultiDepriv*I(log(Black + 1)) + I(log(Asian + 1))*MultiDepriv)
lmsummary(leave.lm2d)
fsummary(leave.lm2c, leave.lm2d)

# We remove the interaction between log(Asian+1) and NoQuals, as well as the
# interaction between White and MultiDepriv
leave.lm2e <- update(leave.lm2b, . ~ . + NoQuals*White + NoQuals*I(log(Black + 1)) 
                     + MultiDepriv*I(log(Black + 1)) +
                       I(log(Asian + 1))*MultiDepriv)
lmsummary(leave.lm2e)
fsummary(leave.lm2e, leave.lm2d)

# We remove the interaction between RegionGroup and White as their p-values are 
# relatively higher
leave.lm2f <- update(leave.lm2e, . ~ . - as.factor(RegionGroup):White)
lmsummary(leave.lm2f)
fsummary(leave.lm2f, leave.lm2e)

# We check the interaction between RetirementAge and HigherOccup
leave.lm2g <- update(leave.lm2f, . ~ . + RetirementAge*C2)
lmsummary(leave.lm2g)
fsummary(leave.lm2f, leave.lm2g)

# Alternatively, we also check the interaction between RetirementAge and C2
# For both of the interactions with RetirementAge, we notice a decrease in 
# te AIC. The AIC for leave.lm2h is slightly lower, so we proceed with that model
leave.lm2h <- update(leave.lm2f, . ~ . + RetirementAge*HigherOccup)
lmsummary(leave.lm2h)
fsummary(leave.lm2f, leave.lm2h)

# We look at the possibility of replacing RetirementAge with the AdultMeanAge. 
# This model has a lower AIC and lower p-values, therefore we proceed with
# this model.
leave.lm2i <- update(leave.lm2h, . ~ . + AdultMeanAge*HigherOccup - RetirementAge:HigherOccup
                     - RetirementAge)
lmsummary(leave.lm2i)

# For our final model, we plot its fitted values against the standardised 
# residuals to test the homoscedasticity assumption. Generally there is a 
# random scatter of points that form a horizontal band suggesting that 
# the homoscedasticity assumption is satisfied. 
sresid2i <- rstandard(leave.lm2i)
par(mfrow=c(1,1))
plot(fitted(leave.lm2i), sresid2i, pch= 20, xlab= "Fitted Values", ylab= 
       "Standardised Residuals", main= "Linear Model 2i")
abline(0, 0, col= "red")

# Generalised Linear Model

# As we continue with the model building process, we consider using a GLM to 
# model the proportion of 'Leave' votes', which allows us to explore and include 
# covariates that showed no clear linear relationship to the proportion of leave 
# votes.

# Since we are modelling the *proportion* of leave votes, we will consider models 
# based on the Binomial distribution with a logit link function to carry 
# out a logistic regression. 

# At each stage of the the model building process, we will use p-values  and 
# F-tests for model comparison,to measure the significance of the covariates we add. 
# As we will be doing this frequently, we create a function that that returns the
# summary of the model we create and one that performs an anova F-test on two 
# nested models.  

modelsummary <- function(model) {
  # a function that returns the summary of the model and prints the results
  
  # model: the model we would like to find the summary of 
  
  cat("Summary of", as.character(substitute(model)), "\n")
  print(summary(model))
  cat("====================\n")
  
  par(mfrow=c(2,2))
  plot(model, pch= 20)
  
}

# For our first GLM we begin by including all the covariates that showed a clear 
# linear relationship with the response variable as suggested by the plots in the 
# EDA. 
# Namely, we add all predictors relating to education, the % of Residents with a 
# Higher Level Occupation variable,the % of with a Routine Occupation or Long Term 
# Unemployed variable,the C1,C2,DE social grade variables and the % of Households 
# Privately Renting variable. 
leave.glm1 <- glm(LeaveProp ~ L4Quals_plus + NoQuals + L1Quals+HigherOccup+
                  RoutineOccupOrLTU+C2+PrivateRent,
                weights=NVotes, family=binomial(link="logit"), data= RefGr1)
modelsummary(leave.glm1)

# Before proceed to add or remove covariates, we check the model for overdispersion.

# We produce residual plots for this model. We observe that there are several 
# residuals outside [-2,2] and potentially influential results from the Cook's
# distance plot. Moreover, the variance of the Pearson residuals is not close 
#to 1.
par(mfrow=c(2,2), lwd=2, mar=c(3,3,2,2), mgp=c(2,0.75,0))
plot(leave.glm1, which=1:4)

# Calculates variance of the Pearson residuals
sum( resid(leave.glm1,type="pearson")^2 )/leave.glm1$df.residual

# This suggests that the proportions of leave votes are more variable than we 
# would expect from a Binomial distribution. Therefore there is evidence of 
# overdispersion. 

# We will look to explain the excess variation by adding more covariates.
leave.glm2 <- update(leave.glm1, . ~ . + Density) # We add the "Density" predictor.
modelsummary(leave.glm2)
# We add the "RegionGroup" predictor
leave.glm3 <- update(leave.glm2, . ~ . + as.factor(RegionGroup)) 
modelsummary(leave.glm3)

# We re-plot the residual plots for the model and re-evaluate the variance of 
# the Pearson residuals
par(mfrow=c(2,2),lwd=2,mar=c(3,3,2,2),mgp=c(2,0.75,0))
plot(leave.glm3, which=1:4) # Creates residual plots

# Calculates variance of the Pearson residuals
sum( resid(leave.glm3, type="pearson")^2 )/leave.glm3$df.residual 

# There are still several residuals outside the range (-2,2) and the variance of
# the Pearson residuals is not close to 1. Therefore, we haven't succeeded in
# explaining the excess variation.

# In this case, we will resort to introducing a "dummy" dispersion parameter into
# the Binomial model, which allows all of the standard errors to be increased to 
# acknowledge the additional variability in the data. We'll do this via the use of
# a "quasibinomial" family of distributions. 
leave.glm4 <- update(leave.glm3, . ~ ., family=quasibinomial(link="logit")) 
modelsummary(leave.glm4)

# Based on the new model summary, all predictors we have so far included, are 
# statistically significant

# Thus, we can now proceed building the model via stepwise regression by adding 
# candidate covariates that had showed no linear relationship with the 
# proportion of 'Leave' votes in the EDA but might be worth exploring based on 
# preliminary readings and the context of the problem at hand. 

# Lord Ashcroft reports in this article that "The older the voters, the more likely 
# they were to have voted to leave the EU", therefore we'll be include 
# an age-related covariate to our previous model.   
leave.glm5 <- update(leave.glm4, . ~ . + MeanAge)
modelsummary(leave.glm5)
fsummary(leave.glm4,leave.glm5)

# Rosenbaum suggests that ethnicity is a "smaller" factor, but one that still 
# influences the proportion of leave votes, the plots relating to ethnicity in the
# EDA,indeed show some relationship between ethnicity and the response
# variable, we'll therefore add it to our model. We continue with only the
# Asian and White covariate based on the fact Black has a relatively high p value
leave.glm6 <- update(leave.glm5, . ~ . + Asian + White + Black)
modelsummary(leave.glm6)
fsummary(leave.glm5, leave.glm6)

leave.glm6 <- update(leave.glm5, . ~ . + Asian + White)
modelsummary(leave.glm6)

# In his report of the Referendum, Lord Ashcroft discloses that "those who owned 
# their homes outright voted to leave by 55% to 45%", we therefore add the 
# OwnedOutright covariate, accordingly. 
leave.glm7 <- update(leave.glm6, . ~ . + OwnedOutright)
modelsummary(leave.glm7)
fsummary(leave.glm6, leave.glm7)

# From Rosenbaum's BBC article, we also learn that income also influences to some 
# extent the proportion of leave votes, so we add a covariate related deprivation.
leave.glm8 <- update(leave.glm7, . ~ . + MultiDepriv)
modelsummary(leave.glm8)
fsummary(leave.glm7, leave.glm8)

#The EDA revealed that Postal voters were slightly less likely to back leaving 
#the EU.
leave.glm9 <- update(leave.glm8, . ~ . + as.factor(Postals))
modelsummary(leave.glm9)
fsummary(leave.glm8, leave.glm9)

# Based on the model summary anova output, we remove conside not accounting for 
# Postal votes in the model as the p-value is relatively large.
leave.glm10 <- update(leave.glm9, . ~ . - as.factor(Postals))
modelsummary(leave.glm10)
fsummary(leave.glm10, leave.glm9)


# We now consider model "leave.glm10" as our current best model.

# Before we investigate any interactions, we check the model to ensure no gross 
# violations of assumptions.

# Testing Assumptions:
#1.Error terms should be  independent. We check by plotting the 
# standardised residuals against each numeric covariate. 
# As we have many numeric covariates, we write a function that will create 
# the plots for us.
ResidPlot <- function(residcov, residmodel) {
  
  # Function that plots the given standardised residuals against the numeric
  # covariates
  
  # residcov: the numeric covariate we want to plot against the standardised
  # residuals
  #model: model object used to calculate the standardised residuals. 
  
  refcov <- RefGr1[ ,residcov]
  sresid <- rstandard(residmodel)
  plot(x= refcov, y= sresid, pch= 20, ylab= 
         "Standardised Residuals", xlab= paste("% of", residcov, "Residents")) 
  abline(0,0, col="cornflowerblue", lwd=2)
}

par(mfrow= c(5,3))
ResidPlot("L4Quals_plus", leave.glm10)
ResidPlot("NoQuals", leave.glm10)
ResidPlot("L1Quals", leave.glm10)
ResidPlot("RoutineOccupOrLTU", leave.glm10)
ResidPlot("HigherOccup", leave.glm10)
ResidPlot("C2", leave.glm10)
ResidPlot("PrivateRent", leave.glm10)
ResidPlot("White", leave.glm10)
ResidPlot("Asian", leave.glm10)
ResidPlot("OwnedOutright", leave.glm10)
ResidPlot("MeanAge", leave.glm10)
ResidPlot("MultiDepriv", leave.glm10)
ResidPlot("Density", leave.glm10)

# The plots show no obvious patterns or trends, therefore we can assume that
# the assumption is satisfied. 

#2.Error terms should be normally distributed. To check if this assumption has 
# been violated, we plot a normality probability plot of the standardised
# residuals.
sr_glm10<-rstandard(leave.glm10)
par(mfrow=c(1,1))
qqnorm(sr_glm10, ylab="Standrdised Residuals",col=alpha(1,0.4), pch=16, main=
"Normal Probability Plot")
qqline(sr_glm10, col="cornflowerblue", lwd=2)

# We observe that the QQ plot has heavy tails as some of the standardised
# residuals deviate from y=x in both the upper and lower tails. However, since
# most residuals lie on the y=x line we conclude that the normality assumption
# has been met. 

#3.Homoscedasticity of error terms. We test whether there is evidence that the 
# error terms have a constant variance by plotting the standardised residuals 
# against the fitted values for the proportion of leave votes, as shown below.
plot(fitted(leave.glm10), sr_glm10, xlab="Fitted Values", ylab=
       "Standardised Residuals", col=alpha(1,0.4), pch=16)
abline(0, 0, col="cornflowerblue", lwd=2)

# The plot shows that there are no obvious departures from homoscedasticity as 
# the points are fairly scattered around y=0 to form a horizontal band, 
# providing no evidence against the homoscedasticity assumption.

#4.There should be a linear relationship between the transformed expected 
# response in terms of the link function and the explanatory variables.
# To check this, we plot the log-odds against each numeric covariate. As we will
# do this repeatedly, we create a function for it. 

LogPlot<- function(logcov) {
  
  # Function that plots the given covariate against plots the log odds.
  # logcov: the covariate we want to plot against the log odds
  
  probabilities <- predict(leave.glm10, type = "response")
  logit_p <- log(probabilities/(1-probabilities))
  
  reflog <- RefGr1[,logcov]
  
  ggplot(data= RefGr1, aes(x= reflog, y= logit_p))+
           geom_point(size = 0.5, alpha = 0.5)+
           geom_smooth(method = "lm")+
           xlab(paste("% of", as.character(substitute(logcov)), "Residents"))+
           ylab("Log of Odds")
}

LogPlot("L4Quals_plus")
LogPlot("NoQuals")
LogPlot("L1Quals")
LogPlot("RoutineOccupOrLTU")
LogPlot("HigherOccup")
LogPlot("C2")
LogPlot("PrivateRent")
LogPlot("White")
LogPlot("Asian")
LogPlot("OwnedOutright")
LogPlot("MeanAge")
LogPlot("MultiDepriv")
LogPlot("Density")

# Most plots, except from the covariates relating to the "% of Households Owning 
# Accomodation Outright", show a fairly linear relationship with the transformed
# expected response variable. 
# We will consider implementing the transformations we explored in the EDA to 
# improve linearity,as seen below.
probabilities <- predict(leave.glm10, type = "response")
logit_p <- log(probabilities/(1-probabilities))
ggplot(data= RefGr1, aes(x=(log(OwnedOutright)), y= logit_p))+
  geom_point(size = 0.5, alpha = 0.5)+
  geom_smooth(method = "lm")+
  xlab(paste("% of log(OwnedOutright) Residents"))+
  ylab("Log of Odds") 

# The transformation improves linearity between log-odds and the "OwnedOutright"
# we therefore reflect this in our model. 
leave.glm11 <- update(leave.glm10, . ~ .- OwnedOutright)
leave.glm12 <- update(leave.glm11, . ~ .+ log(OwnedOutright))
modelsummary(leave.glm12)
fsummary(leave.glm11, leave.glm12)


# Since all assumptions have now been met, we can proceed in the model building
# process by investigating the interactions explored in the EDA and suggested
# by preliminary readings. 

# In his BBC article, Rosenbaum reports that there were wards where electorates 
# with lower education qualifications produced low leave and high remain votes, 
# these wards tend to have a high ethnic minority. 
leave.glm13 <- update(leave.glm12, . ~ .+ NoQuals*Asian)
modelsummary(leave.glm13)
fsummary(leave.glm12, leave.glm13)

# We check the interaction between age and education.
leave.glm14 <- update(leave.glm13, . ~ . + NoQuals*MeanAge)
modelsummary(leave.glm14)
fsummary(leave.glm13, leave.glm14)

# Rosenbaum also discloses that although ethnic minorities generally
# voted to remain in the EU, some Asian populations in London were more likely to
# support Leave than others.
# We reflect this in our model, through an interaction between our region and 
# ethnicity covariates. 
leave.glm15 <- update(leave.glm14, . ~ .+ as.factor(RegionGroup)*Asian)
modelsummary(leave.glm15)
fsummary(leave.glm14, leave.glm15)

# The output from the F-test comparing model "leave.glm14" and model "leave.glm15",
# suggests the interaction between ethnicity and region yields a high p-value, 
# therefore, we remove it. 

# In the BBC article, we learn that despite the type of ethnicity, voters 
# with lower incomes had higher leave vote. We represent this through 
# interactions between the "MultiDeprived", ethicity and employment covariates. 
leave.glm16 <- update(leave.glm14, . ~ .+ MultiDepriv*Asian)
modelsummary(leave.glm16)
fsummary(leave.glm14, leave.glm16)

leave.glm17 <- update(leave.glm16, . ~ . + MultiDepriv*White)
modelsummary(leave.glm17)
fsummary(leave.glm16, leave.glm17)

leave.glm18 <- update(leave.glm17, . ~ . + White*RoutineOccupOrLTU)
modelsummary(leave.glm18)
fsummary(leave.glm17, leave.glm18)

leave.glm19 <- update(leave.glm18, . ~ .+ Asian*RoutineOccupOrLTU)
modelsummary(leave.glm19)
fsummary(leave.glm18, leave.glm19)

# Furthermore, voters with lower qualification were more likely to 
# back Leave than the better qualified, even when they’re in the same social or 
# occupational class. Therefore we include an interaction between social class 
# and education.
leave.glm20 <- update(leave.glm19, . ~ .+C2*NoQuals)
modelsummary(leave.glm20)
fsummary(leave.glm19, leave.glm20)


# Lastly, check the model again to ensure no gross violations of assumptions. 
# Testing Assumptions:

#1. Error terms should be independent.
par(mfrow=c(5,3))
par(mar=c(1,1,1,1))
ResidPlot("L4Quals_plus", leave.glm20)
ResidPlot("NoQuals", leave.glm20)
ResidPlot("L1Quals", leave.glm20)
ResidPlot("RoutineOccupOrLTU", leave.glm20)
ResidPlot("HigherOccup", leave.glm20)
ResidPlot("C2", leave.glm20)
ResidPlot("PrivateRent", leave.glm20)
ResidPlot("White", leave.glm20)
ResidPlot("Asian", leave.glm20)
ResidPlot("OwnedOutright", leave.glm20)
ResidPlot("MeanAge", leave.glm20)
ResidPlot("MultiDepriv", leave.glm20)
ResidPlot("Density", leave.glm20)
# The plots show no obvious patterns or trends, therefore we can assume that
# the assumption is satisfied. 

#2. Error terms should be normally distributed.
par(mfrow=c(1,1))
par(mar= c(5.1, 4.1, 4.1, 2.1))
sr_glm20 <- rstandard(leave.glm20)
qqnorm(sr_glm20, ylab="Standardised Residuals", col=alpha(1,0.4), pch=16, main=
"Normal Probability Plot")
qqline(sr_glm20, col="cornflowerblue", lwd=2)

# We observe that the QQ plot has heavy tails as some of the standardised residuals 
# deviate from y=x in both the upper and lower tails. However, since most residuals 
# match y=x we conclude that the normality assumption has been met. 

#3. Homoscedasticity of error terms. 
plot(fitted(leave.glm20), sr_glm20, xlab="Fitted Values", ylab=
       "Standardised Residuals", col=alpha(1,0.4), pch=16)
abline(0, 0, col="cornflowerblue", lwd=2)

# The plot shows that there are no obvious departures from homoscedasticity as 
# the points are fairly scattered around y=0, thus providing no evidence against 
# the homoscedasticity assumption and also overfitting. 

# Therefore, our final glm is "leave.glm20"

# Generalised Additive Model

# As we proceed in the final stages of the model building process, we consider a
# GAM to model the proportion of 'Leave' votes, in order to reveal and 
# estimate non-linear effects of the covariate on the response variable. 

# As with the previous GLMs, we will consider models based on the Binomial 
# distribution with a logit link function. 

# Again, at each stage of the the model building process, we will use p-values
# and F-tests for model comparison,to measure the significance of the covariates
# we add. As we will be doing this frequently, we create a function that that
# returns the summary of the GAM we create.

gamsummary <- function(gam.model) {
  # a function that returns the summary of the model and prints the results
  
  # gam.model: the model we would like to find the summary of 
  
  cat("Summary of", as.character(substitute(gam.model)), "\n")
  print(summary(gam.model))
  cat("====================\n")
  
}

# Our initial model contains all the covariates that showed a clear
# linear relationship with the response variable as suggested by the plots in the 
# EDA. 
# Model 1 (Initial Model)
leave.gam1<- gam(LeaveProp ~ L4Quals_plus + NoQuals + L1Quals + HigherOccup +
                   RoutineOccupOrLTU + C2 + PrivateRent,
                 family= quasibinomial(link= "logit"), data= RefGr1)
gamsummary(leave.gam1)

#Model 2 (Removing "PrivateRent" because of its high p-value)
leave.gam2 <- update(leave.gam1, . ~ . - PrivateRent)
gamsummary(leave.gam2)
fsummary(leave.gam2, leave.gam1)

# Rosenbaum reports that "The combination of education, age and ethnicity accounts 
# for the large majority of the variation in [Leave] votes", we therefore.
# include the covariates relating to those factors first.

# Model 3 (As suggested by Rosenbaum's BBC article ethnicity is one of the key
# factors influencing the proportion of leave votes as ethnic minorities were
# less likely to back "Leave")
leave.gam3 <- update(leave.gam2, . ~ .+ White + Asian + Black)
gamsummary(leave.gam3)
fsummary(leave.gam2, leave.gam3)

# Model 4 (Removing "RoutineOccupOrLTU" due to relatively large p-value and its
# high correlation with other covariates)
leave.gam4 <- update(leave.gam3, . ~ . - RoutineOccupOrLTU)
gamsummary(leave.gam4)
fsummary(leave.gam4, leave.gam3)

# Model 5 (Adding age-related covariates)
leave.gam5 <- update(leave.gam4, . ~ . + RetirementAge)
gamsummary(leave.gam5)
fsummary(leave.gam4, leave.gam5)

# Model 6 (We include employment related covariates, since unemployed voters 
# were more likely to back "Leave" whereas student populations generally voted
# against it)
leave.gam6 <- update(leave.gam5,. ~ .+ Students + UnempRate_EA)
gamsummary(leave.gam6)
fsummary(leave.gam5, leave.gam6)  

# Model 7 (We notice an increase in the p-values of NoQuals and L1Quals, we therefore
# remove them)
leave.gam7a <- update(leave.gam6,. ~ . - L1Quals)
leave.gam7 <- update(leave.gam7a,. ~ .- NoQuals)
gamsummary(leave.gam7)
fsummary(leave.gam7, leave.gam6) 

# Model 8 (In his report of the Referendum, Lord Ashcroft reveals that voters that owned 
# their house outright and council/housing association tenants were more likely to
# vote leave, we therefore factor that in our model) 
leave.gam8 <- update(leave.gam7,. ~ . + s(OwnedOutright) + s(SocialRent))
gamsummary(leave.gam8)
fsummary(leave.gam7, leave.gam8) 


# Model 9 (Adding deprived variable, to reflect that those on a lower income 
# generally voted to leave the EU)
leave.gam9 <- update(leave.gam8,. ~ . + MultiDepriv)
gamsummary(leave.gam9)
fsummary(leave.gam8, leave.gam9)  

# Model 10 (Based on the summary output for Model 6, we remove SocialRent due to
# relatively large p-value)
leave.gam10<-update(leave.gam9,. ~ .- s(SocialRent))
gamsummary(leave.gam10)
fsummary(leave.gam10, leave.gam9) 

# Model 11 (In the EDA we learnt that Postal voters were slightly less likely to 
# back leaving the EU,we therefore add the Postals covariate to our model)
leave.gam11 <- update(leave.gam10,. ~ .+ as.factor(Postals))
gamsummary(leave.gam11)
fsummary(leave.gam10, leave.gam11) 

#Model 12 (we remove "Postals due to high p-value)
leave.gam12<-update(leave.gam11,. ~ .- as.factor(Postals))
gamsummary(leave.gam12)
fsummary(leave.gam12,leave.gam11) 

# Model 13 (adding region related covariates, AreaType and density )
leave.gam13 <- update(leave.gam10,. ~ .+ as.factor(AreaType) + s(Density))                                                                                                                                                                                  
gamsummary(leave.gam13)
fsummary(leave.gam12, leave.gam13) 


# Having included all the covariates we believe most influence the proportion 
# of leave votes based on our EDA, context and p-values, we will now investigate 
# interactions. 

# Model 14 (Ethnicities and and "MultiDepriv" interactions)
leave.gam14 <- update(leave.gam13,. ~ .+ te(Asian, MultiDepriv)) 
gamsummary(leave.gam14)
fsummary(leave.gam13, leave.gam14) 

# Model 15 
leave.gam15<-update(leave.gam14,. ~ . + te(MultiDepriv, Black))                                                                                                                                                                                 
gamsummary(leave.gam15)
fsummary(leave.gam14, leave.gam15) 

# Model 17 (we remove "Students" due to high p-value)
leave.gam17 <- update(leave.gam15,. ~ .- Students)
gamsummary(leave.gam17)
fsummary(leave.gam15, leave.gam17) 

#Model 19 (Ethnicity and Employment interaction)
leave.gam19 <- update(leave.gam17,. ~ . + te(White,UnempRate_EA))                                                                                                                                                                               
gamsummary(leave.gam19)
fsummary(leave.gam17, leave.gam19) 


#Model 20 (Age and Social Grade interaction)
leave.gam20 <- update(leave.gam19,. ~ . + te(C2,L4Quals_plus))                                                                                                                                                                                 
gamsummary(leave.gam20)
fsummary(leave.gam19, leave.gam20) 

#Model 21 (Area and Education interaction)
leave.gam21 <- update(leave.gam20,. ~ . + te(L4Quals_plus, by=as.factor(AreaType)))                                                                                                                                                                                 
gamsummary(leave.gam21)
fsummary(leave.gam20, leave.gam21) 

#Model 22 (Area and Ethnicity interaction)
leave.gam22 <- update(leave.gam21,. ~ . + te(White, by=as.factor(AreaType)))                                                                                                                                                                                 
gamsummary(leave.gam22)
fsummary(leave.gam21, leave.gam22) 

# The model "leave.gam22" is our final best GAM because of high R^2 value and 
# statistically significant covariates. 

# Lastly, check the model again to ensure no gross violations of assumptions. 
# Testing Assumptions:

#1.Error terms should be normally distributed.
#2. Homoscedasticity of error terms. 
par(mfrow=c(2,2))
gam.check(leave.gam22)
# We observe that the Normal QQ plot has heavy tails as some of the standardised
# residuals deviate from y=x in both the upper and lower tails. However, since
# most residuals lie on y=x we conclude that the normality assumption has been
# met. 
# The plots also shows that there are no obvious departures from
# homoscedasticity as the points are fairly scattered around y=0.

# Predictions

# The linear model, GLM and GAM appear reasonable in terms of: the bias 
# associated with the fitted values, p-values of the estimated coefficients and 
# adjusted R-squared scores.

#T he p-value from the F-test output comparing "leave.glm20" and "leave.gam22" 
# suggests that the GAM is the better model. Moreover, when comparing "leave.gam22" 
# with "leave.lm2i" using diagnostic plots as seen below, we observe that 
#"leave.lm2i" is a better fit to the data because the residuals appear to be
#follow a normal distribution closer than those of the gam model. Comparing the 
#residuals vs fitted plots reinforces this as the variability in the fitted values
#from the linear model is smaller compared to the
#other models. Although "leave.gam22" has a higher R^2 score, GAMs tends to 
#overfit, therefore taking these feature into account, we opt for "leave.lm2i" as 
#our final model.

fsummary(leave.glm20,leave.gam22) #F-test between GLM and GAM

par(mfrow=c(2,2))
gam.check(leave.gam22) #diagnostic plots for final GAM model

par(mfrow=c(2,2))
plot(leave.lm2i,which=1:4) #diagnostic plots for final linear model

# We will now use our chosen model to predict the proportion of ‘Leave’ votes 
# for each of the 267 wards with missing voting data, and also to estimate the 
# standard deviation of the prediction errors.
RefGr2 <- ReferendumResults[is.na(ReferendumResults$Leave),]
New_RefGr2 <- RefGr2

# We first calculate the standard deviations of our prediction errors. 

# Creates predictions for the proportion of leave votes based on our final model
leave.pred <- predict(leave.lm2i, newdata=data.frame(RegionGroup= RefGr2$RegionGroup,
            Postals= RefGr2$Postals, NoQuals= RefGr2$NoQuals, White= RefGr2$White,
            L4Quals_plus= RefGr2$L4Quals_plus, Black= RefGr2$Black, 
            Asian= RefGr2$Asian, HigherOccup= RefGr2$HigherOccup, C2= RefGr2$C2,
            MultiDepriv= RefGr2$MultiDepriv, AdultMeanAge= RefGr2$AdultMeanAge), 
            se.fit=TRUE)

p <- leave.pred$fit # predicted proportion of voting ‘Leave’ for a specific ward
var_p <- (leave.pred$se.fit)^2 #variance of standard error of p
var_Y <- (summary(leave.lm2i)$sigma)^2 # estimated variance of the proportion of
# ‘Leave’ votes
sd_pred <- sqrt(var_Y + var_p) #standard deviation of  prediction error

# We create a table containing the ward identifier, the predicted proportion of 
# ‘Leave’ votes for each ward and the standard deviation of our prediction error.

pred_data <- data.frame(var1=RefGr2["ID"], var2=p, var3=sd_pred)

# We create a data file containing the predictions and standard deviations of 
# our prediction errors.
write.table(pred_data, file= "ICA2_Group_BG_pred.dat", sep=" ", quote=FALSE,
            row.names = FALSE,col.names = FALSE)