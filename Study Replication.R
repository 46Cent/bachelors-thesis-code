install.packages("this.path")
setwd(this.path::this.dir())

# Installing Packages -----------------------------------------------------
library("haven")
library("readxl")
library("tidyverse")

# Study 1a ----------------------------------------------------------------

study1a <- read_sav("Study Data/Study 1a/Data/Study 1a Data.sav")

#'[ Calculating the Mean:
study1a %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(mean(Days))

study1a %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(Days)) #The Means are the same.

#'[ Calculating the Standard Deviation:
study1a %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(sd(Days))

study1a %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(Days))

#'[Conducting the regression:
Study1aReg <- lm(Days ~ WorktoReceive, data = study1a)
summary(Study1aReg)

#'[Confidence Intervalls:
confint(Study1aReg)

#'[ Cohen's d:
cohen.d(Days ~ WorktoReceive, data = study1a)

#'[Controlling for prior Flossing Behavior:
Study1aReg2 <- lm(Days ~ WorktoReceive + Flossed_Before, data = study1a)
summary(Study1aReg2)

Study1aReg2 <- lm(Days ~ WorktoReceive * FlossingBefore_Scale, data = study1a)
summary(Study1aReg2)

#'[Examining the percent of people who flossed for all 12 days:
LogRegStudy1a <- glm(FlossedAllDays ~ WorktoReceive, family = binomial, data = study1a)
summary(LogRegStudy1a)


# Study 1b ---------------------------------------------------------------

study1b <- read_sav("Study Data/Study 1b/Data/Study 1b Data.sav")

#'[ Calculating the Mean
study1b %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(TotalWorkoutsCompleted))

study1b %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(mean(TotalWorkoutsCompleted)) #The Means are the same.

#'[ Calculating the Standard Deviation:
study1b %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(TotalWorkoutsCompleted))

study1b %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(sd(TotalWorkoutsCompleted))

#'[Conducting the regression:
RegStudy1B <- lm(TotalWorkoutsCompleted ~ WorktoReceive, data = study1b)
summary(RegStudy1B)

#'[Confidence Intervalls:
confint(RegStudy1B)

#'[ Cohen's d:
cohen.d(TotalWorkoutsCompleted ~ WorktoReceive, data = study1b)

#'[ Percentage of People completing all workouts:
aggregate(Finishall16Workouts ~ WorktoReceive, study1b, mean)

#'[Examining the percent of people who did all Workouts:
LogRegStudy1b <- glm(Finishall16Workouts ~ WorktoReceive, family = binomial, data = study1b)
summary(LogRegStudy1b)

# Study 1c ---------------------------------------------------------------

study1c <- read_sav("Study Data/Study 1c/Data/Study 1c Data.sav")

#'[ Calculating the Mean
study1c %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(NumberComplete))

study1c %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(mean(NumberComplete)) #The Means are the same.

#'[ Calculating the Standard Deviation:
study1c %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(NumberComplete))

study1c %>% 
  filter(WorktoReceive == 0) %>% 
  summarise(sd(NumberComplete))

#'[Conducting the regression:
RegStudy1c <- lm(NumberComplete ~ WorktoReceive, data = study1c)
summary(RegStudy1c)

#'[Confidence Intervalls:
confint(RegStudy1c)

#'[ Cohen's d:
cohen.d(NumberComplete ~ WorktoReceive, data = study1c)

#'[Examining the percent of people who did all Workouts:
LogRegStudy1c <- glm(Defineall40Words ~ WorktoReceive, family = binomial, data = study1c)
summary(LogRegStudy1c)

# Study 1d ---------------------------------------------------------------

study1d <- read_sav("Study Data/Study 1d/Data/Study 1d Data.sav")

#'[ Calculating the Mean
study1d %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(TotalProductsShopped))

study1d %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(mean(TotalProductsShopped))

study1d %>% 
  filter(WorktoUnlockSurprise == 1) %>% 
  summarise(mean(TotalProductsShopped))

#'[ Calculating the Standard Deviation:
study1d %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(TotalProductsShopped))

study1d %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(sd(TotalProductsShopped))

study1d %>% 
  filter(WorktoUnlockSurprise == 1) %>% 
  summarise(sd(TotalProductsShopped))

#'[Conducting the regression:
RegStudy1d <- lm(TotalProductsShopped ~ WorktoReceive + WorktoUnlockSurprise, data = study1d)
summary(RegStudy1d)

#'[Confidence Intervalls:
confint(RegStudy1d)

#'[ Percentage of People completing tasks:
aggregate(EvaluateAll10Products ~ WorktoReceive, study1d, mean)
aggregate(EvaluateAll10Products ~ WorktoUnlock, study1d, mean)
aggregate(EvaluateAll10Products ~ WorktoUnlockSurprise, study1d, mean)

#'[Examining the percent of people who did all tasks:
LogRegStudy1d <- glm(EvaluateAll10Products ~ WorktoReceive + WorktoUnlockSurprise, family = binomial, data = study1d)
summary(LogRegStudy1d)

# Study 2 ---------------------------------------------------------------

study2 <- read_sav("Study Data/Study 2/Data/Study 2 Data.sav")

#'[ Calculating the Mean(s)

study2 %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(NumberComplete, na.rm = TRUE), na.rm = TRUE)

study2 %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(mean(NumberComplete, na.rm = TRUE), na.rm = TRUE)

study2 %>% 
  filter(WorktoReceiveGoal == 1) %>% 
  summarise(mean(NumberComplete, na.rm = TRUE), na.rm = TRUE)

#'[ Calculating the Standard Deviation:
study2 %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(NumberComplete, na.rm = TRUE), na.rm = TRUE)

study2 %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(sd(NumberComplete, na.rm = TRUE), na.rm = TRUE)

study2 %>% 
  filter(WorktoReceiveGoal == 1) %>% 
  summarise(sd(NumberComplete, na.rm = TRUE), na.rm = TRUE)

#'[Conducting the regression (on persistence):
RegStudy2 <- lm(NumberComplete ~ WorktoReceiveGoal + WorktoReceive, data = study2)
summary(RegStudy2)

#'[Confidence Intervalls:
confint(RegStudy2)

#'[ Removing the candidates who did not start
study2 <- study2 %>% na.omit()

#'[ Conducting the (logistic) regression (on Goal Setting):
LogRegStudy2_GS <- glm(SetGoal ~ WorktoUnlock + WorktoReceiveGoal, data = study2, family = binomial)
summary(LogRegStudy2_GS)

#'[ Conducting the (logistic) regression (on likelyhood to reach goal):
LogRegStudy2 <- glm(ReachorSurpassGoal ~ WorktoUnlock + WorktoReceiveGoal, data = study2, family = binomial)
summary(LogRegStudy2)

#'[ Conducting the regression on persistance after reaching the goal:
#"Of participants who reached the goal, participants were significantly more likely to persist and complete more workouts in the Work-to-Unlock condition"
study2_GSP <- study2 %>% 
  filter(ReachorSurpassGoal == 1) #Taking out the people who didn't reach or surpass their goal

RegStudy2_GSP <- lm(NumberComplete ~ WorktoReceive + WorktoReceiveGoal, data = study2_GSP)
summary(RegStudy2_GSP)

#'[ Percentage of People completing tasks:
aggregate(CompleteAllWorkouts ~ WorktoReceive, study2, mean)
aggregate(CompleteAllWorkouts ~ WorktoUnlock, study2, mean)
aggregate(CompleteAllWorkouts ~ WorktoReceiveGoal, study2, mean)

#'[ Conducting the (logistic) regression on how many people completed all exercises:
LogRegStudy2 <- glm(CompleteAllWorkouts ~ WorktoReceive+ WorktoReceiveGoal, data = study2, family = binomial)
summary(LogRegStudy2)

# Study 3a ---------------------------------------------------------------

study3a <- read_sav("Study Data/Study 3a/Data/Study 3a Data.sav")

#'[ Calculating the Mean
study3a %>% 
  filter(GoalBasedLumpSum == 1) %>% 
  summarise(mean(NumberComplete))

study3a %>% 
  filter(GoalBasedLumpSum == 0) %>% 
  summarise(mean(NumberComplete)) #The Means are the same.

#'[ Calculating the Standard Deviation:
study3a %>% 
  filter(GoalBasedLumpSum == 1) %>% 
  summarise(sd(NumberComplete))

study3a %>% 
  filter(GoalBasedLumpSum == 0) %>% 
  summarise(sd(NumberComplete))

#'[Conducting the regression:
RegStudy3a <- lm(NumberComplete ~ GoalBasedLumpSum, data = study3a)
summary(RegStudy3a)

#'[Confidence Intervalls:
confint(RegStudy3a)

#'[ Cohen's d:
cohen.d(NumberComplete ~ GoalBasedLumpSum, data = study3a)

#'[ Conducting the regression on SurpassGoal to see how many surpass the goal in either category:

study3a_RG <- study3a %>% 
  filter(ReachGoal == 1)

RegStudy3a_RG <- lm(SurpassGoal ~ GoalBasedLumpSum, data = study3a_RG)
summary(RegStudy3a_RG)

#'[Conducting the (logistic) regression:
LogRegStudy3a <- glm(DefinedAllWords ~ GoalBasedLumpSum, family = binomial, data = study3a)
summary(LogRegStudy3a)

# Study 3b ---------------------------------------------------------------

study3b <- read_sav("Study Data/Study 3b/Data/Study 3b Data.sav")

#'[ Calculating the Mean
study3b %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(Persistence))

study3b %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(mean(Persistence))

study3b %>% 
  filter(GoalBasedLumpSum == 1) %>% 
  summarise(mean(Persistence))

#'[ Calculating the Standard Deviation:
study3b %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(Persistence))

study3b %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(sd(Persistence))

study3b %>% 
  filter(GoalBasedLumpSum == 1) %>% 
  summarise(sd(Persistence))

#'[ Conducting the regression:
RegStudy3b <- lm(Persistence ~ WorktoReceive + GoalBasedLumpSum, data = study3b)
summary(RegStudy3b)

confint(RegStudy3b)

#'[ Conducting the regression on how accomplished they feel

study3b_acc <- read_sav("Study Data/Study 3b/Achievement Mechanism Post-Test/Data/Achievement Post Test.sav")

RegStudy3b_acc <- lm(AchivementPerception ~ GoalBasedLumpSum + WorktoReceive, data = study3b_acc)
summary(RegStudy3b_acc)

confint(RegStudy3b_acc)

#'[ Checking the Means of How accomplished they feel:
study3b_acc %>% 
  filter(GoalBasedLumpSum == 1) %>% 
  summarise(mean(AchivementPerception, na.rm = TRUE), na.rm = TRUE)

study3b_acc %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(AchivementPerception, na.rm = TRUE), na.rm = TRUE)

study3b_acc %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(mean(AchivementPerception, na.rm = TRUE), na.rm = TRUE)
# Study 4 ---------------------------------------------------------------

study4 <- read_sav("Study Data/Study 4/Data/Study 4 Data.sav")

#'[ Calculating the Mean
study4 %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(mean(NumberofWordsetsTyped))

study4 %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(mean(NumberofWordsetsTyped))

study4 %>% 
  filter(WorktoUnlockLongerUnlockingPeriod == 1) %>% 
  summarise(mean(NumberofWordsetsTyped))

study4 %>% 
  filter(LumpSum == 1) %>% 
  summarise(mean(NumberofWordsetsTyped))

#'[ Calculating the Standard Deviation:
study4 %>% 
  filter(WorktoReceive == 1) %>% 
  summarise(sd(NumberofWordsetsTyped))

study4 %>% 
  filter(WorktoUnlock == 1) %>% 
  summarise(sd(NumberofWordsetsTyped))

study4 %>% 
  filter(WorktoUnlockLongerUnlockingPeriod == 1) %>% 
  summarise(sd(NumberofWordsetsTyped))

study4 %>% 
  filter(LumpSum == 1) %>% 
  summarise(sd(NumberofWordsetsTyped))

#'[ Conducting the regression on the amount of words typed:

RegStudy4 <- lm(NumberofWordsetsTyped ~ WorktoReceive + LumpSum + WorktoUnlockLongerUnlockingPeriod, data = study4)
summary(RegStudy4)

confint(RegStudy4)

#'[ Conducting the (logistical) regression:
LogRegStudy4 <- glm(CompleteAllWordSets ~ WorktoReceive + LumpSum + WorktoUnlockLongerUnlockingPeriod, family = binomial, data = study4)
summary(LogRegStudy4)

confint(LogRegStudy4)
