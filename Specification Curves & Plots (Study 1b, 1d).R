setwd(this.path::this.dir())

# Installing Packages -----------------------------------------------------
library(haven)
library(MASS)
library(effectsize)
library(specr)
library(tidyverse)
# Study 1b -----------------------------------------------------
study1b <- read_sav("Paper Daten/Study 1b/Data/Study 1b Data.sav")

#'[ Formatting the Data:
data_1b <- study1b %>% 
  mutate(FinishedWorkouts = factor(Finishall16Workouts)) %>% 
  mutate(FinishedWorkouts = fct_recode(FinishedWorkouts,
                                       "FinishedAll" = "1",
                                       "DidntFinishAll" = "0"
  )) %>%
  mutate(FinishedWorkouts = as.character(FinishedWorkouts)) %>% # Creating the subset for people who didn't finish all Workouts
  
  mutate(age_c = scale(study1b$Whatisyourage, center = TRUE, scale = FALSE)) %>% # Creating an age variable centered around the mean
  
  mutate(gender = factor(WhatisyourgenderSelectedChoice)) %>% # factorizing the Gender
  
  mutate(DoesSimilarExercises = factor(Howoftendoyoucompleteexercisessimilartotheonesinthisstudy)) %>%
  mutate(StrongExercisers = fct_recode(DoesSimilarExercises,
                                       "StrongExerciser" = "7",
                                       "StrongExerciser" = "6",
                                       "Regular" = "5",
                                       "Regular" = "4",
                                       "Regular" = "3",
                                       "Regular" = "2",
                                       "Regular" = "1"
  ) ) %>% 
  mutate(StrongExercisers = as.character(StrongExercisers)) # Creating the subset that exludes strong exercisers

#'[Preparing the custom functions:

log_reg <- function(formula, data) {
  glm(formula, data, family = binomial)
}

NB_Reg <- function(formula, data) {
  glm.nb(formula, data)
}

lm_age <- function(formula, data) {
  f <- as.formula(formula)
  lm_int <- update(f, . ~ . + WorktoReceive*age_c)
  lm(lm_int, data = data)
}

tidy_transformed3 <- function(m) {
  tt <- broom::tidy(m, conf.int = TRUE)
  
  # If WorktoReceive is not in this model, return unchanged
  if (!("WorktoReceive" %in% tt$term)) {
    return(tt)
  }
  
  idx <- tt$term == "WorktoReceive"
  
  ## 1) Negative binomial (MASS::glm.nb) or Poisson:
  ##    -> percent change in expected rate: exp(beta) - 1
  if (inherits(m, "negbin") ||
      (inherits(m, "glm") && m$family$family %in% c("poisson"))) {
    
    beta <- tt$estimate[idx]
    low  <- tt$conf.low[idx]
    high <- tt$conf.high[idx]
    
    tt$estimate[idx]  <- exp(beta) - 1
    tt$conf.low[idx]  <- exp(low)  - 1
    tt$conf.high[idx] <- exp(high) - 1
    return(tt)
  }
  
  ## 2) Logistic regression (binomial):
  ##    -> percent change in odds: exp(beta) - 1
  if (inherits(m, "glm") && m$family$family == "binomial") {
    
    beta <- tt$estimate[idx]
    low  <- tt$conf.low[idx]
    high <- tt$conf.high[idx]
    
    tt$estimate[idx]  <- exp(beta) - 1
    tt$conf.low[idx]  <- exp(low)  - 1
    tt$conf.high[idx] <- exp(high) - 1
    return(tt)
  }
  
  ## 3) Linear models (lm):
  ##    -> percent change in mean outcome vs. control (WorktoReceive == 0)
  if (inherits(m, "lm")) {
    
    dat   <- model.frame(m)
    yname <- names(dat)[1]
    
    mu_ref <- mean(dat[[yname]][dat$WorktoReceive == 0], na.rm = TRUE)
    
    beta <- tt$estimate[idx]
    low  <- tt$conf.low[idx]
    high <- tt$conf.high[idx]
    
    tt$estimate[idx]  <- beta / mu_ref
    tt$conf.low[idx]  <- low  / mu_ref
    tt$conf.high[idx] <- high / mu_ref
    return(tt)
  }
  
  # Fallback: if none of the above, return unchanged
  tt
} # This is what transforms the estimates. Made with the help of ChatGPT

#'[ SCA Setup:
SpecCurve1_Setup <- setup(
  data = data_1b,
  x = "WorktoReceive",
  y = c("TotalWorkoutsCompleted", "Finishall16Workouts"),
  model = c("lm", "NB_Reg", "lm_age"),
  controls = c("Whatisyourage", "DoesSimilarExercises", "gender"),
  subsets = list(
    FinishedWorkouts = unique(data_1b$FinishedWorkouts)[1],
    StrongExercisers = unique(data_1b$StrongExercisers)[1]
  ),
  fun1 = tidy_transformed3,
  fun2  = NULL,
  simplify = F
)

#'[ Filtering invalid combinations:
SpecCurve1_Setup$specs <- SpecCurve1_Setup$specs %>%
  filter(!(model == "log_reg" & y != "Finishall16Workouts")) %>%
  filter(!(model == "lm" & y == "Finishall16Workouts")) %>%
  filter(!(model == "log_reg" & subsets == "DidntFinishAll")) %>%
  filter(!(model == "log_reg" & subsets == "DidntFinishAll & Regular")) %>%
  filter(!(model == "NB_Reg" & y != "TotalWorkoutsCompleted")) %>% 
  filter(!(model == "lm_age" & y == "Finishall16Workouts")) %>%
  filter(!(model == "lm_age" & controls == "Whatisyourage + gender")) %>% 
  filter(!(model == "lm_age" & controls == "Whatisyourage" | controls == "Whatisyourage + DoesSimilarExercises")) %>%   
  filter(!(model == "lm_age" & controls == "Whatisyourage + DoesSimilarExercises + gender"))

#'[ Running the model:
summary(SpecCurve1_Setup)
SpecCurve1<- specr(SpecCurve1_Setup)
summary(SpecCurve1)
plot(SpecCurve1)



#'[ Subset Sizes:
data_1b %>% 
  count(data_1b$FinishedWorkouts == "DidntFinishAll")

data_1b %>% 
  count(data_1b$StrongExercisers == "Regular")

#'[ Plot:
plotframe_1b <- study1b %>%
  mutate(Condition = if_else(WorktoReceive == 1, "Work-to-Receive", "Work-to-Unlock"))

plotframe_1b %>%
  count(Condition, TotalWorkoutsCompleted, name = "n") %>%
  group_by(Condition) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(TotalWorkoutsCompleted), y = Percent, fill = Condition)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Work-to-Unlock" = "lightblue", "Work-to-Receive" = "coral3")
  ) +
  labs(x = "Workouts Completed", y = "Percent of Participants", fill = NULL) +
  theme_classic() +
  theme(legend.position = "top", axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))


# Study 1d -----------------------------------------------------
study1d <- read_sav("Paper Daten/Study 1d/Data/Study 1d Data.sav")

#'[ Formatting the Data
data_1d <- study1d %>%
  filter(!(Whatisyourage == 1977)) %>%  #' Removing the Vampire
  mutate(age_c = scale(Whatisyourage, center = TRUE, scale = FALSE)) %>%
  mutate(AgeGroupDecade = case_when(
    Whatisyourage < 30 ~ "Under 30",
    Whatisyourage < 50 ~ "30-50",
    TRUE     ~ "Over 50"
  )
  ) %>% 
  mutate(
    EC_Unlock = case_when(
      WorktoUnlock == 1 ~  1,
      WorktoUnlockSurprise == 1 ~ -1,
      TRUE ~ 0
    ),
    EC_Receive = case_when(
      WorktoReceive == 1 ~  1,
      WorktoUnlockSurprise == 1 ~ -1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    EC2_Unlock = case_when(
      WorktoUnlock == 1 ~  1,
      WorktoReceive == 1 ~ -1,
      TRUE ~ 0
    ),
    EC_Surprise = case_when(
      WorktoUnlockSurprise == 1 ~  1,
      WorktoReceive == 1 ~ -1,
      TRUE ~ 0
    )
  ) # These look like two different types of effect code but that is only because
    # in one case WTU-S functions as the "left-out" group, while the other time it's WTR.

#'[Preparing the custom functions:
lm_1D <- function(formula, data) {
  f <- as.formula(formula)
  if ("EC_Unlock" %in% all.vars(f[[3]])) {
    lm_int <- update(f, . ~ . + EC_Receive)
    
  } else if ("EC_Receive" %in% all.vars(f[[3]])) {
    lm_int <- update(f, . ~ . + EC_Unlock)
    
  } else if ("EC_Surprise" %in% all.vars(f[[3]])){
    lm_int <- update(f, . ~ . + EC2_Unlock)
  }
  lm(lm_int, data = data)
}

NB_1D <- function(formula, data) {
  f <- as.formula(formula)
  if ("EC_Unlock" %in% all.vars(f[[3]])) {
    nb_int <- update(f, . ~ . + EC_Receive)
    
  } else if ("EC_Receive" %in% all.vars(f[[3]])) {
    nb_int <- update(f, . ~ . + EC_Unlock)
    
  } else if ("EC_Surprise" %in% all.vars(f[[3]])){
    nb_int <- update(f, . ~ . + EC2_Unlock)
  }
  glm.nb(nb_int, data = data)
}

log_1D <- function(formula, data) {
  f <- as.formula(formula)
  if ("EC_Unlock" %in% all.vars(f[[3]])) {
    log_int <- update(f, . ~ . + EC_Receive)
    
  } else if ("EC_Receive" %in% all.vars(f[[3]])) {
    log_int <- update(f, . ~ . + EC_Unlock)
    
  } else if ("EC_Surprise" %in% all.vars(f[[3]])){
    log_int <- update(f, . ~ . + EC2_Unlock)
  }
  glm(log_int, data, family = binomial)
} # All these functions are equipped with if-statements which insert the correct condition
                                        # directly into the formula!

tidy_transformed_EC <- function(m) {
  
  tt <- broom::tidy(m, conf.int = TRUE)
  
  # effect-coded condition terms
  ec_terms <- c("EC_Unlock", "EC_Receive", "EC_Surprise")
  idx <- tt$term %in% ec_terms
  
  # if no EC terms in this model, return unchanged
  if (!any(idx)) return(tt)
  
  # keep raw coefficients for checking
  tt$estimate_raw <- tt$estimate
  
  # -----------------------------
  # 1) Negative binomial / Poisson / Logistic
  #    coefficients are on log scale
  # -----------------------------
  if (inherits(m, "negbin") ||
      (inherits(m, "glm") && m$family$family %in% c("poisson", "binomial"))) {
    
    tt$estimate[idx]  <- exp(tt$estimate[idx])  - 1
    tt$conf.low[idx]  <- exp(tt$conf.low[idx])  - 1
    tt$conf.high[idx] <- exp(tt$conf.high[idx]) - 1
    
    return(tt)
  }
  
  # -----------------------------
  # 2) Linear model (lm)
  #    scale relative to Mean of the Means
  # -----------------------------
  if (inherits(m, "lm")) {
    
    dat   <- model.frame(m)
    yname <- names(dat)[1]
    
    mu_grand <- mean(dat[[yname]], na.rm = TRUE)
    
    # safety check
    if (!is.finite(mu_grand) || mu_grand == 0) return(tt)
    
    tt$estimate[idx]  <- tt$estimate[idx]  / mu_grand
    tt$conf.low[idx]  <- tt$conf.low[idx]  / mu_grand
    tt$conf.high[idx] <- tt$conf.high[idx] / mu_grand
    
    return(tt)
  }
  
  tt
}# We need a slightly tweaked version of the transformer. Made with the help of ChatGPT.

#'[ SCA Setup:
SpecCurve2_Setup <- setup(
  data = data_1d,
  x = c("EC_Unlock", "EC_Receive", "EC_Surprise"),
  y = c("TotalProductsShopped", "EvaluateAll10Products"),
  model = c("lm_1D", "NB_1D", "log_1D"),
  controls = c("Whatisyourage", "Gender"),
  fun1 = tidy_transformed_EC,
  fun2  = NULL,
  simplify = F
)

#'[ Filtering invalid combinations:
SpecCurve2_Setup$specs <- SpecCurve2_Setup$specs %>%
  filter(!(model == "log_1D" & y != "EvaluateAll10Products")) %>%
  filter(!(model == "lm_1D" & y == "EvaluateAll10Products")) %>%
  filter(!(model == "NB_1D" & y != "TotalProductsShopped"))

#'[ Running the model:
summary(SpecCurve2_Setup)
SpecCurve2<- specr(SpecCurve2_Setup)
summary(SpecCurve2)
plot(SpecCurve2)


#'[ Plot:
plotframe_1d <- study1d %>%
  mutate(Condition = case_when(
    WorktoReceive == 1 ~ "Work-to-Receive",
    WorktoUnlock == 1 ~ "Work-to-Unlock",
    WorktoUnlockSurprise == 1 ~ "Work-to-Unlock-Surprise"
  )
  )

plotframe_1d %>%
  count(Condition, TotalProductsShopped, name = "n") %>%
  group_by(Condition) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(TotalProductsShopped), y = Percent, fill = Condition)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Work-to-Unlock" = "lightblue", "Work-to-Receive" = "coral3", "Work-to-Unlock-Surprise" = "aquamarine4")
  ) +
  labs(x = "Total Products Shopped", y = "Percent of Participants", fill = NULL) +
  theme_classic() +
  theme(legend.position = "top", axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))




# Proof that Transformer & Effect Coding works --------------------------------------------
Log1D <- glm(EvaluateAll10Products ~ EC2_Unlock + EC_Surprise, family = binomial, data = data_1d)
summary(Log1D)
exp(coef(Log1D)[3]) - 1
SpecCurve2$data$estimate[33]

NB1D <- glm.nb(TotalProductsShopped ~ EC2_Unlock + EC_Surprise, data = data_1d)
summary(NB1D)
exp(coef(NB1D)[3]) - 1
SpecCurve2$data$estimate[29]

LM1D <- lm(TotalProductsShopped ~ EC2_Unlock + EC_Surprise, data = data_1d)
summary(LM1D)
(coef(LM1D)[3])/mean(data_1d$TotalProductsShopped)
SpecCurve2$data$estimate[25]
