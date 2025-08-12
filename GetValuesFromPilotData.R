library(ggplot2)
library(quickpsy)
library(dplyr)
library(lme4)
library(lmerTest)
require(cowplot)
theme_set(theme_cowplot())


#Here we load the Aubert-Fleischl data set, do the outlier analysis as outlined in the pre-registration
#then we extract PSEs and JNDs for each condition and participant, which we will then use as base for our power analysis 
Data = read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                       "/Pilot_Data.csv"))

Data = Data %>% 
  mutate(Good = case_when(
              Environment == "No_Environment" ~ 1,
              Mean_Speed_Ball == 0 ~ 1,
              is.nan(Mean_Speed_Ball) ~ 1,
              velH < 0 & Mean_Speed_Ball < velH/2 & Fixation == "Pursuit" ~ 1,
              velH > 0 & Mean_Speed_Ball > velH/2 & Fixation == "Pursuit" ~ 1,
              abs(Mean_Speed_Ball) < abs(velH/3) & Fixation == "Fixation" ~ 1,
              TRUE ~ 0))

Data = Data %>% 
  mutate(GoodPursuit = case_when(
              Environment == "No_Environment" ~ "No Environment",
              Fixation == "Fixation" ~ "Fixation",
              Mean_Speed_Ball == 0 ~ "Eye-Tracking broken",
              is.nan(Mean_Speed_Ball) ~ "Eye-Tracking broken",
              velH < 0 & Mean_Speed_Ball > velH/2 ~ "BadPursuit",
              velH > 0 & Mean_Speed_Ball < velH/2 ~ "BadPursuit",
              velH < 0 & Mean_Speed_Ball <= velH/2 ~ "GoodPursuit",
              velH > 0 & Mean_Speed_Ball >= velH/2 ~ "GoodPursuit"),
         GoodFixation = case_when(
              Environment == "No_Environment" ~ "No Environment",
              Fixation == "Pursuit" ~ "Pursuit",
              abs(Mean_Speed_Ball) <= abs(velH)/3 ~ "GoodFixation",
              abs(Mean_Speed_Ball) > abs(velH)/3 ~ "BadFixation"))

Data$velH = abs(Data$velH)
Data$velH_Pest = abs(Data$velH_Pest)

#Filter out when there is less than 6 valid trials left in a condition:
Data = Data %>% 
  group_by(ID,Environment, Fixation, velH) %>%
  mutate(nGoodPursuitPerCondition = sum(GoodPursuit == "GoodPursuit"),
         nGoodFixationPerCondition = sum(GoodFixation == "GoodFixation"))


###chuck out participants that don't do eye stuff as we want them to
ParticipantGood = Data %>%
  group_by(ID,Environment, Fixation, velH) %>%
  dplyr::select(ID, Environment, Fixation, velH, nGoodPursuitPerCondition,nGoodFixationPerCondition) %>% 
  filter(Environment != "No_Environment") %>% 
  slice(1)

ParticipantGood = ParticipantGood %>% 
  mutate(ConditionGood = case_when(
    nGoodPursuitPerCondition >= 10 | nGoodFixationPerCondition >= 10 ~ 1,
    TRUE ~ 0)) %>% 
  group_by(ID) %>%
  mutate(ParticipantGood = case_when(
    sum(ConditionGood==1) > 4 ~ 1,
    TRUE ~ 0))
WhichParticipantsGood = ParticipantGood %>% 
  group_by(ID) %>% 
  slice(1)
GoodIDs = WhichParticipantsGood$ID[WhichParticipantsGood$ParticipantGood == 1]


CheckQuality = Data %>%
  filter(Environment != "No_Environment") %>% 
  group_by(ID,Fixation, Environment, velH) %>% 
  mutate(HowManyGood = sum(Good == 1)) %>% 
  slice(1) %>% 
  dplyr::select(ID,Fixation,Environment,velH,HowManyGood,nGoodPursuitPerCondition,nGoodFixationPerCondition)

Fits = (quickpsy::quickpsy(Data %>%
                             filter(!(nGoodPursuitPerCondition < 10 & Fixation == "Pursuit" & 
                                      Environment == "Environment_Local_Info") &
                                    !(nGoodFixationPerCondition < 10 & Fixation == "Fixation" & 
                                      Environment == "Environment_Local_Info")) %>% 
                             filter(Good == 1) %>% #only good trials
                             filter(ID %in% GoodIDs), #only good participants
                               x = velH_Pest, 
                               k = Pest_Faster, 
                               grouping = .(Environment,Fixation,velH,ID), 
                               bootstrap = "none"))$parini

Fits = cbind(Fits %>% 
  filter(paran == "p1"),(Fits %>% 
    filter(paran == "p2"))$par)
colnames(Fits) = c(colnames(Fits)[1:5],"PSE","SD")
Fits

#Environment: "No_Environment" means that the ball moved in a fully black environment (Aubert-Fleischl expected)
#             "Environment_Local_Info" means that the ball moved in front of a textured wall (no Aubert-Fleischl expected)
#Fixation: "Fixation" means that participants were fixating a fixation cross throughout the whole part of the experiment
#          "Pursuit" means that participants were following the sphere while it was on screen. (Still fixation during the sphere  cloud part)
#velH: is the speed of the single sphere
#ID: is the participant ID
#PSE: PSE
#SD: 84.6% JND


############################################################
############get values for power simulations################
############################################################
######PSE Differences
######Take values from LMM:
LMM = lmer(PSE/velH ~ Environment*Fixation + velH +
             (velH| ID), 
           data = Fits %>% 
                  filter(SD > 0 & PSE > 0 & PSE < 3 * velH))
summary(LMM)

#what is the average PSE at the intercept (velH = 0, Environment = "Environment_Local_Info", 
#Fixation = "Fixation", interaction = 0)
Intercept = summary(LMM)$coef["(Intercept)","Estimate"]  + mean(ranef(LMM)$ID$'(Intercept)')

#how different is No_Environment from Environment in %
Coef_Environment = summary(LMM)$coef["EnvironmentNo_Environment","Estimate"]*1/Intercept

#how different is Pursuit from Fixation in %
Coef_Pursuit = summary(LMM)$coef["FixationPursuit","Estimate"]*1/Intercept

#What is the interaction between Environment and Fixation in %
Coef_EnvironmentXPursuit = summary(LMM)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]*1/Intercept



#Let's rename these variables to fit the template we use in our simulation function:
#Difference between "Environment" and "No Environment" in percent:
PSE_Environment = Coef_Environment
PSE_Environment
#Difference between "Fixation" and "Pursuit" in percent
PSE_Pursuit = Coef_Pursuit
PSE_Pursuit
#How much slower is the target perceived at "No Environment" when "Pursuit" in comparison to "Environment" and "Pursuit"
PSE_Interaction = Coef_EnvironmentXPursuit
PSE_Interaction

######SD Differences
######Take values from LMM:
LMM_SD = lmer(SD/velH ~ Environment * Fixation + velH +
             (velH| ID), 
           data = Fits %>% filter(SD > 0 & SD < 3*velH & PSE > 0 & PSE < 3 * velH))

#As above, but for the JNDs
Intercept = summary(LMM_SD)$coef["(Intercept)","Estimate"]
Coef_Environment_SD = summary(LMM_SD)$coef["EnvironmentNo_Environment","Estimate"]*1/Intercept
Coef_Pursuit_SD = summary(LMM_SD)$coef["FixationPursuit","Estimate"]*1/Intercept
Coef_EnvironmentXPursuit_SD = summary(LMM_SD)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]*1/Intercept


#Difference between "Environment" and "No Environment" in percent:
SD_Environment = Coef_Environment_SD
SD_Environment

#Difference between "Fixation" and "Pursuit" in percent
SD_Pursuit = Coef_Pursuit_SD
SD_Pursuit

#How does variability differ in "No Environment" when "Pursuit" in comparison to "Environment" and "Pursuit"
SD_Interaction = Coef_EnvironmentXPursuit_SD
SD_Interaction


########Mean Standard
#how fast do we perceive the stimulus on average in the baseline condition (at the intercept)
Mean_Standard = summary(LMM)$coef["(Intercept)","Estimate"] + mean(ranef(LMM)$ID$'(Intercept)')

####Multiplicator_SD_Standard
#what is the the JND as a function of the presented speed (in %)
Multiplicator_SD_Standard = summary(LMM_SD)$coef["(Intercept)","Estimate"] + mean(ranef(LMM_SD)$ID$'(Intercept)')

####Mean_Variability_Between
#what's the between-participant variability in baseline PSEs?
Mean_Variability_Between = sd(ranef(LMM)$ID$'(Intercept)')

####SD_Variability_Between
#what's the between-participant variability in baseline JNDs?
SD_Variability_Between = sd(ranef(LMM_SD)$ID$'(Intercept)')

#we are neglecting variability in the effect size!

####SD_ResponseFunction ... fitting Cauchy and normal functions
#which distribution do the presented values come from? And how wide is it?
require(MASS)
ResponseDistribution = Data %>% 
  group_by(ID, velH) %>%
  mutate(Scale_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$estimate[2],
         SD_Normal = fitdistr(velH_Pest/velH,"normal")$estimate[2],
         loglikelihood_Cauchy = fitdistr(velH_Pest/velH,"cauchy")$loglik,
         loglikelihood_Normal = fitdistr(velH_Pest/velH,"normal")$loglik,
         loglikelihood_Difference = loglikelihood_Cauchy-loglikelihood_Normal) %>% 
  dplyr::select(ID,velH,Scale_Cauchy,loglikelihood_Cauchy,SD_Normal,loglikelihood_Normal, loglikelihood_Difference) %>% 
  slice(1) %>%
  ungroup() %>% 
  summarise(median_Scale_Cauchy = median(Scale_Cauchy),
            median_SD_Normal = median(SD_Normal),
            median_loglike_CauchyMinusNormal = median(loglikelihood_Difference))

if (ResponseDistribution[3] > 0){
  SD_ResponseFunction = ResponseDistribution$median_Scale_Cauchy
  Type_ResponseFunction = "Cauchy"
} else {
  SD_ResponseFunction = ResponseDistribution$median_SD_Normal
  Type_ResponseFunction = "normal"
}
SD_ResponseFunction
Type_ResponseFunction
