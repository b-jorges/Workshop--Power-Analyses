require(dplyr)
require(ggplot2)
require(purrr)
require(lme4)
require(lmerTest)
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/GetValuesFromPilotData.R"))

############Make a function to simulate the data
#This function simulates one full data depending on a whole bunch of parameters. (Perception is complicated!)
SimulatePsychometricData = function(nParticipants, #how many participants?
                                    ConditionOfInterest1, #name for the first Condition of Interest
                                    ConditionOfInterest2, #name for the second Condition of Interest
                                    StandardValues, #vector with the values for the standard value (i.e., speed of the single sphere in the example)
                                    reps, #how many repetitions per condition? Can range between 30-40-50 for very short staircases, up to several 100s, especially for method of constant stimuli
                                    PSE_Difference_CoI1, #PSE difference between the two levels of CoI 1
                                    PSE_Difference_CoI2, #PSE difference between the two levels of CoI 2
                                    PSE_Interaction, #PSE interaction between CoI 1 and CoI 2
                                    JND_Difference_CoI1, #JND difference between the two levels of CoI 1
                                    JND_Difference_CoI2, #JND difference between the two levels of CoI 2
                                    JND_Interaction, #JND interaction between CoI 1 and CoI 2
                                    Multiplicator_PSE_Standard, #At how many percent of the veridical stimulus strength do we perceive it baseline? (e.g., discrepancies introduced by single sphere versus sphere cloud)
                                    Multiplicator_SD_Standard, #What's the JND at baseline in % of stimulus strength?
                                    Type_ResponseFunction, #which function do we use to simulate the presented stimulus strengths?
                                    #normal or Cauchy for staircases
                                    
                                    SD_ResponseFunction,#how wide should this function be? standard deviation if normal distribution, scale for Cauchy
                                    Mean_Variability_Between,#between participant variability in baseline performance in PSE
                                    SD_Variability_Between){#between participant variability in baseline performance in JND

#make a vector with an ID for each simulated participant
ID = paste0("p0",1:nParticipants)
  
#make a dataframe from the given values to work with, with one row per trial per participant and condition
Psychometric = expand.grid(ID=ID, 
                             ConditionOfInterest1=ConditionOfInterest1,
                             ConditionOfInterest2=ConditionOfInterest2,
                             StandardValues=StandardValues, 
                             reps = 1:reps)

#Factor in between-participant variability for PSE/JND
Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,Mean_Variability_Between), #how much variability is in the means of the psychometric functions between subjects?
           SD_Factor_ID = rnorm(1,1,SD_Variability_Between)) #how much variability is in the standard deviations of the psychometric functions between subjects?


Psychometric = Psychometric %>%
  ungroup() %>%
    mutate(
      
      #factor in "Multiplicator_PSE_Standard", i.e., if there is any difference between presented stimulus strength and baseline PSEs
      Mean_Standard = StandardValues*Multiplicator_PSE_Standard,
      
      #same but for the JND. What's the expected average JND as % of the standard value?
      SD_Standard = StandardValues*Multiplicator_SD_Standard,
      
      #Factor in expected effects for CoI1, CoI2 and their interaction (PSE) 
      Mean_Step1 = case_when(
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "1Fixation" ~ Mean_Standard,
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "2Pursuit" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI2,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "1Fixation" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI1,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "2Pursuit" ~ Mean_Standard + Mean_Standard*PSE_Difference_CoI1 + 
                                                                                                         Mean_Standard*PSE_Difference_CoI2 + 
                                                                                                         Mean_Standard*PSE_Interaction),
      
      #Factor in expected effects for CoI1, CoI2 and their interaction (JND) 
      SD_Step1 = case_when(
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "1Fixation" ~ SD_Standard,
        ConditionOfInterest1 == "1Environment" & ConditionOfInterest2 == "2Pursuit" ~ SD_Standard + JND_Difference_CoI2,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "1Fixation" ~ SD_Standard + JND_Difference_CoI1,
        ConditionOfInterest1 == "2No_Environment" & ConditionOfInterest2 == "2Pursuit" ~ SD_Standard + JND_Difference_CoI1 + 
                                                                                                       JND_Difference_CoI2 + 
                                                                                                       JND_Interaction))

#factor in between-participant variability (PSE_Factor_ID and SD_Factor_ID) into the average PSEs/JNDs per condition
Psychometric = Psychometric %>%
    mutate(
      Mean = Mean_Step1*PSE_Factor_ID,
      SD = SD_Step1*SD_Factor_ID)

#Which function to we pick values from? normal or Cauchy?
if (Type_ResponseFunction == "normal"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rnorm(length(reps),1,SD_ResponseFunction))
    
  } else if (Type_ResponseFunction == "Cauchy"){
    
    Psychometric = Psychometric %>%
      mutate(
        staircase_factor = rcauchy(length(reps),1,SD_ResponseFunction))
    
  } else{
    
    print("distribution not valid")
    
  }

#Scale the presented stimulus strength to the actual values
Psychometric = Psychometric %>%
    mutate(Presented_TestStimulusStrength = Mean*staircase_factor)
  
Psychometric = Psychometric %>%
    mutate(
      #Calculate the probability to choose "standard stimulus is more intense"
      AnswerProbability = pnorm(Presented_TestStimulusStrength,Mean,abs(SD)),
      
      #draw binary yes/no responses based on these probabilities
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )

Psychometric
}


###############################################################
###########################simulate one data set###############
###############################################################
#set the parameters
nParticipants = 200
ConditionOfInterest1 = c("1Environment", "2No_Environment")
ConditionOfInterest2 = c("1Fixation", "2Pursuit")
StandardValues = c(2, 4, 6)
reps = 30
PSE_Difference_CoI1 = PSE_Environment
PSE_Difference_CoI2 = PSE_Pursuit
#for Interaction we got a value of:
PSE_Interaction
#but let's simulate power for a range of values:
Range_PSE_Interaction = PSE_Interaction*c(1,0.5, 0.15) #Performance for CoI2 is 20% lower in presence of CoI1 = NoEnvironment than in presence of CoI1 = Environment
JND_Difference_CoI1 = SD_Environment
JND_Difference_CoI2 = SD_Pursuit
JND_Interaction = SD_Interaction
Multiplicator_PSE_Standard = Mean_Standard
#estimated SDs can be inflated heavily when trial counts are low, so let's divide it by two to get a more appropriate value:
Multiplicator_SD_Standard = Multiplicator_SD_Standard
Type_ResponseFunction = Type_ResponseFunction
SD_ResponseFunction = SD_ResponseFunction
Mean_Variability_Between = Mean_Variability_Between
SD_Variability_Between = SD_Variability_Between

#Build one (large) dataset to check whether the everything was simulated properly
set.seed(657)
Test = SimulatePsychometricData(nParticipants = 1000,
                         ConditionOfInterest1,
                         ConditionOfInterest2,
                         StandardValues,
                         reps,
                         PSE_Difference_CoI1,
                         PSE_Difference_CoI2,
                         PSE_Interaction,
                         JND_Difference_CoI1,
                         JND_Difference_CoI2,
                         JND_Interaction,
                         Multiplicator_PSE_Standard,
                         Multiplicator_SD_Standard,
                         Type_ResponseFunction,
                         SD_ResponseFunction,
                         Mean_Variability_Between,
                         SD_Variability_Between)

#Fit Psychometric Functions to the simulated data
Test_PSEs1 = quickpsy::quickpsy(Test,Presented_TestStimulusStrength,Answer,
                                               grouping = .(ID,StandardValues,ConditionOfInterest1,ConditionOfInterest2),
                                               bootstrap = "none")
Test_PSEs = Test_PSEs1$par %>% filter(parn == "p1")
Test_PSEs$SD_Fitted = (Test_PSEs1$par %>% filter(parn == "p2"))$par

save(Test_PSEs, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SavedVariables/", "Test_PSEs.RData"))
load(file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),, "/SavedVariables/", "Test_PSEs.RData"))


#Sample analysis
Analysis_SimData_PSE = lmer(Mean/StandardValues ~ ConditionOfInterest1*ConditionOfInterest2 + StandardValues + (StandardValues | ID),
            data = Test_PSEs %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))

Intercept_Obs = summary(LMM)$coef["(Intercept)","Estimate"]
Intercept_Sim = summary(Analysis_SimData_PSE)$coef["(Intercept)","Estimate"]
Coef_Environment_Obs = summary(LMM)$coef["EnvironmentNo_Environment","Estimate"]
Coef_Environment_Sim = summary(Analysis_SimData_PSE)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Pursuit_Obs = summary(LMM)$coef["FixationPursuit","Estimate"]
Coef_Pursuit_Sim = summary(Analysis_SimData_PSE)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_Obs = summary(LMM)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]
Coef_EnvironmentXPursuit_Sim = summary(Analysis_SimData_PSE)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]

###Compare Observed SDs to simulated SDs
Analysis_SimData_JND = lmer(SD_Fitted/StandardValues ~ ConditionOfInterest1 * ConditionOfInterest2 +
                (StandardValues| ID),
              data = JointDF %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))

Intercept_SD_Obs = summary(LMM_SD)$coef["(Intercept)","Estimate"]
Intercept_Sim_SD = summary(Analysis_SimData_JND)$coef["(Intercept)","Estimate"]
Coef_Environment_SD_Obs = summary(LMM_SD)$coef["EnvironmentNo_Environment","Estimate"]
Coef_Environment_SD_Sim = summary(Analysis_SimData_JND)$coef["ConditionOfInterest12No_Environment","Estimate"]
Coef_Pursuit_SD_Obs = summary(LMM_SD)$coef["FixationPursuit","Estimate"]
Coef_Pursuit_SD_Sim = summary(Analysis_SimData_JND)$coef["ConditionOfInterest22Pursuit","Estimate"]
Coef_EnvironmentXPursuit_SD_Obs = summary(LMM_SD)$coef["EnvironmentNo_Environment:FixationPursuit","Estimate"]
Coef_EnvironmentXPursuit_SD_Sim = summary(Analysis_SimData_JND)$coef["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"]



###################################################################################
###############################Power Analysis######################################
###################################################################################
ConditionOfInterest1 = c("1Environment", "2No_Environment")
ConditionOfInterest2 = c("1Fixation", "2Pursuit")
StandardValues = c(2, 4, 6)
PSE_Difference_CoI1 = PSE_Environment
PSE_Difference_CoI2 = PSE_Pursuit
#for Interaction we got a value of:
PSE_Interaction
#but let's simulate power for a range of values:
Range_PSE_Interaction = PSE_Interaction*c(1,0.5, 0.15) #Performance for CoI2 is 20% lower in presence of CoI1 = NoEnvironment than in presence of CoI1 = Environment
JND_Difference_CoI1 = SD_Environment
JND_Difference_CoI2 = SD_Pursuit
JND_Interaction = SD_Interaction
Multiplicator_PSE_Standard = Mean_Standard
#estimated SDs can be inflated heavily when trial counts are low, so let's divide it by two to get a more appropriate value:
Multiplicator_SD_Standard = Multiplicator_SD_Standard
Type_ResponseFunction = Type_ResponseFunction
SD_ResponseFunction = SD_ResponseFunction
Mean_Variability_Between = Mean_Variability_Between
SD_Variability_Between = SD_Variability_Between

nIterations = 100
TimeStartSimulations = Sys.time()
PowerfulDataframe = data.frame()

for (nParticipants in c(25,50,75)){
  for (reps in c(30, 50, 70)){
    for (PSE_Interaction in Range_PSE_Interaction){
      
      TimeStartTrial = Sys.time() #get time at beginning of trial
      
      for(i in 1:nIterations){
        
        print(paste0("Number of Participants: ", nParticipants))
        print(paste0("Iteration: ", i))
        print(paste0("PSE_Interaction: ", round(PSE_Interaction,4)))
        
        
        #use our function to 
        Test = SimulatePsychometricData(nParticipants,
                                 ConditionOfInterest1,
                                 ConditionOfInterest2,
                                 StandardValues,
                                 reps,
                                 PSE_Difference_CoI1,
                                 PSE_Difference_CoI2,
                                 PSE_Interaction,
                                 JND_Difference_CoI1,
                                 JND_Difference_CoI2,
                                 JND_Interaction,
                                 Multiplicator_PSE_Standard,
                                 Multiplicator_SD_Standard,
                                 Type_ResponseFunction,
                                 SD_ResponseFunction,
                                 Mean_Variability_Between,
                                 SD_Variability_Between)
        
        #Analysis Option 1: 1-step-approach using generalized linear mixed modelling
        GLMM = glmer(Answer ~ ConditionOfInterest1*ConditionOfInterest2 + Presented_TestStimulusStrength + (Presented_TestStimulusStrength| ID) + (Presented_TestStimulusStrength| StandardValues), 
                     family = binomial(link = "logit"), 
                     data = Test %>% filter(Presented_TestStimulusStrength < StandardValues*3 & Presented_TestStimulusStrength > 0),
                     nAGQ = 1,
                     glmerControl(optimizer = "nloptwrap"))
        
        #Analysis Option 2: 2-step-approach by first fitting PSEs/JNDs and then using linear mixed modelling for stats
        Test_PSEs1 = quickpsy::quickpsy(Test %>% filter(Presented_TestStimulusStrength < StandardValues*3 & Presented_TestStimulusStrength > 0),Presented_TestStimulusStrength, Answer,
                                                 grouping = .(ID,StandardValues,ConditionOfInterest1,ConditionOfInterest2),
                                                 bootstrap = "none")
        Test_PSEs = Test_PSEs1$par %>% filter(parn == "p1")
        Test_PSEs$SD_Fitted = (Test_PSEs1$par %>% filter(parn == "p2"))$par
        
        #Analysis Option 2a: interaction
        LMM = lmer(par ~ ConditionOfInterest1*ConditionOfInterest2 + (StandardValues | ID),
                   data = Test_PSEs %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues))
        
        #Analysis Option 2b: we should see an effect when there is no environment and no effect
        #when there is an environment
        LMM_Split1 = lmer(par ~ ConditionOfInterest2 + (StandardValues | ID),
                   data = Test_PSEs %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues) %>% 
                     filter(ConditionOfInterest1 == "1Environment"))
        
        LMM_Split2 = lmer(par ~ ConditionOfInterest2 + (StandardValues | ID),
                          data = Test_PSEs %>% filter(SD_Fitted > 0 & SD_Fitted < 3*StandardValues & par > 0 & par < 3 * StandardValues) %>% 
                            filter(ConditionOfInterest1 == "2No_Environment"))
        
        #save everything into
        PowerfulDataframe = rbind(PowerfulDataframe,data.frame(nParticipants = rep(nParticipants,4),
                                                      rep = rep(reps,4),
                                                      PSE_Interaction = rep(PSE_Interaction,4),
                                                      WhichValue = c("PSE_GLMM","PSE_LMM","PSE_LMM_Split_Environment","PSE_LMM_Split_NoEnvironment"),
                                                      pvalue = c(summary(GLMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|z|)"],
                                                                 summary(LMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Pr(>|t|)"],
                                                                 summary(LMM_Split1)$coefficients["ConditionOfInterest22Pursuit","Pr(>|t|)"],
                                                                 summary(LMM_Split2)$coefficients["ConditionOfInterest22Pursuit","Pr(>|t|)"]),
                                                      estimate = c(summary(GLMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"],
                                                                   summary(LMM)$coefficients["ConditionOfInterest12No_Environment:ConditionOfInterest22Pursuit","Estimate"],
                                                                   summary(LMM_Split1)$coefficients["ConditionOfInterest22Pursuit","Estimate"],
                                                                   summary(LMM_Split2)$coefficients["ConditionOfInterest22Pursuit","Estimate"]),
                                                      iteration = rep(i,4)))
      }
      print(paste0(nIterations, " iterations took ", round(Sys.time() - TimeStartTrial), " seconds.")) 
      print(paste0("The power for the current run through (",nParticipants," Participants, ", reps, " Repetitions) is ",mean(PowerfulDataframe$pvalue[PowerfulDataframe$nParticipants == nParticipants] < 0.05)))
      save(PowerfulDataframe, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SavedVariables/PowerfulDataframe.RData"))
    }
  }
}

load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PowerfulDataframe.RData"))
DurationSimulations = Sys.time() - TimeStartSimulations
colnames(PowerfulDataframe) = c("nParticipants","rep","PSE_Interaction", "WhichValue","pvalue","estimate","iteration")
alpha = 0.05

PowerfulDataframe = PowerfulDataframe %>% group_by(nParticipants,rep, WhichValue,PSE_Interaction) %>% 
  mutate(Power = mean(pvalue < alpha))

PowerfulDataframe %>% group_by(nParticipants,WhichValue,PSE_Interaction,rep) %>% 
  slice(1)

PowerfulDataframe = PowerfulDataframe %>% 
  mutate(n_trials = paste0(rep, " Trials"),
         AnalysisMethod = case_when(
           WhichValue == "PSE_LMM" ~ "LMM",
           TRUE ~ "GLMM"))

ggplot(PowerfulDataframe %>% 
         filter(PSE_Interaction %in% sort(unique(PowerfulDataframe$PSE_Interaction))[1:3]), 
       aes(nParticipants,Power, color = as.factor(PSE_Interaction))) +
  geom_line(linewidth = 1) +
  geom_point() +
  xlab("Number of Participants") +
  ylab("Power") +
  geom_hline(yintercept = 0.8, linetype=1) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(AnalysisMethod~n_trials) +
  scale_color_manual(values = c("orange","purple","red"),
                     labels = c("100% of Pilot", "50% of Pilot", "15% of Pilot"),
                     name = "Strength of Effect")
ggsave("Figures/FigureOnePowerAnalysis.jpg",w = 7, h = 7)