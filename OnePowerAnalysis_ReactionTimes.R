# install.packges("dplyr", "ggplot2", "lme4", "lmerTest","cowplot")

require(dplyr) #general data manipulation
require(ggplot2) #plotting
require(cowplot) #theme for plotting
theme_set(theme_cowplot())
require(ggdist) #plotting
require(lme4) #for statistics (mixed modelling)
require(lmerTest) #approximate p values for lme4
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path))) #set working directory

#Set up a function that creates a dataset given certain values
Simulate_RT_Data = function(nParticipants,
                            reps,
                            ConditionOfInterest,
                            Mean_Difficulty_Baseline,
                            Mean_Effect,
                            Scale_to_ms,
                            Between_Participant_Variability_Baseline){
  
  #make a vector with an ID for each simulated participant
  ID = paste0("p0",1:nParticipants)
  
  #make a dataframe from the given values to work with, with one row per trial per participant and condition
  ReactionTimes = expand.grid(ID=ID, 
                             ConditionOfInterest=ConditionOfInterest,
                             reps = 1:reps)
  
  
  ReactionTimes = ReactionTimes %>% 
    group_by(ID) %>% 
    mutate(Mean_Difficulty_Baseline = Mean_Difficulty_Baseline,
           #this determines the shape of the log normal distribution we use to model reaction times
           #see: https://lindeloev.github.io/shiny-rt/
           #see also (for plots): https://en.wikipedia.org/wiki/Log-normal_distribution
           
           Mean_Effect = Mean_Effect,
           #Increase in difficulty as a fraction of mean difficulty of baseline
           
           Scale_to_ms = Scale_to_ms,
           #this scale parameter increases the numbers drawn from the log normal distribution to
           #millisecond values that make sense
           
           Between_Participant_Variability_Baseline = Between_Participant_Variability_Baseline,
           #we will represent this as a normal distribution around the baseline difficulty
           Var_per_Participant = rnorm(1, 0, Between_Participant_Variability_Baseline)) %>% 
    group_by(ID, ConditionOfInterest) %>% 
    mutate(Difficulty_Per_ID_and_Condition = case_when(
                ConditionOfInterest == "Hard" ~ Mean_Difficulty_Baseline + 
                                                Var_per_Participant +
                                                Mean_Difficulty_Baseline*Mean_Effect,
                ConditionOfInterest == "Easy" ~ Mean_Difficulty_Baseline + 
                                                Mean_Difficulty_Baseline*Var_per_Participant),
           
           #draw reaction times from a log normal distribution and multiply them by the scale parameter
           ReactionTime = rlnorm(length(ID), Difficulty_Per_ID_and_Condition, 0.5)*Scale_to_ms)
  
  ReactionTimes
}

ReactionTimes = Simulate_RT_Data(nParticipants = 20,
                             reps = 200,
                             ConditionOfInterest = c("Easy", "Hard"),
                             Mean_Difficulty_Baseline = 0.8,
                             Mean_Effect = 0.5,
                             Scale_to_ms = 100,
                             Between_Participant_Variability_Baseline = 0.075)

#Check that the distribution of values generated here makes sense
ggplot(ReactionTimes %>% filter(ReactionTime < 1000), aes(ReactionTime, color = ConditionOfInterest)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 1000))

ConditionOfInterest = c("Easy", "Hard")
#Give names to the conditions you're interested in

Mean_Difficulty_Baseline = 0.8
#What do you expect the mean of the log normal distribution to be? Higher number = higher difficulty

Range_Mean_Effects = c(0.1, 0.2, 0.3)
#What's the difference between the two conditions (in percent of the baseline difficulty)

Scale_to_ms = 100
#We multiply the values drawn from the log normal by this value to achieve values in ms

Between_Participant_Variability_Baseline = 0.075
#What's the between-participant variability in the baseline condition?
#This is the standard deviation of a normal distribution (mean = 0) that we draw one value from for each participant
#We then add this value to the baseline difficulty parameter to get a baseline estimate that includes
#between participant variability

#how many datasets do we simulate?
nIterations = 100

#we create a loop in which we simulate datasets + analyses for a range of participants,
#a range of repetitions per condition
#and a range of effect sizes
#everything is optional to combine. Mix and match. Can't increase number of trials? How about more participants?
#Wanna check how small of a effect you can detect with your resources? Simulate for a range of fairly small effect sizes
#The world is your mf oyster!
# PowerfulDataframe_RT = data.frame()
# for (nParticipants in c(10,15,20)){
#   for (reps in c(10, 20, 30)){
#     for (Mean_Effect in Range_Mean_Effects){
#       
#       TimeStartTrial = Sys.time() #get time at beginning of trial
#       
#       for(i in 1:nIterations){
#         
#         print(paste0("Number of Participants: ", nParticipants))
#         print(paste0("Number of reps: ", reps))
#         print(paste0("Iteration: ", i))
#         print(paste0("Mean_Effect: ", Mean_Effect))
#         
#         
#         #use our function to 
#         ReactionTimes = Simulate_RT_Data(nParticipants,
#                                                  reps,
#                                                  ConditionOfInterest,
#                                                  Mean_Difficulty_Baseline,
#                                                  Mean_Effect = Mean_Effect,
#                                                  Scale_to_ms,
#                                                  Between_Participant_Variability_Baseline)
#         
#         GLMM = glmer(ReactionTime ~ ConditionOfInterest + (ConditionOfInterest | ID),
#                       data = ReactionTimes,
#                       family = gaussian(link = "log"),
#                       control=glmerControl(optimizer="bobyqa"))
#         
# 
#         #save everything into
#         PowerfulDataframe_RT = rbind(PowerfulDataframe_RT,data.frame(nParticipants = nParticipants,
#                                                                rep = reps,
#                                                                Effect_Size = Mean_Effect,
#                                                                pvalue = summary(GLMM)$coefficients["ConditionOfInterestHard","Pr(>|z|)"],
#                                                                estimate = summary(GLMM)$coefficients["ConditionOfInterestHard","Estimate"],
#                                                                iteration = i))
#       }
#       
#       save(PowerfulDataframe_RT, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SavedVariables/PowerfulDataframe_RT.RData"))
#     }
#   }
# }

load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/PowerfulDataframe_RT.RData"))

alpha = 0.05 #set alpha to 0.05

PowerfulDataframe_RT = PowerfulDataframe_RT %>% group_by(nParticipants,rep, Effect_Size) %>% 
  mutate(Power = mean(pvalue < alpha)) %>% 
  slice(1) %>% 
  mutate(n_trials = paste0(rep, " Trials"),
         Effect = paste0(paste0(Effect_Size*100,"% of Baseline")))

#plot the power
ggplot(PowerfulDataframe_RT, 
       aes(nParticipants,Power, color = as.factor(Effect))) +
  geom_line(linewidth = 1) +
  geom_point() +
  xlab("Number of Participants") +
  ylab("Power") +
  geom_hline(yintercept = 0.8, linetype=1) +
  geom_hline(yintercept = 0.9, linetype=2) +
  geom_hline(yintercept = 0.95, linetype=3) +
  ylim(c(0,1)) +
  facet_wrap(.~n_trials) +
  scale_color_manual(values = c("orange","purple","red"),
                     name = "Strength of Effect")
ggsave("Figures/FigureOnePowerAnalysis.jpg",w = 9, h = 5)
