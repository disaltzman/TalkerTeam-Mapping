rm(list = ls(all = TRUE))

# Load packages.
# Data manipulation.
library(data.table)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)
library(parallel)
library(tidyr)

# Plots.
library(ggplot2)
library(cowplot)
library(ggsignif)
library(lemon)

# Analyses.
library(afex)
library(lme4)

# df <- lapply(list.files(path = ".", pattern = "*.csv", all.files = FALSE,
#                         full.names = FALSE, recursive = FALSE), read.csv, header=TRUE)

# set WD to data folder.
setwd("./data")

# Read in files.
file_names <- list.files(path = ".", pattern = "*.csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE)

# Create data frame.
df <- data.frame()

# Loop to create combined dataframe.
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  data$response <- lead(data$response_response, 1) #shifts responses one row up since OpenSesame logs them a row down
  data$response_time <- lead(data$response_time_response, 1) #shifts RTs one row up since OpenSesame logs them a row down
  df <- rbind(df, data,fill=T)
}

#### Clean up data. ####

# Remove columns related to audio onset that were in for debugging purposes
df <- select(df,-9:-26)

# Remove columns where responses/RTs were logged incorrectly by OpenSesame
df <- select(df,-6:-7)

# Create variable to indicate if it was male or female talker on trial
df$gender <- ifelse(grepl("M_",df$current_stimulus),"Male","Female")

# Create variable to indicate which experiment this was
df$exp_orig <- substr(df$subject_nr,1,1) #first digit of subject number indicates experiment number

# Renumbered experiments for manuscript
df$exp_renum <- NA
df$exp_renum[which(df$exp_orig == 1)] <- 1
df$exp_renum[which(df$exp_orig == 2)] <- 4
df$exp_renum[which(df$exp_orig == 3)] <- 3
df$exp_renum[which(df$exp_orig == 4)] <- 2

# Indicate which item number this was within a trial
df$itemNo <- row(as.matrix(df$current_stimulus)) %% 16; df$itemNo[which(df$itemNo==0)] <- 16

# Create variable to add row number for next step
df <- df[order(as.numeric(Trial)),]
df <- df[order(subject_nr),]
df$row <- as.numeric(1:length(df$Trial))

# Responses within lag time (150 ms in M&N 2007) are counted as responses to the previous item
df$corrected_rt <- as.numeric(df$response_time)
df$corrected_rt[which(df$response_time<=150 & df$itemNo > 1)-1] <- df$corrected_rt[which(df$response_time<=150 & df$itemNo > 1)] + 750
df$corrected_rt[which(df$corrected_rt<=150)] <- NA
df <- df %>% drop_na(response)

# Keep only trials in which audio matches target.
df$trial_stimulus <- substr(df$current_stimulus,3,nchar(df$current_stimulus)-4)
df$trialtype <- ifelse(df$Target==df$trial_stimulus,"Target","Distractor")

# Calculate accuracy in obvious way.
df$accuracy <- ifelse(df$trialtype=="Target"&df$response=="space"|df$trialtype=="Distractor"&df$response=="None",1,0)

# Separate target trials for RT analysis.                       
target.trials <- subset(df,df$Target==df$trial_stimulus)

# Remove target trials where participants hit wrong key.
target.trials <- subset(target.trials,target.trials$response=="None"|target.trials$response=="space")

# Keep trials where participant responded immediately after the target trial.
a <- (which(df$Target==df$trial_stimulus&df$response=="None")+1)
missed.targets <- df[a]
missed.targets <- subset(missed.targets, as.numeric(response_time) <= 150)

# Calculate accuracy for complex trials (response in trial after target)
df$accuracy <- ifelse(df$row %in% missed.targets$row,NA,df$accuracy)
df$accuracy <- ifelse(df$row %in% (missed.targets$row-1),1,df$accuracy)

# Remove trials where participants didn't respond in the post-target trial.
# b <- (which(missed.targets$response=="None"))
# c <- missed.targets[b,row]-1
# target.trials <- subset(target.trials,!(row %in% c))
missed.targets <- subset(missed.targets,response=="space")

# Combine these together into single data frame.
critical.trials <- rbind(target.trials,missed.targets)

# Set variables as appropriate types.
critical.trials$Condition <- as.factor(critical.trials$Condition)
critical.trials$Talker <- as.factor(critical.trials$Talker)
critical.trials$response <- as.factor(critical.trials$response)
critical.trials$response_time <- as.numeric(critical.trials$response_time)
critical.trials$subject_nr <- as.factor(critical.trials$subject_nr)

# Order by row number.
critical.trials <- critical.trials[order(row),]

#  Now we can now remove the trials after the target.
critical.trials <- subset(na.omit(critical.trials),Target==trial_stimulus) 

#### Outcomes of interest. ####

# Calculate accuracy.
performance <- df %>%
  group_by(subject_nr,exp_renum) %>%
  summarize(acc=mean(na.omit(accuracy)))

exclude <- filter(performance,acc<=0.9)

critical.trials <- droplevels(critical.trials %>% 
                                filter(!subject_nr %in% exclude$subject_nr))

df.goodSubj <- droplevels(df %>% 
                            filter(!subject_nr %in% exclude$subject_nr))

  
# Average performance by experiment.
avg.performance <- df.goodSubj %>%
  group_by(exp_renum) %>%
  summarize(acc=mean(na.omit(accuracy)))

# Calculate reaction time by condition and talker.
rt <- critical.trials %>%
  group_by(exp_renum, Condition) %>%
  summarise(rt=mean(corrected_rt),SD=sd(corrected_rt))

#### Plots####

# What is distribution of RTs?
ggplot(critical.trials, aes(corrected_rt)) + geom_density()
ggplot(critical.trials, aes(log(corrected_rt))) + geom_density()
# non-transformed RTs look reasonable

# Visualize RTs by subjects
qqnorm(critical.trials$corrected_rt); qqline(critical.trials$corrected_rt); 
qqnorm(log(critical.trials$corrected_rt)); qqline(log(critical.trials$corrected_rt)); 

# Plotting prep
# Mean RTs by subject
rt_summary <- Rmisc::summarySE(data = critical.trials, measurevar = "corrected_rt", 
                               groupvars = c("subject_nr","exp_renum", "Condition"), na.rm = TRUE)

# Create variable with exp_renumeriment labels.
rt_summary$exp_renum <- as.factor(rt_summary$exp_renum)
levels(rt_summary$exp_renum) <- c("Experiment 1 \n Standard design",
                                    "Experiment 2 \n Targets never recycled as distractors and \n targets produced by one talker on each mixed-talker trial",
                                  "Experiment 3 \n Targets never recycled as distractors",
                                  "Experiment 4 \n Targets produced by one talker on each mixed-talker trial")

# For plotting purposes, create numeric version of condition and variable to allow for offsetting of points
rt_summary$CondNum <- ifelse(rt_summary$Condition=="Blocked",1,2)
rt_summary$xPos <- ifelse(rt_summary$Condition=="Blocked",1.25,1.75)

# Mean RTs by group to get CI's for error bar
rt_summary_byGroup <- Rmisc::summarySE(data = critical.trials, measurevar = "corrected_rt",
                                       groupvars = c("exp_renum", "Condition"), na.rm = TRUE)
rt_summary_byGroup$CondNum <- ifelse(rt_summary_byGroup$Condition=="Blocked",1,2)
levels(rt_summary_byGroup$exp_renum) <- c("Experiment 1 \n Standard design",
                                          "Experiment 2 \n Targets never recycled as distractors and \n targets produced by one talker on each mixed-talker trial",
                                          "Experiment 3 \n Targets never recycled as distractors",
                                          "Experiment 4 \n Targets produced by one talker on each mixed-talker trial")

# Create behavioral data figure.
#behav_fig <- 
ggplot(rt_summary, aes(CondNum, corrected_rt, fill=Condition)) + 
  geom_boxplot(aes(group=Condition,fill=Condition),width=0.4) +
  #geom_bar(stat="summary",width=0.4,aes(group=Condition,fill=Condition)) +
  geom_point(data=rt_summary,aes(x=xPos)) + 
  geom_line(data=rt_summary,aes(group=subject_nr,x=xPos),stat="summary") +
  #geom_errorbar(data=rt_summary_byGroup,aes(ymax=corrected_rt+ci, ymin = corrected_rt-ci), width = 0.2) + 
  stat_summary(fun=mean, geom="point", shape=1, size=3) +
  scale_x_continuous("Condition",breaks=c(1,2),labels=c("Blocked","Mixed")) +
  scale_fill_manual(values=c("#ff6e26","#26b7ff")) + 
  facet_rep_wrap(.~exp_renum,ncol=2,repeat.tick.labels=TRUE) +
  labs(y = "Reaction time (ms)") + coord_cartesian(ylim = c(375,725)) + 
  #dark_theme_gray(base_size = 18) + 
  theme(legend.position = "none",strip.text = element_text(size = 16),text=element_text(size=18))

ggsave("behav_fig.png",device="png",type="cairo",dpi="retina",width = 13.5, height = 9)

# Create summary barplot of each exp_renumeriment's MTPC.
rt_differences <- rt[1:3] %>% spread(Condition, rt, drop=TRUE)
rt_differences$diff <- rt_differences$Mixed-rt_differences$Blocked

#MTPC <- 
ggplot(rt_differences,aes(x=as.factor(exp_renum),y=diff)) +
  geom_bar(stat="identity") + labs(y="Multi-talker processing cost (ms)", x="exp_renumeriment") + 
  theme(legend.position = "none",text=element_text(size=16)) + coord_cartesian(ylim=c(0,30)) + 
  geom_signif(xmin=1,xmax=1,y_position=23,tip_length=0,annotation ="***",textsize=8) +
  geom_signif(xmin=3,xmax=3,y_position=23,tip_length=0,annotation ="***",textsize=8)

ggsave("MTPC.png",device="png",type="cairo",dpi="retina")

# Cowplot them together.
# plot_grid(behav_fig,MTPC,labels="AUTO",ncol=1,rel_heights = c(2,1))
# ggsave("plot.png",device="png",type="cairo",dpi="retina",width = 8, height = 10)

# # Line plot of individual MTE
# ggplot(rt_summary,aes(x=Condition,y=corrected_rt)) + geom_point() + 
#   geom_line(aes(group=subject_nr),stat="summary") + theme(legend.position="none") +
#   facet_wrap(~exp_renum) + labs(y = "Reaction time (ms)")
 
# rt_summary_byGroup$exp_renum <- as.factor(rt_summary_byGroup$exp_renum)
# levels(rt_summary_byGroup$exp_renum) <- c("exp_renumeriment 1 \n Standard Design", 
#                                     "exp_renumeriment 2 \n Targets produced by one talker on mixed trials",
#                                     "exp_renumeriment 3 \n Targets never serve as distractors",
#                                     "exp_renumeriment 4 \n Targets produced by one talker on mixed trials \n and targets never serve as distractors")
# 
# # Plotting
# ggplot(rt_summary_byGroup, aes(Condition, corrected_rt)) + geom_boxplot() + facet_wrap(~exp_renum, ncol = 2) +
#   labs(y = "Reaction time (ms)") + ylim(350,700)
# 
# ggplot(rt_summary_byGroup, aes(Condition, corrected_rt)) + 
#   geom_bar(stat = "identity",fill = "#B4B4B4", width = 0.3) +
#   geom_errorbar(aes(ymax = corrected_rt + ci, ymin = corrected_rt - ci), width = 0.2) + 
#   geom_point(data=rt_summary,alpha=0.3) + 
#   geom_line(data=rt_summary,aes(group=subject_nr),stat="summary",alpha=0.3) +
#   facet_wrap(~exp_renum, ncol = 2) +
#   labs(y = "Reaction time (ms)") + coord_cartesian(ylim = c(375,700))

#### Mixed effects models ####

critical.trials$exp_renum <- as.factor(critical.trials$exp_renum)

critical.trials$targRecyc <- NA
critical.trials$targRecyc[critical.trials$exp_renum == "1" | critical.trials$exp_renum == "4"] <- "Yes"
critical.trials$targRecyc[critical.trials$exp_renum == "2" | critical.trials$exp_renum == "3"] <- "No"
critical.trials$targRecyc <- as.factor(critical.trials$targRecyc)

critical.trials$nrTargTalkMixed <- NA
critical.trials$nrTargTalkMixed[critical.trials$exp_renum == "1" | critical.trials$exp_renum == "3"] <- "Two"
critical.trials$nrTargTalkMixed[critical.trials$exp_renum == "2" | critical.trials$exp_renum == "4"] <- "One"
critical.trials$nrTargTalkMixed <- as.factor(critical.trials$nrTargTalkMixed)

# Omnibus
rt_model_slope_gamma_all <- mixed(corrected_rt ~ Condition * targRecyc * nrTargTalkMixed + (Condition||subject_nr),
                          data=critical.trials,family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                          control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_int_gamma_all <-mixed(corrected_rt ~ Condition * targRecyc * nrTargTalkMixed + (1|subject_nr),
                               data=critical.trials,family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

anova(rt_model_slope_gamma_all,rt_model_int_gamma_all) # slope model has better fit.


rt_model_slope_invGauss_all <- mixed(corrected_rt ~ Condition * targRecyc * nrTargTalkMixed + (Condition||subject_nr),
                                  data=critical.trials,family=inverse.gaussian(link="identity"),method="LRT",exp_renumand_re = TRUE,
                                  control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_int_invGauss_all <-mixed(corrected_rt ~ Condition * targRecyc * nrTargTalkMixed + (1|subject_nr),
                               data=critical.trials,family=inverse.gaussian(link="identity"),method="LRT",exp_renumand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

anova(rt_model_slope_invGauss_all,rt_model_int_invGauss_all) # slope model has better fit.

anova(rt_model_slope_invGauss_all, rt_model_slope_gamma_all) #gamma distribution is better

rt_model_slope_gamma_all

# Break this down by experiment


# experiment 1
rt_model_slope_E1 <- mixed(corrected_rt ~ Condition + (Condition||subject_nr),
                                 data=subset(critical.trials, exp_renum=="1"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                           control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_intercept_E1 <- mixed(corrected_rt ~ Condition + (1|subject_nr),
                           data=subset(critical.trials, exp_renum=="1"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                           control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(rt_model_intercept_E1,rt_model_slope_E1) # slope model has better fit.

rt_model_slope_E1
summary(rt_model_slope_E1)

# experiment 2.
rt_model_slope_E2 <- mixed(corrected_rt ~ Condition + (Condition||subject_nr),
                           data=subset(critical.trials, exp_renum=="2"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                           control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_intercept_E2 <- mixed(corrected_rt ~ Condition + (1|subject_nr),
                               data=subset(critical.trials, exp_renum=="2"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(rt_model_intercept_E2,rt_model_slope_E2) # slope model has better fit.

rt_model_slope_E2
summary(rt_model_slope_E2)

# experiment 3.
rt_model_slope_E3 <- mixed(corrected_rt ~ Condition + (Condition||subject_nr),
                           data=subset(critical.trials, exp_renum=="3"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                           control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_intercept_E3 <- mixed(corrected_rt ~ Condition + (1|subject_nr),
                               data=subset(critical.trials, exp_renum=="3"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(rt_model_intercept_E3,rt_model_slope_E3) # slope model has better fit.

rt_model_slope_E3
summary(rt_model_slope_E3)

# experiment 4.
rt_model_slope_E4 <- mixed(corrected_rt ~ Condition + (Condition||subject_nr),
                           data=subset(critical.trials, exp_renum=="4"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                           control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

rt_model_intercept_E4 <- mixed(corrected_rt ~ Condition + (1|subject_nr),
                               data=subset(critical.trials, exp_renum=="4"),family=Gamma(link="identity"),method="LRT",exp_renumand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(rt_model_intercept_E4,rt_model_slope_E4) # slope model has better fit.

rt_model_slope_E4
summary(rt_model_slope_E4)

# Are overall slower RT's in E4 significantly different from previous experiments? Not sure if needed.
# RT_overall <- glmer(corrected_rt ~ exp_renum + (1|subject_nr),data=critical.trials,family=Gamma(link="identity"))
# summary(RT_overall)
# 
# # Post hoc comparisons.
# posthoc <- multcomp::glht(RT_overall, linfct=multcomp::mcp(exp_renum="Tukey"))
# summary(posthoc,test=multcomp::adjusted("bonferroni"))