#Preamble----------------------------------------------------------
library("Rmisc")
library("tidyverse")
library("lme4")
library("lmerTest")
library("MuMIn")
library("optimx")

setwd("/home/matt/Research/Counterfactuals/Wheel/Barely Video (Winning)/")

read.csv("barelywinning.csv") -> data

data%>%
  count(Subject)%>%
  summarise(sum = sum(n))

#Modify Data------------------------------------------------------

data%>%
  select(contains("Subject"), contains("LOSE"))%>%
  gather("Wins", "Rating", 2:85)%>%
  filter(Rating!=11)%>%
  count(Subject)%>%
  filter(n>6)%>% 
  select(contains("Subject")) -> error.list

data%>%
  select(-contains("LOSE"))%>%
  gather("Wins","Value", 4:75)%>%
  filter(Value==11)%>%
  count(Subject)%>%
  filter(n>1)%>%
  select(contains("Subject")) -> error.list2

data%>%
  select(-contains("LOSE"))%>%
  gather("Stim","Value", 4:75)%>%
  filter(Value==11) -> na.values

data%>%
  select("Time", "Subject")%>%
  filter(Time<1000)%>%
  select("Subject") -> time.list  

data%>%
  select(-contains("LOSE"), -contains("IPAddress"), -contains("Time"))%>%
  anti_join(.,error.list, by="Subject")%>%
  anti_join(.,time.list, by="Subject")%>%
  anti_join(.,error.list2, by="Subject")%>%
  gather("Stim", "Rating", 2:73)%>%
  anti_join(.,na.values, by=c("Subject", "Stim"))%>%
  separate(Stim, c("Word", "Speed", "Border", "Closeness", "Color"))%>%
  mutate(Subject=as.factor(Subject))%>%
  filter(Rating!="NA") -> mod.data

union(error.list2, error.list, by="Subject") ->e1

anti_join(time.list, e1, by="Subject")

#BoxPlot of Participants-----------------------------------------------------

mod.data2%>%
  ggplot(aes(x=Subject, y=Rating))+
  geom_boxplot()

mod.data%>%
  filter(Subject!=22, Subject!=55, Subject!=64, Subject!=100, Subject!=107, Subject!=110, Subject!=109, Subject!=101)-> mod.data2

#Analysis(Word)--------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySE(measurevar = "Rating", groupvars = "Word", na.rm = TRUE)

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WORD

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Speed + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WORD

anova(model.WORD, null.model.WORD) #NS

#Analysis(Speed)-------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>% 
  summarySEwithin(measurevar = "Rating", withinvars = "Speed", idvar = "Subject", na.rm = TRUE)


mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.SPEED
  
mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.SPEED
  
anova(model.SPEED, null.model.SPEED) #Significant

#Analysis(Border)-------------------------------------------------------------

mod.data%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(measurevar = "Rating", withinvars = "Border", idvar = "Subject", na.rm = TRUE)

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.BORDER

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.BORDER

anova(model.BORDER, null.model.BORDER) #Significant

#Analysis(Closeness)------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(measurevar = "Rating", withinvars = "Closeness", idvar = "Subject", na.rm = TRUE)


mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Border + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.CLOSENESS 

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Border + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.CLOSENESS

anova(model.CLOSENESS, null.model.CLOSENESS) #Significant 

#Analysis(SpeedxBorder)-------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(measurevar = "Rating", withinvars = c("Speed", "Border"), idvar = "Subject", na.rm = TRUE) ->setest

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Speed * Border + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.SxB

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Speed + Border + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.SxB

anova(model.SxB, null.model.SxB) #Significant 

mod.data2 %>%
  filter(Speed!="FILL")%>%
  group_by(Speed, Border) %>%
  summarise(mean = mean(Rating, na.rm=TRUE)) %>%
  ggplot(aes(x=Border, y=mean, group=Speed)) +
  geom_line(linetype=1, size=1) +
  geom_point(aes(shape=Speed), size=4, color="black", fill="white", stroke=1)+
  scale_shape_manual(labels = c("Quick", "Slow"), values=c(0,1))+
  geom_errorbar(width=.2, aes(ymin=mean - setest$se, ymax=mean + setest$se), size = .4) +
  scale_y_continuous(limits = c(1,10), breaks = c(1:10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("Exited Lose", "Entering Lose")) + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .9), strip.text = element_text(size=20))



#Analysis(SpeedxCloseness)--------------------------------------------------------

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Speed * Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.SxC

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Speed + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.SxC

anova(model.SxC, null.model.SxC) #Significant 


#Analysis(WordxBorder)-------------------------------------------------------------

mod.data%>%
  filter(Speed!="FILL")%>%
  group_by(Word, Border)%>%
  summarise(mean=mean(Rating))

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(., measurevar = "Rating", withinvars = c("Border"), betweenvars = "Word",
                  idvar = "Subject", na.rm=TRUE, conf.interval=.95) -> se_test
mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word * Border + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxB

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Border + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxB

anova(model.WxB, null.model.WxB) #Significant

#Analysis(WordxSpeed)-----------------------------------------------------------------------

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word * Speed + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxS

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxS

anova(model.WxS, null.model.WxS) #Significant

#Analysis(WordxBorderxSpeed)****-----------------------------------------------------------

  mod.data%>%
    filter(Speed!="FILL")%>%
    group_by(Word, Border, Speed)%>%
    summarise(mean=mean(Rating))
  
  mod.data2 %>%
    filter(Speed!="FILL")%>%
    lmer(Rating ~ Word * Border * Speed + (1+Border+Closeness|Subject), ., 
         REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxBxS
  
  mod.data2 %>%
    filter(Speed!="FILL")%>%
    lmer(Rating ~ Word + Border + Speed + (1+Border+Closeness|Subject), ., 
         REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxBxS
  
  anova(model.WxBxS, null.model.WxBxS) #Significant

#Analysis(WordxBorderxCloseness)------------------------------------------------------
  
mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word * Border * Closeness + (1+Border+Closeness|Subject), ., 
         REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxBxC
  
mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Border + Closeness + (1+Border+Closeness|Subject), ., 
         REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxBxC
  
anova(model.WxBxC, null.model.WxBxC) #Significant

#Graph(WordxBorderxCloseness)---------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(., measurevar = "Rating", withinvars = c("Border", "Closeness"), betweenvars = "Word",
                  idvar = "Subject", na.rm=TRUE, conf.interval=.95) -> se3

lab4 <- c("n" = "Near Proximity", "f" = "Far Proximity")


mod.data2 %>%
  filter(Speed!="FILL")%>%
  group_by(Word, Border, Closeness) %>%
  summarise(mean = mean(Rating, na.rm=TRUE)) %>%
  ggplot(aes(x=Border, y=mean, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_shape_manual(labels = c("Almost lost", "Barely won"), values=c(0,1))+
  geom_errorbar(width=.2, aes(ymin=mean - se3$se, ymax=mean + se3$se), size = .4) +
  scale_y_continuous(limits = c(1,10), breaks = c(1:10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("Exited Lose", "Entering Lose")) + 
  facet_grid(~Closeness, labeller = as_labeller(lab4), switch="x") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .9), strip.text = element_text(size=20))


  
#Analysis(WordxSpeedxCloseness)--------------------------------------------------------

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word * Speed * Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxSxC

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Speed + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxSxC

anova(model.WxSxC, null.model.WxSxC) #NS
  
#Simple Effect(BarelyxBorder (SLOW ONLY))---------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  filter(Word=="B")%>%
  filter(Speed=="s")%>%
  lmer(Rating ~ Border + Closeness + (1+Border|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.Sim.WxB

mod.data2%>%
  filter(Speed!="FILL")%>%
  filter(Word=="B")%>%
  filter(Speed=="s")%>%
  lmer(Rating ~ Closeness + (1+Border|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.Sim.WxB

anova(model.Sim.WxB, null.model.Sim.WxB) #Significant


#Graph(WordxBorderxSpeed)--------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(., measurevar = "Rating", withinvars = c("Border", "Speed"), betweenvars = "Word",
                  idvar = "Subject", na.rm=TRUE, conf.interval=.95) -> se

lab2 <- c("q" = "Fast Decel", "s" = "Slow Decel")

mod.data2 %>%
  filter(Speed!="FILL")%>%
  group_by(Word, Border, Speed) %>%
  summarise(mean = mean(Rating, na.rm=TRUE)) %>%
  ggplot(aes(x=Border, y=mean, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_shape_manual(labels = c("Almost lost", "Barely won"), values=c(0,1))+
  geom_errorbar(width=.2, aes(ymin=mean - se$se, ymax=mean + se$se), size = .4) +
  scale_y_continuous(limits = c(1,10), breaks = c(1:10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("Exited Lose", "Entering Lose")) + 
  facet_grid(~Speed, labeller = as_labeller(lab2), switch="x") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .2), strip.text = element_text(size=20))


  #Analysis(WordxBorderxSpeedxCloseness)------------------------------------------------

mod.data%>%
  filter(Speed!="FILL")%>%
  group_by(Word, Border, Speed, Closeness)%>%
  summarise(mean=mean(Rating))

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word * Border * Speed * Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> model.WxBxSxC

mod.data2 %>%
  filter(Speed!="FILL")%>%
  lmer(Rating ~ Word + Border + Speed + Closeness + (1+Border+Closeness|Subject), ., 
       REML=FALSE, control = lmerControl(optimizer = "Nelder_Mead")) -> null.model.WxBxSxC


anova(model.WxBxSxC, null.model.WxBxSxC) #Significant


#Graph(WordxBorderxSpeedxCloseness)--------------------------------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  summarySEwithin(., measurevar = "Rating", withinvars = c("Border", "Speed", "Closeness"), betweenvars = "Word",
                  idvar = "Subject", na.rm=TRUE, conf.interval=.95) -> se2

lab2 <- c("q" = "Fast Decel", "s" = "Slow Decel")
lab3 <- c("n" = "Near Proximity", "f" = "Far Proximity")

mod.data2 %>%
  filter(Speed!="FILL")%>%
  group_by(Word, Border, Speed, Closeness) %>%
  summarise(mean = mean(Rating, na.rm=TRUE)) %>%
  ggplot(aes(x=Border, y=mean, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_shape_manual(labels = c("Almost lost", "Barely won"), values=c(0,1))+
  geom_errorbar(width=.2, aes(ymin=mean - se2$se, ymax=mean + se2$se), size = .4) +
  scale_y_continuous(limits = c(1,10), breaks = c(1:10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("Exited Lose", "Entering Lose")) + 
  facet_grid(Closeness~Speed, labeller = labeller(Speed=lab2, Closeness=lab3), switch="x") +
  theme(axis.text = element_text(size=18), 
        axis.title = element_text(size=20), panel.background = element_blank(), panel.border = element_rect(size=2, fill=NA, colour = "black"),
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .92), strip.text = element_text(size=20))

#Simple-Effect Analsyis (WordxBorderxSpeed)-------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  group_by(Subject, Word, Speed, Border, Closeness)%>%
  summarise(Rating=mean(Rating)) -> collapse.data

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="f")%>%
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Border,., paired = TRUE)

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="n")%>%
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>% 
  t.test(Rating~Border,., paired = TRUE)

data.frame(Word = c("Far", "Near"), 
           rawp = c(0.02274, .003873),
           stringsAsFactors = FALSE) -> raw.p.value

p.adjust(raw.p.value$rawp, method = "bonferroni") -> raw.p.value$Bonferroni


#Simple-Effect Analysis (BorderxWord)----------------------------------------

mod.data2%>%
  filter(Speed!="FILL")%>%
  group_by(Subject, Word, Speed, Border, Closeness)%>%
  summarise(Rating=mean(Rating)) -> collapse.data

collapse.data%>%
  filter(Word=="A")%>%
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Border,., paired = TRUE)

collapse.data%>%
  filter(Word=="B")%>%
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Border,., paired = TRUE)

#Simple-Effect Analysis (SpeedxBorder)---------------------------------------

collapse.data%>%
  filter(Speed=="s")%>%
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Border,., paired = FALSE)

#4-Way Interaction Simple Effects---------------------------------------------

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="f")%>%
  filter(Border=="c")%>%
  group_by(Subject, Speed)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Speed,., paired = TRUE)

data.frame(Word = c("B-Q-Far", "B-Q-Near", "A-Q-Far", "A-Q-Near", "x", "y", "z", "w"), 
           rawp = c(0.1039, .08816, .008056, .000687, 1, 1, 1, 1),
           stringsAsFactors = FALSE) -> raw.p.value

data.frame(Word = c("A-N-C", "B-N-C", "A-F-C", "B-F-C", "x", "y", "z", "w"), 
           rawp = c(0.003086, .003943, .2163, .006054, 1, 1, 1, 1),
           stringsAsFactors = FALSE) -> raw.p.value



p.adjust(raw.p.value$rawp, method = "bonferroni") -> raw.p.value$Bonferroni

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="n")%>%
  filter(Border=="c")%>% 
  group_by(Subject, Speed)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Speed,., paired = TRUE)

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="n")%>%
  filter(Border=="d" | Speed == "s")%>% 
  filter(Border=="c" | Speed == "q")%>% 
  group_by(Subject, Speed)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Speed,., paired = TRUE)

collapse.data%>%
  filter(Word=="B")%>%
  filter(Closeness=="n")%>%
  filter(Speed=="s")%>% 
  group_by(Subject, Border)%>%
  summarise(Rating=mean(Rating))%>%
  t.test(Rating~Border,., paired = TRUE)

data.frame(Word = c("Fast Departed v Slow Departed", "Slow Departed v Fast Approach", "Slow Departed v Slow Approach", "x", "y", "z"), 
           rawp = c(0.003943, .02004, .006147, 1, 1, 1),
           stringsAsFactors = FALSE) -> raw.p.value2

p.adjust(raw.p.value2$rawp, method = "bonferroni") -> raw.p.value2$Bonferroni
