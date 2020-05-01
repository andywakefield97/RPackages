pacman :: p_load(rio)
nk_missile = import('NK_Missile_Tests.xlsx')

head(nk_missile)
summary(nk_missile)


library(ggplot2)
#Graph Showing Missile Type and Distance Travelled
qplot(Missile_Type, Distance_Travelled, data=nk_missile)
qplot(Missile_Type, Distance_Travelled, data=nk_missile, color = Test_Outcome)

#Graph Showing Missile Names and Distance Travelled
qplot(Missile_Name, Distance_Travelled, data=nk_missile, color = Test_Outcome)

#Missile Launches Over Time
qplot(Date, Distance_Travelled, data=nk_missile, color = Missile_Type)

#Basic Bar Chart of Missile Launch outcomes
qplot(Test_Outcome, data=nk_missile, fill=Missile_Type)

#Use of Charts next to each other
qplot(Missile_Type, Distance_Travelled, data=nk_missile, facets=.~ Test_Outcome)
qplot(Missile_Type, Distance_Travelled, data=nk_missile, facets=Test_Outcome~.)


#Adding trendlines/geoms
qplot(Date, Distance_Travelled, data=nk_missile, geom= c("point", "smooth"))
qplot(Date, Distance_Travelled, data=nk_missile, geom= c("point", "smooth"), color=Test_Outcome)


#Cleaning up Key Charts:


qplot(Missile_Type, Distance_Travelled, data=nk_missile,
      color = Test_Outcome,
      main = 'Distance Travelled by each type of missile in North Korea',
      xlab = 'Type of Missile',
      ylab = 'Distance Travelled (KM)')

qplot(Test_Outcome, data=nk_missile, 
      fill=Missile_Type,
      main = 'Missile Test Outcomes in North Korea',
      xlab = 'Test Outcome',
      ylab = 'Number of Tests')


qplot(Date, Distance_Travelled, data=nk_missile,
      geom= c("point", "smooth"),
      color=Test_Outcome,
      main = 'Distance Travelled per Missile test over last 20 years',
      xlab = 'Date',
      ylab = 'Distance Travelled by Missile (KM)')



