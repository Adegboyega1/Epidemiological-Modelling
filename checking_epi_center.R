setwd("C:/Users/ADEBAYO/Desktop/Thesis/Epidemiological modeling")
getwd()
measles <- read.csv("Processed_measles_data.csv")
measles
dim(measles)
names(measles)
str(measles)
summary(measles)
measles_data <- measles[,-1]
measles_data
cases_rate <- measles_data$incidence_per_100000
plot(cases_rate)
levels(cases_rate)
names(measles_data)
library(tidyverse)
test_data_20weeks <- read.csv('model_outputs.csv')
test_data_20weeks['GS_LSTM']

ggplot() +
  geom_line(data=test_data_20weeks, aes(x=Week,y=cases,color=epi_stages)) +
  geom_jitter(data=test_data_20weeks, aes(x=Week,y=BS_CNN_LSTM,color=epi_stages)) 
  





ggplot(data=measles_data, mapping = aes(x=Year ,y=cases, fill = as.factor(Week))) +
  geom_line() +
  facet_wrap(facets = vars(loc))

#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
Average_incidenceRate <- measles_data %>%
  filter(loc=="CALIFORNIA" | loc=="ILLINOIS" | loc=="MICHIGAN" |
           loc=="NEW JERSEY" | loc=="NEW YORK" | loc=="OHIO" | 
           loc=="PENNSYLVANIA" | loc=="TEXAS" | loc=="WISCONSIN" & Year < 1967 )%>%
  summarize(mean(incidence_per_100000))
  
Average_incidenceRate  
  


claifornia_measles <- measles_data %>%
  filter(loc=="CALIFORNIA" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(claifornia_measles,'California_data.csv')

Illinois_measles <- measles_data %>%
  filter(loc=="ILLINOIS" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(Illinois_measles,'Illinois_data.csv')


Michigan_measles <- measles_data %>%
  filter(loc=="MICHIGAN" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(Michigan_measles,'Michigan_data.csv')



New_Jersey_measles <- measles_data %>%
  filter(loc=="NEW JERSEY" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(New_Jersey_measles,'New_Jersey_data.csv')


New_York_measles <- measles_data %>%
  filter(loc=="NEW YORK" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(New_York_measles,'New_York_data.csv')



Ohio_measles <- measles_data %>%
  filter(loc=="OHIO") %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(Ohio_measles,'Ohio_data.csv')



Pennsylvania_measles <- measles_data %>%
  filter(loc=="PENNSYLVANIA" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(Pennsylvania_measles,'PENNSYLVANIA_data.csv')


Texas_measles <- measles_data %>%
  filter(loc=="TEXAS" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(Texas_measles,'Texas.csv')



WISCONSIN_measles <- measles_data %>%
  filter(loc=="WISCONSIN" & Year < 1967) %>%
  select(Year,Week,cases,incidence_per_100000)

write.csv(WISCONSIN_measles,'WISCONSIN_data.csv')

summary(claifornia_measles)

ggplot(data=claifornia_measles, aes(x=Week,y=cases)) +
  geom_jitter() +
  facet_wrap(facets = vars(Year))

cumulative_cases_california <- sum(claifornia_measles$cases)

California_measles_epidemics <- claifornia_measles%>%
  filter(!(Year %in% c(1928, 1929, 1932, 1937, 1941, 1947, 1950, 1963, 1965, 1966))) %>%
  select(Year,Week, cases,incidence_per_100000)


California_measles_epidemics 

write.csv(California_measles_epidemics,'California_measles_epidemics.csv')
summary(California_measles_epidemics)



1018 <- claifornia_measles%>%
  filter(Year == 1930) %>%
  select(Week,cases)

ggplot(data=California_measles_epidemics, aes(x=Week,y=cases)) +
  geom_point(aes(color=Year)) 
ggplot(data=California_measles_epidemics, aes(x=cases, fill=incidence_per_100000)) +
  geom_bar(alpha= 2)

sum(California_measles_epidemics$cases)

California_measles_epidemics_without1942 <- California_measles_epidemics %>%
  filter(!(Year==1942)) %>%
  select(Year,Week,cases)

summary(California_measles_epidemics_without1942)
getwd()

measles_epidemics_stages <- read.csv('epidemic stages.csv')

ggplot(data=measles_epidemics_stages, aes(x=Week,y=cases,color=epi_stages)) +
  geom_jitter() +
  facet_wrap(facets = vars(Year))

measles_epidemics_withStages_firstquater <-  measles_epidemics_stages %>%
  filter(Week<=13)%>%
  select(Year,Week,cases,epi_stages)

measles_epidemics_withStages_secondquater <- measles_epidemics_stages %>%
  filter(Week>=14 & Week<=27)%>%
  select(Year,Week,cases,epi_stages)
measles_epidemics_withStages_thirdquater <- measles_epidemics_stages %>%
  filter(Week>=28 & Week<=40)%>%
  select(Year,Week,cases,epi_stages)

ggplot(data=measles_epidemics_withStages_secondquater, aes(x=cases,y=Week,color=epi_stages)) +
  geom_point() 
write.csv(measles_epidemics_withStages_secondquater, "Second_quater_measles.csv")

measles_epidemics_withStages_fourthquater <- measles_epidemics_stages %>%
  filter(Week > 41)%>%
  select(Year,Week,cases,epi_stages)
  
ggplot(data=measles_epidemics_withStages_fourthquater, aes(x=cases,y=Week,color=epi_stages)) +
  geom_point() 
  
write.csv(measles_epidemics_withStages_fourthquater, "Fouth_quater_measles.csv")
  

Firstq_Initial_stage <- measles_epidemics_withStages_firstquater %>%
  filter(epi_stages == "Initial")%>%
  select(epi_stages)

Firstq_pr_stage <- measles_epidemics_withStages_firstquater %>%
  filter(epi_stages == "Pre-peak")%>%
  select(epi_stages)

Firstq_p_stage <- measles_epidemics_withStages_firstquater %>%
  filter(epi_stages == "Peak")%>%
  select(epi_stages)

Firstq_pk_stage <- measles_epidemics_withStages_firstquater %>%
  filter(epi_stages == "Post-peak")%>%
  select(epi_stages)

Firstq_r_stage <- measles_epidemics_withStages_firstquater %>%
  filter(epi_stages == "Receding")%>%
  select(epi_stages)

Secondq_I_stage <- measles_epidemics_withStages_secondquater %>%
  filter(epi_stages == "Initial")%>%
  select(epi_stages)

Secondq_pr_stage <- measles_epidemics_withStages_secondquater %>%
  filter(epi_stages == "Pre-peak")%>%
  select(epi_stages)
 
Secondq_p_stage <- measles_epidemics_withStages_secondquater %>%
  filter(epi_stages == "Peak")%>%
  select(epi_stages)

Secondq_pk_stage <- measles_epidemics_withStages_secondquater %>%
  filter(epi_stages == "Post_peak")%>%
  select(epi_stages)

Secondq_r_stage <- measles_epidemics_withStages_secondquater %>%
  filter(epi_stages == "Receding")%>%
  select(epi_stages)


thirdq_I_stage <- measles_epidemics_withStages_thirdquater %>%
  filter(epi_stages == "Initial")%>%
  select(epi_stages)

thirdq_pr_stage <- measles_epidemics_withStages_thirdquater %>%
  filter(epi_stages == "Pre-peak")%>%
  select(epi_stages)

thirdq_p_stage <- measles_epidemics_withStages_thirdquater %>%
  filter(epi_stages == "Peak")%>%
  select(epi_stages)

thirdq_pk_stage <- measles_epidemics_withStages_thirdquater %>%
  filter(epi_stages == "Post_peak")%>%
  select(epi_stages)

thirdq_r_stage <- measles_epidemics_withStages_thirdquater %>%
  filter(epi_stages == "Receding")%>%
  select(epi_stages)

fourthq_I_stage <- measles_epidemics_withStages_fourthquater %>%
  filter(epi_stages == "Initial")%>%
  select(epi_stages)

fourthq_pr_stage <- measles_epidemics_withStages_fourthquater %>%
  filter(epi_stages == "Pre-peak")%>%
  select(epi_stages)

fourthq_p_stage <- measles_epidemics_withStages_fourthquater %>%
  filter(epi_stages == "Peak")%>%
  select(epi_stages)

fourthq_pk_stage <- measles_epidemics_withStages_fourthquater %>%
  filter(epi_stages == "Post_peak")%>%
  select(epi_stages)

fourthq_r_stage <- measles_epidemics_withStages_fourthquater %>%
  filter(epi_stages == "Receding")%>%
  select(epi_stages)
