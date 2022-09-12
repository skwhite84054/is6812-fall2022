library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse)



data_new <- Extracted_data_clean_Aug2022 %>%
  select(-Reporting_type) %>%
  mutate(TRALI_def = factor(TRALI_def),
  TRALI_mention = factor(TRALI_mention),
  Denominator = factor(ifelse(Denominator==1, "Yes", "No")),
  Data_extract = factor(Data_extract),
  Hemovig_country = factor(ifelse(Hemovig_country==1, "Yes", "No")),
  Year_cat=cut(Year, breaks=c(-Inf, 2000, 2010, Inf), labels=c("Before 2000","2000-2010","2011 and after")),
  Year_cat2=cut(Year, breaks=c(-Inf, 2000, 2005, 2010, 2015, Inf), labels=c("Before 2000","2000-2005","2006-2010","2011-2015", "2016 and after"))) 


data_new %>%
  count(Year) %>%
  ggplot(aes(x = Year, y = n)) + geom_line() + theme_bw() + coord_cartesian(xlim =c(1995, 2020), ylim = c(0, 40)) + theme(panel.background = element_blank())

data_new %>%
  filter(!is.na(Patient_num)) %>%
  summarize(quant_patient = quantile(Patient_num))


data_new %>%
  filter(!is.na(Trans_episode_num)) %>%
  summarize(quant_trans = quantile(Trans_episode_num))

data_new %>%
  filter(!is.na(Blood_unit_num)) %>%
  summarize(quant_blood = quantile(Blood_unit_num))

xtabs(~Patient + Trans_episode + Blood_unit, data = data_new)

xtabs(~Context + Journal_type, data = data_new)

xtabs(~Year + TRALI_def + Def_type, data = data_new)


library(summarytools)
dfSummary(data_new, style = "grid", plain.ascii = TRUE)

library(gmodels)
  CrossTable(data_new$Country)
  CrossTable(data_new$Journal)
  CrossTable(data_new$Journal_type, data_new$TRALI_def, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Journal_type, data_new$Study_type, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Journal_type, data_new$Year_cat, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Journal_type, data_new$Population, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Journal_type, data_new$Context, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Year, data_new$TRALI_def)
  table(data_new$Journal)
  table(data_new$Def_type)


library(tidyverse)
  data_new %>%
    drop_na(Limited_pop_type) %>%
    ggplot(aes(x = Limited_pop_type)) + geom_bar(stat = "count") 
  
  data_new %>%
    drop_na(Denominator) %>%
    summarize(medDenominator = median(Denominator, na.rm = TRUE)) 
  
  data_new %>%
    filter(Denominator == "Yes") %>%
     ggplot(aes(x = Denominator)) + geom_histogram(stat="count",bins = 30) + theme_bw() + labs(y = "Number of studies", x = "Denominator in blood products")
  
 #Population and context graphs
  pop_groups <- read_excel("S:/Medical Directorship/Medical Directorship 115/Restricted - Center for Effective Medical Testing/Transfusion Reaction Meta-Analysis/Scoping Review/Incidence TRALI review/pop_group_data.xlsx")
  ggplot(data = pop_groups, aes(x = Population, y = Count)) + geom_col() + theme_bw()
  
  context_groups <- read_excel("S:/Medical Directorship/Medical Directorship 115/Restricted - Center for Effective Medical Testing/Transfusion Reaction Meta-Analysis/Scoping Review/Incidence TRALI review/context_group_data.xlsx")
  ggplot(data = context_groups, aes(x = Context, y = Count)) + geom_col() + theme_bw()
  
#Extra cross-tabs requested in November 2021
  
  #Abstracts vs. full-text

library(gmodels)

  CrossTable(data_new$Article_type, data_new$TRALI_def, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Article_type, data_new$Study_type, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Article_type, data_new$Year_cat, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Article_type, data_new$Population, chisq = TRUE, fisher = TRUE)
  CrossTable(data_new$Article_type, data_new$Context, chisq = TRUE, fisher = TRUE)
  
  CrossTable(data_new$Year_cat2, data_new$Context)
  
ggplot(data = data_new, aes(x = Year_cat2, y = Context, fill = factor(Context)))+
    geom_bar(stat = "summary", position = "dodge", fun = sum) +
    coord_flip()


ggplot(data = data_new, aes(x = Year_cat2, y = Population, fill = factor(Population))) +
  geom_bar(stat = "summary", position = "dodge", fun = sum) +
  coord_flip()

xtabs(~Year + Blood_unit, data = data_new)
xtabs(~Year + Trans_episode, data = data_new)
xtabs(~Year + Patient, data = data_new)
