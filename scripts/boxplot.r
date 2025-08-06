# FA box plot of the selection coefficients such as in my Figure 1D (note that here every value is basically x100), not sure which is better but I guess it’s easy to adjust later
library(tidyverse)
library(readxl)

#load in data
original_data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_Main_Screen.xlsx')
print(original_data)

#manipulate data, ! in filter removes any rows that have 'wt' in GFP and RFP columns
filtered_data <- original_data %>% 
  filter(!(GFP == 'wt' & RFP == 'wt')) %>% 
  mutate(Mutant = case_when(       #if GFP value is wt, take RFP value. vice versa for RFP
    GFP == 'wt' ~ RFP,
    RFP == 'wt' ~ GFP
  )) %>% 
  mutate(Condition= fct_inorder(Condition))%>%
  mutate(Mutant= fct_inorder(Mutant)) %>%
  select(Mutant, Condition, selection_coefficient)
filtered_data

#pivot longer, needed for ggplot ?, not really sure what would be pivoted

#agreggate replicates not needed for boxplots
#long_data_aggregated <- filtered_data %>%
 #group_by(Mutant, Condition) %>% 
 #summarize(
  #mean_coeff = mean(selection_coefficient), 
  #sd_coeff = sd(selection_coefficient),
  #lower = mean_coeff - sd_coeff,
  #upper = mean_coeff + sd_coeff
  )
#long_data_aggregated

#plot, still need to remove outliers and do geom_jitter to add in back ALL points
ggplot(filtered_data, aes(x = Condition, y = selection_coefficient, fill= Mutant)) +
  geom_boxplot(position=position_dodge(width=0.7)) +
  labs(x = "Condition", y = "Selection Coefficient", title = "Functional analysis") +
  theme_minimal()
