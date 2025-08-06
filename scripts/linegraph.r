#change of ratio of the porin mutant vs wt in MB003 
#(ratio_mut_over_wt g0 to g30)
library(tidyverse)
library(readxl)

#load in data
original_data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_Main_Screen.xlsx')
print(original_data)

#manipulate data
filtered_data <- original_data %>% 
  filter(Condition == 'MB003') %>% 
  filter(!(GFP=='wt' & RFP== 'wt'))%>%
  filter((GFP== 'ompK35' | RFP == 'ompK35'))%>%
  mutate(SampleID = row_number(),
    Category = case_when(
      GFP == 'ompK35' & RFP == 'wt' ~ 'wt(RFP) vs ompK35(GFP)', #if this combo, give this in category col
      GFP == 'wt' & RFP == 'ompK35' ~ 'wt(GFP) vs ompK35(RFP)', #if this combo, give this in category col
      TRUE ~ 'other'), #to catch any rows that didnt fit with those
  ) %>%
  select(SampleID, Category, Condition, ratio_mut_over_wt_g0, ratio_mut_over_wt_g10, ratio_mut_over_wt_g20, ratio_mut_over_wt_g30)
filtered_data

#pivot data
long_data <- pivot_longer(filtered_data, cols = starts_with('ratio'), #columns with names that start with ratio
  names_to = 'generation',  
  values_to = 'ratio') %>%
  mutate(generation = str_remove(generation, 'ratio_mut_over_wt_')) #this gets rid of this portion of generation string to make x-axis cleaner
long_data

#plot
ggplot(long_data, aes(x = generation, y = ratio, group = SampleID, color = Category)) +
  geom_line() +
  geom_point() + 
  geom_hline(yintercept= 1) + #gives line at y=1 to see where ratio is 1 to 1
  labs(x = "Generation", y = "Ratio", title = "OmpK35 to wt ratio over time") +
  theme_minimal()

ggsave('linegraph.pdf', plot=last_plot(), path = '/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/results/plots/')
#need to make it generalized in case_when() command so that the other mutants can plotted as well
#same for analysing all the medias