#Take the ratio change of Evo1 and Evo2. They are not isogenic but it’s good to put an example of a GFP and RFP tagged strain.
#for the supplements I guess we can have scatter plots for each strain individually
library(tidyverse)
library(readxl)

data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.xlsx')
#data1 <- read_csv('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.csv')

print(data)
#print(data1) #why doesnt this work ?

#manipulate data
filtered_data <- data %>% 
  filter((GFP== 'Evo1' | RFP == 'Evo2'))%>%
  mutate(SampleID = row_number(),
  Category = if_else(GFP == 'wt', str_c('wt(GFP) vs ', RFP, '(RFP)'), str_c(GFP, '(GFP) vs wt(RFP)'))
  

  #case_when(
      #GFP == 'Evo1' & RFP == 'wt' ~ 'wt(RFP) vs Evo1(GFP)', #if this combo, give this in category col
     # GFP == 'wt' & RFP == 'Evo2' ~ 'wt(GFP) vs Evo2(RFP)', #if this combo, give this in category col
      #TRUE ~ 'other'), #to catch any rows that didnt fit with those
  ) %>%
  select(Category, SampleID,ratio_mut_over_wt_g0, ratio_mut_over_wt_g10, ratio_mut_over_wt_g20, ratio_mut_over_wt_g30)
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
  labs(x = "Generation", y = "Ratio", title = "Evo1 and Evo2 to wt ratio over time") +
  theme_minimal()

ggsave('Evo1_Evo2_linegraph.pdf', plot=last_plot(), path = '/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/results/plots/')