# boxplot of selection coefficients
library(tidyverse)
library(readxl)

data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.xlsx')
#data1 <- read_csv('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.csv')

print(data)
#print(data1) #why doesnt this work ?

#manipulate data, ! in filter removes any rows that have 'wt' in GFP and RFP columns
filtered_data <- data %>% 
  filter((str_starts(GFP, 'Evo') | str_starts(RFP, 'Evo'))) %>% 
  mutate(Mutant=if_else(GFP == 'wt', RFP, GFP))%>% 
  select(Mutant, selection_coefficient)
filtered_data

#plot, still need to remove outliers and do geom_jitter to add in back ALL points
ggplot(filtered_data, aes(x = Mutant, y = selection_coefficient, fill= Mutant)) +
  geom_boxplot(position=position_dodge(width=0.7)) +
  labs(x = "Evolved clones", y = "Selection Coefficient", title = "Functional analysis") +
  theme_minimal()

ggsave('Evo_boxplot.pdf', plot=last_plot(), path = '/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/results/plots/')