#Change in abundance of wt vs wt, ompK vs wt, and each evo over 4 days
library(tidyverse)
library(readxl)

#load in data
data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.xlsx')
#data1 <- read_csv('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_evolved_clones.csv')

print(data)
#print(data1) #why doesnt this work ?

#manipulate data
filtered_data <- data %>% 
  mutate(Mutant = if_else(GFP == 'wt', RFP, GFP)) %>% #if GFP column says wt, give RFP column value, if not give GFP column value
  mutate(Mutant= fct_inorder(Mutant)) %>% #keeps this column in order presented instead of alphabetical order
  select(Mutant, starts_with('Kpn_abundance')) #columns to move forward with
filtered_data

#pivot_data
long_data <- pivot_longer(filtered_data, cols = starts_with('Kpn_abundance'), #columns with names that start with Kpn_abundance
        names_to = 'Timepoint',  #names go into new column called timepoint
        values_to = 'Abundance') #the values in columns become 1 abundance column
long_data

#agreggate replicates
long_data_aggregated <- long_data %>%
 group_by(Mutant, Timepoint) %>%  #why group by timepoint
 summarize(
  mean_abundance = mean(Abundance), 
  sd_abundance = sd(Abundance),
  lower = mean_abundance - sd_abundance,
  upper = mean_abundance + sd_abundance)
long_data_aggregated

#plot
ggplot(long_data_aggregated, aes(x = Mutant, y = mean_abundance, fill =Timepoint, group = Timepoint)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
      ),
      position=position_dodge()
    ) +
  labs(x = "Mutant", y = "Kpn Abundance", title = "Kpn Abundance per Sample by Timepoint") +
  theme_minimal()

ggsave('Evo_bargraph.pdf', plot=last_plot(), path = '/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/results/plots/')