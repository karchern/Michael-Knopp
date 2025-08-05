library(tidyverse)
library(readxl)

#load in data
original_data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_Main_Screen.xlsx')
glimpse(data)

#manipulate data
filtered_data <- original_data %>% 
  filter(GFP == 'wt' & RFP == 'wt') %>% 
  mutate(SampleID = row_number()) %>%
  select(SampleID, Condition, Kpn_abundance_g0, Kpn_abundance_g10, Kpn_abundance_g20, Kpn_abundance_g30)%>%
  mutate(Condition= fct_inorder(Condition))
filtered_data

#pivot_data
long_data <- pivot_longer(filtered_data, cols = starts_with('Kpn_abundance'), #columns with names that start with Kpn_abundance
        names_to = 'Timepoint',  #names go into new column called timepoint
        values_to = 'Abundance') #the values in columns become 1 abundance column
long_data

#agreggate replicates
long_data_aggregated <- long_data %>%
 group_by(Condition, Timepoint) %>% 
 summarize(
  mean_abundance = mean(Abundance), 
  sd_abundance = sd(Abundance),
  lower = mean_abundance - sd_abundance,
  upper = mean_abundance + sd_abundance
  )
long_data_aggregated

#plot
ggplot(long_data_aggregated, aes(x = Condition, y = mean_abundance, fill = Timepoint, group = Timepoint)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
      ),
      position=position_dodge()
    ) +
  labs(x = "Sample (Filtered Row)", y = "Kpn Abundance", title = "Kpn Abundance per Sample by Timepoint") +
  theme_minimal()