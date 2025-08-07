library(tidyverse)

#needed for baseR and ggplot2
library(readxl)
data <- read_excel('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/data/Nic_Main_Screen.xlsx')
print(data)
glimpse(data)

view(data) #doesnt work bc im running in an R terminal that doesnt have GUI support?

#filter rows that only contain wt in GFP and RFP column
filtered_data <- data[data$GFP == 'wt' & data$RFP == 'wt',]
dim(filtered_data)

#baseR to plot data as grouped barplot
barplot(cbind(filtered_data$Kpn_abundance_g0, filtered_data$Kpn_abundance_g10, filtered_data$Kpn_abundance_g20, filtered_data$Kpn_abundance_g30), 
main= 'wt vs wt Kpn abundance',
ylab='Kpn abundance',
space= c(0.1,11),
ylim=c(0,100),
col=c("red","green","blue","purple"), 
border=NA,
beside = TRUE,
legend.text = c("g0", "g10", "g20", "g30"))

#cant figure out barplot base R, now trying ggplot2
library(ggplot2)

#this is called pipe-based filtering
filtered_data1 <- data %>% filter(GFP == 'wt' & RFP == 'wt')
filtered_data1 <- filtered_data1 %>% mutate(SampleID = row_number()) #needed to be able to track rows so basically made this column

#data is filtered and given sample ID based on row number, see above
#then reshape data, ggplot doesnt take matrices, it needs "long data"
long_data <- filtered_data1 %>%
    select(SampleID, Condition, Kpn_abundance_g0, Kpn_abundance_g10, Kpn_abundance_g20, Kpn_abundance_g30) %>% #says to keep these 5 columns from table
    pivot_longer(cols = starts_with('Kpn_abundance'), #columns with names that start with Kpn_abundance
        names_to = 'Timepoint',  #names go into new column called timepoint
        values_to = 'Abundance') #the values in columns become 1 abundance column
long_data

#this doesnt work bc c makes a vector, and pivot_longer needs a data frame
#filtered_data1 <- c(filtered_data1$SampleID, filtered_data1$Condition, filtered_data1$Kpn_abundance_g0, filtered_data1$Kpn_abundance_g10, filtered_data1$Kpn_abundance_g20, filtered_data1$Kpn_abundance_g30)
#could have done this
filtered_data1 <- data %>% 
  filter(GFP == 'wt' & RFP == 'wt') %>% 
  mutate(SampleID = row_number()) %>%
  select(SampleID, Condition, Kpn_abundance_g0, Kpn_abundance_g10, Kpn_abundance_g20, Kpn_abundance_g30)%>%
  mutate(Condition= fct_inorder(Condition))
filtered_data1

long_data1 <- pivot_longer(filtered_data1, cols = starts_with('Kpn_abundance'), #columns with names that start with Kpn_abundance
        names_to = 'Timepoint',  #names go into new column called timepoint
        values_to = 'Abundance') #the values in columns become 1 abundance column
long_data1

#plot uses data from long_data. Aes= aesthetics, we tell ggplot how to map data
#x puts each sample (each group of 4 timepoints) on x axis and treats them as discrete (categorical) not continuous
#abundance becomes height of bar
#fill is color of the bar, aka each timepoint
#geom_bar function is how to draw the bars. 
#stat=identity tells it to use actual y values (?)
#position = dodge is so bars are besides and not stacked
long_data_aggregated <- long_data1 %>%
 group_by(Condition, Timepoint) %>% 
 summarize(
  mean_abundance = mean(Abundance), 
  sd_abundance = sd(Abundance),
  lower = mean_abundance - sd_abundance,
  upper = mean_abundance + sd_abundance
  )
long_data_aggregated

ggplot(long_data_aggregated, aes(x = Condition, y = mean_abundance, fill = Timepoint, group = Timepoint)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(
    aes(
      #ymin = mean_abundance - sd_abundance,
      #ymax = mean_abundance + sd_abundance
      ymin = lower,
      ymax = upper
      ),
      position=position_dodge()
    ) +
  #facet_wrap(.~Condition) +
  labs(x = "Sample (Filtered Row)", y = "Kpn Abundance", title = "Kpn Abundance per Sample by Timepoint") +
  theme_minimal()

#little practice
a_function <- function(parameter_1) {
  print(parameter_1 * 3)
  return(parameter_1)
}
