source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

#load in data
data <- read_csv2(
  here("data", "Nic_evolved_clones.csv")
  )
print(data)

#manipulate data
filtered_data <- data %>%
  mutate(
    Mutant = if_else(
      GFP == "wt",
      RFP,
      GFP
    ),
    Mutant = fct_inorder(Mutant)
  ) %>%
  select(
    Mutant,
    starts_with("Kpn_abundance")
  )
filtered_data

#pivot_data
long_data <- pivot_longer(
  filtered_data,
  cols = starts_with("Kpn_abundance"),
  names_to = "Generation",
  values_to = "Abundance"
) %>%
  mutate(
    Generation = str_remove(Generation,"Kpn_abundance_")
  )
long_data

#agreggate replicates
long_data_aggregated <- long_data %>%
  group_by(
    Mutant,
    Generation
  ) %>%  #why group by timepoint
  summarize(
  mean_abundance = mean(Abundance),
  sd_abundance = sd(Abundance),
  lower = mean_abundance - sd_abundance,
  upper = mean_abundance + sd_abundance
  )
long_data_aggregated

#plot
plot_object <- ggplot(
  long_data_aggregated,
  aes(
    x = Mutant,
    y = mean_abundance,
    fill = Generation,
    group = Generation
  )
) +
geom_bar(stat = "identity", position = position_dodge()) +
geom_errorbar(
  aes(
    ymin = lower,
    ymax = upper
  ),
  position = position_dodge()
) +
labs(
  x = "Evolved Clone",
  y = "Kpn Abundance",
  title = "Kpn Abundance in Mutants"
) +
theme_presentation(
) +
theme(axis.text.x = element_text(
  angle = 45,
  hjust = 1)
)

print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "Kpn_abundance_in_evolved_clones.pdf"),
  width = 5.18,
  height = 3.01)