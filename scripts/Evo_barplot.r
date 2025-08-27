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
    Generation = str_remove(Generation,"Kpn_abundance_g")
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
geom_bar(
  stat = "identity",
  position = position_dodge(width = 0.8),
  width = 0.7 #,
  #color = "black"
) +
geom_errorbar(
   aes(
    ymin = lower,
    ymax = upper
    ),
  position = position_dodge(width = 0.8),
  width = 0.5,
  linewidth = 0.4
) +
scale_x_discrete(expand = expansion(add = 0.5)
)+
scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))
) +
labs(
  x = "Evolved Clone",
  y = "Kpn Abundance (%)",
  title = "Kpn Abundance in Evolved Clones"
) +
theme_presentation(
) +
theme(
  panel.border = element_rect(
    color = "black",
    linewidth = 0.8),  # thickness of border line
  axis.text.x = element_text(
    angle = 45,
    hjust = 1),
  legend.position = c(0.99, 0.985),
  legend.justification = c(1, 1),
  legend.text  = element_text(size = 6),
  legend.title = element_text(size = 7),
  legend.key.height = unit(4, "pt"),
  legend.key.width  = unit(8, "pt"),
  legend.background = element_rect(
    color = "black",
    linewidth = 0.4))


#print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "Kpn_abundance_in_evolved_clones.pdf"),
  width = 5.5,
  height = 4.5)