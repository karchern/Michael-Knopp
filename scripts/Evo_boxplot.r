source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

data <- read_csv2(
  here(
    "data",
    "Nic_evolved_clones.csv"
  )
)
print(data)

#manipulate data
filtered_data <- data %>%
  filter((str_starts(GFP, "Evo") | str_starts(RFP, "Evo"))) %>%
  mutate(Mutant = if_else(GFP == "wt", RFP, GFP)) %>%
  select(Mutant, selection_coefficient)
filtered_data

#plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = Mutant,
    y = selection_coefficient,
    fill = Mutant
  )
) +
geom_boxplot(
  position = position_dodge(),
  outlier.shape = NA   # suppress outliers from boxplot
) +
geom_jitter(
  position = position_jitterdodge(
    jitter.width = 0.0
  ),
  alpha = 1,
  size = 0.3
) +
labs(
  x = "Evolved Clones",
  y = "Selection Coefficient",
  title = "Functional Analysis"
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
    "Functional_analysis_evolved_clones.pdf"
  ),
  width = 4.13,
  height = 2.71
)