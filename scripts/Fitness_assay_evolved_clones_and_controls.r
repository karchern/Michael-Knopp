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
  #filter((str_starts(GFP, "Evo") | str_starts(RFP, "Evo"))) %>%
  mutate(clone = if_else(GFP == "wt", RFP, GFP),
  clone=fct_inorder(clone)) %>%
  select(clone, selection_coefficient)
filtered_data

#plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = clone,
    y = selection_coefficient
  )
) +
scale_y_continuous(limits = c(-0.04, 0.09),
breaks = pretty_breaks(n=8)
) +
geom_boxplot(
  position = position_dodge(),
  outlier.shape = NA,   # suppress outliers from boxplot
  show.legend = FALSE,
  #color = "blue",
  fill= "#c66dad"
) +
geom_jitter(
  #position = position_jitterdodge(
    #jitter.width = 0.0
  #),
  width = 0.1,
  alpha = 1,
  size = 0.3,
  color = "black"
) +
labs(
  x = "Evolved Clones",
  y = "Selection Coefficient",
  title = "Fitness Assay of Evolved Clones and controls"
) +
theme_presentation(
) +
theme(
  axis.title.x = element_blank(),
  legend.position = "FALSE"
)

#print(plot_object)

ggsave(
  plot = plot_object,
  filename = here(
    "results",
    "plots",
    "Fitness_assay_of_evolved_clones_and_controls.pdf"
  ),
  width = 8,
  height = 6
)