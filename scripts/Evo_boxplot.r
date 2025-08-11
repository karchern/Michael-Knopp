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
  mutate(clone = if_else(GFP == "wt", RFP, GFP)) %>%
  select(clone, selection_coefficient)
filtered_data

#plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = clone,
    y = selection_coefficient,
    fill = clone #position_jitterdodge() needs 1 aesthetic to dodge by
  )
) +
scale_y_continuous(limits = c(0, 0.09)
) +
geom_boxplot(
  position = position_dodge(),
  outlier.shape = NA,   # suppress outliers from boxplot
  show.legend = FALSE
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
  title = "Fitness Assay of Evolved Clones"
) +
theme_presentation(
) +
theme(
  axis.text.x = element_text(
    angle = 45,
    hjust = 1),
  legend.position = "FALSE"
)

print(plot_object)

ggsave(
  plot = plot_object,
  filename = here(
    "results",
    "plots",
    "Fitness_assay_of_evolved_clones.pdf"
  ),
  width = 4.13,
  height = 2.71
)