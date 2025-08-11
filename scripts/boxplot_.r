source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

#load in data
data <- read_csv2(
  here("data", "Nic_Main_Screen.csv")
  )
print(data)

#manipulate data
filtered_data <- data %>%
  filter(!(GFP == "wt" & RFP == "wt")) %>%
  mutate(
    Mutant = if_else(
      GFP == "wt",
      RFP,
      GFP
    ),
    Condition = fct_inorder(Condition),
    Mutant = fct_inorder(Mutant),
  ) %>%
  select(
    Mutant,
    Condition,
    selection_coefficient
  )
filtered_data

#plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = Condition,
    y = selection_coefficient,
    fill = Mutant
  )
) +
geom_boxplot(
  position = position_dodge(width = 1),
  outlier.shape = NA   # suppress outliers from boxplot
) +
geom_jitter(
  position = position_jitterdodge(dodge.width = 1, jitter.width = 0.4),
  alpha = 1,
  size = 0.1
) + # plot all points (including "outliers")
labs(
  x = "Medium",
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
    "Functional_analysis_in_Medium.pdf"
  ),
  width = 5.8,
  height = 3.89
)
