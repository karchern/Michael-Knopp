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
  filter((GFP == "Evo1" | RFP == "Evo2")) %>%
  mutate(
    SampleID = row_number(),
    Category = if_else(
    GFP == "wt",
    str_c(
      "wt(GFP) vs ",
      RFP,
      "(RFP)"
    ),
    str_c(GFP,
    "(GFP) vs wt(RFP)")
    )
  ) %>%
  select(
    Category,
    SampleID,
    ratio_mut_over_wt_g0,
    ratio_mut_over_wt_g10,
    ratio_mut_over_wt_g20,
    ratio_mut_over_wt_g30
  )
filtered_data

#pivot data
long_data <- pivot_longer(
  filtered_data,
  cols = starts_with("ratio"),
  names_to = "generation",
  values_to = "ratio"
) %>%
  mutate(
    generation = str_remove(
      generation,
      "ratio_mut_over_wt_g"
    )
  )
long_data

#plot
plot_object <- ggplot(
  long_data,
  aes(
    x = generation,
    y = ratio,
    group = SampleID,
    color = Category)
) +
scale_y_log10(
) +
geom_line(
) +
geom_point(
) +
geom_hline(
  yintercept = 1
) +
labs(
  x = "Generation",
  y = "Ratio",
  title = "Evo1 and Evo2 to wt Ratio Over Time"
) +
theme_presentation(
) +
theme(axis.text.x = element_text()
)

print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "Evo1_Evo2_to_wt_ratio_over_time.pdf"),
  width = 4.31,
  height = 2.35
)