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
  #filter((GFP == "Evo1" | RFP == "Evo2")) %>%
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
    starts_with("ratio_mut_over")
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
  breaks = c(10^seq(-2,2)),
  limits = c(0.1, 20)
) +
scale_x_discrete(expand = expansion(add = 0.1)

)+
geom_line(
) +
geom_point(
) +
geom_hline(
  yintercept = 1
) +
labs(
  x = "Generation",
  y = "Ratio GFP/RFP",
  title = "All Evo Mutants vs Wildtype",
  
) +
theme_presentation(
) +
theme(
  axis.title.y = element_text(vjust = -1),
  legend.position = "none",
  #legend.position.inside=c(0.01,0.98),
  legend.justification = c(0, 1),
  legend.text  = element_text(size = 8),
  legend.title = element_text(size = 9),
  legend.key.height = unit(10, "pt"),
  legend.key.width  = unit(10, "pt"),
  legend.background = element_rect(
    color = "black",
    linewidth = 0.4)
) +
facet_wrap(
    ~Category,
    nrow = 2,
    ncol = 5
)

#print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "evo_linegraph_all_conditions.pdf"),
  width = 10,
  height = 4
)