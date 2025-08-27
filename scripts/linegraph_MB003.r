source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

#load in data
data <- read_csv2(
  here("data", "Nic_Main_Screen.csv")
  )
print(data)

#manipulate data
filtered_data <- data %>%
  filter(Condition == "MB003") %>%
  filter(!(GFP == "wt" & RFP == "wt")) %>%
  filter((GFP == "ompK35" | RFP == "ompK35")) %>%
  mutate(
    GFP = recode(GFP, "RFP" = "wt"),
    RFP = recode(RFP,  "RFP" = "wt"),
    SampleID = row_number(),
    Category = if_else(
      GFP == "wt",
      str_c(
        "wt(GFP) vs ",
        RFP,
        "(RFP)"
      ),
      str_c(
        GFP,
        "(GFP) vs wt(RFP)"
      )
    )
  ) %>%
  select(
    SampleID, 
    Category, 
    Condition, 
    starts_with("ratio")
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
geom_line(
) +
geom_point(
) +
scale_y_log10(
  breaks = 10^seq(-2, 2),
  limits = c(0.1, 10)
) +
scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))
) + 
geom_hline(yintercept = 1
) + #gives line at y=1 to see where ratio is 1 to 1
labs(
  x = "Generation",
  y = "Ratio of Mutant/wt",
  title = "MB003 OmpK35 to wt Ratio Over Time"
) +
theme_presentation(
) +
theme(
  axis.text.x = element_text(),
  legend.position = c(0.99, 0.98),
  legend.justification = c(1, 1),
  legend.text  = element_text(size = 6),
  legend.title = element_text(size = 7),
  legend.key.height = unit(4, "pt"),
  legend.key.width  = unit(8, "pt"),
  legend.background = element_rect(
    color = "black",
    linewidth = 0.4)
)

#print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "MB003:OmpK35_to_wt_ratio_over_time.pdf"
  ),
  width = 6,
  height = 5)
