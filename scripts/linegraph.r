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
  mutate(SampleID = row_number(),
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
geom_hline(yintercept = 1
) + #gives line at y=1 to see where ratio is 1 to 1
labs(
  x = "Generation",
  y = "Ratio",
  title = "OmpK35 to wt Ratio Over Time"
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
    "OmpK35_to_wt_ratio_over_time.pdf"
  ),
  width = 4.46,
  height = 2.44)
