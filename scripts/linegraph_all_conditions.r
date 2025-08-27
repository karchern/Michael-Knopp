source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

#load in data
data <- read_csv2(
  here("data", "Nic_Main_Screen.csv")
  )
print(data)

#manipulate data
filtered_data <- data %>%
  #filter(Condition == "MB003") %>%
  #filter(!(GFP == "wt" & RFP == "wt")) %>%
  #filter((GFP == "ompK35" | RFP == "ompK35")) %>%
  mutate(
    Condition= fct_inorder(Condition),
    GFP = recode(GFP,
    "ompK35" = "ompK*",
    "OXA48" = "ompK*/Oxa48",
    "KPC2" = "ompK*/KPC2",
    "RFP" = "wt"),
    RFP = recode(RFP,
    "ompK35" = "ompK*",
    "OXA48" = "ompK*/Oxa48",
    "KPC2" = "ompK*/KPC2",
    "RFP" = "wt"),
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
    ),
    Category_fac = Category,
    Category_leg = Category
  ) %>%
  select(
    SampleID, 
    Category_fac,
    Category_leg,
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
    ),
    Category_leg= stringr::str_replace(Category_leg, " vs ", "\nvs\n")
  ) 
long_data

#plot
plot_object <- ggplot(
  long_data,
  aes(
    x = generation,
    y = ratio,
    group = SampleID,
    color = Category_leg)
) +
geom_line(
) +
geom_point(
) +
scale_y_log10(
  breaks = 10^seq(-2, 2),
  limits = c(0.04, 10)
) +
scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))
) + 
geom_hline(yintercept = 1
) + #gives line at y=1 to see where ratio is 1 to 1
labs(
  x = "Generation",
  y = "Ratio of GFP/RFP",
  title = "All Conditions: mutant vs wt"
) +
theme_presentation(
) +
theme(
    axis.text.y = element_text(
        size = 7,
        angle = 0
    ),
    axis.text.x = element_text(
        size = 7
    ),
  legend.text  = element_text(size = 6),
  legend.title = element_text(size = 9),
  #legend.position = c(0.99, 0.98),
  legend.justification = c(1, 1),
  #legend.key.height = unit(25, "pt"),
  #legend.key.width  = unit(10, "pt"),
#  legend.spacing.y = unit(10, "pt"),
  legend.background = element_rect(
    color = "black",
    linewidth = 0.4),
strip.text.y = element_blank()
) +
  guides(
    color = guide_legend(
      title = "Category",
      ncol = 1,           # one column -> tall legend
      byrow = TRUE,
      keyheight = grid::unit(45, "pt"),   # per-key height (this is the one that matters)
      keywidth  = grid::unit(5, "pt")
      # , override.aes = list(size = 2)   # optional: larger glyph inside keys
    )
) +
 facet_grid(Category_fac ~ Condition)  # one panel per Condition×Mutant 

print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "linegraph_all_conditions.pdf"
  ),
  width = 10,
  height = 7)
