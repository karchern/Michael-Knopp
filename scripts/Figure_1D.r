# source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')
library(here)
source(here("scripts", "utils.r"))

# load in data
data <- read_csv2(
  here("data", "Nic_Main_Screen.csv")
)

# manipulate data
filtered_data <- data %>%
  mutate(
    GFP = recode(GFP, "RFP" = "wt"),
    RFP = recode(RFP, "RFP" = "wt"),
  ) %>%
  filter(!(GFP == "wt" & RFP == "wt")) %>%
  mutate(
    Mutant = if_else(
      GFP == "wt",
      RFP,
      GFP
    ),
    Condition = fct_inorder(Condition),
    Mutant = fct_inorder(Mutant),
    Mutant = recode(Mutant,
      "ompK35" = "ompK*",
      "OXA48" = "ompK*/Oxa48",
      "KPC2" = "ompK*/KPC2"
    )
  ) %>%
  select(
    Mutant,
    Condition,
    selection_coefficient
  )

# plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = Condition,
    y = selection_coefficient,
    fill = Mutant
  )
) +
  scale_x_discrete(expand = expansion(add = 0.2)) +
  geom_hline(
    yintercept = 0,
    alpha = 0.3
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.9),
    outlier.shape = NA # suppress outliers from boxplot
  ) +
  geom_jitter(
    position = position_jitterdodge(dodge.width = 1, jitter.width = 0.4),
    alpha = 1,
    size = 0.1
  ) + # plot all points (including "outliers")
  labs(
    x = NULL,
    y = "Selection Coefficient",
    title = "Fitness Assay of Mutants"
  ) +
  theme_presentation() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = c(0.99, 0.98),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.height = unit(10, "pt"),
    legend.key.width = unit(10, "pt"),
    legend.background = element_rect(
      color = "black",
      linewidth = 0.4
    )
  )

ggsave(
  plot = plot_object,
  filename = here(
    "results",
    "plots",
    "Figure_1D_Fitness_assay_of_mutants.pdf"
  ),
  width = 5.8,
  height = 5
)

# Check
for (mutant in unique(filtered_data$Mutant)) {
  mGAM_fitness <- filtered_data %>%
    # filter(Mutant == "ompK*") %>%
    filter(Mutant == mutant) %>%
    filter(Condition == "mGAM") %>%
    mutate(Mutant = str_replace(Mutant, "\\/", "__"))

  rest_fitness <- filtered_data %>%
    # filter(Mutant == "ompK*") %>%
    filter(Mutant == mutant) %>%
    filter(Condition != "mGAM") %>%
    mutate(Mutant = str_replace(Mutant, "\\/", "__")) %>%
    group_by(
      Condition
    ) %>%
    nest()

  mutant <- str_replace(mutant, "\\/", "__")

  # unpaired mann whitney u test against mGAM
  fitness_against_mGAM <- rest_fitness %>%
    mutate(
      p_value = map_dbl(
        data,
        ~ wilcox.test(
          x = .$selection_coefficient,
          y = mGAM_fitness$selection_coefficient,
          alternative = "two.sided",
          paired = FALSE,
          exact = FALSE
        )$p.value
      )
    ) %>%
    select(-data)


  fitness_against_mGAM$p_value_adjusted <- p.adjust(fitness_against_mGAM$p_value, method = "BH")

  print(mutant)
  print("--------------------------")
  print(fitness_against_mGAM)
  print("#############################")
  print("#############################")
  print("#############################")
}

# [1] "ompK*"
# [1] "--------------------------"
# # A tibble: 10 × 3
# # Groups:   Condition [10]
#    Condition   p_value p_value_adjusted
#    <fct>         <dbl>            <dbl>
#  1 SPP21     0.000419          0.00140
#  2 MB001     0.0000873         0.000749
#  3 MB002     0.000150          0.000749
#  4 MB003     0.00110           0.00220
#  5 MB005     0.00270           0.00385
#  6 MB006     0.373             0.373
#  7 MB007     0.00270           0.00385
#  8 MB008     0.121             0.135
#  9 MB009     0.000684          0.00171
# 10 MB010     0.00621           0.00776
# [1] "#############################"
# [1] "#############################"
# [1] "#############################"
# [1] "ompK*__Oxa48"
# [1] "--------------------------"
# # A tibble: 10 × 3
# # Groups:   Condition [10]
#    Condition   p_value p_value_adjusted
#    <fct>         <dbl>            <dbl>
#  1 SPP21     0.00110           0.00220
#  2 MB001     0.0000873         0.000437
#  3 MB002     0.0000873         0.000437
#  4 MB003     0.00110           0.00220
#  5 MB005     0.0321            0.0459
#  6 MB006     0.0698            0.0872
#  7 MB007     0.249             0.249
#  8 MB008     0.223             0.247
#  9 MB009     0.00217           0.00361
# 10 MB010     0.000684          0.00220
# [1] "#############################"
# [1] "#############################"
# [1] "#############################"
# [1] "ompK*__KPC2"
# [1] "--------------------------"
# # A tibble: 10 × 3
# # Groups:   Condition [10]
#    Condition   p_value p_value_adjusted
#    <fct>         <dbl>            <dbl>
#  1 SPP21     0.0192            0.0275
#  2 MB001     0.0000873         0.000175
#  3 MB002     0.0000873         0.000175
#  4 MB003     0.0698            0.0698
#  5 MB005     0.0000873         0.000175
#  6 MB006     0.0000873         0.000175
#  7 MB007     0.00334           0.00557
#  8 MB008     0.0272            0.0340
#  9 MB009     0.0321            0.0357
# 10 MB010     0.0000873         0.000175
# [1] "#############################"
# [1] "#############################"
# [1] "#############################"
