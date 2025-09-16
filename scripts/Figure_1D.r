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

# Check
ompK_mGAM_fitness <- filtered_data %>%
  filter(Mutant == "ompK*") %>%
  filter(Condition == "mGAM")

ompK_rest_fitness <- filtered_data %>%
  filter(Mutant == "ompK*") %>%
  filter(Condition != "mGAM") %>%
  group_by(
    Condition
  ) %>%
  nest()

# unpaired mann whitney u test against mGAM
ompK_fitness_against_mGAM <- ompK_rest_fitness %>%
  mutate(
    p_value = map_dbl(
      data,
      ~ wilcox.test(
        x = .$selection_coefficient,
        y = ompK_mGAM_fitness$selection_coefficient,
        alternative = "two.sided",
        paired = FALSE,
        exact = FALSE
      )$p.value
    )
  ) %>%
  select(-data)
ompK_fitness_against_mGAM$p_value_adjusted <- p.adjust(ompK_fitness_against_mGAM$p_value, method = "BH")

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
