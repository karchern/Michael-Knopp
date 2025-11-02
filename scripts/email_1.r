library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)
library(readxl)

source(here("scripts", "utils.r"))

eco_panel <- read_excel("/Users/karcher/Michael-Knopp/data/eco_panel.xlsx") %>%
    mutate(
        `Kpn ompK*` = ompK_evo_abundance_d1,
        `Kpn WT` = ompK_wt_abundance_d1,
        `E. coli` = third_competitor_abundance_d1
    )

# give me 3 colors: grey, pale red, pale green for the three strains
strain_colors <- c(
    "Kpn ompK*" = "#568556", # grey
    "Kpn WT"    = "#b37676", # pale red
    "E. coli"   = "grey" # pale green
)


strain_order_from_compensated <- eco_panel %>%
    group_by(third_competitor) %>%
    summarize(compensated = mean(compensated)) %>%
    arrange(desc(compensated)) %>%
    filter(!third_competitor == "mGAM") %>%
    filter(!third_competitor == "Eco MB003") %>%
    pull(third_competitor)

eco_panel$third_competitor <- factor(
    eco_panel$third_competitor,
    levels = c("mGAM", "Eco MB003", strain_order_from_compensated)
)

p1 <- ggplot(
    eco_panel %>%
        select(third_competitor, `Kpn ompK*`, `Kpn WT`, `E. coli`) %>%
        pivot_longer(
            cols = -third_competitor,
            names_to = "strain",
            values_to = "abundance"
        ) %>%
        group_by(third_competitor, strain) %>%
        summarize(abundance = mean(abundance)),
    aes(x = third_competitor, y = abundance, fill = strain)
) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    theme(axis.text.x = element_blank()) +
    xlab("E. coli strains")

# ggsave(
#     here("results/plots", "supplementary_figure_XX_eco_panel_abundances_day1.pdf"),
#     p1,
#     width = 6,
#     height = 4
# )


p2 <- ggplot(
    eco_panel %>%
        select(third_competitor, compensated) %>%
        group_by(third_competitor) %>%
        summarize(sdd = sd(compensated), compensated = mean(compensated)),
    aes(x = third_competitor, y = compensated)
) +
    geom_pointrange(aes(ymin = compensated - sdd, ymax = compensated + sdd), size = 0.2) +
    scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    theme(axis.text.x = element_blank()) +
    xlab("E. coli strains")

# ggsave(
#     here("results/plots", "supplementary_figure_XX_eco_panel_compensated_day1.pdf"),
#     p2,
#     width = 6,
#     height = 4
# )


# Use patchwork to assembly plots, ensuring that coordinates are conserved so Michael can do this in Illustrator
p_combined <- p1 + p2 + plot_layout(ncol = 1)
ggsave(
    here("results/plots", "supplementary_figure_XX_eco_panel_abundances_and_compensated_day1.pdf"),
    p_combined,
    width = 9,
    height = 6
)
