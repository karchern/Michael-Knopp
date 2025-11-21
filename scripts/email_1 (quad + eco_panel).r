library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
# library(ggtree)
library(here)
library(readxl)

source(here("scripts", "utils.r"))

########
# QUAD #
########

quad_panel <- read_excel("/Users/karcher/Michael-Knopp/data/quad_panel.xlsx") %>%
    mutate(
        `Kpn ompK*` = ompK_evo_abundance_d1,
        `Kpn WT` = ompK_wt_abundance_d1,
        `E. coli` = eco_abundance_d1,
        `Fourth competitor` = fourth_competitor_abundance_d1
    ) %>%
    rename(fourth_competitor_name = fourth_competitor) %>%
    filter(!fourth_competitor_name == "NA")

# give me 3 colors: grey, pale red, pale green for the three strains
strain_colors <- c(
    "Kpn ompK*" = "#568556", # grey
    "Kpn WT" = "#b37676", # pale red
    "E. coli" = "#c5c77a", # pale green
    "Fourth competitor" = "grey70" # grey for fourth competitor
)

strain_order_from_compensated <- quad_panel %>%
    group_by(fourth_competitor_name) %>%
    summarize(compensated = mean(compensated)) %>%
    arrange(desc(compensated)) %>%
    # filter(!third_competitor == "mGAM") %>%
    # filter(!third_competitor == "Eco MB003") %>%
    filter(!fourth_competitor_name == "none") %>%
    pull(fourth_competitor_name)

quad_panel$fourth_competitor_name <- factor(
    quad_panel$fourth_competitor_name,
    levels = c("none", strain_order_from_compensated)
)

p1 <- ggplot(
    quad_panel %>%
        select(fourth_competitor_name, `Kpn ompK*`, `Kpn WT`, `E. coli`, `Fourth competitor`) %>%
        pivot_longer(
            cols = -fourth_competitor_name,
            names_to = "strain",
            values_to = "abundance"
        ) %>%
        group_by(fourth_competitor_name, strain) %>%
        summarize(abundance = mean(abundance)),
    aes(x = fourth_competitor_name, y = abundance, fill = strain)
) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    # theme(axis.text.x = element_blank()) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8)
    )

# ggsave(
#     here("results/plots", "supplementary_figure_XX_quad_panel_abundances_day1.pdf"),
#     p1,
#     width = 8,
#     height = 4
# )


p2 <- ggplot(
    quad_panel %>%
        select(fourth_competitor_name, compensated) %>%
        group_by(fourth_competitor_name) %>%
        summarize(sdd = sd(compensated), compensated = mean(compensated)),
    aes(x = fourth_competitor_name, y = compensated)
) +
    geom_pointrange(aes(ymin = compensated - sdd, ymax = compensated + sdd), size = 0.1) +
    # scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    # theme(axis.text.x = element_blank()) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8),
        axis.ticks.x = element_line(size = 0.2)
    )

# ggsave(
#     here("results/plots", "supplementary_figure_XX_quad_panel_compensated_day1.pdf"),
#     p2,
#     width = 6,
#     height = 4
# )

p1_Quad <- p1 + guides(fill = "none")
p2_Quad <- p2 + guides(fill = "none")

# Use patchwork to assembly plots, ensuring that coordinates are conserved so Michael can do this in Illustrator
# p_combined <- p1 + p2 + plot_layout(ncol = 1)
# ggsave(
#     here("results/plots", "quad_panel_abundances_and_compensated_day1.pdf"),
#     p_combined,
#     width = 9,
#     height = 6
# )

#######
# Eco #
#######

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
    theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8)
    )

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
    geom_pointrange(aes(ymin = compensated - sdd, ymax = compensated + sdd), size = 0.0002, linewidth = 0.1) +
    scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8)
    )

# ggsave(
#     here("results/plots", "supplementary_figure_XX_eco_panel_compensated_day1.pdf"),
#     p2,
#     width = 6,
#     height = 4
# )


p1_Eco <- p1 + guides(fill = "none")
p2_Eco <- p2 + guides(fill = "none")

# Use patchwork to assembly plots, ensuring that coordinates are conserved so Michael can do this in Illustrator
p_combined <- (p1_Eco + theme(axis.ticks.x = element_blank())) + (p2_Eco + theme(axis.ticks.x = element_blank())) + (p1_Quad + theme(axis.text.x = element_blank())) + p2_Quad + plot_layout(ncol = 2, nrow = 2, byrow = FALSE)
# ggsave(
#     here("results/plots", "eco_panel_abundances_and_compensated_day1.pdf"),
#     p_combined,
#     width = 14,
#     height = 4
# )
ggsave(
    here("results/plots", "eco_plus_quad_panel_abundances_and_compensated_day1.pdf"),
    p_combined,
    width = 18,
    height = 5,
    unit = "cm"
)
