library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)
library(readxl)

source(here("scripts", "utils.r"))

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Various strains")

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
    geom_pointrange(aes(ymin = compensated - sdd, ymax = compensated + sdd), size = 0.2) +
    # scale_fill_manual(values = strain_colors) +
    theme_presentation() +
    # theme(axis.text.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("E. coli strains")

# ggsave(
#     here("results/plots", "supplementary_figure_XX_quad_panel_compensated_day1.pdf"),
#     p2,
#     width = 6,
#     height = 4
# )


# Use patchwork to assembly plots, ensuring that coordinates are conserved so Michael can do this in Illustrator
p_combined <- p1 + p2 + plot_layout(ncol = 1)
ggsave(
    here("results/plots", "quad_panel_abundances_and_compensated_day1.pdf"),
    p_combined,
    width = 9,
    height = 6
)
