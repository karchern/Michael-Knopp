library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)
library(readxl)

source(here("scripts", "utils.r"))

knockouts <- read_excel("/Users/karcher/Michael-Knopp/data/knockouts.xlsx") %>%
    mutate(strain = factor(strain, levels = unique(strain)))

for (com in unique(knockouts$community)) {
    tmp <- knockouts %>%
        filter(community == com)
    p1 <- ggplot() +
        geom_boxplot(
            data = tmp,
            aes(
                x = strain,
                y = selection_coefficient,
            )
        ) +
        theme_presentation() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylab("Selection Coefficient") +
        xlab("Strain")

    ggsave(
        plot = p1,
        filename = here("results/plots", str_c("supplementary_figure_XX_knockouts_selection_coefficients_", com, ".pdf")),
        width = 4, height = 4
    )
}
