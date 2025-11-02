library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)
library(readxl)

source(here("scripts", "utils.r"))

knockouts <- read_excel("/Users/karcher/Michael-Knopp/data/knockouts.xlsx")
p1 <- ggplot(data = knockouts) +
    geom_boxplot(aes(
        x = community,
        y = selection_coefficient,
        fill = strain
    )) +
    theme_presentation()

ggsave(
    plot = p1,
    filename = here("results/plots", "supplementary_figure_XX_knockouts_selection_coefficients.pdf"),
    width = 8, height = 4
)
