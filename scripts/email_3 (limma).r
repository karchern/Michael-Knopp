library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)
library(readxl)
library(ggrepel)

source(here("scripts", "utils.r"))

limma <- read_csv("/Users/karcher/Michael-Knopp/data/limma_results.csv")


vc_plots <- list()
comparisons <- c(
    "GFP_porin_Evo1-GFP_porin",
    "GFP_porin_Evo2-GFP_porin",
    "GFP_porin_Evo3-GFP_porin",
    "GFP_porin_Evo4-GFP_porin",
    "RFP_porin_Evo1-RFP_porin",
    "RFP_porin_Evo2-RFP_porin",
    "RFP_porin_Evo3-RFP_porin",
    "RFP_porin_Evo4-RFP_porin",
    "GFP_porin-GFP_WT",
    "RFP_porin-RFP_WT"
)

for (comp in comparisons) {
    tmp <- limma %>%
        filter(comparison == comp)
    p <- ggplot() +
        geom_point(data = tmp %>% filter(!hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "grey", alpha = 0.5) +
        geom_point(data = tmp %>% filter(hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "red", alpha = 0.8) +
        geom_text_repel(
            data = tmp %>% filter(hit),
            aes(x = logFC, y = -log10(adj.P.Val), label = gene_name),
            size = 3,
            max.overlaps = Inf
        ) +
        theme_presentation() +
        ggtitle(comp)
    vc_plots[[comp]] <- p
}

walk(
    names(vc_plots),
    function(x) {
        ggsave(
            plot = vc_plots[[x]],
            filename = here("results/plots", paste0("volcano_plot_", x, ".pdf")),
            width = 4,
            height = 3
        )
    }
)
