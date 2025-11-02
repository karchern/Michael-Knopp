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

tpp <- read_csv("/Users/karcher/Michael-Knopp/data/TPP_results_all.csv")

vc_plots <- list()
comparisons <- c(
    "evolved-wt",
    "evolved-porin",
    "porin-wt"
)
for (sc in c(
    "solubility",
    "stability"
)
) {
    for (comp in comparisons) {
        tmp <- tpp %>%
            filter(comparison == comp) %>%
            filter(score == sc) %>%
            rename(
                adj.P.Val = adj.P.Val.limma,
                logFC = logFC.limma,
                hit = hit_annotation_limma,
                gene_name = Entry.Name
            ) %>%
            mutate(hit = !hit == "no_hit")
        p <- ggplot() +
            geom_point(data = tmp %>% filter(!hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "grey", alpha = 0.1) +
            geom_point(data = tmp %>% filter(hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "red", alpha = 0.5) +
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
