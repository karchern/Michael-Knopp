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

# plan: plot non-adjusted p-value, don't color anything, other than the 3 things that Michael will send you.
# then mention in the legends that these where identified as hits in the adjusted analysis.

tpp <- read_csv("/Users/karcher/Michael-Knopp/data/TPP_results_all.csv") %>%
    group_by(
        score, comparison
    ) %>%
    mutate(p_adj_v2 = p.adjust(P.Value.limma, method = "BH"))

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
            geom_point(data = tmp %>% filter(!hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "grey", alpha = 0.5) +
            geom_point(data = tmp %>% filter(hit), aes(x = logFC, y = -log10(adj.P.Val)), color = "red", alpha = 0.8) +
            # geom_point(data = tmp, aes(x = logFC, y = -log10(P.Value.limma)), color = "grey", alpha = 0.2) +
            geom_text_repel(
                data = tmp %>% filter(hit),
                aes(x = logFC, y = -log10(adj.P.Val), label = gene_name),
                size = 3,
                max.overlaps = Inf
            ) +
            theme_presentation() +
            ggtitle(comp)
        vc_plots[[str_c(comp, sc, sep = "___")]] <- p
    }
}

walk(
    names(vc_plots),
    function(x) {
        xx <- str_split(x, "___")[[1]][1]
        sc <- str_split(x, "___")[[1]][2]
        ggsave(
            plot = vc_plots[[x]],
            filename = here("results/plots", paste0("volcano_plot_", xx, "__", sc, ".pdf")),
            width = 12,
            height = 12,
            units = "cm"
        )
    }
)
