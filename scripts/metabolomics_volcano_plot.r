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

metabolites_to_highlight <- c(
    "_284.16_10.447_Glyceryl 5-hydroxydecanoate",
    "_330.313_1.349_Hexadecyl Methyl Glycerol",
    "_215.056_11.26_sn-glycero-3-phosphoethanolamine",
    # "_238.107_9.293_x-1,2-Propanediol 1-O-b-D-glucopyranoside"
    "1-O-b-D-glucopyranoside"
)

metabolomics <- read_csv("/Users/karcher/Michael-Knopp/data/metabolomics_data.csv") %>%
    rename(metabolite_name = stats_Metabolites) %>%
    # mutate(to_highlight = metabolite_name %in% metabolites_to_highlight) %>%
    mutate(`Glycerol-containing\ncompound` = map_lgl(
        metabolite_name,
        function(x) {
            any(str_detect(x, metabolites_to_highlight))
        }
    )) %>%
    mutate(
        p_fdr <- ifelse(
            p_fdr < 1E-20, 1E-20, p_fdr
        )
    )

plot <- ggplot() +
    geom_point(
        data = metabolomics %>% filter(significant != "Significant"),
        aes(x = log2FC, y = -log10(p_fdr)), color = "grey", alpha = 0.2, stroke = 0
    ) +
    geom_point(
        data = metabolomics %>% filter(significant == "Significant" & log2FC > 0),
        aes(x = log2FC, y = -log10(p_fdr)), color = "#abb7ff", alpha = 0.5, stroke = 0
    ) +
    geom_point(
        data = metabolomics %>% filter(significant == "Significant" & log2FC < -0),
        aes(x = log2FC, y = -log10(p_fdr)), color = "#f5b3b3", alpha = 0.5, stroke = 0
    ) +
    geom_point(
        data = metabolomics %>% filter(`Glycerol-containing\ncompound`),
        aes(x = log2FC, y = -log10(p_fdr)), color = "#b70b0b"
    ) +
    geom_text_repel(
        data = metabolomics %>% filter(`Glycerol-containing\ncompound`),
        aes(x = log2FC, y = -log10(p_fdr), label = metabolite_name),
        size = 3,
        max.overlaps = Inf,
        min.segment.length = 0
    ) +
    theme_presentation()


ggsave(
    plot = plot,
    filename = here("results/plots", "metabolomics_volcano_plot.pdf"),
    width = 12,
    height = 12,
    units = "cm"
)
