library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)

source(here("scripts", "utils.r"))

taxon_level <- "genus"
# taxon_level <- "species"

# bork <- readRDS("/g/typas/Personal_Folders/Nic/personal_communities_and_strain_diversity/taxonomic_profiles/WGS/XXX_MB_communities_bork/Results/collated/res_mOTUs.rds")
bork <- read_tsv(here("data", "WGS_profiles_motus3_1.tsv"))
bork <- bork %>%
    # as.data.frame() %>%
    # rownames_to_column("motu_full") %>%
    filter(str_detect(dataset, "Bork")) %>%
    select(-oxygen, -cultivation, -dataset)
# pivot_longer(-motu_full, names_to = "sampleID", values_to = "count")
# From Michael's email
# bork_meta <- tibble(
#   donor = c("MB001", "MB002", "MB003", "MB005", "MB006", "MB007", "MB008", "MB009", "MB010"),
#   sampleIDGenecore = c("MPEV81846163ST", "MPEV93398635ST", "MPEV30404405ST", "MPEV13708611ST", "MPEV24205620ST", "MPEV87476673ST", "MPEV74913627ST", "MPEV43979590ST", "MPEV74592751ST")
# )
# bork <- bork %>%
#     inner_join(bork_meta, by = c("sampleID" = "sampleIDGenecore")) %>%
#     select(
#         motu_full,
#         donor,
#         count
#     )

all_data <- rbind(
    # zm,
    bork
) %>%
    pivot_wider(
        id_cols = donor,
        names_from = motu_full,
        values_from = count,
        values_fill = 0
    ) %>%
    pivot_longer(
        cols = -donor,
        names_to = "motu_full",
        values_to = "count"
    ) %>%
    group_by(motu_full)

# B: motus richness
number_motus_rarefied <- all_data %>%
    pivot_wider(
        id_cols = donor,
        names_from = motu_full,
        values_from = count,
        values_fill = 0
    ) %>%
    column_to_rownames("donor") %>%
    as.data.frame()
all_data_rarefied_wide <- vegan::rrarefy(number_motus_rarefied, min(apply(number_motus_rarefied, 1, sum)))
all_data_rarefied_long <- all_data_rarefied_wide %>%
    as.data.frame() %>%
    rownames_to_column("donor") %>%
    pivot_longer(
        cols = -donor,
        names_to = "motu_full",
        values_to = "count"
    ) %>%
    as_tibble()

richness_rarefied <- data.frame(`mOTU Richness\n(rarefied)` = apply(all_data_rarefied_wide, 1, \(x) sum(x > 0)), check.names = F) %>%
    rownames_to_column("donor")

# C: Shannon diversity (unrarefied)
shannon_diversity_unrarefied <- all_data %>%
    group_by(donor) %>%
    summarise(
        `Shannon\ndiversity` = vegan::diversity(count, index = "shannon")
    )

# C,1 : Shannon diversity (rarefied)
shannon_diversity_rarefied <- all_data_rarefied_long %>%
    group_by(donor) %>%
    summarise(
        `Shannon\ndiversity` = vegan::diversity(count, index = "shannon")
    )

extract_2_first_whitespace_separated_fields <- function(x) {
    fields <- str_split_fixed(x, " ", 3)
    if (ncol(fields) >= 2) {
        return(paste(fields[, 1], fields[, 2], sep = " "))
    } else {
        return(x)
    }
}

# A: genus-level relative abundance heatmap
all_data_genus <- all_data %>%
    group_by(donor) %>%
    mutate(`Relative abundance` = count / sum(count)) %>%
    select(-count) %>%
    ungroup() %>%
    filter(!motu_full == "unassigned") %>%
    mutate(`Relative abundance (log10)` = log10(`Relative abundance` + pc_WGS)) %>%
    mutate(motu_full = str_replace_all(motu_full, "\\[Eubacterium\\]", "Eubacterium")) %>%
    mutate(motu_full = str_replace_all(motu_full, "\\[Clostridium\\]", "Clostridium")) %>%
    mutate(motu_full = str_replace_all(motu_full, "\\[Ruminococcus\\]", "Ruminococcus")) %>%
    mutate(species = str_split_fixed(motu_full, "\\|", 7)[, 7]) %>%
    mutate(species = str_replace(species, "s__", "")) %>%
    mutate(species = str_replace(species, "^ ", "")) %>%
    mutate(species = map_chr(species, extract_2_first_whitespace_separated_fields)) %>%
    mutate(species = str_replace(species, "\\|.*", "")) %>%
    mutate(genus = str_split_fixed(motu_full, "\\|", 7)[, 6]) %>%
    mutate(genus = str_replace(genus, "g__", "")) %>%
    mutate(family = str_split_fixed(motu_full, "\\|", 7)[, 5]) %>%
    mutate(order = str_split_fixed(motu_full, "\\|", 7)[, 4]) %>%
    mutate(class = str_split_fixed(motu_full, "\\|", 7)[, 3]) %>%
    group_by(donor, .data[[taxon_level]]) %>%
    summarize(`Relative abundance` = sum(`Relative abundance`)) %>%
    filter(!str_detect(.data[[taxon_level]], "incert")) %>%
    filter(!str_detect(.data[[taxon_level]], "sp.")) %>%
    filter(!str_detect(.data[[taxon_level]], " species")) %>%
    filter(!str_detect(.data[[taxon_level]], "gen\\.")) %>%
    filter(!str_detect(.data[[taxon_level]], "fam\\.")) %>%
    filter(!str_detect(.data[[taxon_level]], "order"))

donor_hclust_object <- all_data_genus %>%
    pivot_wider(
        names_from = all_of(taxon_level),
        values_from = `Relative abundance`,
        values_fill = 0
    ) %>%
    as.data.frame() %>%
    column_to_rownames("donor") %>%
    dist() %>%
    hclust()
donor_hclust_order <- donor_hclust_object$labels[donor_hclust_object$order]

genus_hclust_object <- all_data_genus %>%
    pivot_wider(
        names_from = donor,
        values_from = `Relative abundance`,
        values_fill = 0
    ) %>%
    as.data.frame() %>%
    column_to_rownames(taxon_level) %>%
    dist() %>%
    hclust()
genus_hclust_order <- genus_hclust_object$labels[genus_hclust_object$order]

# reorder donors
all_data_genus <- all_data_genus %>%
    # mutate(donor = factor(donor, levels = donor_hclust_order)) %>%
    # mutate(genus = factor(genus, levels = genus_hclust_order))
    mutate(donor = factor(donor, levels = donor_hclust_order)) %>%
    mutate("{taxon_level}" := factor(.data[[taxon_level]], levels = genus_hclust_order))
shannon_diversity_unrarefied <- shannon_diversity_unrarefied %>%
    mutate(donor = factor(donor, levels = donor_hclust_order))
richness_rarefied <- richness_rarefied %>%
    mutate(donor = factor(donor, levels = donor_hclust_order))

# Remove lowly abundant genera
all_data_genus <- all_data_genus %>%
    group_by(.data[[taxon_level]]) %>%
    filter(any(`Relative abundance` > 0.01))

p_genus_heatmap <- ggplot() +
    geom_tile(
        data = all_data_genus,
        # aes(x = genus, y = donor, fill = log10(`Relative abundance` + pc_WGS)),
        aes(x = .data[[taxon_level]], y = donor, fill = `Relative abundance`),
        width = 1,
        height = 1
    ) +
    theme_presentation() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    # scale_fill_gradientn(
    # 	#colours = viridis::viridis(100),
    # 	# use OrRd
    # 	colours = colorRampPalette(c("white", "red"))(100),
    # 	na.value = "white"
    # ) +
    scale_fill_gradient2(
        low = "#053067",
        mid = "white",
        high = "#78160b",
        midpoint = 0.1,
        na.value = "white"
    ) +
    NULL

p_shannon <- ggplot() +
    # geom_tile(
    # 	data = shannon_diversity_unrarefied,
    # 	aes(x = 1, y = donor, fill = `Shannon\ndiversity`),
    # ) +
    geom_point(
        data = shannon_diversity_unrarefied,
        aes(x = `Shannon\ndiversity`, y = donor),
    ) +
    geom_segment(
        data = shannon_diversity_unrarefied,
        aes(x = -Inf, xend = `Shannon\ndiversity`, y = donor, yend = donor),
    ) +
    theme_presentation() +
    theme(
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
    ) +
    xlim(c(2, 5)) +
    NULL

p_richness <- ggplot() +
    geom_point(
        data = richness_rarefied,
        aes(x = `mOTU Richness\n(rarefied)`, y = donor),
    ) +
    geom_segment(
        data = richness_rarefied,
        aes(x = -Inf, xend = `mOTU Richness\n(rarefied)`, y = donor, yend = donor),
    ) +
    theme_presentation() +
    theme(
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        # axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
    ) +
    # change color scheme to something black and white
    scale_fill_gradient(low = "black", high = "white") +
    NULL

# transform genus cladogram to ggtree object
phylo_object <- ape::as.phylo(donor_hclust_object)
ggtree_object <- ggtree(phylo_object) +
    # geom_tiplab(aes(label = label), offset = 0.5) +
    # geom_cladelabel(aes(label = label), offset = 0.5) +
    theme_tree2() +
    theme(legend.position = "none") +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()
    )

p <- ggtree_object + (p_genus_heatmap + theme(axis.title.y = element_blank())) + p_shannon + p_richness + plot_layout(nrow = 1, widths = c(0.3, 1, 0.2, 0.2), guides = "collect")
ggsave(
    plot = p,
    # filename = "/g/typas/Personal_Folders/Nic/michael_visualize_in_vitro_community_comp/plots/Figure.pdf",
    filename = here("results", "plots", "Figure_1A_Genus_level_heatmap.pdf"),
    width = 11,
    height = 5
)
