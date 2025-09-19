library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)

source(here("scripts", "utils.r"))

taxon_level <- "genus"

bork <- read_tsv(here("data", "WGS_profiles_motus3_1.tsv"))
bork <- bork %>%
    # as.data.frame() %>%
    # rownames_to_column("motu_full") %>%
    filter(str_detect(dataset, "Bork")) %>%
    select(-oxygen, -cultivation, -dataset)
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
    mutate(motu_full = str_replace_all(motu_full, "\\[Bacteroides\\]", "Bacteroides")) %>%
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
    filter(!str_detect(.data[[taxon_level]], "order")) %>%
    filter(!str_detect(.data[[taxon_level]], "uncultured")) %>%
    filter(!str_detect(.data[[taxon_level]], "Family"))

all_data_genus_escherichia <- all_data_genus %>%
    filter(genus == "Escherichia")

donor_ordered_by_abundance <- all_data_genus_escherichia %>%
    arrange(desc(`Relative abundance`)) %>%
    pull(donor)

all_data_genus_escherichia <- all_data_genus_escherichia %>%
    mutate(donor = factor(donor, levels = donor_ordered_by_abundance))


p <- ggplot(data = all_data_genus_escherichia, aes(x = donor, y = `Relative abundance`)) +
    geom_point() +
    theme_presentation() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    ylab("Escherichia\nrelative abundance")

ggsave(
    filename = here("results", "plots", "supplementary_figure_XX_E_coli_abundances_in_MBasterisk.pdf"),
    plot = p,
    width = 5,
    height = 3
)
