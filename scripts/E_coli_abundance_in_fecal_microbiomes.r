library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)

source(here("scripts", "utils.r"))

bork <- read_tsv(here("data", "WGS_profiles_motus3_1.tsv"))
bork <- bork %>%
    # as.data.frame() %>%
    # rownames_to_column("motu_full") %>%
    filter(!str_detect(dataset, "Bork")) %>%
    inner_join(
        data.frame(
            donor = c(
                "MB001",
                "MB002",
                "MB003",
                "MB004",
                "MB005",
                "MB006",
                "MB007",
                "MB008",
                "MB009"
            )
        )
    ) %>%
    # select(-oxygen, -cultivation, -dataset)
    identity()

bork <- bork %>%
    filter(str_detect(motu_full, "Escherichia")) %>%
    filter(str_detect(motu_full, "coli")) %>%
    filter(str_detect(motu_full, "ref_mOTU_v31_00095")) # this is E. coli :)

donor_sorted_by_e_coli_abundances <- bork %>%
    group_by(donor) %>%
    summarize(e_coli_abundance = sum(relative_abundance)) %>%
    arrange(desc(e_coli_abundance)) %>%
    pull(donor)

bork$donor <- factor(
    bork$donor,
    levels = donor_sorted_by_e_coli_abundances
)

p <- ggplot() +
    geom_bar(data = bork %>% mutate(`Relative abundance [%]` = relative_abundance * 100), aes(x = donor, y = `Relative abundance [%]`), stat = "identity") +
    theme_presentation() +
    facet_wrap(. ~ oxygen) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8)
    )

ggsave(
    here("results", "plots", "E_coli_abundance_in_fecal_microbiomes.pdf"),
    p,
    width = 8,
    height = 5,
    units = "cm"
)
