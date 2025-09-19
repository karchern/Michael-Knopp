library(tidyverse)
library(patchwork)
library(vegan)
library(ggembl)
library(ComplexHeatmap)
library(ggtree)
library(here)

taxon_level <- "genus"
taxon_prefix_to_kick <- "g__"

clean_final_condition <- function(s) {
    return(case_when(
        s == "hih ph" ~ "high ph",
        TRUE ~ s
    ))
}


drug_map <- read_tsv(here("data", "condition_drug_map_between_fitness_and_profiles.tsv")) %>%
    rename(
        Medium_old = Medium_new,
        Drug_old = Drug_new,
        Medium_new = Medium,
        Drug_new = Drug
    ) %>%
    rename(
        Medium = Medium_old,
        Drug = Drug_old
    )

profiles <- read_csv("/Users/karcher/Michael-Knopp/data/run1_run3_16S_genus_profiles.csv")
profiles <- profiles %>%
    group_by(
        Drug, Medium, Donor, run, Day
    ) %>%
    mutate(
        "{taxon_level}" := str_remove(.data[[taxon_level]], taxon_prefix_to_kick),
    ) %>%
    filter(Donor == "MB003") %>%
    mutate(
        condition_group = case_when(
            Drug == "no drug" ~ "Medium perturbation (no drug)",
            TRUE ~ "non-Medium based perturbation"
        )
    ) %>%
    mutate(
        final_condition = ifelse(
            condition_group == "non-Medium based perturbation",
            Drug,
            Medium
        )
    )

abundant_genera <- profiles %>%
    group_by(.data[[taxon_level]]) %>%
    filter(any(relative_abundance > 0.05)) %>%
    pull(.data[[taxon_level]]) %>%
    unique()

taxon_level <- "genus"
profiles <- profiles %>%
    # inner_join(
    #     tibble("{taxon_level}" := abundant_genera),
    # )
    mutate("{taxon_level}" := ifelse(
        .data[[taxon_level]] %in% abundant_genera,
        .data[[taxon_level]],
        "Other"
    )) %>%
    mutate("{taxon_level}" := factor(.data[[taxon_level]], levels = c(abundant_genera, "Other"))) %>%
    mutate(
        final_condition = clean_final_condition(final_condition)
    )

genus_colors <- setNames(
    c(
        "#E41A1C", # red
        "#377EB8", # blue
        "#4DAF4A", # green
        "#FF7F00", # orange
        "#984EA3", # purple
        "#FFFF33", # yellow
        "#A65628", # brown
        "#F781BF", # pink
        "#999999", # dark grey
        "#66C2A5", # teal
        "#FC8D62", # salmon
        "#8DA0CB", # light blue
        "#E78AC3", # magenta
        "#A6D854", # lime
        "#FFD92F", # gold
        "#E5C494", # beige
        "#B3B3B3", # light grey
        "#1B9E77", # dark teal
        "#D95F02", # dark orange
        "#7570B3", # indigo
        "#E7298A", # hot pink
        "#66A61E", # olive
        "#A6761D", # ochre
        "grey80" # for "Other"
    ),
    c(abundant_genera, "Other")
)

genus_colors["Eggerthella"] <- "#FF1493" # deep pink
genus_colors["Parabacteroides"] <- "#00CED1" # dark turquoise

fitness <- read_tsv(here("data", "subcommunity_fitness_data.tsv")) %>%
    rename(
        Drug = condition,
        Medium = media
    ) %>%
    left_join(
        drug_map,
        by = c("Drug", "Medium")
    ) %>%
    mutate(
        Drug = Drug_new, Medium = Medium_new
    ) %>%
    select(-Drug_new, -Medium_new) %>%
    # mutate(Drug = ifelse(Drug == "none", "no drug", Drug)) %>%
    mutate(
        condition_group = case_when(
            Drug == "no drug" ~ "Medium perturbation (no drug)",
            TRUE ~ "non-Medium based perturbation"
        )
    ) %>%
    mutate(
        final_condition = ifelse(
            condition_group == "non-Medium based perturbation",
            Drug,
            Medium
        )
    )

a <- data.frame(co = unique(profiles$final_condition)) %>% mutate(Kind = "Profile")
b <- data.frame(co = unique(fitness$final_condition)) %>% mutate(Kind = "Fitness")
all_conditions <- full_join(a, b, by = "co") %>% arrange()

# Take the intersection of the 'final_condition' values present in both datasets
common_conditions <- intersect(
    unique(profiles$final_condition),
    unique(fitness$final_condition)
) %>%
    unique()

fitness <- fitness %>%
    group_by(final_condition, condition_group) %>%
    summarize(
        sd_ratio = sd(ratio),
        ratio = mean(ratio)
    ) %>%
    mutate(
        final_condition = clean_final_condition(final_condition)
    )

conditions_ordered_by_fitness <- fitness %>%
    arrange(desc(ratio)) %>%
    pull(final_condition)

profiles <- profiles %>%
    inner_join(
        data.frame(final_condition = common_conditions)
    ) %>%
    mutate(final_condition = factor(final_condition, levels = conditions_ordered_by_fitness))

fitness <- fitness %>%
    inner_join(
        data.frame(final_condition = common_conditions)
    ) %>%
    mutate(final_condition = factor(final_condition, levels = conditions_ordered_by_fitness))

# Create a highlight dataframe for bars where abs(ratio) > 1.5

p <- ggplot() +
    geom_segment(
        data = fitness %>%
            mutate(show_arrow = as.character(ratio <= 2)) %>%
            select(final_condition, condition_group, show_arrow) %>%
            mutate(
                x = final_condition,
                xend = final_condition,
                y = 1.2,
                yend = 1.05
            ),
        # arrow
        aes(x = x, xend = xend, y = y, yend = yend, alpha = show_arrow),
        arrow = arrow(length = unit(0.1, "cm")),
        show.legend = FALSE
    ) +
    geom_bar(
        data = profiles,
        aes(x = final_condition, y = relative_abundance, fill = .data[[taxon_level]]),
        position = "stack",
        stat = "identity"
    ) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    theme_presentation() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_blank()
    ) +
    facet_grid(. ~ condition_group, scales = "free_x", space = "free_x") +
    scale_fill_manual(
        values = genus_colors
    ) +
    ylab("Relative abundance") +
    xlab("Condition") +
    NULL

p2 <- ggplot() +
    geom_hline(yintercept = 2, linetype = "dashed", color = "grey50") +
    geom_point(
        data = fitness %>% mutate(co = as.character(ratio > 2)),
        aes(x = final_condition, y = ratio, alpha = co),
        size = 0.5, show.legend = FALSE
    ) +
    geom_linerange(data = fitness %>% mutate(co = ratio > 2), aes(ymin = ratio - sd_ratio, ymax = ratio + sd_ratio, x = final_condition, alpha = co), size = 0.3, show.legend = FALSE) +
    scale_color_manual(
        values = c("TRUE" = "red", "FALSE" = "black")
    ) +
    scale_alpha_manual(
        values = c("TRUE" = 0.2, "FALSE" = 1)
    ) +
    # geom_bar(
    #     data = fitness %>%
    #         ungroup() %>%
    #         filter(ratio > 2) %>%
    #         select(final_condition, condition_group) %>%
    #         distinct() %>%
    #         mutate(relative_abundance = max(fitness$ratio + fitness$sd_ratio) * 1.01),
    #     aes(
    #         x = final_condition,
    #         y = relative_abundance
    #     ),
    #     fill = "grey",
    #     alpha = 0.25,
    #     show.legend = FALSE,
    #     stat = "identity"
    # ) +
    theme_presentation() +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
    ) +
    # ylim(c(-3, 7)) +
    facet_grid(. ~ condition_group, scales = "free_x", space = "free_x") +
    ylab("Ratio ompK35/36/\n WT") +
    xlab("Condition")

ggsave(
    here("results", "plots", "Figure_2A.pdf"),
    p2 + p + plot_layout(ncol = 1, heights = c(1.75, 4)),
    width = 16,
    height = 5.5
)

# Also correlate all genera with all fitness values
# For each genus, compute correlation with fitness ratio across conditions
genus_fitness_j <- profiles %>%
    filter(genus != "Other") %>%
    group_by(final_condition, genus) %>%
    summarize(rel_abund = mean(relative_abundance), .groups = "drop") %>%
    inner_join(
        fitness %>% select(final_condition, ratio),
        by = "final_condition"
    )

genus_fitness_cor_log10 <- genus_fitness_j %>%
    group_by(genus) %>%
    mutate(
        rel_abund_log10 = log10(rel_abund + 1e-5)
    ) %>%
    select(-rel_abund) %>%
    summarize(
        cor = cor(rel_abund_log10, ratio, use = "complete.obs"),
        p.value = cor.test(rel_abund_log10, ratio)$p.value,
        n = n()
    ) %>%
    arrange(desc(cor))

genus_fitness_cor_identity <- genus_fitness_j %>%
    group_by(genus) %>%
    summarize(
        cor = cor(rel_abund, ratio, use = "complete.obs"),
        p.value = cor.test(rel_abund, ratio)$p.value,
        n = n()
    ) %>%
    arrange(desc(cor))

genus_order <- genus_fitness_cor_log10 %>%
    arrange(desc(cor)) %>%
    pull(genus)

genus_fitness_j <- genus_fitness_j %>%
    mutate(
        genus = factor(genus, levels = genus_order)
    )
genus_fitness_cor_log10 <- genus_fitness_cor_log10 %>%
    mutate(
        genus = factor(genus, levels = genus_order)
    )
genus_fitness_cor_identity <- genus_fitness_cor_identity %>%
    mutate(
        genus = factor(genus, levels = genus_order)
    )

p_sc <- ggplot() +
    geom_point(
        data = genus_fitness_j %>% mutate(`Fitness ratio` = ifelse(as.character(ratio > 2), "high", "low")),
        aes(x = log10(rel_abund + 1e-5), y = ratio, color = `Fitness ratio`),
        alpha = 0.2
    ) +
    geom_text(
        data = genus_fitness_cor_log10,
        aes(
            x = -4.85,
            y = 5,
            label = paste0(genus, "\n", "r=", round(cor, 2), "\n", "p=", signif(p.value, 2))
        ),
        hjust = 0,
        vjust = 1,
        size = 2
    ) +
    theme_presentation() +
    facet_wrap(. ~ genus) +
    scale_color_manual(values = c("high" = "#E41A1C", "low" = "#377EB8")) +
    xlab("log10(Relative abundance + 1e-5)") +
    ylab("Fitness ratio ompK35/36/ WT") +
    NULL

ggsave(
    plot = p_sc,
    filename = "/Users/karcher/Michael-Knopp/results/plots/supplementary_fitness_abundance_correlations.pdf",
)

# For each genus, compare relative abundance between ratio > 2 and ratio <= 2 using Wilcoxon test

genus_fitness_j <- profiles %>%
    filter(genus != "Other") %>%
    group_by(final_condition, genus) %>%
    summarize(rel_abund = mean(relative_abundance), .groups = "drop") %>%
    inner_join(
        fitness %>% select(final_condition, ratio),
        by = "final_condition"
    ) %>%
    mutate(
        `Fitness ratio` = ifelse(ratio > 2, "high", "low")
    ) %>%
    mutate(
        rel_abund_log10 = log10(rel_abund + 1e-5)
    ) %>%
    select(-rel_abund)

# Wilcoxon test for each genus
genus_fitness_wilcox <- genus_fitness_j %>%
    group_by(genus) %>%
    summarize(
        p.value = tryCatch(
            wilcox.test(rel_abund_log10 ~ `Fitness ratio`)$p.value,
            error = function(e) NA_real_
        ),
        n_high = sum(`Fitness ratio` == "high"),
        n_low = sum(!`Fitness ratio` == "low"),
        .groups = "drop"
    ) %>%
    arrange(p.value)

# DOn't compute this, instead use same order as correlation plot
# genus_order <- genus_fitness_wilcox %>%
#     arrange(p.value) %>%
#     pull(genus)

genus_fitness_j <- genus_fitness_j %>%
    mutate(
        genus = factor(genus, levels = genus_order)
    )

# Boxplot of relative abundance by fitness group for each genus
p_box <- ggplot(genus_fitness_j, aes(x = `Fitness ratio`, y = rel_abund_log10, fill = `Fitness ratio`)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(. ~ genus, scales = "free_y") +
    theme_presentation() +
    scale_fill_manual(values = c("high" = "#E41A1C", "low" = "#377EB8")) +
    # xlab("High fitness (ratio > 2)") +
    ylab("Relative abundance") +
    geom_text(
        data = genus_fitness_wilcox,
        aes(
            x = 1.5, y = Inf, label = paste0("p=", signif(p.value, 2))
        ),
        inherit.aes = FALSE,
        vjust = 1.2,
        hjust = 0.5,
        size = 2
    ) +
    NULL

ggsave(
    plot = p_box,
    filename = "/Users/karcher/Michael-Knopp/results/plots/supplementary_fitness_abundance_wilcox_boxplots.pdf",
    width = 12,
    height = 6
)
