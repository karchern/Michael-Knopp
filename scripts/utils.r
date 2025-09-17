min_counts_16S <- 1000
min_counts_WGS <- 1000

#pc_16S <- 1/min_counts_16S
pc_16S <- 0.000001
pc_WGS <- 1/min_counts_WGS


library(RColorBrewer)

# Define the names
names <- c("MB001", "MB002", "MB003", "MB005", "MB006", "MB007", "MB008", "MB009", "MB010")

# Generate 10 distinct colors using the 'Set3' palette from RColorBrewer
library(RColorBrewer)
colors <- brewer.pal(n = 10, name = "Set3")

# Create a named vector
donors_named <- setNames(colors, names)
donors_named[2] <- "#85855d" 

antibiotics <- c("Azlocillin", "Carbadox", "Cefixime", "Cefuroxime", "Cephalexin", "Ciprofloxacin", "imipenem", "MitomycinC", "alexidine",
                 "Colistin", "Erythromycin", "Imipenem", "Meropenem", "Metronidazole", "Ofloxacin", 
                 "Rifaximin low", "Sparfloxacin", "Tetracycline", "Chloramphenicol", "Trimethoprim", 
                 "Erythromycin+Ciprofloxacin", "Ampicillin+Ciprofloxacin")
antibiotics_lowercase <- tolower(antibiotics)            

get_heatmap <- function(
    heatmap_values_to_display_raw = NULL,
    genera_to_show_in_heatmap = NULL,
    genera_order_vector = NULL,
    row_hclust = NULL,
    col_on_left_vector_1 = NULL,
    col_on_left_vector_2 = NULL,
	heatmap_title  = NULL,
    outpath = NULL
) {
    heatmap_perturbed <- heatmap_values_to_display_raw %>%
        inner_join(genera_to_show_in_heatmap) %>%
        mutate(genus = factor(genus, levels = genera_order_vector)) %>%
        mutate(Drug = factor(Drug, levels = row_hclust$labels[row_hclust$order])) %>%
            select(Drug, Medium, genus, relative_abundance_log10_perturbed) %>%
            #mutate(tmp = str_c(Drug, Medium, Donor, run, sep = "___")) %>%
            mutate(tmp = str_c(Drug, sep = "___")) %>%
            mutate(tmp = factor(tmp, levels = row_hclust$labels[row_hclust$order])) %>%
            ungroup() %>%
            select(-Drug, -Medium) %>%
            select(-any_of(c("run", "Donor"))) %>%
            complete(tmp, genus) %>%
            pivot_wider(id_cols = tmp, names_from = genus, values_from = relative_abundance_log10_perturbed)


    fp_per <- heatmap_perturbed %>%        
            as.data.frame() %>%
            column_to_rownames("tmp") %>%
            as.matrix() %>%
            t()

    fp_per <- fp_per[match(genera_order_vector, rownames(fp_per)), ]
    fp_per <- fp_per[, match(row_hclust$labels[row_hclust$order], colnames(fp_per))]

    # Combine all data to determine the global min and max
    all_values <- c(fp_per, col_on_left_vector_1, col_on_left_vector_2)
    global_min <- min(all_values, na.rm = TRUE)
    global_max <- max(all_values, na.rm = TRUE)

    # Define a shared color scale function based on the global range
    shared_col_scale <- circlize::colorRamp2(
        breaks = seq(global_min, global_max, length.out = 256),
        colors = viridis(256)
    )

    get_col_scale_function <- function(x) {
        return(
            colorRamp2(
                    breaks = seq(global_min, global_max, length.out = 256), # 256 breaks
                    colors = viridis(256) # 256 colors
                )
        )
    }

    h1 <- ComplexHeatmap::Heatmap(
        fp_per,
        col = colorRamp2(seq(global_min, global_max, length = 256), viridis(256)),
        heatmap_legend_param = list(title = "log10(relative abundance)"),
        cluster_rows = FALSE,
        cluster_columns = FALSE, # confusing, I know...
        row_dend_reorder = FALSE, column_dend_reorder = FALSE,
        left_annotation = rowAnnotation(
            `mean ab. (unperturbed)` = col_on_left_vector_1,
            `mean ab. (perturbed)` = col_on_left_vector_2,
            col = list(
                `mean ab. (unperturbed)` = get_col_scale_function(), 
                `mean ab. (perturbed)` = get_col_scale_function()
                )
        ),
        column_title = heatmap_title,
        column_title_gp = gpar(fontsize = 16, fontface = "bold")
    )

    pdf(file=outpath, width = 14, height = 14)
    draw(h1)
    dev.off()

}


aggregate_profiles <- function(x, lev, pc) {
    return(x %>%
        group_by(Drug, Medium, Donor, run,  Day, .data[[lev]]) %>%
        summarize(count = sum(count)) %>%
        group_by(Drug, Medium, Donor, run,  Day) %>%
        mutate(relative_abundance = count / sum(count)) %>%
        select(-count) %>%
        filter(!is.na(.data[[lev]])) %>%
        mutate(relative_abundance_log10 = log10(relative_abundance + pc)))
}