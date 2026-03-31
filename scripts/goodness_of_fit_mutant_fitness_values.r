library(here)
library(tidyverse)
library(readxl)
library(ggembl)

extract_last_underscore_separated_part <- function(x) {
    str_split(x, "_") %>%
        map_chr(~ tail(.x, 1))
}

data <- read_excel(here("data", "data_main_screen.xlsx")) %>%
    rowwise() %>%
    mutate(
        group = str_c(sort(c(GFP, RFP)), collapse = "_")
    ) %>%
    anti_join(
        data.frame(
            group = c("wt_wt")
        )
    ) %>%
    select(-GFP, -RFP) %>%
    relocate(
        group
    ) %>%
    rename(
        condition = Condition
    ) %>%
    select(
        group, condition, contains("ratio_mut_over")
    ) %>%
    group_by(group, condition) %>%
    mutate(
        replicate = row_number(),
        sample = str_c(group, condition, replicate, sep = "_")
    ) %>%
    pivot_longer(
        cols = contains("ratio_mut_over"),
        names_to = "mutant",
        values_to = "fitness_ratio"
    ) %>%
    mutate(
        timepoint = extract_last_underscore_separated_part(mutant),
        timepoint = str_replace(timepoint, "g", ""),
        timepoint = as.numeric(timepoint)
    ) %>%
    select(-mutant) %>%
    relocate(group, condition, timepoint, sample)

p <- data %>%
    ggplot(
        aes(x = timepoint, y = fitness_ratio, group = sample)
    ) +
    facet_grid(condition ~ group) +
    theme_embl() +
    ylim(
        c(-6, 6)
    ) +
    geom_abline(
        slope = 0,
        intercept = 1,
        linetype = "dashed",
        color = "grey50"
    ) +
    geom_line(alpha = 0.2)

ggsave(
    plot = p,
    filename = here("results/plots", "mutant_fitness_values_over_time.pdf"),
    width = 8,
    height = 14
)

data_bler <- data %>%
    group_by(group, condition, sample) %>%
    nest() %>%
    mutate(
        fit = map(
            data, \(x) {
                lm(fitness_ratio ~ timepoint, data = x)
            }
        ),
        r2 = map_dbl(fit, \(x) summary(x)$r.squared),
        rmse = map_dbl(fit, \(x) sqrt(mean(resid(x)^2))),
        intercept = map_dbl(fit, \(x) coef(x)[1]),
        slope = map_dbl(fit, \(x) coef(x)[2])
    ) %>%
    # group_by(
    #     condition, group
    # ) %>%
    # summarise(
    #     r2_mean = mean(r2),
    #     r2_sd = sd(r2)
    # ) %>%
    identity()


p <- ggplot(
    data = data_bler,
    aes(x = condition, y = rmse, color = group)
) +
    # geom_point(position = position_jitterdodge(height = 0, width = 0.05)) +
    geom_point(
        position = position_jitterdodge(),
    ) +
    theme_embl() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave(
    plot = p,
    filename = here("results/plots", "goodness_of_fit_mutant_fitness_values.pdf"),
    width = 6,
    height = 3
