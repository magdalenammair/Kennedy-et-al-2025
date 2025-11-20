##### Code to generate figures 4 and 5 ####

#load libraries
library(tidyverse)
library(ggpubr)
library(lmtest)

#import data prepared in prep_data.R
source("Doyle-et-al-2025/Code/prep_data.R")
# this imports aoc_final_with_id

#############################################
############## RUN STATISTICS ###############
#############################################
# assess normality and determine correlation
risk.year <- lm(log10(risk.quality + 1) ~ year, data = aoc_final_with_id)
# Shapiro-Wilk test
shapiro.test(residuals(risk.year))
# Breusch-Pagan test
bptest(risk.year)

spearman_risk.year <- cor.test(
  aoc_final_with_id$risk.quality,
  aoc_final_with_id$year,
  method = "spearman"
)

spearman_cor_risk.year <- signif(spearman_risk.year$estimate, 2)
spearman_p_value_risk.year <- signif(spearman_risk.year$p.value, 2)

print(paste(
  "Journal risk.yearmetric vs. Total Quality_Spearman's rank correlation coefficient: ",
  spearman_cor_risk.year
))

print(paste(
  "Journal risk.yearmetric vs. Total Quality_Spearman's rank p-value: ",
  spearman_p_value_risk.year
))

spearman_total.year <- cor.test(
  aoc_final_with_id$total.quality,
  aoc_final_with_id$year,
  method = "spearman"
)

spearman_cor_total.year <- signif(spearman_total.year$estimate, 2)
spearman_p_value_total.year <- signif(spearman_total.year$p.value, 2)

print(paste(
  "Journal total.yearmetric vs. Total Quality_Spearman's rank correlation coefficient: ",
  spearman_cor_total.year
))

print(paste(
  "Journal total.yearmetric vs. Total Quality_Spearman's rank p-value: ",
  spearman_p_value_total.year
))

spearman_technical.year <- cor.test(
  aoc_final_with_id$technical.quality,
  aoc_final_with_id$year,
  method = "spearman"
)

spearman_cor_technical.year <- signif(spearman_technical.year$estimate, 2)
spearman_p_value_technical.year <- signif(spearman_technical.year$p.value, 2)

print(paste(
  "Journal technical.yearmetric vs. Total Quality_Spearman's rank correlation coefficient: ",
  spearman_cor_technical.year
))
print(paste(
  "Journal technical.yearmetric vs. Total Quality_Spearman's rank p-value: ",
  spearman_p_value_technical.year
))


##########################################
############## Make Figures ##############
##########################################

###### FIGURE 4  #######
# Determine x-axis breaks: every 2 years from the minimum to maximum year in your data.
year_min <- floor(min(long_time$year, na.rm = TRUE) / 2) * 2
year_max <- ceiling(max(long_time$year, na.rm = TRUE) / 2) * 2
x_breaks <- seq(year_min, year_max, by = 2)

### Create the scatterplot ###
scatterplot_base <- ggplot(
  long_time,
  aes(x = year, y = score, color = quality_type, fill = quality_type)
) +
  # Plot the points with slight horizontal jitter
  geom_point(
    position = position_jitter(width = 0.2, height = 0),
    alpha = 0.35
  ) +
  # Manually annotate the Spearman correlations with tightened spacing.
  annotate(
    "text",
    x = year_min + 0.3,
    y = 38,
    label = "Spearman Rank Correlations (coefficient, significance)",
    color = "black",
    hjust = 0,
    size = 5
  ) +
  annotate(
    "text",
    x = year_min + 0.3,
    y = 35,
    label = sprintf(
      "Applicability~to~Risk~Assessment:~italic(rho)==%s~','~italic(p)==%s",
      formatC(spearman_cor_risk.year, format = "f", digits = 2),
      formatC(spearman_p_value_risk.year, format = "f", digits = 2)
    ),
    parse = TRUE,
    color = "#7570B3",
    hjust = 0,
    size = 5
  ) +
  annotate(
    "text",
    x = year_min + 0.3,
    y = 32,
    label = sprintf(
      "Technical~Criteria:~italic(rho)==%s~','~italic(p)==%s",
      formatC(spearman_cor_technical.year, format = "f", digits = 2),
      formatC(spearman_p_value_technical.year, format = "f", digits = 2)
    ),
    parse = TRUE,
    color = "#D95F02",
    hjust = 0,
    size = 5
  ) +
  annotate(
    "text",
    x = year_min + 0.3,
    y = 29,
    label = sprintf(
      "Total~Accumulated~Score:~italic(rho)==%s~','~italic(p)==%s",
      formatC(spearman_cor_total.year, format = "f", digits = 2),
      formatC(spearman_p_value_total.year, format = "f", digits = 2)
    ),
    parse = TRUE,
    color = "#1B9E77",
    hjust = 0,
    size = 5
  ) +

  # Add a purple vertical (dotted) line at x = 2020
  geom_vline(
    xintercept = 2020,
    color = "purple4",
    linetype = "dotdash",
    size = 1.8,
    alpha = 0.8
  ) +
  # Annotate the vertical line. Adjust x and y a bit to move it down and left.
  annotate(
    "text",
    x = 2019.5,
    y = 15.8,
    label = "De Ruijter et al. (2020)",
    color = "purple4",
    angle = 90,
    hjust = -0.1,
    fontface = "italic",
    size = 5
  ) +

  ylab("Score") +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_breaks,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(limits = c(0, 40)) +

  # Manual legend: map the internal quality_type names to pretty names and specify colors.
  scale_color_manual(
    values = c(
      "risk.quality" = "#7570B3",
      "technical.quality" = "#D95F02",
      "total.quality" = "#1B9E77"
    ),
    labels = c(
      "risk.quality" = "Risk Applicability",
      "technical.quality" = "Technical Quality",
      "total.quality" = "Total Accumulated Score"
    )
  ) +
  scale_fill_manual(
    values = c(
      "risk.quality" = "#7570B3",
      "technical.quality" = "#D95F02",
      "total.quality" = "#1B9E77"
    ),
    labels = c(
      "risk.quality" = "Risk Applicability",
      "technical.quality" = "Technical Quality",
      "total.quality" = "Total Accumulated Score"
    )
  ) +
  theme(legend.position = "none")

# Create light themed versions for the plot.
scatterplot_light <- scatterplot_base +
  theme_bw(base_size = 18) +
  theme(
    axis.title.x = element_blank()
  ) +
  theme(legend.position = "none")


### Bottom Plot: Total Number of Studies Published per Year ###
# Ensure the publication year is numeric here as well
aoc_final <- aoc_final %>%
  mutate(year = as.numeric(year))

# Group by year and count the total number of studies (using distinct doi)
year_counts <- aoc_final %>%
  group_by(year) %>%
  summarize(total_studies = n_distinct(doi))

# Ensure x-axis breaks every 2 years.
x_breaks_bottom <- seq(year_min, year_max, by = 2)

plot_bottom_base <- year_counts %>%
  ggplot(aes(x = year, y = total_studies)) +
  geom_col(fill = "grey60") +
  scale_x_continuous(
    breaks = x_breaks_bottom,
    labels = x_breaks_bottom,
    expand = expansion(mult = c(0, 0))
  ) +
  ylab("Studies/year") +
  coord_cartesian(clip = "off") +
  scale_y_continuous() +
  theme(legend.position = "none")

plot_bottom_light <- plot_bottom_base +
  theme_bw(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank()
  )


### Arrange the Composite Figure ###
quality_time <- ggarrange(
  scatterplot_light,
  plot_bottom_light,
  nrow = 2,
  heights = c(3, 1)
)

ggsave(
  plot = quality_time,
  filename = "Figure4.png",
  path = "Doyle-et-al-2025/Plots/",
  width = 8,
  height = 6,
  units = "in",
  bg = "white",
  dpi = 500
)

# show Figure 4
quality_time


###### FIGURE 5  #######
### ANOVA Quality Before/After De Ruijter Criteria

# Create a grouping variable: "<2021" for studies before 2021 and ">=2021" for studies in 2021 and after.
aoc_final_with_id$year_group <- ifelse(
  aoc_final_with_id$year < 2021,
  "<=2020",
  ">2020"
)

# Run ANOVA on log-transformed risk quality
risk_anova <- aov(risk.quality ~ year_group, data = aoc_final_with_id)
summary(risk_anova)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(risk_anova))

# Breusch-Pagan test for homoscedasticity
bptest(risk_anova)


# Because residuals are non-normal, we must use a non-parametric test instead.
# 2. Mann–Whitney U Tests ----------------------------------------------------

# Run the tests for each quality measure:
wilcox_risk <- wilcox.test(risk.quality ~ year_group, data = aoc_final_with_id)
wilcox_total <- wilcox.test(
  total.quality ~ year_group,
  data = aoc_final_with_id
)
wilcox_tech <- wilcox.test(
  technical.quality ~ year_group,
  data = aoc_final_with_id
)

# Save p-value outputs
risk_p_value <- wilcox_risk$p.value
total_p_value <- wilcox_total$p.value
tech_p_value <- wilcox_tech$p.value

# Print the results with explanation:
cat("Mann-Whitney U Test Results:\n")
cat("Risk Quality: p-value =", risk_p_value, "\n")
cat("Total Quality: p-value =", total_p_value, "\n")
cat("Technical Quality: p-value =", tech_p_value, "\n")

# Print full test statistics
wilcox_total
wilcox_risk
wilcox_tech

# 3. Prepare Data for Plotting -----------------------------------------------
# Reshape the data from wide to long format
aoc_long <- aoc_final_with_id %>%
  pivot_longer(
    cols = c(risk.quality, total.quality, technical.quality),
    names_to = "quality_type",
    values_to = "quality_value"
  )

# Create prettier facet labels for the quality measures
quality_labels <- c(
  "risk.quality" = "Applicability to Risk Assessment",
  "total.quality" = "Total Accumulated Score",
  "technical.quality" = "Technical Criteria"
)

# Compute, for each facet overall, the maximum value (for positioning the overall p‑value)
pval_data <- aoc_long %>%
  group_by(quality_type) %>%
  summarize(max_val = max(quality_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    p_label = case_when(
      quality_type == "risk.quality" ~ paste(
        "Mann-Whitney \n p =",
        signif(risk_p_value, 2)
      ),
      quality_type == "total.quality" ~ paste(
        "Mann-Whitney \n p =",
        signif(total_p_value, 2)
      ),
      quality_type == "technical.quality" ~ paste(
        "Mann-Whitney \n p =",
        signif(tech_p_value, 2)
      )
    )
  )

# Compute median and standard error for each measure by year group
stats_data <- aoc_long %>%
  group_by(quality_type, year_group) %>%
  summarize(
    median_val = median(quality_value, na.rm = TRUE),
    se_val = sd(quality_value, na.rm = TRUE) / sqrt(n()),
    group_max = max(quality_value, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Plotting with ggplot2 ---------------------------------------------------

quality_boxplot <- ggplot(
  aoc_long,
  aes(x = year_group, y = quality_value, fill = year_group, color = year_group)
) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  facet_wrap(
    ~quality_type,
    scales = "free_y",
    labeller = labeller(quality_type = quality_labels)
  ) +
  labs(
    #title = "Boxplots of Quality Scores Before/After Publication of de Ruijter et al. (2020)",
    title = "",
    x = "Before / After de Ruijter et al. (2020) Quality Criteria Published",
    y = "Quality Score"
  ) +
  scale_fill_manual(values = c("<=2020" = "#66c2a5", ">2020" = "#fc8d62")) +
  scale_color_manual(values = c("<=2020" = "#66c2a5", ">2020" = "#fc8d62")) +
  theme_bw(base_size = 13.5) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  # Annotate the overall Mann–Whitney U p-value (centered in each facet)
  geom_text(
    data = pval_data,
    mapping = aes(
      x = 1.5, # y = max_val*0.95,
      label = p_label
    ),
    color = "black",
    y = Inf,
    vjust = 1.5,
    inherit.aes = FALSE,
    size = 3.5
  ) +
  # Annotate median and standard error for each year group (one label per box)
  geom_text(
    data = stats_data,
    mapping = aes(
      x = year_group,
      label = paste0(
        "Median: \n",
        round(median_val, 2),
        " ± ",
        round(se_val, 2),
        " (SE)"
      )
    ),
    y = Inf,
    vjust = 3.5,
    size = 3.5,
    vjust = 0,
    color = "black"
  )

ggsave(
  plot = quality_boxplot,
  filename = "Figure5.png",
  path = "Doyle-et-al-2025/Plots/",
  width = 8,
  height = 5,
  units = "in",
  bg = "white",
  dpi = 500
)

# Print the plot
print(quality_boxplot)
