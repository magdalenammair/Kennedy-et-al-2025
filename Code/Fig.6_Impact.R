##### Code to generate Figure 6 ####

#load libraries
library(tidyverse)
library(ggpubr)
library(lmtest)
library(smplot2)
library(wesanderson)

#import data prepared in prep_data.R
#source("Doyle-et-al-2025/Code/prep_data.R") # can take a very long time to run - better to simply load data
aoc_final_jrnl <- readRDS("Doyle-et-al-2025/Data/aoc_final_jrnl.RDS")
aoc_quality <- readRDS("Doyle-et-al-2025/Data/aoc_quality.RDS")


### Altmetric vs. Impact Factor
#### LM
# Fit a linear model
alt_IF <- lm(log(altmetric) ~ journal_if, aoc_final_jrnl)
# Get the R-squared value
r_squared <- summary(alt_IF)$r.squared
summary(alt_IF)
# Plotting diagnostics
par(mfrow = c(2, 2)) # Set up the graphics layout
# 1. Residuals vs Fitted
plot(alt_IF, which = 1)
# 2. Normal Q-Q Plot
plot(alt_IF, which = 2)
# 3. Scale-Location Plot
plot(alt_IF, which = 3)
# 4. Residuals vs Leverage
plot(alt_IF, which = 5)
alt_IF <- aoc_final_jrnl %>%
  # distinct(doi, .keep_all = TRUE) %>%
  ggplot(aes(x = journal_if, y = altmetric)) +
  geom_jitter(aes(color = year), alpha = 0.8, size = 3) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    formula = y ~ x,
    linetype = "dashed",
    color = "black"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ x,
    method = "lm",
    aes(
      label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")
    ),
    label.x = 0.9,
    label.y = 0.5,
    parse = TRUE
  ) +
  xlab("Journal Impact Factor") +
  ylab("Altmetrics Score") +
  scale_y_log10() +
  scale_color_gradientn(
    colors = wes_palette("Zissou1", type = "continuous", n = 13),
    name = "Study Year"
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(0.1, 0.85),
    # legend.title.align = 0.5, #center the title of the legend
    #legend.title = element_text(hjust = 0.5),# Align the title text
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(
      linetype = "solid",
      color = "black",
      size = 0.5
    )
  ) # Add a thin line box around the legend

alt_IF

##### Save Plot
# ggsave(alt_IF,
#        filename = "altMetric_IF_scatter.jpeg",
#        path = "../../output/figures/",
#        width = 10, height = 8, units = "in",
#        bg = "white",
#        dpi = 300)

### Altmetric
#### LM

# Fit a linear model
alt <- lm(log10(total.quality) ~ log10(altmetric), aoc_final_jrnl)

# Get the R-squared value
r_squared <- summary(alt)$r.squared

summary(alt)

# Plotting diagnostics
par(mfrow = c(2, 2)) # Set up the graphics layout

# 1. Residuals vs Fitted
plot(alt, which = 1)

# 2. Normal Q-Q Plot
plot(alt, which = 2)

# 3. Scale-Location Plot
plot(alt, which = 3)

# 4. Residuals vs Leverage
plot(alt, which = 5)

# Shapiro-Wilk test
shapiro.test(residuals(alt))

library(lmtest)
# Breusch-Pagan test
bptest(alt)

spearman_alt <- cor.test(
  aoc_final_jrnl$altmetric,
  aoc_final_jrnl$total.quality,
  method = "spearman"
)
spearman_cor_alt <- signif(spearman_alt$estimate, 2)
spearman_p_value_alt <- signif(spearman_alt$p.value, 2)
print(paste(
  "Journal Altmetric vs. Total Quality_Spearman's rank correlation coefficient: ",
  spearman_cor_alt
))
print(paste(
  "Journal Altmetric vs. Total Quality_Spearman's rank p-value: ",
  spearman_p_value_alt
))

altGG <- aoc_final_jrnl %>%
  ggplot(aes(x = log10(altmetric), y = total.quality)) +
  geom_jitter(aes(color = year), alpha = 0.8, size = 3) +
  smplot2::sm_statCorr(
    show_text = FALSE,
    fit.params = list(color = 'black', linetype = 'dashed')
  ) +
  annotate(
    'text',
    x = 1.2,
    y = 27,
    label = sprintf(
      "Spearman~italic(rho)==%s",
      formatC(spearman_cor_alt[[1]], format = "f", digits = 2)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  annotate(
    'text',
    x = 1.2,
    y = 24,
    label = sprintf(
      "italic(p)==%s",
      formatC(spearman_p_value_alt[[1]], format = "f", digits = 5)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  ylab("Total Quality Score") +
  xlab("log10(Altmetrics Score)") +
  #  scale_y_log10() +
  scale_color_gradientn(
    colors = wes_palette("Zissou1", type = "continuous", n = 13),
    name = "Study Year"
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    # legend.title.align = 0.5, #center the title of the legend
    #legend.title = element_text(hjust = 0.5),# Align the title text
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(
      linetype = "solid",
      color = "black",
      size = 0.5
    )
  ) # Add a thin line box around the legend

altGG

##### Save Plot

# ggsave(altGG,
#        filename = "altMetric_score_scatter.jpeg",
#        path = "../../output/figures/",
#        width = 10, height = 8, units = "in",
#        bg = "white",
#        dpi = 300)
#### Plotly

require(plotly)

# Create a text label for tooltips
aoc_final_jrnl$tooltip_text <- paste(
  "Quality Score:",
  aoc_final_jrnl$total.quality,
  "<br>Altmetric Score:",
  aoc_final_jrnl$altmetric,
  "<br>Impact Factor:",
  aoc_final_jrnl$journal_if,
  "<br>Journal:",
  aoc_final_jrnl$journal_full,
  "<br>Authors:",
  aoc_final_jrnl$authors,
  "<br>Year:",
  aoc_final_jrnl$year,
  "<br>Title:",
  aoc_final_jrnl$title,
  "<br>DOI:",
  aoc_final_jrnl$doi
)

# generate ggplot
altGG <- aoc_final_jrnl %>%
  distinct(doi, .keep_all = TRUE) %>%
  ggplot(aes(y = total.quality, x = altmetric)) +
  geom_jitter(aes(text = tooltip_text, color = year), alpha = 0.8, size = 3) +
  sm_statCorr(
    show_text = FALSE,
    fit.params = list(color = 'black', linetype = 'dashed')
  ) +
  annotate(
    'text',
    x = 4,
    y = 37,
    # paste0("Spearman𝜌=", spearman_cor_alt, "\n p = ", spearman_p_value_alt),
    label = sprintf(
      "Spearman~italic(rho)==%s",
      formatC(spearman_cor_alt, format = "f", digits = 2)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  annotate(
    'text',
    x = 4,
    y = 35,
    label = sprintf(
      "italic(p)==%s",
      formatC(spearman_p_value_alt, format = "f", digits = 5)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  ylab("Total Quality Score") +
  xlab("Altmetric Score") +
  scale_x_log10() +
  scale_color_gradientn(
    colors = wes_palette("Zissou1", type = "continuous", n = 13),
    name = "Study Year"
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(0.2, 0.85),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(
      linetype = "solid",
      color = "black",
      size = 0.5
    )
  ) # Add a thin line box around the legend

gg_alt_year_plotly <- ggplotly(altGG, tooltip = "text")

gg_alt_year_plotly


##### Save as widget (OPTIONAL)
require(htmlwidgets)

# Save the plot as an HTML file
#saveWidget(gg_alt_year_plotly , file = "../../output/widgets/gg_alt_year_plotly .html")

### Impact Factor
#### LM
# Fit a linear model
IF <- lm(log10(total.quality) ~ log10(journal_if), aoc_final_jrnl)
# Get the R-squared value
r_squared <- summary(IF)$r.squared
summary(IF)

# Plotting diagnostics
par(mfrow = c(2, 2)) # Set up the graphics layout
# 1. Residuals vs Fitted
plot(IF, which = 1)
# 2. Normal Q-Q Plot
plot(IF, which = 2)
# 3. Scale-Location Plot
plot(IF, which = 3)
# 4. Residuals vs Leverage
plot(IF, which = 5)

# Shapiro-Wilk test
shapiro.test(residuals(IF))
# Breusch-Pagan test
bptest(IF)

# Robust regression using the MASS package
library(MASS)
IF_robust <- rlm(log10(total.quality) ~ log10(journal_if), aoc_final_jrnl)

# Check normality again
shapiro.test(residuals(IF_robust))
qqnorm(residuals(IF_robust))
qqline(residuals(IF_robust), col = "red")

spearman_IF <- cor.test(
  aoc_final_jrnl$journal_if,
  aoc_final_jrnl$total.quality,
  method = "spearman"
)
spearman_cor_IF <- signif(spearman_IF$estimate, 2)
spearman_p_value_IF <- signif(spearman_IF$p.value, 2)

print(paste(
  "Journal IF vs. Total Quality_Spearman's rank correlation coefficient: ",
  spearman_cor_IF
))

print(paste(
  "Journal IF vs. Total Quality_Spearman's rank p-value: ",
  spearman_p_value_IF
))

#### Ggplot

gg_if_year_base <- aoc_final_jrnl %>%
  ggplot(aes(x = journal_if, y = total.quality)) +
  geom_jitter(aes(color = year), alpha = 0.8, size = 3) +
  ylab("Total Quality Score") +
  xlab("Journal Impact Factor") +
  scale_color_gradientn(
    colors = wes_palette("Zissou1", type = "continuous", n = 13),
    name = "Study Year"
  ) +
  guides(fill = guide_colorbar(title.position = "left", title.hjust = 0.5))


if_gg <- gg_if_year_base +
  sm_statCorr(
    show_text = FALSE,
    fit.params = list(color = 'black', linetype = 'dashed')
  ) +
  annotate(
    'text',
    x = 6,
    y = 37,
    label = sprintf(
      "Spearman~italic(rho)==%s",
      formatC(spearman_cor_IF, format = "f", digits = 2)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  annotate(
    'text',
    x = 6,
    y = 35,
    label = sprintf(
      "italic(p)==%s",
      formatC(spearman_p_value_IF, format = "f", digits = 5)
    ),
    size = 5,
    color = "black",
    parse = TRUE
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = c(0.18, 0.80),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(
      linetype = "solid",
      color = "black",
      size = 0.5
    )
  ) # Add a thin line box around the legend

if_gg

#ARRANGED plot

# Adjust legend text size and spacing in the individual plots
if_gg <- if_gg +
  theme(
    legend.text = element_text(size = 8), # Increase legend text size
    legend.title = element_text(size = 14), # Increase legend title size
    legend.spacing.x = unit(1.5, 'cm') # Add spacing between legend items
  )

altGG <- altGG +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 14),
    legend.spacing.x = unit(1.5, 'cm')
  )


gg_if_alt_year <- ggarrange(
  if_gg,
  altGG,
  ncol = 2,
  labels = c("A", "B"),
  common.legend = T,
  legend = "bottom"
)

ggsave(
  gg_if_alt_year,
  filename = "Figure6.png",
  path = "Doyle-et-al-2025/Plots/",
  width = 8,
  height = 5,
  units = "in",
  bg = "white",
  dpi = 600
)

gg_if_alt_year
