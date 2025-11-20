#### CODE TO GENERATE FIGURES 7 and 8
#Assess effect of study quality on presence/absence of effect
library(tidyverse)
library(cowplot)

# Import data
aoc_z <- readRDS("Doyle-et-al-2025/Data/aoc_z_tomex2.RDS")

# Summarize presence absence of effect on level of unique quality criteria-study combination
prep_effect <- aoc_z %>%
        distinct(
                risk.quality,
                technical.quality,
                total.quality,
                effect_f,
                doi
        ) %>%
        mutate(
                unique_quality_study = paste0(
                        risk.quality,
                        technical.quality,
                        total.quality,
                        doi
                )
        ) %>%
        group_by(unique_quality_study) %>%
        mutate(
                effect_found_in_study = ifelse("Yes" %in% effect_f, "Yes", "No")
        ) %>%
        distinct(
                risk.quality,
                technical.quality,
                total.quality,
                effect_found_in_study,
                doi
        )

# How does study quality influence the detection of significant effects?
# Logistic regression
prep_effect$binary_effect_found_in_study = as.numeric(as.factor(
        prep_effect$effect_found_in_study
)) -
        1
model <- glm(
        binary_effect_found_in_study ~ risk.quality + technical.quality,
        data = prep_effect,
        family = binomial
)
model2 = glm(
        binary_effect_found_in_study ~ total.quality,
        data = prep_effect,
        family = binomial
)
#check residuals and get results
res = simulateResiduals(model)
plot(res) # ok
summary(model)

res2 = simulateResiduals(model2)
plot(res2) # good
summary(model2)


## Illustrate results

png("Plots/Figure7.png", width = 30, height = 23, units = "cm", res = 1000)


# Plot results
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
#layout.show(3)

# plot effect presence by total score
newdat = data.frame(total.quality = seq(1, 50, by = 1))
preds = predict(model2, newdata = newdat, type = "link", se.fit = TRUE)
critval = qnorm(0.975)
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- model$family$linkinv(fit)
upr2 <- model$family$linkinv(upr)
lwr2 <- model$family$linkinv(lwr)

op = par(bty = "l", xpd = TRUE, las = 1, cex = 1.1)
plot(
        fit2 ~ newdat$total.quality,
        type = "l",
        ylim = c(-0.1, 1.1),
        ylab = "Probability of detecting effect",
        xlab = "Total quality score"
)
polygon(
        x = c(newdat$total.quality, rev(newdat$total.quality)),
        y = c(lwr2, rev(upr2)),
        col = adjustcolor("#1B9E77", alpha.f = 0.2),
        border = "white"
)
lines(lwr2 ~ newdat$total.quality, lty = "dashed", col = "#1B9E77")
lines(upr2 ~ newdat$total.quality, lty = "dashed", col = "#1B9E77")
lines(fit2 ~ newdat$total.quality, lwd = 2, col = "#1B9E77")
points(
        jitter(
                prep_effect$binary_effect_found_in_study,
                factor = 0.2
        ) ~ prep_effect$total.quality,
        pch = 21,
        col = "#1B9E77",
        bg = adjustcolor("#1B9E77", alpha.f = 0.2)
)
text(
        paste0(
                "P-value: ",
                round(summary(model2)$coefficients[2, 4], digits = 2)
        ),
        x = 45,
        y = 0.7,
        col = "#1B9E77"
)

# plot effect presence by technical quality score
newdat = data.frame(
        risk.quality = mean(prep_effect$risk.quality, na.rm = TRUE),
        technical.quality = seq(1, 24, by = 1)
)
preds = predict(model, newdata = newdat, type = "link", se.fit = TRUE)
critval = qnorm(0.975)
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- model$family$linkinv(fit)
upr2 <- model$family$linkinv(upr)
lwr2 <- model$family$linkinv(lwr)

op = par(bty = "l", xpd = TRUE, las = 1)
plot(
        fit2 ~ newdat$technical.quality,
        type = "l",
        ylim = c(-0.1, 1.1),
        ylab = "Probability of detecting effect",
        xlab = "Technical reporting criteria score"
)
polygon(
        x = c(newdat$technical.quality, rev(newdat$technical.quality)),
        y = c(lwr2, rev(upr2)),
        col = adjustcolor("#D95F02", alpha.f = 0.2),
        border = "white"
)
lines(lwr2 ~ newdat$technical.quality, lty = "dashed", col = "#D95F02")
lines(upr2 ~ newdat$technical.quality, lty = "dashed", col = "#D95F02")
lines(fit2 ~ newdat$technical.quality, lwd = 2, col = "#D95F02")
points(
        jitter(
                prep_effect$binary_effect_found_in_study,
                factor = 0.2
        ) ~ prep_effect$technical.quality,
        pch = 21,
        col = "#D95F02",
        bg = adjustcolor("#D95F02", alpha.f = 0.2)
)
text(
        paste0(
                "P-value: ",
                round(summary(model)$coefficients[3, 4], digits = 2)
        ),
        x = 20,
        y = 0.6,
        col = "#D95F02"
)

# plot effect presence by risk quality score
newdat = data.frame(
        technical.quality = mean(prep_effect$technical.quality, na.rm = TRUE),
        risk.quality = seq(1, 24, by = 1)
)
preds = predict(model, newdata = newdat, type = "link", se.fit = TRUE)
critval = qnorm(0.975)
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- model$family$linkinv(fit)
upr2 <- model$family$linkinv(upr)
lwr2 <- model$family$linkinv(lwr)

op = par(bty = "l", xpd = TRUE, las = 1)
plot(
        fit2 ~ newdat$risk.quality,
        type = "l",
        ylim = c(-0.1, 1.1),
        ylab = "Probability of detecting effect",
        xlab = "Applicability to risk assessment score"
)
polygon(
        x = c(newdat$risk.quality, rev(newdat$risk.quality)),
        y = c(lwr2, rev(upr2)),
        col = adjustcolor("#7570B3", alpha.f = 0.2),
        border = "white"
)
lines(lwr2 ~ newdat$risk.quality, lty = "dashed", col = "#7570B3")
lines(upr2 ~ newdat$risk.quality, lty = "dashed", col = "#7570B3")
lines(fit2 ~ newdat$risk.quality, lwd = 2, col = "#7570B3")
points(
        jitter(
                prep_effect$binary_effect_found_in_study,
                factor = 0.2
        ) ~ prep_effect$technical.quality,
        pch = 21,
        col = "#7570B3",
        bg = adjustcolor("#7570B3", alpha.f = 0.2)
)
text(
        paste0(
                "P-value: ",
                round(summary(model)$coefficients[2, 4], digits = 2)
        ),
        x = 20,
        y = 0.4,
        col = "#7570B3"
)
par(op)

dev.off()

##################################################
##### Code to generate FIgure 8
######################################################
# read aoc_effects_meta_jrnl (prepared in prep_data.R)
aoc_effects_meta_jrnl <- readRDS(
        "Doyle-et-al-2025/Data/aoc_effects_meta_jrnl.rds"
)


#### Run statistics
total.quality.year.logistic <- glm(
        effect_status_binary ~ year,
        data = aoc_effects_meta_jrnl,
        family = binomial(link = "logit")
)

# Summary of the model to see results
summ_total.quality.year.logistic <- summary(total.quality.year.logistic)
p_value_total_year <- summ_total.quality.year.logistic$coefficients[
        "year",
        "Pr(>|z|)"
]
coeff_year <- summ_total.quality.year.logistic$coefficients["year", "Estimate"]
coeff_year_se <- summ_total.quality.year.logistic$coefficients[
        "year",
        "Std. Error"
]

summ_total.quality.year.logistic

### Probability of Effect vs. Publication IF
total.quality.if.logistic <- glm(
        effect_status_binary ~ journal_if,
        data = aoc_effects_meta_jrnl,
        family = binomial(link = "logit")
)

# Summary of the model to see results
summ_total.quality.if.logistic <- summary(total.quality.if.logistic)
p_value_total_journal_if <- summ_total.quality.if.logistic$coefficients[
        "journal_if",
        "Pr(>|z|)"
]
coeff_journal_if <- summ_total.quality.if.logistic$coefficients[
        "journal_if",
        "Estimate"
]
coeff_journal_if_se <- summ_total.quality.if.logistic$coefficients[
        "journal_if",
        "Std. Error"
]

summ_total.quality.if.logistic

### Probability of Effect vs. Journal (factor)
total.quality.journal.logistic <- glm(
        effect_status_binary ~ journal_abbr,
        data = aoc_effects_meta_jrnl,
        family = binomial(link = "logit")
)

# Summary of the model to see results
summ_total.quality.journal.logistic <- summary(total.quality.journal.logistic)
summ_total.quality.journal.logistic

####### FIGURE 8 ######
# Creating prediction data with a sequence to cover the range of interest
year_data <- data.frame(
        year = seq(
                min(aoc_effects_meta_jrnl$year, na.rm = TRUE),
                max(aoc_effects_meta_jrnl$year, na.rm = TRUE),
                length.out = 100
        )
)

# Predict probabilities and standard errors
year_data <- cbind(
        year_data,
        predict(
                total.quality.year.logistic,
                newdata = year_data,
                type = "response",
                se.fit = TRUE
        )
)

# Convert predictions from log-odds to probabilities and calculate confidence intervals
year_data$predicted_prob <- plogis(year_data$fit) # Convert log-odds to probabilities
ci_width <- qnorm(0.975) * year_data$se.fit # 1.96 * SE for 95% CI
year_data$lower <- plogis(year_data$fit - ci_width)
year_data$upper <- plogis(year_data$fit + ci_width)

# Plotting the predicted probabilities with 95% confidence intervals
year_effect_ggplot <- ggplot() +
        geom_jitter(
                data = aoc_effects_meta_jrnl,
                aes(x = year, y = effect_status_binary),
                alpha = 0.4,
                width = 0.1,
                height = 0.1,
                color = "orange2"
        ) + # Plot the raw data points
        geom_ribbon(
                data = year_data,
                aes(ymin = lower, ymax = upper, x = year),
                fill = "orange",
                alpha = 0.2
        ) + # Confidence interval
        geom_line(
                data = year_data,
                aes(x = year, y = predicted_prob),
                color = "orange3"
        ) + # Add the regression line
        labs(
                #title = "Predicted Probability vs. Study Quality",
                x = "Publication Year",
                y = "Probability of Detecting Effect"
        ) +
        ylim(c(0, 1)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme_classic(base_size = 16) +
        annotate(
                "text",
                x = 2015,
                y = 0.5, # Adjust y to be the midpoint of your actual y-axis range
                label = paste("P-value:", signif(p_value_total_year, 2)),
                size = 4, # Adjust text size as needed
                color = "orange2" # Text color
        )

#### Publication IF
# Creating prediction data with a sequence to cover the range of interest
journal_if_data <- data.frame(
        journal_if = seq(
                min(aoc_effects_meta_jrnl$journal_if, na.rm = TRUE),
                max(aoc_effects_meta_jrnl$journal_if, na.rm = TRUE),
                length.out = 100
        )
)

# Predict probabilities and standard errors
journal_if_data <- cbind(
        journal_if_data,
        predict(
                total.quality.if.logistic,
                newdata = journal_if_data,
                type = "response",
                se.fit = TRUE
        )
)

# Convert predictions from log-odds to probabilities and calculate confidence intervals
journal_if_data$predicted_prob <- plogis(journal_if_data$fit) # Convert log-odds to probabilities
ci_width <- qnorm(0.975) * journal_if_data$se.fit # 1.96 * SE for 95% CI
journal_if_data$lower <- plogis(journal_if_data$fit - ci_width)
journal_if_data$upper <- plogis(journal_if_data$fit + ci_width)

# Plotting the predicted probabilities with 95% confidence intervals
journal_if_effect_ggplot <- ggplot() +
        geom_jitter(
                data = aoc_effects_meta_jrnl,
                aes(x = journal_if, y = effect_status_binary),
                alpha = 0.4,
                width = 0.1,
                height = 0.1,
                color = "purple3"
        ) + # Plot the raw data points
        geom_ribbon(
                data = journal_if_data,
                aes(ymin = lower, ymax = upper, x = journal_if),
                fill = "purple2",
                alpha = 0.2
        ) + # Confidence interval
        geom_line(
                data = journal_if_data,
                aes(x = journal_if, y = predicted_prob),
                color = "purple3"
        ) + # Add the regression line
        labs(
                #title = "Predicted Probability vs. Study Quality",
                x = "Journal Impact Factor",
                y = "Probability of Detecting Effect"
        ) +
        ylim(c(0, 1)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme_classic(base_size = 16) +
        annotate(
                "text",
                x = 8,
                y = 0.5, # Adjust y to be the midpoint of your actual y-axis range
                label = paste("P-value:", signif(p_value_total_journal_if, 2)),
                size = 4, # Adjust text size as needed
                color = "purple4" # Text color
        )
### arrane and export

prob_journal_plots <- plot_grid(
        year_effect_ggplot,
        journal_if_effect_ggplot,
        ncol = 2,
        labels = c("A", "B"),
        label_size = 18
)


ggsave(
        prob_journal_plots,
        filename = "prob_journal_plots.jpeg",
        path = "Doyle-et-al-2025/Plots",
        width = 8,
        height = 4,
        units = "in",
        bg = "white",
        dpi = 300
)


prob_journal_plots
