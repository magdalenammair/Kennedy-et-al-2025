# Load packages
library(dplyr)
library(ggplot2)
library(DHARMa) 
library(car) 
library(tidyr)
library(ggpubr)


# Import data
aoc_z <- readRDS("Data/aoc_z_tomex2.RDS") 

# Change Taxon order and adjust level names
aoc_z$Group_sorted = recode_factor(aoc_z$Group, 
                                   Bacterium = "Bacteria", 
                                   Cyanobacteria = "Cyanobacteria",
                                   Dinoflagellate = "Dinoflagellata", 
                                   Algae = "Algae",
                                   Plant = "Plants",
                                   Rotifera = "Rotifera", 
                                   Ciliophora = "Ciliophora",
                                   Cnidaria = "Cnidaria",
                                   Echinoderm = "Echinodermata", 
                                   Mollusca = "Mollusca",
                                   Nematoda = "Nematoda",
                                   Annelida = "Annelidae",
                                   Crustacea = "Crustacea",
                                   Insect = "Insects",
                                   Fish = "Fish",
                                   Mixed = "Community")
aoc_z$Group_sorted = droplevels(aoc_z$Group_sorted)

# Quality differences among taxa -----

# Data subset 
# only include distinct combinations of doi and quality score patterns
prep_group <- aoc_z %>%
  distinct(risk.quality, technical.quality, total.quality, Group_sorted, doi)

# center quality criteria to mean
prep_group$risk.quality.centered = prep_group$risk.quality - mean(prep_group$risk.quality, na.rm = TRUE)
prep_group$technical.quality.centered = prep_group$technical.quality - mean(prep_group$technical.quality, na.rm = TRUE)
prep_group$total.quality.centered = prep_group$total.quality - mean(prep_group$total.quality, na.rm = TRUE)

# Add samples sizes to group names
sample_sizes = table(prep_group$Group_sorted)
levels(prep_group$Group_sorted) = paste0(levels(prep_group$Group_sorted), " (", sample_sizes, ")")


## 1. risk quality -----
model1 <- lm(risk.quality.centered ~ Group_sorted - 1, data = prep_group)

res = simulateResiduals(model1)
plot(res) 

summary(model1)

# prepare vectors for plotting
coeff = summary(model1)$coefficients
risk.color = ifelse(coeff[,4] >= 0.05, 1, ifelse(coeff[,1] > 0, 2, 3))

## 2. technical quality -----
model2 <- lm(technical.quality.centered ~ Group_sorted - 1, data = prep_group)

res = simulateResiduals(model2)
plot(res) 

summary(model2)

# prepare vectors for plotting
coeff = summary(model2)$coefficients
tech.color = ifelse(coeff[,4] >= 0.05, 1, ifelse(coeff[,1] > 0, 2, 3))


## 3. total quality ------
model3 <- lm(total.quality.centered ~ Group_sorted - 1, data = prep_group)
res = simulateResiduals(model3)
plot(res) 

summary(model3)

# prepare vectors for plotting
coeff = summary(model3)$coefficients
total.color = ifelse(coeff[,4] >= 0.05, 1, ifelse(coeff[,1] > 0, 2, 3))

# define color palette
palette(c("grey80", "#1E90FF", "#FF6347"))

# Figure 2 - illustrate which groups ranked better/worse than average

png("Plots/Figure2.png", width = 21, height = 29.7, units = "cm", res = 1000)
op = par(mfrow = c(3,1), cex = 1, mar = c(3,9,1,1), las = 1, bty = "l")
boxplot(technical.quality.centered ~ Group_sorted, data = prep_group, col = tech.color, notch = F, 
        horizontal = TRUE,
        ylab = "", xlab = "")
mtext("Technical reporting criteria - centered score", side = 1, line = 2)
arrows(0, 0 ,0,15.5, lty = "dashed", col = "grey50", length = 0, lwd = 2)
mtext("A", side = 3, line = -1, at = -6.5, las = 1, font = 2)
boxplot(risk.quality.centered ~ Group_sorted, data = prep_group, col = risk.color, notch = F, 
        horizontal = TRUE,
        ylab = "", xlab = "")
mtext("Applicability to risk assessment - centered score", side = 1, line = 2)
arrows(0, 0 ,0,15.5, lty = "dashed", col = "grey50", length = 0, lwd = 2)
mtext("B", side = 3, line = -1, at = -6, las = 1, font = 2)
boxplot(total.quality.centered ~ Group_sorted, data = prep_group, col = total.color, notch = F, 
        horizontal = TRUE,
        xlab = "", ylab = "")
mtext("Total quality score (centered)", side = 1, line = 2)
arrows(0, 0 ,0,15.5, lty = "dashed", col = "grey50", length = 0, lwd = 2)
mtext("C", side = 3, line = -1, at = -10.4, las = 1, font = 2)
par(op)
dev.off()



# Figure 3 - Detailed differences among taxa -----

risk_taxa <- aoc_z %>%
  dplyr::select(starts_with("risk") & !ends_with("quality") & !starts_with("risk.b"), doi, Group_sorted) %>%
  distinct() %>%
  pivot_longer(cols = c(-"doi", -"Group_sorted"), names_to = "score_type", values_to = "score") %>%
  filter(!is.na(score)) %>%
  group_by(score_type, Group_sorted, score) %>%
  summarise(count = n()) %>%
  mutate(probability = count/sum(count)) %>%
  ungroup() %>% 
  mutate(score_type = as.factor(score_type), score = as.factor(score)) %>%  
  mutate(score_type_name = recode_factor(score_type, risk.13 = "Endpoints", risk.14 = "Presence of\nnatural\n(food) particles", 
                                           risk.15 = "Reporting of\neffect\nthresholds", risk.16 = "Quality of\ndose-response\nrelationship",
                                           risk.17 = "Concentration\nrelevance", risk.18 = "Aging and\nbiofouling",
                                           risk.19 = "Diversity of\nMP tested", risk.20 = "Exposure\ntime"))
  

tech_taxa <- aoc_z %>%
  dplyr::select(starts_with("tech") & !ends_with("quality") & !starts_with("tech.a"), doi, Group_sorted) %>%
  distinct() %>%
  pivot_longer(cols = c(-"doi", -"Group_sorted"), names_to = "score_type", values_to = "score") %>%
  filter(!is.na(score)) %>%
  group_by(score_type, Group_sorted, score) %>%
  summarise(count = n()) %>%
  mutate(probability = count/sum(count)) %>%
  ungroup() %>% 
  mutate(score_type = as.factor(score_type), score = as.factor(score)) %>% 
  mutate(score_type_name = recode_factor(score_type, tech.1 = "Particle\nsize", tech.2 = "Particle\nshape", tech.3 = "Polymer\ntype", 
                                                        tech.4 = "Source\nof MP", tech.5 = "Concentration\nrange\ntested", 
                                                        tech.6 = "Chemical\npurity", tech.7 = "Laboratory\npreparation",
                                                        tech.8 = "Background\ncontamination", tech.9 = "Verification\nof exposure", 
                                                        tech.10 = "Homogeneity\nof exposure", tech.11 = "Exposure\nassessment", tech.12 = "Replication"))


riskplot = ggplot(risk_taxa, aes(x = factor(Group_sorted, levels = unique(Group_sorted)), y = probability, fill = score, label = count)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(score_type_name ~., switch = "y") + 
  scale_fill_manual(values = c("#FF6347", "#CDCDC1", "#1E90FF")) + 
  geom_text(position = position_stack(vjust = 0.5), size = 2) + 
  theme_classic(base_size = 12) + 
  xlab("") +
  ylab("Proportion") +
  theme(strip.text.y.left = element_text(angle = 0), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(0,-0.05), legend.justification = "right", legend.direction = "horizontal",
        legend.key.size = unit(0.3, 'cm'),
        axis.text.y = element_blank(), axis.ticks = element_blank())+
  guides(fill = guide_legend(title = "Score", title.position = "top", title.hjust = 0.5))

techplot = ggplot(tech_taxa, aes(x = factor(Group_sorted, levels = unique(Group_sorted)), y = probability, fill = score, label = count)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(score_type_name ~., switch = "y") + 
  scale_fill_manual(values = c("#FF6347", "#CDCDC1", "#1E90FF")) +
  geom_text(position = position_stack(vjust = 0.5), size = 2) + 
  theme_classic(base_size = 12) + 
  xlab("") +
  ylab("Proportion") +
  theme(strip.text.y.left = element_text(angle = 0), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,),
        legend.position = "none",
        axis.text.y = element_blank(), axis.ticks = element_blank()) 

# save plot
png("Plots/Figure3.png",  width = 30, height = 23, units = "cm", res = 1000)
ggarrange(techplot,riskplot, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()

###END###