library(ggplot2)
library(dplyr)
library(viridis)
library(cowplot)
library(calecopal)
library(FactoMineR)
library(ggsci)
library(tidyr)
library(readxl)
library(tidyr)


ToMExFinal <- readRDS("Data/aoc_z_tomex2.RDS")
colnames(ToMExFinal)

TechScoring<-select(ToMExFinal,c(tech.a1,tech.a2,tech.a3,tech.a4,tech.a5,tech.a6,tech.1,tech.2,tech.3,tech.4,tech.5,tech.6,tech.7,tech.8,tech.9,tech.10,tech.11,tech.12))
TechScoring2<-pivot_longer(TechScoring,tech.a1:tech.12)
TechScoring2[TechScoring2 == 3] <- 2
TechScoring2<- TechScoring2 %>% 
  mutate(name= case_match(name, 'tech.a1' ~ 'Dispersant Reported', 'tech.a2' ~ 'Exposure Route Reported',
                          'tech.a3' ~ 'Species Reported','tech.a4' ~ 'Sample Size','tech.a5' ~ 'Control Group',
                          'tech.a6' ~ 'Exposure Duration','tech.1' ~ 'Particle Size','tech.2' ~ 'Particle Shape',
                          'tech.3' ~ 'Polymer Type','tech.4' ~ 'Source of MP','tech.5' ~ 'Data Reporting',
                          'tech.6' ~ 'Chemical Purity','tech.7' ~ 'Lab Preparation','tech.8' ~ 'Background Contamination',
                          'tech.9' ~ 'Verification of Exposure','tech.10' ~ 'Homogeneity of Exposure',
                          'tech.11' ~ 'Exposure Assessment','tech.12' ~ 'Replication'))


RiskScoring<-select(ToMExFinal,c(risk.b1,risk.13,risk.14,risk.15,risk.16,risk.17,risk.18,risk.19,risk.20))

RiskScoring<-pivot_longer(RiskScoring,risk.b1:risk.20)
RiskScoring2<- RiskScoring %>% 
  mutate(name= case_match(name, 'risk.b1' ~ 'Three or More Concentrations', 'risk.13' ~ 'Relevant Biological Endpoints',
                          'risk.14' ~ 'Natural Particles Present','risk.15' ~ 'Qaulity of Effect Thresholds',
                          'risk.16' ~ 'Dose-Reponse Relationship','risk.17' ~ 'Concentration Range Tested',
                          'risk.18' ~ 'Aging and Biofouling','risk.19' ~ 'Diveristy of MPs Testing','risk.20' ~ 'Exposure Time'))
#Scoring2$Rank<-factor(Scoring2$Rank, levels=c('2', '1','0'))

# Figure 1A ---

png("Plots/Figure1A.png", width = 20, height = 18, units = "cm", res = 1000)
na.omit(TechScoring2) %>%
  count(value,name) %>% 
  group_by(name) %>%
  mutate(pct = n/sum(n))%>%
  ggplot(aes(x=name,y=pct,fill=as.factor(value))) +
  geom_col()+
  guides(fill = guide_legend(reverse = T)) +
  ggtitle("Technical Reporting Criteria") +
  geom_text(aes(label = scales::percent(pct,accuracy = 1)),position = position_stack(vjust = 0.5))+
  theme_bw() + 
  theme(legend.position ="bottom",
        legend.title=element_blank(),
        plot.title= element_text(hjust=0.5),
        axis.ticks.length.x  = unit(0.5, "cm"),
        axis.text.x=element_text(angle=45,vjust=0.5),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )+
  ylim(c(0,1))+
  labs(y="Frequency",x=NULL)+
  theme(text=element_text(size=15),axis.title.x=element_blank())+coord_flip()+
  scale_fill_manual(values = c("#FF6347", "#CDCDc1", "#1E90FF"), name="Scoring Criteria")
dev.off()

# Figure 1B ---

png("Plots/Figure1B.png", width = 20, height = 18, units = "cm", res = 1000)
na.omit(RiskScoring2) %>%
  count(value,name) %>% 
  group_by(name) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x=name,y=pct,fill=as.factor(value)))+
  guides(fill = guide_legend(reverse = T))+ 
  geom_col()+
  ggtitle("Applicability to Risk Assessment")+
  geom_text(aes(label = scales::percent(pct,accuracy = 1)),position = position_stack(vjust = 0.5))+
  labs(y="Frequency",x=NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title= element_text(hjust=0.5),
        axis.text.x=element_text(angle=45,vjust=0.5),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )+ 
  theme(text=element_text(size=15),axis.title.x=element_blank())+coord_flip()+
  scale_fill_manual(values = c("#FF6347", "#CDCDc1", "#1E90FF"), name="Scoring Criteria")
dev.off()


### END ###
