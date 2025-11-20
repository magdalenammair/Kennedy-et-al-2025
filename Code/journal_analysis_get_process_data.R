###### Journal Analysis

#Journal Analysis
## Matching with journal metadata from API
### CRAN ###
# install.packages("devtools")
#install.packages("RCurl")
#install.packages("citecorp")
#install.packages("gcite")
#devtools::install_github("ropensci/rAltmetric")
#devtools::install_github("kamclean/impactr")
library(impactr)
library(readxl)

# this imports aoc_final
source("Doyle-et-al-2025/Code/prep_data.R")

#subset for testing
subset <- aoc_final %>%
  distinct(doi) %>%
  sample_n(., 3)

#function to extract data from DOIs of studies. Warning: this takes a VERY LONG TIME to run!
extract_doi(
  subset$doi,
  get_authors = TRUE,
  get_altmetric = TRUE,
  get_impact = TRUE
)

### Eco tox data
# *SKIP THIS STEP!* Data is already present, and this chunk takes a long time to run

#get unique DOIs
DOIs <- aoc_final %>%
  distinct(doi) %>%
  dplyr::select(doi)

#get data (still takes forever)
jrnlDF2 <- extract_doi(
  DOIs$doi[-c(220, 235, 248, 258, 285, 286, 287)], #some DOIs are in weird journals that can't be fetched
  get_authors = TRUE,
  get_altmetric = TRUE,
  get_impact = TRUE
)

#saveRDS(jrnlDF2, file = "Doyle-et-al-2025/Data/jrnlDF.RDS")

##### Manually annotate missing IFs
#Unclear why some journal IFs are missing. Will manually annotate here.

jrnlDF <- readRDS("Doyle-et-al-2025/Data/jrnlDF.RDS") #note that this may not load due to an RStudio glitch

jrnlDF <- jrnlDF %>%
  mutate(unique_id = paste(journal_abbr, year))

missing <- jrnlDF %>%
  filter(is.na(journal_if)) %>%
  distinct(journal_full, journal_abbr, year, unique_id)

missing

##check if data already exists for journal-years
available <- jrnlDF %>%
  filter(!is.na(journal_if)) %>%
  distinct(journal_full, journal_abbr, year, journal_if, unique_id)

missing_available <- missing %>%
  left_join(
    available %>% dplyr::select(unique_id, journal_if),
    by = "unique_id"
  )

missing_available

## have to manually annotate all

#So it's only possible to get official IFs for journals from WebOfScience. I don't have a Clarivariate account, so I'm using scijournal.org - which has questionable quality...

missing <- missing %>%
  mutate(
    journal_if = case_when(
      unique_id == "Sci. Adv. 2020" ~ 14.39,
      unique_id == "Environ Sci Pollut Res 2020" ~ 3.942,
      unique_id == "Front. Environ. Sci. 2019" ~ 3.622,
      unique_id == "Marine Pollution Bulletin 2020" ~ 5.794,
      unique_id == "Heliyon 2020" ~ 2.806,
      unique_id == "Water Research 2020" ~ 11.347,
      unique_id == "Nanotoxicology 2020" ~ 6.212,
      unique_id == "Environmental Pollution 2020" ~ 8.356
    )
  )

missing_full <- jrnlDF %>%
  filter(is.na(journal_if)) %>%
  dplyr::select(-journal_if) %>%
  left_join(missing %>% dplyr::select(unique_id, journal_if))

notMissing <- jrnlDF %>% filter(!is.na(journal_if))

# recombine
jrnlDF <- bind_rows(missing_full, notMissing)

#saveRDS(jrnlDF, "Doyle-et-al-2025/Data/jrnlDF.RDS")

##### Manually annotate missing Journal Abbreviations

jrnlDF <- readRDS("Doyle-et-al-2025/Data/jrnlDF.RDS") #note that this may not load due to an RStudio glitch

missing <- jrnlDF %>%
  filter(is.na(journal_abbr)) %>%
  distinct(journal_full, journal_abbr)

missing

jrnlDF <- jrnlDF %>%
  mutate(
    journal_abbr = case_when(
      journal_full == "PeerJ" ~ "PeerJ",
      journal_abbr ==
        "Comparative Biochemistry and Physiology Part C: Toxicology &amp; Pharmacology" ~ "CBPC",
      journal_full == "Journal of Molluscan Studies" ~ "J. Molluscan Stud.",
      TRUE ~ journal_abbr
    )
  )

jrnlDF %>% filter(is.na(journal_abbr))

#saveRDS(jrnlDF, "Doyle-et-al-2025/Data/jrnlDF.RDS")

#Remaining missing journal names/abbreviations and impact factors were input by Yuichi Iwasaki.

jrnlDF_update <- read_xlsx(
  "Doyle-et-al-2025/Data/aoc_finaL_jrnl_rev.xlsx",
  sheet = "aoc_final_jrnl_added"
)
#ensure nothing missing
jrnlDF_update <- jrnlDF_update %>%
  distinct(doi, title, journal_full, journal_abbr, journal_if, altmetric) %>%
  mutate(altmetric = as.numeric(altmetric))

#skim(jrnlDF_update)

### Human Tox data

#### Fetch data
#*SKIP THIS STEP!* Data is already present, and this chunk takes a long time to run

require(impactr)

#get unique DOIs
DOIs_human <- aoc_human %>%
  #get distinct dataset of variables needed for modelling (i.e. individual experiments)
  dplyr::select(c(
    doi,
    source,
    starts_with("tech."),
    starts_with("risk."),
    contains("quality"),
    contains("particle"),
    contains("design"),
    total.quality,
    authors,
    year,
    species_h_f,
    life_h_f,
    vivo_h_f,
    sex,
    exp_type_f,
    mix
  )) %>%
  distinct()

#### The impactr package chokes if any single article can't be fetched. I'm abandoning that to use for-loops instead (see below)
# #get data (still takes forever)
# jrnlDF_human <- extract_doi(DOIs_human$doi[-c(3, 4, 9, 10, 35, 36)], #some articles can't be fetched.
#             get_authors = TRUE, get_altmetric = FALSE, get_impact = TRUE)

#saveRDS(jrnlDF_human, file = "Doyle-et-al-2025/Data/jrnlDF_human.RDS")

#### Method using For-Loops

# Initialize an empty dataframe to store the results
results_df <- data.frame()

# Loop through each study
for (i in 1:nrow(DOIs_human)) {
  # Extract the DOI for the current study
  current_doi <- DOIs_human$doi[i]

  # Try to fetch the metadata for the current DOI
  tryCatch(
    {
      # Fetch the data (replace 'fetch_metadata' with the actual function you use)
      metadata <- extract_doi(
        current_doi,
        get_authors = TRUE,
        get_altmetric = TRUE,
        get_impact = TRUE
      )

      # Add the DOI to the metadata
      metadata$doi <- current_doi

      # Append the metadata to the results dataframe
      results_df <- rbind(results_df, metadata)
    },
    error = function(e) {
      # Error handling: you can choose to print an error message or do something else
      cat("Error fetching data for DOI:", current_doi, "\n")
    }
  )
}

# clean up and remove missing entries
results_df2 <- distinct(results_df, .keep_all = TRUE)

# Optional: Merge the results with the original dataframe
# This step depends on how you want to structure the final output
jrnlDF_human <- left_join(DOIs_human, results_df2, by = "doi")

jrnlDF_human_final <- jrnlDF_human %>%
  drop_na(risk.1) %>%
  distinct(total.quality, doi, .keep_all = TRUE)

#saveRDS(jrnlDF_human_final, file = "Doyle-et-al-2025/Data/jrnlDF_human.RDS")

#### Read in previously fetched data

jrnlDF_human_final <- readRDS("Doyle-et-al-2025/Data/jrnlDF_human.RDS")


aoc_human <- readRDS("Doyle-et-al-2025/Data/human_setup_tomex2.RDS") %>% #human tox data created in human shiny repo
  mutate(doi = str_trim(doi, side = "both")) %>% #fix dois
  mutate(human_eco = 'human') %>% #variable to distinguish datasets when joining with eco
  rowwise() %>% #group rowwise so we can calculate aggregated quality score
  mutate(
    total.quality = sum(
      c_across(c(
        starts_with("design."),
        starts_with("risk."),
        "particle.1",
        "particle.2",
        "particle.3",
        "particle.4",
        "particle.5",
        "particle.6",
        "particle.7"
      )),
      na.rm = T
    )
  ) %>%
  mutate(
    technical.quality = sum(
      c_across(c(
        starts_with("design."),
        "particle.1",
        "particle.2",
        "particle.3",
        "particle.4",
        "particle.5",
        "particle.6",
        "particle.7"
      )),
      na.rm = T
    )
  ) %>%
  mutate(risk.quality = sum(c_across(c(starts_with("risk."))), na.rm = T))


## Data filtering

## Read in data (saved previously)
### Eco Tox data

#eco tox data
jrnlDF <- readRDS("Doyle-et-al-2025/Data/jrnlDF.RDS") #note that this may not load due to an RStudio glitch
# join journal data to full dataset
aoc_final_jrnl <- left_join(aoc_final, jrnlDF_update, by = "doi") %>%
  #get distinct dataset of variables needed for modelling (i.e. individual experiments)
  dplyr::select(c(
    doi,
    source,
    #`Issue Flag`,
    starts_with("tech."),
    starts_with("risk."),
    contains("quality"),
    total.quality,
    authors,
    year,
    Species,
    Group,
    env_f,
    life_f,
    vivo_f,
    sex,
    exp_type_f,
    mix,
    exposure.duration.d,
    title,
    starts_with("journal"),
    altmetric,
    journal_if
  )) %>%
  distinct() # %>%
# drop_na(tech.a1) #this should remove all the zeros... but we need to fix these studies!

##### Distinct dataset by experiments
#Some doi contain multiple experiments. Can distinguish by quality criteria

experiment_vars <- c(
  "doi",
  "tech.a1",
  "tech.a2",
  "tech.a3",
  "tech.a4",
  "tech.a5",
  "tech.a6",
  "tech.1",
  "tech.2",
  "tech.3",
  "tech.4",
  "tech.5",
  "tech.6",
  "tech.7",
  "tech.8",
  "tech.9",
  "tech.10",
  "tech.11",
  "tech.12",
  "risk.b1",
  "risk.13",
  "risk.14",
  "risk.15",
  "risk.16",
  "risk.17",
  "risk.18",
  "risk.19",
  "risk.20",
  "risk.quality",
  "total.quality",
  "technical.quality"
)

# Step 1. just get at experiment-level data and annotate with unique ID
aoc_final_jrnl <- aoc_final_jrnl %>%
  drop_na(tech.a1, risk.b1, total.quality, risk.quality, technical.quality) %>%
  distinct(across(all_of(experiment_vars)), .keep_all = TRUE) %>%
  mutate(experimentID = sapply(seq_len(n()), function(x) uuid::UUIDgenerate()))

#human tox data
jrnlDF_human <- readRDS("Doyle-et-al-2025/Data/jrnlDF_human.RDS")
# join journal data to full dataset
aoc_human_jrnl <- left_join(aoc_human, jrnlDF_human, by = "doi")

### Export dataset for SI
# Export
# csv
# of
# quality
# data

simple_aoc_final_jrnl <- aoc_final_jrnl %>%
  dplyr::select(doi, altmetric, year, contains("journal")) %>%
  distinct()

# import AOC file generated in ToMEx 2.0 RShiny repo
aoc_quality <- readRDS("Doyle-et-al-2025/Data/aoc_quality_tomex2.RDS")

aoc_quality_export_long <- aoc_quality %>%
  ungroup() %>%
  dplyr::select(Study, doi, contains(("_f")), Score) %>%
  left_join(simple_aoc_final_jrnl, by = "doi") %>%
  rename(
    "DOI" = doi,
    Species = species_f,
    "Organism Group" = org_f,
    "Environment" = env_f,
    "Life Stage" = life_f,
    "Polymer" = poly_f,
    "Shape" = shape_f,
    "Size Category" = size_f,
    "Effect" = effect_f,
    "Broad Endpoint Category" = lvl1_f,
    "Specific Endpoint Category" = lvl2_f,
    "Level of Biological Organization" = bio_f,
    "Acute/Chronic" = acute.chronic_f,
    "Red Criteria (Technical)" = tier_zero_tech_f,
    "Red Criteria (Risk)" = tier_zero_risk_f,
    "Score (categorical)" = Score_f,
    "Score (numerical)" = Score,
    "Quality category" = Category_f,
    "Quality criteria" = Criteria_f,
    "Altmetric score" = altmetric,
    "Publication year" = year,
    "Journal Name" = journal_full,
    "Journal Abbreviation" = journal_abbr,
    "Journal Impact Factor at time of Publication" = journal_if
  )

write.csv(
  aoc_quality_export_long,
  "Doyle-et-al-2025/Data/tomex2_quality_data.csv"
)

# save aoc_final_jrnl as RDS
saveRDS(aoc_final_jrnl, file = "Doyle-et-al-2025/Data/aoc_final_jrnl.RDS")
# save aoc_quality as RDS
saveRDS(aoc_quality, file = "Doyle-et-al-2025/Data/aoc_quality.RDS")
