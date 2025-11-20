# Code to prep data for Fig. 4 and 5
library(tidyverse)

aoc_z <- readRDS("Doyle-et-al-2025/Data/aoc_z_tomex2.RDS") %>% # AQUATIC TOX DATA ONLY
  ### This script is generated from the RShiny repo with ToMEx2.0_Onboarding/ToMEx2.0_Data_Tidying.R ##
  #fix DOI errors
  mutate(
    doi = case_when(
      doi == "0.1021/es404295e" ~ "10.1021/es404295e",
      doi == " 10.1016/j.ecoenv.2020.111153 " ~ "10.1016/j.ecoenv.2020.111153",
      doi ==
        "10.1371/journal.pone.0032254.g002" ~ "doi.org/10.1371/journal.pone.0032254",
      doi == "doi: 10.3389/fmars.2021.716349" ~ "10.3389/fmars.2021.716349",
      doi == "10.3390/ toxics9110289" ~ "10.3390/toxics9110289",
      doi == "10.3390/\r\ntoxics9110289" ~ "10.3390/toxics9110289",
      doi == " 10.1016/j.jhazmat.2020.124287" ~ "10.1016/j.jhazmat.2020.124287",
      doi == "	10.1016/j.ecoenv.2020.111153 " ~ "10.1016/j.ecoenv.2020.111153",
      doi == "10.1016/j.jhazmat.2020.126059" ~ "10.1016/j.jhazmat.2021.126059",
      doi ==
        "doi.org/10.1371/journal.pone.0032254" ~ "10.1371/journal.pone.0032254",
      TRUE ~ doi
    )
  ) %>% #fix missing number in doi
  mutate(doi = str_trim(doi, side = "both")) %>% # fix dois
  mutate(human_eco = 'eco') %>% #variable to distinguish datasets when joining with eco
  ungroup()

## First filter data with global filters
aoc_intermediate <- aoc_z %>%
  mutate(max.size.ingest.um = 1000 * max.size.ingest.mm) %>% #makes it less confusing below
  droplevels() #eliminate polymer aand shape data that's not needed


###define sizes for alignment##
x1M_set <- 1 #um lower size for all alignments
x1D_set <- 1 #um lower size for all alignments
x2D_set <- 5000 #um
upper.tissue.trans.size.um <- 83 #10 #um #set size for x2M

## parametrization ##
# Define params for correction #
alpha = 2.07 #table s4 for marine surface water. length
# define parameters for power law coefficients
a.sa = 1.5 #marine surface area power law
a.v = 1.48 #a_V for marine surface water volume
a.m = 1.32 # upper limit fora_m for mass for marine surface water in table S4
a.ssa = 1.98 # A_SSA for marine surface water

#define additional parameters for calculations based on averages in the environment
R.ave = 0.77 #average width to length ratio for microplastics in marine enviornment
p.ave = 1.10 #average density in marine surface water

###function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(
  a, #default alpha from Koelmans et al (2020)
  x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault
  x1D, #1 um is lower default size
  x2M,
  x1M
) {
  CF = (x2D^(1 - a) - x1D^(1 - a)) / (x2M^(1 - a) - x1M^(1 - a))
  return(CF)
}

#### equations for mu_x_poly (note that there are three depending on certain alphas for limits of equation)
#generalizable if a.x =2 or not
mux.polyfnx_generalizable = Vectorize(
  function(a.x, x_UL, x_LL) {
    if (a.x == 1) {
      # in case a.x = 1
      mux.poly = (x_UL - x_LL) / (log(x_UL / x_LL))
      return(mux.poly)
    }
    if (a.x == 2) {
      # in case a.x = 2
      mux.poly = (log10(x_UL / x_LL)) / (x_LL^(-1) - x_UL^-1)
      return(mux.poly)
    } else {
      #in case alpha is not 2 or 1
      mux.poly = ((1 - a.x) / (2 - a.x)) *
        ((x_UL^(2 - a.x) - x_LL^(2 - a.x)) / (x_UL^(1 - a.x) - x_LL^(1 - a.x)))
      return(mux.poly)
    }
  },
  vectorize.args = "a.x"
) # if Vectorize isn't here, the if else won't work
## ^^ Note that the above generalizable function doesn't play well with mutate(case_when), likely due to some bug with dplyr. I don't have a solution to this, so a special equation will need to be used when those values are used...

#in case alpha is not 1 or 2
mux.polyfnx = function(a.x, x_UL, x_LL) {
  mux.poly = ((1 - a.x) / (2 - a.x)) *
    ((x_UL^(2 - a.x) - x_LL^(2 - a.x)) / (x_UL^(1 - a.x) - x_LL^(1 - a.x)))
  return(mux.poly)
}

##### If alpha does equal 2 #####
mux.polyfnx2 = function(a.x, x_UL, x_LL) {
  mux.poly = (log(x_UL / x_LL)) / (x_LL^(-1) - x_UL^-1)
  return(mux.poly)
}

##### If alpha equals 1 #####
mux.polyfnx1 = function(a.x, x_UL, x_LL) {
  mux.poly = (x_UL - x_LL) / (log(x_UL / x_LL)) #natural log
  return(mux.poly)
}

### Calculating max ingestible parameters ###
## function to calcualte min and max ingestible surface area ##
SAfnx = function(
  a, # a = 0.5 * length
  b, # b = 0.5 * width
  c # c = 0.5 * height (note that hieght is 0.67 * width)
) {
  SA = 4 * pi * (((a * b)^1.6 + (a * c)^1.6 + (b * c)^1.6) / 3)^(1 / 1.6)
  return(SA)
}

## max ingestible volume ##

volumefnx = function(R, L) {
  volume = 0.111667 * pi * R^2 * L^3 #assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
  return(volume)
}

volumefnx_poly = function(width, length) {
  height = 0.67 * width
  volume = (4 / 3) * pi * (length / 2) * (width / 2) * (height / 2) #assumes height = 0.67 * Width
  return(volume)
}

#max ingestible mass (only used for mu_mono calculations)
massfnx = function(R, L, p) {
  mass = p * #density (g/cm^3)
    0.111667 *
    pi *
    R^2 *
    L^3 * # volume (um^3): assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
    1 /
    1e12 *
    1e6 #correction factor
  return(mass)
}

massfnx_poly = function(width, length, p) {
  height = 0.67 * width
  volume = (4 / 3) * pi * (length / 2) * (width / 2) * (height / 2) #assumes height = 0.67 * Width
  mass = p * #density (g/cm^3)
    volume * # volume (um^3): assumes height = 0.67 * Width, and Width:Length ratio is 'R' (compartment-specific)
    1 /
    1e12 *
    1e6 #correction factor
  return(mass)
}

#max ingestible specific surface area
SSAfnx = function(
  sa, #surface area, calcaulted elsewhere
  m
) {
  #mass, calculated elsewhere
  SSA = sa / m
  return(SSA)
}

#max ingestible specific surface area
SSA.inversefnx = function(
  sa, #surface area, calcaulted elsewhere
  m
) {
  #mass, calculated elsewhere
  SSA.inverse = m / sa
  return(SSA.inverse)
}

#join alpha values for each data point
aoc_intermediate_alphas <- aoc_intermediate %>%
  mutate(alpha = alpha) %>%
  mutate(a.sa = a.sa) %>%
  mutate(a.v = a.v) %>%
  mutate(a.m = a.m) %>%
  mutate(a.ssa = a.ssa) %>%
  mutate(R.ave = R.ave) %>%
  mutate(p.ave = p.ave)

# calculate ERM for each species
aoc_final <- aoc_intermediate_alphas %>%
  #### TISSUE TRANSLOCATION ####
  # define upper size length for Translocation
  #set to 83um for upper limit or max size ingest, whichever is smaller
  mutate(
    x2M_trans = case_when(
      is.na(max.size.ingest.um) ~ upper.tissue.trans.size.um,
      max.size.ingest.um < upper.tissue.trans.size.um ~ max.size.ingest.um,
      max.size.ingest.um >
        upper.tissue.trans.size.um ~ upper.tissue.trans.size.um
    )
  ) %>%

  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_trans = dose.particles.mL.master) %>%
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(
    mu.p.poly_trans = mux.polyfnx(
      a.x = alpha, #alpha for particles
      x_UL = x2M_trans, #upper ingestible size limit (width of particle)
      x_LL = x1M_set
    )
  ) %>%
  # polydisperse effect threshold for particles
  mutate(
    EC_poly_p.particles.mL_trans = (EC_mono_p.particles.mL_trans * mu.p.mono) /
      mu.p.poly_trans
  ) %>%
  #calculate CF_bio for all conversions
  mutate(
    CF_bio_trans = CFfnx(
      x1M = x1M_set, #lower size bin
      x2M = x2M_trans, #upper translocatable
      x1D = x1D_set, #default
      x2D = x2D_set, #default
      a = alpha
    )
  ) %>%
  ## Calculate environmentally relevant effect threshold for particles
  mutate(
    EC_env_p.particles.mL_trans = EC_poly_p.particles.mL_trans * CF_bio_trans
  ) %>% #aligned particle effect concentraiton (1-5000 um)

  #### Surface area ERM ####
  ##--- environmental calculations ---###
  #calculate lower translocatable surface area
  mutate(
    x_LL_sa_trans = SAfnx(
      a = 0.5 * x1D_set, #length
      b = 0.5 * x1D_set, #0.5 * R.ave * x1D_set, #width
      c = 0.5 * x1D_set #0.5 * R.ave * 0.67 * x1D_set #height
    )
  ) %>%
  #calculate upper translocatable surface area
  mutate(
    x_UL_sa_trans = SAfnx(
      a = 0.5 * x2M_trans,
      b = 0.5 * x2M_trans, #width #0.5 * R.ave * x2M,
      c = 0.5 * x2M_trans #heigth #0.5 * R.ave * 0.67 * x2M
    )
  ) %>%
  #calculate mu_x_poly (env) for surface area
  mutate(mu.sa.poly_trans = mux.polyfnx(a.sa, x_UL_sa_trans, x_LL_sa_trans)) %>%

  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(
    mu.sa.mono = case_when(
      polydispersity == "monodisperse" ~ particle.surface.area.um2, # use reported surface area in monodisperse
      polydispersity == "polydisperse" ~ mux.polyfnx(
        a.x = a.sa,
        x_LL = particle.surface.area.um2.min,
        x_UL = particle.surface.area.um2.max
      )
    )
  ) %>%

  #calculate polydisperse effect concentration for surface area (particles/mL)
  mutate(
    EC_poly_sa.particles.mL_trans = (EC_mono_p.particles.mL_trans *
      mu.sa.mono) /
      mu.sa.poly_trans
  ) %>%
  #calculate environmentally realistic effect threshold
  mutate(
    EC_env_sa.particles.mL_trans = EC_poly_sa.particles.mL_trans * CF_bio_trans
  ) %>%

  ##### FOOD DILUTION ####
  # define upper size length for ingestion
  mutate(
    x2M_ingest = case_when(
      is.na(max.size.ingest.um) ~ x2D_set,
      max.size.ingest.um < x2D_set ~ max.size.ingest.um,
      max.size.ingest.um > x2D_set ~ x2D_set
    )
  ) %>% #set to 5,000 as upper limit or max size ingest, whichever is smaller
  # calculate effect threshold for particles
  mutate(EC_mono_p.particles.mL_ingest = dose.particles.mL.master) %>%
  mutate(mu.p.mono = 1) %>% #mu_x_mono is always 1 for particles to particles
  mutate(
    mu.p.poly_ingest = mux.polyfnx(
      a.x = alpha, #alpha for particles
      x_UL = x2M_ingest, #upper ingestible size limit
      x_LL = x1M_set
    )
  ) %>%
  # polydisperse effect threshold for particles
  mutate(
    EC_poly_p.particles.mL_ingest = (EC_mono_p.particles.mL_ingest *
      mu.p.mono) /
      mu.p.poly_ingest
  ) %>%
  #calculate CF_bio for all conversions
  mutate(
    CF_bio_ingest = CFfnx(
      x1M = x1M_set, #lower size bin
      x2M = x2M_ingest, #upper ingestible length
      x1D = x1D_set, #default
      x2D = x2D_set, #default upper size range
      a = alpha
    )
  ) %>%
  ## Calculate environmentally relevant effect threshold for particles
  mutate(
    EC_env_p.particles.mL_ingest = EC_poly_p.particles.mL_ingest * CF_bio_ingest
  ) %>% #aligned particle effect concentraiton (1-5000 um)

  #### volume ERM ####
  ##--- environmental calculations ---###
  #calculate lower ingestible volume
  mutate(x_LL_v_ingest = volumefnx_poly(length = x1D_set, width = x1D_set)) %>%
  #calculate maximum ingestible volume
  mutate(
    x_UL_v_ingest = volumefnx_poly(
      length = x2M_ingest, # length-limited
      #x2D_set, #upper definiton (accouunts for fibers) CONSERVATIVE
      width = x2M_ingest
    )
  ) %>% #ingestion-limited
  # calculate mu.v.poly
  mutate(mu.v.poly_ingest = mux.polyfnx(a.v, x_UL_v_ingest, x_LL_v_ingest)) %>%
  ##--- laboratory calculations ---###
  ## define mu_x_mono OR mu_x_poly (lab) for alignment to ERM  #
  #(note that if mixed particles were used, a different equation must be used)
  mutate(
    mu.v.mono = case_when(
      polydispersity == "monodisperse" ~ particle.volume.um3, # use reported volume in monodisperse
      polydispersity == "polydisperse" ~ mux.polyfnx(
        a.x = a.v,
        x_LL = particle.volume.um3.min,
        x_UL = particle.volume.um3.max
      )
    )
  ) %>%

  #calculate polydisperse effect concentration for volume (particles/mL)
  mutate(
    EC_poly_v.particles.mL_ingest = (EC_mono_p.particles.mL_ingest *
      mu.v.mono) /
      mu.v.poly_ingest
  ) %>%
  #calculate environmentally realistic effect threshold
  mutate(
    EC_env_v.particles.mL_ingest = EC_poly_v.particles.mL_ingest * CF_bio_ingest
  ) %>%

  ###### CLEANUP #####
  mutate(
    particles.mL.ox.stress = EC_env_sa.particles.mL_trans,
    particles.mL.food.dilution = EC_env_v.particles.mL_ingest
  ) |>
  mutate(year = as.numeric(year))


### preparte data for plotting
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
aoc_final_with_id <- aoc_final %>%
  drop_na(tech.a1, risk.b1, total.quality, risk.quality, technical.quality) %>%
  distinct(across(all_of(experiment_vars)), .keep_all = TRUE) %>%
  mutate(experimentID = sapply(seq_len(n()), function(x) uuid::UUIDgenerate()))

# long data
long_time <- aoc_final_with_id %>%
  dplyr::select(c(
    year,
    doi,
    risk.quality,
    technical.quality,
    total.quality
  )) %>%
  mutate(year_date = lubridate::ymd(year, truncated = 2L)) %>%
  pivot_longer(
    cols = -c(year_date, year, doi),
    names_to = "quality_type",
    values_to = "score"
  ) %>%
  group_by(year, quality_type, doi) %>% #get just one score from each study
  dplyr::summarize(score = max(score, na.rm = TRUE)) |>
  # Ensure publication year is numeric
  mutate(year = as.numeric(year))

saveRDS(long_time, file = "Doyle-et-al-2025/Data/long_time_data.rds")

# import aoc_final_jrnl (prepared in journal_analysis_get_process_data.R)
aoc_final_jrnl <- readRDS("Doyle-et-al-2025/Data/aoc_final_jrnl.rds")

# annotate whether effect was found 
aoc_effects <- aoc_final_with_id %>%
  group_by(experimentID) %>%
  summarize(
     all_na_or_honec = all((is.na(effect.metric) | effect.metric == "HONEC") & effect_f == "No", na.rm = TRUE),
    .groups = 'drop'  # Drop grouping after summarizing
  ) %>%
  mutate(
    effect_status = factor(if_else(all_na_or_honec, "no effect found", "effect found"))
  )

aoc_effects_meta <- left_join(aoc_effects, 
                              #join with metadata - but just get experiment-level data
                             aoc_final_with_id,
                              by = "experimentID") %>% 
  mutate(effect_status_binary = case_when(
    effect_status == "effect found" ~ 1,
    effect_status == "no effect found" ~ 0
  ))

aoc_effects_meta_jrnl <- left_join(aoc_effects_meta,
                                   aoc_final_jrnl %>% 
                                     distinct(doi, year, journal_if, journal_abbr),
                                   by = "doi")  %>% 
  mutate(journal_abbr = as.factor(journal_abbr)) %>% 
  rename(year = year.x)

# save aoc_effects_meta_jrnl as RDS
saveRDS(aoc_effects_meta_jrnl, file = "Doyle-et-al-2025/Data/aoc_effects_meta_jrnl.rds")
