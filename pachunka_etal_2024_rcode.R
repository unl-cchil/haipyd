## ---
##
## Script name: pachunka_etal_2024_rcode.R
##
## Purpose of script: Analyze human-animal interaction/positive youth development data
##
## Author: Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com) and Jay Jeffries (jjeffries8@unl.edu)
##
## Date Created: 2024-08-22
##
## Date Finalized: 2024-11-11
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licencor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
##
## ---

#Load Packages ----

suppressPackageStartupMessages({
  library(BayesFactor)
  library(cocoon)
  library(flextable)
  library(ggbeeswarm)
  library(gtsummary)
  
  library(Hmisc)
  library(labelled)
  library(lavaan)
  library(papaja)
  library(patchwork)
  library(psych)
  library(semTable)
  library(tidyverse)
})


# Create Functions ----

# Calculate reliability (Cronbach's alpha)
calculate_alpha <- function(x, scale1, scale2 = NULL, name = scale1) {
  if (is.null(scale2)) {
    scale_data <- x |>
      select(starts_with(scale1))
  } else {
    scale_data <- x |>
      select(starts_with(scale1) | starts_with(scale2))
  }
  items <- names(scale_data)
  cronalpha <- psych::alpha(scale_data, warnings = FALSE)
  scale_clean <- gsub("\\.\\.\\.", "", scale1) |>
    gsub("_", "", x = _)
  print(paste0(
    length(items),
    " items in the ",
    name,
    " scale with a Cronbach's alpha of ",
    round(cronalpha$total$raw_alpha, 3)
  ))
  list(items = items, nitems = length(items), alpha = cronalpha)
}

# Extract regression coefficients from SEM models
extract_estimates_se <- function(mod, names = NULL) {
  mod_summary <- summary(mod)
  mod_standard <- standardizedSolution(mod)
  if ("pe" %in% names(mod_summary)) {
    unstandard <- mod_summary$pe
  }
  unstd <- unstandard |>
    filter(op == "=~") |>
    select(scale = lhs, subscale = rhs, unstd = est, unstd_se = se)
  std <- mod_standard |>
    filter(op == "=~") |>
    select(scale = lhs, subscale = rhs, std = est.std, std_se = se)
  if (is.null(names)) {
    names = std$subscale
  } else {
    if (length(names) != nrow(std)) {
      stop("Length of vector `names` does not match the number of subscales.")
    }
  }
  unstd |>
    full_join(std, by = join_by(scale, subscale)) |>
    mutate(subscale = names)
}

# Extract model fit indices from SEM models
extract_model_fit <- function(mod) {
  fit <- fitMeasures(mod)
  c(
    fit[names(fit) == "chisq"],
    fit[names(fit) == "df"],
    fit[names(fit) == "pvalue"],
    fit[names(fit) == "cfi"],
    fit[names(fit) == "rmsea"],
    fit[names(fit) == "rmsea.ci.lower"],
    fit[names(fit) == "rmsea.ci.upper"],
    fit[names(fit) == "srmr"]
  )
}

# Extract regression info from SEM models
extract_regression <- function(model, dv, grp = NULL) {
  model_se <- lavaan::standardizedSolution(model)
  if (is.null(grp)) {
    model_se |>
      filter(op == "~" & rhs == dv) |>
      select(dv = rhs, predictor = lhs, est.std, se, z, pvalue) |>
      mutate(
        est.std = format_num(est.std, digits = 3),
        est.star = case_when(
          pvalue < 0.05 ~ paste0(est.std, "*"),
          pvalue >= 0.05 ~ as.character(est.std)
        )
      )
  } else {
    model_se |>
      filter(op == "~" & rhs == dv & group == grp) |>
      select(dv = rhs, predictor = lhs, est.std, se, z, pvalue) |>
      mutate(
        est.std = format_num(est.std, digits = 3),
        est.star = case_when(
          pvalue < 0.05 ~ paste0(est.std, "*"),
          pvalue >= 0.05 ~ as.character(est.std)
        )
      )
  }
}

# Rescale scale values (from scales::rescale.numeric method)
rescale <- function(
  x,
  to = c(0, 1),
  from = range(x, na.rm = TRUE, finite = TRUE)
) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

# Select regressions for specific dependent variable
select_regressions <- function(x, colname) {
  x |>
    select(predictor, !!colname := est.star)
}


# Import Data ----

data_dictionary <- read.csv(
  "pachunka_etal_2024_datadictionary.csv",
  na = "NA"
)
hai_data <- read.csv("pachunka_etal_2024_data.csv", na = "NA") |>
  labelled::set_variable_labels(.labels = data_dictionary$label)

# Split study data
hai_data1 <- filter(hai_data, !grepl("REP", source))
hai_data2 <- filter(hai_data, grepl("REP", source))

# Check data dimensions are correct
stopifnot(dim(hai_data1) == c(432, 206))
stopifnot(dim(hai_data2) == c(265, 206))


# Reliability ----

scales1 <- c(
  "ccas",
  "laps",
  "mrcps",
  "aas",
  "caring",
  "character",
  "competence",
  "confidence",
  "connection",
  "contribution",
  "cesd",
  "stait"
)
reliability1 <- map(scales1, ~ calculate_alpha(hai_data1, .x)) |>
  setNames(scales1)

scales2 <- c(
  "mrcps",
  "caring",
  "character",
  "competence",
  "confidence",
  "connection",
  "contribution",
  "cesd",
  "isr"
)
attach_alpha2 <- calculate_alpha(
  hai_data2,
  scale1 = "attach",
  scale2 = "ccas",
  name = "attachment"
)
animaluse_alpha2 <- calculate_alpha(
  hai_data2,
  scale1 = "animaluse",
  scale2 = "aas",
  name = "animaluse"
)
reliability2 <- map(scales2, ~ calculate_alpha(hai_data2, .x)) |>
  setNames(scales2) |>
  list_modify(attachment = attach_alpha2, animaluse = animaluse_alpha2)


# Study 1 ----

## Data Aggregation, Parcelling ----
study1_data <- hai_data1 |>
  rowwise() |>
  mutate(
    # Caring parcels
    caring1 = mean(c_across(starts_with("caring1"))),
    caring2 = mean(c_across(starts_with("caring2"))),
    caring3 = mean(c_across(starts_with("caring3"))),
    # Character parcels
    conbeh = mean(c_across(starts_with("character_conbeh"))),
    perval = mean(c_across(starts_with("character_perval"))),
    soccon = mean(c_across(starts_with("character_soccon"))),
    valdiv = mean(c_across(starts_with("character_valdiv"))),
    # Competence parcels
    accomp = mean(c_across(starts_with("competence_accomp"))),
    physcomp = mean(c_across(starts_with("competence_physcomp"))),
    soccomp = mean(c_across(starts_with("competence_soccomp"))),
    # Confidence parcels
    appear = mean(c_across(starts_with("confidence_appear"))),
    posid = mean(c_across(starts_with("confidence_posid"))),
    selfworth = mean(c_across(starts_with("confidence_selfworth"))),
    # Connection parcels
    confam = mean(c_across(starts_with("connection_confam"))),
    conneigh = mean(c_across(starts_with("connection_conneigh"))),
    conpeer = mean(c_across(starts_with("connection_conpeer"))),
    consch = mean(c_across(starts_with("connection_consch"))),
    # Contribution parcels
    cont1 = mean(c_across(starts_with("contribution1"))),
    cont2 = mean(c_across(starts_with("contribution2"))),
    cont3 = mean(c_across(starts_with("contribution3"))),
    depress1 = mean(c_across(c(cesd2, cesd3, cesd10, cesd11, cesd14))),
    depress2 = mean(c_across(c(cesd5, cesd6, cesd9, cesd13, cesd19))),
    depress3 = mean(
      c_across(c(cesd1, cesd15, cesd17, cesd18, cesd20)),
      na.rm = TRUE
    ),
    show_compete = recode(
      showanimals_frequency,
      `1` = 0.083,
      `2` = 0.25,
      `3` = 0.5,
      `4` = 0.83
    ),
    feeding = `animal_activities...Feeding`,
    ,
    cleaning = `animal_activities...Cleaning`,
    grooming = `animal_activities...Grooming`,
    training = `animal_activities...Training`,
    playing = recode(
      `animal_activities...Petting.playing`,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 12,
      `5` = 30
    ),
    therapy = recode(
      `animal_activities...Engaging.in.therapy.sessions`,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 12,
      `5` = 30
    ),
    riding = recode(
      `animal_activities...Riding.handling`,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 12,
      `5` = 30
    ),
    haiint = sum(
      across(c("playing", "therapy", "riding", "show_compete")),
      na.rm = TRUE
    ),
    haicare = mean(
      c_across(c("feeding", "cleaning", "grooming", "training")),
      na.rm = TRUE
    ),
    haicare = rescale(haicare, to = c(0, 4), from = c(0, 5)),
    haiact = case_when(haiint > 0 ~ 1, haiint == 0 ~ 0),
    haipet = factor(
      case_when(ownership == "Yes" ~ 1, ownership == "No" ~ 0),
      levels = c(0, 1),
      labels = c("No animal", "Animal owner")
    ),
    attach_score = mean(c_across(c(ccas1, ccas2, ccas3))),
    commit_score = mean(c_across(c(mrcps1, mrcps2, mrcps3, mrcps4))),
    animaluse_score = mean(c_across(c(aas1, aas2, aas3, aas4, aas5))),
    caring_score = sum(c_across(starts_with("caring"))),
    character_score = sum(c_across(starts_with("character"))),
    competence_score = sum(c_across(starts_with("competence"))),
    confidence_score = sum(c_across(starts_with("confidence"))),
    connection_score = sum(c_across(starts_with("connection"))),
    cesd_score = sum(c_across(starts_with("cesd"))),
    stait_score = sum(c_across(starts_with("stait"))),
    member = case_when(
      ffa4h_membership == "4-H only" ~ 1,
      ffa4h_membership == "FFA only" ~ 1,
      ffa4h_membership == "4-H and FFA" ~ 1,
      ffa4h_membership == "Neither" ~ 0
    )
  )

## Measurement Models ----
### Caring Model ----
caring_model <- '
caring =~ NA*caring1 + caring2 + caring3
caring ~~ 1*caring
'

fit_caring_model1 <- sem(
  caring_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
caring_estimates1 <- extract_estimates_se(
  fit_caring_model1,
  names = c("Caring 1", "Caring 2", "Caring 3")
)

### Character Model ----
char_model <- '
character =~ NA*conbeh + perval + soccon + valdiv
character ~~ 1*character
'

fit_char_model1 <- sem(
  char_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
char_estimates1 <- extract_estimates_se(
  fit_char_model1,
  names = c(
    "Conduct behavior",
    "Personal values",
    "Social conscience",
    "Values diversity"
  )
)

### Competence Model ----
comp_model <- '
competence =~ NA*accomp + physcomp + soccomp
competence ~~ 1*competence
'

fit_comp_model1 <- sem(
  comp_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
comp_estimates1 <- extract_estimates_se(
  fit_comp_model1,
  names = c("Academic", "Physical", "Social")
)

### Confidence Model ----
conf_model <- '
confidence =~ NA*appear + posid + selfworth
confidence ~~ 1*confidence
'

fit_conf_model1 <- sem(
  conf_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
conf_estimates1 <- extract_estimates_se(
  fit_conf_model1,
  names = c("Physical appearance", "Positive identity", "Self-worth")
)

### Connection Model ----
conn_model <- '
connection =~ NA*confam + conneigh + consch + conpeer
connection ~~ 1*connection
'

fit_conn_model1 <- sem(
  conn_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
conn_estimates1 <- extract_estimates_se(
  fit_conn_model1,
  names = c("Family", "Neighborhood", "Peer connection", "School")
)

### Contribution Model ----
contr_model <- '
contribution =~ NA*cont1 + cont2 + cont3
contribution ~~ 1*contribution
'

fit_contr_model1 <- sem(
  contr_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
contr_estimates1 <- extract_estimates_se(
  fit_contr_model1,
  names = c("Contribution 1", "Contribution 2", "Contribution 3")
)

### Depression Model ----
dep_model <- '
depression =~ NA*depress1 + depress2 + depress3
depression ~~ 1*depression
'

fit_dep_model1 <- sem(
  dep_model,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
dep_estimates1 <- extract_estimates_se(
  fit_dep_model1,
  names = c("Depress 1", "Depress 2", "Depress 3")
)

### Attachment Model ----
attach_model1 <- '
attachment =~ NA*ccas1 + ccas2 + ccas3
attachment ~~ 1*attachment
'

fit_attach_model1 <- sem(
  attach_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
attach_estimates1 <- extract_estimates_se(
  fit_attach_model1,
  names = c("HAI01", "HAI03", "HAI13")
)

### Commitment Model ----
commit_model1 <- '
commitment =~ NA*mrcps1 + mrcps2 + mrcps3 + mrcps4
commitment ~~ 1*commitment
'

fit_commit_model1 <- sem(
  commit_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
commit_estimates1 <- extract_estimates_se(
  fit_commit_model1,
  names = c("HAI05", "HAI08", "HAI09", "HAI12")
)

### Animal Use Model ----
animaluse_model1 <- '
animaluses =~ NA*aas1 + aas2 + aas3 + aas4 + aas5
animaluses ~~ 1*animaluses
'

fit_animaluse_model1 <- sem(
  animaluse_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
animaluse_estimates1 <- extract_estimates_se(
  fit_animaluse_model1,
  names = c("HAI14", "HAI15", "HAI16", "HAI17", "HAI18")
)


### Loadings Tables ----
loadings_pyd1 <- bind_rows(
  caring_estimates1,
  char_estimates1,
  comp_estimates1,
  conf_estimates1,
  conn_estimates1,
  contr_estimates1,
  dep_estimates1
)
loadings_attcommmor1 <- bind_rows(
  attach_estimates1,
  commit_estimates1,
  animaluse_estimates1
)


### Model Fit Table ----
meas_model_fits1 <- map_dfr(
  list(
    fit_caring_model1,
    fit_char_model1,
    fit_comp_model1,
    fit_conf_model1,
    fit_conn_model1,
    fit_dep_model1,
    fit_attach_model1,
    fit_commit_model1,
    fit_animaluse_model1
  ),
  extract_model_fit
) |>
  select(-contains(".ci.")) |>
  mutate(
    measure = c(
      "Caring",
      "Character",
      "Competence",
      "Confidence",
      "Connection",
      "Depression",
      "Attachment",
      "Commitment",
      "Perception of animal use"
    ),
    .before = 1
  )


## Pet Ownership Predicts PYD (Structural Equation Model 1.2) ----
### Full Model ----
structure_haipet1 <- '
caring + character + competence + confidence + connection + contribution + depression ~ haipet
'

covs_sixcs <- '
caring ~~ character + competence + confidence + connection + contribution
character ~~ competence + confidence + connection + contribution
competence ~~ confidence + connection + contribution
confidence ~~ connection + contribution
connection ~~ contribution
'

full_haipet_model1 <- paste(
  char_model,
  comp_model,
  conf_model,
  caring_model,
  conn_model,
  contr_model,
  dep_model,
  structure_haipet1,
  covs_sixcs
)
fit_haipet_model1 <- sem(
  full_haipet_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)

haipet_regression1 <- extract_regression(fit_haipet_model1, "haipet")

haipet_fit1 <- extract_model_fit(fit_haipet_model1)


### Moderation of Group Membership on Pet Ownership to PYD ----
fit_haipet_mod1 <- sem(
  full_haipet_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)

lavTestScore(fit_haipet_mod1)
# All p-values > 0.05
lavTestLRT(fit_haipet_model1, fit_haipet_mod1)
haipet_member_mod1 <- extract_regression(fit_haipet_mod1, "haipet", grp = 1)


## Activities, Care, and Interactions (Structural Equation Model 1.1) ----
### Full Model ----
structure_haiactintcare1 <- '
caring + character + competence + confidence + connection + contribution + depression ~ haiact
caring + character + competence + confidence + connection + contribution + depression ~ haiint
caring + character + competence + confidence + connection + contribution + depression ~ haicare
'

full_haiactintcare_model1 <- paste(
  caring_model,
  char_model,
  comp_model,
  conf_model,
  conn_model,
  contr_model,
  dep_model,
  structure_haiactintcare1,
  covs_sixcs
)
fit_haiactintcare_model1 <- sem(
  full_haiactintcare_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)

haicare_regression1 <- extract_regression(fit_haiactintcare_model1, "haicare")
haiact_regression1 <- extract_regression(fit_haiactintcare_model1, "haiact")
haiint_regression1 <- extract_regression(fit_haiactintcare_model1, "haiint")

haiactintcare_fit1 <- extract_model_fit(fit_haiactintcare_model1)


### Moderation of Group Membership on HAI to PYD ----
fit_haiactintcare_mod1 <- sem(
  full_haiactintcare_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)
fit_haiactintcare_mod_sum1 <- summary(
  fit_haiactintcare_mod1,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_haiactintcare_model1, fit_haiactintcare_mod1)
lavTestScore(fit_haiactintcare_mod1)
parTable(fit_haiactintcare_mod1)

fit_haiactintcare_eq1_mod1 <- sem(
  full_haiactintcare_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group.partial = c("connection ~ haiint"),
  group = "member"
)

lavTestLRT(fit_haiactintcare_mod1, fit_haiactintcare_eq1_mod1)
summary(fit_haiactintcare_eq1_mod1, fit.measures = TRUE, standardized = TRUE)
# standard_output_haiactintcare_member1 <- standardizedSolution(fit_haiactintcare_eq1_mod1)

## Attachment, Commitment, Animal Use (Structural Equation Model 2) ----
### Latent Correlations ----
latent_cors <- '
attachment ~~ commitment + animaluses
commitment ~~ animaluses
'

latent_cors_model1 <- paste(
  attach_model1,
  commit_model1,
  animaluse_model1,
  latent_cors
)
latent_cors_output1 <- sem(
  latent_cors_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
latent_cors_summary1 <- summary(latent_cors_output1, standardized = TRUE)

##### Table ----
latent_cor_table_sample1 <- latent_cors_summary1$pe |>
  filter(
    op == "~~",
    lhs == "attachment" | lhs == "commitment" | lhs == "animaluses",
    rhs == "attachment" | rhs == "commitment" | rhs == "animaluses"
  ) |>
  slice(-c(1:3)) |>
  select(lhs, rhs, std.lv, pvalue) |>
  mutate(
    std.lv = round(std.lv, 3),
    std.path = case_when(
      pvalue < 0.05 ~ paste0(std.lv, "*"),
      pvalue > 0.05 ~ paste0(std.lv, "")
    ),
    row = lhs,
    col = rhs,
  ) |>
  select(-c(std.lv, lhs, rhs, pvalue)) |>
  relocate(c(row, col), .before = std.path) |>
  pivot_wider(names_from = row, values_from = std.path) |>
  rename(attachment1 = attachment, commitment1 = commitment)

### Full Model ----
structure_attcommmor1 <- '
caring + character + competence + confidence + connection + contribution + depression ~ attachment
caring + character + competence + confidence + connection + contribution + depression ~ commitment
caring + character + competence + confidence + connection + contribution + depression ~ animaluses
'

covs_attcommmor <- '
attachment ~~ commitment + animaluses
commitment ~~ animaluses
'

full_attcommmor_model1 <- paste(
  caring_model,
  char_model,
  comp_model,
  conf_model,
  conn_model,
  contr_model,
  dep_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  structure_attcommmor1,
  covs_attcommmor
)

fit_attcommmor_model1 <- sem(
  full_attcommmor_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)

attach_regression1 <- extract_regression(fit_attcommmor_model1, "attachment")
commit_regression1 <- extract_regression(fit_attcommmor_model1, "commitment")
animaluse_regression1 <- extract_regression(fit_attcommmor_model1, "animaluses")

attcommmor_fit1 <- extract_model_fit(fit_attcommmor_model1)


### Moderation of Group Members hip on HAI to Cognitions & Emotions ----

fit_attcommmor_mod1 <- sem(
  full_attcommmor_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)

attcommmor_mod_sum1 <- summary(
  fit_attcommmor_mod1,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestScore(fit_attcommmor_mod1)
parTable(fit_attcommmor_mod1)

fit_attcommmor_eq1_mod1 <- sem(
  full_attcommmor_model1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group.partial = c("depression ~ commitment"),
  group = "member"
)

attcommmor_eq1_mod_sum <- summary(
  fit_attcommmor_eq1_mod1,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_attcommmor_mod1, fit_attcommmor_eq1_mod1)
lavTestScore(fit_attcommmor_eq1_mod1)
parTable(fit_attcommmor_eq1_mod1)

fit_attcommmor_eq2_mod1 <- sem(
  fit_attcommmor_mod1,
  data = study1_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group.partial = c("depression ~ commitment", "caring ~ animaluses"),
  group = "member"
)

attcommmor_eq2_mod_sum <- summary(
  fit_attcommmor_eq2_mod1,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_attcommmor_eq1_mod1, fit_attcommmor_eq2_mod1)
lavTestScore(fit_attcommmor_eq2_mod1)
parTable(fit_attcommmor_eq2_mod1)


### Trimmed Model ----

#### ---- Full Model ----
full_models1 <- '
competence + confidence + character + caring + connection + contribution + depression ~ attachment
competence + confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + contribution + depression ~ animaluses
'

covs <- '
attachment ~~ commitment + animaluses
commitment ~~ animaluses
'

full_model1_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  full_models1,
  covs
)
full_model1 <- sem(
  full_model1_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
full_model1_sum <- summary(full_model1, fit.measures = T)

fit_measures <- c("cfi", "rmsea", "srmr", "chisq", "df", "pvalue")

trim_modelfull1_fit <- full_model1_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "Full model", dchi = " ", ddf = " ")


#### ---- Trimmed Models ----

# Remove contribution ~ animaluse
trim_model1_1 <- '
competence + confidence + character + caring + connection + contribution + depression ~ attachment
competence + confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_1_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_1,
  covs
)
trim_model1_1 <- sem(
  trim_model1_1_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_1_sum <- summary(trim_model1_1, fit.measures = T)
models1 <- c("Full" = full_model1, "Trimmed" = trim_model1_1)

compare_1_1 <- compareLavaan(models1, nesting = "Full > Trimmed", scaled = T)

dchi_1 <- str_sub(compare_1_1$dchi[2], end = -2)
ddf_1 <- compare_1_1$ddf[2]

trim_model1_1_fit <- trim_model1_1_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "1", dchi = dchi_1, ddf = ddf_1)

# Remove depression ~ attachment
trim_model1_2 <- '
competence + confidence + character + caring + connection + contribution ~ attachment
competence + confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_2_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_2,
  covs
)
trim_model1_2 <- sem(
  trim_model1_2_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_2_sum <- summary(trim_model1_2, fit.measures = T)
models2 <- c("Full" = full_model1, "Trimmed" = trim_model1_2)

compare_1_2 <- compareLavaan(models2, nesting = "Full > Trimmed", scaled = T)

dchi_2 <- str_sub(compare_1_2$dchi[2], end = -2)
ddf_2 <- compare_1_2$ddf[2]

trim_model1_2_fit <- trim_model1_2_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "2", dchi = dchi_2, ddf = ddf_2)

# Remove contribution ~ attachment
trim_model1_3 <- '
competence + confidence + character + caring + connection ~ attachment
competence + confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_3_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_3,
  covs
)
trim_model1_3 <- sem(
  trim_model1_3_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_3_sum <- summary(trim_model1_3, fit.measures = T)
models3 <- c("Full" = full_model1, "Trimmed" = trim_model1_3)

compare_1_3 <- compareLavaan(models3, nesting = "Full > Trimmed", scaled = T)

dchi_3 <- str_sub(compare_1_3$dchi[2], end = -2)
ddf_3 <- compare_1_3$ddf[2]

trim_model1_3_fit <- trim_model1_3_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "3", dchi = dchi_3, ddf = ddf_3)

# Remove character ~ attachment
trim_model1_4 <- '
competence + confidence + caring + connection ~ attachment
competence + confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_4_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_4,
  covs
)
trim_model1_4 <- sem(
  trim_model1_4_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_4_sum <- summary(trim_model1_4, fit.measures = T)
models4 <- c("Full" = full_model1, "Trimmed" = trim_model1_4)

compare_1_4 <- compareLavaan(models4, nesting = "Full > Trimmed", scaled = T)

dchi_4 <- str_sub(compare_1_4$dchi[2], end = -2)
ddf_4 <- compare_1_4$ddf[2]

trim_model1_4_fit <- trim_model1_4_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "4", dchi = dchi_4, ddf = ddf_4)

# Remove competence ~ commitment
trim_model1_5 <- '
competence + confidence + caring + connection ~ attachment
confidence + character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_5_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_5,
  covs
)
trim_model1_5 <- sem(
  trim_model1_5_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_5_sum <- summary(trim_model1_5, fit.measures = T)
models5 <- c("Full" = full_model1, "Trimmed" = trim_model1_5)

compare_1_5 <- compareLavaan(models5, nesting = "Full > Trimmed", scaled = T)

dchi_5 <- str_sub(compare_1_5$dchi[2], end = -2)
ddf_5 <- compare_1_5$ddf[2]

trim_model1_5_fit <- trim_model1_5_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "5", dchi = dchi_5, ddf = ddf_5)

# Remove confidence ~ commitment
trim_model1_6 <- '
competence + confidence + caring + connection ~ attachment
character + caring + connection + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_6_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_6,
  covs
)
trim_model1_6 <- sem(
  trim_model1_6_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_6_sum <- summary(trim_model1_6, fit.measures = T)
models6 <- c("Full" = full_model1, "Trimmed" = trim_model1_6)

compare_1_6 <- compareLavaan(models6, nesting = "Full > Trimmed", scaled = T)

dchi_6 <- str_sub(compare_1_6$dchi[2], end = -2)
ddf_6 <- compare_1_6$ddf[2]

trim_model1_6_fit <- trim_model1_6_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "6", dchi = dchi_6, ddf = ddf_6)

# Remove connection ~ commitment
trim_model1_7 <- '
competence + confidence + caring + connection ~ attachment
character + caring + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_7_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_7,
  covs
)
trim_model1_7 <- sem(
  trim_model1_7_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_7_sum <- summary(trim_model1_7, fit.measures = T)
models7 <- c("Full" = full_model1, "Trimmed" = trim_model1_7)

compare_1_7 <- compareLavaan(models7, nesting = "Full > Trimmed", scaled = T)

dchi_7 <- str_sub(compare_1_7$dchi[2], end = -2)
ddf_7 <- compare_1_7$ddf[2]

trim_model1_7_fit <- trim_model1_7_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "7", dchi = dchi_7, ddf = ddf_7)

# Remove caring ~ attachment
trim_model1_8 <- '
competence + confidence + connection ~ attachment
character + caring + contribution + depression ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model1_8_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_8,
  covs
)
trim_model1_8 <- sem(
  trim_model1_8_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_8_sum <- summary(trim_model1_8, fit.measures = T)
models8 <- c("Full" = full_model1, "Trimmed" = trim_model1_8)

compare_1_8 <- compareLavaan(models8, nesting = "Full > Trimmed", scaled = T)

dchi_8 <- str_sub(compare_1_8$dchi[2], end = -2)
ddf_8 <- compare_1_8$ddf[2]

trim_model1_8_fit <- trim_model1_8_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "8", dchi = dchi_8, ddf = ddf_8)

# Remove caring ~ commitment
trim_model1_9 <- '
competence + confidence + connection ~ attachment
character + contribution + depression ~ commitment
caring + character + competence + confidence + connection + depression ~ animaluses
'

trim_model1_9_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_9,
  covs
)
trim_model1_9 <- sem(
  trim_model1_9_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_9_fitstats <- extract_model_fit(trim_model1_9)
trim_model1_9_sum <- summary(trim_model1_9, fit.measures = T)

models9 <- c("Full" = full_model1, "Trimmed" = trim_model1_9)

compare_1_9 <- compareLavaan(models9, nesting = "Full > Trimmed", scaled = T)

dchi_9 <- str_sub(compare_1_9$dchi[2], end = -2)
ddf_9 <- compare_1_9$ddf[2]

trim_model1_9_fit <- trim_model1_9_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "9", dchi = dchi_9, ddf = ddf_9)

# Remove contribution ~ commitment
# STOP model
trim_model1_10 <- '
competence + confidence + connection ~ attachment
character + depression ~ commitment
caring + character + competence + confidence + connection + depression ~ animaluses
'

trim_model1_10_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  attach_model1,
  commit_model1,
  animaluse_model1,
  trim_model1_10,
  covs
)
trim_model1_10 <- sem(
  trim_model1_10_syntax,
  data = study1_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model1_10_sum <- summary(trim_model1_10, fit.measures = T)
models10 <- c("Full" = full_model1, "Trimmed" = trim_model1_10)

compare_1_10 <- compareLavaan(models10, nesting = "Full > Trimmed", scaled = T)

dchi_10 <- str_sub(compare_1_10$dchi[2], end = -2)
ddf_10 <- compare_1_10$ddf[2]

trim_model1_10_fit <- trim_model1_10_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "10", dchi = dchi_10, ddf = ddf_10)


#### ---- Extract Best Model Coefficients -----

attach_regression_trimmed1 <- extract_regression(trim_model1_9, "attachment")
commit_regression_trimmed1 <- extract_regression(trim_model1_9, "commitment")
animaluse_regression_trimmed1 <- extract_regression(trim_model1_9, "animaluses")


#### ---- Combine Summaries ----

options(scipen = 1000000)

wide_fullmodel1_fit <- pivot_wider(
  trim_modelfull1_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = " ", dchi = " ", model = case_when(model == 1 ~ "Full model")) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_1_fit <- pivot_wider(
  trim_model1_1_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_1, dchi = dchi_1) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_2_fit <- pivot_wider(
  trim_model1_2_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_2, dchi = dchi_2) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_3_fit <- pivot_wider(
  trim_model1_3_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_3, dchi = dchi_3) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_4_fit <- pivot_wider(
  trim_model1_4_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_4, dchi = dchi_4) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_5_fit <- pivot_wider(
  trim_model1_5_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_5, dchi = dchi_5) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_6_fit <- pivot_wider(
  trim_model1_6_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_6, dchi = dchi_6) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_7_fit <- pivot_wider(
  trim_model1_7_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_7, dchi = dchi_7) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_8_fit <- pivot_wider(
  trim_model1_8_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_8, dchi = dchi_8) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_9_fit <- pivot_wider(
  trim_model1_9_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_9, dchi = dchi_9) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel1_10_fit <- pivot_wider(
  trim_model1_10_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_10, dchi = dchi_10) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

trim_model1_fits <- rbind(
  wide_fullmodel1_fit,
  wide_trimmodel1_1_fit,
  wide_trimmodel1_2_fit,
  wide_trimmodel1_3_fit,
  wide_trimmodel1_4_fit,
  wide_trimmodel1_5_fit,
  wide_trimmodel1_6_fit,
  wide_trimmodel1_7_fit,
  wide_trimmodel1_8_fit,
  wide_trimmodel1_9_fit,
  wide_trimmodel1_10_fit
) |>
  mutate(
    chisq = format(chisq, digits = 8),
    model = case_when(is.na(model) ~ "Full model", !is.na(model) ~ model)
  ) |>
  relocate(dchi, .before = ddf) |>
  select(-pvalue)


## Group differences -----
group_data1 <- study1_data |>
  select(
    id,
    source,
    member,
    ownership_score = haipet,
    activities_score,
    caring_score:stait_score,
    attach_score,
    commit_score,
    animaluse_score
  )

group_member1_long <- group_data1 |>
  pivot_longer(
    cols = activities_score:animaluse_score,
    names_to = "measure",
    values_to = "value"
  )

group_member1_split <- group_member1_long |>
  split(group_member1_long$measure)

group_member_BFttests <- group_member1_split |>
  map(\(x) drop_na(x, value) |> ttestBF(formula = value ~ member, data = _)) |>
  map_df(~ extractBF(.x)$bf) |>
  pivot_longer(everything(), names_to = "measure", values_to = "bf")

group_member_ttests <- group_member1_split |>
  map(\(x) t.test(formula = value ~ member, data = x)) |>
  map_df(function(x) c(x[["statistic"]], x[["p.value"]])) |>
  mutate(stat = c("tstatistic", "pvalue")) |>
  pivot_longer(!stat, names_to = "measure") |>
  pivot_wider(id_cols = measure, names_from = stat, values_from = value) |>
  full_join(group_member_BFttests, by = "measure") |>
  rowwise() |>
  mutate(
    bf10 = cocoon::format_bf(
      bf,
      cutoff = 1000,
      digits1 = 1,
      italics = FALSE,
      subscript = ""
    ),
    p_value = format_p(pvalue, 3, italics = FALSE)
  )

group_member1_longer <- group_member1_long |>
  left_join(group_member_ttests, by = "measure") |>
  mutate(
    measure = fct_relevel(
      measure,
      "activities_score",
      "attach_score",
      "commit_score",
      "animaluse_score",
      "caring_score",
      "character_score",
      "competence_score",
      "confidence_score",
      "connection_score",
      "cesd_score",
      "stait_score"
    ),
    measure = fct_recode(
      measure,
      "HAI activities" = "activities_score",
      "Attachment" = "attach_score",
      "Commitment" = "commit_score",
      "Perception of animal use" = "animaluse_score",
      "Caring" = "caring_score",
      "Character" = "character_score",
      "Competence" = "competence_score",
      "Confidence" = "confidence_score",
      "Connection" = "connection_score",
      "Depression" = "cesd_score",
      "Anxiety" = "stait_score"
    ),
    member = if_else(member == 1, "Yes", "No")
  )

group_diff_plot1 <- group_member1_longer |>
  mutate(member = fct_relevel(member, "Yes", "No")) |>
  ggplot(aes(x = as.factor(member), y = value)) +
  geom_beeswarm(color = "grey85") +
  stat_summary(fun.data = mean_cl_normal, size = 1.5, shape = "-") +
  facet_wrap(vars(measure), scales = "free_y") +
  geom_text(
    x = 1.5,
    y = Inf,
    label = group_member1_longer$bf10,
    vjust = 11,
    size = 3
  ) +
  geom_text(
    x = 1.5,
    y = Inf,
    label = group_member1_longer$p_value,
    vjust = 13,
    size = 3
  ) +
  labs(x = "4-H/FFA Member", y = "Score") +
  coord_cartesian(xlim = c(1, 2)) +
  theme_bw()


# Study 2 ----

## Data Aggregation, Parcelling ----
study2_data <- hai_data2 |>
  rowwise() |>
  mutate(
    # Caring parcels
    caring1 = mean(c_across(starts_with("caring1"))),
    caring2 = mean(c_across(starts_with("caring2"))),
    caring3 = mean(c_across(starts_with("caring3"))),
    # Character parcels
    conbeh = mean(c_across(starts_with("character_conbeh"))),
    perval = mean(c_across(starts_with("character_perval"))),
    soccon = mean(c_across(starts_with("character_soccon"))),
    valdiv = mean(c_across(starts_with("character_valdiv"))),
    # Competence parcels
    accomp = mean(c_across(starts_with("competence_accomp"))),
    physcomp = mean(c_across(starts_with("competence_physcomp"))),
    soccomp = mean(c_across(starts_with("competence_soccomp"))),
    # Confidence parcels
    appear = mean(c_across(starts_with("confidence_appear"))),
    posid = mean(c_across(starts_with("confidence_posid"))),
    selfworth = mean(c_across(starts_with("confidence_selfworth"))),
    # Connection parcels
    confam = mean(c_across(starts_with("connection_confam"))),
    conneigh = mean(c_across(starts_with("connection_conneigh"))),
    conpeer = mean(c_across(starts_with("connection_conpeer"))),
    consch = mean(c_across(starts_with("connection_consch"))),
    # Contribution parcels
    cont1 = mean(c_across(starts_with("contribution1"))),
    cont2 = mean(c_across(starts_with("contribution2"))),
    cont3 = mean(c_across(starts_with("contribution3"))),
    # Depression parcels
    depress1 = mean(c_across(c(cesd2, cesd3, cesd10, cesd11, cesd14))),
    depress2 = mean(c_across(c(cesd5, cesd6, cesd9, cesd13, cesd19))),
    depress3 = mean(
      c_across(c(cesd1, cesd15, cesd17, cesd18, cesd20)),
      na.rm = TRUE
    ),
    riding = recode(
      animal_activities...Horseback.riding,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 8,
      `5` = 20
    ),
    showing = recode(
      animal_activities...Dog.showing.or.livestock.competitions,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 8,
      `5` = 20
    ),
    club = recode(
      animal_activities...Animal.related.club.or.extracurricular.activity,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 8,
      `5` = 20
    ),
    therapy = recode(
      animal_activities...Volunteering.in.an.animal.shelter.or.animal.therapy.program,
      `0` = 0,
      `1` = 1,
      `2` = 2,
      `3` = 4,
      `4` = 8,
      `5` = 20
    ),
    haiint = sum(
      across(c("riding", "showing", "club", "therapy")),
      na.rm = TRUE
    ),
    haicare = care_score,
    haiact = case_when(haiint > 0 ~ 1, haiint == 0 ~ 0),
    haipet = factor(
      case_when(ownership == "Yes" ~ 1, ownership == "No" ~ 0),
      levels = c(0, 1),
      labels = c("No animal", "Animal owner")
    ),
    attach_score = mean(c_across(c(
      ccas1,
      ccas2,
      ccas3,
      attach1,
      attach2,
      attach3
    ))),
    commit_score = mean(c_across(c(mrcps1, mrcps2, mrcps3, mrcps4))),
    animaluse_score = mean(c_across(c(
      aas1,
      aas3,
      aas4,
      aas5,
      animaluse1,
      animaluse2
    ))),
    caring_score = sum(c_across(starts_with("caring"))),
    character_score = sum(c_across(starts_with("character"))),
    competence_score = sum(c_across(starts_with("competence"))),
    confidence_score = sum(c_across(starts_with("confidence"))),
    connection_score = sum(c_across(starts_with("connection"))),
    cesd_score = sum(c_across(starts_with("cesd")), na.rm = TRUE),
    isr_score = sum(c_across(starts_with("isr"))),
    member = case_when(
      ffa4h_membership == "4H" ~ 1,
      ffa4h_membership == "FFA" ~ 1,
      ffa4h_membership == "4H and FFA" ~ 1,
      ffa4h_membership == "Neither" ~ 0
    )
  )


## Measurement Models ----
### Caring Model ----
fit_caring_model2 <- sem(
  caring_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
caring_estimates2 <- extract_estimates_se(
  fit_caring_model2,
  names = c("Caring 1", "Caring 2", "Caring 3")
)

### Character Model ----
char_model2b <- '
character =~ NA*conbeh + perval + soccon + valdiv
character ~~ 1*character
soccon ~~ 0*soccon
'

fit_char_model2 <- sem(
  char_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
char_estimates2 <- extract_estimates_se(
  fit_char_model2,
  names = c(
    "Conduct behavior",
    "Personal values",
    "Social conscience",
    "Values diversity"
  )
)

### Competence Model ----
fit_comp_model2 <- sem(
  comp_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
comp_estimates2 <- extract_estimates_se(
  fit_comp_model2,
  names = c("Academic", "Physical", "Social")
)

### Confidence Model ----
fit_conf_model2 <- sem(
  conf_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
conf_estimates2 <- extract_estimates_se(
  fit_conf_model2,
  names = c("Physical appearance", "Positive identity", "Self-worth")
)

### Connection Model ----
fit_conn_model2 <- sem(
  conn_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
conn_estimates2 <- extract_estimates_se(
  fit_conn_model2,
  names = c("Family", "Neighborhood", "Peer connection", "School")
)

### Contribution Model ----
fit_contr_model2 <- sem(
  contr_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
contr_estimates2 <- extract_estimates_se(
  fit_contr_model2,
  names = c("Contribution 1", "Contribution 2", "Contribution 3")
)

### Depression Model ----
fit_dep_model2 <- sem(
  dep_model,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
dep_estimates2 <- extract_estimates_se(
  fit_dep_model2,
  names = c("Depress 1", "Depress 2", "Depress 3")
)

### Intentional Self-Regulation (ISR) Model ----
isr_model2 <- '
isr =~ NA*isr3 + isr5 + isr7 + isr8 + isr10 + isr13 + isr15 + isr17 + isr18
isr ~~ 1*isr
'

fit_isr_model2 <- sem(
  isr_model2,
  data = study2_data,
  estimator = "DWLS",
  ordered = TRUE
)
isr_estimates2 <- extract_estimates_se(
  fit_isr_model2,
  names = paste0("ISR", c(3, 5, 7, 8, 10, 13, 15, 17, 18))
)

### Attachment Model ----
attach_model2 <- '
attachment =~ NA*ccas1 + attach1 + ccas2 + attach2 + attach3 + ccas3
attachment ~~ 1*attachment
'

fit_attach_model2 <- sem(
  attach_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
attach_estimates2 <- extract_estimates_se(
  fit_attach_model2,
  names = c("HAI01", "HAI02", "HAI03", "HAI04", "HAI11", "HAI13")
)

### Commitment Model ----
commit_model2 <- '
commitment =~ NA*mrcps1 + mrcps2 + mrcps3 + mrcps4
commitment ~~ 1*commitment
'

fit_commit_model2 <- sem(
  commit_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
commit_estimates2 <- extract_estimates_se(
  fit_commit_model2,
  names = c("HAI05", "HAI08", "HAI09", "HAI12")
)

### Animal Use Model ----
animaluse_model2 <- '
animaluses =~ NA*aas1 + animaluse1 + aas3 + aas4 + aas5 + animaluse2
animaluses ~~ 1*animaluses
'

fit_animaluse_model2 <- sem(
  animaluse_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
animaluse_estimates2 <- extract_estimates_se(
  fit_animaluse_model2,
  names = c("HAI14", "HAI15", "HAI16", "HAI17", "HAI18", "HAI19")
)


### Loadings Tables ----
loadings_pyd2 <- bind_rows(
  caring_estimates2,
  char_estimates2,
  comp_estimates2,
  conf_estimates2,
  conn_estimates2,
  contr_estimates2,
  dep_estimates2,
  isr_estimates2
)
loadings_attcommmor2 <- bind_rows(
  attach_estimates2,
  commit_estimates2,
  animaluse_estimates2
)


### Model Fit Table ----
meas_model_fits2 <- map_dfr(
  list(
    fit_caring_model2,
    fit_char_model2,
    fit_comp_model2,
    fit_conf_model2,
    fit_conn_model2,
    fit_dep_model2,
    fit_isr_model2,
    fit_attach_model2,
    fit_commit_model2,
    fit_animaluse_model2
  ),
  extract_model_fit
) |>
  select(-contains(".ci.")) |>
  mutate(
    measure = c(
      "Caring",
      "Character",
      "Competence",
      "Confidence",
      "Connection",
      "Depression",
      "ISR",
      "Attachment",
      "Commitment",
      "Perception of animal use"
    ),
    .before = 1
  )


## Pet Ownership Predicts PYD (Structural Equation Model 1.2) ----
### Full Model ----
structure_haipet2 <- '
caring + character + competence + confidence + connection + contribution + depression + isr ~ haipet
'

full_haipet_model2 <- paste(
  caring_model,
  char_model,
  comp_model,
  conf_model,
  conn_model,
  contr_model,
  dep_model,
  isr_model2,
  structure_haipet2,
  covs_sixcs
)
fit_haipet_model2 <- sem(
  full_haipet_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)

haipet_regression2 <- extract_regression(fit_haipet_model2, "haipet")

haipet_fit2 <- extract_model_fit(fit_haipet_model2)


### Moderation of Group Membership on Pet Ownership to PYD ----
fit_haipet_mod2 <- sem(
  full_haipet_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)

lavTestScore(fit_haipet_mod2)
parTable(fit_haipet_mod2)


## Activities, Care, and Interactions (Structural Equation Model 1.1) ----
### Full Model ----
structure_haiactintcare2 <- '
caring + character + competence + confidence + connection + contribution + depression + isr ~ haiact
caring + character + competence + confidence + connection + contribution + depression + isr ~ haicare
caring + character + competence + confidence + connection + contribution + depression + isr ~ haiint
'

full_haiactintcare_model2 <- paste(
  caring_model,
  char_model,
  comp_model,
  conf_model,
  conn_model,
  contr_model,
  dep_model,
  isr_model2,
  structure_haiactintcare2,
  covs_sixcs
)
fit_haiactintcare_model2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  estimator = "MLR",
  fixed.x = FALSE
)

fit_haiactintcare_model_summary2 <- summary(
  fit_haiactintcare_model2,
  fit.measures = TRUE,
  standardized = TRUE
)
haiact_regression2 <- extract_regression(fit_haiactintcare_model2, "haiact")
haiint_regression2 <- extract_regression(fit_haiactintcare_model2, "haiint")
haicare_regression2 <- extract_regression(fit_haiactintcare_model2, "haicare")

haiactintcare_fit2 <- extract_model_fit(fit_haiactintcare_model2)


### Moderation of Group Membership on HAI to PYD ----
#### Activities, Care, and Interactions ----
fit_haiactintcare_eq_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)

lavTestScore(fit_haiactintcare_eq_mod2)
parTable(fit_haiactintcare_eq_mod2)

fit_haiactintcare_eq1_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group = "member",
  group.equal = "regressions",
  group.partial = c("contribution ~ haiact")
)
fit_haiactintcare_eq1_mod_sum2 <- summary(
  fit_haiactintcare_eq1_mod2,
  fit.measures = TRUE,
  standardized = TRUE
)
# standard_output_haiactintcare_member2 <- standardizedsolution(fit_haiactintcare_eq1_mod2)

lavTestLRT(fit_haiactintcare_eq_mod2, fit_haiactintcare_eq1_mod2)
lavTestScore(fit_haiactintcare_eq1_mod2)
parTable(fit_haiactintcare_eq1_mod2)

fit_haiactintcare_eq2_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group = "member",
  group.equal = "regressions",
  group.partial = c("contribution ~ haiact", "isr ~ haiint")
)
fit_haiactintcare_eq2_mod_sum2 <- summary(
  fit_haiactintcare_eq2_mod2,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_haiactintcare_eq1_mod2, fit_haiactintcare_eq2_mod2)
lavTestScore(fit_haiactintcare_eq2_mod2)
parTable(fit_haiactintcare_eq2_mod2)

fit_haiactintcare_eq3_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group = "member",
  group.equal = "regressions",
  group.partial = c(
    "contribution ~ haiact",
    "isr ~ haiint",
    "depression ~ haiint"
  )
)
fit_haiactintcare_eq3_mod_sum2 <- summary(
  fit_haiactintcare_eq3_mod2,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_haiactintcare_eq2_mod2, fit_haiactintcare_eq3_mod2)
lavTestScore(fit_haiactintcare_eq3_mod2)
parTable(fit_haiactintcare_eq3_mod2)

fit_haiactintcare_eq4_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group = "member",
  group.equal = "regressions",
  group.partial = c(
    "contribution ~ haiact",
    "isr ~ haiint",
    "depression ~ haiint",
    "confidence ~ haiact"
  )
)
fit_haiactintcare_eq4_mod_sum2 <- summary(
  fit_haiactintcare_eq4_mod2,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_haiactintcare_eq3_mod2, fit_haiactintcare_eq4_mod2)
lavTestScore(fit_haiactintcare_eq4_mod2)
parTable(fit_haiactintcare_eq4_mod2)

fit_haiactintcare_eq5_mod2 <- sem(
  full_haiactintcare_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group = "member",
  group.equal = "regressions",
  group.partial = c(
    "contribution ~ haiact",
    "isr ~ haiint",
    "depression ~ haiint",
    "confidence ~ haiact",
    "competence ~ haiact"
  )
)
fit_haiactintcare_eq5_mod_sum2 <- summary(
  fit_haiactintcare_eq5_mod2,
  fit.measures = TRUE,
  standardized = TRUE
)

lavTestLRT(fit_haiactintcare_eq4_mod2, fit_haiactintcare_eq5_mod2)
lavTestScore(fit_haiactintcare_eq5_mod2)
parTable(fit_haiactintcare_eq5_mod2)

## Attachment, Commitment, Animal Use (Structural Equation Model 2) ----
### Latent Correlations ----
latent_cors_model2 <- paste(
  attach_model2,
  commit_model2,
  animaluse_model2,
  latent_cors
)
latent_cors_output2 <- sem(
  latent_cors_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
latent_cors_summary2 <- summary(latent_cors_output2, standardized = TRUE)

#### Table ----
latent_cor_table_sample2 <- latent_cors_summary2$pe |>
  filter(
    op == "~~",
    lhs == "attachment" | lhs == "commitment" | lhs == "animaluses",
    rhs == "attachment" | rhs == "commitment" | rhs == "animaluses"
  ) |>
  slice(-c(1:3)) |>
  select(lhs, rhs, std.lv, pvalue) |>
  mutate(
    std.lv = round(std.lv, 3),
    std.path = case_when(
      pvalue < 0.05 ~ paste0(std.lv, "*"),
      pvalue > 0.05 ~ paste0(std.lv, "")
    ),
    row = lhs,
    col = rhs,
  ) |>
  select(-c(std.lv, lhs, rhs, pvalue)) |>
  relocate(c(row, col), .before = std.path) |>
  pivot_wider(names_from = row, values_from = std.path) |>
  rename(attachment2 = attachment, commitment2 = commitment)

### Full Model ----
structure_attcommmor2 <- '
caring + character + competence + confidence + connection + contribution + depression + isr ~ attachment
caring + character + competence + confidence + connection + contribution + depression + isr ~ commitment
caring + character + competence + confidence + connection + contribution + depression + isr ~ animaluses
'

full_attcommmor_model2 <- paste(
  caring_model,
  comp_model,
  conf_model,
  char_model,
  conn_model,
  contr_model,
  dep_model,
  isr_model2,
  attach_model2,
  commit_model2,
  animaluse_model2,
  structure_attcommmor2
)
fit_attcommmor_model2 <- sem(
  full_attcommmor_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)

attach_regression2 <- extract_regression(fit_attcommmor_model2, "attachment")
commit_regression2 <- extract_regression(fit_attcommmor_model2, "commitment")
animaluse_regression2 <- extract_regression(fit_attcommmor_model2, "animaluses")

attcommmor_fit2 <- extract_model_fit(fit_attcommmor_model2)


### Moderation of Group Membership on HAI to Cognitions & Emotions ----
fit_attcommmor_model2 <- sem(
  full_attcommmor_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR",
  group.equal = "regressions",
  group = "member"
)

lavTestScore(fit_attcommmor_model2)
parTable(fit_attcommmor_model2)

### Trimmed Model ----
#### ---- Full Model ----
full_model2 <- '
competence + confidence + character + caring + connection + contribution + isr + depression ~ attachment
competence + confidence + character + caring + connection + contribution + isr +  depression ~ commitment
competence + confidence + character + caring + connection + contribution + isr +  depression ~ animaluses
'

full_model2 <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  full_model2,
  covs
)
full_model2 <- sem(
  full_model2,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
full_model2_sum <- summary(full_model2, fit.measures = T)
trim_modelfull2_fit <- full_model2_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "Full model", dchi = " ", ddf = " ")

#### ---- Trimmed Models ----
# Remove depression ~ commitment
trim_model2_1 <- '
competence + confidence + character + caring + connection + contribution + isr + depression ~ attachment
competence + confidence + character + caring + connection + contribution + isr ~ commitment
competence + confidence + character + caring + connection + contribution + isr +  depression ~ animaluses
'

trim_model2_1_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_1,
  covs
)
trim_model2_1 <- sem(
  trim_model2_1_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_1_sum <- summary(trim_model2_1, fit.measures = T)
models1 <- c("Full" = full_model2, "Trimmed" = trim_model2_1)

compare_2_1 <- compareLavaan(models1, nesting = "Full > Trimmed", scaled = T)

dchi_1 <- str_sub(compare_2_1$dchi[2], end = -2)
ddf_1 <- compare_2_1$ddf[2]

trim_model2_1_fit <- trim_model2_1_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "1", dchi = dchi_1, ddf = ddf_1)

# Remove isr ~ attachment
trim_model2_2 <- '
competence + confidence + character + caring + connection + contribution + depression ~ attachment
competence + confidence + character + caring + connection + contribution + isr ~ commitment
competence + confidence + character + caring + connection + contribution + isr +  depression ~ animaluses
'

trim_model2_2_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_2,
  covs
)
trim_model2_2 <- sem(
  trim_model2_2_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_2_sum <- summary(trim_model2_2, fit.measures = T)
models2 <- c("Full" = full_model2, "Trimmed" = trim_model2_2)

compare_2_2 <- compareLavaan(models2, nesting = "Full > Trimmed", scaled = T)

dchi_2 <- str_sub(compare_2_2$dchi[2], end = -2)
ddf_2 <- compare_2_2$ddf[2]

trim_model2_2_fit <- trim_model2_2_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "2", dchi = dchi_2, ddf = ddf_2)

# Remove connection ~ attachment
trim_model2_3 <- '
competence + confidence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + connection + contribution + isr ~ commitment
competence + confidence + character + caring + connection + contribution + isr +  depression ~ animaluses
'

trim_model2_3_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_3,
  covs
)
trim_model2_3 <- sem(
  trim_model2_3_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_3_sum <- summary(trim_model2_3, fit.measures = T)
models3 <- c("Full" = full_model2, "Trimmed" = trim_model2_3)

compare_2_3 <- compareLavaan(models3, nesting = "Full > Trimmed", scaled = T)

dchi_3 <- str_sub(compare_2_3$dchi[2], end = -2)
ddf_3 <- compare_2_3$ddf[2]

trim_model2_3_fit <- trim_model2_3_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "3", dchi = dchi_3, ddf = ddf_3)

# Remove contribution ~ animaluses
trim_model2_4 <- '
competence + confidence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + connection + contribution + isr ~ commitment
competence + confidence + character + caring + connection + isr +  depression ~ animaluses
'

trim_model2_4_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_4,
  covs
)
trim_model2_4 <- sem(
  trim_model2_4_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_4_sum <- summary(trim_model2_4, fit.measures = T)
models4 <- c("Full" = full_model2, "Trimmed" = trim_model2_4)

compare_2_4 <- compareLavaan(models4, nesting = "Full > Trimmed", scaled = T)

dchi_4 <- str_sub(compare_2_4$dchi[2], end = -2)
ddf_4 <- compare_2_4$ddf[2]

trim_model2_4_fit <- trim_model2_4_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "4", dchi = dchi_4, ddf = ddf_4)

# Remove contribution ~ animaluses
trim_model2_5 <- '
competence + confidence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + connection + isr ~ commitment
competence + confidence + character + caring + connection + isr +  depression ~ animaluses
'

trim_model2_5_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_5,
  covs
)
trim_model2_5 <- sem(
  trim_model2_5_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_5_sum <- summary(trim_model2_5, fit.measures = T)
models5 <- c("Full" = full_model2, "Trimmed" = trim_model2_5)

compare_2_5 <- compareLavaan(models5, nesting = "Full > Trimmed", scaled = T)

dchi_5 <- str_sub(compare_2_5$dchi[2], end = -2)
ddf_5 <- compare_2_5$ddf[2]

trim_model2_5_fit <- trim_model2_5_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "5", dchi = dchi_5, ddf = ddf_5)

# Remove
trim_model2_6 <- '
competence + confidence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + connection + isr ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model2_6_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_6,
  covs
)
trim_model2_6 <- sem(
  trim_model2_6_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_6_sum <- summary(trim_model2_6, fit.measures = T)
models6 <- c("Full" = full_model2, "Trimmed" = trim_model2_6)

compare_2_6 <- compareLavaan(models6, nesting = "Full > Trimmed", scaled = T)

dchi_6 <- str_sub(compare_2_6$dchi[2], end = -2)
ddf_6 <- compare_2_6$ddf[2]

trim_model2_6_fit <- trim_model2_6_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "6", dchi = dchi_6, ddf = ddf_6)

# Remove confidence ~ attachment
trim_model2_7 <- '
competence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + connection + isr ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model2_7_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_7,
  covs
)
trim_model2_7 <- sem(
  trim_model2_7_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_7_sum <- summary(trim_model2_7, fit.measures = T)
models7 <- c("Full" = full_model2, "Trimmed" = trim_model2_7)

compare_2_7 <- compareLavaan(models7, nesting = "Full > Trimmed", scaled = T)

dchi_7 <- str_sub(compare_2_7$dchi[2], end = -2)
ddf_7 <- compare_2_7$ddf[2]

trim_model2_7_fit <- trim_model2_7_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "7", dchi = dchi_7, ddf = ddf_7)

# Remove connection ~ commitment
trim_model2_8 <- '
competence + character + caring + contribution + depression ~ attachment
competence + confidence + character + caring + isr ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model2_8_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_8,
  covs
)
trim_model2_8 <- sem(
  trim_model2_8_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_8_sum <- summary(trim_model2_8, fit.measures = T)
models8 <- c("Full" = full_model2, "Trimmed" = trim_model2_8)

compare_2_8 <- compareLavaan(models8, nesting = "Full > Trimmed", scaled = T)

dchi_8 <- str_sub(compare_2_8$dchi[2], end = -2)
ddf_8 <- compare_2_8$ddf[2]

trim_model2_8_fit <- trim_model2_8_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "8", dchi = dchi_8, ddf = ddf_8)

# Remove character ~ attachment
trim_model2_9 <- '
caring + competence + contribution + depression ~ attachment
caring + character + competence + confidence + isr ~ commitment
caring + character + competence + confidence + connection + depression ~ animaluses
'

trim_model2_9_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_9,
  covs
)
trim_model2_9 <- sem(
  trim_model2_9_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_9_fitstats <- extract_model_fit(trim_model2_9)
trim_model2_9_sum <- summary(trim_model2_9, fit.measures = T)

models9 <- c("Full" = full_model2, "Trimmed" = trim_model2_9)

compare_2_9 <- compareLavaan(models9, nesting = "Full > Trimmed", scaled = T)

dchi_9 <- str_sub(compare_2_9$dchi[2], end = -2)
ddf_9 <- compare_2_9$ddf[2]

trim_model2_9_fit <- trim_model2_9_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "9", dchi = dchi_9, ddf = ddf_9)

# Remove contribution ~ attachment
# STOP model
trim_model2_10 <- '
competence + caring + depression ~ attachment
competence + confidence + character + caring + isr ~ commitment
competence + confidence + character + caring + connection + depression ~ animaluses
'

trim_model2_10_syntax <- paste(
  dep_model,
  caring_model,
  comp_model,
  conf_model,
  char_model2b,
  conn_model,
  contr_model,
  attach_model2,
  commit_model2,
  isr_model2,
  animaluse_model2,
  trim_model2_10,
  covs
)
trim_model2_10 <- sem(
  trim_model2_10_syntax,
  data = study2_data,
  missing = "ML",
  estimator = "MLR"
)
trim_model2_10_sum <- summary(trim_model2_10, fit.measures = T)
models10 <- c("Full" = full_model2, "Trimmed" = trim_model2_10)

compare_2_10 <- compareLavaan(models10, nesting = "Full > Trimmed", scaled = T)

dchi_10 <- str_sub(compare_2_10$dchi[2], end = -2)
ddf_10 <- compare_2_10$ddf[2]

trim_model2_10_fit <- trim_model2_10_sum$fit |>
  as.data.frame() |>
  rownames_to_column("fit_measure") |>
  rename(value = 2) |>
  filter(fit_measure %in% fit_measures) |>
  mutate(model = "10", dchi = dchi_10, ddf = ddf_10)

#### ---- Extract Best Model Coefficients -----
attach_regression_trimmed2 <- extract_regression(trim_model2_9, "attachment")
commit_regression_trimmed2 <- extract_regression(trim_model2_9, "commitment")
animaluse_regression_trimmed2 <- extract_regression(trim_model2_9, "animaluses")

#### ---- Combine Summaries ----

options(scipen = 1000000)

wide_fullmodel2_fit <- pivot_wider(
  trim_modelfull2_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = " ", dchi = " ", model = case_when(model == 1 ~ "Full model")) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_1_fit <- pivot_wider(
  trim_model2_1_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_1, dchi = dchi_1) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_2_fit <- pivot_wider(
  trim_model2_2_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_2, dchi = dchi_2) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_3_fit <- pivot_wider(
  trim_model2_3_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_3, dchi = dchi_3) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_4_fit <- pivot_wider(
  trim_model2_4_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_4, dchi = dchi_4) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_5_fit <- pivot_wider(
  trim_model2_5_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_5, dchi = dchi_5) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_6_fit <- pivot_wider(
  trim_model2_6_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_6, dchi = dchi_6) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_7_fit <- pivot_wider(
  trim_model2_7_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_7, dchi = dchi_7) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_8_fit <- pivot_wider(
  trim_model2_8_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_8, dchi = dchi_8) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_9_fit <- pivot_wider(
  trim_model2_9_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_9, dchi = dchi_9) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

wide_trimmodel2_10_fit <- pivot_wider(
  trim_model2_10_fit,
  id_cols = model,
  names_from = fit_measure,
  values_from = value
) |>
  mutate(ddf = ddf_10, dchi = dchi_10) |>
  relocate(ddf, .after = df) |>
  relocate(dchi, .after = ddf)

trim_model2_fits <- rbind(
  wide_fullmodel2_fit,
  wide_trimmodel2_1_fit,
  wide_trimmodel2_2_fit,
  wide_trimmodel2_3_fit,
  wide_trimmodel2_4_fit,
  wide_trimmodel2_5_fit,
  wide_trimmodel2_6_fit,
  wide_trimmodel2_7_fit,
  wide_trimmodel2_8_fit,
  wide_trimmodel2_9_fit,
  wide_trimmodel2_10_fit
) |>
  mutate(
    chisq = format(chisq, digits = 8),
    model = case_when(is.na(model) ~ "Full model", !is.na(model) ~ model)
  ) |>
  relocate(dchi, .before = ddf) |>
  select(-pvalue)


## Group differences -----
group_data2 <- study2_data |>
  select(
    id,
    source,
    member,
    ownership_score = haipet,
    activities_score,
    caring_score:isr_score,
    cesd_score,
    attach_score,
    commit_score,
    animaluse_score
  )

group_member2_long <- group_data2 |>
  pivot_longer(
    cols = activities_score:animaluse_score,
    names_to = "measure",
    values_to = "value"
  )

group_member2_split <- group_member2_long |>
  split(group_member2_long$measure)

group_member_BFttests <- group_member2_split |>
  map(\(x) drop_na(x, value) |> ttestBF(formula = value ~ member, data = _)) |>
  map_df(~ extractBF(.x)$bf) |>
  pivot_longer(everything(), names_to = "measure", values_to = "bf")

group_member_ttests <- group_member2_split |>
  map(\(x) t.test(formula = value ~ member, data = x)) |>
  map_df(function(x) c(x[["statistic"]], x[["p.value"]])) |>
  mutate(stat = c("tstatistic", "pvalue")) |>
  pivot_longer(!stat, names_to = "measure") |>
  pivot_wider(id_cols = measure, names_from = stat, values_from = value) |>
  full_join(group_member_BFttests, by = "measure") |>
  rowwise() |>
  mutate(
    bf10 = cocoon::format_bf(
      bf,
      cutoff = 1000,
      digits1 = 1,
      italics = FALSE,
      subscript = ""
    ),
    p_value = format_p(pvalue, 3, italics = FALSE)
  )

group_member2_longer <- group_member2_long |>
  left_join(group_member_ttests, by = "measure") |>
  mutate(
    measure = fct_relevel(
      measure,
      "activities_score",
      "attach_score",
      "commit_score",
      "animaluse_score",
      "caring_score",
      "character_score",
      "competence_score",
      "confidence_score",
      "connection_score",
      "cesd_score",
      "isr_score"
    ),
    measure = fct_recode(
      measure,
      "HAI activities" = "activities_score",
      "Attachment" = "attach_score",
      "Commitment" = "commit_score",
      "Perception of animal use" = "animaluse_score",
      "Caring" = "caring_score",
      "Character" = "character_score",
      "Competence" = "competence_score",
      "Confidence" = "confidence_score",
      "Connection" = "connection_score",
      "Depression" = "cesd_score",
      "Self-regulation" = "isr_score"
    ),
    member = if_else(member == 1, "Yes", "No")
  )

group_diff_plot2 <- group_member2_longer |>
  mutate(member = fct_relevel(member, "Yes", "No")) |>
  ggplot(aes(x = as.factor(member), y = value)) +
  geom_beeswarm(color = "grey85") +
  stat_summary(fun.data = mean_cl_normal, size = 1.5, shape = "-") +
  facet_wrap(vars(measure), scales = "free_y") +
  geom_text(
    x = 1.5,
    y = Inf,
    label = group_member2_longer$bf10,
    vjust = 11,
    size = 3
  ) +
  geom_text(
    x = 1.5,
    y = Inf,
    label = group_member2_longer$p_value,
    vjust = 13,
    size = 3
  ) +
  labs(x = "4-H/FFA Member", y = "Score") +
  coord_cartesian(xlim = c(1, 2)) +
  theme_bw()

group_diff_plot1 / group_diff_plot2 + plot_annotation(tag_levels = "A")
ggsave("figures/group_diff_measures.png", height = 10.5, width = 9)


# Demographics ----
demographics1 <- hai_data1 |>
  select(source, gender:income, ffa4h_membership, ownership) |>
  mutate(
    rural_urban = fct_recode(
      rural_urban,
      "Urban-Small to Medium City" = "Small city",
      "Urban-Big City" = "Big city"
    ),
    income = fct_relevel(
      income,
      "Less than $10,000",
      "$10,000 - $24,999",
      "$25,000 - $49,999",
      "$50,000 - $74,999",
      "$75,000 - $99,999",
      "$100,000 - $149,999",
      "More than $150,000",
      "I would prefer not to answer"
    )
  )
demographics2 <- hai_data2 |>
  select(source, gender:income, ffa4h_membership, ownership) |>
  mutate(
    income = fct_relevel(
      income,
      "Less than $10,000",
      "$10,000 - $24,999",
      "$25,000 - $49,999",
      "$50,000 - $74,999",
      "$75,000 - $99,999",
      "$100,000 - $149,999",
      "More than $150,000",
      "I would prefer not to answer"
    ),
    ffa4h_membership = sub("4H", "4-H", ffa4h_membership),
    ffa4h_membership = fct_recode(
      ffa4h_membership,
      "4-H only" = "4-H",
      "FFA only" = "FFA"
    ),
    source = sub("_REP", "", source)
  )

num_participants1 <- nrow(demographics1)
num_participants2 <- nrow(demographics2)

(gender1 <- count(demographics1, gender) |>
  mutate(percent = n / sum(n) * 100))

(gender2 <- count(demographics2, gender) |>
  mutate(percent = n / sum(n) * 100))

(latinx1 <- sum(
  demographics1$`race...Latina.o.x.or.Hispanic.or.heritage.from.a.Latin.American.country`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)
(black1 <- sum(demographics1$`race...African.American.Black`, na.rm = TRUE) /
  nrow(demographics1) *
  100)
(native1 <- sum(
  demographics1$`race...Native.American.American.Indian.Indigenous`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)
(middleeastern1 <- sum(
  demographics1$`race...Middle.Eastern.Arab.Turkish.Iranian`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)
(asian1 <- sum(
  demographics1$`race...Asian.Asian.American.Pacific.Islander`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)
(white1 <- sum(demographics1$`race...White.European.American`, na.rm = TRUE) /
  nrow(demographics1) *
  100)
(multiracial1 <- sum(
  demographics1$`race...Biracial.multiracial`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)
(racenoanswer1 <- sum(
  demographics1$`race...I.would.prefer.not.to.answer`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)

(latinx2 <- sum(
  demographics2$`race...Latina.o.x.or.Hispanic.or.heritage.from.a.Latin.American.country`,
  na.rm = TRUE
) /
  nrow(demographics2) *
  100)
(black2 <- sum(demographics2$`race...African.American.Black`, na.rm = TRUE) /
  nrow(demographics2) *
  100)
(native2 <- sum(
  demographics2$`race...Native.American.American.Indian.Indigenous`,
  na.rm = TRUE
) /
  nrow(demographics2) *
  100)
(middleeastern2 <- sum(
  demographics2$`race...Middle.Eastern.Arab.Turkish.Iranian`,
  na.rm = TRUE
) /
  nrow(demographics2) *
  100)
(asian2 <- sum(
  demographics2$`race...Asian.Asian.American.Pacific.Islander`,
  na.rm = TRUE
) /
  nrow(demographics2) *
  100)
(white2 <- sum(demographics2$`race...White.European.American`, na.rm = TRUE) /
  nrow(demographics2) *
  100)
(multiracial2 <- sum(
  demographics2$`race...Biracial.multiracial`,
  na.rm = TRUE
) /
  nrow(demographics2) *
  100)
(racenoanswer2 <- sum(
  demographics2$`race...I.would.prefer.not.to.answer`,
  na.rm = TRUE
) /
  nrow(demographics1) *
  100)

(rural_urban1 <- count(demographics1, rural_urban) |>
  mutate(percent = n / sum(n) * 100))

(rural_urban2 <- count(demographics2, rural_urban) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h1 <- count(demographics1, ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h2 <- count(demographics2, ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h_casnr1 <- filter(demographics1, source == "CASNR") |>
  count(ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h_sona1 <- filter(demographics1, source == "SONA") |>
  count(ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h_casnr2 <- filter(demographics2, grepl("CASNR", source)) |>
  count(ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ffa4h_sona2 <- filter(demographics2, source == "SONA") |>
  count(ffa4h_membership) |>
  mutate(percent = n / sum(n) * 100))

(ownership1 <- count(demographics1, ownership) |>
  mutate(percent = n / sum(n) * 100))

(ownership2 <- count(demographics2, ownership) |>
  mutate(percent = n / sum(n) * 100))

(source1 <- count(demographics1, source) |>
  mutate(percent = n / sum(n) * 100))

(source2 <- count(demographics2, source) |>
  mutate(percent = n / sum(n) * 100))


## Study 1 ----
gender_demographics1 <- select(demographics1, source, gender)
gender_gt1 <- tbl_summary(
  gender_demographics1,
  by = source,
  sort = list(everything() ~ "frequency"),
  label = list(gender ~ "Gender")
) |>
  add_overall(last = TRUE)
race_demographics1 <- select(demographics1, source, contains("race...")) |>
  pivot_longer(
    cols = contains("race..."),
    names_to = "race",
    values_to = "values"
  ) |>
  drop_na(values) |>
  select(-values) |>
  mutate(
    race = sub("race...", "", race),
    race = fct_recode(
      race,
      "Latina/o/x or Hispanic" = "Latina.o.x.or.Hispanic.or.heritage.from.a.Latin.American.country",
      "African American/Black" = "African.American.Black",
      "Native American/American Indian/Indigenous" = "Native.American.American.Indian.Indigenous",
      "Middle Eastern/Arab/Turkish/Iranian" = "Middle.Eastern.Arab.Turkish.Iranian",
      "Asian/Asian American/Pacific Islander" = "Asian.Asian.American.Pacific.Islander",
      "White/European American" = "White.European.American",
      "Biracial/multiracial" = "Biracial.multiracial"
    )
  )
race_gt1 <- tbl_summary(
  race_demographics1,
  by = source,
  sort = list(everything() ~ "frequency"),
  label = list(race ~ "Race")
) |>
  add_overall(last = TRUE)
other_demographics1 <- select(
  demographics1,
  source,
  rural_urban:ffa4h_membership
)
other_gt1 <- tbl_summary(
  other_demographics1,
  by = source,
  sort = list(-income ~ "frequency"),
  label = list(
    rural_urban ~ "Living environment",
    relationship ~ "Relationship status",
    income ~ "Household income",
    ffa4h_membership ~ "Group membership"
  )
) |>
  add_overall(last = TRUE)
(demo_table1 <- tbl_stack(list(gender_gt1, race_gt1, other_gt1), quiet = TRUE))

## Study 2 ----
gender_demographics2 <- select(demographics2, source, gender)
gender_gt2 <- tbl_summary(
  gender_demographics2,
  by = source,
  sort = list(everything() ~ "frequency"),
  label = list(gender ~ "Gender")
) |>
  add_overall(last = TRUE)
race_demographics2 <- select(demographics2, source, contains("race...")) |>
  pivot_longer(
    cols = contains("race..."),
    names_to = "race",
    values_to = "values"
  ) |>
  drop_na(values) |>
  select(-values) |>
  mutate(
    race = sub("race...", "", race),
    race = fct_recode(
      race,
      "Latina/o/x or Hispanic" = "Latina.o.x.or.Hispanic.or.heritage.from.a.Latin.American.country",
      "African American/Black" = "African.American.Black",
      "Native American/American Indian/Indigenous" = "Native.American.American.Indian.Indigenous",
      "Middle Eastern/Arab/Turkish/Iranian" = "Middle.Eastern.Arab.Turkish.Iranian",
      "Asian/Asian American/Pacific Islander" = "Asian.Asian.American.Pacific.Islander",
      "White/European American" = "White.European.American",
      "Biracial/multiracial" = "Biracial.multiracial"
    )
  )
race_gt2 <- tbl_summary(
  race_demographics2,
  by = source,
  sort = list(everything() ~ "frequency"),
  label = list(race ~ "Race")
) |>
  add_overall(last = TRUE)
other_demographics2 <- select(
  demographics2,
  source,
  rural_urban:ffa4h_membership
)
other_gt2 <- tbl_summary(
  other_demographics2,
  by = source,
  sort = list(-income ~ "frequency"),
  label = list(
    rural_urban ~ "Living environment",
    relationship ~ "Relationship status",
    income ~ "Household income",
    ffa4h_membership ~ "Group membership"
  )
) |>
  add_overall(last = TRUE)
(demo_table2 <- tbl_stack(list(gender_gt2, race_gt2, other_gt2), quiet = TRUE))

(demo_table_full <- tbl_merge(
  list(demo_table1, demo_table2),
  tab_spanner = c("**Study 1**", "**Study 2**")
) |>
  modify_header(
    all_stat_cols() ~ "**{level}**,\n  N = {n} ({style_percent(p)}%)"
  ) |>
  modify_footnote(all_stat_cols() ~ NA))

companion_livestock1 <- study1_data |>
  count(member, companion_livestock) |>
  ungroup() |>
  mutate(prop = prop.table(n), .by = member)


# Tables ----
## Demographics table ----
demo_table <- demo_table_full |>
  as_flex_table() |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 9, part = "body") |>
  width(j = 1, width = 1.5) |>
  width(j = 2:8, width = 0.6)

## Loadings tables ----
loadings1_table <- loadings_pyd1 |>
  mutate(scale = str_to_sentence(scale)) |>
  flextable() |>
  set_header_labels(
    scale = "Scale",
    subscale = "Subscale",
    unstd = "Factor Loading",
    unstd_se = "SE",
    std = "Standardized Factor Loading",
    std_se = "SE"
  ) |>
  merge_v(j = ~scale) |>
  valign(j = ~scale, valign = "top") |>
  colformat_double(digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 9, part = "body")

loadings2_table <- loadings_pyd2 |>
  mutate(
    scale = str_to_sentence(scale),
    scale = sub("Isr", "Self-regulation", scale)
  ) |>
  flextable() |>
  set_header_labels(
    scale = "Scale",
    subscale = "Subscale",
    unstd = "Factor Loading",
    unstd_se = "SE",
    std = "Standardized Factor Loading",
    std_se = "SE"
  ) |>
  merge_v(j = ~scale) |>
  valign(j = ~scale, valign = "top") |>
  colformat_double(digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 9, part = "body")


## Measure model fits ----
meas_model_fit1_table <- meas_model_fits1 |>
  flextable() |>
  set_header_labels(
    measure = "Measure",
    cfi = "CFI",
    rmsea = "RMSEA",
    srmr = "SRMR"
  ) |>
  mk_par(
    part = "header",
    j = "chisq",
    value = as_paragraph(as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(part = "header", j = "df", value = as_paragraph(as_i("df"))) |>
  mk_par(
    part = "header",
    j = "pvalue",
    value = as_paragraph(as_i("p"), "-value")
  ) |>
  colformat_double(j = "chisq", digits = 2) |>
  colformat_double(j = c("pvalue", "cfi", "rmsea", "srmr"), digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 9, part = "body")

meas_model_fit2_table <- meas_model_fits2 |>
  mutate(measure = sub("ISR", "Self-regulation", measure)) |>
  flextable() |>
  set_header_labels(
    measure = "Measure",
    cfi = "CFI",
    rmsea = "RMSEA",
    srmr = "SRMR"
  ) |>
  mk_par(
    part = "header",
    j = "chisq",
    value = as_paragraph(as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(part = "header", j = "df", value = as_paragraph(as_i("df"))) |>
  mk_par(
    part = "header",
    j = "pvalue",
    value = as_paragraph(as_i("p"), "-value")
  ) |>
  colformat_double(j = "chisq", digits = 2) |>
  colformat_double(j = c("pvalue", "cfi", "rmsea", "srmr"), digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 9, part = "body")

## Model coefficients ----

model_coefs_hai_table <- select_regressions(haipet_regression1, "ownership1") |>
  full_join(
    select_regressions(haicare_regression1, "care1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haiact_regression1, "activities1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haiint_regression1, "intensity1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haipet_regression2, "ownership2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haicare_regression2, "care2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haiact_regression2, "activities2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(haiint_regression2, "intensity2"),
    by = join_by(predictor)
  ) |>
  mutate(
    predictor = str_to_sentence(predictor),
    predictor = sub("Isr", "Self-regulation", predictor)
  ) |>
  flextable() |>
  set_header_labels(
    predictor = "Measure",
    ownership1 = "Ownership",
    care1 = "Care",
    activities1 = "Presence of activities",
    intensity1 = "Frequency of activities",
    ownership2 = "Ownership",
    care2 = "Care",
    activities2 = "Presence of activities",
    intensity2 = "Frequency of activities"
  ) |>
  add_header_row(
    values = c("", "Study 1", "Study 2"),
    colwidths = c(1, 4, 4)
  ) |>
  rotate(i = 2, j = 2:9, rotation = "btlr", part = "header") |>
  rotate(i = 2, j = 1, rotation = "lrtb", align = "bottom", part = "header") |>
  height(i = 2, height = 1, part = "header") |>
  bg(i = 6, j = c(2:9), bg = "grey90") |>
  width(j = 1, width = 0.8) |>
  align(align = "center", part = "header") |>
  align(align = "center", part = "body") |>
  align(align = "left", part = "footer") |>
  bold(part = "header") |>
  bold(~ grepl("\\*", ownership1), 2) |>
  bold(~ grepl("\\*", care1), 3) |>
  bold(~ grepl("\\*", activities1), 4) |>
  bold(~ grepl("\\*", intensity1), 5) |>
  bold(~ grepl("\\*", ownership2), 6) |>
  bold(~ grepl("\\*", care2), 7) |>
  bold(~ grepl("\\*", activities2), 8) |>
  bold(~ grepl("\\*", intensity2), 9) |>
  add_footer_lines(
    value = as_paragraph(
      "Bold values with * represent significant effects. Grey cells represent significant effects found in Mueller (2014)."
    )
  ) |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 8, part = "body") |>
  fontsize(size = 9, part = "footer") |>
  font(fontname = "Times", part = "all")

model_coefs_cog_table <- select_regressions(
  attach_regression1,
  "attachment1"
) |>
  full_join(
    select_regressions(commit_regression1, "commitment1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(animaluse_regression1, "animaluse1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(attach_regression_trimmed1, "attachment_trimmed1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(commit_regression_trimmed1, "commitment_trimmed1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(animaluse_regression_trimmed1, "animaluse_trimmed1"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(attach_regression2, "attachment2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(commit_regression2, "commitment2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(animaluse_regression2, "animaluse2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(attach_regression_trimmed2, "attachment_trimmed2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(commit_regression_trimmed2, "commitment_trimmed2"),
    by = join_by(predictor)
  ) |>
  full_join(
    select_regressions(animaluse_regression_trimmed2, "animaluse_trimmed2"),
    by = join_by(predictor)
  ) |>
  mutate(
    predictor = str_to_sentence(predictor),
    predictor = sub("Isr", "Self-regulation", predictor)
  ) |>
  flextable() |>
  set_header_labels(
    predictor = "Measure",
    attachment1 = "Attachment",
    commitment1 = "Commitment",
    animaluse1 = "Perception of animal use",
    attachment_trimmed1 = "Attachment",
    commitment_trimmed1 = "Commitment",
    animaluse_trimmed1 = "Perception of animal use",
    attachment2 = "Attachment",
    commitment2 = "Commitment",
    animaluse2 = "Perception of animal use",
    attachment_trimmed2 = "Attachment",
    commitment_trimmed2 = "Commitment",
    animaluse_trimmed2 = "Perception of animal use"
  ) |>
  add_header_row(
    values = c(
      "",
      "Full model",
      "Trimmed model",
      "Full model",
      "Trimmed model"
    ),
    colwidths = c(1, 3, 3, 3, 3)
  ) |>
  add_header_row(
    values = c("", "Study 1", "Study 2"),
    colwidths = c(1, 6, 6)
  ) |>
  rotate(i = 3, j = 2:13, rotation = "btlr", part = "header") |>
  rotate(i = 3, j = 1, rotation = "lrtb", align = "bottom", part = "header") |>
  height(i = 3, height = 1.2, part = "header") |>
  bg(i = 1, j = c(5:6, 11:12), bg = "grey90") |>
  bg(i = 2, j = c(6:7, 12:13), bg = "grey90") |>
  bg(i = 3, j = c(5, 11), bg = "grey90") |>
  bg(i = 5, j = c(5:7, 11:13), bg = "grey90") |>
  bg(i = 6, j = c(6:7, 12:13), bg = "grey90") |>
  bg(i = 7, j = c(6:7, 12:13), bg = "grey90") |>
  width(j = 1, width = 1) |>
  align(align = "center", part = "header") |>
  align(align = "center", part = "body") |>
  align(align = "left", part = "footer") |>
  bold(part = "header") |>
  bold(~ grepl("\\*", attachment1), 2) |>
  bold(~ grepl("\\*", commitment1), 3) |>
  bold(~ grepl("\\*", animaluse1), 4) |>
  bold(~ grepl("\\*", attachment_trimmed1), 5) |>
  bold(~ grepl("\\*", commitment_trimmed1), 6) |>
  bold(~ grepl("\\*", animaluse_trimmed1), 7) |>
  bold(~ grepl("\\*", attachment2), 8) |>
  bold(~ grepl("\\*", commitment2), 9) |>
  bold(~ grepl("\\*", animaluse2), 10) |>
  bold(~ grepl("\\*", attachment_trimmed2), 11) |>
  bold(~ grepl("\\*", commitment_trimmed2), 12) |>
  bold(~ grepl("\\*", animaluse_trimmed2), 13) |>
  add_footer_lines(
    value = as_paragraph(
      "Bold values with * represent significant effects. Grey cells represent significant effects found in Mueller (2014). In all cases overlapping with effects in our studies, the direction of the effects were congruent. "
    )
  ) |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 8, part = "body") |>
  fontsize(size = 9, part = "footer") |>
  font(fontname = "Times", part = "all")


## Latent correlations ----
latent_cor_table <- latent_cor_table_sample1 |>
  full_join(latent_cor_table_sample2, by = "col") |>
  mutate(
    col = str_to_sentence(col),
    col = sub("Animaluses", "Perception of animal use", col)
  ) |>
  flextable() |>
  add_header_row(
    values = c("", "Study 1", "Study 2"),
    colwidths = c(1, 2, 2)
  ) |>
  set_header_labels(
    col = "",
    attachment1 = "Attachment",
    commitment1 = "Commitment",
    attachment2 = "Attachment",
    commitment2 = "Commitment"
  ) |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bold(~ grepl("\\*", attachment1), 2) |>
  bold(~ grepl("\\*", commitment1), 3) |>
  bold(~ grepl("\\*", attachment2), 4) |>
  bold(~ grepl("\\*", commitment2), 5) |>
  add_footer_lines(
    value = as_paragraph("Bold values with * represent significant effects.")
  ) |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 8, part = "body") |>
  fontsize(size = 9, part = "footer")


## Trimmed models ----
trim_model_fits1_table <- trim_model1_fits |>
  mutate(across(!model, as.numeric)) |>
  flextable() |>
  set_header_labels(
    model = "Trimmed Model",
    cfi = "CFI",
    rmsea = "RMSEA",
    srmr = "SRMR"
  ) |>
  mk_par(
    part = "header",
    j = "chisq",
    value = as_paragraph(as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(part = "header", j = "df", value = as_paragraph(as_i("df"))) |>
  mk_par(
    part = "header",
    j = "dchi",
    value = as_paragraph("\u0394", as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(
    part = "header",
    j = "ddf",
    value = as_paragraph("\u0394", as_i("df"))
  ) |>
  colformat_double(j = c("chisq", "dchi"), digits = 2) |>
  colformat_double(j = c("cfi", "rmsea", "srmr"), digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 8, part = "body") |>
  fontsize(size = 9, part = "footer")

trim_model_fits2_table <- trim_model2_fits |>
  mutate(across(!model, as.numeric)) |>
  flextable() |>
  set_header_labels(
    model = "Trimmed Model",
    cfi = "CFI",
    rmsea = "RMSEA",
    srmr = "SRMR"
  ) |>
  mk_par(
    part = "header",
    j = "chisq",
    value = as_paragraph(as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(part = "header", j = "df", value = as_paragraph(as_i("df"))) |>
  mk_par(
    part = "header",
    j = "dchi",
    value = as_paragraph("\u0394", as_i("\u03c7"), as_sup("2"))
  ) |>
  mk_par(
    part = "header",
    j = "ddf",
    value = as_paragraph("\u0394", as_i("df"))
  ) |>
  colformat_double(j = c("chisq", "dchi"), digits = 2) |>
  colformat_double(j = c("cfi", "rmsea", "srmr"), digits = 2) |>
  bold(part = "header") |>
  fontsize(size = 9, part = "header") |>
  fontsize(size = 8, part = "body") |>
  fontsize(size = 9, part = "footer")

save.image("R/pachunka_etal_2024.RData")
