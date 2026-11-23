# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of PhenotypeTemplates
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Active comparator new user (ACNU) drug exposure cohorts ----------------------
#
# Builds one new-user cohort per treatment arm for a drug vs drug comparison.
# The arms are independent of any outcome phenotype: the index event is the
# drug exposure, and no outcome, indication or comorbidity concept appears in
# either arm.
#
# Companion of vignette("acnu-drug-exposure-cohorts"). It is not run during
# R CMD check (extras/ is in .Rbuildignore) because it needs a vocabulary and
# a Capr concept set per arm.

library(PhenotypeTemplates)

outputFolder <- file.path(getwd(), "acnu_cohorts")
dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = "localhost/ohdsi",
  user = "postgres",
  password = Sys.getenv("CDM_PASSWORD")
)
vocabDatabaseSchema <- "cdm"

# ---------------------------------------------------------------------------
# 1. The comparison                                                        --
# ---------------------------------------------------------------------------
# One row per treatment arm. Both arms must be given the same clean window and
# the same demographic restrictions; only the drug concept set differs. This
# table is the study-design record for the cohorts produced below.

acnuDesign <- data.frame(
  arm = c("DOAC", "Warfarin"),
  indexConceptSet = c("Apixaban or rivaroxaban", "Warfarin"),
  drugConceptIds = c("43013024, 40241331", "1310149"),
  cleanWindowDays = c(365L, 365L),
  minAgeYears = c(18L, 18L),
  maxAgeYears = c(NA_integer_, NA_integer_),
  clinicalCourse = "persistent_stable",
  stringsAsFactors = FALSE
)

if (length(unique(acnuDesign$cleanWindowDays)) != 1L) {
  stop("The clean window must be the same in both arms of an ACNU comparison.", call. = FALSE)
}
if (length(unique(acnuDesign$minAgeYears)) != 1L ||
    length(unique(acnuDesign$maxAgeYears)) != 1L) {
  stop("Age restrictions must be identical in both arms of an ACNU comparison.", call. = FALSE)
}
utils::write.csv(acnuDesign, file.path(outputFolder, "acnu_design.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. Build the concept sets for each arm                                    --
# ---------------------------------------------------------------------------
# A drug arm is a concept set of drug ingredients with their descendants. The
# template treats it exactly like a disease of interest (bucket I): it is the
# index event.

connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

armConceptSet <- function(conceptIds, name) {
  conceptSet <- Capr::cs(Capr::descendants(as.integer(conceptIds)), name = name)
  Capr::getConceptSetDetails(
    conceptSet,
    connection,
    vocabularyDatabaseSchema = vocabDatabaseSchema
  )
}

# ---------------------------------------------------------------------------
# 3. One config, shared by both arms                                       --
# ---------------------------------------------------------------------------
# The templates generate the L0 (index only) variant when no evidence bucket
# is enabled. That is the only correct setting for an ACNU arm: any evidence
# requirement would differ between the arms or would encode the outcome.

acnuConfig <- phenotypeConfig(
  clinicalCourse = acnuDesign$clinicalCourse[1],
  expectedCareSetting = "outpatient_expected",
  minAge = acnuDesign$minAgeYears[1],
  maxAge = NULL,
  hasDescreteRecordedSymptoms = FALSE,
  requiresDiagnosticTestOrProcedure = FALSE,
  requiresActiveTreatmentWithin30d = FALSE,
  expectsConditionSpecificFollowupOrSequelae1yr = FALSE,
  firstOccurrenceOnly = TRUE,
  primaryCriteriaLimit = "All",
  expressionLimit = "First"
)

# ---------------------------------------------------------------------------
# 4. Build one cohort set per arm                                         --
# ---------------------------------------------------------------------------

armConcepts <- stats::setNames(
  lapply(acnuDesign$drugConceptIds, function(ids) {
    strsplit(ids, ",\\s*")[[1]]
  }),
  acnuDesign$arm
)

armCohorts <- vector("list", nrow(acnuDesign))
startId <- 0L
for (i in seq_len(nrow(acnuDesign))) {
  arm <- acnuDesign$arm[i]
  message(sprintf("[%d/%d] Building new-user arm: %s", i, nrow(acnuDesign), arm))
  # Bucket I is the drug exposure: in an ACNU arm the index event is the
  # treatment, not a diagnosis. Only I is supplied on purpose.
  armCohorts[[i]] <- buildPhenotypeTemplates(
    conceptSets = list(
      I = armConceptSet(armConcepts[[arm]], name = sprintf("%s drugs", arm))
    ),
    phenotypeLabel = sprintf("%s new users", arm),
    config = acnuConfig,
    startCohortId = startId
  )
  startId <- max(armCohorts[[i]]$cohortId)
  armCohorts[[i]]$arm <- arm
}

cohorts <- do.call(rbind, armCohorts)
saveRDS(cohorts, file.path(outputFolder, "acnu_cohorts.rds"))

utils::write.csv(
  cohorts[, setdiff(colnames(cohorts), c("json", "sql"))],
  file.path(outputFolder, "acnu_cohorts.csv"),
  row.names = FALSE
)

if (nrow(cohorts) != nrow(acnuDesign)) {
  warning(
    sprintf(
      "Generated %d cohort(s) for %d arm(s); an arm produced no cohort. ",
      nrow(cohorts), nrow(acnuDesign)
    ),
    call. = FALSE
  )
}

print(cohorts[, intersect(c("cohortId", "cohortName", "phenotype", "arm"), colnames(cohorts))])
message("Wrote ", nrow(cohorts), " new-user arm cohorts to ", outputFolder)
