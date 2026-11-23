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

# Build template cohort definitions from LLM (Phenelope) concept sets ---------
#
# This script turns the concept sets produced by OHDSI/Phenelope into the
# bucket-labelled concept sets that buildPhenotypeTemplates() expects, and
# then builds the template cohort definition set for one phenotype.
#
# It is the runnable companion of
# vignette("phenelope-concept-sets-to-templates"). It needs:
#   * Phenelope, Capr, DatabaseConnector, ellmer (an LLM client) and a
#     database with the OMOP vocabulary tables (CONCEPT, CONCEPT_ANCESTOR,
#     CONCEPT_RELATIONSHIP, ...);
#   * a phenotype clinical context, either one row of
#     inst/examples/clinical_contexts.csv or one produced by
#     Pegasus::generatePhenotypeMetaData().
#
# It is NOT run during R CMD check (extras/ is in .Rbuildignore and the
# LLM calls are not reproducible). Every step writes its output to
# outputFolder so the run can be inspected and resumed.

library(PhenotypeTemplates)

# ---------------------------------------------------------------------------
# 1. Configuration                                                          --
# ---------------------------------------------------------------------------

outputFolder <- file.path(getwd(), "phenotpl_from_phenelope")
dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)

phenotype <- "Gastrointestinal hemorrhage"

# Clinical context. Read it from the shipped examples, or replace with the
# row produced by Pegasus for this phenotype.
contexts <- utils::read.csv(
  system.file("examples", "clinical_contexts.csv", package = "PhenotypeTemplates"),
  stringsAsFactors = FALSE
)
context <- contexts[contexts$condition_name == phenotype, ]
stopifnot(nrow(context) == 1L)
validateClinicalContext(context)

# Vocabulary database. The HADES/Eunomia demo database is enough for a smoke
# test; a full vocabulary is strongly preferable because Phenelope ranks
# candidate concepts with PHOEBE, which needs a populated vocabulary.
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = "localhost/ohdsi",
  user = "postgres",
  password = Sys.getenv("CDM_PASSWORD")
)
vocabDatabaseSchema <- "cdm"

# LLM client (see vignette("CreatingLLMConceptSets", package = "Phenelope")).
llmClient <- ellmer::chat_azure_openai(
  endpoint = Sys.getenv("GENAI_ENDPOINT"),
  api_version = "2023-03-15-preview",
  model = Sys.getenv("GENAI_MODEL", unset = "o3"),
  credentials = function() Sys.getenv("GENAI_API_KEY")
)

# ---------------------------------------------------------------------------
# 2. Define the clinical roles of the concept sets                          --
# ---------------------------------------------------------------------------
# Phenelope returns one concept set per call, with no knowledge of what role
# the set plays in a phenotype. The list below is the human decision that
# turns a set of LLM concept sets into template input: each request is
# labelled with its template bucket (see conceptCategoryMap()).
#
# Only the disease of interest (I) is mandatory. Add or drop roles as the
# clinical context requires; buckets that are not supplied simply do not
# produce their template variants.

phenelopeRequests <- list(
  list(
    bucket = "Disease of interest",
    name = "Gastrointestinal hemorrhage",
    clinicalDefinition = paste(
      "Acute bleeding anywhere in the gastrointestinal tract, presenting as",
      "hematemesis, melena or hematochezia, confirmed or evaluated by endoscopy."
    ),
    seedConceptIds = NULL
  ),
  list(
    bucket = "Clinical presentation",
    name = "Gastrointestinal hemorrhage signs",
    clinicalDefinition = paste(
      "Vomiting of blood, coffee ground emesis, black tarry stool, blood in",
      "stool, or other signs of a gastrointestinal bleed."
    ),
    seedConceptIds = NULL
  ),
  list(
    bucket = "Diagnostic procedure",
    name = "Endoscopy of the gastrointestinal tract",
    clinicalDefinition = paste(
      "Upper or lower gastrointestinal endoscopy or sigmoidoscopy, including",
      "esophagogastroduodenoscopy and colonoscopy."
    ),
    seedConceptIds = NULL
  ),
  list(
    bucket = "Drug",
    name = "Proton pump inhibitors",
    clinicalDefinition = paste(
      "Proton pump inhibitors (omeprazole, pantoprazole, lansoprazole,",
      "esomeprazole) used for acid suppression after a gastrointestinal bleed."
    ),
    seedConceptIds = NULL
  ),
  list(
    bucket = "Alternative diagnosis",
    name = "Peptic ulcer",
    clinicalDefinition = paste(
      "Gastric or duodenal ulcer, with or without bleeding, as an alternative",
      "cause of the presentation."
    ),
    seedConceptIds = NULL
  )
)

# Record the two inputs that make a run reproducible: the clinical context
# and the list of Phenelope requests. Neither can be recovered from the
# concept sets afterwards.
utils::write.csv(context, file.path(outputFolder, "clinical_context.csv"), row.names = FALSE)
saveRDS(phenelopeRequests, file.path(outputFolder, "phenelope_requests.rds"))

# ---------------------------------------------------------------------------
# 3. Run Phenelope per role                                                  --
# ---------------------------------------------------------------------------

conceptSetFiles <- file.path(
  outputFolder,
  sprintf("Phenelope_%s.json", gsub("[^A-Za-z0-9]+", "_", vapply(phenelopeRequests, `[[`, character(1), "name")))
)

for (i in seq_along(phenelopeRequests)) {
  request <- phenelopeRequests[[i]]
  if (file.exists(conceptSetFiles[i])) {
    message("Reusing cached concept set: ", conceptSetFiles[i])
    next
  }
  message(sprintf(
    "[%d/%d] Phenelope: %s -> bucket '%s'",
    i, length(phenelopeRequests), request$name, request$bucket
  ))
  setJson <- Phenelope::createConceptSet(
    name = request$name,
    seedConceptIds = request$seedConceptIds,
    clinicalDefinition = request$clinicalDefinition,
    llmClient = llmClient,
    connectionDetails = connectionDetails,
    vocabDatabaseSchema = vocabDatabaseSchema,
    cacheFolder = file.path(outputFolder, "phenelope_cache", i)
  )
  # createConceptSet() returns a JSON string (Capr::as.json(); jsonlite::toJSON()
  # for the condensed path). Normalise before writing so that the file can be
  # re-read with jsonlite::fromJSON().
  if (!is.character(setJson) || length(setJson) != 1L) {
    stop("Phenelope did not return a single JSON string for '", request$name, "'", call. = FALSE)
  }
  writeLines(setJson, conceptSetFiles[i])
}

# ---------------------------------------------------------------------------
# 4. Pre-flight checks before spending time on template generation         --
# ---------------------------------------------------------------------------
# The most common failure when moving from LLM concept sets to templates is a
# concept set that is empty, or whose domain does not match its role (for
# example a "Drug" bucket that came back as an Observation concept set).
# Phenelope adjudicates against a vocabulary, so check with the same
# vocabulary rather than assuming.

connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

classify <- function(conceptSetJson) {
  expression <- jsonlite::fromJSON(conceptSetJson, simplifyVector = FALSE)
  items <- expression$items
  if (is.null(items) || length(items) == 0L) {
    return(data.frame(n = 0L, domains = NA_character_, stringsAsFactors = FALSE))
  }
  ids <- vapply(items, function(x) as.integer(x$concept$CONCEPT_ID), integer(1))
  ids <- ids[!vapply(items, function(x) isTRUE(x$isExcluded), logical(1))]
  if (length(ids) == 0L) {
    return(data.frame(n = 0L, domains = NA_character_, stringsAsFactors = FALSE))
  }
  domains <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT DISTINCT domain_id FROM @vocab.concept WHERE concept_id IN (@ids);",
    vocab = vocabDatabaseSchema,
    ids = paste(ids, collapse = ", ")
  )$DOMAIN_ID
  data.frame(n = length(ids), domains = paste(sort(domains), collapse = "|"), stringsAsFactors = FALSE)
}

preflight <- do.call(rbind, lapply(seq_along(conceptSetFiles), function(i) {
  cbind(
    data.frame(
      name = phenelopeRequests[[i]]$name,
      bucket = phenelopeRequests[[i]]$bucket,
      stringsAsFactors = FALSE
    ),
    classify(paste(readLines(conceptSetFiles[i], warn = FALSE), collapse = "\n"))
  )
}))

expectedDomains <- c(
  "Disease of interest" = "Condition",
  "Hypernym" = "Condition",
  "Clinical presentation" = "Condition",
  "Diagnostic procedure" = "Procedure",
  "Measurement" = "Measurement",
  "Drug" = "Drug",
  "Treatment procedure" = "Procedure",
  "Progression complication" = "Condition",
  "Alternative diagnosis" = "Condition",
  "Etiology" = "Condition"
)

preflight$expectedDomain <- unname(expectedDomains[preflight$bucket])
preflight$domainOk <- mapply(
  function(domains, expected) {
    !is.na(domains) && grepl(expected, domains, fixed = TRUE)
  },
  preflight$domains, preflight$expectedDomain,
  USE.NAMES = FALSE
)
preflight$empty <- preflight$n == 0L

print(preflight[, c("name", "bucket", "n", "domains", "expectedDomain", "domainOk", "empty")])
write.csv(preflight, file.path(outputFolder, "preflight.csv"), row.names = FALSE)

if (any(preflight$empty) || any(!preflight$domainOk)) {
  stop(
    "Some concept sets are empty or do not match the domain expected for their role. ",
    "See preflight.csv. Fix the Phenelope request (or adjudicate the set in Atlas) and re-run.",
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# 5. Label the sets by bucket and build the templates                       --
# ---------------------------------------------------------------------------
# buildPhenotypeTemplates() accepts concept sets named either by bucket code
# ("I", "T", ...) or by the canonical category ("Disease of interest",
# "Drug", ...); conceptCategoryToBucket() does the matching. Passing the
# category names keeps the script readable and makes the mapping explicit.
#
# Two sets that resolve to the same bucket must be combined first: do that
# here before the call, e.g. with unionConceptsets().

conceptSetsByRole <- stats::setNames(
  lapply(conceptSetFiles, function(f) {
    jsonlite::fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
  }),
  vapply(phenelopeRequests, `[[`, character(1), "bucket")
)

duplicatedBuckets <- names(conceptSetsByRole)[
  duplicated(conceptCategoryToBucket(names(conceptSetsByRole)))
]
if (length(duplicatedBuckets) > 0) {
  stop(
    "More than one concept set maps to the same bucket: ",
    paste(unique(duplicatedBuckets), collapse = ", "),
    ". Union them first (see unionConceptsets()) and pass a single set per bucket.",
    call. = FALSE
  )
}

# Capr ConceptSet objects are the input contract of buildPhenotypeTemplates().
# Rebuild them from the Phenelope JSON so the expression that is templated is
# exactly the expression Phenelope returned, not a re-derived concept list.
asCaprConceptSet <- function(conceptSetExpression, name) {
  items <- conceptSetExpression$items
  keep <- items[!vapply(items, function(x) isTRUE(x$isExcluded), logical(1))]
  ids <- vapply(keep, function(x) as.integer(x$concept$CONCEPT_ID), integer(1))
  descendantFlags <- vapply(keep, function(x) isTRUE(x$includeDescendants), logical(1))
  if (any(descendantFlags != descendantFlags[1])) {
    stop(
      "Mixed includeDescendants within one concept set cannot be rebuilt as a single Capr set. ",
      "Flatten it with unionConceptsets() first.",
      call. = FALSE
    )
  }
  conceptSet <- if (isTRUE(descendantFlags[1])) {
    Capr::cs(Capr::descendants(ids), name = name)
  } else {
    Capr::cs(ids, name = name)
  }
  Capr::getConceptSetDetails(
    conceptSet,
    connection,
    vocabularyDatabaseSchema = vocabDatabaseSchema
  )
}

conceptSets <- lapply(names(conceptSetsByRole), function(bucket) {
  asCaprConceptSet(conceptSetsByRole[[bucket]], name = bucket)
})
names(conceptSets) <- names(conceptSetsByRole)

# `context` is the single-row clinical context data frame read in step 1;
# buildPhenotypeTemplates() converts it with phenotypeConfigFromContext().
cohorts <- buildPhenotypeTemplates(
  conceptSets = conceptSets,
  phenotypeLabel = phenotype,
  config = context
)

utils::write.csv(
  cohorts[, setdiff(colnames(cohorts), c("json", "sql"))],
  file.path(outputFolder, "template_cohorts.csv"),
  row.names = FALSE
)
saveRDS(cohorts, file.path(outputFolder, "template_cohorts.rds"))

print(cohorts[, c("cohortId", "cohortName", "evidenceLevel", "hospitalEntry", "altDxExcluded")])
message("Wrote ", nrow(cohorts), " template cohorts to ", outputFolder)
