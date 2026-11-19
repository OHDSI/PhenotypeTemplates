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

# Clinical context contract ---------------------------------------------------
#
# A "clinical context" is one row of structured clinical knowledge about a
# phenotype (clinical course, care setting, which evidence is expected, ...).
# It is the input contract between context producers (e.g. a human curator,
# or OHDSI/Pegasus' LLM-based generatePhenotypeMetaData()) and this package.
# The machine-readable form of this contract is
# inst/schema/clinical-context.schema.json; clinicalContextSpecification()
# must stay in sync with it (enforced by tests).

# Each enum maps the canonical (UPPER_SNAKE) context value to the value used
# by phenotypeConfig(). NA means the field is descriptive only.
.clinicalContextEnums <- list(
  clinical_course = c(
    PERSISTENT_STABLE    = "persistent_stable",
    PERSISTENT_RELAPSING = "persistent_recurrent",
    TRANSIENT_RECURRENT  = "transient_recurrent",
    TRANSIENT_SINGLE     = "transient_single"
  ),
  expected_care_setting = c(
    ACUTE_CARE_EXPECTED = "acute_care_expected",
    ACUTE_CARE_COMMON   = "acute_care_common",
    OUTPATIENT_EXPECTED = "outpatient_expected"
  ),
  sex_constraint = c(
    BOTH_SEXES  = "both",
    FEMALE_ONLY = "female",
    MALE_ONLY   = "male"
  ),
  recommended_cohort_exit = c(
    END_OF_CONTINUOUS_OBSERVATION   = "continuous_observation",
    END_OF_ERA_WITH_PERSISTENCE_GAP = "era_persistence",
    FIXED_WINDOW_FROM_INDEX         = "fixed_window"
  ),
  is_pharmacovigilance_outcome = c(YES = NA, NO = NA, CONDITIONAL = NA),
  confidence = c(HIGH = NA, MODERATE = NA, LOW = NA)
)

#' Clinical context specification
#'
#' Describes the fields of a clinical context: one row of structured clinical
#' knowledge about a phenotype that drives template generation. A clinical
#' context can be written by hand or produced by another tool (for example
#' `Pegasus::generatePhenotypeMetaData()`), validated with
#' [validateClinicalContext()] and turned into a template configuration with
#' [phenotypeConfigFromContext()].
#'
#' Field names are matched ignoring case and underscores, so both
#' `clinical_course` and `clinicalCourse` are accepted. Enum values are matched
#' case-insensitively; the `phenotypeConfig()` spelling (for example
#' `"persistent_recurrent"`) is accepted as an alias of the canonical value
#' (`"PERSISTENT_RELAPSING"`). Columns not in the specification are ignored.
#'
#' The same contract is published as a JSON Schema in
#' `system.file("schema", "clinical-context.schema.json", package = "PhenotypeTemplates")`.
#'
#' @return A data frame with one row per field and columns `field`, `type`
#'   (`"string"`, `"enum"`, `"boolean"` or `"integer"`), `required`,
#'   `allowedValues` (`"|"`-separated, enums only) and `description`.
#' @examples
#' clinicalContextSpecification()
#' @export
clinicalContextSpecification <- function() {
  spec <- data.frame(
    field = c(
      "source_phenotype", "source_definition", "condition_name", "clinical_rationale",
      "is_pharmacovigilance_outcome", "clinical_course", "expected_care_setting",
      "sex_constraint", "min_age_years", "max_age_years", "minimum_interepisode_gap_days",
      "recommended_cohort_exit", "recommended_exit_days", "has_discrete_recorded_symptoms",
      "requires_diagnostic_test_or_procedure", "requires_active_treatment_within_30d",
      "expects_condition_specific_followup_or_sequelae_1yr", "confidence"
    ),
    type = c(
      "string", "string", "string", "string",
      "enum", "enum", "enum",
      "enum", "integer", "integer", "integer",
      "enum", "integer", "boolean",
      "boolean", "boolean",
      "boolean", "enum"
    ),
    required = c(
      FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, TRUE,
      FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, TRUE,
      TRUE, TRUE,
      TRUE, FALSE
    ),
    description = c(
      "Name of the phenotype this context was written for (join key to concept sets).",
      "Free-text clinical description the context was derived from.",
      "Display name of the condition; may carry a disambiguating suffix.",
      "Free-text justification for the structured choices below.",
      "Whether the condition is typically a pharmacovigilance outcome. Descriptive only.",
      "Natural history of the condition. Sets the default cohort era and exit logic.",
      "Where the condition is usually diagnosed. Controls the hospitalization variants generated.",
      "Restricts the cohort to one sex. Defaults to BOTH_SEXES.",
      "Minimum age in years at index, if the condition is age restricted.",
      "Maximum age in years at index, if the condition is age restricted.",
      "Gap in days separating distinct episodes. Required when the exit resolves to an era with persistence gap.",
      "Overrides the cohort exit implied by clinical_course.",
      "Days from index to cohort exit. Required when the exit resolves to a fixed window.",
      "Symptoms are discretely recorded in the 30 days before diagnosis.",
      "A diagnostic test or procedure is expected in the 30 days before diagnosis.",
      "Condition-specific treatment is expected within 30 days after diagnosis.",
      "Condition-specific follow-up care or sequelae are expected within 1 year.",
      "Confidence of the author in this context. Descriptive only."
    ),
    stringsAsFactors = FALSE
  )
  spec$allowedValues <- vapply(spec$field, function(f) {
    if (is.null(.clinicalContextEnums[[f]])) NA_character_ else paste(names(.clinicalContextEnums[[f]]), collapse = "|")
  }, character(1), USE.NAMES = FALSE)
  spec[, c("field", "type", "required", "allowedValues", "description")]
}

.normalizeFieldKey <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

.isBlank <- function(x) {
  is.na(x) | trimws(as.character(x)) %in% c("", "NA", "NULL", "null", "Null")
}

.asContextDataFrame <- function(context) {
  if (is.data.frame(context)) {
    return(as.data.frame(context, stringsAsFactors = FALSE))
  }
  if (is.list(context) && !is.null(names(context))) {
    context <- lapply(context, function(v) if (is.null(v)) NA else v)
    return(as.data.frame(context, stringsAsFactors = FALSE))
  }
  stop("`context` must be a data frame or a named list", call. = FALSE)
}

#' Normalize a clinical context to canonical field names and values
#'
#' @return A list with `context` (data frame restricted to specification
#'   fields, canonical names and canonical values; invalid values become NA)
#'   and `problems` (data frame of row, field, value, problem).
#' @noRd
.normalizeClinicalContext <- function(context) {
  context <- .asContextDataFrame(context)
  spec <- clinicalContextSpecification()
  n <- nrow(context)

  problems <- list()
  addProblem <- function(rows, field, values, problem) {
    if (length(rows) == 0) return()
    problems[[length(problems) + 1]] <<- data.frame(
      row = rows, field = field, value = as.character(values), problem = problem,
      stringsAsFactors = FALSE
    )
  }

  inputKeys <- .normalizeFieldKey(names(context))
  out <- data.frame(row.names = seq_len(n))
  for (i in seq_len(nrow(spec))) {
    field <- spec$field[i]
    type <- spec$type[i]
    col <- which(inputKeys == .normalizeFieldKey(field))
    raw <- if (length(col) == 0) rep(NA, n) else context[[col[1]]]
    blank <- .isBlank(raw)

    if (spec$required[i]) {
      addProblem(which(blank), field, raw[blank], "required field is missing")
    }

    value <- switch(type,
      string = ifelse(blank, NA_character_, as.character(raw)),
      enum = {
        lookup <- .clinicalContextEnums[[field]]
        upper <- toupper(trimws(as.character(raw)))
        byAlias <- names(lookup)[match(tolower(trimws(as.character(raw))), lookup)]
        canonical <- ifelse(upper %in% names(lookup), upper, byAlias)
        bad <- !blank & is.na(canonical)
        addProblem(which(bad), field, raw[bad],
                   sprintf("must be one of %s", paste(names(lookup), collapse = ", ")))
        ifelse(blank, NA_character_, canonical)
      },
      boolean = {
        text <- tolower(trimws(as.character(raw)))
        parsed <- ifelse(text %in% c("true", "t", "yes", "y", "1"), TRUE,
                         ifelse(text %in% c("false", "f", "no", "n", "0"), FALSE, NA))
        bad <- !blank & is.na(parsed)
        addProblem(which(bad), field, raw[bad], "must be TRUE or FALSE")
        as.logical(ifelse(blank, NA, parsed))
      },
      integer = {
        parsed <- suppressWarnings(as.numeric(as.character(raw)))
        bad <- !blank & (is.na(parsed) | parsed < 0 | parsed != round(parsed))
        addProblem(which(bad), field, raw[bad], "must be a non-negative whole number")
        parsed[blank | bad] <- NA
        parsed
      }
    )
    out[[field]] <- value
  }

  # Cross-field rules
  ageBad <- which(!is.na(out$min_age_years) & !is.na(out$max_age_years) &
                    out$min_age_years > out$max_age_years)
  addProblem(ageBad, "max_age_years", out$max_age_years[ageBad], "must be >= min_age_years")

  course <- unname(.clinicalContextEnums$clinical_course[out$clinical_course])
  defaultExit <- c(
    persistent_stable = "continuous_observation",
    persistent_recurrent = "era_persistence",
    transient_recurrent = "era_persistence",
    transient_single = "fixed_window"
  )[course]
  explicitExit <- unname(.clinicalContextEnums$recommended_cohort_exit[out$recommended_cohort_exit])
  resolvedExit <- ifelse(is.na(explicitExit), defaultExit, explicitExit)
  needsDays <- which(resolvedExit %in% "fixed_window" & is.na(out$recommended_exit_days))
  addProblem(needsDays, "recommended_exit_days", NA,
             "required when the cohort exit resolves to FIXED_WINDOW_FROM_INDEX")
  needsGap <- which(resolvedExit %in% "era_persistence" & is.na(out$minimum_interepisode_gap_days))
  addProblem(needsGap, "minimum_interepisode_gap_days", NA,
             "required when the cohort exit resolves to END_OF_ERA_WITH_PERSISTENCE_GAP")

  problems <- if (length(problems) == 0) {
    data.frame(row = integer(0), field = character(0), value = character(0),
               problem = character(0), stringsAsFactors = FALSE)
  } else {
    p <- do.call(rbind, problems)
    p[order(p$row, match(p$field, spec$field)), , drop = FALSE]
  }
  rownames(problems) <- NULL
  list(context = out, problems = problems)
}

#' Validate a clinical context
#'
#' Checks one or more clinical contexts against
#' [clinicalContextSpecification()]: required fields, enum values, booleans,
#' non-negative whole numbers, and cross-field rules (age range, and the exit
#' parameters that the resolved cohort exit needs).
#'
#' @param context A data frame (one row per phenotype) or a named list (one
#'   phenotype). Field names may be snake_case or camelCase.
#' @param throw If `TRUE` (default), stop with a readable message when any
#'   problem is found.
#' @return Invisibly, a data frame of problems with columns `row`, `field`,
#'   `value` and `problem`. It has zero rows when the context is valid.
#' @examples
#' context <- list(
#'   condition_name = "Rheumatoid arthritis",
#'   clinical_course = "PERSISTENT_STABLE",
#'   expected_care_setting = "OUTPATIENT_EXPECTED",
#'   has_discrete_recorded_symptoms = TRUE,
#'   requires_diagnostic_test_or_procedure = TRUE,
#'   requires_active_treatment_within_30d = TRUE,
#'   expects_condition_specific_followup_or_sequelae_1yr = TRUE
#' )
#' validateClinicalContext(context)
#' @export
validateClinicalContext <- function(context, throw = TRUE) {
  problems <- .normalizeClinicalContext(context)$problems
  if (throw && nrow(problems) > 0) {
    lines <- sprintf("row %d, %s = '%s': %s", problems$row, problems$field,
                     ifelse(is.na(problems$value), "", problems$value), problems$problem)
    stop("Invalid clinical context:\n", paste0("  - ", lines, collapse = "\n"), call. = FALSE)
  }
  invisible(problems)
}

#' Infer occurrence and limit settings from a clinical course
#'
#' Maps a normalized `clinicalCourse` (see [phenotypeConfig()]) to the
#' occurrence settings used by the templates. `firstOccurrenceOnly` and
#' `primaryCriteriaLimit` are fixed (all events qualify); only the
#' qualifying-event count (`expressionLimit`) depends on the clinical course.
#'
#' @param clinicalCourse One of `"persistent_stable"`, `"persistent_recurrent"`,
#'   `"transient_recurrent"`, `"transient_single"`.
#' @return A list with `firstOccurrenceOnly`, `primaryCriteriaLimit`,
#'   `expressionLimit` and `hospitalVisitOverlapWindow`.
#' @examples
#' inferPhenotypeOccurrenceSettings("transient_recurrent")
#' @export
inferPhenotypeOccurrenceSettings <- function(clinicalCourse) {
  expressionLimit <- switch(
    clinicalCourse,
    persistent_stable     = "First",
    persistent_recurrent  = "First",
    transient_recurrent   = "All",
    transient_single      = "First",
    stop(sprintf("Unknown clinical course: %s", clinicalCourse), call. = FALSE)
  )

  list(
    firstOccurrenceOnly = FALSE,
    primaryCriteriaLimit = "All",
    expressionLimit = expressionLimit,
    hospitalVisitOverlapWindow = 99999
  )
}

#' Create a phenotype config from a clinical context
#'
#' Validates a single clinical context (see [clinicalContextSpecification()])
#' and translates it into the config list expected by
#' [buildPhenotypeTemplates()].
#' Occurrence settings are derived from the clinical course with
#' [inferPhenotypeOccurrenceSettings()].
#'
#' @param context A single-row data frame or a named list. Field names may be
#'   snake_case or camelCase.
#' @return A phenotype config list, see [phenotypeConfig()].
#' @examples
#' config <- phenotypeConfigFromContext(list(
#'   clinical_course = "TRANSIENT_RECURRENT",
#'   expected_care_setting = "ACUTE_CARE_COMMON",
#'   minimum_interepisode_gap_days = 90,
#'   has_discrete_recorded_symptoms = FALSE,
#'   requires_diagnostic_test_or_procedure = TRUE,
#'   requires_active_treatment_within_30d = TRUE,
#'   expects_condition_specific_followup_or_sequelae_1yr = FALSE
#' ))
#' config$clinicalCourse
#' @export
phenotypeConfigFromContext <- function(context) {
  normalized <- .normalizeClinicalContext(context)
  if (nrow(normalized$context) != 1L) {
    stop(sprintf("`context` must describe exactly one phenotype, got %d rows", nrow(normalized$context)),
         call. = FALSE)
  }
  validateClinicalContext(context, throw = TRUE)
  row <- normalized$context
  enums <- .clinicalContextEnums

  clinicalCourse <- unname(enums$clinical_course[row$clinical_course])
  sex <- if (is.na(row$sex_constraint)) "both" else unname(enums$sex_constraint[row$sex_constraint])
  occurrenceSettings <- inferPhenotypeOccurrenceSettings(clinicalCourse)
  naToNull <- function(x) if (is.na(x)) NULL else x

  phenotypeConfig(
    clinicalCourse = clinicalCourse,
    expectedCareSetting = unname(enums$expected_care_setting[row$expected_care_setting]),
    minAge = naToNull(row$min_age_years),
    maxAge = naToNull(row$max_age_years),
    male = sex == "male",
    female = sex == "female",
    minimumInterepisodeDayGap = naToNull(row$minimum_interepisode_gap_days),
    recommendedCohortExit = if (is.na(row$recommended_cohort_exit)) NULL else
      unname(enums$recommended_cohort_exit[row$recommended_cohort_exit]),
    fixedExitDays = naToNull(row$recommended_exit_days),
    hasDescreteRecordedSymptoms = row$has_discrete_recorded_symptoms,
    requiresDiagnosticTestOrProcedure = row$requires_diagnostic_test_or_procedure,
    requiresActiveTreatmentWithin30d = row$requires_active_treatment_within_30d,
    expectsConditionSpecificFollowupOrSequelae1yr = row$expects_condition_specific_followup_or_sequelae_1yr,
    firstOccurrenceOnly = occurrenceSettings$firstOccurrenceOnly,
    primaryCriteriaLimit = occurrenceSettings$primaryCriteriaLimit,
    expressionLimit = occurrenceSettings$expressionLimit,
    hospitalVisitOverlapWindow = occurrenceSettings$hospitalVisitOverlapWindow
  )
}
