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

# Pheval Outcome Phenotype Evaluation Templates --------------------------------
#
# Builds first-occurrence evaluation cohorts by composing clinical evidence
# categories into attrition rules that tune specificity. Based on the Phevaluator
# xSpec pattern: index on the first diagnosis of a disease, then layer on
# supporting evidence from related clinical categories.
#
# Categories:
#   I - disease of interest (index event, Day 0)
#   H - hypernym: broader/less-specific term than I, usable as an alternative
#       index event (useHypernymAsIndex); also feeds the Follow-up Care
#       multi-visit component
#   S - symptoms (-30 to 0 days around index)
#   D - diagnostic tests (-30 to 0 days around index)
#   T - treatments (0 to +30 days after index)
#   C - complications (+1 to +365 days after index)
#   E - etiology / suspected cause (-30 to +30 days around index); only used
#       as a component of Follow-up Care, never a standalone rule
#   A - alternative diagnoses (excluded, -30 to +30 days around index)
#
# Follow-up Care is a composite criterion (withAny of):
#   - Complication (C) in the +1 to +365 day window
#   - Etiology (E) in the -30 to +30 day window
#   - Another code for (I OR H) in the +7 to +365 day window
#
# requiresHospitalization restricts the entry criteria (I, and H when used as
# index) to records with a visit type in the hard-coded ER/Inpatient concept
# set — attached to the index event on Day 0, not a separate inclusion rule.
#
# Each category queries multiple OMOP domains where the concept may be recorded:
#   S, C, E, A -> conditionOccurrence OR observation
#   D          -> measurement OR procedure OR deviceExposure
#   T          -> drugExposure OR procedure OR deviceExposure
#   I, H (entry / follow-up) -> conditionOccurrence OR observation
#
# A NULL concept set means that category is not relevant for this disease
# and is silently omitted from the logic group.
#
# All inclusion criteria have "allow events outside observation period" enabled.
#
# Concept set overlap precedence (highest -> lowest), enforced by
# resolveConceptSetOverlaps():
#
#   1. I  - wins over A, H, E, S, C
#   2. A  - wins over H, E, S, C (hypernyms are broad/general and may
#           legitimately overlap with alternative-diagnosis concepts, so A
#           outranks H)
#   3. H  - wins over E, S, C
#   4. E  - wins over S, C
#   5. S and C - tied, allowed to overlap each other
#
# Precedence is applied to each bucket's resolved code list when a vocabulary
# connection is supplied, so an includeDescendants overlap is detected even
# when the two buckets name different (ancestor/descendant) concepts. Without
# a connection only expression items are compared.
#
# Phenotype config (see buildPhenotypeTemplates()) replaces the old fixed
# V0..V11 lattice with a dynamic set of variants driven by a per-phenotype
# config list:
#   clinicalCourse         - persistent_stable | persistent_recurrent |
#                            transient_recurrent | transient_single; sets
#                            era/exit defaults (see resolvePhenotypeExitStrategy())
#   expectedCareSetting    - acute_care_expected | acute_care_common |
#                            outpatient_expected; controls which
#                            requiresHospitalization variants are emitted (see
#                            resolveHospitalizationVariants())
#   minAge/maxAge/male/female         - demographic entry criteria
#   minimumInterepisodeDayGap         - era collapse gap (days)
#   recommendedCohortExit/fixedExitDays - explicit exit override, see
#                            resolvePhenotypeExitStrategy()
#   hasDescreteRecordedSymptoms                    - gates S (-30..0)
#   requiresDiagnosticTestOrProcedure               - gates D (-30..0)
#   requiresActiveTreatmentWithin30d                - gates T (0..+30)
#   expectsConditionSpecificFollowupOrSequelae1yr   - gates Follow-up Care
#   firstOccurrenceOnly                             - index only first-ever diagnosis
#   primaryCriteriaLimit                            - "First"|"All"|"Last" per person
#   expressionLimit                                 - "First"|"All"|"Last" post-attrition
#   hospitalVisitOverlapWindow                      - ±days around index for hospitalization
#
# Each phenotype call produces a combinatorial set of cohorts crossed on:
#   evidence-strictness ladder (base -> care -> full, skipping duplicate
#     levels when the relevant flags are all FALSE) x index specificity
#     (I vs I|H, only if cs_H supplied) x hospitalization (per
#     expectedCareSetting) x alternative-diagnosis exclusion (with/without,
#     only if cs_A supplied)
#
# Naming convention:
#   [PheTpl] <phenotypeLabel> - <tokens joined by ", ">
#   Tokens are sparse and ordered as evidence, index, setting, and alternative
#   diagnosis. For example: "Evid Sx|Test, Hypernym, Hospital, Excl alt dx".
#   The absence of a token means that axis is not required. A long-form
#   human-readable description (see buildCohortDescription()) is attached
#   separately as the `cohortDescription` attribute/column.
#
# Usage:
#   buildPhenotypeTemplates(
#     conceptSets = list(I = afib_I, H = afib_H, T = afib_T, C = afib_C, E = afib_E),
#     phenotypeLabel = "Atrial Fibrillation",
#     config = list(
#       clinicalCourse = "persistent_stable",
#       expectedCareSetting = "acute_care_common",
#       requiresActiveTreatmentWithin30d = TRUE,
#       expectsConditionSpecificFollowupOrSequelae1yr = TRUE
#     )
#   )

#' @import Capr
NULL

#' Hard-coded ER/Inpatient visit concept set
#'
#' This is not user-supplied; it is a fixed part of the template logic.
#' Concept IDs: 9201 (Inpatient Visit), 9203 (Emergency Room Visit),
#' 262 (ER + Inpatient)
#' @noRd
getERInpatientVisitCs <- function() {
  cs(
    descendants(9201L, 9203L, 262L),
    name = "ER/Inpatient visit types (hard-coded)"
  )
}

#' Build a multi-domain criterion group
#'
#' Given a concept set and a list of domain query constructors, build
#' `withAny(domain1(cs, ...), domain2(cs, ...), ...)` — all share the same
#' window. Returns NULL if cs is NULL.
#'
#' All criteria allow events outside the observation period
#' (`duringInterval(ignoreObservationPeriod = TRUE)`).
#'
#' @param cs A ConceptSet, or NULL.
#' @param domainFns List of domain constructor functions (e.g.
#'   `list(conditionOccurrence, observation)`).
#' @param startWindow An EventWindow from `eventStarts()`.
#' @param endWindow An optional EventWindow from `eventEnds()`.
#' @param minCount Minimum occurrences. Default 1 (`atLeast`). Use 0 for
#'   absence checks.
#' @return A Group (from `withAny`) or NULL.
#' @noRd
makeMultiDomainCriterion <- function(cs, domainFns, startWindow,
                                     endWindow = NULL, minCount = 1L) {
  if (is.null(cs)) return(NULL)

  criteria <- lapply(domainFns, function(fn) {
    if (minCount == 0L) {
      exactly(0L, fn(cs),
              duringInterval(startWindow = startWindow, endWindow = endWindow,
                             ignoreObservationPeriod = TRUE))
    } else {
      atLeast(as.integer(minCount), fn(cs),
              duringInterval(startWindow = startWindow, endWindow = endWindow,
                             ignoreObservationPeriod = TRUE))
    }
  })

  do.call(withAny, criteria)
}

#' Build the Follow-up Care criterion group
#'
#' Follow-up Care = withAny() of:
#'   - Complication (cs_C): condition OR observation, +1 to +365 days
#'   - Etiology (cs_E): condition OR observation, -30 to +30 days (etiology
#'     has no standalone rule — it only appears here)
#'   - Another code for (cs_I OR cs_H): condition OR observation, +7 to +365
#'     days after index
#'
#' @param cs_I The index disease ConceptSet.
#' @param cs_H ConceptSet for hypernyms, or NULL.
#' @param cs_C ConceptSet for complications, or NULL.
#' @param cs_E ConceptSet for etiology, or NULL.
#' @return A Group or NULL.
#' @noRd
makeFollowUpCareCriterion <- function(cs_I, cs_H, cs_C, cs_E) {
  complicationCriterion <- makeMultiDomainCriterion(
    cs_C, list(conditionOccurrence, observation),
    startWindow = eventStarts(1, 365)
  )

  etiologyCriterion <- makeMultiDomainCriterion(
    cs_E, list(conditionOccurrence, observation),
    startWindow = eventStarts(-30, 30)
  )

  secondCodeCs <- Filter(Negate(is.null), list(cs_I, cs_H))
  secondCodeCriterion <- if (length(secondCodeCs) == 1L) {
    makeMultiDomainCriterion(
      secondCodeCs[[1L]], list(conditionOccurrence, observation),
      startWindow = eventStarts(7, 365)
    )
  } else {
    combineCriteria(
      lapply(secondCodeCs, function(cs) {
        makeMultiDomainCriterion(
          cs, list(conditionOccurrence, observation),
          startWindow = eventStarts(7, 365)
        )
      }),
      "any"
    )
  }

  combineCriteria(list(complicationCriterion, etiologyCriterion, secondCodeCriterion), "any")
}

#' Build the A (alternative diagnosis) exclusion criterion
#'
#' Returns a `withAll(exactly(0, conditionOccurrence(cs_A), ...),
#' exactly(0, observation(cs_A), ...))` group — zero records in either domain.
#' Window: -30 to +30 days around index.
#'
#' @param cs_A ConceptSet for alternative diagnoses, or NULL.
#' @return A Group or NULL.
#' @noRd
makeExclusionCriterion <- function(cs_A) {
  if (is.null(cs_A)) return(NULL)

  critCond <- exactly(
    0L, conditionOccurrence(cs_A),
    duringInterval(startWindow = eventStarts(-30, 30), ignoreObservationPeriod = TRUE)
  )
  critObs <- exactly(
    0L, observation(cs_A),
    duringInterval(startWindow = eventStarts(-30, 30), ignoreObservationPeriod = TRUE)
  )

  withAll(critCond, critObs)
}

#' Combine a list of criteria with `withAny` or `withAll`
#'
#' Filters NULLs, then applies the operator. Returns NULL if no non-NULL
#' members remain.
#'
#' @param criteria A list of Group / Criteria objects (may contain NULLs).
#' @param op `"any"` or `"all"`.
#' @return A Group or NULL.
#' @noRd
combineCriteria <- function(criteria, op, atLeastN = NULL) {
  parts <- Filter(Negate(is.null), criteria)
  if (length(parts) == 0L) return(NULL)
  if (length(parts) == 1L) return(parts[[1L]])
  if (identical(op, "any")) return(do.call(withAny, parts))
  if (identical(op, "all")) return(do.call(withAll, parts))
  if (identical(op, "atLeast")) {
    if (is.null(atLeastN)) stop("atLeastN must be provided for op = 'atLeast'", call. = FALSE)
    args <- c(list(as.integer(atLeastN)), parts)
    return(do.call(withAtLeast, args))
  }
  stop(sprintf("Unsupported combine operator: %s", as.character(op)), call. = FALSE)
}

#' Build a human-readable long-form description for a cohort variant
#'
#' Complements the short `cohortName` (kept compact for lists/dropdowns) with
#' a full sentence explaining what the variant requires, for the
#' `cohortDescription` column.
#'
#' @param phenotypeLabel Character. The phenotype label.
#' @param useHypernymAsIndex,hasDescreteRecordedSymptoms,requiresDiagnosticTestOrProcedure,
#'   requiresActiveTreatmentWithin30d,expectsConditionSpecificFollowupOrSequelae1yr,
#'   requiresHospitalization,excludeA Same meaning as in `outcomePhenotypeTpl()`.
#' @param treatmentFollowUpOp `"any"` or `"all"`, as in `outcomePhenotypeTpl()`.
#' @return A single descriptive character string.
#' @noRd
buildCohortDescription <- function(phenotypeLabel,
                                   useHypernymAsIndex,
                                   hasDescreteRecordedSymptoms,
                                   requiresDiagnosticTestOrProcedure,
                                   requiresActiveTreatmentWithin30d,
                                   expectsConditionSpecificFollowupOrSequelae1yr,
                                   treatmentFollowUpOp,
                                   requiresHospitalization,
                                   excludeA) {
  indexPhrase <- if (isTRUE(useHypernymAsIndex)) {
    "Indexes on the disease of interest (I) or its hypernym (H)."
  } else {
    "Indexes on the disease of interest (I) only."
  }

  preParts <- c(
    if (isTRUE(hasDescreteRecordedSymptoms)) "symptom (S)",
    if (isTRUE(requiresDiagnosticTestOrProcedure)) "diagnostic test/procedure (D)"
  )
  postParts <- c(
    if (isTRUE(requiresActiveTreatmentWithin30d)) "active treatment (T)",
    if (isTRUE(expectsConditionSpecificFollowupOrSequelae1yr)) "condition-specific follow-up/sequelae (FU)"
  )

  evidencePhrase <- if (length(preParts) == 0L && length(postParts) == 0L) {
    "Requires no supporting clinical evidence beyond the index diagnosis."
  } else {
    joiner <- if (identical(treatmentFollowUpOp, "all")) "AND" else "OR"
    paste0(
      "Requires supporting evidence of: ",
      paste(c(preParts, postParts), collapse = sprintf(" %s ", joiner)),
      "."
    )
  }

  hospPhrase <- if (isTRUE(requiresHospitalization)) {
    "Requires the index event to occur during an ER/inpatient visit."
  } else {
    "No hospitalization requirement."
  }

  exclPhrase <- if (isTRUE(excludeA)) {
    "Excludes persons with an alternative diagnosis within 30 days of index. (A)"
  } else {
    "No alternative-diagnosis exclusion."
  }

  paste(
    sprintf("%s outcome phenotype template.", phenotypeLabel),
    indexPhrase, evidencePhrase, hospPhrase, exclPhrase
  )
}

#' Warn about oversized concept set inputs
#'
#' Large concept sets (>1000 concepts) bloat the generated cohort JSON/SQL
#' file size and make the resulting cohort definition difficult for a human
#' reviewer to interpret/validate. This only warns — it does not truncate or
#' otherwise modify the concept sets.
#'
#' @param csList A named list of ConceptSet objects (may contain NULLs).
#' @param threshold Concept count above which to warn. Default 1000.
#' @return Invisibly, NULL.
#' @noRd
warnLargeConceptSets <- function(csList, threshold = 1000L) {
  for (label in names(csList)) {
    cs <- csList[[label]]
    if (is.null(cs)) next
    n <- length(cs@Expression)
    if (n > threshold) {
      warning(
        sprintf(
          paste0(
            "Concept set '%s' has %d concepts (> %d). Large concept sets ",
            "inflate the generated cohort json/sql file size and make the ",
            "resulting cohort definition harder for a human reviewer to ",
            "interpret and validate. Consider using includeDescendants on a ",
            "smaller set of ancestor concepts instead of enumerating individual ",
            "concept ids."
          ),
          label, n, threshold
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Build the symptom criterion group (hasDescreteRecordedSymptoms)
#'
#' Symptom (cs_S) evidence in the -30 to 0 day window. Complications are
#' intentionally excluded here -- cs_C is only used in the Follow-up Care
#' criterion.
#'
#' @param cs_S ConceptSet for symptoms, or NULL.
#' @return A Group or NULL.
#' @noRd
makeSymptomCriterion <- function(cs_S) {
  makeMultiDomainCriterion(
    cs_S, list(conditionOccurrence, observation), startWindow = eventStarts(-30, 0)
  )
}

# Concept Set Overlap Resolution -----------------------------------------------
#
# Enforces precedence rules for concept sets that share condition/observation
# concepts. D and T live in different domains (measurement/procedure/device,
# drug/procedure/device) and are not subject to these rules.
#
# Precedence (highest -> lowest):
#   1. I - wins over A, H, E, S, C
#   2. A - wins over H, E, S, C (hypernyms are broad/general and may
#      legitimately overlap with alternative-diagnosis concepts)
#   3. H - wins over E, S, C
#   4. E - wins over S, C
#   5. S and C - allowed to overlap (different time windows)

# Once-per-session notice that precedence ran without a vocabulary.
.unresolvedPrecedenceWarned <- new.env(parent = emptyenv())

.warnUnresolvedPrecedence <- function() {
  if (isTRUE(.unresolvedPrecedenceWarned$done)) return(invisible())
  .unresolvedPrecedenceWarned$done <- TRUE
  warning(
    "Concept set precedence is being resolved from expression items only because no connection ",
    "was supplied. Concepts reached through includeDescendants will not be detected as overlaps ",
    "between buckets. Pass connection and vocabularyDatabaseSchema to buildPhenotypeTemplates() ",
    "to resolve precedence against the vocabulary. This warning is shown once per session.",
    call. = FALSE
  )
}

#' Resolve overlapping concepts across clinical category concept sets
#'
#' Applies precedence rules to remove concepts from lower-priority sets when
#' they also appear in higher-priority sets. Only affects condition/observation
#' domain sets (I, A, H, E, S, C). D and T are passed through unchanged.
#'
#' Precedence is decided on *resolved* code lists, not on expression items. An
#' expression root with `includeDescendants = TRUE` covers all of its
#' descendants, so `I = 192671*` and `S = 434797` (a descendant of 192671)
#' overlap completely even though their roots are different concepts.
#'
#' If `connection` is supplied, every bucket is resolved against the
#' vocabulary once (see `.resolveConceptSetCodesAll()`), and an item is removed
#' only when every code it contributes is also provided by a higher-priority
#' set. An item that is only partly covered is kept, so precedence never
#' deletes codes the higher-priority set does not provide.
#'
#' If `connection` is NULL, only identical concept IDs can be compared, which
#' is correct only when every set is a plain list of codes without
#' descendants. A warning is emitted in that case.
#'
#' @param cs_I ConceptSet for disease of interest. Required.
#' @param cs_H ConceptSet for hypernyms, or NULL.
#' @param cs_S ConceptSet for symptoms, or NULL.
#' @param cs_D ConceptSet for diagnostic tests, or NULL. Passed through unchanged.
#' @param cs_T ConceptSet for treatments, or NULL. Passed through unchanged.
#' @param cs_C ConceptSet for complications, or NULL.
#' @param cs_A ConceptSet for alternative diagnoses, or NULL.
#' @param cs_E ConceptSet for etiology, or NULL.
#' @param warnOnOverlap Logical. If TRUE (default), emit a message listing
#'   removed concepts for traceability.
#' @param connection An open DatabaseConnector connection to a database with
#'   the OMOP vocabulary tables, or NULL.
#' @param vocabularyDatabaseSchema Schema holding the vocabulary tables.
#'   Required when `connection` is not NULL.
#' @param warnUnresolved Logical. If TRUE (default), warn when running without
#'   a connection, since precedence is then applied to expression items only.
#'   [buildPhenotypeTemplates()] passes FALSE because it reports the same
#'   limitation once per call in its own message.
#'
#' @return A named list with cleaned concept sets:
#'   `cs_I`, `cs_H`, `cs_S`, `cs_D`, `cs_T`, `cs_C`, `cs_A`, `cs_E`.
#'   Any set reduced to zero concepts becomes NULL.
#' @noRd
resolveConceptSetOverlaps <- function(cs_I,
                                      cs_H       = NULL,
                                      cs_S       = NULL,
                                      cs_D       = NULL,
                                      cs_T       = NULL,
                                      cs_C       = NULL,
                                      cs_A       = NULL,
                                      cs_E       = NULL,
                                      warnOnOverlap = TRUE,
                                      connection = NULL,
                                      vocabularyDatabaseSchema = NULL,
                                      warnUnresolved = TRUE) {
  if (is.null(cs_I)) stop("cs_I (disease of interest) is required", call. = FALSE)
  if (!is.null(connection) && is.null(vocabularyDatabaseSchema)) {
    stop("vocabularyDatabaseSchema is required when connection is supplied", call. = FALSE)
  }

  conceptSets <- list(I = cs_I, H = cs_H, S = cs_S, C = cs_C, A = cs_A, E = cs_E)
  resolved <- !is.null(connection)

  if (!resolved && warnUnresolved) {
    warning(
      "resolveConceptSetOverlaps() is comparing expression items, not resolved code lists. ",
      "Concepts reached through includeDescendants will not be detected as overlaps. ",
      "Supply connection and vocabularyDatabaseSchema to resolve precedence against the vocabulary.",
      call. = FALSE
    )
  }

  # Resolve every item of every bucket in one round trip. Without a connection,
  # a set's codes are just the concept IDs of its included items and each item
  # contributes only itself.
  if (resolved) {
    resolvedCodes <- .resolveConceptSetCodesAll(
      sets = conceptSets,
      connection = connection,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema
    )
    setCodes <- resolvedCodes$setCodes
    itemCodes <- resolvedCodes$itemCodes
  } else {
    setCodes <- lapply(conceptSets, function(cs) {
      if (is.null(cs)) return(integer(0))
      included <- Filter(function(x) !isTRUE(x@isExcluded), cs@Expression)
      sort(unique(vapply(included, function(x) as.integer(x@Concept@concept_id), integer(1))))
    })
    itemCodes <- list()
    for (setName in names(conceptSets)) {
      if (is.null(conceptSets[[setName]])) next
      items <- conceptSets[[setName]]@Expression
      for (position in seq_along(items)) {
        itemCodes[[paste0(setName, ":", position)]] <- as.integer(items[[position]]@Concept@concept_id)
      }
    }
  }

  # Drop the items of bucket `bucket` that a higher-priority set fully covers.
  # An item is removed only when every code it contributes is also provided by
  # the higher-priority set, so precedence never deletes codes that set lacks.
  removeCoveredItems <- function(cs, bucket, priorityCodes, label) {
    if (is.null(cs) || length(priorityCodes) == 0L) return(cs)

    items <- cs@Expression
    covered <- vapply(seq_along(items), function(i) {
      # Excluded items contribute no codes, so precedence never removes them.
      if (isTRUE(items[[i]]@isExcluded)) return(FALSE)
      codes <- itemCodes[[paste0(bucket, ":", i)]]
      length(codes) > 0L && all(codes %in% priorityCodes)
    }, logical(1))

    if (!any(covered)) return(cs)

    removed <- vapply(items[covered], function(x) as.integer(x@Concept@concept_id), integer(1))
    if (warnOnOverlap) {
      message(sprintf(
        "resolveConceptSetOverlaps: removed %d concept(s) from %s due to precedence: [%s]%s",
        length(removed), label, paste(removed, collapse = ", "),
        if (resolved) " (resolved code lists)" else " (expression items only)"
      ))
    }

    keepIdx <- which(!covered)
    if (length(keepIdx) == 0L) {
      if (warnOnOverlap) message(sprintf("  -> %s is now empty (set to NULL)", label))
      return(NULL)
    }
    cs@Expression <- unname(items[keepIdx])
    cs
  }

  # Rule 1: I wins over everything
  cs_A <- removeCoveredItems(cs_A, "A", setCodes$I, "cs_A (alternative diagnoses)")
  cs_H <- removeCoveredItems(cs_H, "H", setCodes$I, "cs_H (hypernym)")
  cs_E <- removeCoveredItems(cs_E, "E", setCodes$I, "cs_E (etiology)")
  cs_S <- removeCoveredItems(cs_S, "S", setCodes$I, "cs_S (symptoms)")
  cs_C <- removeCoveredItems(cs_C, "C", setCodes$I, "cs_C (complications)")

  # Rule 2: A wins over H, E, S, C
  cs_H <- removeCoveredItems(cs_H, "H", setCodes$A, "cs_H (hypernym, A-precedence)")
  cs_E <- removeCoveredItems(cs_E, "E", setCodes$A, "cs_E (etiology, A-precedence)")
  cs_S <- removeCoveredItems(cs_S, "S", setCodes$A, "cs_S (symptoms, A-precedence)")
  cs_C <- removeCoveredItems(cs_C, "C", setCodes$A, "cs_C (complications, A-precedence)")

  # Rule 3: H wins over E, S, C
  cs_E <- removeCoveredItems(cs_E, "E", setCodes$H, "cs_E (etiology, H-precedence)")
  cs_S <- removeCoveredItems(cs_S, "S", setCodes$H, "cs_S (symptoms, H-precedence)")
  cs_C <- removeCoveredItems(cs_C, "C", setCodes$H, "cs_C (complications, H-precedence)")

  # Rule 4: E wins over S, C
  cs_S <- removeCoveredItems(cs_S, "S", setCodes$E, "cs_S (symptoms, E-precedence)")
  cs_C <- removeCoveredItems(cs_C, "C", setCodes$E, "cs_C (complications, E-precedence)")

  # Rule 5: S and C are allowed to overlap - no action needed

  list(
    cs_I = cs_I,
    cs_H = cs_H,
    cs_S = cs_S,
    cs_D = cs_D,
    cs_T = cs_T,
    cs_C = cs_C,
    cs_A = cs_A,
    cs_E = cs_E
  )
}
# Phenotype-level config resolvers ----------------------------------------------

#' Treat NULL and scalar NA as "value not provided"
#' @param x A value to check.
#' @return TRUE if `x` is NULL or a length-1 NA.
#' @noRd
isMissingValue <- function(x) {
  is.null(x) || (length(x) == 1L && is.na(x))
}

#' Resolve the era-collapse gap and exit strategy for a phenotype
#'
#' `clinicalCourse` sets a default `recommendedCohortExit`/era-gap behavior;
#' an explicit `recommendedCohortExit` and/or `minimumInterepisodeDayGap`
#' always overrides that default.
#'
#' @param clinicalCourse One of `"persistent_stable"`, `"persistent_recurrent"`,
#'   `"transient_recurrent"`, `"transient_single"`.
#' @param recommendedCohortExit Optional override: `"era_persistence"`,
#'   `"continuous_observation"`, or `"fixed_window"`. NULL uses the
#'   clinicalCourse default.
#' @param minimumInterepisodeDayGap Era-collapse gap in days. Required when
#'   the resolved exit type is `"era_persistence"`.
#' @param fixedExitDays Fixed exit window length in days. Required when the
#'   resolved exit type is `"fixed_window"`.
#' @return A list with `eraDays` (integer) and `endStrategy` (a Capr end
#'   strategy object).
#' @noRd
resolvePhenotypeExitStrategy <- function(clinicalCourse,
                                         recommendedCohortExit = NULL,
                                         minimumInterepisodeDayGap = NULL,
                                         fixedExitDays = NULL) {
  # metadata-sourced NA is "not provided", same as NULL
  if (isMissingValue(minimumInterepisodeDayGap)) minimumInterepisodeDayGap <- NULL
  if (isMissingValue(fixedExitDays)) fixedExitDays <- NULL

  clinicalCourse <- match.arg(clinicalCourse, c(
    "persistent_stable", "persistent_recurrent", "transient_recurrent", "transient_single"
  ))

  defaultExit <- switch(clinicalCourse,
    persistent_stable    = "continuous_observation",
    persistent_recurrent = "era_persistence",
    transient_recurrent  = "era_persistence",
    transient_single     = "fixed_window"
  )

  exitType <- if (!is.null(recommendedCohortExit)) {
    match.arg(recommendedCohortExit, c("era_persistence", "continuous_observation", "fixed_window"))
  } else {
    defaultExit
  }

  if (exitType == "fixed_window") {
    if (is.null(fixedExitDays)) {
      stop("fixedExitDays is required when recommendedCohortExit/clinicalCourse resolves to 'fixed_window'", call. = FALSE)
    }
    return(list(eraDays = 0L, endStrategy = fixedExit(index = "endDate", offsetDays = as.integer(fixedExitDays))))
  }

  if (exitType == "era_persistence") {
    if (is.null(minimumInterepisodeDayGap)) {
      stop("minimumInterepisodeDayGap is required when recommendedCohortExit/clinicalCourse resolves to 'era_persistence'", call. = FALSE)
    }
    # persistent courses exit at end of observation; transient courses are a
    # fixed-length event (0 days unless fixedExitDays overrides it)
    isPersistent <- startsWith(clinicalCourse, "persistent")
    endStrategy <- if (isPersistent) {
      observationExit()
    } else {
      fixedExit(index = "endDate", offsetDays = if (is.null(fixedExitDays)) 0L else as.integer(fixedExitDays))
    }
    return(list(eraDays = as.integer(minimumInterepisodeDayGap), endStrategy = endStrategy))
  }

  # continuous_observation
  list(
    eraDays = if (is.null(minimumInterepisodeDayGap)) 0L else as.integer(minimumInterepisodeDayGap),
    endStrategy = observationExit()
  )
}

#' Resolve which requiresHospitalization variants to emit for a care setting
#'
#' @param expectedCareSetting One of `"acute_care_expected"`,
#'   `"acute_care_common"`, `"outpatient_expected"`.
#' @return A logical vector of `requiresHospitalization` values to cross
#'   (length 1 for expected/outpatient, length 2 for common).
#' @noRd
resolveHospitalizationVariants <- function(expectedCareSetting) {
  expectedCareSetting <- match.arg(expectedCareSetting, c(
    "acute_care_expected", "acute_care_common", "outpatient_expected"
  ))
  switch(expectedCareSetting,
    acute_care_expected = TRUE,
    acute_care_common   = c(TRUE, FALSE),
    outpatient_expected  = FALSE
  )
}

#'Create gender criteria for templates
#' @param minAge minumum age to apply at index (NULL default)
#' @param maxAge maximum age to apply at index (NULL default)
#' @param male require male gender record?
#' @param female require female gender record?
#' @noRd
greateDemographicCriteria <- function(minAge = NULL, maxAge = NULL, male = FALSE, female = FALSE) {
  demography <- c() 
  if (!is.null(minAge)) {
    demography <- c(demography, Capr::age(Capr::gte(minAge)))
  }
  
  if (!is.null(maxAge)) {
    demography <- c(demography, Capr::age(Capr::lte(maxAge)))
  }
  
  if (male) {
    demography <- c(demography, Capr::male())
  }
  
  if (female) {
    demography <- c(demography, Capr::female())
  }

  return(demography)  
}

# Main function ----------------------------------------------------------------

#' Build a Phevaluator-style outcome phenotype variant
#'
#' Constructs a first-occurrence evaluation cohort: index on the first
#' diagnosis of the disease (I) — or, when `useHypernymAsIndex` is TRUE,
#' either I or the broader Hypernym (H) — in condition OR observation domain,
#' then layer on supporting evidence gated by the phenotype's clinical-evidence
#' booleans. Usually called once per variant from `buildPhenotypeTemplates()`
#' rather than directly.
#'
#' All inclusion criteria allow events outside the observation period.
#'
#' @param cs_I ConceptSet for the disease of interest (index event). Required.
#' @param cs_H ConceptSet for hypernyms (broader/less-specific alternative to
#'   cs_I), or NULL.
#' @param cs_S ConceptSet for symptoms, or NULL.
#' @param cs_D ConceptSet for diagnostic tests, or NULL.
#' @param cs_T ConceptSet for treatments, or NULL.
#' @param cs_C ConceptSet for complications, or NULL.
#' @param cs_A ConceptSet for alternative diagnoses (excluded), or NULL.
#' @param cs_E ConceptSet for etiology / suspected cause, or NULL. Only used
#'   as a component of Follow-up Care — never a standalone rule.
#' @param phenotypeLabel Character string. The phenotype label (from the Concept
#'   Set Builder). Used in cohort naming. Required.
#' @param templateSuffix Character. Variant suffix for naming (e.g.
#'   `"Evid Test, Hospital"`). Default `"base_case"`.
#' @param useHypernymAsIndex If TRUE, index on cs_I OR cs_H (whichever occurs
#'   first). If FALSE (default), index on cs_I only.
#' @param hasDescreteRecordedSymptoms If TRUE, include a rule requiring
#'   symptom (S, -30 to 0d) evidence. Default FALSE.
#' @param requiresDiagnosticTestOrProcedure If TRUE, include a rule requiring
#'   diagnostic (D) evidence within -30 to 0 days. Default FALSE.
#' @param requiresActiveTreatmentWithin30d If TRUE, include a rule requiring
#'   Treatment (T) evidence within 0 to +30 days. Default FALSE.
#' @param expectsConditionSpecificFollowupOrSequelae1yr If TRUE, include a
#'   rule requiring Follow-up Care evidence (see `makeFollowUpCareCriterion()`).
#'   Default FALSE.
#' @param treatmentFollowUpOp When both `requiresActiveTreatmentWithin30d` and
#'   `expectsConditionSpecificFollowupOrSequelae1yr` are TRUE, how to combine
#'   them: `"any"` (OR) or `"all"` (AND). Default `"any"`.
#' @param evidenceThreshold Optional integer. If set, replaces the individual
#'   evidence rules with one rule requiring at least this many of the
#'   available evidence components (S, D, T, Follow-up Care).
#' @param excludeA If TRUE, add an exclusion rule: zero records for alternative
#'   diagnoses (A) within -30 to +30 days around index. Default FALSE.
#' @param requiresHospitalization Does the entry event itself require
#'   hospitalization (ER or Inpatient)? When TRUE, restricts the entry
#'   criteria (I, and H when used as index) to records with a visit type in
#'   the hard-coded ER/Inpatient concept set — attached to the index event
#'   on Day 0, not a separate inclusion rule. Default FALSE.
#' @param demographicCriteria Optional Capr criteria from
#'   `greateDemographicCriteria()`, added as an inclusion rule.
#' @param hospitalVisitOverlapWindow When `requiresHospitalization` is TRUE,
#'   the ER/inpatient visit must start up to this many days before index and
#'   end up to this many days after it. Default 99999, which only requires the
#'   visit to span the index date.
#' @param firstOccurrenceOnly If TRUE, index only a person's first-ever
#'   I (and/or H) diagnosis. If FALSE, every qualifying diagnosis is a candidate
#'   index event. Default FALSE.
#' @param primaryCriteriaLimit Which qualifying index event(s) to keep per
#'   person: `"All"`, `"First"`, or `"Last"`. Default `"All"`.
#' @param expressionLimit Which qualifying events survive attrition: `"First"`,
#'   `"All"`, or `"Last"`. Default `"First"`.
#' @param eraDays Gap in days below which consecutive episodes are collapsed
#'   into a single era. Default 0 (no collapse). See
#'   `resolvePhenotypeExitStrategy()`.
#' @param endStrategy A Capr end-strategy object (e.g. from
#'   `resolvePhenotypeExitStrategy()`). Defaults to `observationExit()`.
#'
#' @return A Capr Cohort object.
#' @noRd
outcomePhenotypeTpl <- function(cs_I,
                                cs_H           = NULL,
                                cs_S           = NULL,
                                cs_D           = NULL,
                                cs_T           = NULL,
                                cs_C           = NULL,
                                cs_A           = NULL,
                                cs_E           = NULL,
                                phenotypeLabel,
                                templateSuffix = "base_case",
                                useHypernymAsIndex        = FALSE,
                                hasDescreteRecordedSymptoms = FALSE,
                                requiresDiagnosticTestOrProcedure = FALSE,
                                requiresActiveTreatmentWithin30d = FALSE,
                                expectsConditionSpecificFollowupOrSequelae1yr = FALSE,
                                treatmentFollowUpOp        = c("any", "all"),
                                evidenceThreshold = NULL,
                                excludeA                  = FALSE,
                                requiresHospitalization   = FALSE,
                                demographicCriteria = NULL,
                                firstOccurrenceOnly  = FALSE,
                                primaryCriteriaLimit = c("All", "First", "Last"),
                                expressionLimit      = c("First", "All", "Last"),
                                hospitalVisitOverlapWindow = 99999,
                                eraDays        = 0L,
                                endStrategy    = NULL) {
  if (is.null(cs_I)) stop("cs_I (disease of interest) is required", call. = FALSE)
  if (missing(phenotypeLabel) || !nzchar(phenotypeLabel)) {
    stop("phenotypeLabel is required", call. = FALSE)
  }
  treatmentFollowUpOp  <- match.arg(treatmentFollowUpOp)
  primaryCriteriaLimit <- match.arg(primaryCriteriaLimit)
  expressionLimit      <- match.arg(expressionLimit)
  if (is.null(endStrategy)) endStrategy <- observationExit()

  # ---- cohort name & description ----
  cohortName <- sprintf("[PheTpl] %s - %s", phenotypeLabel, templateSuffix)
  cohortDescription <- buildCohortDescription(
    phenotypeLabel = phenotypeLabel,
    useHypernymAsIndex = useHypernymAsIndex,
    hasDescreteRecordedSymptoms = hasDescreteRecordedSymptoms,
    requiresDiagnosticTestOrProcedure = requiresDiagnosticTestOrProcedure,
    requiresActiveTreatmentWithin30d = requiresActiveTreatmentWithin30d,
    expectsConditionSpecificFollowupOrSequelae1yr = expectsConditionSpecificFollowupOrSequelae1yr,
    treatmentFollowUpOp = treatmentFollowUpOp,
    requiresHospitalization = requiresHospitalization,
    excludeA = excludeA
  )

  # ---- entry event (condition OR observation) ----
  # Disease code can exist in either condition or observation domain.
  # useHypernymAsIndex ORs in cs_H as an alternative, less-specific index event.
  # requiresHospitalization is attached here via visitTypeCS so it restricts
  # the index event itself (Day 0), rather than a separate inclusion rule.
  entryAttrs <- list()
  if (isTRUE(firstOccurrenceOnly)) entryAttrs <- c(entryAttrs, list(firstOccurrence()))
  if (!is.null(demographicCriteria)) entryAttrs <- c(entryAttrs, demographicCriteria)

  indexCondition   <- do.call(conditionOccurrence, c(list(cs_I), entryAttrs))
  indexObservation <- do.call(observation, c(list(cs_I), entryAttrs))

  entryCriteria <- list(indexCondition, indexObservation)

  if (isTRUE(useHypernymAsIndex) && !is.null(cs_H)) {
    entryCriteria <- c(entryCriteria, list(
      do.call(conditionOccurrence, c(list(cs_H), entryAttrs)),
      do.call(observation, c(list(cs_H), entryAttrs))
    ))
  }

  entryParams <- list(primaryCriteriaLimit = primaryCriteriaLimit)
  entryDef <- do.call(entry, c(entryCriteria, entryParams))

  # ---- individual evidence components (may be combined or used for threshold) ----
  symptomCriterion <- if (isTRUE(hasDescreteRecordedSymptoms)) {
    makeSymptomCriterion(cs_S)
  } else NULL

  diagnosticCriterion <- if (isTRUE(requiresDiagnosticTestOrProcedure)) {
    makeMultiDomainCriterion(
      cs_D, list(measurement, procedure, deviceExposure), startWindow = eventStarts(-30, 0)
    )
  } else NULL

  tCriterion  <- if (isTRUE(requiresActiveTreatmentWithin30d)) {
    makeMultiDomainCriterion(
      cs_T, list(drugExposure, procedure, deviceExposure),
      startWindow = eventStarts(0, 30)
    )
  } else NULL

  fuCriterion <- if (isTRUE(expectsConditionSpecificFollowupOrSequelae1yr)) {
    makeFollowUpCareCriterion(cs_I, cs_H, cs_C, cs_E)
  } else NULL

  # ---- pre-index evidence (symptom and/or diagnostic) ----
  preGroup <- NULL
  if (isTRUE(hasDescreteRecordedSymptoms) || isTRUE(requiresDiagnosticTestOrProcedure)) {
    preGroup <- combineCriteria(list(symptomCriterion, diagnosticCriterion), "any")
  }

  # ---- post-index evidence (Treatment and/or Follow-up Care) ----
  postGroup <- NULL
  if (isTRUE(requiresActiveTreatmentWithin30d) || isTRUE(expectsConditionSpecificFollowupOrSequelae1yr)) {
    postGroup <- combineCriteria(list(tCriterion, fuCriterion), treatmentFollowUpOp)
  }

  # ---- A exclusion (-30 to +30) ----
  aGroup <- NULL
  if (isTRUE(excludeA)) {
    aGroup <- makeExclusionCriterion(cs_A)
  }

  # ---- assemble attrition rules ----
  ruleList <- list()

  if (isTRUE(requiresHospitalization)) {
    ruleList[["requires hospitalization"]] <- withAll(
      atLeast(
        x = 1, 
        query = visit(getERInpatientVisitCs()), 
        aperture = duringInterval(
          startWindow = eventStarts(-hospitalVisitOverlapWindow, 0, index = "startDate"),
          endWindow   = eventEnds(0, hospitalVisitOverlapWindow, index = "endDate")
        )
      )
    )
  }

  if (!is.null(preGroup)) {
    # If evidenceThreshold is provided we will use the individual components
    # to build a single threshold rule instead of the separate pre/post rules.
    if (is.null(evidenceThreshold)) {
      ruleList[["symptom/diagnostic evidence"]] <- preGroup
    }
  }
  if (!is.null(postGroup)) {
    if (is.null(evidenceThreshold)) {
      ruleList[["treatment and/or follow-up care evidence"]] <- postGroup
    }
  }
  if (!is.null(aGroup)) {
    ruleList[["no alternative diagnosis (A excluded -30 to +30d)"]] <- aGroup
  }

  # If an evidenceThreshold is requested, require atLeast N of the available
  # individual evidence components (symptomComplication, diagnostic, T, FU).
  if (!is.null(evidenceThreshold)) {
    evidenceParts <- Filter(Negate(is.null), list(
      symptomCriterion, diagnosticCriterion, tCriterion, fuCriterion
    ))
    if (length(evidenceParts) == 0L) {
      stop("evidenceThreshold provided but no evidence components available for this phenotype", call. = FALSE)
    }
    ruleList[[sprintf("evidence at least %d of 4", as.integer(evidenceThreshold))]] <-
      do.call(withAtLeast, c(list(as.integer(evidenceThreshold)), evidenceParts))
  }

  # ---- build cohort ----
  cohortObj <- cohort(
    entry = entryDef,
    attrition = do.call(attrition, c(ruleList, list(expressionLimit = expressionLimit))),
    exit = exit(endStrategy = endStrategy),
    era = era(eraDays = eraDays)
  )

  # Attach cohort name/description as attributes for downstream use
  attr(cohortObj, "cohortName") <- cohortName
  attr(cohortObj, "cohortDescription") <- cohortDescription

  cohortObj
}

# Phenotype config & batch builder -----------------------------------------------

#' Treat a data frame as a clinical context; anything else as a config list
#' @noRd
.asPhenotypeConfig <- function(config) {
  if (is.data.frame(config)) phenotypeConfigFromContext(config) else config
}

#' Validate and apply defaults to a phenotype config list
#'
#' @param config A list with the phenotype config fields (see
#'   `buildPhenotypeTemplates()`).
#' @return The validated config list with `clinicalCourse`/`expectedCareSetting`
#'   match.arg'd and boolean flags coerced/defaulted to FALSE.
#' @noRd
validatePhenotypeConfig <- function(config) {
  if (is.null(config$clinicalCourse)) {
    stop("config$clinicalCourse is required", call. = FALSE)
  }
  if (is.null(config$expectedCareSetting)) {
    stop("config$expectedCareSetting is required", call. = FALSE)
  }

  if (is.null(config$minimumInterepisodeDayGap) || is.na(config$minimumInterepisodeDayGap)) {
    config$minimumInterepisodeDayGap <- 0
  }

  # metadata-sourced NA is "not provided", same as NULL
  naNullFields <- c("minAge", "maxAge", "fixedExitDays")
  for (f in naNullFields) {
    if (isMissingValue(config[[f]])) config[[f]] <- NULL
  }

  config$clinicalCourse <- match.arg(config$clinicalCourse, c(
    "persistent_stable", "persistent_recurrent", "transient_recurrent", "transient_single"
  ))
  config$expectedCareSetting <- match.arg(config$expectedCareSetting, c(
    "acute_care_expected", "acute_care_common", "outpatient_expected"
  ))

  boolFields <- c(
    "hasDescreteRecordedSymptoms", "requiresDiagnosticTestOrProcedure",
    "requiresActiveTreatmentWithin30d", "expectsConditionSpecificFollowupOrSequelae1yr",
    "male", "female"
  )
  for (f in boolFields) {
    config[[f]] <- isTRUE(config[[f]])
  }

  config$firstOccurrenceOnly <- if (is.null(config$firstOccurrenceOnly)) {
    TRUE
  } else {
    isTRUE(config$firstOccurrenceOnly)
  }

  config$primaryCriteriaLimit <- match.arg(
    if (is.null(config$primaryCriteriaLimit)) "First" else config$primaryCriteriaLimit,
    c("First", "All", "Last")
  )
  config$expressionLimit <- match.arg(
    if (is.null(config$expressionLimit)) "First" else config$expressionLimit,
    c("First", "All", "Last")
  )
  if (is.null(config$hospitalVisitOverlapWindow) || is.na(config$hospitalVisitOverlapWindow)) {
    config$hospitalVisitOverlapWindow <- 99999
  }

  config
}

#' Create a phenotype config list for use with buildPhenotypeTemplates()
#'
#' Initializes a config list with all phenotype parameters, using sensible
#' defaults for omitted boolean flags (defaulting to FALSE).
#'
#' @param clinicalCourse One of `"persistent_stable"`, `"persistent_recurrent"`,
#'   `"transient_recurrent"`, `"transient_single"`. Required.
#' @param expectedCareSetting One of `"acute_care_expected"`, `"acute_care_common"`,
#'   `"outpatient_expected"`. Required.
#' @param minAge Minimum age at index, or NULL. Default NULL.
#' @param maxAge Maximum age at index, or NULL. Default NULL.
#' @param male Require male gender? Default FALSE.
#' @param female Require female gender? Default FALSE.
#' @param minimumInterepisodeDayGap Era-collapse gap. Required for era_persistence
#'   exit types. Default NULL.
#' @param recommendedCohortExit Explicit exit override, or NULL to use the
#'   clinicalCourse default. Default NULL.
#' @param fixedExitDays Fixed exit window length (days). Required for fixed_window
#'   exit types. Default NULL.
#' @param hasDescreteRecordedSymptoms Gate symptom/complication evidence? Default FALSE.
#' @param requiresDiagnosticTestOrProcedure Gate diagnostic evidence? Default FALSE.
#' @param requiresActiveTreatmentWithin30d Gate treatment evidence? Default FALSE.
#' @param expectsConditionSpecificFollowupOrSequelae1yr Gate follow-up care? Default FALSE.
#' @param firstOccurrenceOnly Index only a person's first-ever diagnosis? Default FALSE.
#' @param primaryCriteriaLimit Which qualifying index event(s) to keep per person:
#'   `"All"`, `"First"`, or `"Last"`. Default `"All"`.
#' @param expressionLimit Which qualifying events survive attrition: `"First"`,
#'   `"All"`, or `"Last"`. Default `"First"`.
#' @param hospitalVisitOverlapWindow Days before/after index to look for an
#'   ER/Inpatient visit when hospitalization is required. Default 99999.
#' @return A named list suitable for passing as `config` to buildPhenotypeTemplates().
#' @export
phenotypeConfig <- function(
  clinicalCourse,
  expectedCareSetting,
  minAge = NULL,
  maxAge = NULL,
  male = FALSE,
  female = FALSE,
  minimumInterepisodeDayGap = NULL,
  recommendedCohortExit = NULL,
  fixedExitDays = NULL,
  hasDescreteRecordedSymptoms = FALSE,
  requiresDiagnosticTestOrProcedure = FALSE,
  requiresActiveTreatmentWithin30d = FALSE,
  expectsConditionSpecificFollowupOrSequelae1yr = FALSE,
  firstOccurrenceOnly = FALSE,
  primaryCriteriaLimit = c("All", "First", "Last"),
  expressionLimit = c("First", "All", "Last"),
  hospitalVisitOverlapWindow = 99999
) {
  primaryCriteriaLimit <- match.arg(primaryCriteriaLimit)
  expressionLimit      <- match.arg(expressionLimit)

  list(
    clinicalCourse = clinicalCourse,
    expectedCareSetting = expectedCareSetting,
    minAge = minAge,
    maxAge = maxAge,
    male = male,
    female = female,
    minimumInterepisodeDayGap = minimumInterepisodeDayGap,
    recommendedCohortExit = recommendedCohortExit,
    fixedExitDays = fixedExitDays,
    hasDescreteRecordedSymptoms = hasDescreteRecordedSymptoms,
    requiresDiagnosticTestOrProcedure = requiresDiagnosticTestOrProcedure,
    requiresActiveTreatmentWithin30d = requiresActiveTreatmentWithin30d,
    expectsConditionSpecificFollowupOrSequelae1yr = expectsConditionSpecificFollowupOrSequelae1yr,
    firstOccurrenceOnly = firstOccurrenceOnly,
    primaryCriteriaLimit = primaryCriteriaLimit,
    expressionLimit = expressionLimit,
    hospitalVisitOverlapWindow = hospitalVisitOverlapWindow
  )
}

#' Build the deduped evidence-strictness ladder for a phenotype config
#'
#' L0 (index only) is always included. L1 (Treatment | Follow-up Care, "any")
#' is included only if at least one of those two flags is TRUE. L2 adds a
#' second, separate attrition rule for Symptom/Diagnostic ("any") on top of
#' L1's Treatment/Follow-up Care rule — the two rules are ANDed together by
#' being separate attrition rules, so Treatment/Follow-up Care stays "any"
#' (OR) at L2 too; it is included only if at least one of the pre-index flags
#' is TRUE (otherwise it would be identical to L1).
#'
#' @param config Validated phenotype config list.
#' @return A named list of ladder levels, each a list of the 4 evidence
#'   booleans and `treatmentFollowUpOp` to pass to `outcomePhenotypeTpl()`.
#' @noRd
buildEvidenceLadder <- function(config) {
  careEnabled <- config$requiresActiveTreatmentWithin30d ||
    config$expectsConditionSpecificFollowupOrSequelae1yr
  preEnabled <- config$hasDescreteRecordedSymptoms ||
    config$requiresDiagnosticTestOrProcedure

  ladder <- list(
    L0 = list(
      hasDescreteRecordedSymptoms = FALSE, requiresDiagnosticTestOrProcedure = FALSE,
      requiresActiveTreatmentWithin30d = FALSE, expectsConditionSpecificFollowupOrSequelae1yr = FALSE,
      treatmentFollowUpOp = "any"
    )
  )

  if (careEnabled) {
    ladder$L1 <- list(
      hasDescreteRecordedSymptoms = FALSE, requiresDiagnosticTestOrProcedure = FALSE,
      requiresActiveTreatmentWithin30d = config$requiresActiveTreatmentWithin30d,
      expectsConditionSpecificFollowupOrSequelae1yr = config$expectsConditionSpecificFollowupOrSequelae1yr,
      treatmentFollowUpOp = "any"
    )
  }

  if (preEnabled) {
    ladder$L2 <- list(
      hasDescreteRecordedSymptoms = config$hasDescreteRecordedSymptoms,
      requiresDiagnosticTestOrProcedure = config$requiresDiagnosticTestOrProcedure,
      requiresActiveTreatmentWithin30d = config$requiresActiveTreatmentWithin30d,
      expectsConditionSpecificFollowupOrSequelae1yr = config$expectsConditionSpecificFollowupOrSequelae1yr,
      treatmentFollowUpOp = "any"
    )
  }

  ladder
}

#' Build a sparse, human-readable template name suffix
#'
#' @param level Evidence ladder level, including its component flags.
#' @param useH Whether the hypernym is used as an index.
#' @param hosp Whether hospitalization is required.
#' @param excl Whether alternative diagnoses are excluded.
#' @return A character suffix containing only active axes.
#' @noRd
buildSparseTemplateSuffix <- function(level, useH, hosp, excl) {
  evidenceComponents <- c(
    Sx = level$hasDescreteRecordedSymptoms,
    Test = level$requiresDiagnosticTestOrProcedure,
    Tx = level$requiresActiveTreatmentWithin30d,
    FU = level$expectsConditionSpecificFollowupOrSequelae1yr
  )
  tokens <- character()
  if (any(evidenceComponents)) {
    tokens <- c(tokens, paste0("Evid ", paste(names(evidenceComponents)[evidenceComponents], collapse = "|")))
  }
  if (isTRUE(useH)) tokens <- c(tokens, "Hypernym")
  if (isTRUE(hosp)) tokens <- c(tokens, "Hospital")
  if (isTRUE(excl)) tokens <- c(tokens, "Excl alt dx")
  if (length(tokens) == 0) "Base" else paste(tokens, collapse = ", ")
}

#' Build a cohort definition set of phenotype template variants
#'
#' Replaces the old fixed V0-V11 lattice: generates a combinatorial set of
#' cohort definitions for one phenotype, crossed on evidence-strictness
#' ladder level x index specificity (I vs I|H) x hospitalization (per
#' `config$expectedCareSetting`) x alternative-diagnosis exclusion
#' (with/without, only if cs_A supplied). See header comment for the full
#' config field reference.
#'
#' @param cs_I,cs_H,cs_S,cs_D,cs_T,cs_C,cs_A,cs_E Deprecated: use
#'   `conceptSets`. Capr ConceptSets for each bucket, or NULL.
#' @param phenotypeLabel Character. The phenotype label, used in cohort names.
#' @param config Either a phenotype config list (see [phenotypeConfig()]) or
#'   a single-row clinical context data frame (see
#'   [clinicalContextSpecification()]), which is converted with
#'   [phenotypeConfigFromContext()]. A named list is always treated as a
#'   config.
#' @param startCohortId cohortId to begin templates (use if you're building multiple template sets). First output id will be param + 1.
#' @param conceptSets A named list of Capr ConceptSets, named by bucket code
#'   (`I`, `H`, `S`, `D`, `T`, `C`, `A`, `E`) or by category (see
#'   [conceptCategoryMap()]). `I` is required; omitted buckets are not used.
#' @param connection Optional open DatabaseConnector connection to a database
#'   with the OMOP vocabulary tables. When supplied, concept set precedence is
#'   applied to resolved code lists, so `includeDescendants` overlaps between
#'   buckets are detected. When NULL, precedence falls back to comparing
#'   expression items only and a warning is emitted.
#' @param vocabularyDatabaseSchema Schema holding the vocabulary tables.
#'   Required when `connection` is supplied.
#' @return A cohort definition set data frame with cohort identifiers, names,
#'   descriptions, axis metadata (`evidenceLevel`, `evidenceComponents`,
#'   `hypernymIndex`, `hospitalEntry`, `altDxExcluded`), and json/sql
#'   definitions. Use the axis columns, not `cohortName`, to identify variants
#'   programmatically.
#' @export
buildPhenotypeTemplates <- function(cs_I = NULL,
                                    cs_H = NULL,
                                    cs_S = NULL,
                                    cs_D = NULL,
                                    cs_T = NULL,
                                    cs_C = NULL,
                                    cs_A = NULL,
                                    cs_E = NULL,
                                    phenotypeLabel,
                                    config,
                                    startCohortId = 0,
                                    conceptSets = NULL,
                                    connection = NULL,
                                    vocabularyDatabaseSchema = NULL) {
  if (missing(phenotypeLabel) || !nzchar(phenotypeLabel)) {
    stop("phenotypeLabel is required", call. = FALSE)
  }
  cs <- .resolveConceptSetArgs(conceptSets, list(
    cs_I = cs_I, cs_H = cs_H, cs_S = cs_S, cs_D = cs_D,
    cs_T = cs_T, cs_C = cs_C, cs_A = cs_A, cs_E = cs_E
  ))
  if (is.null(cs$cs_I)) {
    stop("A disease of interest (I) concept set is required", call. = FALSE)
  }
  cs_I <- cs$cs_I
  cs_H <- cs$cs_H
  cs_S <- cs$cs_S
  cs_D <- cs$cs_D
  cs_T <- cs$cs_T
  cs_C <- cs$cs_C
  cs_A <- cs$cs_A
  cs_E <- cs$cs_E
  config <- validatePhenotypeConfig(.asPhenotypeConfig(config))

  # ---- resolve concept set overlaps ----
  if (is.null(connection)) .warnUnresolvedPrecedence()
  resolved <- resolveConceptSetOverlaps(
    cs_I, cs_H, cs_S, cs_D, cs_T, cs_C, cs_A, cs_E,
    connection = connection,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    warnUnresolved = FALSE
  )
  cs_H <- resolved$cs_H
  cs_S <- resolved$cs_S
  cs_D <- resolved$cs_D
  cs_T <- resolved$cs_T
  cs_C <- resolved$cs_C
  cs_A <- resolved$cs_A
  cs_E <- resolved$cs_E

  warnLargeConceptSets(list(I = cs_I, H = cs_H, S = cs_S, D = cs_D, T = cs_T, C = cs_C, A = cs_A, E = cs_E))

  # ---- shared era/exit + demographics ----
  exitConfig <- resolvePhenotypeExitStrategy(
    clinicalCourse = config$clinicalCourse,
    recommendedCohortExit = config$recommendedCohortExit,
    minimumInterepisodeDayGap = config$minimumInterepisodeDayGap,
    fixedExitDays = config$fixedExitDays
  )
  demographicCriteria <- greateDemographicCriteria(
    minAge = config$minAge, maxAge = config$maxAge,
    male = config$male, female = config$female
  )

  # ---- axes ----
  ladder <- buildEvidenceLadder(config)
  indexOptions <- if (!is.null(cs_H)) c(FALSE, TRUE) else FALSE
  hospOptions  <- resolveHospitalizationVariants(config$expectedCareSetting)
  aOptions     <- if (!is.null(cs_A)) c(FALSE, TRUE) else FALSE

  # ---- build one Capr cohort per combination ----
  templates <- list()
  templateMetadata <- list()
  for (levelName in names(ladder)) {
    level <- ladder[[levelName]]
    for (useH in indexOptions) {
      for (hosp in hospOptions) {
        for (excl in aOptions) {
          suffix <- buildSparseTemplateSuffix(level, useH, hosp, excl)
          if (!is.null(templates[[suffix]])) {
            stop("Template suffixes must be unique within a phenotype", call. = FALSE)
          }
          templateMetadata[[suffix]] <- list(
            evidenceLevel = levelName,
            evidenceComponents = names(c(
              Sx = level$hasDescreteRecordedSymptoms,
              Test = level$requiresDiagnosticTestOrProcedure,
              Tx = level$requiresActiveTreatmentWithin30d,
              FU = level$expectsConditionSpecificFollowupOrSequelae1yr
            ))[c(
              level$hasDescreteRecordedSymptoms,
              level$requiresDiagnosticTestOrProcedure,
              level$requiresActiveTreatmentWithin30d,
              level$expectsConditionSpecificFollowupOrSequelae1yr
            )],
            hypernymIndex = isTRUE(useH),
            hospitalEntry = isTRUE(hosp),
            altDxExcluded = isTRUE(excl)
          )
          templates[[suffix]] <- outcomePhenotypeTpl(
            cs_I, cs_H, cs_S, cs_D, cs_T, cs_C, cs_A, cs_E,
            phenotypeLabel = phenotypeLabel,
            templateSuffix = suffix,
            useHypernymAsIndex = useH,
            hasDescreteRecordedSymptoms = level$hasDescreteRecordedSymptoms,
            requiresDiagnosticTestOrProcedure = level$requiresDiagnosticTestOrProcedure,
            requiresActiveTreatmentWithin30d = level$requiresActiveTreatmentWithin30d,
            expectsConditionSpecificFollowupOrSequelae1yr = level$expectsConditionSpecificFollowupOrSequelae1yr,
            treatmentFollowUpOp = level$treatmentFollowUpOp,
            excludeA = excl,
            requiresHospitalization = hosp,
            demographicCriteria = demographicCriteria,
            firstOccurrenceOnly = config$firstOccurrenceOnly,
            primaryCriteriaLimit = config$primaryCriteriaLimit,
            expressionLimit = config$expressionLimit,
            hospitalVisitOverlapWindow = config$hospitalVisitOverlapWindow,
            eraDays = exitConfig$eraDays,
            endStrategy = exitConfig$endStrategy
          )
        }
      }
    }
  }

  templateNames <- vapply(templates, attr, character(1), which = "cohortName")
  if (anyDuplicated(templateNames)) {
    stop("Template cohort names must be unique within a phenotype", call. = FALSE)
  }

  message("Template cohorts complete")
  message("Constructing cohort definition set")
  cohortId <- startCohortId
  cohortsToCreate <- data.frame()
  lapply(names(templates), function(templateName) {
    tpl <- templates[[templateName]]
    metadata <- templateMetadata[[templateName]]
    tryCatch(
      {
        cohortId <<- cohortId + 1
        message(attr(tpl, "cohortName"))
        cohortsToCreate <<- cohortsToCreate |>
          rbind(
            data.frame(
              cohortId   = cohortId,
              cohortName = attr(tpl, "cohortName"),
              cohortDescription = attr(tpl, "cohortDescription"),
              evidenceLevel = metadata$evidenceLevel,
              evidenceComponents = paste(metadata$evidenceComponents, collapse = "|"),
              hypernymIndex = metadata$hypernymIndex,
              hospitalEntry = metadata$hospitalEntry,
              altDxExcluded = metadata$altDxExcluded,
              phenotype = phenotypeLabel,
              json = Capr::toCohortJson(tpl),
              sql = CirceR::buildCohortQuery(expression = Capr::toCohortJson(tpl), options = CirceR::createGenerateOptions(generateStats = FALSE))
            )
          )
      },
      error = function(e) {
        warning(sprintf("Cohort %s (%s) failed to generate: %s",
                        cohortId, attr(tpl, "cohortName"), conditionMessage(e)), call. = FALSE)
      }
    )
  })

  return(cohortsToCreate)
}
