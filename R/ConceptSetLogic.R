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

#' Concept set category vocabulary
#'
#' The canonical mapping from concept set categories (as produced by concept
#' set generators such as OHDSI/Pegasus) to the template buckets used by
#' [buildPhenotypeTemplates()]:
#'
#' * `I` disease of interest (index event)
#' * `H` hypernym (broader alternative index)
#' * `S` clinical presentation / symptoms (-30 to 0 days)
#' * `D` diagnostic procedures and measurements (-30 to 0 days)
#' * `T` drugs and treatment procedures (0 to +30 days)
#' * `C` progression and complications (+1 to +365 days)
#' * `A` alternative diagnoses (excluded, -30 to +30 days)
#' * `E` etiology (-30 to +30 days, follow-up care only)
#'
#' Categories mapped to `NA` (risk factors) are not used by the templates.
#'
#' @return A named character vector: names are canonical category labels,
#'   values are bucket codes.
#' @seealso [conceptCategoryToBucket()]
#' @examples
#' conceptCategoryMap()
#' @export
conceptCategoryMap <- function() {
  c(
    "Disease of interest"      = "I",
    "Hypernym"                 = "H",
    "Clinical presentation"    = "S",
    "Diagnostic procedure"     = "D",
    "Measurement"              = "D",
    "Drug"                     = "T",
    "Treatment procedure"      = "T",
    "Progression complication" = "C",
    "Alternative diagnosis"    = "A",
    "Etiology"                 = "E",
    "Risk factor"              = NA_character_
  )
}

#' Map concept set categories to template buckets
#'
#' Matching ignores case, spaces, underscores and hyphens, so
#' `"Disease of interest"`, `"disease_of_interest"` and `"DISEASE-OF-INTEREST"`
#' are equivalent. Bucket codes (`"I"`, `"H"`, ...) are passed through.
#'
#' @param category Character vector of categories.
#' @return Character vector of bucket codes, `NA` for categories the
#'   templates do not use. Unrecognized categories also return `NA` with a
#'   warning.
#' @examples
#' conceptCategoryToBucket(c("Disease of interest", "drug", "risk_factor", "H"))
#' @export
conceptCategoryToBucket <- function(category) {
  map <- conceptCategoryMap()
  keys <- .normalizeFieldKey(names(map))
  input <- .normalizeFieldKey(category)
  bucket <- unname(map[match(input, keys)])
  isCode <- toupper(category) %in% names(conceptCategoryLabels)
  bucket[isCode] <- toupper(category[isCode])
  unknown <- is.na(bucket) & !(input %in% keys) & !is.na(category)
  if (any(unknown)) {
    warning(sprintf("Unrecognized concept set categories ignored: %s",
                    paste(unique(category[unknown]), collapse = ", ")), call. = FALSE)
  }
  bucket
}

#' Normalize concept set inputs into one Capr ConceptSet (or NULL) per bucket
#'
#' Accepts either the `conceptSets` list (named by bucket code or category)
#' or the legacy `cs_*` arguments, never both.
#' @noRd
.resolveConceptSetArgs <- function(conceptSets, legacy) {
  legacyUsed <- !vapply(legacy, is.null, logical(1))
  if (is.null(conceptSets)) {
    if (any(legacyUsed)) .warnLegacyConceptSetArgs()
    return(legacy)
  }
  if (any(legacyUsed)) {
    stop("Supply concept sets via `conceptSets` or the `cs_*` arguments, not both", call. = FALSE)
  }
  if (!is.list(conceptSets) || is.null(names(conceptSets)) || any(!nzchar(names(conceptSets)))) {
    stop("`conceptSets` must be a named list, e.g. list(I = csI, T = csT)", call. = FALSE)
  }
  buckets <- conceptCategoryToBucket(names(conceptSets))
  result <- stats::setNames(vector("list", length(legacy)), names(legacy))
  for (i in seq_along(conceptSets)) {
    bucket <- buckets[i]
    if (is.na(bucket) || is.null(conceptSets[[i]])) next
    key <- paste0("cs_", bucket)
    if (!is.null(result[[key]])) {
      stop(sprintf("More than one concept set supplied for bucket '%s'; union them first (see unionConceptsets())", bucket),
           call. = FALSE)
    }
    if (!methods::is(conceptSets[[i]], "ConceptSet")) {
      stop(sprintf("conceptSets[['%s']] must be a Capr ConceptSet", names(conceptSets)[i]), call. = FALSE)
    }
    result[[key]] <- conceptSets[[i]]
  }
  result
}

.legacyArgsWarned <- new.env(parent = emptyenv())

.warnLegacyConceptSetArgs <- function() {
  if (isTRUE(.legacyArgsWarned$done)) return(invisible())
  .legacyArgsWarned$done <- TRUE
  warning("The `cs_I` ... `cs_E` arguments are deprecated; pass `conceptSets = list(I = ..., H = ...)` instead. ",
          "This warning is shown once per session.", call. = FALSE)
}

#' Full clinical category names, keyed by their standard letter code
#'
#' Used to give Capr `ConceptSet` objects a self-describing `name` (e.g.
#' "Disease of Interest (I)") instead of the bare letter code.
#' @noRd
conceptCategoryLabels <- c(
  I = "Disease of Interest (I)",
  H = "Hypernym (H)",
  S = "Symptoms (S)",
  D = "Diagnostic Tests/Procedures (D)",
  T = "Treatments (T)",
  C = "Complications (C)",
  A = "Alternative Diagnoses (A)",
  E = "Etiology / Suspected Cause (E)"
)
