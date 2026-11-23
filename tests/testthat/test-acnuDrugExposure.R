# Active comparator new user (ACNU) drug exposure cohorts --------------------
#
# An ACNU arm is built with the drug exposure as the only concept set (bucket
# I). These tests lock in the properties the design depends on: one L0 cohort
# per arm, no evidence, outcome or indication evidence in the entry event, and
# symmetric arms when the same config is reused.

acnuConfig <- list(
  clinicalCourse = "persistent_stable",
  expectedCareSetting = "outpatient_expected"
)

test_that("a drug-only arm produces exactly one L0 cohort", {
  cds <- buildPhenotypeTemplates(
    conceptSets = list(I = mockCs(c(21600993L), "DOAC arm")),
    phenotypeLabel = "DOAC new users",
    config = acnuConfig
  )

  expect_equal(nrow(cds), 1L)
  expect_equal(cds$evidenceLevel, "L0")
  expect_equal(cds$evidenceComponents, "")
  expect_false(cds$hypernymIndex)
  expect_false(cds$hospitalEntry)
  expect_false(cds$altDxExcluded)
  expect_equal(cds$cohortName, "[PheTpl] DOAC new users - Base")
})

test_that("the arm cohort contains only the drug concept set", {
  cds <- buildPhenotypeTemplates(
    conceptSets = list(I = mockCs(c(21600993L), "DOAC arm")),
    phenotypeLabel = "DOAC new users",
    config = acnuConfig
  )

  expression <- jsonlite::fromJSON(cds$json[1], simplifyVector = FALSE)
  conceptIds <- unlist(lapply(expression$ConceptSets, function(cs) {
    vapply(cs$expression$items, function(i) as.integer(i$concept$CONCEPT_ID), integer(1))
  }))

  # Only the arm's own drug concept, nothing else.
  expect_equal(conceptIds, 21600993L)

  # Entry event is the drug exposure itself, not a diagnosis.
  expect_true("DrugExposure" %in% names(expression$PrimaryCriteria$CriteriaList[[1]]))
})

test_that("both arms built from one config have identical, disjoint structure", {
  config <- acnuConfig
  doac <- buildPhenotypeTemplates(
    conceptSets = list(I = mockCs(c(21600993L), "DOAC arm")),
    phenotypeLabel = "DOAC new users",
    config = config
  )
  warfarin <- buildPhenotypeTemplates(
    conceptSets = list(I = mockCs(c(1310149L), "Warfarin arm")),
    phenotypeLabel = "Warfarin new users",
    config = config,
    startCohortId = max(doac$cohortId)
  )

  # Same number of cohorts per arm: neither arm gets extra variants.
  expect_equal(nrow(doac), nrow(warfarin))

  # Disjoint cohort ids, so the arms can be combined into one set.
  expect_length(intersect(doac$cohortId, warfarin$cohortId), 0L)

  # Same design axes in both arms (row names differ by cohort id).
  designColumns <- c("evidenceLevel", "evidenceComponents", "hypernymIndex",
                     "hospitalEntry", "altDxExcluded")
  for (column in designColumns) {
    expect_equal(unname(doac[[column]]), unname(warfarin[[column]]), label = column)
  }
})
