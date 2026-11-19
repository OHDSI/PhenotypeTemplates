validContext <- function(...) {
  ctx <- data.frame(
    condition_name = "Test condition",
    clinical_course = "TRANSIENT_RECURRENT",
    expected_care_setting = "ACUTE_CARE_EXPECTED",
    minimum_interepisode_gap_days = 90,
    has_discrete_recorded_symptoms = TRUE,
    requires_diagnostic_test_or_procedure = TRUE,
    requires_active_treatment_within_30d = FALSE,
    expects_condition_specific_followup_or_sequelae_1yr = FALSE,
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (n in names(overrides)) ctx[[n]] <- overrides[[n]]
  ctx
}

test_that("shipped example contexts are valid", {
  path <- system.file("examples", "clinical_contexts.csv", package = "PhenotypeTemplates")
  contexts <- read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(validateClinicalContext(contexts)), 0L)
})

test_that("validateClinicalContext reports missing, enum, boolean and number problems", {
  ctx <- validContext(
    clinical_course = "SOMETIMES",
    requires_active_treatment_within_30d = "maybe",
    min_age_years = -1
  )
  ctx$expected_care_setting <- NULL
  problems <- validateClinicalContext(ctx, throw = FALSE)
  expect_setequal(
    problems$field,
    c("clinical_course", "expected_care_setting", "requires_active_treatment_within_30d", "min_age_years")
  )
  expect_error(validateClinicalContext(ctx), "Invalid clinical context")
})

test_that("validateClinicalContext enforces cross-field rules", {
  problems <- validateClinicalContext(
    validContext(min_age_years = 50, max_age_years = 20, minimum_interepisode_gap_days = NA),
    throw = FALSE
  )
  expect_setequal(problems$field, c("max_age_years", "minimum_interepisode_gap_days"))

  problems <- validateClinicalContext(
    validContext(clinical_course = "TRANSIENT_SINGLE"),
    throw = FALSE
  )
  expect_equal(problems$field, "recommended_exit_days")
})

test_that("camelCase names, list input and config-spelling aliases are accepted", {
  ctx <- list(
    clinicalCourse = "persistent_recurrent",
    expectedCareSetting = "acute_care_common",
    minimumInterepisodeGapDays = 365,
    hasDiscreteRecordedSymptoms = "yes",
    requiresDiagnosticTestOrProcedure = 1,
    requiresActiveTreatmentWithin30d = FALSE,
    expectsConditionSpecificFollowupOrSequelae1yr = "false",
    sexConstraint = "male_only"
  )
  expect_equal(nrow(validateClinicalContext(ctx)), 0L)
  cfg <- phenotypeConfigFromContext(ctx)
  expect_equal(cfg$clinicalCourse, "persistent_recurrent")
  expect_null(cfg$recommendedCohortExit)
  expect_equal(cfg$minimumInterepisodeDayGap, 365)
  expect_true(cfg$hasDescreteRecordedSymptoms)
  expect_true(cfg$male)
  expect_false(cfg$female)
})

test_that("phenotypeConfigFromContext requires exactly one valid row", {
  expect_error(phenotypeConfigFromContext(rbind(validContext(), validContext())), "one")
  expect_error(phenotypeConfigFromContext(validContext(clinical_course = "BAD")), "Invalid clinical context")
})

test_that("JSON Schema file matches clinicalContextSpecification()", {
  schemaPath <- system.file("schema", "clinical-context.schema.json", package = "PhenotypeTemplates")
  schema <- jsonlite::fromJSON(schemaPath, simplifyVector = FALSE)
  spec <- clinicalContextSpecification()
  expect_setequal(names(schema$properties), spec$field)
  expect_setequal(unlist(schema$required), spec$field[spec$required])
  for (field in spec$field[spec$type == "enum"]) {
    expect_setequal(unlist(schema$properties[[field]]$enum), strsplit(spec$allowedValues[spec$field == field], "|", fixed = TRUE)[[1]])
  }
})

test_that("buildPhenotypeTemplates accepts a context row as config", {
  csI <- Capr::cs(Capr::descendants(192671L), name = "I")
  ctx <- validContext()
  fromContext <- buildPhenotypeTemplates(conceptSets = list(I = csI), phenotypeLabel = "X", config = ctx)
  fromConfig <- buildPhenotypeTemplates(conceptSets = list(I = csI), phenotypeLabel = "X",
                                        config = phenotypeConfigFromContext(ctx))
  expect_equal(fromContext$json, fromConfig$json)
  expect_true(all(fromContext$hospitalEntry))
})

test_that("phenotypeConfigFromContext produces a valid phenotypeConfig()", {
  metadataRow <- data.frame(
    clinical_course = "PERSISTENT_STABLE",
    expected_care_setting = "OUTPATIENT_EXPECTED",
    min_age_years = 18,
    max_age_years = NA,
    minimum_interepisode_gap_days = NA,
    recommended_cohort_exit = "END_OF_CONTINUOUS_OBSERVATION",
    recommended_exit_days = NA,
    has_discrete_recorded_symptoms = TRUE,
    requires_diagnostic_test_or_procedure = FALSE,
    requires_active_treatment_within_30d = FALSE,
    expects_condition_specific_followup_or_sequelae_1yr = FALSE,
    stringsAsFactors = FALSE
  )

  cfg <- phenotypeConfigFromContext(metadataRow)
  expect_equal(cfg$clinicalCourse, "persistent_stable")
  expect_equal(cfg$expectedCareSetting, "outpatient_expected")
  expect_equal(cfg$recommendedCohortExit, "continuous_observation")
  expect_false(cfg$firstOccurrenceOnly)
  expect_equal(cfg$primaryCriteriaLimit, "All")
  expect_equal(cfg$expressionLimit, "First")
  expect_equal(cfg$hospitalVisitOverlapWindow, 99999)

  exitConfig <- resolvePhenotypeExitStrategy(
    clinicalCourse = cfg$clinicalCourse,
    recommendedCohortExit = cfg$recommendedCohortExit,
    minimumInterepisodeDayGap = cfg$minimumInterepisodeDayGap,
    fixedExitDays = cfg$fixedExitDays
  )
  expect_equal(exitConfig$eraDays, 0L)
})
