test_that("conceptCategoryToBucket maps labels, snake_case and bucket codes", {
  expect_equal(
    conceptCategoryToBucket(c("Disease of interest", "clinical_presentation", "Drug", "Treatment procedure", "h")),
    c("I", "S", "T", "T", "H")
  )
  expect_true(is.na(conceptCategoryToBucket("Risk factor")))
  expect_warning(out <- conceptCategoryToBucket("Not a category"), "Unrecognized")
  expect_true(is.na(out))
})

test_that("conceptSets argument is equivalent to legacy cs_* arguments", {
  csI <- Capr::cs(Capr::descendants(192671L), name = "I")
  csT <- Capr::cs(Capr::descendants(923645L), name = "T")
  cfg <- phenotypeConfig(clinicalCourse = "persistent_stable", expectedCareSetting = "outpatient_expected",
                         requiresActiveTreatmentWithin30d = TRUE)
  viaList <- buildPhenotypeTemplates(conceptSets = list("Disease of interest" = csI, Drug = csT),
                                     phenotypeLabel = "X", config = cfg)
  viaLegacy <- suppressWarnings(buildPhenotypeTemplates(cs_I = csI, cs_T = csT, phenotypeLabel = "X", config = cfg))
  expect_equal(viaList$json, viaLegacy$json)
})

test_that("conceptSets input errors are clear", {
  csI <- Capr::cs(Capr::descendants(192671L), name = "I")
  expect_error(buildPhenotypeTemplates(conceptSets = list(I = csI), cs_I = csI, phenotypeLabel = "X"), "not both")
  expect_error(buildPhenotypeTemplates(conceptSets = list(I = csI, "Disease of interest" = csI), phenotypeLabel = "X"),
               "More than one")
  expect_error(buildPhenotypeTemplates(conceptSets = list(I = 1L), phenotypeLabel = "X"), "ConceptSet")
  expect_error(buildPhenotypeTemplates(conceptSets = list(T = csI), phenotypeLabel = "X"), "I")
})
