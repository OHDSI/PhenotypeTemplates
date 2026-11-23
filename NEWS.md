PhenotypeTemplates 0.1.0.9000
========================

Development version.

- New article `vignette("acnu-drug-exposure-cohorts")` describing how to
  build active comparator new user (ACNU) drug exposure cohorts with the
  templates: the drug is the bucket `I` index event, each arm is its own
  cohort, and no outcome, indication or comorbidity evidence is encoded.
- New `extras/AcnuDrugExposureCohorts.R` worked example that builds one
  new-user cohort per treatment arm from a design table shared by both arms.
- New tests locking in the ACNU invariants: a drug-only arm yields exactly one
  L0 cohort, whose entry event is a drug exposure and whose only concept set
  is the arm's drug set.

PhenotypeTemplates 0.1.0
========================

First open-source release.

- Clinical context contract: `clinicalContextSpecification()`,
  `validateClinicalContext()` and `phenotypeConfigFromContext()`, with a JSON
  Schema in `inst/schema/clinical-context.schema.json`.
- `buildPhenotypeTemplates()` takes `conceptSets = list(...)`, named by bucket
  code (`I`, `H`, ...) or by category (`"Disease of interest"`, `"Drug"`,
  ...). See `conceptCategoryMap()` and `conceptCategoryToBucket()`. The
  `cs_I` ... `cs_E` arguments still work but are deprecated.
- `config` may be a single-row clinical context data frame instead of a
  `phenotypeConfig()` list.
- `inferPhenotypeOccurrenceSettings()` is exported.
- `unionConceptsets()` combines several concept sets for one bucket.
- Example clinical contexts in `inst/examples/clinical_contexts.csv`.
