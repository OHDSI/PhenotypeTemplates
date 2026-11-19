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
