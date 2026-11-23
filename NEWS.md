PhenotypeTemplates 0.1.0.9000
========================

Development version.

- New article `vignette("phenelope-concept-sets-to-templates")` describing how
  concept sets produced by OHDSI/Phenelope (LLM-based concept set generation)
  are assigned to template buckets and passed to `buildPhenotypeTemplates()`.
- New `extras/PhenelopeTemplateSets.R` worked example: runs Phenelope once per
  clinical role, checks each concept set is non-empty and in the domain its
  role implies, then builds the template cohort set.

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
