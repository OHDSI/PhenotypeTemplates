PhenotypeTemplates 0.1.0.9000
========================

Development version.

- Concept set precedence now runs on *resolved* code lists when a vocabulary
  connection is supplied. `buildPhenotypeTemplates()` gains `connection` and
  `vocabularyDatabaseSchema`; when set, every bucket is resolved once through
  `CONCEPT_ANCESTOR` / 'Maps to', so an `includeDescendants` overlap between
  buckets is detected even when the expression roots differ (e.g.
  `I = 192671*` and `S = 434797`). Previously precedence compared expression
  items only, so such overlaps were silently missed.
- An item is now removed by precedence only when *every* code it contributes
  is also provided by the higher-priority set. Partially covered items are
  kept, so precedence no longer deletes codes the higher-priority set lacks.
- Excluded concept set items are no longer candidates for removal: they
  contribute no codes.
- Without a connection, precedence still falls back to comparing expression
  items, and `buildPhenotypeTemplates()` warns once per session to say so.


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
