PhenotypeTemplates
==================

[![Build Status](https://github.com/OHDSI/PhenotypeTemplates/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/PhenotypeTemplates/actions?query=workflow%3AR-CMD-check)

PhenotypeTemplates is part of [HADES](https://ohdsi.github.io/Hades/).

Introduction
============

PhenotypeTemplates generates a set of candidate OHDSI cohort definitions for a
phenotype from two inputs:

1. **Concept sets grouped by clinical role**: disease of interest, broader
   (hypernym) terms, symptoms, diagnostic procedures, treatments,
   complications, etiology and alternative diagnoses.
2. **A clinical context**: a small, structured description of the condition
   (clinical course, expected care setting, demographic limits, and which
   supporting evidence is expected).

From these it builds cohort variants that differ in evidence strictness, index
specificity, hospitalization and exclusion of alternative diagnoses. The
result is a `CohortGenerator` cohort definition set that can be generated and
compared, for example with PheValuator or CohortDiagnostics, to choose a final
definition.

The package is used by [Pegasus](https://github.com/OHDSI/Pegasus), which
produces the clinical context and concept sets automatically. It can be used
on its own with hand-written inputs.

Features
========

- A documented clinical context contract (`clinicalContextSpecification()`),
  published as a JSON Schema in `inst/schema/clinical-context.schema.json`.
- Validation of clinical contexts with readable errors
  (`validateClinicalContext()`).
- Translation of a clinical context into a template configuration
  (`phenotypeConfigFromContext()`).
- One builder for the full set of cohort variants (`buildPhenotypeTemplates()`).
- A concept set union across sources (`unionConceptsets()`), for combining
  several concept sets that fill the same bucket.

Example
=======

```r
library(Capr)
library(PhenotypeTemplates)

context <- data.frame(
  condition_name = "Gastrointestinal hemorrhage",
  clinical_course = "TRANSIENT_RECURRENT",
  expected_care_setting = "ACUTE_CARE_EXPECTED",
  minimum_interepisode_gap_days = 90,
  recommended_cohort_exit = "FIXED_WINDOW_FROM_INDEX",
  recommended_exit_days = 30,
  has_discrete_recorded_symptoms = TRUE,
  requires_diagnostic_test_or_procedure = TRUE,
  requires_active_treatment_within_30d = TRUE,
  expects_condition_specific_followup_or_sequelae_1yr = FALSE
)
validateClinicalContext(context)

cohorts <- buildPhenotypeTemplates(
  conceptSets = list(
    "Disease of interest"   = cs(descendants(192671L), name = "GI hemorrhage"),
    "Clinical presentation" = cs(descendants(200219L), name = "Abdominal pain"),
    "Diagnostic procedure"  = cs(descendants(4028949L), name = "Endoscopy"),
    "Drug"                  = cs(descendants(923645L), name = "Pantoprazole")
  ),
  phenotypeLabel = "Gastrointestinal hemorrhage",
  config = context
)
cohorts[, c("cohortId", "cohortName", "evidenceLevel", "hospitalEntry")]
```

The concept IDs above are illustrative and are not a validated phenotype.

Concept set buckets
===================

| Bucket | Category (as produced by concept set generators) | Role in the templates |
| --- | --- | --- |
| `I` | Disease of interest | Index event |
| `H` | Hypernym | Optional broader index event; follow-up care evidence |
| `S` | Clinical presentation | Evidence in the 30 days before index |
| `D` | Diagnostic procedure, Measurement | Evidence in the 30 days before index |
| `T` | Drug, Treatment procedure | Evidence in the 30 days after index |
| `C` | Progression complication | Follow-up care evidence, 1 to 365 days after index |
| `E` | Etiology | Follow-up care evidence, 30 days either side of index |
| `A` | Alternative diagnosis | Optional exclusion, 30 days either side of index |

Pass concept sets as `conceptSets = list(...)` named by bucket code or
category. See `conceptCategoryMap()` for the full mapping. Combine several
concept sets for the same bucket with `unionConceptsets()` first.

Cohort naming
=============

Cohort names are short, human-readable labels such as
`[PheTpl] Gastrointestinal hemorrhage - Evid Sx|Test, Hospital`. Names are for
display and may change between versions. To identify a variant
programmatically, use the structured columns `evidenceLevel`,
`evidenceComponents`, `hypernymIndex`, `hospitalEntry` and `altDxExcluded`.

Technology
==========

PhenotypeTemplates is an R package. Cohort definitions are built with
[Capr](https://github.com/OHDSI/Capr) and rendered with
[CirceR](https://github.com/OHDSI/CirceR).

System requirements
===================

Requires R (version 4.1.0 or higher) and Java, which is needed by CirceR and
DatabaseConnector.

Installation
============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for
   configuring your R environment, including Java.

2. In R, use the following commands to download and install PhenotypeTemplates:

   ```r
   install.packages("remotes")
   remotes::install_github("OHDSI/PhenotypeTemplates")
   ```

User documentation
==================

Documentation can be found on the
[package website](https://ohdsi.github.io/PhenotypeTemplates/).

Vignettes:

- From clinical context to phenotype

Support
=======

- Developer questions and comments: [OHDSI Forums](https://forums.ohdsi.org/c/developers)
- Bugs and feature requests: [GitHub issues](https://github.com/OHDSI/PhenotypeTemplates/issues)

Contributing
============

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can
contribute to this package.

License
=======

PhenotypeTemplates is licensed under Apache License 2.0.

Development
===========

PhenotypeTemplates is being developed in R Studio.

### Development status

Beta. The clinical context contract is versioned with the package; breaking
changes will be noted in NEWS.md.
