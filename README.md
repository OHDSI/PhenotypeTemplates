PhenotypeTemplates
==================

[![Build Status](https://github.com/OHDSI/PhenotypeTemplates/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/PhenotypeTemplates/actions?query=workflow%3AR-CMD-check)

PhenotypeTemplates is part of [HADES](https://ohdsi.github.io/Hades/).

Introduction
============

Generates a set of candidate cohort definitions for a phenotype from concept
sets grouped by clinical role and a structured clinical context (clinical
course, care setting and expected evidence). The variants can be generated
with CohortGenerator and compared to choose a final definition.

Used by [Pegasus](https://github.com/OHDSI/Pegasus), and usable on its own.

Example
=======

```r
library(PhenotypeTemplates)

context <- read.csv(system.file("examples", "clinical_contexts.csv", package = "PhenotypeTemplates"))
validateClinicalContext(context)

cohorts <- buildPhenotypeTemplates(
  conceptSets = list(I = myDiseaseConceptSet, T = myTreatmentConceptSet),
  phenotypeLabel = "Rheumatoid arthritis",
  config = context[1, ]
)
```

See `vignette("clinical-context-to-phenotype")` for the clinical context
fields, concept set buckets and the cohort variants produced.

Installation
============

Requires R (>= 4.1.0) and Java. See the
[HADES R setup](https://ohdsi.github.io/Hades/rSetup.html), then:

```r
remotes::install_github("OHDSI/PhenotypeTemplates")
```

User documentation
==================

Documentation can be found on the [package website](https://ohdsi.github.io/PhenotypeTemplates/).

Support
=======

- Developer questions and comments: [OHDSI Forums](https://forums.ohdsi.org/c/developers)
- Bugs and feature requests: [GitHub issues](https://github.com/OHDSI/PhenotypeTemplates/issues)

License
=======

PhenotypeTemplates is licensed under Apache License 2.0.
