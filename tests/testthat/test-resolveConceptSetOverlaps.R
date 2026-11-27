test_that("I wins over A, H, E, S, C", {
  csI <- mockCs(c(1L, 2L), "I")
  csA <- mockCs(c(2L, 3L), "A")
  csH <- mockCs(c(2L, 4L), "H")
  csE <- mockCs(c(2L, 5L), "E")
  csS <- mockCs(c(2L, 6L), "S")
  csC <- mockCs(c(2L, 7L), "C")

  expect_warning(
    resolved <- resolveConceptSetOverlaps(
      csI, cs_H = csH, cs_S = csS, cs_D = NULL, cs_T = NULL,
      cs_C = csC, cs_A = csA, cs_E = csE, warnOnOverlap = FALSE
    ),
    "expression items"
  )

  expect_false(2L %in% (resolved$cs_A@Expression |> vapply(function(x) x@Concept@concept_id, integer(1))))
  expect_false(2L %in% (resolved$cs_H@Expression |> vapply(function(x) x@Concept@concept_id, integer(1))))
  expect_false(2L %in% (resolved$cs_E@Expression |> vapply(function(x) x@Concept@concept_id, integer(1))))
  expect_false(2L %in% (resolved$cs_S@Expression |> vapply(function(x) x@Concept@concept_id, integer(1))))
  expect_false(2L %in% (resolved$cs_C@Expression |> vapply(function(x) x@Concept@concept_id, integer(1))))
})

test_that("A wins over H, E, S, C but not I", {
  csI <- mockCs(c(1L), "I")
  csA <- mockCs(c(10L), "A")
  csH <- mockCs(c(10L, 11L), "H")

  expect_warning(
    resolved <- resolveConceptSetOverlaps(
      csI, cs_H = csH, cs_A = csA, warnOnOverlap = FALSE
    ),
    "expression items"
  )

  hIds <- vapply(resolved$cs_H@Expression, function(x) x@Concept@concept_id, integer(1))
  expect_false(10L %in% hIds)
  expect_true(11L %in% hIds)
})

test_that("S and C are allowed to overlap", {
  csI <- mockCs(c(1L), "I")
  csS <- mockCs(c(20L), "S")
  csC <- mockCs(c(20L), "C")

  expect_warning(
    resolved <- resolveConceptSetOverlaps(csI, cs_S = csS, cs_C = csC, warnOnOverlap = FALSE),
    "expression items"
  )

  expect_false(is.null(resolved$cs_S))
  expect_false(is.null(resolved$cs_C))
})

test_that("a set reduced to zero concepts becomes NULL", {
  csI <- mockCs(c(30L), "I")
  csA <- mockCs(c(30L), "A")

  expect_warning(
    resolved <- resolveConceptSetOverlaps(csI, cs_A = csA, warnOnOverlap = FALSE),
    "expression items"
  )

  expect_null(resolved$cs_A)
})

test_that("without a connection, precedence warns that it is not resolving descendants", {
  csI <- mockCs(c(1L), "I")
  csS <- mockCs(c(2L), "S")

  expect_warning(
    resolveConceptSetOverlaps(csI, cs_S = csS, warnOnOverlap = FALSE),
    "includeDescendants"
  )
})

test_that("connection without vocabularyDatabaseSchema is an error", {
  csI <- mockCs(c(1L), "I")
  con <- fakeConnection()

  expect_error(
    resolveConceptSetOverlaps(csI, connection = con),
    "vocabularyDatabaseSchema is required"
  )
})

# ---- resolved code lists --------------------------------------------------
# These tests replace the vocabulary round trip with a stub so they exercise
# the precedence logic without a database. `.resolveConceptSetCodesAll()` is
# the only place the vocabulary is consulted.

# Run resolveConceptSetOverlaps() with .resolveConceptSetCodesAll stubbed.
withStubCodes <- function(stub, expr) {
  ns <- asNamespace("PhenotypeTemplates")
  original <- get(".resolveConceptSetCodesAll", envir = ns)
  unlockBinding(".resolveConceptSetCodesAll", ns)
  assign(".resolveConceptSetCodesAll", stub, envir = ns)
  on.exit({
    assign(".resolveConceptSetCodesAll", original, envir = ns)
    lockBinding(".resolveConceptSetCodesAll", ns)
  })
  force(expr)
}

fakeConnection <- function() structure(list(), class = "fakeConnection")

test_that("precedence acts on resolved descendants, not on expression roots", {
  # I is a root with descendants; S is a descendant of that root, written as
  # its own concept. Their roots differ, but their code lists do not.
  csI <- Capr::cs(Capr::descendants(192671L), name = "I")
  csS <- Capr::cs(Capr::descendants(434797L), name = "S")

  stubResolve <- function(sets, connection, vocabularyDatabaseSchema) {
    # 434797 is a descendant of 192671 in this stub vocabulary.
    codes <- list(I = c(192671L, 434797L), S = c(434797L))
    setCodes <- lapply(names(sets), function(n) codes[[n]])
    itemCodes <- list()
    for (n in names(sets)) {
      for (i in seq_along(sets[[n]]@Expression)) itemCodes[[paste0(n, ":", i)]] <- codes[[n]]
    }
    list(setCodes = stats::setNames(setCodes, names(sets)), itemCodes = itemCodes)
  }

  resolved <- withStubCodes(stubResolve,
    resolveConceptSetOverlaps(
      csI, cs_S = csS, warnOnOverlap = FALSE,
      connection = fakeConnection(),
      vocabularyDatabaseSchema = "vocab"
    )
  )

  # S is entirely covered by I's resolved codes, so S is dropped.
  expect_null(resolved$cs_S)

  # The old behaviour would have kept S because 434797 != 192671.
  rootsDiffer <- as.integer(csS@Expression[[1]]@Concept@concept_id) %in%
    as.integer(csI@Expression[[1]]@Concept@concept_id)
  expect_false(rootsDiffer)
})

test_that("an item only partly covered by a higher-priority set is kept", {
  # S has two items; only one of them is covered by I.
  csI <- Capr::cs(Capr::descendants(100L), name = "I")
  csS <- Capr::cs(Capr::descendants(200L), Capr::descendants(300L), name = "S")

  stubResolve <- function(sets, connection, vocabularyDatabaseSchema) {
    setCodes <- list(I = c(100L), S = c(200L, 300L))
    itemCodes <- list("I:1" = c(100L), "S:1" = c(200L), "S:2" = c(300L, 999L))
    list(
      setCodes = stats::setNames(lapply(names(sets), function(n) setCodes[[n]]), names(sets)),
      itemCodes = itemCodes
    )
  }

  resolved <- withStubCodes(stubResolve,
    resolveConceptSetOverlaps(
      csI, cs_S = csS, warnOnOverlap = FALSE,
      connection = fakeConnection(),
      vocabularyDatabaseSchema = "vocab"
    )
  )

  # Item 1 (200) is covered by I and dropped; item 2 (300, 999) is not fully
  # covered, so it stays and 300 is not lost.
  remainingIds <- vapply(resolved$cs_S@Expression, function(x) as.integer(x@Concept@concept_id), integer(1))
  expect_equal(remainingIds, 300L)
})

test_that("excluded items are never removed by precedence", {
  csI <- Capr::cs(Capr::descendants(100L), name = "I")
  csS <- Capr::cs(
    Capr::descendants(100L),
    Capr::exclude(Capr::descendants(500L)),
    name = "S"
  )

  stubResolve <- function(sets, connection, vocabularyDatabaseSchema) {
    list(
      setCodes = stats::setNames(lapply(names(sets), function(n) {
        if (n == "I") c(100L) else c(100L, 500L)
      }), names(sets)),
      # Excluded items contribute no codes, so they have no item entry.
      itemCodes = list("I:1" = c(100L), "S:1" = c(100L))
    )
  }

  resolved <- withStubCodes(stubResolve,
    resolveConceptSetOverlaps(
      csI, cs_S = csS, warnOnOverlap = FALSE,
      connection = fakeConnection(),
      vocabularyDatabaseSchema = "vocab"
    )
  )

  expect_equal(length(resolved$cs_S@Expression), 1L)
  expect_true(isTRUE(resolved$cs_S@Expression[[1]]@isExcluded))
})

test_that("buildPhenotypeTemplates warns once when precedence is unresolved", {
  csI <- mockCs(c(1L), "I")
  cfg <- list(clinicalCourse = "persistent_stable", expectedCareSetting = "outpatient_expected")

  # Reset the once-per-session flag so this test observes the warning.
  ns <- asNamespace("PhenotypeTemplates")
  env <- get(".unresolvedPrecedenceWarned", envir = ns)
  if (exists("done", envir = env, inherits = FALSE)) rm("done", envir = env)

  expect_warning(
    buildPhenotypeTemplates(
      conceptSets = list(I = csI), phenotypeLabel = "X", config = cfg
    ),
    "includeDescendants"
  )

  # Second call in the same session does not repeat it.
  expect_silent(
    buildPhenotypeTemplates(
      conceptSets = list(I = csI), phenotypeLabel = "X", config = cfg
    )
  )
})

test_that("missing buckets are tolerated with and without a connection", {
  csI <- mockCs(c(1L, 2L), "I")
  csH <- mockCs(c(2L, 4L), "H")

  # No connection: every bucket except I and H is NULL. This used to error
  # with "no applicable method for `@` applied to an object of class NULL".
  expect_warning(
    resolved <- resolveConceptSetOverlaps(csI, cs_H = csH, warnOnOverlap = FALSE),
    "expression items"
  )
  expect_null(resolved$cs_S)
  expect_null(resolved$cs_A)

  # With a connection the resolver filters NULL buckets before resolving.
  stubResolve <- function(sets, connection, vocabularyDatabaseSchema) {
    list(
      setCodes = stats::setNames(lapply(names(sets), function(n) {
        if (n == "I") c(1L, 2L) else c(2L, 4L)
      }), names(sets)),
      itemCodes = list("I:1" = 1L, "I:2" = 2L, "H:1" = 2L, "H:2" = 4L)
    )
  }
  resolved2 <- withStubCodes(stubResolve,
    resolveConceptSetOverlaps(
      csI, cs_H = csH, warnOnOverlap = FALSE,
      connection = fakeConnection(),
      vocabularyDatabaseSchema = "vocab"
    )
  )
  expect_null(resolved2$cs_S)
  hIds <- vapply(resolved2$cs_H@Expression, function(x) x@Concept@concept_id, integer(1))
  expect_equal(hIds, 4L)
})
