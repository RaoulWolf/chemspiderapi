chemspiderapi
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/NIVANorge/chemspiderapi/workflows/R-CMD-check/badge.svg)](https://github.com/NIVANorge/chemspiderapi/actions)
[![CodeCov test
coverage](https://codecov.io/gh/NIVANorge/chemspiderapi/branch/master/graph/badge.svg)](https://codecov.io/gh/NIVANorge/chemspiderapi?branch=master)
<!-- badges: end -->

*As of 1 June 2021 this is the new official repository of
{chemspiderapi}. The old repository is orphaned.*

> R functionalities for ChemSpider’s API services

ChemSpider has introduced a new API syntax in late 2018, and [the old
ChemSpider API syntax will be shut down at the end of November
2018](http://link.rsc.org/rsps/m/xSq8Cm8ovjN8-Elm0eYB3Sey61zutqNIUUaMcyc14sQ).
`{chemspiderapi}` provides R wrappers around the new API services from
ChemSpider.

The aim of this package is to:

1.  Translate the new ChemSpider API services into R-friendly functions.
2.  Include thorough quality checking *before* the query is send, to
    avoid using up the query quota on, e.g., spelling errors.
3.  Implement the R functionality in a way that is suitable for both
    `{base}`- and `{tidyverse}`-style programming.

`{chemspiderapi}` relies on API keys to access ChemSpider’s API
services. For this, we recommend storing the ChemSpider API key using
the [`{keyring}`](https://cran.r-project.org/package=keyring) package.

To limit the rate of API queries, we recommend using the
[`{ratelimitr}`](https://cran.r-project.org/package=ratelimitr) package
or `purrr::slowly()` within the
[`{tidyverse}`](https://cran.r-project.org/package=tidyverse) package
collection.

To handle PNG images, the
[`{magick}`](https://cran.r-project.org/package=magick) package is
recommended.

We furthermore recommend the
[`{memoise}`](https://cran.r-project.org/package=memoise) package to
“remember” the results of API queries (*i.e.*, to not ruin the API
allowance).

Additionally, the superb
[`{webchem}`](https://cran.r-project.org/web/packages/webchem/) package
provides access to a lot of additional chemistry-related API services
and is highly recommended.

## Installation

### R package

Install the package from GitHub (using the `{remotes}` package;
automatically installed as part of
[`{devtools}`](https://cran.r-project.org/package=devtools)):

``` r
# install.packages("devtools")
remotes::install_github("RaoulWolf/chemspiderapi")
```

The development version can be installed with:

``` r
# install.packages("devtools")
remotes::install_github("RaoulWolf/chemspiderapi", ref = "dev")
```

Currently the only tested continuous integration environment for
`{chemspiderapi}` is Linux. It *should* install smoothly on Windows 10
and mac OS machines as well. Please open an issue if you run into any
troubles.

### Dependencies

`{chemspiderapi}` relies on two essential dependencies. The
[`{curl}`](https://cran.r-project.org/package=curl) package is used to
handle the API queries and the
[`{jsonlite}`](https://cran.r-project.org/package=jsonlite) package is
necessary to wrap and unwrap information for the queries.

If not already installed, these packages will be installed automatically
when installing `{chemspiderapi}`. Should this result in trouble, the
dependency packages can be installed manually:

``` r
install.packages(c("curl", "jsonlite"))
```

If `{curl}` or `{jsonlite}` are missing, (almost) all functions of
`{chemspiderapi}` will fail and throw an error.

## Coverage

As of 2021-06-07, the following functionalities are implemented (100%
functionality with 100% annotation):

**FILTERING**

| ChemSpider Compound API                  | `{chemspiderapi}` Wrapper              |
|:-----------------------------------------|:---------------------------------------|
| filter-element-post                      | `post_element()`                       |
| filter-formula-batch-post                | `post_formula_batch()`                 |
| filter-formula-batch-queryId-results-get | `get_formula_batch_query_id_results()` |
| filter-formula-batch-queryId-status-get  | `get_formula_batch_query_id_status()`  |
| filter-formula-post                      | `post_formula()`                       |
| filter-inchi-post                        | `post_inchi()`                         |
| filter-inchikey-post                     | `post_inchikey()`                      |
| filter-intrinsicproperty-post            | `post_intrinsic_property()`            |
| filter-mass-batch-post                   | `post_mass_batch()`                    |
| filter-mass-batch-queryId-results-get    | `get_mass_batch_query_id_results()`    |
| filter-mass-batch-queryId-status-get     | `get_mass_batch_query_id_status()`     |
| filter-mass-post                         | `post_mass()`                          |
| filter-name-post                         | `post_name()`                          |
| filter-queryId-results-get               | `get_query_id_results()`               |
| filter-queryId-results-sdf-get           | `get_query_id_results_sdf()`           |
| filter-queryId-status-get                | `get_query_id_status()`                |
| filter-smiles-post                       | `post_smiles()`                        |

**LOOKUPS**

| ChemSpider Compound API | `{chemspiderapi}` Wrapper |
|:------------------------|:--------------------------|
| lookups-datasources-get | `get_data_sources()`      |

**RECORDS**

| ChemSpider Compound API                 | `{chemspiderapi}` Wrapper             |
|:----------------------------------------|:--------------------------------------|
| records-batch-post                      | `post_batch()`                        |
| records-recordId-details-get            | `get_record_id_details()`             |
| records-recordId-externalreferences-get | `get_record_id_external_references()` |
| records-recordId-image-get              | `get_record_id_image()`               |
| records-recordId-mol-get                | `get_record_id_mol()`                 |

**TOOLS**

| ChemSpider Compound API      | `chemspiderapi` Wrapper    |
|:-----------------------------|:---------------------------|
| tools-convert-post           | `post_convert()`           |
| tools-validate-inchikey-post | `post_validate_inchikey()` |

## Best practices for ChemSpider’s Compound APIs

*This section will be updated with practical examples in the future.*

The basic workflow order for the above **FILTERING** queries is:

1.  POST Query

2.  GET Status

3.  GET Results (after GET Status returns `"Complete"`)

In practice, this means the following possible workflows can be
implemented (from left to right):

| POST Query                  | GET Status                            | GET Results                            |
|:----------------------------|:--------------------------------------|:---------------------------------------|
| `post_element()`            | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_formula()`            | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_formula_batch()`      | `get_formula_batch_query_id_status()` | `get_formula_batch_query_id_results()` |
| `post_inchi()`              | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_inchikey()`           | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_intrinsic_property()` | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_mass()`               | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_mass_batch()`         | `get_mass_batch_query_id_status()`    | `get_mass_batch_query_id_results()`    |
| `post_mass()`               | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_name()`               | `get_query_id_status()`               | `get_query_id_results()`               |
| `post_smiles()`             | `get_query_id_status()`               | `get_query_id_results()`               |

Typically, the result will be one or multiple ChemSpider IDs
(`recordId`). They can be used as input into the above **RECORDS**
queries, e.g., `get_recordId_details()`.

## Vignettes

As of 2021-06-07, the following five vignettes are available:

-   **Storing and Accessing API Keys**: A basic example on how to safely
    store and retrieve API keys using the `keyring` package.

-   **Rate-Limiting API Queries**: Multiple examples on how to slow down
    API queries. `base`, `ratelimitr`, and `tidyverse` workflows are
    provided.

-   **Remembering API Queries**: A basic example on how to “remember”
    the results of API queries using the `memoise` package. This can
    greatly reduce redundant API queries.

-   **Saving MOL and SDF Files**: Examples on how to save MOL and SDF
    files as returned by `get_record_id_mol()` and
    `get_query_id_results_sdf()`.

-   **Saving PNG Images**: Examples on how to save PNG files, as
    returned by `get_record_id_image()`, using functionalities of the
    `magick` package.

## Funding

This package was created at the [Norwegian Institute for Water Research
(NIVA)](https://www.niva.no/en) in conjunction with [NIVA’s
Computational Toxicology Program
(NCTP)](https://www.niva.no/en/projectweb/nctp) at NIVA’s [Section for
Ecotoxicology and Risk
Assessment](https://www.niva.no/en/research/ecotoxicology_and_risk_assessment)
and funded by [The Research Council of Norway
(RCN)](https://www.forskningsradet.no/en/), project 268294: [Cumulative
Hazard and Risk Assessment of Complex Mixtures and Multiple Stressors
(MixRisk)](https://www.forskningsradet.no/prosjektbanken/#/project/NFR/268294/Sprak=en).

## License

MIT © Raoul Wolf
