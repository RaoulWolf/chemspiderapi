
<!-- README.md is generated from README.Rmd. Please edit that file -->
chemspideR
==========

> R functionalities for ChemSpider's new API services

ChemSpider has introduced a new API syntax in late 2018. This package provides R wrappers around the new API services. E.g., `records-recordId-details-get` becomes `chemspideR::get_details()`. The `chemspideR` package is platform independent, but requires a valid ChemSpider API key. While the `chemspideR` package does *not* provide any tools to securely store and access the API key, a look into the excellent [`keyring`](https://github.com/r-lib/keyring) package is recommended.

The aim of this package is to

1.  translate the new ChemSpider API services into R-friendly functions
2.  include thorough quality checking *before* the query is send, to avoid using up the query quota because of, e.g., spelling errors
3.  create the R functionality in a way that is suitable for both `base` and `tidyverse` programming
4.  include (non-ChemSpider) convenience functions to complement the functionality

Coverage
--------

As of 2018-10-10, the following functionalities are implemented (56% coverage):

**FILTERING**

-   filter-element-post
-   filter-formula-batch-post
-   filter-formula-batch-queryId-results-get
-   filter-formula-batch-queryId-status-get
-   filter-formula-post: `chemspideR::post_formula()`
-   filter-inchi-post: `chemspideR::post_inchi()`
-   filter-inchikey-post: `chemspideR::post_inchikey()`
-   filter-intrinsicproperty-post
-   filter-mass-batch-post
-   filter-mass-batch-queryId-results-get
-   filter-mass-batch-queryId-status-get
-   filter-mass-post: `chemspideR::post_mass()`
-   filter-name-post: `chemspideR::post_name()`
-   filter-queryId-results-get: `chemspider::get_results()`
-   filter-queryId-results-sdf-get <span style="color:red">**TROUBLE**<span>
-   filter-queryId-status-get `chemspideR::get_status()`
-   filter-smiles-post: `chemspideR::post_smiles()`

**LOOKUPS**

-   lookups-datasources-get `chemspideR::get_data_sources()`

**RECORDS**

-   records-batch-post <span style="color:red">**TROUBLE**<span>
-   records-recordId-details-get: `chemspideR::get_details()`
-   records-recordId-externalreferences-get: `chemspideR::get_external_references()`
-   records-recordId-image-get: `chemspideR::get_image()`
-   records-recordId-mol-get: `chemspideR::get_mol()`

**TOOLS**

-   tools-convert-post: `chemspideR::post_convert()`
-   tools-validate-inchikey-post: `chemspideR::post_validate_inchikey()`

**WRITING (`chemspideR` EXCLUSIVE)**

-   `chemspider::write_image()`
-   `chemspider::write_mol()`

Installation
------------

### Dependencies

This packages relies on two essential dependencies, namely `httr` and `jsonlite`. Additional (but not crucial) functionality for handling images is coming from the `png` package.

If not already installed, these packages *should* be installed automatically when installing `chemspideR`. Should this result in trouble, the dependency packages can be installed manually:

``` r
install.packages(c("httr", "jsonlite"))
# install.packages("png")
```

If `httr` and/or `jsonlite` are missing from the R library, all functions of `chemspider` will throw an error.

### R package

Install the package from GitHub (using `devtools`):

``` r
# install.packages("devtools")
devtools::install_github("NIVANorge/chemspideR")
```

Currently the only tested environment for `chemspideR` is Windows 10, but it should install smoothly on macOS and Linux distributions as well. Please open an issue if you run into any troubles.

Usage
-----

Funding
-------

This package was created in conjunction with NIVA's Computational Toxicity Program (NCTP) at NIVA's Section for Ecotoxicology and Risk Assessment and funded by the MixRisk project ().

License
-------

MIT © Raoul Wolf