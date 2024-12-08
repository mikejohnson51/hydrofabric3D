
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrofabric3D <img src="man/figures/logo.png" width=130 height = 150 align="right" />

<!-- badges: start -->

[![R CMD
Check](https://github.com/mikejohnson51/hydrofabric3D/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/hydrofabric3D/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of hydrofabric3D is to generate DEM-based cross sections for
hydrographic networks.

### Installation

You can install the development version of hydrofabric3D from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lynker-spatial/hydrofabric3D")
```

## Overview

This project provides the general tooling needed to create a flexible
river channel data product based for the [Reference
Hydrofabric](https://noaa-owp.github.io/hydrofabric/articles/02-design-deep-dive.html)
that supports the modeling needs of NOAA and USGS. It is comprised of
multiple modules that together form a clear picture of hydrofabric:

<img src="man/figures/data_model.png" width="100%" />

This package focuses on the fourth element, “multiscale cross sections”
that can be integrated from a variety of sources:

- Development of an automated tools to generate cross sections from a
  DEM (this package)
- Access to machine learning models that predict river channel depth,
  width, and shape (see [channel-shape-ML
  repo](https://github.com/NOAA-OWP/3d-hydrofabric/)
- Integration of channel cross sections from HEC-RAS
  ([RRASSLER](https://github.com/NOAA-OWP/RRASSLER) and eHydro
  ([eHydRo](https://github.com/JamesColl-NOAA/eHydRo))
- Estimates of channel width from multi-source data (e.g. remote sensing
  and OpenStreetMaps)

The interoprability between these is empowered by a shared data model
shown below that is in line with the hydrofabric data model at large.

<img src="man/figures/data_model2.png" width="100%" />

visit this
[website](https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html)
for more updates.

## Base line cross section generation

One of the core utilities of this package is to generate DEM-based
cross-sections (flood plains) for hydrographic networks. An example of
how these cross-sections look is shown below and a full description is
available at
[hydrofabric3D](https://mikejohnson51.github.io/hydrofabric3D/articles/basic_use.html)

<img src="man/figures/cs2.png" width="100%" /> In addition to
generation, the package can classify the cross section into left, right
banks and in-channel as shown below. The problem is the flat line on the
bottom of these plots, which represents the water level when this data
was collected and nothing about the conditions at the time of collection
(flood, dry year, etc.)

<img src="man/figures/cs3.png" width="100%" />

## Getting involved

This project is in active development and all contributions are welcome.
To get started a list of contact information is outlined below loosely
by area of focus. Feel free to reach out to any and all: Mike Johnson
(<mike.johnson@noaa.gov>) hydrofabric development and data models.

Angus Watters (<angus.watters@noaa.gov>) cross section generation.

Dami Eyelade (<dami.eyelade@noaa.gov>) integration of satellite derived
products.

Arash Modaresi Rad (<arash.rad@noaa.gov>) development of machine
learning models.
