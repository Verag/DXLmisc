# DXLmisc

**Miscellaneous functions for internal use at DataXL**

[![R](https://img.shields.io/badge/R-%23276DC3.svg?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![License: GPL (>= 3)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%203%29-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Description

**DXLmisc** is an R package that collects a set of diverse utility functions developed for internal use at **DataXL**.

It provides tools for data cleaning, robust date handling, report formatting, Excel file operations, and various convenience functions that frequently appear in analysis and automation projects.

Current version: **0.1.1**

## Installation

Since the package is not on CRAN, install it directly from GitHub:

```r
# Using remotes / devtools
remotes::install_github("dxl-vg/DXLmisc")

# Recommended for advanced users (using pak)
pak::pkg_install("dxl-vg/DXLmisc")
