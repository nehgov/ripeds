# Download and Manipulate Integrated Postsecondary Education Data System (IPEDS) Data

[![R-CMD-check](https://github.com/nehgov/ripeds/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nehgov/ripeds/actions/workflows/R-CMD-check.yaml)
[![GitHub
release](https://img.shields.io/github/release/nehgov/ripeds.svg)](https://github.com/nehgov/ripeds)
![R CMD check status](https://github.com/nehgov/ripeds/actions)
![beta](https://img.shields.io/badge/beta-blue)

The `ripeds` R package provides a series of piped functions to
facilitate downloading Department of Education Integrated Postsecondary
Education Data System (IPEDS) complete data files.

All command pipes must start with `ipeds_init()`, end with
`ipeds_get()`, and be linked with the base pipe, `|>`, or magrittr pipe
function, `%>%`. Internal commands, `ipeds_select()`, `ipeds_filter()`,
and `ipeds_year()` come in any order in the pipe chain. Only
`ipeds_select()` is required.

# Installation

    devtools::install_github("nehgov/ripeds")

# Example

    ## perform data pull
    df <- ipeds_init() |>
      ipeds_select(instnm, pt_ug) |>
      ipeds_filter(stabbr == "KY") |>
      ipeds_year(2020:2021) |>
      ipeds_get()

    ## show
    df |> head()

    ##   unitid year                      instnm stabbr pt_ug
    ## 1 156189 2020         Alice Lloyd College     KY     1
    ## 2 156189 2021         Alice Lloyd College     KY     1
    ## 3 156213 2020           Asbury University     KY     1
    ## 4 156213 2021           Asbury University     KY     1
    ## 5 156222 2020 Asbury Theological Seminary     KY     2
    ## 6 156222 2021 Asbury Theological Seminary     KY     2

# Disclaimer

This software package is licensed under the CC0 license. It is offered
as-is and makes no representations or warranties of any kind concerning
the Work, express, implied, statutory or otherwise, including without
limitation warranties of title, merchantability, fitness for a
particular purpose, non infringement, or the absence of latent or other
defects, accuracy, or the present or absence of errors, whether or not
discoverable, all to the greatest extent permissible under applicable
law.
