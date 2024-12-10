# Download and Manipulate Integrated Postsecondary Education Data System (IPEDS) Data

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
