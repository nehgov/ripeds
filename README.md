# Download and Manipulate Integrated Postsecondary Education Data System (IPEDS) Data

[![R-CMD-check](https://github.com/nehgov/ripeds/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nehgov/ripeds/actions/workflows/R-CMD-check.yaml)
![GitHub Release](https://img.shields.io/github/v/release/nehgov/ripeds)
![Static Badge](https://img.shields.io/badge/release-beta-blue)

**THIS SOFTWARE PACKAGE IS IN BETA RELEASE AND MAY CONTAIN SOME BUGS. IN
ADDITION, SOME FUNCTIONALITY MAY CHANGE. PLEASE SEE DISCLAIMER BELOW AND
USE ACCORDINGLY.**

The `ripeds` R package provides a series of piped functions to
facilitate downloading Department of Education Integrated Postsecondary
Education Data System (IPEDS) complete data files.

All command pipes must start with `ipeds_init()`, end with
`ipeds_get()`, and be linked with the base pipe, `|>`, or magrittr pipe
function, `%>%`. Internal commands, `ipeds_select()`, `ipeds_filter()`,
and `ipeds_year()` come in any order in the pipe chain. Only
`ipeds_select()` is required.

This software package was developed by the Office of Data and Evaluation
of the National Endowment for the Humanities.

# Installation

    devtools::install_github("nehgov/ripeds")

# Example

    library(ripeds)

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

# Help and vignettes

See the [`ripeds`](https://nehgov.github.io/ripeds) site for more help
as well as vignettes on explaining package functionality.

# Disclaimer

The software package is licensed under the CC0 license. It is provided
“as is” without any warranty of any kind, either expressed, implied, or
statutory, including, but not limited to, any warranty that the subject
software will conform to specifications, any implied warranties of
merchantability, fitness for a particular purpose, or freedom from
infringement, any warranty that the subject software will be error free,
or any warranty that documentation, if provided, will conform to the
subject software. This agreement does not, in any manner, constitute an
endorsement by NEH or any prior recipient of any results, resulting
designs, hardware, software products or any other applications resulting
from use of the subject software. Further, NEH disclaims all warranties
and liabilities regarding third-party software, if present in the
original software, and distributes it “as is.”

The recipient agrees to waive any and all claims against the United
States government, its contractors and subcontractors, as well as any
prior recipient. If recipient’s use of the subject software results in
any liabilities, demands, damages, expenses or losses arising from such
use, including any damages from products based on, or resulting from,
recipient’s use of the subject software, recipient shall indemnify and
hold harmless the United States government, its contractors and
subcontractors, as well as any prior recipient, to the extent permitted
by law. Recipient’s sole remedy for any such matter shall be the
immediate, unilateral termination of this agreement.
