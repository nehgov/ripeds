## -----------------------------------------------------------------------------
## -- utils --
## -----------------------------------------------------------------------------

## -------------------------------------
## read temp files necessary for tests
## -------------------------------------

lapply(c(paste0(c("EF2021A_DIST", "EF2022A_DIST"), ".zip"),
         "ipeds_file_list.RDS"),
       function(x) {
         file.copy(file.path("..", "..", "inst", "extdata", x),
                   file.path(tempdir(), x))
       })

## -------------------------------------
## formatting
## -------------------------------------

test_that("%+%", {
  expect_equal("a" %+% "b", paste0("a", "b"))
  expect_equal("a" %+% "b", paste("a", "b", sep = ""))
})

test_that("hline", {
  expect_equal(hline(5), "-----")
  expect_equal(hline(3, "="), "===")
})

test_that("create_search_str", {
  expect_equal(create_search_str(letters[1:3]), "a|b|c")
})

## -------------------------------------
## hash helpers
## -------------------------------------

test_that("convert_hash_name", {
  expect_equal(convert_hash_name("idxf"), "filename")
  expect_equal(convert_hash_name("idxv"), "varname")
  expect_equal(convert_hash_name("idxd"), "description")
})

test_that("convert_hash_df_names", {
  df <- data.frame("idxf" = letters[1:3],
                   "idxv" = letters[1:3],
                   "idxd" = letters[1:3])
  expect_equal(names(convert_hash_df_names(df)),
               c("filename", "varname", "description"))
})

## get_hash
test_that("get_hash", {
  ## NB: hashes will change with updates of data, so checking that *_hash and
  ## *_hash_lu are inverses
  ## file names
  expect_equal(get_hash(get_hash("HD2020", file_hash), file_hash_lu), c("HD2020" = "HD2020"))
  ## var names
  expect_equal(get_hash(get_hash("instnm", vars_hash), vars_hash_lu), c("instnm" = "instnm"))
  ## descriptions
  expect_equal(get_hash(get_hash("11/12-month contract", desc_hash), desc_hash_lu),
               c("11/12-month contract" = "11/12-month contract"))
  ## vector
  expect_equal(get_hash(get_hash(c("HD2020","HD2021"), file_hash), file_hash_lu),
               c("HD2020" = "HD2020", "HD2021" = "HD2021"))
})

## convert_hash_vec
test_that("convert_hash_vec", {
  df <- data.frame(i = 1:11,
                   idxf = get_hash(paste0("HD", 2010:2020), file_hash))
  vec <- convert_hash_vec(df, "idxf")
  expect_equal(unname(vec), paste0("HD", 2010:2020))
})

## convert_hash_df
test_that("convert_hash_df", {
  df <- data.frame("idxf" = get_hash(paste0("HD", 2010:2012), file_hash),
                   "idxv" = get_hash(c("instnm", "instrfte", "instsize"), vars_hash),
                   "idxd" = get_hash(c("11/12-month contract",
                                       "12-month full-time equivalent enrollment",
                                       "12-month instructional activity clock hours: undergraduates"),
                                     desc_hash))
  df_conv <- convert_hash_df(df)
  rownames(df_conv) <- NULL
  df_comp <- data.frame("idxf" = paste0("HD", 2010:2012),
                        "idxv" = c("instnm", "instrfte", "instsize"),
                        "idxd" = c("11/12-month contract",
                                   "12-month full-time equivalent enrollment",
                                   "12-month instructional activity clock hours: undergraduates"))
  expect_equal(df_conv, df_comp)
})

## -------------------------------------
## i/o
## -------------------------------------

test_that("get_file_stub_name", {
  expect_equal(get_file_stub_name("../dir/subdir/file.txt"), "file")
})

## get_internal_file_name
test_that("get_internal_file_name", {
  zf <- file.path(tempdir(), "EF2021A_DIST.zip")
  expect_equal(get_internal_file_name(zf, use_revised = FALSE),
               "ef2021a_dist.csv")
  expect_equal(get_internal_file_name(zf, use_revised = TRUE),
               "ef2021a_dist_rv.csv")
  expect_equal(get_internal_file_name(zf),
               "ef2021a_dist_rv.csv")
})

## get_file_location_or_download
test_that("get_file_location_or_download", {
  zf <- "HD2021.zip"
  ldir <- file.path("..", "..", "inst", "extdata")
  expect_equal(get_file_location_or_download(zf), tempdir())
  file.remove(file.path(tempdir(), zf))
  expect_equal(get_file_location_or_download(zf, local_dir = ldir), ldir)
})

## -------------------------------------
## hash helpers
## -------------------------------------

test_that("lower_names_df", {
  df <- data.frame("A" = 1:2,
                   "B" = 3:4,
                   "CDE" = 5:6)
  expect_equal(names(lower_names_df(df)), c("a", "b", "cde"))
})

test_that("bind_rows_df", {
  df_list <- list(data.frame("a" = 1, "b" = 3, "c" = 5),
                  data.frame("a" = 2, "b" = 4, "c" = 6))
  df <- data.frame("a" = 1:2, "b" = 3:4, "c" = 5:6)
  expect_equal(bind_rows_df(df_list), df)
})

test_that("make_distinct", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(make_distinct(df), data.frame("a" = 1, "b" = c(1,2), "c" = 3))
  expect_equal(make_distinct(df, c("a", "c")), data.frame("a" = 1, "b" = 1, "c" = 3))
})

test_that("filter_equals", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(filter_equals(df, "b", 1), data.frame("a" = 1, "b" = 1, "c" = 3))
  expect_equal(filter_equals(df, "b", 2), data.frame("a" = 1, "b" = c(2,2), "c" = 3),
               ignore_attr = TRUE)
  expect_equal(filter_equals(df, "c", 3), df)
})

test_that("filter_in", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(filter_in(df, "b", 1), data.frame("a" = 1, "b" = 1, "c" = 3))
  expect_equal(filter_in(df, "b", 2), data.frame("a" = 1, "b" = c(2,2), "c" = 3),
               ignore_attr = TRUE)
  expect_equal(filter_in(df, "c", 3), df)
})

test_that("filter_rows", {
  df <- data.frame("unitid" = rep(1:5, each = 10), "year" = 2010:2019, "c" = letters[1:10])
  fdf <- data.frame("unitid" = c(1,1,5,5), "year" = c(2010,2012,2010,2012))
  expect_equal(filter_rows(df, fdf), data.frame("unitid" = c(1,1,5,5),
                                                "year" = c(2010,2012,2010,2012),
                                                "c" = c("a","c","a","c")))
})

test_that("cyear_to_ayear", {
  expect_equal(cyear_to_ayear(2020), "2021")
  expect_equal(cyear_to_ayear(1999), "9900")
})

## order_vars
test_that("order_vars", {
  expect_equal(order_vars("a,b,c"), c("a", "b", "c"))
  expect_equal(order_vars("a;b;c", ";"), c("a", "b", "c"))
})

## roll_join_full
test_that("roll_join_full", {
  ## perfect alignment
  x <- data.frame(a = 1:3,
                  b = 4:6,
                  c = 7:9)
  y <- data.frame(a = 1:3,
                  b = 4:6,
                  d = 10:12)
  z <- data.frame(a = 1:3,
                  b = 4:6,
                  e = 13:15)
  df <- data.frame(a = 1:3,
                   b = 4:6,
                   c = 7:9,
                   d = 10:12,
                   e = 13:15)
  expect_equal(roll_join_full(list(x,y,z), c("a","b")), df)
  ## misalignment creating some NAs
  z <- data.frame(a = 1:3,
                  b = c(5,5,6),
                  e = 13:15)
  df <- data.frame(a = c(1,1,2,3),
                   b = c(4,5,5,6),
                   c = c(7,NA,8,9),
                   d = c(10,NA,11,12),
                   e = c(NA,13,14,15))
  expect_equal(roll_join_full(list(x,y,z), c("a","b")), df)
})

## get_vars_from_file
test_that("get_vars_from_file", {
  vars <- get_vars_from_file("HD2020", ipeds_dict(".", return_dict = TRUE, print_off = TRUE))
  comp <- c("unitid", "instnm", "ialias", "addr", "city", "stabbr", "zip",
            "fips", "obereg", "chfnm", "chftitle", "gentele", "ein", "duns",
            "opeid", "opeflag", "webaddr", "adminurl", "faidurl", "applurl",
            "npricurl", "veturl", "athurl", "disaurl", "sector", "iclevel",
            "control", "hloffer", "ugoffer", "groffer", "hdegofr1", "deggrant",
            "hbcu", "hospital", "medical", "tribal", "locale", "openpubl",
            "act", "newid", "deathyr", "closedat", "cyactive", "postsec",
            "pseflag", "pset4flg", "rptmth", "instcat", "c18basic", "c18ipug",
            "c18ipgrd", "c18ugprf", "c18enprf", "c18szset", "c15basic",
            "ccbasic", "carnegie", "landgrnt", "instsize", "f1systyp",
            "f1sysnam", "f1syscod", "cbsa", "cbsatype", "csa", "necta",
            "countycd", "countynm", "cngdstcd", "longitud", "latitude",
            "dfrcgid", "dfrcuscg")
  expect_equal(vars, comp)
})

## get_file_year
test_that("get_file_year", {
  expect_equal(get_file_year("HD2020"), 2020)
  expect_equal(get_file_year("SFA1718"), 2018)
})

## subset_file_table_by_year
test_that("subset_file_table_by_year", {
  comp <- c("ADM2020", "AL2020", "C2020_A", "C2020_B", "C2020_C", "C2020DEP",
            "EAP2020", "EF2020A", "EF2020A_DIST", "EF2020B", "EF2020C",
            "EF2020CP", "EF2020D", "EFFY2020", "EFFY2020_DIST", "EFIA2020",
            "F1920_F1A", "F1920_F2", "F1920_F3", "FLAGS2020", "GR200_20",
            "GR2020", "GR2020_L2", "GR2020_PELL_SSL", "HD2020", "IC2020",
            "IC2020_AY", "IC2020_PY", "OM2020", "S2020_IS", "S2020_NH",
            "S2020_OC", "S2020_SIS", "SAL2020_IS", "SAL2020_NIS", "SFA1920",
            "SFAV1920") |> sort()
  expect_equal(subset_file_table_by_year(2020), comp)
})

## subset_dict_by_var_year
test_that("subset_dict_by_var_year", {
  comp <- data.frame(filename = "HD2020",
                     varname = "instnm",
                     description = "Institution (entity) name",
                     long = FALSE)
  expect_equal(subset_dict_by_var_year("instnm", "instnm", "HD2020"), comp)
})

## -------------------------------------
## package structure helpers
## -------------------------------------

## confirm_chain
test_that("confirm_chain", {
  good_chain <- ipeds_init()
  expect_equal(confirm_chain(good_chain), good_chain)
  expect_error(confirm_chain(a),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

## confirm_vars
test_that("confirm_vars", {
  expect_equal(confirm_vars("unitid"), list(NULL))
  expect_error(confirm_vars("x"),
               label = paste0("Variable \"x\" not found in dictionary. ",
                              "Please check your spelling or search dictionary: ",
                              "?ipeds_dict()"))
})
