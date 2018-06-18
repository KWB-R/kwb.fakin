#' Wrapper function for preparing R package with KWB styling
#' @param author author information in list format (default:
#' list(name = "Michael Rustler", orcid = "0000-0003-0647-7726",
#' url = "http://mrustl.de"))
#' @param pkg package description in list format (default:\cr
#' pkg = list(name = "kwb.lca",\cr
#' title = "R package supporting UMERTO LCA at KWB",\cr
#' desc = "Helper functions for data aggregation and visualisation of\cr
#' UMBERTO (https://www.ifu.com/umberto) model output"))
#' @param version user defined version number (e.g. 0.1.0) or default version
#' number 0.0.0.9000 in case (version = NULL) of first release
#' @param license license (default: "MIT + file LICENSE")
#' @param copyright_holder list with name of copyright holder and additional
#' start_year (e.g. "2017") of copyright protection. If not specified
#' (start_year = NULL), the current year is used as year only!
#' (default: "list(name ="Kompetenzzentrum Wasser Berlin gGmbH (KWB)",
#' start_year = NULL)")
#' @param funder funder/funding agengy of R package (default: NULL), e.g. project
#' name (e.g. AQUANES)
#' @return writes DESCRIPTION file using usethis::use_description() with KWB
#' style
#' @importFrom fs dir_create
#' @importFrom stringr str_detect
#' @export
use_pkg <- function(author = list(name = "Michael Rustler",
                                          orcid = "0000-0003-0647-7726",
                                          url = "http://mrustl.de"),
pkg = list(name = "kwb.lca",
title = "R package supporting UMERTO LCA at KWB",
desc = paste0("Helper functions for data aggregation and visualisation",
" of UMBERTO (https://www.ifu.com/umberto/) model output.")),
version = NULL,
license = "MIT + file LICENSE",
copyright_holder = list(name ="Kompetenzzentrum Wasser Berlin gGmbH (KWB)",
                        start_year = NULL),
                            funder = NULL) {


### 1) Create DESCRIPTION file
use_description(author,
                pkg,
                version,
                license,
                copyright_holder_name = copyright_holder$name)



### 2) Create MIT LICENSE file
mit_licence <- stringr::str_detect(string = license, pattern = "MIT")

if(mit_licence) {
use_mit_license(copyright_holder)
}


### 3) Create PKGDOWN YML
use_pkgdown(author, copyright_holder$name)

### 4) Create .gitlab-ci.yml
fs::dir_create("docs")
use_gitlab_ci()

### 5) Create .travis.yml
use_travis()

### 6) Create appveyor.yml
use_appveyor()

### 7) Use codecov
use_codecov()
}

