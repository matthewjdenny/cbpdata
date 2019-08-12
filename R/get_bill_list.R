#' @title Generate list of Bills to Collect
#' @description Downloads sitemaps for specified year range and generates file
#' lists to be downloaded.
#'
#' @param start_year Defaults to NULL, in which case results are returned for
#' the current calendar year. Can be any number greater than or equal to 1992,
#' in which case the results for all years in that range will be returned. If
#' not NULL, must be of the form `1994` or `2019`
#' @param end_year Defaults to NULL, in which case results are returned for
#' the current calendar year. Can be any number greater than or equal to 1992,
#' in which case the results for all years in that range will be returned. If
#' not NULL, must be of the form `1994` or `2019`
#' @return A list object with `bill_metadata`, `bill_text`, and `year` fields
#' where the first two fields contains a vector of urls and the `year` field
#' contains a vector of years that correspond to the returned URLS. Also
#' contains a `last_modified` field with a vector of dates the relevant pages
#' were last modified.
#' @export
get_bill_list <- function(start_year = NULL,
                          end_year = NULL) {

    # auto generate the start year:
    if (is.null(start_year)) {
        start_year <- as.numeric(stringr::str_split(Sys.Date(),"-")[[1]][1])
    }

    if (!is.numeric(start_year)) {
        (stop("start_year must be numeric, of the form '1994' or similar..."))
    }

    if (is.null(end_year)) {
        # get the current year
        end_year <- as.numeric(stringr::str_split(Sys.Date(),"-")[[1]][1])
    }

    if (!is.numeric(end_year)) {
        (stop("end_year must be numeric, of the form '1994' or similar..."))
    }


    if (start_year > end_year) {
        stop("Cannot provide a start_year in the future...")
    }

    # create the vector of years to traverse:
    years <- start_year:end_year

    # create the vector of sitemap urls:
    urls <- paste("https://www.govinfo.gov/sitemap/BILLS_",
                  years,
                  "_sitemap.xml",
                  sep = "")

    output <- list(bill_metadata = NULL,
                   bill_text = NULL,
                   year = NULL,
                   last_modified = NULL)

    # define functions to extract relevant fields:
    get_url <- function(l) {
        return(stringr::str_replace(l[1],".*/",""))
    }

    get_date_last_modified <- function(l) {
        return(stringr::str_split(l[2],"T")[[1]][1])
    }

    # loop over urls and collect them:
    for (i in 1:length(urls)) {
        cat("Currently working on",years[i],"\n")
        page <- httr::GET(urls[i])
        page_content <- httr::content(page, "text")
        page_list <- XML::xmlToList(page_content)

        bills <- as.character(unlist(lapply(page_list, get_url)))
        last_modified <- as.character(unlist(lapply(page_list, get_date_last_modified)))

        bill_metadata <- paste("https://www.govinfo.gov/metadata/pkg/",
                               bills,
                               "/mods.xml",
                               sep = "")

        bill_text <- paste("https://www.govinfo.gov/content/pkg/",
                           bills,
                           "/html/",
                           bills,
                           ".htm",
                           sep = "")

        # store everything:
        output$bill_metadata <- c(output$bill_metadata,bill_metadata)
        output$bill_text <- c(output$bill_text, bill_text)
        output$year <- c(output$year, rep(years[i],length(bill_metadata)))
        output$last_modified <- c(output$last_modified, last_modified)

        cat("Found",length(bill_metadata), "bill versions for",
            years[i],"\n")
    }

    cat("Found",length(output$bill_text), "total bill versions for the period",
        start_year,"to",end_year,"\n")

    return(output)
}
