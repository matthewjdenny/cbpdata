download_data_chunk <- function(scraped_data,
                                intermediate_directory,
                                sleep_time,
                                chunk_number,
                                status_interval) {

    start <- Sys.time()

    for (i in 1:nrow(scraped_data)) {
        if (i %% status_interval == 0) {
            cat("Currently working on Bill",i,"of",nrow(scraped_data),
                "\nwith URL:",scraped_data$bill_metadata_url[i],"\n")
        }
        page <- httr::GET(scraped_data$bill_metadata_url[i])
        scraped_data$metadata_xml[i] <- httr::content(page, "text")
        scraped_data$metadata_request_status[i] <- page$status_code

        if (page$status_code != 200) {
            cat("-------- Potental Error ---------",
                "\nPage:",scraped_data$bill_metadata_url[i],
                "\nStatus Code:",page$status_code,"\n")
        }

        page <- httr::GET(scraped_data$bill_text_url[i])
        scraped_data$bill_text_html[i] <- httr::content(page, "text")
        scraped_data$bill_text_request_status[i] <- page$status_code

        if (page$status_code != 200) {
            cat("-------- Potental Error ---------",
                "\nPage:",scraped_data$bill_text_url[i],
                "\nStatus Code:",page$status_code,"\n")
        }

        Sys.sleep(sleep_time)
    }

    # now we save the data:
    filename <- paste(intermediate_directory,"Data_Chunk_",chunk_number,
                      ".RData",sep = "")

    cat("Saving Data Chunk...")
    save(scraped_data, file = filename)

    end <- Sys.time()
    cat("Scraping chunk:",chunk_number,"complete with:\n")
    print(end - start)

}
