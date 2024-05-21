RSelenium Data Collection
================

The captions were collected from [Downsub](https://downsub.com/) by
using the RSelenium package. For those looking for a tutorial on
webscraping with this package, you can find one
[here](https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html).
There are several tools needed to get this specific script to work:

1.  Chromium Browser for
    [Automation](https://chromedriver.storage.googleapis.com/index.html?path=114.0.5735.90/).
2.  Chrome WebDriver for
    [RSelenium](https://chromedriver.chromium.org/downloads).
3.  Adblock needed for blocking
    [popups](https://chromewebstore.google.com/detail/adblocker-ultimate/ohahllgiabjaoigichmmfljhkcfikeof?hl=en&pli=1).
4.  Java must be installed and the PATH variable must be updated with
    its location.

This demo script only goes through five examples, this can be adjusted
by: ‘stop_row’. Each scrape takes three seconds, with 48,000 videos this
should take nearly two complete days.

``` r
library(RSelenium) # R Scraping
library(tidyverse) # Data Manipulation
library(netstat) # Network Statistics
library(base64enc) # Base64 Encoding
library(wdman) # Webdriver Manager
```

``` r
download_directory <- "D:\\files" # This is where the txt files are downloaded
chrome_path <- "INSERT_FILE_PATH" # This is the path to chromium
filepath <- "Chromium/User Data/Default/Extensions/ohahllgiabjaoigichmmfljhkcfikeof/3_8_7_0.crx" # Path to Extension
extension_base64 <- base64encode(filepath) # Encoded file path
```

Creating DF

``` r
new_vids <- read.csv("videos_complete.csv") # Loading in Videos
new_vids2 <- new_vids %>%
  mutate(URL = paste0("https://www.subtitle.to/youtube.com/watch?v=", videoId))
threshold_date <- as.Date("2017-01-03")
videos_after_threshold <- new_vids2[new_vids2$publishedAt >= threshold_date, ]
download_status <- data.frame(
  URL = videos_after_threshold$URL,
  videoId = videos_after_threshold$videoId,
  Status = NA,
  title = videos_after_threshold$videoTitle,
  file_name = NA
)
download_status$file_name <- paste0("[English (auto-generated)] ", download_status$title, " [DownSub.com].txt")
download_status$file_name <- gsub(":", "_", download_status$file_name)
```

``` r
start_row <- 1
stop_row <- 5
save_interval <- 2000
downloaded_count <- 0
no_captions_count <- 0
```

Setting up RSelenium

``` r
cprof <- list(
  chromeOptions = list(
    prefs = list(
      "profile.default_content_settings.popups" = 0,  # Block pop-ups
      "download.default_directory" = download_directory),
    extensions = list(extension_base64),
    binary = chrome_path  # Specify the path to the Chrome binary here
  )
)

driver <- rsDriver(browser = "chrome", chromever = "114.0.5735.90", 
                   extraCapabilities = cprof, verbose = FALSE, 
                   port = free_port())
remDr <- driver[["client"]]
```

Loop that starts the process

``` r
for (i in start_row:min(stop_row, nrow(download_status))) {
  if (!is.na(download_status$Status[i])) {
    next
  }
  url <- download_status$URL[i]
  remDr$navigate(url)
  
  Sys.sleep(3)
  
  selector <- "#app > div > main > div > div.container.ds-info.outlined > div > div.row.no-gutters > div.pr-1.col-sm-7.col-md-6.col-12 > div.flex.mt-5.text-center > div.layout.justify-start.align-center > button:nth-child(2)"
  
  button <- tryCatch({
    remDr$findElement(using = "css selector", value = selector)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(button)) {
    download_status$Status[i] <- "No Captions"
    no_captions_count <- no_captions_count + 1
  } else {
    button$clickElement()
    Sys.sleep(3)
    download_status$Status[i] <- "Downloaded"
    downloaded_count <- downloaded_count + 1
  }
  
  # Print progress
  cat(sprintf("Processed %d rows. Downloaded: %d, No Captions: %d\n", i, downloaded_count, no_captions_count))
  
  if (i %% save_interval == 0) {
    saveRDS(download_status, paste0("download_status_", i, ".rds"))
  }
}
```

    ## Processed 1 rows. Downloaded: 1, No Captions: 0
    ## Processed 2 rows. Downloaded: 2, No Captions: 0
    ## Processed 3 rows. Downloaded: 3, No Captions: 0
    ## Processed 4 rows. Downloaded: 4, No Captions: 0
    ## Processed 5 rows. Downloaded: 5, No Captions: 0

Closing the RSelenium process

``` r
remDr$close()
driver$server$stop()
```

    ## [1] TRUE
