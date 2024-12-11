#merged dataset
setwd("C:/Users/hornu/OneDrive/Master Social Scientific Data Analysis/Courses/SIMM71 - Computational Content Analysis for the Social Sciences/SIMM71---Pragmatism-in-IR/sampled speeches")
#Putin speeches

library(httr)
library(rvest)
library(stringr)
library(openxlsx)

# Base URL for pagination
base_url <- "http://en.kremlin.ru/events/president/transcripts/speeches/page/"

# Number of pages to scrape
num_pages <- 20  # Adjust as needed

# Custom headers to mimic a browser
custom_headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36"
)

# Introduce delays
random_delay <- function() {
  Sys.sleep(runif(1, 2, 5))  # Random delay between 2 and 5 seconds
}

# Create a list to store results
list_of_dfs <- list()

for (page in 1:num_pages) {
  # Introduce a delay to avoid server blocking
  random_delay()
  
  # Construct the URL
  url <- paste0(base_url, page)
  
  # Make the GET request with custom headers
  response <- GET(url, add_headers(.headers = custom_headers))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the HTML content
    website <- read_html(content(response, as = "text"))
    
    # Extract titles - ensure they are not generic elements like "Categories"
    titles <- website %>%
      html_elements("h3.hentry__title a span.entry-title") %>% 
      html_text2()
    
    # Extract links - ensure they correspond to titles
    links <- website %>%
      html_elements("h3.hentry__title a") %>% 
      html_attr("href") %>%
      paste0("http://en.kremlin.ru", .) # Append base URL for full link
    
    # Extract dates - ensure alignment
    dates <- website %>%
      html_elements("h3.hentry__title span.hentry__meta time.published") %>% 
      html_text2()
    
    # Combine data into a data frame
    page_df <- data.frame(
      title = titles,
      link = links,
      date = dates,
      stringsAsFactors = FALSE
    )
    
    list_of_dfs[[page]] <- page_df
    print(paste0("Finished scraping page ", page))
  } else {
    warning(paste("Failed to fetch page", page, "with status code:", status_code(response)))
  }
}

# Combine all pages into a single data frame
all_speeches <- do.call(rbind, list_of_dfs)

# Randomly sample 10 speeches from the 'all_speeches' dataframe
set.seed(123)  # Set a seed for reproducibility
sampled_speeches <- all_speeches[sample(nrow(all_speeches), 5), ]

# Scrape the full text of speeches
sampled_speeches$raw_text <- NA

for (i in seq_len(nrow(sampled_speeches))) {
  random_delay()  # Add delay to mimic human behavior
  
  try({
    # Access the individual speech link
    speech_url <- sampled_speeches$link[i]
    response <- GET(speech_url, add_headers(.headers = custom_headers))
    
    if (status_code(response) == 200) {
      speech_page <- read_html(content(response, as = "text"))
      
      # Extract all <p> tags (the text content of the speech)
      full_text <- speech_page %>%
        html_elements("p") %>%  # Capture all <p> tags, which contain the speech content
        html_text2() %>%        # Extract the text
        paste(collapse = " ")   # Combine all <p> text into one single string
      
      # Clean up text (remove extra whitespace)
      full_text <- gsub("\\s+", " ", full_text)  # Replace multiple spaces with a single space
      
      sampled_speeches$raw_text[i] <- full_text
      print(paste0("Scraped text from speech ", i))
    } else {
      warning(paste("Failed to fetch speech", i, "with status code:", status_code(response)))
    }
  }, silent = TRUE)
}

## clean speeches
sampled_speeches$raw_text <- gsub(
  "All content on this site is licensed under Creative Commons Attribution 4.0 International ", 
  "", 
  sampled_speeches$raw_text 
)





# Save the sampled_speeches data frame as a TSV file
write.csv(sampled_speeches, "sampled_Speeches_Russia.csv", row.names = FALSE)
sampled_speeches_loaded <- read.csv("sampled_Speeches_Russia.csv", stringsAsFactors = FALSE)
