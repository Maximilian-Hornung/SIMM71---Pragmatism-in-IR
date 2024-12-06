#merged dataset
setwd("C:/Users/hornu/OneDrive/Master Social Scientific Data Analysis/Courses/SIMM71 - Computational Content Analysis for the Social Sciences/SIMM71---Pragmatism-in-IR/sampled speeches")

##Council of Europe 

library(httr)
library(rvest)
library(stringr)
library(textcat) #for detecting non english text
library(openxlsx)
#access URL
url <- "https://www.coe.int/en/web/secretary-general/speeches-and-op-eds?p_p_id=com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_redirect=%2Fen%2Fweb%2Fsecretary-general%2Fspeeches-and-op-eds%3Fp_p_id%3Dcom_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_redirect%3D%252Fen%252Fweb%252Fsecretary-general%252Fspeeches-and-op-eds%26_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_delta%3D20%26p_r_p_resetCur%3Dfalse%26_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_cur%3D5&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_delta=20&p_r_p_resetCur=false&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_cur=2#p_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW"

website <- read_html(url)


##I use the same link structure for all pages including the first page!
#https://www.coe.int/en/web/secretary-general/speeches-and-op-eds?p_p_id=com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_redirect=%2Fen%2Fweb%2Fsecretary-general%2Fspeeches-and-op-eds&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_delta=20&p_r_p_resetCur=false&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_cur=2#p_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbWalso works.


beg_url <- "https://www.coe.int/en/web/secretary-general/speeches-and-op-eds?p_p_id=com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_redirect=%2Fen%2Fweb%2Fsecretary-general%2Fspeeches-and-op-eds%3Fp_p_id%3Dcom_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_redirect%3D%252Fen%252Fweb%252Fsecretary-general%252Fspeeches-and-op-eds%26_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_delta%3D20%26p_r_p_resetCur%3Dfalse%26_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_cur%3D5&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_delta=20&p_r_p_resetCur=false&_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW_cur="


end_url <- "#p_com_liferay_asset_publisher_web_portlet_AssetPublisherPortlet_INSTANCE_JMmT2OuzAtbW"


##First  create a vector of numbers 1-11. 
custom_pages <- c(1:11)

##create an empty list: 
empty_list <- list()


##Then, let's write a for loop for R to create links for each number in the vector.

for (i in custom_pages) { #Here we are saying for each item in the vector we created with numbers 1-11
  links <- print(paste0(beg_url, i, end_url))##Then, paste 'i' (the number) in between beg_url and end_url
  empty_list <- rbind(empty_list, links) } #It's empty. Let's fill it by telling R to save each link at the end of each iteration of the for loop...


empty_list

list_of_links <- empty_list

list_of_dfs <- list()


#loop...

for (i in list_of_links[1:11]){ ##I am only doing all 11 pages here...
  
  url <- i ##Just for clarity, I am renaming 'i' to be a url
  website <- read_html(i) ##Remember to read the html from each page! Then we can get the title, price, etc. 
  
  
  # Pulling the titles
  titles <- website %>%
    html_elements("h3 a") %>%
    html_text2()
  
  
  # Pulling the link
  link <- website %>% 
    html_elements("h3 a") %>% 
    html_attr("href")
  
  
  #Because the location and date information is not consistently present above the title on all pages in the loop: let's apply the lapply() function to test every single element for the presence of date and location.
  elements <- website %>%
    html_elements("div.element.clearfix")
  
  data <- lapply(elements, function(element) {
    
    # Extract date
    date_element <- element %>% html_element("span.date")
    date <- ifelse(is.null(date_element), NA, date_element %>% html_text())
    
    # Extract location
    location_element <- element %>% html_element("span.location")
    location <- ifelse(is.null(location_element), NA, location_element %>% html_text())
    
    # Check if date is NA and location is not NA
    if (is.na(date) && !is.na(location)) {
      
      # Try to detect if the text contains a date using regex
      if (grepl("\\b\\d{1,4}\\b", location)) {
        date <- location
        location <- NA
      }
    }
    # Check for additional information in date and move it to location if there is NA
    if (is.na(location) && !is.na(date)) {
      additional_info <- str_match(date, "\\((.*?)\\)")
      if (!is.null(additional_info) && !is.na(additional_info[2])) {
        location <- additional_info[2]
        date <- gsub("\\(.*?\\)", "", date)
      }
    }
    
    
    return(data.frame(date = date, location = location, stringsAsFactors = FALSE))
  })
  
  result <- do.call(rbind, data)
  
  
  # Create a data frame with title, date, and location
  page_df <- data.frame(
    title = titles,
    link = link,
    date = result$date,
    location = result$location,
    stringsAsFactors = FALSE
  )
  
  list_of_dfs <- rbind(list_of_dfs, page_df)
  
  print(paste0("Finished collecting from link ", i)) ##I have added a short message here to print in the console, so we know it's running
}

###inspecting data
## manual data cleaning // swap the dates with the location in row 76 and 81 because they were already swaped on the webpage before scraping


# Swap date and location in row 81
list_of_dfs[81, c("date", "location")] <- list_of_dfs[81, c("location", "date")]

# Swap date and location in row 76
list_of_dfs[76, c("date", "location")] <- list_of_dfs[76, c("location", "date")]




# Set a seed for reproducibility
#set.seed(111) 

# Randomly sample 10 speeches from the scraped data
sampled_links <- list_of_dfs$link#[sample(nrow(list_of_dfs), 10)]   ##I'll sampe after collecting the data

# Create a list to store scraped texts
sampled_speeches <- list()

for (i in sampled_links) {  
  # Access the URL for the speech
  url <- i
  website <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message(paste("Error accessing URL:", url))
    return(NULL)
  })
  
  if (!is.null(website)) {
    # Extract the text of the speech
    text <- website %>%
      html_elements("div.text") %>% 
      html_elements("p") %>% 
      html_text2()
    
    if (length(text) == 0) {
      # Alternative structure for some pages
      text <- website %>%
        html_elements("div.content") %>% 
        html_elements("p") %>% 
        html_text2()
    }
    
    # Collapse text into a single cohesive string
    cohesive_text <- paste(text, collapse = " ")
    
    # Store the result
    speech_data <- data.frame(
      link = url,
      cohesive_text = cohesive_text,
      stringsAsFactors = FALSE
    )
    sampled_speeches <- rbind(sampled_speeches, speech_data)
    
    print(paste("Scraped text for:", url))
  }
}

# Extract the first part of the text (before any non-English section)
sampled_speeches$first_part <- str_extract(sampled_speeches$cohesive_text, "^[^*]+")

# Detect language for the first part of the speech
sampled_speeches$language <- textcat(sampled_speeches$first_part)

# Filter for only English texts
sampled_speeches <- sampled_speeches[sampled_speeches$language == "english", ]

# Function to keep only English parts of the text (if there are mixed languages)
keep_english_parts <- function(text) {
  # Split text into sentences
  sentences <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  
  # Detect language of each sentence
  sentence_languages <- textcat(sentences)
  
  # Keep only sentences detected as English
  english_sentences <- sentences[sentence_languages == "english"]
  
  # Combine the English sentences back into a cohesive text
  paste(english_sentences, collapse = " ")
}

# Apply the function to remove non-English parts of cohesive_text
sampled_speeches$english_only_text <- sapply(sampled_speeches$cohesive_text, keep_english_parts)

# Check if the English-only text column is non-empty and filter them out
sampled_speeches <- sampled_speeches[!sampled_speeches$english_only_text == "", ]

# Merge both dataframes (list_of_dfs with sampled_speeches) based on the link
merged_df <- merge(list_of_dfs, sampled_speeches, by = "link")


#now sample for 10 speeches 
set.seed(123) 
sampled_speeches <- merged_df[sample(nrow(merged_df), 10), ]

#remove column 5 and 6
sampled_speeches <- sampled_speeches[, -(5:6)]

# Change column name of 'english_only_text' to 'text'
colnames(sampled_speeches)[colnames(sampled_speeches) == "english_only_text"] <- "text"

# Apply gsub() to each element in the data frame to remove all tabs
sampled_speeches[] <- lapply(sampled_speeches, function(x) gsub("\t", "", x))



# Save the sampled_speeches data frame as a TSV file
write.csv(sampled_speeches, "sampled_Speeches_CoE.csv", row.names = FALSE)
sampled_speeches_loaded <- read.csv("sampled_Speeches_CoE.csv", stringsAsFactors = FALSE)
