# Translation in R
Jonthan Jayes
2023-10-26

## Purpose

Translation for Jakob.

### Structure

We will use the [DeepL](https://www.deepl.com/docs-api) API. You need to
sign up to get an API key. You get 500,000 characters per month for
free. The API is very simple. You send a POST request to the API with
your text, and it returns the translated text.

We need to read in the text, split it into sentences, translate the
sentences, put them back together, and save the translated text.

We will save the file as an Excel file, with one row per sentence. Excel
is good at dealing with Swedish letters, e.g. å, ä, ö.

### Read in the text

``` r
library(data.table)
```

    Warning: package 'data.table' was built under R version 4.2.2

``` r
library(stringi)
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.2.3

    Warning: package 'ggplot2' was built under R version 4.2.3

    Warning: package 'tidyr' was built under R version 4.2.2

    Warning: package 'purrr' was built under R version 4.2.2

    Warning: package 'stringr' was built under R version 4.2.2

    Warning: package 'forcats' was built under R version 4.2.3

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.0.10     ✔ readr     2.1.3 
    ✔ forcats   1.0.0      ✔ stringr   1.5.0 
    ✔ ggplot2   3.4.3      ✔ tibble    3.1.8 
    ✔ lubridate 1.8.0      ✔ tidyr     1.3.0 
    ✔ purrr     1.0.1      
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::between()     masks data.table::between()
    ✖ dplyr::filter()      masks stats::filter()
    ✖ dplyr::first()       masks data.table::first()
    ✖ lubridate::hour()    masks data.table::hour()
    ✖ lubridate::isoweek() masks data.table::isoweek()
    ✖ dplyr::lag()         masks stats::lag()
    ✖ dplyr::last()        masks data.table::last()
    ✖ lubridate::mday()    masks data.table::mday()
    ✖ lubridate::minute()  masks data.table::minute()
    ✖ lubridate::month()   masks data.table::month()
    ✖ lubridate::quarter() masks data.table::quarter()
    ✖ lubridate::second()  masks data.table::second()
    ✖ purrr::transpose()   masks data.table::transpose()
    ✖ lubridate::wday()    masks data.table::wday()
    ✖ lubridate::week()    masks data.table::week()
    ✖ lubridate::yday()    masks data.table::yday()
    ✖ lubridate::year()    masks data.table::year()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
process_text <- function(file_path, chunk_size = 1000) {
  # Validate input
  if (!file.exists(file_path)) {
    stop("The file path provided does not exist.")
  }
  
  if (!is.numeric(chunk_size) || chunk_size <= 0) {
    stop("The chunk size must be a positive number.")
  }
  
  message("Reading the text file...")
  raw_text <- fread(file_path, header = FALSE)
  
  message("Converting to a tibble with one row per line of text...")
  raw_text_tbl <- raw_text %>% 
    as_tibble() %>%
    pivot_longer(everything(), names_to = "line", values_to = "line_text")
  
  message("Combining the text into a single string...")
  whole_text_str <- paste(raw_text_tbl$line_text, collapse = " ")
  
  message("Splitting text into chunks...")
  split_text <- function(text, chunk_size) {
    sentences <- unlist(stri_split_boundaries(text, type = "sentence"))
    chunks <- vector("list", length(sentences))
  
    chunk_text <- ""
    chunk_count <- 1
    
    for (sentence in sentences) {
      if (nchar(chunk_text) + nchar(sentence) <= chunk_size) {
        chunk_text <- paste0(chunk_text, sentence)
      } else {
        chunks[[chunk_count]] <- chunk_text
        chunk_count <- chunk_count + 1
        chunk_text <- sentence
      }
    }
    
    chunks[[chunk_count]] <- chunk_text # Add the last chunk
    chunks <- chunks[1:chunk_count] # Remove empty elements
    
    return(chunks)
  }
  
  text_chunks <- split_text(whole_text_str, chunk_size)
  
  message("Converting to a tibble...")
  text_tbl <- tibble(line = seq_along(text_chunks), text = text_chunks)
  
  message("Adding a column to show the character count for each chunk...")
  text_tbl <- text_tbl %>%
    unnest(text) %>%
    mutate(char_count = str_count(text))
  
  message("Processing complete.")
  return(text_tbl)
}

# Example Usage:
# result <- process_text(here::here("data", "emigrationsutredningen.txt"))
```

### Translation function

``` r
library(httr)
library(jsonlite)
```


    Attaching package: 'jsonlite'

    The following object is masked from 'package:purrr':

        flatten

``` r
library(dplyr)
library(writexl)
```

    Warning: package 'writexl' was built under R version 4.2.3

``` r
# Define function to interact with DeepL API
translate_text <- function(text, api_key, cache_file, wait_time = 0.5) {
  # Load existing cache
  if (file.exists(cache_file)) {
    cache <- fromJSON(cache_file)
  } else {
    cache <- list()
  }
  
  # Check cache for existing translation
  if (!is.null(cache[[text]])) {
    return(cache[[text]])
  }
  
  url <- "https://api-free.deepl.com/v2/translate"
  headers <- add_headers(
    `Authorization` = paste0("DeepL-Auth-Key ", api_key),
    `Content-Type` = "application/json"
  )
  
  payload <- list(
    text = list(text),
    target_lang = "EN"
  )
  json_payload <- toJSON(payload, auto_unbox = TRUE)
  
  response <- POST(url, headers, body = json_payload, encode = "json")
  
  if (http_error(response)) {
    stop("Failed to translate text: ", content(response, "text", encoding = "UTF-8"))
  }
  
  parsed_response <- content(response, as = "parsed")
  
  translated_text <- parsed_response$translations[[1]]$text

  # After successful translation, print translated text
  print(translated_text)
  
  # Update cache and save to file
  cache[[text]] <- translated_text
  write(toJSON(cache, auto_unbox = TRUE), cache_file)

  # Wait before sending next request
  Sys.sleep(wait_time)
  
  return(translated_text)
}

# Function to process and translate text
process_and_translate_text <- function(file_path, api_key, output_file_path, 
                                       cache_file = here::here("data", "cache", "translation_cache.json"), 
                                       n_rows = NULL, print_output = TRUE) {
  text_tbl <- process_text(file_path)  # Assuming process_text is defined as before
  
  # Subset the tibble if n_rows is provided
  if (!is.null(n_rows)) {
    text_tbl <- slice(text_tbl, 1:n_rows)
  }
  
  message("Translating text...")
  translated_tbl <- text_tbl %>%
    rowwise() %>%
    mutate(translated_text = {
      translated <- translate_text(text, api_key, cache_file)
      if (print_output) {
        cat(translated, "\n")
      }
      translated
    })

  message("Dropping the character count column...")
  translated_tbl <- translated_tbl %>%
    select(-char_count)
  
  message("Saving to Excel file...")
  write_xlsx(translated_tbl, output_file_path)
  
  message("Processing and translation complete.")
  return(translated_tbl)
}


# Define your API key
api_key <- "Your API key goes here"
# I am using an environment variable to store my API key so that it is not pushed to GitHub.

# Set file paths
file_name = "emigrationsutredningen.txt"
file_name_processed = "emigrationsutredningen_processed.xlsx"

# Run the function
result <- process_and_translate_text(here::here("data", "raw", file_name), api_key, here::here("data", "processed", file_name_processed), n_rows = 15)
```

    Reading the text file...

    Converting to a tibble with one row per line of text...

    Combining the text into a single string...

    Splitting text into chunks...

    Converting to a tibble...

    Adding a column to show the character count for each chunk...

    Processing complete.

    Translating text...

    ANNEX VII. emigrants' tasks. agents to collect information on male emigrants, since there was a prospect of obtaining the assistance of a female agent for similar work among female emigrants. The information obtained through Mr. Bagge's and Mr. Tkörnberg's trips seems to be of unquestionable value to the Emigration Commission. However, since the cost of the trips turned out to be quite considerable - in comparison with the number of emigrants who could be interviewed during the trip - the Emigration Commission did not consider it appropriate to continue the program in this respect. The questioning of the female emigrants was also, for the sake of economy, organized somewhat differently than for the men. As it is known that at least some of the emigrants stay for some time in Liverpool during the journey, it was decided that the female agent should stay in Liverpool for a month and try to obtain the necessary information. 
    For the performance of this task, the Emigration Commission succeeded in obtaining the housing inspector in Stockholm, Ms. Kerstin Hesselgren, after the Stockholm Health Board, at her request, willingly granted her the necessary leave of absence from her position in the city's service. Mr. Bagge, Mr. Thörnberg and Miss Hesselgren have submitted the following reports to the Emigration Inquiry. Travel report by Fil. Candidate Gösta Bagge. On board the White Star Line steamer Cedric, which left Liverpool on April 5, 1907, there were 86 Swedish men emigrating for the first time. Of these, 50 were questioned. In none of the cases I have examined have political and religious conditions in any way been the cause of emigration. 
    That these factors generally play a comparatively minor role in emigration was further confirmed by statements from the Swedish emigrant missionaries on Ellis Island in New York, who more than anyone else should be considered to have personal knowledge of the conditions and mindset of the Swedish emigrants and the reasons for their decision to emigrate. Pastor C. Samuelson, the Methodist Church's emigrant missionary in New York, who, according to his own statement, worked for more than 30 years as an emigrant missionary and director of the Scandinavian Immigrant Home in New York and as such also participated in the work of this institution's employment agency, stated that throughout his activities he had not come into contact with any Swedish emigrant who had emigrated for political or religious reasons. However, economic reasons seem to have led to emigration in a number of cases. 
    Most of the emigrants with whom I came into contact were the sons of homesteaders, either sons of homesteaders or those who had already left their father's farm and for some time worked as farmhands, craftsmen or industrial workers. As a reason for their emigration they stated that there was no room for them on the farm. It is noteworthy that in most cases, according to them, home farming was successful and the farms were debt-free, but too small to provide work and sustenance for all the siblings. All of these emigrants' future plans were based on returning after a few years in America with saved assets large enough to buy their own farm in Sweden. A recurring complaint was the difficulty of saving enough in Sweden to raise their economic and social status. 
    Difficulty in obtaining permanent work also seems to have been a contributory cause of emigration in some cases; in particular, emigrants who had been employed as farmhands on Öland complained about the short and irregular period of employment, which followed the introduction of agricultural machinery, and the lost season in the sawmill industry and also the closure of sawmills were in several cases driving causes of emigration. The reason for emigration was similar in one case, where the emigrant, who had enjoyed comparatively high wages at Kronan's harbor construction in Böda on Öland, after this source of income ended, could not obtain any equally profitable work, but instead emigrated. Among other economic reasons for emigration, high taxes and conscription were often highlighted. As far as the latter is concerned, repetition exercises seem to be the most troublesome, due to the difficulty of obtaining a permanent place during the conscription years. 
    Whether conscription was a direct cause of emigration, however, is difficult to determine. My personal impression was that many took the opportunity to emigrate before the age of conscription, but that the actual cause lay elsewhere. For the emigration of the few industrial workers who came with them, only in two cases could any definite economic reason be established. In both cases the reason was difficulty in obtaining technical training. Of decisive importance is the influence of relatives and acquaintances who had already emigrated to America. None of the interviewees did not have family and friends where he intended to go. The feeling of affinity with America, the tradition of traveling to America as soon as the course of everyday life is interrupted in some way, are factors that cannot be given enough importance. Several of the respondents had received help with the ticket or had the entire ticket paid for by relatives in America. 
    The fact that most of these helpful relatives were farmers is not a coincidence, as the demand for agricultural workers in the western states greatly exceeds the supply and a paid emigrant ticket can be considered a very cheap and good means of obtaining reliable and good labor. The demand for Scandinavian laborers in the United States seems to be practically unlimited, according to the information I have received from private and public employment agencies, from trade union leaders and large businessmen in America.1 The contact of the inquirers with the emigrant agents seems in most cases to consist in the fact that, after they have obtained knowledge of the agents of the various lines through advertisements, they have written to them and have been sent information and brochures concerning the amenities on board the various emigrant ships. These pamphlets contain, if not always outright lies, at least highly exaggerated descriptions. 
    In one case, the inquirer received a ticket through the agent's representative, who was the inquirer's employer. In other cases the tickets were obtained by returning emigrants who were agents of the agent of the White Star Line in Christiania. According to one such agent - he was himself on board the Cedric - the agent had come to him when he landed in 1 This was written in May 1907, thus before the downturn in the economy had begun to take effect. 10 EMIGRATION INVESTIGATION. ANNEX VII. EMIGRANTS' DUTIES Kristiania, and offered him 5 kr. for each emigrant he brought with him when he returned to America. According to him, this was a widely used method. That it is effective should be evident from the fact that all emigrants from his homeland received a ticket through the Kristiania agent. Surprisingly large was the number of cases where the emigration was caused by purely temporary little things, or general wanderlust. About 20 % of the cases examined by me can be attributed to this. 
    General wanderlust and purely temporary minor matters seem to have played a major role in most cases. Almost all those I asked stated their intention to return to Sweden sooner or later. Stockholm in May 1907. Gösta Bagge. Testimony of Mr. B. H. Thörnberg. On behalf of the Emigration Inquiry, the undersigned started on April 12, 1907 from Gothenburg with the steamer "Ariosto" (Wilson Line) the journey across the North Sea to Hull and continued on the 16th of the same month from Liverpool the journey across the Atlantic with the Cunard Line steamer "Ivernia" to Boston, where I arrived on April 25. Both the captain of the "Ariosto" and the general agent of the Cunard line in Sweden, the officials of the said line at headquarters in Liverpool, and the officers of the steamer "Ivernia" showed me the greatest courtesy. 
    On board the "Ariosto" there were about 450 second and third class passengers, all of whom, with seven or eight exceptions, were going as first-time emigrants or returning immigrants to the United States and Canada - only about twenty were heading for the latter area. For the voyage with "Ivernia" 404 emigrant passengers were registered from the office in Gothenburg. Of these, however, some had to stop over in Liverpool, whereas several Swedes came via Copenhagen and Esbjerg and were accommodated on the same steamer. With regard to the aforementioned list displayed at the office in Gothenburg, I would like to inform you of the following. Of the 404 emigrants who had thus concluded emigration contracts with the Cunard Line's general agent in Gothenburg, 7 were said to be domiciled in Finland, 2 (Swedish-born) in Africa and 60 in America. Of the remaining 335, some twenty had probably stayed in America before, but had been re-enrolled in the church in Sweden. 
    A not inconsiderable number of families were among the emigrants. 54 of the 335 had not yet reached the age of 16. It should be noted that the list included 93 young people aged between 16 and 21. Of the 404 persons, 158 were women. On the steamer "Ariosto" some of the emigrants had to put up with apparently very unfavorable conditions in the third class, and strong complaints were heard. In general things were better, especially for the Scandinavian emigrants, in the same class on the "Ivernia," but even here many of them undoubtedly had reason for rather strong complaints. When I undertook the inquiries which it was my task to make, as a rule the people I spoke to were willing to meet me. However, they had by no means always correctly understood the meaning of my mission, although I tried to clarify it as clearly as possible. On the contrary, several strange notions prevailed, such as that I was sent to prevent some people from landing on the other side, etc. 
    And I felt very strongly the need for a printed summary of the purpose of my journey. I did not address any questions to the female emigrants, and only exceptionally to the Swedish-Americans. To what extent the answers given to the questions about relatives in America and the economic data correspond to the actual conditions, I naturally do not dare to express a definite opinion. I have the impression, however, that as far as the industrial workers in particular are concerned, they would generally tell the truth. On the other hand, I have a feeling that certain reports about the success of their previously emigrated relatives in America, the sums of money sent home by them, etc., contain several elements of boasting and exaggeration. It was quite difficult to obtain detailed answers to the questions about the place of origin at 15 years. 
    The same was the case with the information concerning occupation and employment and the industries in which the emigrant had worked, when the most precise possible designations of the nature of the occupations were desired. A couple of three of those questioned firmly refused to answer the question about the number of siblings, and I found, especially among farmers' sons, a certain reluctance to give detailed accounts in this area. As to promises or prospects of work and employment, and expected wages, some hesitated to give reliable answers, instructed, as they seemed to be, to beware in this respect of the provision against "contract labor" in the immigration laws of the United States. When asked whether the emigration had been mediated by an agent, I received no answer which clearly indicated that any formal labor activity had been carried on, either on behalf of the steamship or land company, in the regions concerned, and that the business of the emigrants was the result of such activity. 
    On the other hand, I was told that in Ångermanland and other regions a number of pamphlets etc. about Canada are being distributed. The completed forms let us understand, however, that there may be a recruitment activity in America through relatives and friends. It would be of very great interest to learn to what extent among the Swedish colonies or "settlements" in North America the sub-agents of the steamship companies develop such an energy to induce the emigrants to send tickets to relatives and friends at home in Sweden, as some American immigration restrictionists claim is developed in all immigrant colonies. Often the emigrants I interviewed had taken the Cunard line on the recommendation of expatriate relatives and friends. In a few cases they had turned to the line's local agents, such as in Sköfde and Trollhättan. But generally they had entered into immediate contact with the line's general agent. 
    Several of them, after deciding, or in the process of deciding, to emigrate from the country, had, on the strength of advertisements in newspapers, written to the chief agents of several transatlantic lines in Sweden. From these they had received not inconsiderable bundles of prospectuses and brochures with illustrations, illustrating the amenities of third class. Several also expressed in sharp terms their disapproval of the difference between these descriptions and the actual conditions on the "Ivernia" - the latter to their disadvantage! I beg leave to call special attention to some observations I have made concerning the position of the emigrants and the causes of their emigration, which are more or less effective. 1) I do not think I am exaggerating when I say that at least half of the emigrants with whom I conversed brought with them larger sums of money than the 10 dollars prescribed by the legislation of the United States. 

    Dropping the character count column...

    Saving to Excel file...

    Processing and translation complete.
