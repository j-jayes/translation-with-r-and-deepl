# Translation With R and DeepL

## User Guide

### Overview

This R script is designed to:

1. Read a text file and process its content into chunks of text.
2. Translate each chunk of text using the DeepL API.
3. Cache translations to avoid redundant API calls.
4. Optionally, process a subset of rows for testing purposes.
5. Print translated text to the console (optional).
6. Save the translated text along with the original text and character count to an Excel file.

### Dependencies

The script relies on the following R libraries:

- `httr`
- `jsonlite`
- `dplyr`
- `writexl`
- `data.table`
- `stringi`
- `tidyverse`

Ensure you have these libraries installed using the `install.packages()` function before running the script.

### Functions

#### `translate_text(text, api_key, cache_file)`

- **Parameters**:
  - `text` (string): The text to translate.
  - `api_key` (string): Your DeepL API key.
  - `cache_file` (string): Path to the JSON cache file.
- **Behavior**: Translates a given text using the DeepL API, with caching to avoid redundant translations.
- **Return Value**: The translated text.

#### `process_and_translate_text(file_path, api_key, output_file_path, cache_file = "translation_cache.json", n_rows = NULL, print_output = TRUE)`

- **Parameters**:
  - `file_path` (string): Path to the input text file.
  - `api_key` (string): Your DeepL API key.
  - `output_file_path` (string): Path to save the output Excel file.
  - `cache_file` (string, optional): Path to the JSON cache file. Default is "translation_cache.json".
  - `n_rows` (integer, optional): Number of rows to process for testing. Default is NULL (process all rows).
  - `print_output` (boolean, optional): Whether to print translated text to the console. Default is TRUE.
- **Behavior**: Processes and translates text from the input file, and saves the results to an Excel file.
- **Return Value**: A tibble with the original and translated text.

### Usage

Load the necessary libraries and define your DeepL API key:

```r
library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(data.table)
library(stringi)
library(tidyverse)

api_key <- "your-api-key-here"
```

Call `process_and_translate_text` with the path to your text file, your API key, and the desired output file path:

```r
result <- process_and_translate_text("input.txt", api_key, "output.xlsx", n_rows = 15)
```

In this example, only the first 15 rows will be processed, translated text will be printed to the console, and the results will be saved to "output.xlsx".
