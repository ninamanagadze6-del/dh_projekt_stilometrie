install.packages("stylo")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("ggrepel")

library(stylo)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(ggrepel)

input_path <- "scripts_data" 
if (!dir.exists(input_path)) {
  input_path <- "."
}

movie_files <- list.files(path = input_path, pattern = "\\.txt$", full.names = TRUE)
cat("Found", length(movie_files), "files for processing.\n")

dir.create("cleaned_scripts", showWarnings = FALSE)
dir.create("split_parts", showWarnings = FALSE)

for (f in movie_files) {
  raw_text <- readLines(f, warn = FALSE, encoding = "UTF-8")
  
  clean_text <- raw_text[!grepl("^[A-Z\\s]+$", raw_text)] 
  clean_text <- tolower(clean_text)
  clean_text <- gsub("\\(.*?\\)", "", clean_text)       
  clean_text <- gsub("\\[.*?\\]", "", clean_text)       
  clean_text <- gsub("[[:punct:]]", " ", clean_text)    
  clean_text <- str_squish(clean_text)
  clean_text <- clean_text[nzchar(clean_text)]
  
  writeLines(clean_text, file.path("Cleaned_Scripts", basename(f)))
  
  n_lines <- length(clean_text)
  chunk_size <- ceiling(n_lines / 5)
  
  for (i in 1:5) {
    start_idx <- ((i - 1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, n_lines)
    
    if (start_idx <= n_lines) {
      chunk_data <- clean_text[start_idx:end_idx]
      part_name <- paste0(basename(gsub(".txt", "", f)), "_part_", i, ".txt")
      writeLines(chunk_data, file.path("Split_Parts", part_name))
    }
  }
}

stylo(gui = TRUE, corpus.dir = "split_parts")

data("stop_words")
drug_terms <- c("heroin", "cocaine", "pills", "addict", "high", "overdose", "needle")

analysis_results <- list.files("split_parts", full.names = TRUE) %>%
  map_df(~tibble(text = readLines(.x, warn = FALSE), filename = basename(.x))) %>%
  mutate(era = ifelse(grepl("^90s", filename), "1990s", "2010s/2020s")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word %in% drug_terms) %>%
  count(era, word)


ggplot(analysis_results, aes(x = reorder(word, n), y = n, fill = era)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Drug Vocabulary Comparison", x = "Keywords", y = "Frequency")

ggsave("drug_vocabulary_comparison.png", width = 10, height = 7)


stylo_results_ca <- stylo(
  gui = FALSE,               
  corpus.dir = "split_parts", 
  analysis.type = "CA",        
  mfp.min = 100,              
  mfp.max = 100,
  distance.measure = "delta", 
  sampling = "no.sampling",    
  display.on.screen = TRUE,    
  write.pdf.file = TRUE,      
  plot.custom.height = 10,    
  plot.custom.width = 15,     
  plot.font.size = 0.6
) 

stylo_results_pca <- stylo(
  gui = FALSE,
  corpus.dir = "split_parts",
  analysis.type = "PCR",        
  mfp.min = 100,
  mfp.max = 500,                
  distance.measure = "delta",
  display.on.screen = TRUE,
  write.pdf.file = TRUE,
  plot.custom.height = 10,
  plot.custom.width = 15,
  plot.font.size = 0.6
)

pca_300 <- stylo(
  gui = FALSE,
  corpus.dir = "split_parts", 
  analysis.type = "PCR",        
  mfp.min = 300, mfp.max = 300, 
  display.on.screen = TRUE,
  write.pdf.file = TRUE,
  plot.custom.height = 10,
  plot.custom.width = 15,
  plot.font.size = 0.6
)


pca_500 <- stylo(
  gui = FALSE,
  corpus.dir = "split_parts",
  analysis.type = "PCR",        
  mfp.min = 500, mfp.max = 500, 
  display.on.screen = TRUE,
  write.pdf.file = TRUE
)
stylo(
  gui = FALSE, 
  corpus.dir = "split_parts", 
  analysis.type = "PCR", 
  mfp.min = 300, mfp.max = 300, 
  culling = 70,                 
  write.pdf.file = TRUE, 
  custom.graph.title = "PCA_300_MFW"
)


View(as.matrix(stylo_results_ca$distance.table))
