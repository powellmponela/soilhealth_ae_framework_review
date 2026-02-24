# Load necessary libraries
library(pdftools)
library(stringr)

# Define the path to your Terms.txt file and PDFs folder
terms_file <- "Terms.txt"
pdf_folder <- "Frameworks"

# Function to load themes and synonyms from the Terms.txt file
load_terms <- function(file_path) {
  source(file_path, local = TRUE)
  if (!exists("themes") || !exists("synonyms")) {
    stop("Error: 'themes' or 'synonyms' not found in the Terms.txt file.")
  }
  list(themes = themes, synonyms = synonyms)
}

# Load the themes and synonyms
terms <- load_terms(terms_file)
themes <- terms$themes
synonyms <- terms$synonyms

# Function to extract proximity of terms ensuring full-term match
extract_terms_within_proximity <- function(text, theme_terms, all_synonyms, proximity = 5) {
  results <- list()
  
  # Tokenize the text and clean it
  tokens <- unlist(strsplit(text, "\\s+"))
  
  for (theme_term in theme_terms) {
    # Find positions of theme terms ensuring full-term match
    theme_positions <- which(tokens == theme_term)
    
    for (theme_name in names(all_synonyms)) {
      for (synonym in all_synonyms[[theme_name]]) {
        # Find positions of synonyms ensuring full-term match
        synonym_positions <- which(tokens == synonym)
        
        for (theme_pos in theme_positions) {
          # Check for proximity of the synonyms to the theme terms
          close_terms <- synonym_positions[abs(synonym_positions - theme_pos) <= proximity]
          
          if (length(close_terms) > 0) {
            # Log the result with theme, synonym, and context
            results[[length(results) + 1]] <- list(
              theme_term = theme_term,
              synonym = synonym,
              theme_name = theme_name,
              context = paste(tokens[max(1, theme_pos - proximity):min(length(tokens), theme_pos + proximity)], collapse = " ")
            )
          }
        }
      }
    }
  }
  return(results)
}

# Function to process each PDF and extract relevant terms for all themes
process_pdfs_for_themes <- function(pdf_folder, all_synonyms) {
  pdf_files <- list.files(pdf_folder, pattern = "*.pdf", full.names = TRUE)
  if (length(pdf_files) == 0) {
    stop("Error: No PDF files found in the specified folder.")
  }
  
  all_results <- list()
  
  for (pdf_file in pdf_files) {
    # Extract text from the PDF
    text <- pdf_text(pdf_file)
    
    # For each page of the PDF, extract terms within proximity for all themes
    for (page_text in text) {
      results <- extract_terms_within_proximity(page_text, unlist(synonyms), all_synonyms)
      
      # Add "Framework" (PDF file name) column to results
      if (length(results) > 0) {
        results <- lapply(results, function(res) {
          res$Framework <- basename(pdf_file)  # Add the PDF name as the Framework
          return(res)
        })
      }
      
      all_results <- c(all_results, results)
    }
  }
  return(all_results)
}

# Run the process
results <- process_pdfs_for_themes(pdf_folder, synonyms)

# Display results
if (length(results) > 0) {
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  write.csv(results_df, file = "all_themes_proximity_results.csv", row.names = FALSE)
  print(head(results_df))  # Show a preview of the results
} else {
  message("No matches found in the PDFs.")
}


#-----------------MERGE----
#install.packages("readxl")
library(readxl)

# Define the path to the Excel file
file_path <- "C:/Users/PMPONELA/OneDrive - CGIAR/CIMMYT/SOIL HEALTH WRITESHOP MALAWI 17-20 September/Reading Materials/50integrate/List of frameworks.xlsx"

# Read the specific sheet "AE Indicators"
ae_indicators_data <- read_excel(file_path, sheet = "AE Indicators")

# View the first few rows of the data
head(ae_indicators_data)

# Optionally, print the structure of the data
str(ae_indicators_data)

# Define the folder path
base_folder <- "C:/Users/PMPONELA/OneDrive - CGIAR/CIMMYT/SOIL HEALTH WRITESHOP MALAWI 17-20 September/Reading Materials/50integrate"

# Define the subfolder path
subfolder_path <- file.path(base_folder, "results")

# Define the CSV file name
csv_file_path <- file.path(subfolder_path, "all_themes_proximity_results.csv")

# Read the CSV file
proximity_results <- read.csv(csv_file_path)

# View the first few rows of the data
head(proximity_results)

# Optionally, print the structure of the data
str(proximity_results)

# Assuming ae_indicators_data and proximity_results are already loaded in R

# Merge the datasets based on the "indicator" column
ae_indicators_data$`indicator` <- ae_indicators_data$`AE indicator`
merged_data <- merge(ae_indicators_data, proximity_results, by = "indicator", all = TRUE)

# View the first few rows of the merged data
head(merged_data)

# Optionally, check the structure of the merged data
str(merged_data)

# Define the output file path
output_file_path <- file.path(base_folder, "results", "merged_ae_indicators_proximity_results.csv")

# Save the merged data as a CSV file
write.csv(merged_data, output_file_path, row.names = FALSE)

# Print a confirmation message
cat("Merged data saved as:", output_file_path)




library(tidyverse)
library(scales)
library(cSEM)
library(pheatmap)
library(RColorBrewer)
library(openxlsx)
library(DiagrammeR)



# Load your original long-format data
data <- read.csv("data/data0526.csv", encoding = "latin1")
# Preview structure
str(data)
data <- data %>% select(-proximity_text)

#--------------------DATA0------------------------------------------------------
# Standardize column names
names(data) <- tolower(names(data))
data <- data %>%
  mutate(
    principle = tolower(trimws(principle)),
    pathway = tolower(trimws(pathway)),
    indicator = tolower(trimws(indicator)),
    pdf_name = tolower(trimws(pdf_name))
  )

to_remove <- c("resource", "matching", "alignment", "health", "soil", "price",  "sustainable", "practices", "knowledge",
               "human", "quality")

#"natural", "diversity", "landscape", "ecosystem", "decision-making", "organizations", "indigenous", "integrated", "diverse" "permaculture","markets", "packaging",

data <- data[!data$indicator %in% to_remove, ]

head(data)

data <- data %>%
  mutate(indicator = case_when(
    indicator == "emissions" ~ "emission",
    indicator == "cover crops" ~ "cover crop",
    indicator == "non-crop plants" ~ "non-crop plant",
    indicator == "shade trees" ~ "shade tree",
    indicator == "natural enemies" ~ "natural enemy",
    indicator == "hedgerows" ~ "hedgerow",
    indicator == "zai pits" ~ "zai pit",
    indicator == "pollinators" ~ "pollinator",
    indicator == "flower strips" ~ "flower strip",
    indicator == "diversified diets" ~ "diversified diet",
    indicator == "seed banks" ~ "seed bank",
    indicator == "insects" ~ "insect",
    indicator == "on-farm trials" ~ "on-farm trial",
    indicator == "exchange visits" ~ "exchange visit",
    indicator == "crop residues" ~ "crop residue",
   TRUE ~ indicator
  ))

data$pdf_name <- ifelse(
  data$pdf_name == "posthumus et al 2023 food systems decision support toolkit.pdf",
                        "posthumus-fsdst",
  data$pdf_name
)
data$pdf_name <- ifelse(
  data$pdf_name == "check and add soil health1.pdf",
  "covind-dus",
  data$pdf_name
)


data$principle <- ifelse(
  data$principle == "land and natural resource governance ",
  "land and nr governance",
  data$principle
)

#------------CHECK INDICATORS PER PRINCIPLE---------------------------------------------------------
indicator_table <- data %>%
  distinct(principle, indicator) %>%
  arrange(principle, indicator)

print(indicator_table$principle)
principles <- unique(data$principle)

indicator_table <- indicator_table %>%
  mutate(principle = principle %>%
           str_replace_all("[\\u00A0\\u2007\\u202F]", " ") %>%  # NBSP, Figure space, Narrow NBSP → space
           str_squish() %>%                                    # collapse + trim
           str_to_lower())
#SOIL HEALTH -------------------------------------------------------------------



soil_ind <- indicator_table %>% filter(tolower(principle) == "soil health")
soil_ind_classed <- soil_ind %>%
  mutate(indicator_norm = str_squish(tolower(indicator))) %>%
  mutate(class = case_when(
    # -------- BIOLOGICAL --------
    str_detect(indicator_norm, paste(c(
      "earthworm|worm\\b", "nematode", "microb", "macro-?organism", "micro-organism activity",
      "mycorrh", "fungi|bacteria", "fauna|flora", "community|diversity",
      "enzyme", "respiration rate|respirat", "biomass c and n|biomass",
      "mineraliz|potentially mineralizable nitrogen",
      "cellulose decomposition|decomposition", "food web",
      "soil proteins", "root pathogen", "weed seed bank"
    ), collapse="|")) ~ "biological",
    
    # -------- CHEMICAL --------
    str_detect(indicator_norm, paste(c(
      "\\bph\\b|soil ph", "electrical conductivity|\\bec\\b",
      "salinit|sodicit|alkalin|acid|exchangeable acidity",
      "cation exchange capacity|\\bcec\\b", "base saturation",
      "active carbon|reactive carbon|total organic carbon|particulate organic matter",
      "organic matter content|organic matter\\b(?! management)", "organic",
      "\\bc/n\\b|carbon and nitrogen", "nitrate nitrogen|ammonium",
      "phosphorus concentration|phosphor", "potassium|\\bk\\b",
      "calcium", "magnesium", "manganese", "iron", "zinc",
      "aluminium|aluminum", "heavy metals", "copper", "soil quality"
    ), collapse="|")) ~ "chemical",
    
    # -------- PHYSICAL --------
    str_detect(indicator_norm, paste(c(
      "aggregate stability|wet and dry aggregate size",
      "structure and macroporosity|structure\\b|macroporosity",
      "bulk density", "penetration resistance", "hardpan",
      "sub-surface hardness|surface hardness|crusting|compaction",
      "porosity|residual porosity", "texture", "soil depth|root depth",
      "infiltration rate|field infiltration",
      "saturated hydraulic conductivity|hydraulic conductivity",
      "erosion rating", "water holding capacity"
    ), collapse="|")) ~ "physical",
    
    # -------- MANAGEMENT (practices) --------
    str_detect(indicator_norm, paste(c(
      "cover crop", "crop residue|residue\\b", "mulch|mulching",
      "manure|compost|composting|vermicomposting", "biochar",
      "contour planting|terrace",
      "minimum till|no-?till|reduced till|tillage",
      "soil cover", "rotation\\b", "hand weeding",
      "agroforestry|permaculture",
      "soil amendment", "forest litter", "organic farming", "band", "nutrient cycling", "weed seed bank"
    ), collapse="|")) ~ "management",
    
    TRUE ~ NA_character_
  )) %>%
  select(principle, indicator, class)









#------INDICATOR FREQUENCY -----------------------------------------------------
for (p in principles) {
  df_sub <- data %>%
    filter(principle == p) %>%
    group_by(pdf_name, indicator) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = count, values_fill = 0)
  if (ncol(df_sub) <= 2) next
  mat <- df_sub %>%
    column_to_rownames("pdf_name") %>%
    as.matrix()
  if (!all(sapply(mat, is.numeric))) next
  # Drop indicators with total frequency = 0
  mat <- mat[, colSums(mat) > 0]
  if (ncol(mat) == 0) next
  file_label <- gsub("[^a-z0-9]+", "_", tolower(p))
  jpeg(paste0("Results/Indicators09_", file_label, ".jpeg"), width = 2000, height = 2000, res = 300)
  pheatmap(
    mat,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
    fontsize_row = 6,
    fontsize_col = 6,
    fontsize = 10
  )
  dev.off()
}

head(data)

#------------- View principle frequencies across the dataset--------------------
principle_table <- data %>%
  count(principle, sort = TRUE)

# Print the table
print(principle_table)

# Count the frequency of each indicator per pdf_name
indicator_matrix <- data %>%
  group_by(pdf_name, indicator) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = indicator,
    values_from = count,
    values_fill = 0
  )

# View the wide-format matrix
head(indicator_matrix)




#-------------SAVE EXCEL--------------------------------------------------------
# Step 2: Create full hierarchical column label
data <- data %>%
  group_by(pdf_name, pathway, principle, indicator) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(column_label = paste(pathway, principle, indicator, sep = " | "))
head(data)

# Step 3: Reshape to wide format
wide_data <- data %>%
  select(pdf_name, column_label, count) %>%
  pivot_wider(names_from = column_label, values_from = count, values_fill = 0)
head(wide_data)
# Step 4: Split column names for multi-header
header_parts <- str_split_fixed(names(wide_data)[-1], " \\| ", 3)
header_df <- as.data.frame(t(header_parts))
names(header_df) <- NULL
head(header_df)
# Step 5: Write to Excel with 3-level headers
wb <- createWorkbook()
addWorksheet(wb, "Indicator Matrix")

# Write top three header rows
writeData(wb, sheet = 1, x = header_df[1, ], startRow = 1, startCol = 2, colNames = FALSE)
writeData(wb, sheet = 1, x = header_df[2, ], startRow = 2, startCol = 2, colNames = FALSE)
writeData(wb, sheet = 1, x = header_df[3, ], startRow = 3, startCol = 2, colNames = FALSE)

# Write pdf_name column and data starting from row 4
writeData(wb, sheet = 1, x = wide_data, startRow = 4, startCol = 1, colNames = TRUE)

# Save Excel file
saveWorkbook(wb, "indicator_matrix_hierarchical.xlsx", overwrite = TRUE)





#---LOC-- MATRIX Principles ------------------------

# Create wide-format matrix of principles per framework (pdf_name)
principle_matrix <- data %>%
  group_by(pdf_name, principle) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = principle, values_from = count, values_fill = 0)
principle_matrix <- principle_matrix %>%
  mutate(pdf_name = str_remove(str_to_lower(str_squish(pdf_name)), "\\.pdf$"))

author <- read_excel("authors_abbreviation.xlsx")

principle_matrix <- merge(principle_matrix,author, by.x = "pdf_name", by.y = "Abbreviated", all = TRUE)


principle_matrix <- principle_matrix %>%
  # drop unwanted columns if they exist
  select(-any_of(c("pdf_name", "Author", "split1", "split2"))) %>%
  # ensure author_abbreviation is first
  relocate(author_abbreviation, .before = 1)
principle_matrix <- principle_matrix %>%
  filter(!is.na(author_abbreviation) & author_abbreviation != "")

principle_matrix_z <- principle_matrix %>%
  mutate(across(-author_abbreviation, ~ scale(.)[, 1]))


# Convert to matrix and assign row names
matrix_data <- as.matrix(principle_matrix_z[, -1])
rownames(matrix_data) <- gsub("\\.pdf$", "", principle_matrix_z$author_abbreviation)



# Save heatmap as JPEG
jpeg("principle_heatmap_zscore.jpeg", width = 2200, height = 2800, res = 300)
pheatmap(
  mat = matrix_data,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
  #main = "Z-score Normalized Heatmap of Principles by Framework",
  fontsize_row = 8,      # reduce row font size (frameworks)
  fontsize_col = 8
)
dev.off()



#---------HOC1: High Order Construct 'PATHWAY' ---------------------------------
principle_matrix <- data %>%
  group_by(pdf_name, principle) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = principle, values_from = count, values_fill = 0)

# Step 2: Z-score normalize principle columns
principle_matrix_z <- principle_matrix %>%
  mutate(across(-pdf_name, ~ scale(.)[, 1]))

# Step 3: Clean rownames
principle_matrix_z <- as.data.frame(principle_matrix_z)
rownames(principle_matrix_z) <- gsub("\\.pdf$", "", principle_matrix$pdf_name)

# Step 4: Map principles to pathways
principle_to_pathway <- data %>%
  distinct(principle, pathway) %>%
  mutate(
    principle = tolower(trimws(principle)),
    pathway = tolower(trimws(pathway))
  )

# Step 5: Create a named list of principles grouped by pathway
pathway_groups <- split(principle_to_pathway$principle, principle_to_pathway$pathway)

# Step 6: Compute pathway-level HOC scores by averaging Z-scores
# Filter only valid principles in each group
hoc_scores <- map_dfc(pathway_groups, function(principles) {
  valid_principles <- principles[principles %in% colnames(principle_matrix_z)]
  rowMeans(principle_matrix_z[, valid_principles, drop = FALSE], na.rm = TRUE)
})

# Step 7: Combine with framework ID
hoc_scores <- hoc_scores %>%
  mutate(framework = rownames(principle_matrix_z)) %>%
  relocate(framework)

# Step 8: View and export
print(hoc_scores)
write.csv(hoc_scores, "pathway_hoc_scores.csv", row.names = FALSE)

# Step 9: Heatmap of HOC scores
hoc_matrix <- as.matrix(hoc_scores[, -1])
rownames(hoc_matrix) <- hoc_scores$framework

#-A--CLUSTERED------------------------------------------------------------------
jpeg("pathway_hoc_heatmap_clustered.jpeg", width = 1000, height = 2000, res = 300)
pheatmap(
  mat = hoc_matrix,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
  fontsize_row = 6,   
  fontsize_col = 7,
  fontsize = 10
)
dev.off()

#-B--ORDERED---------------------------------------------------------------------

#Sort pathways (columns) by their average score (descending)
col_means <- colMeans(hoc_matrix, na.rm = TRUE)
ordered_cols <- names(sort(col_means, decreasing = TRUE))
hoc_matrix <- hoc_matrix[, ordered_cols]

#Order frameworks by average HOC score (descending)
row_means <- rowMeans(hoc_matrix, na.rm = TRUE)
ordered_rows <- names(sort(row_means, decreasing = TRUE))
hoc_matrix_ordered <- hoc_matrix[ordered_rows, ]


# Save high-res heatmap (600 DPI)
jpeg("pathway_hoc_heatmap_ordered.jpeg", width = 1000, height = 2000, res = 300)
pheatmap(
  mat = hoc_matrix,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
  fontsize_row = 6,    # increased for 600 DPI
  fontsize_col = 7,
  fontsize = 10
)
dev.off()


#--------------- Create Agroecology Index by averaging pathway scores-----------
hoc_scores <- hoc_scores %>%
  mutate(
    agroecology_index = rowMeans(select(., -framework), na.rm = TRUE)
  )

# Export
head(hoc_scores)
write.csv(hoc_scores, "agroecology_index_by_framework.csv", row.names = FALSE)


# Step 2: Reorder frameworks by index (descending)
hoc_scores_sorted <- hoc_scores %>%
  arrange(desc(agroecology_index))

# Step 3: Create matrix for heatmap using all columns except 'framework'
hoc_matrix <- as.matrix(hoc_scores_sorted[, -1])
rownames(hoc_matrix) <- hoc_scores_sorted$framework

# Step 4: Save heatmap (with adjusted title, fonts, legend)
jpeg("agroecology_index_heatmap.jpeg", width = 500, height = 2000, res = 300)

pheatmap(
  mat = hoc_matrix,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
  fontsize_row = 6,
  fontsize_col = 7,
  fontsize = 10
)

dev.off()

# Install only if not already installed
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("cSEM")
#install.packages("pheatmap")


library(tidyverse)
library(scales)
library(cSEM)
library(pheatmap)
library(RColorBrewer)
library(openxlsx)
library(DiagrammeR)



# Load your original long-format data
data <- read.csv("data/data0526.csv", encoding = "latin1")
# Preview structure
str(data)
data <- data %>% select(-proximity_text)

#--------------------DATA0------------------------------------------------------
# Standardize column names
names(data) <- tolower(names(data))
data <- data %>%
  mutate(
    principle = tolower(trimws(principle)),
    pathway = tolower(trimws(pathway)),
    indicator = tolower(trimws(indicator)),
    pdf_name = tolower(trimws(pdf_name))
  )

to_remove <- c("resource", "matching", "alignment", "health", "soil", "price",  "sustainable", "practices", "knowledge",
               "human", "quality")

#"natural", "diversity", "landscape", "ecosystem", "decision-making", "organizations", "indigenous", "integrated", "diverse" "permaculture","markets", "packaging",

data <- data[!data$indicator %in% to_remove, ]

head(data)

data <- data %>%
  mutate(indicator = case_when(
    indicator == "emissions" ~ "emission",
    indicator == "cover crops" ~ "cover crop",
    indicator == "non-crop plants" ~ "non-crop plant",
    indicator == "shade trees" ~ "shade tree",
    indicator == "natural enemies" ~ "natural enemy",
    indicator == "hedgerows" ~ "hedgerow",
    indicator == "zai pits" ~ "zai pit",
    indicator == "pollinators" ~ "pollinator",
    indicator == "flower strips" ~ "flower strip",
    indicator == "diversified diets" ~ "diversified diet",
    indicator == "seed banks" ~ "seed bank",
    indicator == "insects" ~ "insect",
    indicator == "on-farm trials" ~ "on-farm trial",
    indicator == "exchange visits" ~ "exchange visit",
    indicator == "crop residues" ~ "crop residue",
    TRUE ~ indicator
  ))

data$pdf_name <- ifelse(
  data$pdf_name == "posthumus et al 2023 food systems decision support toolkit.pdf",
  "posthumus-fsdst",
  data$pdf_name
)
data$pdf_name <- ifelse(
  data$pdf_name == "check and add soil health1.pdf",
  "covind-dus",
  data$pdf_name
)


data$principle <- ifelse(
  data$principle == "land and natural resource governance ",
  "land and nr governance",
  data$principle
)

#------------CHECK INDICATORS PER PRINCIPLE---------------------------------------------------------
indicator_table <- data %>%
  distinct(principle, indicator) %>%
  arrange(principle, indicator)

print(indicator_table$principle)
principles <- unique(data$principle)

indicator_table <- indicator_table %>%
  mutate(principle = principle %>%
           str_replace_all("[\\u00A0\\u2007\\u202F]", " ") %>%  # NBSP, Figure space, Narrow NBSP → space
           str_squish() %>%                                    # collapse + trim
           str_to_lower())


data <- data %>%
  mutate(principle = principle %>%
           str_replace_all("[\\u00A0\\u2007\\u202F]", " ")  %>%
           str_squish() %>%                                   
           str_to_lower())

soil_rows <- data %>%
  filter(principle == "soil health") %>%
  distinct(pdf_name, indicator) %>%
  mutate(indicator_norm = str_squish(str_to_lower(indicator)))

# ========= B) Classify indicators (Bio/Chem/Phys/Management/Unclassified) =========
soil_rows_classed <- soil_rows %>%
  mutate(class = case_when(
    # ---- Biological
    str_detect(indicator_norm, paste(c(
      "earthworm|worm\\b","nematode","microb","macro-?organism","micro-organism activity",
      "mycorrh","fungi|bacteria","fauna|flora","community|diversity","food web",
      "enzyme","respiration rate|respirat","biomass c and n|biomass",
      "mineraliz|potentially mineralizable nitrogen|\\bpmn\\b",
      "cellulose decomposition|decomposition",
      "soil proteins","root pathogen","weed seed bank"
    ), collapse="|")) ~ "Biological",
    
    # ---- Chemical
    str_detect(indicator_norm, paste(c(
      "\\bph\\b|soil ph","electrical conductivity|\\bec\\b",
      "salinit|sodicit|alkalin|acid|exchangeable acidity",
      "cation exchange capacity|\\bcec\\b","base saturation",
      "active carbon|reactive carbon|total organic carbon|\\btoc\\b|particulate organic matter|\\bpom\\b",
      "organic matter content|\\bom\\b|organic matter\\b(?! management)",
      "\\bc/n\\b|carbon and nitrogen","nitrate nitrogen|ammonium|\\bno3\\b|\\bnh4\\b",
      "phosphorus concentration|available p|olsen|bray","potassium|\\bk\\b",
      "calcium|magnesium|manganese|iron|zinc|aluminium|aluminum|copper",
      "soil quality\\b", "organic", "heavy metals"
    ), collapse="|")) ~ "Chemical",
    
    # ---- Physical
    str_detect(indicator_norm, paste(c(
      "aggregate stability|wet and dry aggregate size|slake",
      "structure and macroporosity|\\bstructure\\b|macroporosity",
      "bulk density","penetration resistance|hardpan",
      "sub-surface hardness|surface hardness|crusting|compaction",
      "porosity|residual porosity","\\btexture\\b","soil depth\\b|root depth\\b",
      "infiltration rate|field infiltration",
      "saturated hydraulic conductivity|hydraulic conductivity",
      "erosion rating","water holding capacity|pawc|fc|pwp",
      "soil cover\\b"
    ), collapse="|")) ~ "Physical",
    
    # ---- Management (kept separate; exclude below if you only want B/C/P)
    str_detect(indicator_norm, paste(c(
      "cover crop","crop residue|\\bresidue\\b","mulch|mulching",
      "manure|compost|vermicompost|vermicomposting","biochar",
      "contour planting|terrace", "band",
      "minimum till|no-?till|reduced till|tillage",
      "\\brotation\\b","soil amendment","organic farming",
      "agroforestry|permaculture","hand weeding","nutrient cycling", "forest litter"
    ), collapse="|")) ~ "Management",
    
    TRUE ~ "Unclassified"
  ))

unmatched <- soil_rows_classed %>% filter(class == "Unclassified") %>% distinct(indicator_norm)
if (nrow(unmatched)) print(unmatched)

framework_clusters <- tribble(
  ~framework,                  ~cluster_id,
  "fao-agroecology.pdf",        1,
  "ifdc-fsha.pdf",              1,
  "ftf_gsiaf.pdf",              1,
  "siaf.pdf",                   1,
  "nestle-raf.pdf",             1,
  "tgsn.pdf",                   1,
  "teeb.pdf",                   1,
  "eu-ss2030.pdf",              1,
  "fao-vgssm.pdf",              1,
  "cornell-cash.pdf",           1,
  "eea-smeitsha.pdf",           1,
  "ejp-siren.pdf",              1,
  
  "ciat-isfm.pdf",              2,
  "permaculture.pdf",           2,
  "tittonel-sysageco.pdf",      2,
  "fao-almsfa.pdf",             2,
  "fao-esfsi.pdf",              2,
  "common-4returns.pdf",        2,
  "mea.pdf",                    2,
  "wri-ehwb.pdf",               2,
  "unccd-ldn.pdf",              2,
  "caadp.pdf",                  2,
  "fao-tape.pdf",               2,
  
  "caadpresult.pdf",            3,
  "ejp-shttas.pdf",             3,
  "sai-raag.pdf",               3,
  "cbd.pdf",                    3,
  
  "montg-shohph.pdf",           4,
  "devine-rscfish.pdf",         4,
  "nunes-shape.pdf",            4,
  "usda-cfshag.pdf",            4,
  "cornell-shmf.pdf",           4,
  "gwimire-sham4wle.pdf",       4,
  "stockdale-cfmsh.pdf",        4,
  "andrews-smaf.pdf",           4,
  "lehmann-fpsh.pdf",           4,
  "ifa-g4rnutstewf.pdf",        4,
  "covind-dus.pdf",             4,
  "fao-passm.pdf",              4,
  "steve-aesshf.pdf",           4,
  "jian-dgsha.pdf",             4,
  "arshad-sqi.pdf",             4,
  "deel-semwise.pdf",           4,
  "ros-oshafssm.pdf",           4,
  
  "la4aa.pdf",                  5,
  "posthumus-fsdst.pdf",        5,
  "ski-lstoolkit.pdf",          5,
  "cgiar-ll.pdf",               5,
  "mn-shaf.pdf",                5,
  "unccd-ilm.pdf",              5,
  "ploeg-peae.pdf",             5,
  "4rns-inue.pdf",              5,
  "ilm-practical.pdf",          5,
  "ilm_tool_guide.pdf",         5,
  "cices.pdf",                  5,
  "fao-gspaf.pdf",              5,
  "ipes-food.pdf",              5,
  "birner-pepsa.pdf",           5,
  "ldn_cf_journal.pdf",         5,
  "mesmis.pdf",                 5,
  "sayer-la.pdf",               5,
  "ids-srlf.pdf",               5,
  "ifpri-gr.pdf",               5,
  "wezel-tape.pdf",             5,
  "fsat.pdf",                   5,
  "cbd-nea.pdf",                5,
  "si4af.pdf",                  5
)

cluster_labels <- c(
  `1` = "Agroecology & ecosystem",
  `2` = "Efficiency & soil management",
  `3` = "Policy/outcome & standards",
  `4` = "Soil-health assessment tools",
  `5` = "Integrated landscape & livelihoods"
)

framework_clusters <- framework_clusters %>%
  mutate(
    pdf_name = str_squish(str_to_lower(framework)),
    cluster  = recode(as.character(cluster_id), !!!cluster_labels)
  ) %>%
  select(pdf_name, cluster)


soil_bcpm <- soil_rows_classed %>%
  inner_join(framework_clusters, by = "pdf_name") %>%
  filter(class %in% c("Biological","Chemical","Physical", "Management")) %>%   # drop Unclassified
  distinct(pdf_name, cluster, indicator_norm, class)

# ========= D) Frequencies: groups × clusters =========
freq_tbl <- soil_bcpm %>%
  count(cluster, class, name = "n_indicators") %>%
  group_by(cluster) %>%
  mutate(cluster_total = sum(n_indicators),
         share = n_indicators / cluster_total) %>%
  ungroup() %>%
  arrange(cluster, class)

print(freq_tbl)
# columns: cluster | class | n_indicators | cluster_total | share

# Wide matrix (clusters × groups)
freq_wide <- freq_tbl %>%
  select(cluster, class, n_indicators) %>%
  pivot_wider(names_from = class, values_from = n_indicators, values_fill = 0) %>%
  arrange(cluster)

print(freq_wide)

# ========= E) Quick visuals =========
# % stacked bar chart of group mix per cluster
ggplot(freq_tbl, aes(x = cluster, y = share, fill = class)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Framework cluster",
       y = "Share of soil-health indicators",
       fill = "Group",
       title = "Soil-health indicator groups vs. framework clusters") +
  theme_minimal(base_size = 12)

# Heatmap of shares (row = cluster)
mat_share <- freq_tbl %>%
  select(cluster, class, share) %>%
  pivot_wider(names_from = class, values_from = share, values_fill = 0) %>%
  column_to_rownames("cluster") %>%
  as.matrix()

pheatmap(mat_share,
         color = colorRampPalette(RColorBrewer::brewer.pal(9, "PuOr"))(101),
         breaks = seq(0, 1, length.out = 101),
         border_color = NA,
         main = "Share of Biological / Chemical / Physical per cluster")

# ========= F) (Optional) Export =========
# dir.create("out", showWarnings = FALSE)
# readr::write_csv(freq_tbl,  "out/soil_health_by_cluster_tidy.csv")
readr::write_csv(freq_wide, "Results/soil_health_by_cluster_wide.csv")

#------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)

# 1️⃣ Summarize total number of indicators per cluster × class
avg_tbl <- soil_bcpm %>%
  group_by(cluster, class) %>%
  summarise(n_indicators = n(), .groups = "drop")

# 2️⃣ Pivot wider so each class is its own column
avg_wide <- avg_tbl %>%
  pivot_wider(
    names_from = class,
    values_from = n_indicators,
    values_fill = 0
  ) %>%
  arrange(cluster)

# 3️⃣ Optionally add total indicators per cluster
avg_wide <- avg_wide %>%
  mutate(total_indicators = rowSums(across(where(is.numeric))))

# ✅ View result
print(avg_wide)

#-------------------------------------------------------------------------------
principle_clusters <- data.frame(
  principle = c(
    "co-creation of knowledge", "participation",                         # (i)
    "fairness", "connectivity", "land and natural resource governance",  # (ii)
    "economic diversification", "social values and diets",               # (iii)
    "input reduction", "synergy", "recycling", "biodiversity"            # (iv)
  ),
  principle_cluster = c(
    rep("Co-creation & participation", 2),
    rep("Fairness, connectivity & governance", 3),
    rep("Economic diversification & social values", 2),
    rep("Biophysical functioning", 4)
  )
)

# If you want to merge this back to your dataset:
data <- data %>%
  left_join(principle_clusters, by = "principle")

# Check assignment
data %>%
  count(principle_cluster, principle)

head(data)
data <- data %>%
  mutate(pdf_name = str_to_lower(str_squish(pdf_name)))

framework_clusters <- framework_clusters %>%
  mutate(pdf_name = str_to_lower(str_squish(pdf_name)))

data <- data %>%
  left_join(framework_clusters, by = "pdf_name")

wide_tbl <- data %>%
  count(principle_cluster, cluster, name = "n_indicators") %>%
  pivot_wider(
    names_from  = cluster,
    values_from = n_indicators,
    values_fill = 0
  ) %>%
  arrange(principle_cluster)

print(wide_tbl)

# Step 1️⃣: Count indicators per principle_cluster × framework (pdf_name)
ind_per_framework <- data %>%
  count(principle_cluster, cluster, pdf_name, name = "n_indicators")

# Step 2️⃣: Compute the average number of indicators per framework within each cluster
avg_per_framework <- ind_per_framework %>%
  group_by(principle_cluster, cluster) %>%
  summarise(
    avg_indicators = mean(n_indicators),
    n_frameworks = n(),                # frameworks counted
    total_indicators = sum(n_indicators),
    .groups = "drop"
  )

# Step 3️⃣: Pivot to wide format (Principle cluster × Framework cluster)
avg_wide <- avg_per_framework %>%
  select(principle_cluster, cluster, avg_indicators) %>%
  pivot_wider(
    names_from = cluster,
    values_from = avg_indicators,
    values_fill = 0
  ) %>%
  arrange(principle_cluster)

# ✅ View result
print(avg_wide)
readr::write_csv(avg_wide, "Results/pathway_by_cluster.csv")

