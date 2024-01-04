#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Indexing PSY-GS 8875 Syllabus ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load {reticulate}
library(reticulate)

#%%%%%%%%%%%%%%%%%%%%%%%%
## Install miniconda ----
#%%%%%%%%%%%%%%%%%%%%%%%%

# Check for whether conda is already installed
conda_version()

# If error, then install conda
install_miniconda()
# Only need to install once

# Create conda environment
conda_create(envname = "syllabus-ai")
# Only need to create once

# Use environment
use_condaenv(condaenv = "syllabus-ai")

# Install necessary modules
conda_install(
  packages = c(
    "llama-index", "pypdf",
    "nltk", "llama-cpp-python",
    "transformers", "torch",
    "accelerate"
  ),
  envname = "syllabus-ai",
  pip = TRUE
)
# Only need to install once

# Check that modules were installed
py_list_packages()

# Import {llama-index}
llama_index <- import("llama_index")

#%%%%%%%%%%%%%%%%%%%%%%%%%
## Set up for LLAMA-2 ----
#%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up LLAMA-2
service_context <- llama_index$ServiceContext$from_defaults(
  embed_model = "local", llm = "local"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%
## Set up for ChatGPT ----
#%%%%%%%%%%%%%%%%%%%%%%%%%

# Import {os}
# os <- import("os")

# Set API Key
# os$environ["OPENAI_API_KEY"] = "YOUR_OPENAI_API_KEY_HERE"
# My key is already set to my global environment

# Set up GPT-3.5 Turbo
service_context <- llama_index$ServiceContext$from_defaults(
  llm = llama_index$llms$OpenAI(
    temperature = 0.20, model = "gpt-3.5-turbo"
  )
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Set up for huggingface model ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# See more information on this model:
# https://huggingface.co/TinyLlama/TinyLlama-1.1B-Chat-v1.0

# Pass to service context
service_context <- llama_index$ServiceContext$from_defaults(
  llm = llama_index$llms$HuggingFaceLLM(
    model_name = "TinyLlama/TinyLlama-1.1B-Chat-v1.0",
    tokenizer_name = "TinyLlama/TinyLlama-1.1B-Chat-v1.0",
    query_wrapper_prompt = llama_index$PromptTemplate("<|system|>\nYou are a chatbot who can help students!</s>\n<|user|>\n{query_str}</s>\n<|assistant|>\n"),
    device_map = "cpu"
  ),
  embed_model = "local:BAAI/bge-small-en-v1.5"
)

#%%%%%%%%%%%%%%%%%%%%%%%
## General Querying ----
#%%%%%%%%%%%%%%%%%%%%%%%

# Set global service context
llama_index$set_global_service_context(service_context)

# Load documents
documents <- llama_index$SimpleDirectoryReader("./documents")$load_data()

# Set indices
index <- llama_index$VectorStoreIndex(
  documents, service_context = service_context,
  show_progress = TRUE
)

# Query from index
engine <- index$as_query_engine(
  similarity_top_k = 3, # number of responses
  response_mode = "tree_summarize" # leave "as-is"
)

# Get query
tictoc::tic()
extract_query <- engine$query("What course is this syallbus for?")
tictoc::toc()
# Takes quite a bit of time...

# Using OpenAI's API goes much faster...

# Extract page numbers
page_numbers <- sapply(
  extract_query$metadata,
  function(x){x$page_label}
); unname(page_numbers)

# Extract document label
document_labels <- sapply(
  extract_query$metadata,
  function(x){x$file_name}
); unname(document_labels)

# Get response
response <- extract_query$response; trimws(response)

# Relevant text
relevant_text <- try(
  sapply(
    extract_query$source_nodes,
    function(x){x$node$text}
  ), silent = TRUE
); relevant_text

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Storing and Loading Indices ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Storing

## Create new directory
dir.create("./index_storage")

## Store context
index$storage_context$persist("./index_storage/")

# Remove `index` as part of demonstration that it works...
rm(index)

# Loading

## Get storage context
storage_context <- llama_index$StorageContext$from_defaults(
  persist_dir = "./index_storage"
)

## Load index from storage
index <- llama_index$load_index_from_storage(storage_context)

## Good to go again!
engine <- index$as_query_engine(
  similarity_top_k = 3, # number of responses
  response_mode = "tree_summarize" # leave "as-is"
)

## Get query
tictoc::tic()
extract_query <- engine$query("What course is this syallbus for?")
tictoc::toc()
