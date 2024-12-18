# Load libs
library(tidyverse)
library(readr)
library(stringdist)
library(fuzzyjoin)
library(magrittr)
library(here)
library(stringi)
library(arrow)
library(readr)
library(progress)
library(janitor)

# Load utils
source(here("scripts/00_utils.R"))

# 
elex_raj_05_10 <- read_parquet(here("data/raj/elex_raj_05_10.parquet"))
