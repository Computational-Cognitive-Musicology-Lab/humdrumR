#! /usr/bin/Rscript

library(jsonlite)

search <- fromJSON('search.json')

search$code <- ""

write_json(search, 'search.json')

