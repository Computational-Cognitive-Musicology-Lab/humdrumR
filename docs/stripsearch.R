#! /usr/bin/Rscript

# Shrink pkgdown's search.json by blanking the `code` field, which is dominated
# by repetitive humdrumR console output and makes fuse.js search slow.
#
# We only strip Articles (vignettes) -- that's where the bulk of the repetitive
# output lives (~66% of all `code`). Reference `code` is left intact so function
# usage in @examples stays searchable.
#
# NOTE: pkgdown regenerates search.json on every build_site(), so run this AFTER
# each build.

library(jsonlite)

search <- fromJSON('search.json')

# which() drops records that have no `dir` (NA), avoiding an NA-subscript error
search$code[which(search$dir == "Articles")] <- ""

write_json(search, 'search.json')
