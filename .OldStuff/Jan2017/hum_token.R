library(stringr)
setClass('hum_token',
         contains = 'data.frame',
         slots = c(ExclusiveInterpretation = 'character',
                   SubInterpretations = 'character',
                   TandemInterpretations = 'character'))

setGeneric('hum_token', function(input, interp) standardGeneric('hum_token'))
setMethod('hum_token',
          definition = function(input, interp) {
            data = humdrum_slice_token(input, interp)

            new('hum_token',
                data,
                ExclusiveInterpretation = c(interp, head(colnames(data), -1))
            )

          }
          )


humdrum_slice_token = function(token, regexes) {
  input = regexes

  regexes = append(regexes, unlist(lapply(regexes[grepl('\\*\\*', regexes)],humdrum_lookupInterpretation)))
  regexes = regexes[!grepl('\\*\\*', regexes)]

  Names = names(regexes)
  Names[Names == ''] = input[!grepl('\\*\\*', input)]

    tokens = unlist(strsplit(token, split = ' '))

    out = lapply(regexes, function(regex) {unlist(lapply(str_extract_all(tokens, regex), paste, collapse = '.*'))})
    #
    out = do.call('data.frame', list(out, stringsAsFactors = FALSE))

    leftovers = tokens
    invisible(lapply(gsub('\\[', '\\\\[', out), function(x) {
      if(any(!is.na(x)) && x != '')  leftovers <<- str_replace(leftovers, x[!is.na(x)], '')
    }
      )
      )
    leftovers[leftovers == ''] = NA_character_

    out[] = lapply(out, function(x) gsub('\\.\\*', '', x))
    # lapply(1:nrow(out), function(i) browser())

    colnames(out) = c(Names)
    rownames(out) = unlist(strsplit(token, split = ' '))
    out$Unknown = leftovers

    out

}