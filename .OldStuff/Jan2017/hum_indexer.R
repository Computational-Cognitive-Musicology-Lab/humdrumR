# creating special regular expression objects: hum_regex


hum_regex = function(a, b){
  index = a
  indref = b

  if(length(indref) == 0) {
    indref = NULL
  }

  output = list(index = index, where = indref)
  class(output) = append('hum_regex', class(output))
  output
}


print.hum_regex = function(x) {
  index =  paste('"',gsub('\\|', ' OR ', x$index), '"', sep = '', collapse = ' AND ')

  if(!is.numeric(x$where)) {
    if(is.null(x$where)) {
      where = 'Anywhere'
    } else {
      where =  paste('"',gsub('\\|', ' OR ', x$where), '"', sep = '', collapse = ' AND ')
    }
  } else {
    where = x$where
  }
  where = unlist(lapply(where, function(w) {if(is.null(w)) 'Anywhere' else w}))

  cat('\n \tHumdrum regex object:\n\n')
  cat('\t\tSearch for: (',
      index,
      ')\t IN \t(',
      where,
      ')\n')
}

### REGEX INFIX OPERATORS

# |

`|` = function(e1, e2){ UseMethod('|')}

`|.default` = function(e1, e2){ .Primitive('|')(e1, e2)}

`|.character` = function(e1, e2){
  if(inherits(e2, 'hum_regex')) {
    e1 = e1 ~ NULL
    return(e1 | e2)
  } else {
  paste(e1, e2, sep = '|')
  }
}

`|.hum_regex` = function(e1, e2){

  if(!inherits(e2, 'hum_regex')) e2 = e2 ~ NULL

  output = list('init' = e1, 'OR' = e2)
  class(output) = append('hum_regexlist', 'hum_regex', class(output))
  output
}


# &

`&` = function(e1, e2){ UseMethod('&')}

`&.default` = function(e1, e2){ .Primitive("&")(e1, e2)}

`&.character` = function(e1, e2){
  if(inherits(e2, 'hum_regex')) {
    e1 = e1 ~ NULL
    return(e1 & e2)
  } else {
   c(e1, e2)
  }
}

`&.hum_regex` = function(e1, e2){
  if(!inherits(e2, 'hum_regex')) e2 = e2 ~ NULL
  output = list('init' = e1, 'AND' = e2)
  class(output) = append(c('hum_regexlist', 'hum_regex'), class(output))
  output}


# ~

`~` = function(a,b){ UseMethod('~') }

`~.default` = function(a,b){ .Primitive('~')(a, b) }

`~.character` = function(a, b){ hum_regex(a, b) }






# searching for hum_regex in hum_score

hum_grep = function(regexes, score, margin, return_logical = TRUE){
  ##-- general prep
  other_margin  = (margin %% 2) + 1
  ## -- check regexes arguments
  if(missing(regexes) || length(regexes) == 0){
    if(return_logical) return(rep(TRUE, dim(score)[other_margin])) else return(seq_len(dim(score)[other_margin]))
  }

  if(class(regexes)[1] != 'hum_regexlist'){
    if(is.numeric(regexes) || is.logical(regexes)) return(regexes)


    if(is.list(regexes) && all(unlist(lapply(regexes,
                                             function(listitem) {
                                               class(listitem)[1] %in% c('hum_regex', 'character')
                                               })
    ))) {
      regexes = lapply(regexes, function(listitem) {
        if(is.character(listitem)) {listitem ~ NULL} else {listitem}
      })
      class(regexes) = append('hum_regexlist', class(regexes))
    }


    if(class(regexes)[1] == 'hum_regex') {
      regexes = list(regexes)
      class(regexes) = append('hum_regexlist', class(regexes))
    }

    if(is.character(regexes)) {
      regexes = lapply(regexes, '~', b = NULL)
      class(regexes) = append('hum_regexlist', class(regexes))
    }
  }

  if(is.null(names(regexes))) {
    names(regexes) = c('init',rep_len('AND', length(regexes) - 1))
  }
  names(regexes)[names(regexes) == ''] = 'AND'
  names(regexes)[names(regexes) == 'AND'] = paste('AND',seq_len(sum(names(regexes) == 'AND')))
  names(regexes)[names(regexes) == 'OR'] = paste('OR',seq_len(sum(names(regexes) == 'OR')))

  ## -- check score argument
  if(missing(score) || class(score) != 'hum_score') stop("\nYou forgot to give hum_grep a hum_score argument. Nothing is returned.\n")

  ## -- check margin argument
  if(!is.numeric(margin) || !margin %in% c(1,2)) {
    message("\n You've asked hum_grep to look across a margin other than 1 (rows) or 2 (columns).\n
            hum_scores are always two dimmensional so this is invalid. As a result, the default margin 2 (columns), is used.")
    margin = 2
  }

  ## -- check return_logical argument
  if(length(return_logical) != 1 || !is.logical(return_logical)) {
    return_logical = TRUE
  }


  ## --
  Data  = list(list(Data    = unlist(apply(as.data.frame(score) , 1, list), recursive = FALSE),
                    Tandems = unlist(apply(score@Tandems, 1, list), recursive = FALSE)),
               list(Data    = as.data.frame(score),
                    Tandems = score@Tandems))
  hits = lapply(regexes,
                function(hum_regex) {
                  where = hum_parse_where(hum_regex$where, Data[[other_margin]])
                  hum_greper(hum_regex$index,
                             lapply(Data[[margin]], '[', where))
                  })

  if(length(hits) > 1){
  hits = Reduce(function(a, b) {
    if(grepl('AND', b)){
      hits[[a]] & hits[[b]]
      } else {
        hits[[a]] | hits[[b]]
        }
    }, names(hits))
  } else {
    hits = unlist(hits)
  }

   hits

}


hum_parse_where = function(where, data){
  default = seq_len(length(data$Data[[1]]))

  if(is.null(where) || length(where) == 0) return(default)
  if(is.numeric(where)){
    where = as.integer(where)
    where = where[where != 0L && abs(where) <= max(default)]
    if(length(where) == 0L) where = default
    return(where)
  }

  if(is.logical(where) && length(where) != length(default)){
    where = rep_len(where, length(default))
    return(which(where))
  }

  which(hum_greper(where, data))

}

hum_greper = function(regex_vector, df){
  #searches for regexes vectors in a 2-length list of DFs (data, and tandems)

  Reduce('&',
         lapply(regex_vector,
                function(regex){
                  if(grepl('^[*]|[|][*]', regex)){
                    target = df$Tandems
                  } else {
                    target = df$Data
                  }

                  regex = gsub('\\*','\\\\*',regex)
                  Reduce('|', lapply(target, grepl, pattern = regex))
                  })
         )
}
