humdrumValidate = function(records, filename) {
  localn = grep('!!', records, invert = TRUE, useBytes = TRUE)
  local = records[localn]
  
  if(!grepl('^\\*\\*', local[1], useBytes = TRUE))       stop('\n\tError in ', filename, '\n',
                                                              '\tin record #', localn[1],':\n',
                                                              '\tThe first local record must be an exclusive interpretation (**)',
                                                              call. = FALSE)
  if(!grepl('^\\*\\-', tail(local, 1), useBytes = TRUE)) stop('\nError in ', filename, ,'\n',
                                                              '\tin record #', tail(localn, 1), ':\n', 
                                                              '\tThe last local record must be a termination (*-)',
                                                              call. = FALSE)
  
  local = splitColumns(local)
  
  Map(function(record, n) { 
    types = str_extract(record, '^(\\*\\*|\\*|=|!)')
    types[is.na(types)] = '.'
    
    if (length(unique(types)) != 1) {
      stop('\n\tError in ', filename, '\n',
           '\tIn record #', n, ':\n',
           '\tMismatched record types: ',
           paste(types, collapse = ' '),
           call. = FALSE)
    }
  },
  local,
  localn
  )
}

