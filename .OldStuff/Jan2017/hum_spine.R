#This file defines the hum_spine data type, and associated methods.

setClass('hum_spine',
         contains = 'list',
         slots = c(spineName = 'character',
                   subspines = 'character',
                   dataType = 'character',
                   recordN = 'integer',
                   exclusiveInterpretation = 'character',
                   tandemInterpretations = 'list')
)
setGeneric('hum_spine', function(list) standardGeneric('hum_spine'))

setMethod('hum_spine', signature = c(list = 'list'),
          #The hum_spine constructor
          function(list) {

            tandems = spine_tandems(list)
            exclusiveInterpretation = grep('\\*\\*', list[[1]], value = TRUE)

            list = lapply(list,
                       function(s) {
                         s = s[s != '.']
                         s = s[!grepl('^[!*=]', s)]

                         if(all(s %in% c('TRUE', 'FALSE'))) s = setNames(as.logical(s), names(s))
                         if(all(grepl('^[-+]?[0-9]*[.0-9]+$', s))) {
                           if(any(grepl('\\.', s))) s = setNames(as.double(s), names(s)) else s = setNames(as.integer(s), names(s))
                         }
                         s
                       }
            )

            recordN = sort(unique(as.integer(setNames(nm = NULL, unlist(lapply(list, names))))))

            hum_spine = new('hum_spine', list)
            hum_spine@spineName = names(list)[1]
            hum_spine@subspines = names(tandems)
            hum_spine@recordN = recordN

            hum_spine@dataType = class(list[[1]])
            hum_spine@exclusiveInterpretation = exclusiveInterpretation
            hum_spine@tandemInterpretations = tandems
            hum_spine
          }
        )

spine_tandems = function(list){
  #This function is used by the hum_spine constructor method

  known = unlist(hum_knownTandemInterpretations)

  interp1 = setNames(str_trim(gsub('  *', ' ',
                                   do.call('paste', lapply(X = known, function(k) {
                                     c('', grep(k, list[[1]], value = TRUE))[1 + cumsum(grepl(k, list[[1]]))]
                                   })))),
                     names(list[[1]]))
  interps = lapply(list[-1], function(ss) {
    curspine = list[[1]]
    curspine[names(list[[1]]) %in% names(ss)] = ss

    interps = setNames(str_trim(gsub('  *', ' ',
                                     do.call('paste', lapply(X = known, function(k) {
                                       c('', grep(k, curspine, value = TRUE))[1 + cumsum(grepl(k, curspine))]
                                     })))),
                       names(list[[1]]))
    interps = interps[names(list[[1]]) %in% names(ss)]
    interps[!grepl('^[!=*]', ss) & ss != '.']
  })

  interp1 = interp1[!grepl('^[!=*]', list[[1]]) & list[[1]] != '.']

  interps = append(list(interp1), interps)
  names(interps) = names(list)
  interps
}


setMethod('as.vector',
          signature = c(x = 'hum_spine'),
          function(x) {
            spinelist = x@.Data

            spinelist = lapply(spinelist, function(ss) {
                ss[is.na(ss)] = '.'
                ss
              })
            tapply(unlist(spinelist), names(unlist(spinelist)),
                   function(x) {
                     paste(x, collapse = ' <> ')
                   }
            )
          }
        )

setMethod('grep',
          signature = c(x = 'hum_spine'),
          function(pattern, x, value = FALSE, invert = FALSE) {
            inds = lapply(x@.Data,
                     function(ss) {
                       as.numeric(names(ss)[grep(pattern, ss, invert = invert)])
                     }
                    )
            names(inds) = x@subspines
            inds = new('spinegrep', inds)
            inds
          }
        )

setClass('spinegrep', contains = 'list')


setMethod('[', signature = c(x = 'hum_spine', i = 'numeric'),
          function(x, i, j , drop = FALSE) {

            if(!any(i %in% x@recordN)) {
              x@.Data = list()
              x@tandemInterpretations = list()
            } else {
              x@.Data = lapply(x@.Data, function(ss) {ss[names(ss) %in% as.character(i)]})
              x@tandemInterpretations = lapply(x@tandemInterpretations, function(ss) {ss[names(ss) %in% as.character(i)]})
            }

            x@recordN = x@recordN[x@recordN %in% unlist(lapply(x@.Data, names))]
            x
          }
)

setMethod('[', signature = c(x = 'hum_spine', i = 'spinegrep'),
          function(x, i, j , drop = FALSE) {

            x@.Data = Map(x@.Data, i,
                          f = function(ss, inds) {
                            if(any(inds %in% names(ss))) ss[names(ss) %in% as.character(inds)] else NA
                            }
                          )
            x@tandemInterpretations = Map(x@tandemInterpretations, i,
                                         f = function(ss, inds) {
                                           if(any(inds %in% names(ss))) ss[names(ss) %in% as.character(inds)] else NA
                                         }
            )


            not = unlist(lapply(x@.Data, function(x) length(x) == 1 && is.na(x)))

            x@.Data = x@.Data[!not]
            x@tandemInterpretations = x@tandemInterpretations[!not]
            x@subspines = x@subspines[!not]
            x@recordN = x@recordN[x@recordN %in% unlist(lapply(x@.Data, names))]

            x
          }
)

setMethod('[', signature = c(x = 'hum_spine', i = 'character'),
          function(x, i, j , drop = FALSE) {

            if(length(i) > 1) i = paste(i, collapse = '|')

            x[grep(i, x)]
          }
)



expand_spine = function(spine, recordN = NULL) {
  if(is.null(recordN)) {
    Ran = range(as.numeric(names(unlist(spine@.Data))))
    recordN = seq(Ran[1], Ran[2])
  }
  recordN = recordN

  first = TRUE
  output = Map(
          function(ss, st) {

#             if(first) {
#               first <<- FALSE
#               cur_recordN = recordN
#             } else {
#                 curRan = as.numeric(range(names(ss)))
#                 cur_recordN = recordN[recordN %in% seq(curRan[1], curRan[2])]
#             }

            # cur_recordN = as.character(cur_recordN)
            cur_recordN = as.character(recordN)

            newss = newst = setNames(rep(NA, length(cur_recordN)), cur_recordN)
            newss[match(names(ss), cur_recordN)] = ss

            newst[match(names(st), cur_recordN)] = st

            list(newss, newst)

          },
          spine@.Data,
          spine@tandemInterpretations)

  spine@.Data = lapply(output, '[[', 1)
  spine@tandemInterpretations = lapply(output, '[[', 2)

  spine

}


setMethod('show', signature = c(object = 'hum_spine'),
          function(object) {
            spinelist = object@.Data
            tandemlist = setNames(object@tandemInterpretations, NULL)

            if(length(spinelist) == 0) {
              cat('\n',
                  str_pad('Record#',20,'right'),
                  str_pad('Data tokens#',20,'right'),
                  '*interpretation(s)\n',
                  rep(str_pad('NULL',20, 'right'), 2),
                  'NULL',
                  '\n')
              invisible(return(NULL))
            }
            spinelist = lapply(spinelist, function(ss) {
              ss[is.na(ss)] = ''
              ss})
            tandemlist[is.na(tandemlist)]  = ''

            # tandemlist = lapply(tandemlist, tandem_changes)
            datarecords = tapply(unlist(spinelist), names(unlist(spinelist)),
                                 function(x) {
                                   paste(str_pad(x, 20, side = 'right'), collapse = ' ^ ')
                                   }
                                 )

            datarecords = datarecords[order(as.numeric(names(datarecords)))]

            tandems = tapply(unlist(tandemlist), names(unlist(tandemlist)), paste, collapse=' ^ ')
            tandems[!grepl('[^* \\^]', tandems)] = ''

            tandems = tandems[order(as.numeric(names(tandems)))]

            maxRdigits = max(floor(log(max(as.numeric(names(datarecords))),10)), 2)
            maxDdigits = max(max(nchar(datarecords)) + 4,20)

            cat('\n',
                str_pad('Record#', max(maxRdigits, 10), side = 'right' ),
                str_pad('Data tokens', maxDdigits, side = 'right'),
                '   *interpretation(s)')
            cat('\n\n',str_pad('', max(maxRdigits, 10) + maxDdigits + 4), object@exclusiveInterpretation,'\n')
            cat('\n',
                paste0(
                  paste0(str_pad(paste0('[',str_pad(names(datarecords), width = maxRdigits),']'),max(10, maxRdigits), side = 'right'),
                  str_pad(datarecords,maxDdigits, side = 'right'),
                  '     ',
                  tandems
                  ),'\n'))

            }
          )






