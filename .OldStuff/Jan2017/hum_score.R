setClass('hum_score',
         contains = 'list',
         slots = c(recordN = 'numeric',
                   spineNames= 'character',
                   exclusiveInterpretations = 'character',
                   NonData = 'list',
                   Filename = 'character',
                   Reference = 'data.frame')
         )
#



###
#########

hum_file2hum_score = function (filename){

  filerecords =readLines(filename)

  Spinepaths = hum_records2spines(filerecords)
  Spines = Spinepaths$Spines
  TanLocBar = Spinepaths$Nondata

  NonData = list(
    GlobalComments    = setNames(grep('^!!$|^!![^!]+', filerecords, value = T), grep('^!!$|^!![^!]+', filerecords)),
    ReferenceRecords  = setNames(grep('^!!!.*'  , filerecords, value = T),  grep('^!!!.*'  , filerecords))
  )

  #
  Reference = NonData$ReferenceRecords
  if(length(Reference) != 0) {
    ReferenceFrame = data.frame(stringsAsFactors = FALSE,
                                  Type = gsub('!!!', '',
                                              gsub(':.*', '',
                                                   Reference)),
                                  Records = str_trim(side = 'both',
                                                     gsub('^!!!.*:', '',
                                                          gsub('\t+', ' ',
                                                               Reference))),
                                  row.names = names(Reference))
  } else ReferenceFrame = NULL


  recordN = sort(unique(unlist(lapply(Spines,function(S) {S@recordN}))))


  spineNames = spine_namer(Spines, recordN)

  exclusiveInterpretations = unlist(lapply(Spines, function(S) {S@exclusiveInterpretation}))
  ###

  #

  TanLocBar = lapply(setNames(TanLocBar, NULL), function(S) {
      tokens = unlist(setNames(S, NULL))
      tokens = tapply(tokens, names(tokens), paste, collapse = ' <> ')
      tokens = tokens[order(as.numeric(names(tokens)))]
      tokens
  }
  )
  TanLocBar = apply(do.call('cbind', TanLocBar), 1, paste, collapse = '\t')

  NonData$Interpretations = grep('^\\*',           TanLocBar, value = TRUE)
  NonData$LocalComments   = grep('^\\!$|^\\![^!]', TanLocBar, value = TRUE)
  NonData$Barlines        = grep('^\\=',           TanLocBar, value = TRUE)

return(Spines)
  new('hum_score',
      Spines,
      recordN = recordN,
      spineNames = spineNames,
      exclusiveInterpretations = exclusiveInterpretations,
      Filename = filename,
      NonData = NonData,
      Reference = ReferenceFrame
  )

}



# -- Functions used by hum_file2hum_score

spine_namer = function(Spines, RecordN){

  interplists = lapply(Spines,
                       function(S) {
                         c(S@exclusiveInterpretation,
                           unique(unlist(strsplit(split = ' ', expand_spine(S, RecordN)@tandemInterpretations[[1]] ))))
                         }
                       )

  snames = vector('character', length(interplists))
  for(i in seq_along(interplists)){
    matches = !interplists[[i]] %in% unlist(interplists[-i]) & !interplists[[i]] %in% c('*', '*^', '*v', '*-')
    if(any(matches)) snames[i] = interplists[[i]][which(matches)[1]]
  }
  snames[snames == ''] = paste('Spine_',toupper(letters[seq_len(sum(snames == ''))]),sep='')

  gsub('\\**', '', snames)
}

spine_vector_expand = function(x , index, n){
  # This funcition takes a vector and repeats the data at each index by n times.
  for(i in index){
    lets = unlist(lapply(letters, function(let) ifelse(any(grepl(let, x)),NA,let)) )
    lets = lets[!is.na(lets)]
    x[i] = paste(paste(rep(x[i], n),
                       c('', paste('_', lets[seq_len(n - 1)], sep = '')),
                       sep = ''),
                 collapse = '__')
  }
  unlist(strsplit(x,split='__'))
}

hum_records2spines = function(Records) {

  spine_vector = toupper(letters[seq_len(1 + str_count(grep('^\\*', Records, value = T)[1], '\t'))])

  spines = replicate(length(spine_vector), list())
  names(spines) = spine_vector

  for (i in grep('^!!', Records, invert = T)) {
      tokens = unlist(strsplit(Records[i], split = '\t'))

      lapply(seq_along(spine_vector), function(j) {
        ss = spine_vector[j] # specific (sub)spine
        S = gsub('_.*', '', spine_vector[j]) #top-level spine
        spines[[S]][[ss]] <<- setNames(append(spines[[S]][[ss]], tokens[j]), c(names(spines[[S]][[ss]]), i))
       })


    # if there's a spine path change:
    path_open  = grep('\\*\\^', strsplit(Records[i], split='\t')[[1]])
    path_close = grep('\\*v'  , strsplit(Records[i], split='\t| <> ')[[1]])
    if (length(path_open) > 0){
      spine_vector = spine_vector_expand(spine_vector, path_open, 2)
    }
    if(length(path_close) > 0){
      spine_vector[path_close] =  str_replace(spine_vector[path_close],'_[^_]*$','')
      spine_vector = unique(spine_vector)
    }


  } #close for loop
  nondata = lapply(spines,function(S) lapply(S, grep, pattern = '^[*!=]',value = TRUE))
  spines = lapply(spines, hum_spine)

  list(Spines = spines, Nondata = nondata)
}



##-----hum_score methods ----------------------------------------------------------------------------##



## -- indexing -- #
source('~/Bridge/Research/Programming/humdrumR/hum_indexer.R', echo=TRUE)


hum_parse_indexes = function(i, j, x) {
  ##j prep
  if(inherits(j, 'hum_regex') || is.character(j)){
    if(is.character(j)) {
      namei = unique(unlist(lapply(j, function(regex) {grep(j, x@SpineInfo$Names,ignore.case = TRUE)})))
    } else {namei = c()}
    j = union(namei, which(hum_grep(j, score = x, margin = 1)))
  } else {
    if(is.logical(j)){
      if(length(j) != ncol(x)){
        j = rep(j, length.out = ncol(x))
      }
      j = which(j)
    }
  }
  j = j[j <= ncol(x)]
  if(length(j) == 0) {
    message("\nNo matching rows in column index\n")
    j = 0
  }

  ## i prep

  if(inherits(i, 'hum_regex') || is.character(i)){
    i = hum_grep(i, score = x, margin = 2)
    i = which(i)
  } else {
    if(all(is.logical(i))){
      if(length(i) != nrow(x)){
        i = rep(i, length.out = nrow(x))
      }
      i = which(i)
    }
  }

  i = i[i <= nrow(x)]
  if(length(i) == 0) {
    message("\nNo matching rows in row index.\n")
    i = 0
  }

  list(i = i, j = j)

}


setMethod('nrow',
          signature = 'hum_score',
          function(x) {
            length(x@recordN)
          }
)

setMethod('ncol',
          signature = 'hum_score',
          function(x) {
            length(x@spineNames)
          }
)
setMethod('dim',
          signature = 'hum_score',
          function(x) {
            c(length(x@recordN), length(x@spineNames))
          }
)
setMethod('as.data.frame',
          signature = c(x = 'hum_score'),
          function(x) {
            as.data.frame(lapply(align_spines(x), as.vector), row.names = x@recordN, col.names = x@spineNames)
            })

setMethod('as.matrix',
          signature = c(x = 'hum_score'),
          function(x) {
            as.matrix(as.data.frame(x))
          })

setMethod('[', signature = c(x = 'hum_score'),
          function(x, i, j, drop = FALSE) {
            x@.Data = lapply(x@.Data, '[', i)

            extant = unlist(lapply(x@.Data, function(x) length(x) > 0))
            x@.Data = x@.Data[extant]
            x@spineNames = x@spineNames[extant]
            x@exclusiveInterpretations = x@exclusiveInterpretations[extant]

            x@NonData[3:5] = lapply(x@NonData[3:5], function(x) lapply(x, function(z) paste(unlist(strsplit(z, split='\t'))[extant],collapse='\t')))

            x@recordN = x@recordN[x@recordN %in% unlist(lapply(x@.Data, function(s) s@recordN))]
            x
          }
)

setMethod('[[', signature = c(x = 'hum_score', i = 'character'),
          function(x, i, j, drop = FALSE) {

            if(length(i) > 1) i = paste(i, collapse = '|')

            i = unlist(lapply(x@.Data, grep, pattern = i))

            x@.Data = lapply(x@.Data, '[', i)

            x@recordN = x@recordN[x@recordN %in% unlist(lapply(x@.Data, function(s) s@recordN))]
            x
          }
)
#
# setMethod('[',
#           signature = c(x = 'hum_score'),
#           function(x, i, j, drop = FALSE) {
#             if(missing(i) || is.null(i)) i = seq_len(nrow(x))
#             if(missing(j) || is.null(j)) j = seq_len(ncol(x))
#
#             # indices = hum_parse_indexes(i, j, x)
#             # i = indices$i
#             # j = indices$j
#
#             i = i[i <= nrow(x) & i > 0 & is.whole(i)]
#             fileIndices = as.numeric(rownames(x))[i]
#             if(1 %in% i) { #pulls in records from before first data point
#                fileIndices = c(1:(min(fileIndices) - 1), fileIndices)
#             }
#             if(length(fileIndices) > 0 ){
#                irange = min(fileIndices):max(fileIndices)
#              } else {
#                irange = 0
#              }
#
#             x@.Data = lapply(x@.Data, '[', irange)
#             x@recordN = x@recordN[as.numeric(x@recordN) %in% irange]
#             x@NonData = lapply(x@NonData, function(x) x[names(x) %in% as.character(irange)])
#             x@spineNames = x@spineNames[j]
#             x@exclusiveInterpretations = x@exclusiveInterpretations[j]
#
#             x
#           })

setMethod('[[',
          signature = c(x = 'hum_score'),
          function(x, i, j, drop = FALSE) {
            if(missing(i) || is.null(i)) i = seq_len(nrow(x))
            if(missing(j) || is.null(j)) j = seq_len(ncol(x))

            indices = hum_parse_indexes(i, j, x)
            i = indices$i
            j = indices$j

            fileIndices = as.numeric(rownames(x))[i]
            if(any(i == 1)) { #pulls in records from before first data point
              fileIndices = append(1, fileIndices)
            }
            if(length(fileIndices) > 0 ){
              irange = min(fileIndices):max(fileIndices)
            } else {
              irange = 0
            }

            #actual indexing
            df = as.data.frame(x)[i,j, drop =FALSE]

            if(drop){return(df)}

            x@.Data = df
            x@Tandems = x@Tandems[i, j, drop = FALSE]
            x@row.names = x@row.names[i]
            x@names = x@names[j]
            x@SpinePaths = x@SpinePaths[i, j, drop = FALSE]
            x@SpineInfo = x@SpineInfo[j, ]
            x@NonData[c('Interpretations', 'LocalComments', 'Barlines')] = lapply(x@NonData[c('Interpretations', 'LocalComments', 'Barlines')],
                                                                                  function(dataset) {dataset[ , j, drop = FALSE]})
            x@NonData = lapply(x@NonData,
                               function(dataset) {
                                 if(is.null(dim(dataset))){
                                   dataset[as.numeric(names(dataset)) %in% irange]
                                 } else {
                                   dataset[as.numeric(rownames(dataset)) %in% irange, , drop = FALSE]
                                 }
                               })

            # attr(x, 'i') = append(attr(x, 'i'), list(i))
            # attr(x, 'j') = append(attr(x, 'j'), list(j))
            x
          })


## -------

`[<-.hum_score` = function(x, i, j, value){
  if(missing(i)) i = seq_len(nrow(x))
  if(missing(j)) i = seq_len(ncol(x))

  indices = hum_parse_indexes(i, j, x)
  i = indices$i
  j = indices$j

  if(inherits(value, 'hum_score')) value = value@.Data

  x[i, j] <- value

  x

}

##


hum_apply = function(hum_score, func, margin = 2, i = NULL, j = NULL, ...) {
  if(identical(func, save)){
    save(hum_score, homeenv = homeenv)
    return(hum_score)
  }

  if(func %in% list(ditto)){
    pathmethod = 3
  } else  if(func %in% list(table, `-`, `%strip%`)){
    pathmethod = 2
  } else pathmethod = 1


  ##indexing
  if(!is.null(attr(func, 'i'))) i = attr(func, 'i')
  if(!is.null(attr(func, 'j'))) j = attr(func, 'j')
  #

  ind_score = hum_score[i, j, drop = FALSE]

  i = attr(ind_score, 'i')[[1]]
  j = attr(ind_score, 'j')[[1]]

  spinenames = ind_score@SpineInfo$Names
  ind_paths = ind_score@SpinePaths
  ind_score = ind_score

  #

  if(margin == 1) ind_score = t(ind_score)

  output = vector('list', ncol(ind_score))
  names(output) = spinenames
  for(k in seq_along(output)) {
      curspine = ind_score[ , k]
      curspine[curspine == '.'] = NA

     if(all(grepl('^[0-9.]+$', curspine[!is.na(curspine)]))) curspine = as.numeric(curspine)

    if(margin == 2 && any(grepl(' <> ', ind_score[ , k])) && pathmethod %in% c(1,2,3)){

      output[[k]] = spine_pathize(func, method = pathmethod, spinepath = ind_paths[ , k])(curspine, ...)
    } else {
      output[[k]] = func(curspine, ...)
    }
  }
  if(length(unique(sapply(output, length))) == 1){
    output = as.data.frame(output, stringsAsFactors = FALSE)
    if(margin == 1) output = t(output)
  }

  if(!is.null(dim(output)) && dim(output) == dim(ind_score)){
    hum_score[i, j] = output
    hum_score
  } else {
    output
  }
}



######  ---- spine path shit!! ----





spine_paths = function(func, spine, spinepath, ...) {
  spinelist = spine_split(spine, multistops = FALSE)
  pathlist = spine_split(spinepath, multistops = FALSE)

  pathnames = unique(unlist(pathlist))
  pathnames = pathnames[sapply(seq_along(pathnames), function(x) all(!str_detect(pathnames[-x],pathnames[x])))]

  for(j in pathnames){
    curpath_j = sapply(pathlist, function(x) which(str_detect(j, unlist(x))))
    curpath = unlist(Map('[[', spinelist, curpath_j))

    curpath = func(curpath, ...)

    lapply(seq_along(curpath), function(x){
      spinelist[[x]][[curpath_j[[x]]]] <<- curpath[[x]]
    })
    }

    spine_zip(spinelist)
  }





# ------- #

setMethod('summary',
          signature = c(object = 'hum_score'),
          function(object){
            DIM = dim(object)
            sinfo = object@SpineInfo

            cat('\n')
            cat('Humdrum score with', DIM[1], 'data records in', DIM[2],'spines:\n\n')

            for(i in seq_len(DIM[2])){
              cat("\tSpine ",i,"\t'",sinfo$Names[i],"':\n",sep = '')
              cat('\t\t\tInterpretation =', sinfo$Interpretations[i],'\n')
              cat('\t\t\tNumber of data tokens =', sinfo$Ntokens[i])

              tokens = table(grep('^[.][.( <> )*]', invert = TRUE, value = TRUE, object[ , i]))
              cat('  (',length(tokens), ' unique tokens)\n', sep='')
              # cat('\n')

            }

          })


setMethod('grepl',
          signature = c(x = 'hum_score'),
          function(pattern,x, ignore.case = FALSE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE) {

            x@.Data=as.data.frame(lapply(x, grepl, pattern = pattern,
                                 useBytes = useBytes,perl = perl,
                                 fixed = fixed, ignore.case = ignore.case),
                          row.names = rownames(x),col.names = colnames(x))

            x
          })

setMethod('rownames',
          signature = c(x = 'hum_score'),
          function(x) x@recordN)

setMethod('show',
          signature = c(object = 'hum_score'),
          function(object) {
            if(!any(dim(object) == 0)) {
              hum_plothum_score(object, Nscreens = 1)
            } else {
              cat("\n Empty hum_score.\n")
            }
          })

align_spines = function(hum_score) {
 spineslist = hum_score@.Data

 lapply(spineslist, expand_spine, recordN = hum_score@recordN)

}

hum_plothum_score = function(object, Nscreens = NULL, RperScreen = 50, Cex = .8){
  #Prepare data
  data_lines = apply(as.data.frame(object), 1, paste, collapse = '\t')
  data_indices = as.numeric(object@recordN)

  nondata_lines = lapply(object@NonData,
                         function(dataset){
                           if(!is.null(dim(dataset))){
                             dataset = apply(dataset, 1, paste, collapse ='\t')
                           }
                           dataset
                         }
  )

  nondata_indices = as.numeric(names(unlist(setNames(nondata_lines,NULL))))
  types = c(rep('D', length(data_lines)) ,gsub('(^.).*','\\1',names(unlist(nondata_lines))))[order(c(data_indices, nondata_indices))]
  nondata_lines = unlist(nondata_lines)

  records = c(data_lines, nondata_lines)[order(c(data_indices, nondata_indices))]
  recordIndices = sort(c(data_indices, nondata_indices))
  spinenames = object@spineNames

  newdata_indices = vector(mode='character', length(recordIndices))
  newdata_indices[recordIndices %in% data_indices] = seq_along(data_indices)

  records = str_replace_all(records, 'NA', '.')

  if(is.null(Nscreens)) { Nscreens = ceiling(length(records) / RperScreen)}

  splitrecords=strsplit(records, split = '\t')
  rightedge=max(sapply(splitrecords,length))+1

  #do plot(s):
  if(dev.cur() != 2) X11()
  dev.set(2)

  for(screen in 1:Nscreens){
    #if more than one screen is needed to see the whole thing

    if(screen != 1) readline('Press any key to see next screen')

    top = (screen*RperScreen)+1

    plot.new()
    par(mar = c(0, 0, 5, 0))
    plot.window(x=c(1,rightedge), y=c(1,RperScreen))


    for(i in seq(1,rightedge-1,2)){
      polygon(x=c(i,i,i+1,i+1),y=c(0,RperScreen+.5,
                                   RperScreen+.5,0),
              col='grey95',border=F)
    }


    mtext(side = 3, text = hum_refhum_score(object, 'OTL'), line = 1)

    text(seq_len(length(spinenames)) + .5,
         rep(RperScreen + 1, length(spinenames)),
         spinenames, adj = .5, xpd = T)

    SEQ = ((screen-1) * RperScreen) + (seq_len(RperScreen))
    SEQ = SEQ[SEQ <= length(splitrecords)]

    for(i in SEQ){
      cur = strsplit(splitrecords[[i]],split=' <> ')

      linecolor=switch(types[i],
                       'B'='blue',
                       'L'='darkgreen',
                       'G'='green',
                       'D'='black',
                       'I'='red',
                       'R'='white')

      cexscale=ifelse(types[i] == 'D', 1, .75)

      if(types[i] %in% c('B')){
        RGB = as.vector(col2rgb(linecolor)) / 255
        polygon(x = c(1, 1, rightedge, rightedge),
                y = c(top - i + .5, top - i - .5, top - i - .5, top - i + .5),
                border = FALSE,
                col=rgb(RGB[1], RGB[2], RGB[3], alpha = .05))
      }
      if(types[i] %in% c('G','R')){
        polygon(x = c(1, 1, rightedge, rightedge),
                y = c(top - i + .5, top - i - .5, top - i - .5, top - i + .5),
                border = FALSE,
                col='grey30')
      }

      X = cumsum(head(c(1, unlist(lapply(cur, function(x) rep(1 / length(x), length(x))))), -1))

      # text(1, top - i, paste('[', recordIndices[i], ']', sep = ''), cex = .5, pos = 2)
      text(1, top - i, newdata_indices[i], cex = .5, pos = 2)
      text(X, cex = Cex * cexscale, top - i, labels = unlist(cur), family = 'monospace', pos = 4, col = linecolor)
    }
  }
}

#### -------- extractor functions  #########


SpineInfo = function(x) x@SpineInfo
Tandems = function(x) x@Tandems
SpinePaths = function(x) x@SpinePaths


hum_refhum_score = function(hum_score, type = 'COM'){
  refs = hum_score@Reference
  output = list()
  for(i in type){
    output[[i]] = refs$Records[grep(i, refs$Type)]
  }
  return(output)
}



