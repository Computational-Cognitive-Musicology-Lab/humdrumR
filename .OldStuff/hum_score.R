
# -------------------Define hum_score S4 -----------------------------#

# A data.frame-like object which represents a humdrum score
# This file defines the hum_score class, a constructor function
# (which reads a humdrum file), index methods ([]), and a show method for that class.
# There are also a number of "helper" functions which are used by the construtor function.
# In order to work, one of the helper functions relies on the object hum_knownTandemInterpetations, which is defined
# in the higher level Rhumdrum script.

setClass('hum_score',
         slots = c(Data = 'data.frame',
                   NonData = 'list',
                   SpineInfo = 'data.frame',
                   Filename = 'character',
                   Reference = 'data.frame',
                   Tandems = 'data.frame',
                   SpinePaths = 'data.frame')
         )
#

hum_file2hum_score = function (filename){

  filerecords =readLines(filename)

  filerecords = hum_read_spinePaths(filerecords) #Deal with spine paths. Outputs a list with Records and SpinePaths
  SpinePaths = filerecords$SpinePaths
  filerecords = filerecords$Records

  names(filerecords) = seq_along(filerecords)


  NonData = list(
    GlobalComments    = grep('^!!$|^!![^!]+', filerecords, value = T),
    ReferenceRecords  = grep('^!!!.*'  , filerecords, value = T)
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



  Data = data.frame(
    do.call('rbind',
            strsplit(grep('^!!', filerecords, value = TRUE, invert = TRUE), split='\t')),
    stringsAsFactors = F
  )

  colnames(Data) = colnames(SpinePaths) = spine_namer(subset(Data, grepl('^\\*', Data[ , 1])))
  rownames(Data) = rownames(SpinePaths) = grep('^!!', filerecords, invert = TRUE)

  ###
  SpineInfo = data.frame(Names = colnames(Data), stringsAsFactors = F,
                         Interpretations = unlist(lapply(Data, function(spine) {grep('^\\*\\*', spine, value = T)[1]})),
                         Ntokens = unlist(lapply(Data, function(spine) {
                             sum(!grepl('^[.][.( <> )]*|^[=*!]', spine))})))

  ##

  Tandems = tandem_frame(Data)
  rownames(Tandems) = rownames(Data)

  #
  NonData$Interpretations = subset(Data, grepl('^\\*', Data[ , 1]))
  NonData$LocalComments   = subset(Data, grepl('^!'  , Data[ , 1]))
  NonData$Barlines        = subset(Data, grepl('^='  , Data[ , 1]))

  SpinePaths = subset(SpinePaths, !grepl('^[=*!]', Data[ , 1]))
  Tandems = subset(Tandems, !grepl('^[=*!]', Data[ , 1]))
  Data = subset(Data, !grepl('^[=*!]', Data[ , 1]))

  new('hum_score',
      Data = Data,
      Filename = filename,
      SpineInfo = SpineInfo,
      NonData = NonData,
      SpinePaths = SpinePaths,
      Tandems = Tandems,
      Reference = ReferenceFrame
  )

}



# -- Functions used by hum_file2hum_score

spine_namer = function(interpf){
  interpf = gsub(' <> .*', '', as.matrix(interpf))
  snames = vector('character', ncol(interpf))
  for(i in seq_len(ncol(interpf))){
    matches = !interpf[ , i] %in% interpf[ , -i] & !interpf[ , i] %in% c('*', '*^', '*v', '*-')
    if(any(matches)) snames[i] = interpf[which(matches)[1] , i]
  }
  snames[snames == ''] = paste('Spine_',toupper(letters[seq_len(sum(snames == ''))]),sep='')

  gsub('\\**', '', snames)


}

spine_vector_expand=function(x,index,n){
  # This funcition takes a vector and repeats the data at each index by n times.
  for(i in index){
    x[i]=paste(paste(rep(x[i],n),seq_len(n),sep='.'),collapse='__')
  }
  unlist(strsplit(x,split='__'))
}

hum_read_spinePaths=function(Records){
  #this currently can't handle triple *v


  spine_vectors = vector('list' , length(Records))
  spine_vector = as.character(seq_len(1 + str_count(grep('^\\*', Records, value = T)[1], '\t')))

  for (i in grep('^!!', Records, invert = T)) {
    if(any(table(gsub('\\..*','',spine_vector)) > 1)){
      Records[i] = paste(
        tapply(
          unlist(strsplit(Records[i], split='\t| <> ')), #tapply to
          str_replace(spine_vector,'\\..*',''), # tapply groups
          paste,collapse=' <> '
        ), #close tapply
        collapse='\t') # close paste
    }

    # if there's a spine path change:
    path_open  = grep('\\*\\^', strsplit(Records[i], split='\t')[[1]])
    path_close = grep('\\*v'  , strsplit(Records[i], split='\t| <> ')[[1]])
    if (length(path_open) > 0){
      spine_vector = spine_vector_expand(spine_vector, path_open, 2)
    }
    if(length(path_close) > 0){
      spine_vector[path_close] =  str_replace(spine_vector[path_close],'\\.[^.]*$','')
      spine_vector = unique(spine_vector)
    }
    spine_vectors[[i]] = spine_vector

  } #close for loop
  SpineFrame = as.data.frame(stringsAsFactors = FALSE,
                             do.call('rbind',lapply(spine_vectors,function(vec){
    if(!is.null(vec)){
      tapply(vec, gsub('\\..*', '', vec), paste, collapse = ' <> ')
      } else {NULL}})))

  list(Records = Records, SpinePaths = SpineFrame)
}

tandem_frame = function(df){
  known = unlist(hum_knownTandemInterpretations)

  interpf=as.data.frame(do.call('cbind',lapply(df,
         function(spine) {
           str_trim(gsub('  *',' ',
                         paste(
                           grep('\\*\\*', spine, value = TRUE),
                           sep=' ',
                           do.call('paste',
                                 lapply(X=known,function(k) {
                                   c('', grep(k, spine, value = TRUE))[1 + cumsum(grepl(k,spine))]
                                   })))))
         }
  )
  ),row.names = rownames(df),stringsAsFactors = F)

  colnames(interpf) = colnames(df)

  interpf
}

##-----hum_score methods ----------------------------------------------------------------------------##



## -- indexing -- #
source('~/Bridge/Research/Programming/Rhumdrum/hum_indexer.R', echo=TRUE)

setMethod('[',
          signature = c(x = 'hum_score', i = 'missing', j = 'missing'),
          function(x, i, j, drop = FALSE) {
            if(drop){ x@Data } else { x }
          }
)

hum_parse_indexes = function(i, j, x) {
  ##j prep
  if(inherits(j, 'hum_regex') || is.character(j)){
    if(is.character(j)) {
      namei = unique(unlist(lapply(j, function(regex) {grep(j, x@SpineInfo$Names,ignore.case = TRUE)})))
    } else {namei = c()}
    j = union(namei, which(hum_grep(j, score = x, margin = 1)))
  } else {
    if(is.logical(j)){
      if(length(j) != ncol(x@Data)){
        j = rep(j, length.out = ncol(x@Data))
      }
      j = which(j)
    }
  }
  j = j[j <= ncol(x@Data)]
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
      if(length(i) != nrow(x@Data)){
        i = rep(i, length.out = nrow(x@Data))
      }
      i = which(i)
    }
  }

  i = i[i <= nrow(x@Data)]
  if(length(i) == 0) {
    message("\nNo matching rows in row index.\n")
    i = 0
  }

  list(i = i, j = j)

}


setMethod('[',
          signature = c(x = 'hum_score'),
          function(x, i, j, drop = FALSE) {
            if(missing(i) || is.null(i)) i = seq_len(nrow(x))
            if(missing(j) || is.null(j)) j = seq_len(ncol(x))

            indices = hum_parse_indexes(i, j, x)
            i = indices$i
            j = indices$j

            fileIndices = as.numeric(rownames(x@Data))[i]
            if(any(i == 1)) { #pulls in records from before first data point
              fileIndices = append(1, fileIndices)
            }
            if(length(fileIndices) > 0 ){
              irange = min(fileIndices):max(fileIndices)
            } else {
              irange = 0
            }


            #actual indexing
            x@Data = x@Data[i, j, drop = FALSE]
            if(drop){return(x@Data)}

            x@Tandems = x@Tandems[i, j, drop = FALSE]
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

            attr(x, 'i') = append(attr(x, 'i'), list(i))
            attr(x, 'j') = append(attr(x, 'j'), list(j))
            x
          })


## -------

`[<-.hum_score` = function(x, i, j, value){
  if(missing(i)) i = seq_len(nrow(x))
  if(missing(j)) i = seq_len(ncol(x))

  indices = hum_parse_indexes(i, j, x)
  i = indices$i
  j = indices$j

  if(inherits(value, 'hum_score')) value = value@Data

  x@Data[i, j] <- value

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
  ind_score = ind_score@Data

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


spine_split = function(spine, multistops = TRUE) {
  spinelist = strsplit(spine, split = ' <> ')
  if(multistops) spinelist = lapply(spinelist, strsplit, split = ' ')
  spinelist = lapply(spinelist, function(x) {lapply(x, function(z) {z[z=='.'] =NA; z})})
  spinelist
}

spine_zip = function(spinelist){

  spine =  lapply(spinelist,
                  function(tokenlist) {
                    paste(sapply(tokenlist, paste, sep = ' '),  collapse = ' <> ')
                    }
                  )
  unlist(spine)
}

hum_apply_spinelist = function(spinelist, func, ...) {
  lapply(spinelist,
         function(tokenlist) {
           lapply(tokenlist, func, ...)
         }
  )
}

spine_pathize = function(func, method = 1, spinepath = NULL){
  func = match.fun(func)

  function(spine, ...) {
    if(method == 1) {
      output = spine_zip(hum_apply_spinelist(spine_split(spine), func, ...))
    }
    if(method == 2) {
      output = func(unlist(spine_split(spine)), ...)
    }
    if(method ==3) {
      output = spine_paths(func, spine, spinepath, ...)
    }

    output
  }
}

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
            DIM = dim(object@Data)
            sinfo = object@SpineInfo

            cat('\n')
            cat('Humdrum score with', DIM[1], 'data records in', DIM[2],'spines:\n\n')

            for(i in seq_len(DIM[2])){
              cat("\tSpine ",i,"\t'",sinfo$Names[i],"':\n",sep = '')
              cat('\t\t\tInterpretation =', sinfo$Interpretations[i],'\n')
              cat('\t\t\tNumber of data tokens =', sinfo$Ntokens[i])

              tokens = table(grep('^[.][.( <> )*]', invert = TRUE, value = TRUE, object@Data[ , i]))
              cat('  (',length(tokens), ' unique tokens)\n', sep='')
              # cat('\n')

            }

          })

setMethod('ncol', signature = c(x = 'hum_score'), function(x) ncol(x@Data))
setMethod('nrow', signature = c(x = 'hum_score'), function(x) nrow(x@Data))
setMethod('dim' , signature = c(x = 'hum_score'), function(x)  dim(x@Data))


setMethod('show',
          signature = c(object = 'hum_score'),
          function(object) {
            if(!any(dim(object@Data) == 0)) {
              hum_plothum_score(object, Nscreens = 1)
            } else {
              cat("\n Empty hum_score.\n")
            }
          })


hum_plothum_score = function(object, Nscreens = NULL, RperScreen = 50, Cex = .8){
  #Prepare data
  data_lines = apply(object@Data, 1, paste, collapse = '\t')
  data_indices = as.numeric(rownames(object@Data))

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
  spinenames = object@SpineInfo$Names

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

      text(1, top - i, paste('[', recordIndices[i], ']', sep = ''), cex = .5, pos = 2)
      text(X, cex = Cex * cexscale, top - i, labels = unlist(cur), family = 'monospace', pos = 4, col = linecolor)
    }
  }
}

#### -------- extractor functions  #########

getScore = function(hum_score) {environment(hum_score)$hum_score}
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



