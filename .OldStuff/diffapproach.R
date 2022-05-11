library(stringr)

setClass('hum_frame',
         contains = 'list')

setGeneric('hum_frame',
           function(df){
            standardGeneric('hum_frame')
           })


setMethod('hum_frame',
          signature = c(df = 'data.frame'),
          function(df){
            output = vector('list', ncol(df))
            names(output) = colnames(df)

            output[] = lapply(df,
                            function(vec) {
                              vec = lapply(str_split(vec, pattern = ' <> '), as.list)
                              names(vec) = rownames(df)

                              vec = lapply(vec,
                                           function(tokenlist) {
                                             lapply(tokenlist,
                                                    function(token){
                                                      unlist(str_split(token, pattern = ' '))
                                                      }
                                                    )
                                             }
                                           )
                              vec

                            })
            new('hum_frame',output)
          })

#
setMethod('show',
          signature = c(object = 'hum_frame'),
          function(object){
            print(as.data.frame(lapply(object,
                                 function(column) {
                                   unlist(lapply(column,
                                                 function(tokenlist) {
                                                   paste(collapse = ' <> ',
                                                         lapply(tokenlist,
                                                                function(token) {
                                                                  paste(token, collapse = ' ')
                                                                }
                                                         )
                                                   )
                                                 }
                                   )
                                   )
                                 }
            )))



          }
)


setMethod('[',
          signature = c(x = 'hum_frame'),
          function(x, i, j, drop = FALSE){
            if(missing(i)) i = seq_along(x[[1]])
            if(missing(j)) j = seq_len(length(x))

            x@.Data = x@.Data[j]

            x@.Data = lapply(x@.Data, '[', i)

            x
          }
)

flatten = function(func){

  function(spine){
   spine = unlist(lapply(spine,unlist))
   func(spine)
  }
}


humdrumize = function(func){
 function(spine,...) {
            spine[[1]] = lapply(spine[[1]],
                          function(tokenlist) {
                                  lapply(tokenlist,
                                         function(token) {
                                           func(token,...)
                                         }
                                  )
                            }
                          )
  spine
 }
}
