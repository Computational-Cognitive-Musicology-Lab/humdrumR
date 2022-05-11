setClass('hum_func', contains = c('function'))

hum_load = function(filename){
            hum_data = hum_file2hum_score(filename)
            homeenv = environment()

            new('hum_func',
                function(func = force, ...) {

                  if(!any(class(func) == 'hum_apply')) func = func:'each'

                  hum_data = func(hum_score)

                  if(is.null(hum_score)) return(NULL)


                  output = sys.function()
                  environment(output) = environment()

                  output
                }
            )

          }



setMethod('show',
          signature = c(object = 'hum_func'),
          function(object){
            show(getScore(object))
          })

setMethod('$',
          signature = c(x = 'hum_func'),
          function(x, name){
            environment(x)$hum_score = environment(x)$homeenv$hum_trace[[name]]

            x
          })

getScore = function(hum_func) {environment(hum_func)$hum_data}

setMethod('[',
          signature = c(x = 'hum_func'),
          function(x, i, j, drop = FALSE){
            if(missing(i)) i = NULL
            if(missing(j)) j = NULL

            hum_score = environment(x)$hum_score[i, j]


            new('hum_func',
                function(func = force, margin = 2, pathmethod = 3,  ...) {

                  output = hum_apply(hum_score, func, margin = margin, pathmethod = pathmethod, i = i, j = j, ...)

                  if(is.null(output)) return(NULL)

                  print(output)

                  if(inherits(output, 'hum_score')){
                    hum_score = output

                    output = sys.function()
                    environment(output) = environment()
                    invisible(output)

                  } else {
                    invisible(output)
                  }

                }
            )

          })


`:` = function(e1, e2) { UseMethod(':')}
`:.default` = function (e1,e2) { .Primitive(':')(e1, e2)}
`:.function` = function(e1, e2) {

  out = function(score){
   if(e1 == 'each') e1 = seq_len(ncol(score))

   score[ , e2] = e1(score[ , e2])
  }
  class(out) = append(class(out), 'hum_apply')
  out

}


##### ------------------ Indexing infixes   --------------------- ####

#
# `~.function` = function(a, b) {
#   func = a
#   indices = b
#   if(!is.list(indices)){
#     attr(func, 'j') = indices
#   } else {
#     if(length(indices)==1){
#       attr(func, 'j') = b[[1]]
#     } else {
#       attr(func, 'i') = b[[1]]
#       attr(func, 'j') = b[[2]]
#     }
#   }
#
#   func
# }
#
# `~.hum_func` = function(a, b){
#   indices = b
#   if(!is.list(indices)){
#     j = indices
#     i = NULL
#   } else { if(length(indices)==1){ j = b[[1]] ; i = NULL } else { i = b[[1]]  ; j = b[[2]] }}
#
#   hum_score = getScore(a)
#   rm(a, b)
#
#   new('hum_func',
#       function(func = force, margin = 2,  ...) {
#
#         output = hum_apply(hum_score, func, margin = margin, i = i, j = j, ...)
#
#         if(is.null(output)) return(NULL)
#
#         print(output)
#
#         if(inherits(output, 'hum_score')){
#           hum_score = output
#
#           output = sys.function()
#           environment(output) = environment()
#           invisible(output)
#
#         } else {
#           invisible(output)
#         }
#
#       }
#   )
# }
