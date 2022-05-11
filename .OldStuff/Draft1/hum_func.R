setClass('hum_func', contains = c('function'))

hum_load = function(filename){
            hum_score = hum_file2hum_score(filename)
            homeenv = environment()

            new('hum_func',
                function(func = force, margin = 2, pathmethod = 3,  ...) {

                  hum_score = hum_apply(hum_score, func, margin = margin,  ...)

                  if(is.null(hum_score)) return(NULL)



                  rm(margin, pathmethod)
                  homeenv = homeenv
                  output = sys.function()
                  environment(output) = environment()

                  output
                }
            )

          }


save = function(hum_score, homeenv){ homeenv$hum_score = hum_score }

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



##### ------------------ Indexing infixes   --------------------- ####


`~.function` = function(a, b) {
  func = a
  indices = b
  if(!is.list(indices)){
    attr(func, 'j') = indices
  } else {
    if(length(indices)==1){
      attr(func, 'j') = b[[1]]
    } else {
      attr(func, 'i') = b[[1]]
      attr(func, 'j') = b[[2]]
    }
  }

  func
}

`~.hum_func` = function(a, b){
  indices = b
  if(!is.list(indices)){
    j = indices
    i = NULL
  } else { if(length(indices)==1){ j = b[[1]] ; i = NULL } else { i = b[[1]]  ; j = b[[2]] }}

  hum_score = getScore(a)
  rm(a, b)

  new('hum_func',
      function(func = force, margin = 2,  ...) {

        output = hum_apply(hum_score, func, margin = margin, i = i, j = j, ...)

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
}
