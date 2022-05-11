`|`=function(a,b) UseMethod('|')
`|.default` = function(a,b) .Primitive('|')(a,b)
`|.function` = function(a,b) function(...) b(a(...))


`:`=function(a,b) UseMethod(':')
`:.default` = function(a,b) .Primitive(':')(a,b)
`:.function` = function(a,b) {
 if(is.function(b)) {
   return(b(a))
 } else {
  function(x) do.call(a,append(list(x),b))
 }
}

skip = function(targets) {

  function(func) {

    function(input) {
    hits = grep(paste(targets,collapse='|'), input)
    nothits = grep(paste(targets,collapse='|'), input, invert = TRUE)

    innerout = func(input[nothits])

    if(!length(innerout) == length(nothits)) return(innerout)

    output = input
    output[nothits] = innerout
    output

  }
}
}

nth = function(x) {
 paste(head(x,-1),tail(x,-1),sep=' ')
}

segments = function(boundary = 'r'){
 function(func) {
  function(input) {


  }
 }

}


test = function() {
 data = rnorm(100,50,10)
 function(func) {
  func(data)
 }
}