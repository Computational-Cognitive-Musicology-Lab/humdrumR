
`|` = function(e1, e2) { UseMethod('|')}
`|.default` = function(e1, e2) {.Primitive("|")(e1, e2)  }
`|.function` = function(e1, e2) { function(...) e2(e1(...)) }


f = function(func, ...) {
 function(x) func(x, ...)
}

`%args%` = function(func, args) {
  if(length(args) == 0)  function(x) do.call(deparse(substitute(func)), list(x))

  function(x) {
    arglist = list(x)
    arglist = append(arglist, args, after = 1)
    do.call(deparse(substitute(func)), arglist)
  }
  }

