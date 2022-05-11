source('ditto.R')


#
`+` = function(e1, e2) { UseMethod('+')}
`+.default` = function(e1, e2) {
  if(missing(e2)) .Primitive("+")(e1) else .Primitive("+")(e1, e2)
}

`-` = function(e1, e2) { UseMethod('-')}
`-.default` = function(e1, e2) {
  if(missing(e2)) .Primitive("-")(e1) else .Primitive("-")(e1, e2)
}

`-.character` = function(e1, e2) {
  str_replace_all(e1, e2 , '')
}

`-.hum_score` = function(e1, e2) {
  e1@Data = e1@Data - e2
  e1
}

`-.hum_func` = function(e1, e2){
  humfunc = e1
  pattern = ifelse(str_detect(e2, '^\\*'),
                   hum_knownExclusiveInterpretations[[e2 - '\\*+']],
                   e2
  )

  humfunc(`-`, e2 = pattern, pathmethod = 1)
}

`==.hum_func` = function(e1, e2) {
  humfunc = e1
  comparison = e2

  humfunc(`==`, e2 = comparison, pathmethod = 1)
}

`!=.hum_func` = function(e1, e2) {
  humfunc = e1
  comparison = e2

  humfunc(`!=`, e2 = comparison, pathmethod = 1)
}

`%strip%` = function(e1, e2) {
  humfunc = e1
  pattern = ifelse(str_detect(e2, '^\\*'),
                   hum_knownExclusiveInterpretations[[e2 - '\\*+']],
                   e2
  )

  humfunc(str_extract, pattern = pattern)

}

`%graft%` = function(e1, e2){
  humfunc = e1

  humfunc(paste, ...= e2, pathmethod = 1)

}


cppFunction('CharacterVector ApplyCollapse(CharacterMatrix matrix, String sep) {
            int nrows = matrix.nrow();
            int ncols = matrix.ncol();

            CharacterVector newvec = no_init(nrows);

            for(int i = 0; i < nrows; ++i) {
              String newstring = "";

              CharacterVector curvec = matrix(i, _);

              for(int j = 0; j < ncols; ++j) {
                newstring += curvec[j];
                if(j != (ncols - 1)) {
                newstring += sep;
                }
              }

             newvec[i] = newstring;
            }

            return(newvec);

            }')



ngramx = function(vec, n = 2){

    vec = vec[!is.na(vec) & vec !='.']

    out = suppressWarnings(matrix(rep(vec, n), nrow = length(vec) + 1))
    out = head(out, -n)

    out
    ApplyCollapse(out, sep = ' ')
#
}



