library(Rcpp)
ditto = function(vec, mark = NULL){

  # function(vec) {
  out = switch(class(vec),
         'character' = CdittoChar(vec),
         'logical'   = as.logical(CdittoLog(vec)),
         'integer'   = CdittoInt(vec),
         'numeric'   = CdittoNum(vec),
         'factor'    = CdittoInt(vec)
  )

  if(!is.null(mark)) out[is.na(vec) | vec == '.'] = out[is.na(vec) | vec =='.'] + mark

  out
  # }
}

cppFunction('CharacterVector CdittoChar(CharacterVector vec) {

            CharacterVector newvec = clone(vec);

            int n = vec.size();

            for(int i = 1; i < n; ++i) {
              if (CharacterVector::is_na(vec[i])) {
                newvec[i] = newvec[i - 1];
              }
            }

            return(newvec);

            }')

cppFunction('LogicalVector CdittoLog(LogicalVector vec) {

            LogicalVector newvec = clone(vec);

            int n = vec.size();

            for(int i = 1; i < n; ++i) {
              if (LogicalVector::is_na(vec[i])) {
                newvec[i] = newvec[i - 1];
              }
            }

            return(newvec);

            }')



cppFunction('IntegerVector CdittoLog(IntegerVector vec) {

            IntegerVector newvec = clone(vec);

            int n = vec.size();

            for(int i = 1; i < n; ++i) {
              if (IntegerVector::is_na(vec[i])) {
                newvec[i] = newvec[i - 1];
              }
            }

            return(newvec);

            }')


cppFunction('NumericVector CdittoNum(NumericVector vec) {

            NumericVector newvec = clone(vec);

            int n = vec.size();

            for(int i = 1; i < n; ++i) {
              if (NumericVector::is_na(vec[i])) {
                newvec[i] = newvec[i - 1];
              }
            }

            return(newvec);

            }')


