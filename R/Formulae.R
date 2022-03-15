# This file defines preexisting formulae suitable for calls to with(in)Humdrum commands.


#' ------------------------------------------->             NEEDS DOCUMENTATION             <-------------------------------------------
#' Standard humdrumR formulae.
#'
#' `humdrumR` predefines a few [with(in)humdrum][withinHumdrum()] command combinations, as R formulae or lists of formulae.
#' 
#' + `ditto` calls [fillThru()] across pieces/spines/paths, replicating the classic humdrum toolkit ditto command.
#' @name humFormulae
NULL

#' @rdname humFormulae
#' @export
ditto <- c(do ~ fillThru(.), by ~ Spine ~ Piece ~ Path, recordTypes ~ 'Dd')
