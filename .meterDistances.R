

x <- cbind(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1), c(0,1,0,1,0,1,0,1))
x[] <- as.character(x)
x <- apply(x,1, paste,collapse='')

mat <- local({
  mat <- matrix('0', nrow = 4, ncol = 4) 
  colnames(mat) <- rownames(mat) <- c('-', '0', '1', '2') 
  
  mat['-', '-'] <- '0'
  mat['-', '0'] <- '-'
  mat['-', '1'] <- '2'
  mat['-', '2'] <- '-'
  
  mat['0', '-'] <- '-'
  mat['0', '0'] <- '0'
  mat['0', '1'] <- '1'
  mat['0', '2'] <- '0'
  
  mat['1', '-'] <- '0'
  mat['1', '0'] <- '-'
  mat['1', '1'] <- '2'
  mat['1', '2'] <- '1'
  
  mat['2', '-'] <- '-'
  mat['2', '0'] <- '0'
  mat['2', '1'] <- '1'
  mat['2', '2'] <- '2' 
  mat
})

op <- function(x, y) {
  x <- strsplit(x, split = '')[[1]]
  y <- strsplit(y, split = '')[[1]]
  
   
  

  paste(mat[cbind(x,y)], collapse = '')
}
ops <- Vectorize(op, vectorize.args = c('x','y'))

sh <- function(x, twozero = TRUE, type = 'p') {
  x <- strsplit(x, split = '')
  plot.new()
  plot.window(xlim = c(-2.5,2.5), ylim = c(-2.4,2.4))
  axis(1, -1:2, tick = FALSE)
  axis(2, -1:2, tick = FALSE)
  abline(0,0, lty='dashed')
  graphics::segments(x0=c(0),x1=0,y0=-1,y1=2.5, lty='dashed')
  
  X <- Y <- Z <- c()
  for (i in 1:length(x)) {
    cur <- rev(x[[i]])
    
    cur <- c('-' = -1, '0' = 0, '1' = 1, '2' = if (twozero) 0 else 2)[cur]
    X <- c(X, cur[1])
    Y <- c(Y, cur[2] + cur[3] * .06)
    Z <- c(Z, c('red', 'black','blue', 'purple')[cur[3] + 2])
  }
  
  points(X, Y, pch = 16, col = Z, cex=3, type = type)

  
}
inv <- function(x) {
  
  gsub('X', '1', gsub('1', '-', gsub('-','X',x)))
}

# X - 0 = X
# Y - 0 = Y
# Y - X = A

# Y - X = A
# Y = A + X
# Y - A = X
# 
library(humdrumR)
opx <- function(x, y){
  ops(x, inv(y))
}

dif <- outer(x,x,ops)
eighths <- lag(dif[((col(dif) - 1) %% 8) == (row(dif) %%8)], 1 )
quarters <- lag(dif[((col(dif) - 2) %% 8) == (row(dif) %%8)],2)
dottedquarters <- lag(dif[((col(dif) - 3) %% 8) == (row(dif) %%8)],3)
halfs <- lag(dif[((col(dif) - 4) %% 8) == (row(dif) %%8)],4)

syncquarters <- quarters[c(2,4,6,8)]
notsyncquarters <- quarters[c(1,3,5,7)]

syncdotted <-  dottedquarters[c(1,3,5,7)]
notsyncdotted <- dottedquarters[c(2,4,6,8)]

dur <- function(x ) {
  x <- strsplit(x, split = '')
  
  sapply(x, \(cur) {
    cur[cur == '-'] <- '-1'
    cur[cur == '2'] <- '0'
    cur <- as.numeric(cur)
    

      sum(cur * c(4,2,1))
      
   
  })
}

par(mfrow=c(4,2))
sh(c('011','12-', '--0'))
sh(c('10-','211', '--2'))
sh(c('1-1','-0-', '010'))
sh(c('12-','--1', '012'))

sh(c('211','-2-', '1-0'))
sh(c('-0-','011', '1-2'))
sh(c('--1','10-', '210'))
sh(c('-2-','1-1', '212'))



# x + a