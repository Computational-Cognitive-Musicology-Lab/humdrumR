commit a56388dda6e8d472f722abe04dc1bf89cd22b110
Author: ncondits3 <ncondits3@gatech.edu>
Date:   Fri Sep 27 11:38:30 2024 -0600

    Changed 'selecter' to 'parentSelecter'

diff --git a/R/Metric.R b/R/Metric.R
index 3008cc66..24070c46 100644
--- a/R/Metric.R
+++ b/R/Metric.R
@@ -1287,7 +1287,7 @@ metcount.default <- function(dur, meter = duple(5), level = tactus(meter), picku
   checks(pickup, xnull | (xlogical & xmatch(dur)), seealso = c("?metcount", 'the rhythm vignette'))
   
   met <- .metric(dur = dur, meter = meter, pickup = pickup, groupby = groupby, parseArgs = parseArgs, Exclusive = Exclusive, 
-                 selecter = min,
+                 parentSelecter = min,
                  remainderSubdivides = remainderSubdivides, callname = 'metcount', ...)
   
   counts <- met$Counts
@@ -1347,11 +1347,11 @@ metsubpos <- function(dur, meter = duple(5), pickup = NULL, deparser = duration,
 
 
 .metric <- function(dur, meter = duple(5),  groupby = list(), pickup = NULL, ..., 
-                    selecter = max,
+                    parentSelecter = max,
                     parseArgs = list(), Exclusive = NULL, remainderSubdivides = TRUE, callname = '.metric') {
   
   if (length(unique(meter)) > 1L) {
-    return(.metrics(dur, meter = meter, pickup = pickup, selecter = selecter,
+    return(.metrics(dur, meter = meter, pickup = pickup, parentSelecter = parentSelecter,
                     groupby = groupby, parseArgs = parseArgs, Exclusive = Exclusive, remainderSubdivides = remainderSubdivides,
                     callname = callname, ...))
   }
@@ -1382,7 +1382,7 @@ metsubpos <- function(dur, meter = duple(5), pickup = NULL, deparser = duration,
                             spn <= spans & 
                             spn %divides% spans &
                             !(nbeats[i] > 1 & nbeats > 1)
-                          if (any(hits)) selecter(which(hits)) else 0L
+                          if (any(hits)) parentSelecter(which(hits)) else 0L
                         }))
   
   counts <- do.call('cbind', 
@@ -1425,7 +1425,7 @@ metsubpos <- function(dur, meter = duple(5), pickup = NULL, deparser = duration,
   remainder <- c(remainders[cbind(seq_len(nrow(remainders)), lowestLevel)])
     
   # remove redundant counts
-  if (identical(selecter, max)) {
+  if (identical(parentSelecter, max)) {
     counts[sweep(col(counts), 1L, lowestLevel, '>')] <- 0L
     counts[sweep(col(counts), 1L, parents[lowestLevel], '>') & !sweep(col(counts), 1L, lowestLevel, '==')] <- 0L
     
