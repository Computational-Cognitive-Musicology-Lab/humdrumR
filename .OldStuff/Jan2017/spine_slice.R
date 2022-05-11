library(stringr)


apply_math = function(spine, f, ...) {
 nums = as.numeric(str_match(spine, '[0-9]*|[0-9]*\\.[0-9]+') )
 nums = f(nums, ...)

 locs = str_locate(spine, '[0-9]*|[0-9]*\\.[0-9]+')

 str_sub(spine, locs) = as.character(nums)

 spine

}