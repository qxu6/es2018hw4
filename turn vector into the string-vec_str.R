#Function Name:vec_str()
#a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. 
#Author: Qingqing Xu

vec_str <- function(x, sep = ", ", and = ", and ") {
  if (length(x) > 1) {
    if (length(x) == 2)
      str_c(str_c(x[-length(x)], x[length(x)],sep=" and "))
    else
      str_c(str_c(x[-length(x)], collapse = sep),
            x[length(x)],
            sep = and)
  }
  else {
    x
  }
}

#example
vec_str(c("a", "b", "c"))
vec_str("")
vec_str("a")
vec_str(c("a", "b"))