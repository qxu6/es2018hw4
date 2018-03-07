#Function Name:extract_middle
#Use str_length() and str_sub() to extract the middle character from a string 
#Author: Qingqing Xu

extract_middle <- function(x){
  L <- str_length(x)
  l <- length(L)
  res <- c()
  y <- vector(mode="character", length=l) 
  i=1
  while (i<=l) {
    if (L[i] %% 2 ==0)
      y[i] <- str_sub(x[i], L[i]/2, L[i]/2+1)
    else
      y[i] <- str_sub(x[i], L[i]/2+0.5, L[i]/2+0.5)
    res <- c(res,y[i])
    i=i+1
  }
  res
}

#example
x <- c("a", "abc", "abcd", "abcde", "abcdef")
extract_middle(x)