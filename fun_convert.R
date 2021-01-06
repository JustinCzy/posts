
######################## CONVERT TO DF #####################
fun_to_df <- function (xts) {
  coln <- colnames(xts)
  df <- data.frame("date" = index(xts), matrix(as.numeric(xts), ncol = ncol(xts)))
  colnames(df)[2:ncol(df)] <- coln
  return(df)
}
######################## CONVERT TO XTS #####################
fun_to_xts <- function (df) {
  coln <- colnames(df)[-1]
  xt <- xts(df[, -which(colnames(df) == "date")], order.by = df[, "date"])
  return(xt)
}
######################## CONVERT DF TO LONG #####################
fun_to_long <- function (df) {
  lg <- pivot_longer(df, cols = colnames(df)[2:ncol(df)])
  lg$name <- factor(lg$name, levels = colnames(df)[2:ncol(df)])
  return(lg)
}  
