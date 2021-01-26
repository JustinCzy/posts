#Tickers from DJIA
dow_tickers <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "WBA", "T", "GE", "AA", "BAC", "HPQ", "C", "AIG", "MO", "HON", "IP", "GT", "OI", "NAV", "X", "AMGN", "CRM")
#Remove UTX
tickers <- dow_tickers[which(dow_tickers != "UTX")]

rets <- list()
for (n in seq_along(tickers)) {
  r <- getSymbols(tickers[n], from = as.Date("1960-01-01"), auto.assign = FALSE)[, 6]
  r <- Return.calculate(r)
  r[1] <- 0
  r <- cumprod(1 + na.exclude(r))
  rets[[n]] <- r
  }

rets <- do.call(merge, rets)
colnames(rets) <- gsub(".Adjusted", "", colnames(rets))

rets_df <- fun_to_long(fun_to_df(rets))
rets_df$name <- gsub(".Adjusted", "", rets_df$name)

ggplot(rets_df, aes(date, value, color = name)) +
  geom_label_repel(data = filter(rets_df, date == max(date)), aes(label = name), fill = back_col, nudge_x = 500) +
  geom_line(size = 1.5) +
  scale_y_log10(name = NULL, label = scales::label_dollar()) +
  scale_x_date(name = NULL, limits = c(min(rets_df$date), max(rets_df$date) + 800)) +
  labs(color = NULL, title = "Growth of $1 since 1990") +
  brn_theme +
  theme(legend.position = "bottom", legend.justification = "center")

#Calculate Drawdowns
dds <- Drawdowns(Return.calculate(rets))

dd_len <- list()
for (co in 1:ncol(dds)) {
  d <- na.exclude(dds[, co])
  in_d <- which(d < 0 & lag.xts(d, 1) == 0)
  in_d <- in_d[which(in_d - in_d[-1] != -1)]
  dr <- list()
  for (row in seq_along(in_d)) {
    sta <- index(d[in_d[row]])
    win <- window(d, start = sta)
    sto <- first(index(win[which(win == 0)]))
    if (is.na(sto) == TRUE) {sto <- max(index(d))}
    win <- window(win, end = sto)
    win <- data.frame(start = sta, stop = sto, min = min(win), years = time_length(interval(start = sta, end = sto), unit = "years"))
    dr[[row]] <- win
  }
  dr <- do.call(rbind, dr)
  dr$ticker <- colnames(d)
  dd_len[[co]] <- dr
}
dd_len <- do.call(rbind, dd_len)

dd_max <- list()
for (n in seq_along(unique(dd_len$ticker))) {
  d <- filter(dd_len, ticker == unique(dd_len$ticker)[n])
  dd_max[[n]] <- d[which.max(d$years),]
}
dd_max <- do.call(rbind, dd_max)

ggplot(dd_max, aes(x = years, y = min)) +
  geom_point() +
  geom_label_repel(aes(label = ticker), fill = 
      ifelse(dd_max$stop == max(dd_max$stop), "tomato", back_col)) +
  scale_x_continuous(limits = c(0, 25), name = "Length of Longest Drawdown in Years") +
  scale_y_continuous(name = "Largest Loss during Longest Drawdown", label = scales::label_percent()) +
  labs(title = "Length and Depth of Longest Drawdown for DJIA Stocks", subtitle = "Red means stock is still in longest drawdown", caption = paste("NAV not shown (>40 year and 98% drawdown), Source: Yahoo,", format.Date(min(index(dds)), "%m/%d/%y"), "to", format.Date(max(index(dds)), "%m/%d/%y"))) + 
  brn_theme


  