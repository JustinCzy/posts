#Function for rolling correlation
fun_roll_corr <- function (ret_1, ret_2, per, width) {
  rs <- na.exclude(merge(ret_1, ret_2))
  a <- list(to.period(cumprod(rs[,1] + 1), per, OHLC = FALSE))
  a[[2]] <- to.period(cumprod(rs[,2] + 1), per, OHLC = FALSE)
  a <- lapply(a, Return.calculate)
  corr <- xts(as.numeric(roll_cor(a[[1]], a[[2]], width = width)), order.by = index(a[[1]]))
  return(corr)
}

#Get prices (weekly)
spx <- getSymbols("SPHB", from = as.Date("1900-01-01"), auto.assign = FALSE)
spx <- to.period(spx[,6], "weeks", OHLC = FALSE)
btc <- getSymbols("BTC-USD", from = as.Date("1900-01-01"), auto.assign = FALSE)[,6]

#Merge
comp <- na.exclude(merge(spx, btc))
comp <- Return.calculate(comp)

#Calculate correlations
corrs <- list()
widths <- c(seq(4, 12, 4), seq(4, 12, 2), seq(6, 16, 2))
pers <- c(rep("week", 3), rep("month", 5), rep("quarter", 6))
for (p in seq_along(pers)) {
  cr <- per_corr(comp[,1], comp[,2], pers[p], widths[p])
  colnames(cr) <- "comp"
  cr <- merge(s_b, cr, fill = na.locf)
  cr <- cr[, 3]
  cr <- fun_to_df(cr)
  cr$per <- paste0(widths[p], "_", pers[p])
  corrs[[p]] <- cr
}
corrs <- do.call(rbind, corrs)
corrs$per <- gsub("_", "-", corrs$per)
corrs$per <- factor(corrs$per, levels = unique(corrs$per))

#Sort for plot
corrs <- na.exclude(corrs)
corrs$per <- factor(corrs$per, levels = unique(corrs$per))


#GGPLOT ANI corrs
ani_corr <- ggplot(corrs, aes(x = date, y = comp, color = per)) +
  geom_line(size = 2, color = "darkblue") +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Correlation of Changes", limits = c(-1, 1)) +
  labs(title = "{closest_state} Correlation", subtitle = "Rolling Correlation of SPHB to Bitcoin (Friday to Friday)", caption = paste0("Sources: Yahoo ", format.Date(min(corrs$date), "%m/%d/%y"), " to ", format.Date(max(corrs$date), "%m/%d/%y"))) +
#  brn_theme +
  theme(legend.position = "none", plot.title = element_text(hjust = 1, size = 24), plot.subtitle = element_text(size = 18)) +
  transition_states(states = per, transition_length = 3, state_length = 5) +
  shadow_mark(alpha = .2, size = .7)

anim_save("corr_ani_btc_tsla.gif", ani_corr, height = 600, width = 800, nframe = 200)


#Still GGPLOT corrs
ggplot(corrs, aes(x = date, y = comp, color = per)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0, color = "grey20", size = 1) +
  scale_x_date(name = NULL, date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(name = "Correlation of Changes", limits = c(-1, 1)) +
  scale_color_viridis_d(end = .95) +
  facet_wrap(~per, nrow = 5) +
  labs(title = "Rolling Correlation of S&P 500 to 10-year Treasuries", caption = paste0("Sources: Yahoo and Fred, ", format.Date(min(corrs$date), "%m/%d/%y"), " to ", format.Date(max(corrs$date), "%m/%d/%y"))) +
  brn_theme +
  theme(legend.position = "none")
