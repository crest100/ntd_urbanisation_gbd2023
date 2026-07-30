# Global NTD Burden x Urbanization Gradient Analysis
# GBD 2023: NTD DALYs (age-standardized rate), 204 countries, 1990-2023
# Urbanization: World Bank SP.URB.TOTL.IN.ZS
# China folic acid policy evaluation (2009)

options(scipen=5, stringsAsFactors=FALSE)
outdir <- "C:/Users/dsc88/Desktop/NTD_China_GBD2021/output"
datadir <- "C:/Users/dsc88/Desktop/NTD_China_GBD2021/data"
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)

# ---- 1. Load data ----
cat("=== Loading data ===\n")

gbd <- read.csv(file.path(datadir, "IHME-GBD_2023_DATA-04b137c8-1.csv"), stringsAsFactors=FALSE)
colnames(gbd)[6]  <- "loc"
colnames(gbd)[15] <- "yr"
colnames(gbd)[16] <- "val"

cat(sprintf("GBD: %d rows, %d countries, %d years\n",
    nrow(gbd), length(unique(gbd$loc)), length(unique(gbd$yr))))

wb <- read.csv(file.path(datadir, "wb_urbanization.csv"), stringsAsFactors=FALSE)
map_df <- read.csv(file.path(datadir, "country_name_mapping.csv"), stringsAsFactors=FALSE)

# Merge urbanization
gbd$urb_pct <- NA
for (i in 1:nrow(gbd)) {
  nm <- gbd$loc[i]
  wb_name <- map_df$wb_name[match(nm, map_df$gbd_name)]
  if (!is.na(wb_name) && nchar(wb_name) > 0) {
    val <- wb$urban_rate[match(wb_name, wb$country_name)]
    if (!is.na(val)) gbd$urb_pct[i] <- val
  }
}
# Manual assignments
gbd$urb_pct[gbd$loc == "Cook Islands"] <- 75.0
gbd$urb_pct[gbd$loc == "Niue"] <- 44.0
gbd$urb_pct[gbd$loc == "Taiwan"] <- 78.0
gbd$urb_pct[gbd$loc == "Tokelau"] <- 0.0

# Classify into quintiles
quints <- quantile(unique(gbd$urb_pct), probs=seq(0,1,0.2), na.rm=TRUE)
cat("Urbanization quintiles:\n")
print(round(quints, 1))

gbd$urb_g <- cut(gbd$urb_pct, breaks=quints,
                 labels=c("Q1_Lowest","Q2_Low","Q3_Middle","Q4_High","Q5_Highest"),
                 include.lowest=TRUE)
cat("Obs per group:\n")
print(table(gbd$urb_g[!duplicated(gbd$loc)]))

china <- gbd[gbd$loc == "China", ]
years <- 1990:2023

# ---- 2. Figure 1: Global trends by urbanization group ----
cat("\n=== Figure 1 ===\n")
grps <- levels(gbd$urb_g)
cols <- c("#2166AC","#67A9CF","#D1E5F0","#F4A582","#B2182B")

grp_means <- sapply(grps, function(g) {
  sapply(years, function(y) mean(gbd$val[gbd$yr == y & gbd$urb_g == g], na.rm=TRUE))
}, simplify="array")
grp_sds <- sapply(grps, function(g) {
  sapply(years, function(y) sd(gbd$val[gbd$yr == y & gbd$urb_g == g], na.rm=TRUE))
}, simplify="array")
grp_ns <- sapply(grps, function(g) {
  sapply(years, function(y) sum(!is.na(gbd$val[gbd$yr == y & gbd$urb_g == g])))
}, simplify="array")
grp_ses <- grp_sds / sqrt(grp_ns)
china_mn <- sapply(years, function(y) mean(china$val[china$yr == y], na.rm=TRUE))
china_sd <- sapply(years, function(y) sd(china$val[china$yr == y], na.rm=TRUE))
china_se <- china_sd / sqrt(sapply(years, function(y) sum(!is.na(china$val[china$yr == y]))))

png(file.path(outdir, "fig1_global_trends.png"), width=10, height=7, units="in", res=300)
par(mar=c(5,5,2,2))
plot(0,0,type="n", xlim=c(1990,2023), ylim=c(0, max(grp_means + 1.96*grp_ses, na.rm=TRUE)),
     xlab="Year", ylab="NTD DALYs per 100,000 (age-standardized)",
     main="Global NTD Burden Trends by Urbanization Level")
for (i in seq_along(grps)) {
  upper <- grp_means[,i] + 1.96 * grp_ses[,i]
  lower <- grp_means[,i] - 1.96 * grp_ses[,i]
  polygon(c(years, rev(years)), c(upper, rev(lower)),
          col=adjustcolor(cols[i], 0.2), border=NA)
  lines(years, grp_means[,i], col=cols[i], lwd=2.5)
}
upper_c <- china_mn + 1.96 * china_se
lower_c <- china_mn - 1.96 * china_se
polygon(c(years, rev(years)), c(upper_c, rev(lower_c)),
        col=adjustcolor("black", 0.1), border=NA)
lines(years, china_mn, col="black", lwd=3, lty=2)
legend("topright", legend=c(grps, "China"), col=c(cols, "black"),
       lwd=2.5, lty=c(rep(1,5),2), cex=0.9, bg="white")
dev.off()
cat("Done\n")

# ---- 3. Figure 2: Country ranking ----
cat("\n=== Figure 2 ===\n")
d23 <- gbd[gbd$yr == 2023, ]
d23 <- d23[order(d23$val, decreasing=TRUE), ]
top20 <- head(d23, 20)
bot20 <- tail(d23, 20)

png(file.path(outdir, "fig2_top20_ranking.png"), width=10, height=8, units="in", res=300)
par(mar=c(4,14,3,8))
bp <- barplot(rev(top20$val), names.arg=rev(top20$loc), horiz=TRUE, las=1,
              xlab="NTD DALYs per 100,000 (2023)", cex.names=0.8,
              main="Top 20 Countries with Highest NTD Burden (2023)",
              col=cols[rev(as.numeric(top20$urb_g))], border=NA)
legend("right", inset=c(-0.18,0), xpd=TRUE,
       legend=c("Q1 (lowest urbanisation)","Q2","Q3","Q4","Q5 (highest)"),
       fill=cols, cex=0.85, bty="n", title="Urbanisation quintile")
dev.off()
cat("Done\n")

# ---- 4. Figure 3: Heatmap (pheatmap) ----
cat("\n=== Figure 3 ===\n")
suppressPackageStartupMessages(library(pheatmap, quietly=TRUE))

top50 <- head(d23, 50)$loc
hm <- matrix(NA, nrow=50, ncol=34)
for (i in 1:50) {
  sub <- gbd[gbd$loc == top50[i], ]
  hm[i,] <- sapply(years, function(y) mean(sub$val[sub$yr == y], na.rm=TRUE))
}
rownames(hm) <- top50; colnames(hm) <- as.character(years)

# (no log transform — use raw rate per 100k)

# Custom colour palette (white → light red → dark red)
pal <- colorRampPalette(c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15"))(100)

png(file.path(outdir, "fig3_heatmap.png"), width=14, height=11, units="in", res=300)
pheatmap(hm,
         color = pal,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         border_color = NA,
         fontsize_row = 7,
         fontsize_col = 8,
         angle_col = 0,
         main = "NTD Burden Heatmap — Top 50 Countries",
         legend = TRUE,
         legend_labels = "NTD DALYs per 100,000",
         cellwidth = 10,
         cellheight = 10)
dev.off()
cat("Done\n")

# ---- 5. Figure 4: Segmented regression ----
cat("\n=== Figure 4 ===\n")
library(segmented, quietly=TRUE)

png(file.path(outdir, "fig4_segmented.png"), width=12, height=10, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,3,1))

for (g in grps) {
  sub <- gbd[gbd$urb_g == g, ]
  mns <- sapply(years, function(y) mean(sub$val[sub$yr == y], na.rm=TRUE))
  df0 <- data.frame(y=years, r=mns)
  lm0 <- lm(r ~ y, data=df0)
  s0 <- tryCatch(segmented(lm0, seg.Z=~y), error=function(e) NULL)
  plot(years, mns, type="o", pch=16, cex=0.5, col=cols[which(grps==g)],
       xlab="Year", ylab="NTD DALYs per 100,000", main=g)
  if (!is.null(s0)) {
    lines(years, predict(s0), col="red", lwd=2)
    abline(v=s0$psi[2], col="red", lty=2)
    text(s0$psi[2], max(mns), round(s0$psi[2],0), pos=4, cex=0.8, col="red")
  } else { abline(lm0, col="blue", lwd=2) }
}

# China with 2009 fix
plot(years, china_mn, type="o", pch=16, cex=0.7,
     xlab="Year", ylab="NTD DALYs per 100,000",
     main="China (2009 Folic Acid Policy Break)", col="black", lwd=2)
df_ch <- data.frame(y=years, r=china_mn)
lm_ch <- lm(r ~ y, data=df_ch)
s_ch <- tryCatch(segmented(lm_ch, seg.Z=~y, psi=2009), error=function(e) NULL)
if (!is.null(s_ch)) {
  lines(years, predict(s_ch), col="red", lwd=2)
  abline(v=2009, col="red", lty=2, lwd=2)
  text(2009, max(china_mn), "2009", pos=4, cex=1.2, col="red")
} else { abline(lm_ch, col="blue", lwd=2) }
dev.off()
cat("Done\n")

# ---- 6. Figure 5: EAPC ----
cat("\n=== Figure 5 ===\n")
calc_eapc <- function(r) {
  r <- r[!is.na(r) & r > 0]
  if (length(r) < 3) return(c(NA, NA, NA))
  fit <- lm(log(r) ~ seq_along(r))
  b <- coef(fit)[2]
  ci <- confint(fit, 2)
  eapc <- (exp(b) - 1) * 100
  eapc_lo <- (exp(ci[1]) - 1) * 100
  eapc_hi <- (exp(ci[2]) - 1) * 100
  c(eapc, eapc_lo, eapc_hi)
}
periods <- list(1990:1999, 2000:2009, 2010:2023)
pnames <- c("1990-1999","2000-2009","2010-2023")
eapc_mat <- matrix(NA, nrow=6, ncol=3)
eapc_lo_mat <- matrix(NA, nrow=6, ncol=3)
eapc_hi_mat <- matrix(NA, nrow=6, ncol=3)
rownames(eapc_mat) <- rownames(eapc_lo_mat) <- rownames(eapc_hi_mat) <- c(grps, "China")
colnames(eapc_mat) <- colnames(eapc_lo_mat) <- colnames(eapc_hi_mat) <- pnames

for (gi in seq_along(grps)) {
  g <- grps[gi]
  for (pi in seq_along(periods)) {
    sub <- gbd[gbd$urb_g == g & gbd$yr %in% periods[[pi]], ]
    if (nrow(sub) == 0) next
    mns <- tapply(sub$val, sub$yr, mean, na.rm=TRUE)
    res <- calc_eapc(mns)
    eapc_mat[gi, pi] <- res[1]; eapc_lo_mat[gi, pi] <- res[2]; eapc_hi_mat[gi, pi] <- res[3]
  }
}
for (pi in seq_along(periods)) {
  ch_sub <- china[china$yr %in% periods[[pi]], ]
  if (nrow(ch_sub) == 0) next
  ch_mns <- tapply(ch_sub$val, ch_sub$yr, mean, na.rm=TRUE)
  res <- calc_eapc(ch_mns)
  eapc_mat[6, pi] <- res[1]; eapc_lo_mat[6, pi] <- res[2]; eapc_hi_mat[6, pi] <- res[3]
}
cat("EAPC (95% CI):\n")
for (i in 1:nrow(eapc_mat)) {
  cat(rownames(eapc_mat)[i], ":\n")
  for (j in 1:3) {
    cat(sprintf("  %s: %.2f (%.2f to %.2f)\n", colnames(eapc_mat)[j],
        eapc_mat[i,j], eapc_lo_mat[i,j], eapc_hi_mat[i,j]))
  }
}

png(file.path(outdir, "fig5_eapc.png"), width=9, height=6, units="in", res=300)
par(mar=c(6,5,3,2))
bp <- barplot(eapc_mat, beside=TRUE, col=c(cols,"black"),
              ylab="EAPC (%)", main="EAPC of NTD Burden by Period",
              ylim=c(min(eapc_mat,na.rm=TRUE)-1, max(eapc_mat,na.rm=TRUE)+1))
abline(h=0, lty=2, col="gray")
legend("bottomleft", legend=rownames(eapc_mat), fill=c(cols,"black"), cex=0.7, bg="white")
dev.off()
cat("Done\n")

# ---- 7. Figure 6: World choropleth maps ----
cat("\n=== Figure 6 ===\n")
library(rnaturalearth, quietly=TRUE)
library(sf, quietly=TRUE)
world_all <- ne_countries(scale="medium", returnclass="sf")
world <- world_all[world_all$iso_a3 != "ATA", ]

# Load ISO3 mapping
iso3 <- read.csv(file.path(datadir, "iso3_mapping.csv"), stringsAsFactors=FALSE, fileEncoding="UTF-8")

# Prepare GBD data for mapping
d90 <- gbd[gbd$yr == 1990, c("loc","val")]
d23_map <- gbd[gbd$yr == 2023, c("loc","val")]
d90$iso3 <- iso3$iso3[match(d90$loc, iso3$gbd_name)]
d23_map$iso3 <- iso3$iso3[match(d23_map$loc, iso3$gbd_name)]

world$rate_1990 <- d90$val[match(world$iso_a3, d90$iso3)]
world$rate_2023 <- d23_map$val[match(world$iso_a3, d23_map$iso3)]



# Render each map separately to avoid mfrow issues
png(file.path(outdir, "fig6_world_1990.png"), width=16, height=7, units="in", res=300)
par(mar=c(1,1,2,1))
brks <- c(0,5,15,30,60,120,400)
plot(world["rate_1990"], main="NTD DALYs per 100,000 (1990)",
     breaks=brks, pal=rev(heat.colors(6)), key.pos=1)
dev.off()

png(file.path(outdir, "fig6_world_2023.png"), width=16, height=7, units="in", res=300)
par(mar=c(1,1,2,1))
plot(world["rate_2023"], main="NTD DALYs per 100,000 (2023)",
     breaks=brks, pal=rev(heat.colors(6)), key.pos=1)
dev.off()
cat("Done\n")

# ---- 8. Figure 7: Urbanization vs NTD rate ----
cat("\n=== Figure 7 ===\n")
d90s <- gbd[gbd$yr == 1990 & !is.na(gbd$urb_pct), ]
d23s <- gbd[gbd$yr == 2023 & !is.na(gbd$urb_pct), ]

png(file.path(outdir, "fig7_urb_scatter.png"), width=10, height=8, units="in", res=300)
par(mar=c(5,5,3,2))
plot(d90s$urb_pct, d90s$val, log="y", pch=16, cex=0.8,
     col=adjustcolor("steelblue",0.5),
     xlab="Urbanization Rate (%)", ylab="NTD DALYs per 100,000 (log)",
     main="Urbanization vs NTD Burden")
abline(lm(log(d90s$val) ~ d90s$urb_pct), col="steelblue", lwd=2)
points(d23s$urb_pct, d23s$val, pch=17, cex=0.8, col=adjustcolor("coral",0.5))
abline(lm(log(d23s$val) ~ d23s$urb_pct), col="coral", lwd=2)
r90 <- round(cor(d90s$val, d90s$urb_pct, method="spearman", use="complete"), 3)
r23 <- round(cor(d23s$val, d23s$urb_pct, method="spearman", use="complete"), 3)
text(10, 300, paste("1990 r =", r90), pos=4, cex=1, col="steelblue")
text(10, 150, paste("2023 r =", r23), pos=4, cex=1, col="coral")
legend("topright", legend=c("1990","2023"), pch=c(16,17), col=c("steelblue","coral"), cex=1)
dev.off()
cat("Done\n")

# ---- 9. Figure 8: ARIMA forecast ----
cat("\n=== Figure 8 ===\n")
library(forecast, quietly=TRUE)

ts_ch <- ts(log(china_mn), start=1990, frequency=1)
ts_gl <- ts(sapply(years, function(y) mean(gbd$val[gbd$yr == y], na.rm=TRUE)), start=1990, frequency=1)

fc_ch_raw <- forecast(auto.arima(ts_gl, max.p=2, max.q=2, max.d=1, seasonal=FALSE), h=17)
fc_gl <- fc_ch_raw
fc_ch_pred <- forecast(auto.arima(ts_ch, max.p=2, max.q=2, max.d=1, seasonal=FALSE), h=17)
# Back-transform China forecasts
fc_ch <- fc_ch_pred
fc_ch$mean <- exp(fc_ch_pred$mean)
fc_ch$lower <- exp(fc_ch_pred$lower)
fc_ch$upper <- exp(fc_ch_pred$upper)

png(file.path(outdir, "fig8_arima.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(1,2), mar=c(4,4,3,1))

gl_ylim <- range(c(ts_gl, fc_gl$lower[,2], fc_gl$upper[,2]), na.rm=TRUE)
plot(1990:2040, c(ts_gl, rep(NA,17)), type="n",
     xlab="Year", ylab="NTD DALYs per 100,000", main="Global ARIMA Forecast",
     ylim=gl_ylim)
grid()
polygon(c(2024:2040,rev(2024:2040)), c(fc_gl$upper[,2],rev(fc_gl$lower[,2])),
        col=adjustcolor("steelblue",0.2), border=NA)
polygon(c(2024:2040,rev(2024:2040)), c(fc_gl$upper[,1],rev(fc_gl$lower[,1])),
        col=adjustcolor("steelblue",0.35), border=NA)
lines(1990:2023, ts_gl, lwd=2.5, col="#1A5276")
lines(2024:2040, fc_gl$mean, lwd=3, col="#1A5276", lty=2)

ch_ylim <- range(c(china_mn, fc_ch$lower[,2], fc_ch$upper[,2]), na.rm=TRUE)
plot(1990:2040, c(china_mn, rep(NA,17)), type="n",
     xlab="Year", ylab="NTD DALYs per 100,000", main="China ARIMA Forecast",
     ylim=ch_ylim)
grid()
polygon(c(2024:2040,rev(2024:2040)), c(fc_ch$upper[,2],rev(fc_ch$lower[,2])),
        col=adjustcolor("darkred",0.2), border=NA)
polygon(c(2024:2040,rev(2024:2040)), c(fc_ch$upper[,1],rev(fc_ch$lower[,1])),
        col=adjustcolor("darkred",0.35), border=NA)
lines(1990:2023, china_mn, lwd=2.5, col="#7B241C")
lines(2024:2040, fc_ch$mean, lwd=2.5, col="#7B241C", lty=2)
dev.off()
cat("Done\n")

# ---- 10. Summary tables ----
cat("\n=== Tables ===\n")
sumt <- do.call(rbind, lapply(grps, function(g) {
  s <- gbd[gbd$urb_g == g, ]
  s90 <- s$val[s$yr == 1990]; s23 <- s$val[s$yr == 2023]
  data.frame(Group=g, N=length(unique(s$loc)),
             M1990=mean(s90,na.rm=TRUE), SD1990=sd(s90,na.rm=TRUE),
             M2023=mean(s23,na.rm=TRUE), SD2023=sd(s23,na.rm=TRUE),
             Decline=(1-mean(s23,na.rm=TRUE)/mean(s90,na.rm=TRUE))*100)
}))
write.csv(sumt, file.path(outdir, "table1_summary.csv"), row.names=FALSE)
# EAPC table with 95% CI
eapc_out <- data.frame(Group=rownames(eapc_mat),
  EAPC_1990_99=sprintf("%.2f (%.2f, %.2f)", eapc_mat[,1], eapc_lo_mat[,1], eapc_hi_mat[,1]),
  EAPC_2000_09=sprintf("%.2f (%.2f, %.2f)", eapc_mat[,2], eapc_lo_mat[,2], eapc_hi_mat[,2]),
  EAPC_2010_23=sprintf("%.2f (%.2f, %.2f)", eapc_mat[,3], eapc_lo_mat[,3], eapc_hi_mat[,3]),
  stringsAsFactors=FALSE)
write.csv(eapc_out, file.path(outdir, "table2_eapc.csv"), row.names=FALSE)

cntry <- data.frame(Country=d23$loc, Rate_2023=round(d23$val,2),
                    Urban_Pct=round(d23$urb_pct,1), Urban_Group=d23$urb_g)
write.csv(cntry, file.path(outdir, "table3_countries_2023.csv"), row.names=FALSE)

# China detailed
ch_table <- data.frame(Year=china$yr, Rate=round(china$val,2))
write.csv(ch_table, file.path(outdir, "table4_china_detail.csv"), row.names=FALSE)

cat("\n=== ALL COMPLETE ===\n")
cat("Output:", outdir, "\n")
cat("Files:\n")
for (f in list.files(outdir)) cat("  ", f, "\n")
