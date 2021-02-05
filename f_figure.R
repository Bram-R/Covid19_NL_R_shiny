f_figure <- function(df = df, date_start = date_start, figure = figure) {
  
  if(as.numeric(Sys.Date() - date_start) > 100){lbls_by <- "4 week"}
  if(as.numeric(Sys.Date() - date_start) < 100){lbls_by <- "week"}
  if(as.numeric(Sys.Date() - date_start) < 20){lbls_by <- "day"}
  lbls <- format(seq(date_start, Sys.Date() + 30, by = lbls_by), "%e %b")  
  
  
  ###### Infectie cijfers ###### 
  
  # Incidentie
  if(figure == 1){
    evalq({
      #png("Figures/1_Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV$I_3d / 3 ~ COV$date, ylab = "Incidentie / dag", xlab = "Datum", lwd = 2, xlim = c(date_start, Sys.Date() + 10), 
           main = "COVID-19 aantal nieuwe gemelde patienten  (RIVM-GGD)", type = "l", xaxt = "n", yaxt = "n", ylim = c(0, max(c(COV$I, pred$COV_I$up), na.rm = TRUE)))
      points(COV$I ~ COV$date, cex = 0.6, pch = 16)
      lines(COV_test$I_pos_7d / 7 ~ COV_test$date, type = "l", lty = 3, lwd = 2)
      factor <- 1 / 7 / (100000 / Population$NLD) 
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(35 * factor, 35 * factor, 100 * factor, 100 * factor), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(100 * factor, 100 * factor, 250 * factor, 250 * factor), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(250 * factor, 250 * factor, 100000 * factor, 100000 * factor), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      axis(side = 2, at = seq(0, ceiling(max(c(COV$I, pred$COV_I$up), na.rm = TRUE) / 2500) * 2500, 2500))
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(COV$I, pred$COV_I$up), na.rm = TRUE) / 2500) * 2500, 2500), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$COV_I$lo[7], pred$COV_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$COV_I$lo[7], pred$COV_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12),
           # labels = ceiling(((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2) / 50) * 50, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", "dotted"), cex = 0.6, pch = c(16, NA, NA), box.lty = 1, 
             legend = c("Gemeld aantal", 
                        "Gemeld aantal (3-dagen gemiddelde)", 
                        "Aantal positieve testen (7-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  # Incidentie per 100.000 inwoners 
  if(figure == 2){
    evalq({
      #png("Figures/2_Incidentie_NL_per100000.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV$I_3d_rel / 3 ~ COV$date, ylab = "Incidentie / dag per 100.000 inwoners", xlab = "Datum", 
           xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(COV$I_rel, pred$COV_I_rel$up), na.rm = TRUE)), 
           main = "COVID-19 aantal nieuwe gemelde patienten (RIVM-GGD)", type = "l", lty = 1, lwd = 2, xaxt = "n")
      lines(COV$I_3d_rel_limb / 3 ~ COV$date, type = "l", lty = "9414", lwd = 2)
      points(COV$I_rel ~ COV$date, cex = 0.6, pch = 16)
      points(COV$I_rel_limb ~ COV$date, cex = 0.6, pch = 1)
      lines(COV_test$I_pos_7d_rel / 7 ~ COV_test$date, type = "l", lty = 3, lwd = 2)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(35 / 7, 35 / 7, 100 / 7, 100 / 7), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(100 / 7, 100 / 7, 250 / 7, 250 / 7), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(250 / 7, 250 / 7, 100000 / 7, 100000 / 7), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(COV$I_rel, pred$COV_I_rel$up), na.rm = TRUE) / 5) * 5, 5), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$COV_I_rel$lo[7], pred$COV_I_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$COV_I_rel$lo[7], pred$COV_I_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2) ~ as.Date(max(COV$date) + 12),
      #      labels = ceiling(((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2) / 5) * 5, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "9414", "dotted"), cex = 0.6, pch = c(16, NA, 1, NA, NA), 
             box.lty = 1, legend = c("Gemeld aantal nationaal", 
                                     "Gemeld aantal nationaal (3-dagen gemiddelde)", 
                                     "Gemeld aantal Limburg", 
                                     "Gemeld aantal Limburg (3-dagen gemiddelde)", 
                                     "Aantal positieve testen (7-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  # Percentage positieve testen
  if(figure == 3){
    evalq({
      #png("Figures/3_Perc_test_pos_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10), 
           main = "COVID-19 percentage positieve testen (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(0.05 * 100, 0.05 * 100, 0.1 * 100, 0.1 * 100), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(0.1 * 100, 0.1 * 100, 1 * 100, 1 * 100), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(COV_test$prop_pos * 100, na.rm = TRUE) / 2) * 2, 2), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      
      #dev.off()
    }, envir = df)
  }
  
  # Reproductie index
  if(figure == 4){
    evalq({
      #png("Figures/4_Rt_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV_Rt$R ~ COV_Rt$date, ylab = "R", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10), 
           ylim = c(0, 2), main = "COVID-19 reproductie index (RIVM)", type = "l", lwd = 2, xaxt = "n")
      polygon(c(COV_Rt$date, rev(COV_Rt$date)), c(COV_Rt$R_lo, rev(COV_Rt$R_up)), 
              col = adjustcolor("black", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, # circa 1.0 aangenomen als 0.98 - 1.02
                date_start - 30), c(0.98, 0.98, 1.02, 1.02), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(1.02, 1.02, 100, 100), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(COV_Rt$R_up, na.rm = TRUE) / 0.5) * 0.5, 0.5), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      
      #dev.off()
    }, envir = df)
  }
  ###### Ziekenhuisopnames ###### 
  if(figure == 5){
    evalq({
      #png("Figures/5_Opnames_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(Hosp$I_3d / 3 ~ Hosp$date, ylab = "Incidentie / dag (met verdachte of bewezen COVID-19)", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
           ylim = c(0, max(c(Hosp$I, pred$Hosp_I$up), na.rm = TRUE)), main = "COVID-19 ziekenhuisopnames exclusief IC (NICE)", type = "l", lwd = 2, xaxt = "n")
      points(Hosp$I ~ Hosp$date, cex = 0.6, pch = 16)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(12, 12, 40, 40), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(40, 40, 80, 80), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(80, 80, 1000, 1000), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(Hosp$I, pred$Hosp_I$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$Hosp_I$lo[7], pred$Hosp_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$Hosp_I$lo[7], pred$Hosp_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
      #      labels = ceiling(((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2) / 10) * 10, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid"), cex = 0.6, pch = c(16, NA), box.lty = 1, 
             legend = c("Aantal", 
                        "Aantal (3-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  ###### IC opnames ###### 
  if(figure == 6){
    evalq({
      #png("Figures/6_ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(IC$I_3d / 3 ~ IC$date, ylab = "Incidentie / dag (met verdachte of bewezen COVID-19)", xlab = "Datum", 
           lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC$I, pred$IC_I$up), na.rm = TRUE)), 
           main = "COVID-19 IC opnames (NICE)", type = "l", lty = 1, xaxt = "n")
      points(IC$I ~ IC$date, cex = 0.6, pch = 16)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(3, 3, 10, 10), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(10, 10, 20, 20), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(20, 20, 10000, 10000), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(IC$I, pred$IC_I$up), na.rm = TRUE) / 10) * 10, 10), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$IC_I$lo[7], pred$IC_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$IC_I$lo[7], pred$IC_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
      #      labels = ceiling(((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2) / 5) * 5, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid"), cex = 0.6, pch = c(16, NA), box.lty = 1, 
             legend = c("Aantal", 
                        "Aantal (3-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  ###### Bezetting ziekenhuisbedden ###### 
  if(figure == 7){
    evalq({
      # IC bedden bezetting COVID-19
      #png("Figures/7_ICbezetting_cov_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(IC_LCPS$B_7d / 7 ~ IC_LCPS$date, ylab = "Bezetting (COVID-19)", xlab = "Datum", 
           lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B, IC$B, pred$IC_LCPS_B$up), na.rm = TRUE)), 
           main = "COVID-19 IC bedden bezetting (LCPS & NICE)", type = "l", lty = 1, xaxt = "n")
      points(IC_LCPS$B ~ IC_LCPS$date, cex = 0.6, pch = 16)
      lines(IC$B_7d / 7  ~ IC$date, type = "l", lty = 3, lwd = 2)
      points(IC$B ~ IC$date, cex = 0.6, pch = 1)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(IC_LCPS$B, IC$B, pred$IC_LCPS_B$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$IC_LCPS_B$lo[7], pred$IC_LCPS_B$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$IC_LCPS_B$lo[7], pred$IC_LCPS_B$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
      #      labels = ceiling(((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2) / 10) * 10, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
             legend = c("Aantal LCPS", 
                        "Aantal LCPS (7-dagen gemiddelde)", 
                        "Aantal NICE", 
                        "Aantal NICE (7-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  # IC bedden bezetting non-COVID-19 en totaal
  if(figure == 8){
    evalq({
      #png("Figures/8_ICbezetting_noncov_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(IC_LCPS$B_total_7d / 7 ~ IC_LCPS$date, ylab = "Bezetting", xlab = "Datum", 
           lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B_total, pred$IC_LCPS_B_total$up), na.rm = TRUE)), 
           main = "IC bedden bezetting (LCPS)", type = "l", lty = 1, xaxt = "n")
      points(IC_LCPS$B_total ~ IC_LCPS$date, cex = 0.6, pch = 16)
      lines(IC_LCPS$B_non_covid_7d / 7  ~ IC_LCPS$date, type = "l", lty = 3, lwd = 2)
      points(IC_LCPS$B_non_covid ~ IC_LCPS$date, cex = 0.6, pch = 1)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(IC_LCPS$B_total, pred$IC_LCPS_B_total$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$IC_LCPS_B_total$lo[7], pred$IC_LCPS_B_total$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$IC_LCPS_B_total$lo[7], pred$IC_LCPS_B_total$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
      #      labels = ceiling(((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2) / 25) * 25, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
             legend = c("Totaal aantal", 
                        "Totaal aantal (7-dagen gemiddelde)", 
                        "Aantal zonder COVID-19", 
                        "Aantal zonder COVID-19 (7-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  # Ziekenhuisbedden bezetting COVID-19
  if(figure == 9){
    evalq({
      #png("Figures/9_Ziekenhuisbezetting_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot((IC_LCPS$B_7d + Hosp_LCPS$B_7d) / 7 ~ IC_LCPS$date, ylab = "Bezetting (COVID-19)", xlab = "Datum", 
           lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B + Hosp_LCPS$B, pred$Hosp_IC_LCPS_B_total_cov$up), na.rm = TRUE)), 
           main = "COVID-19 ziekenhuisbedden bezetting (LCPS)", type = "l", lty = 1, xaxt = "n")
      points((IC_LCPS$B + Hosp_LCPS$B)  ~ IC_LCPS$date, cex = 0.6, pch = 16)
      lines(Hosp_LCPS$B_7d / 7  ~ Hosp_LCPS$date, type = "l", lty = 3, lwd = 2)
      points(Hosp_LCPS$B ~ Hosp_LCPS$date, cex = 0.6, pch = 1)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(IC_LCPS$B + Hosp_LCPS$B, pred$Hosp_IC_LCPS_B_total_cov$up), na.rm = TRUE) / 100) * 100, 100), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      # points((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
      # points(c(pred$Hosp_IC_LCPS_B_total_cov$lo[7], pred$Hosp_IC_LCPS_B_total_cov$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
      # lines(c(pred$Hosp_IC_LCPS_B_total_cov$lo[7], pred$Hosp_IC_LCPS_B_total_cov$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
      # text(((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
      #      labels = ceiling(((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2) / 25) * 25, col = "black", font = 1, cex = 0.6)
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
             legend = c("Aantal inclusief IC", 
                        "Aantal inclusief IC (7-dagen gemiddelde)", 
                        "Aantal exclusief IC", 
                        "Aantal exclusief IC (7-dagen gemiddelde)"))
      
      #dev.off()
    }, envir = df)
  }
  
  ###### Verpleeghuislocaties ######
  if(figure == 10){
    evalq({
      #png("Figures/10_Verpleeghuislocaties_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(Nurs$I_7d / 7 ~ Nurs$date, ylab = "Verpleeghuislocaties", xlab = "Datum", 
           xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE)), 
           main = "COVID-19 verpleeghuislocaties (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
      points(Nurs$I ~ Nurs$date, cex = 0.6, pch = 16)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE) / 5) * 5, 5), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      par(new = TRUE)
      plot(Nurs$A_prop * 100 ~ Nurs$date, type = "l", lty = 3, lwd = 2, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
           xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE)))
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", "dashed"), cex = 0.6, pch = c(16, NA, NA), box.lty = 1, 
             legend = c("Aantal nieuwe locaties met minimaal 1 besmette bewoner", 
                        "Aantal nieuwe locaties met minimaal 1 besmette bewoner (7-dagen gemiddelde)",
                        "Totaal aantal besmette locaties (%)"))
      
      #dev.off()
    }, envir = df)
  }
  
  ###### Sterfte ######
  if(figure == 11){
    evalq({
      #png("Figures/11_Sterfte_NL.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(Death$I_7d_rel / 7 ~ Death$date, ylab = "Incidentie / dag per 100.000 inwoners", xlab = "Datum", 
           xlim = c(date_start, Sys.Date() + 10), ylim = c(0, 1.5), 
           main = "COVID-19 sterfte (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
      lines(Death$I_7d_rel_limb / 7 ~ Death$date, type = "l", lty = "9414")
      #points(Death$I_rel ~ Death$date, cex = 0.6, pch = 16)
      #points(Death$I_rel_limb ~ Death$date, cex = 0.6, pch = 1)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(1.5) / 0.1) * 0.1, 0.1), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "9414"), cex = 0.6, pch = c(16, NA, 1, NA), 
             box.lty = 1, legend = c("Gemeld aantal nationaal", 
                                     "Gemeld aantal nationaal (7-dagen gemiddelde)", 
                                     "Gemeld aantal Limburg", 
                                     "Gemeld aantal Limburg (7-dagen gemiddelde)"))
      #dev.off()
    }, envir = df)
  }
  
  ###### Internationaal ######
  if(figure == 12){
    evalq({
      # Incidentie
      #png("Figures/12_Incidentie_INT_per100000.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV$I_7d_rel ~ COV$date, ylab = "Incidentie / week per 100.000 inwoners", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
           ylim = c(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE) / 100) * 100), main = "COVID-19 aantal nieuwe gemelde patienten", type = "l", col = "black", lwd = 4, xaxt = "n")
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(35, 35, 100, 100), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(100, 100, 250, 250), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(250, 250, 100000, 100000), 
              col = adjustcolor("red", alpha.f = 0.3), border = NA)
      lines(Int[Int$iso == levels(Int$iso)[1], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[2], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[3], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[4], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[5], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[6], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[7], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[8], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[9], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[10], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[11], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[12], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[14], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[15], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[16], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[17], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE) / 100) * 100, 100), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty = c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex = 0.6, box.lty = 1, 
             legend = levels(Int$iso)[c(13, 1:12, 14:17)])
      
      #dev.off()
    }, envir = df)
  }
  
  # Percentage positieve testen per 100.000 inwoners per week
  if(figure == 13){
    evalq({
      #png("Figures/13_Perc_test_pos_INT.png", width = 1000, height = 600, pointsize = 18)
      par(mar = c(5.1, 4.1, 4.1, 1.1))
      
      plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
           ylim = c(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE) / 5) * 5), main = "COVID-19 percentage positieve testen", type = "l", col = "black", lwd = 4, xaxt = "n")
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(5, 5, 10, 10), 
              col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
      polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
                date_start - 30), c(10, 10, 100, 100), 
              col = adjustcolor("orange", alpha.f = 0.3), border = NA)
      lines(Int[Int$iso == levels(Int$iso)[1], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[2], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[3], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[4], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[5], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[6], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[7], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[8], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
      lines(Int[Int$iso == levels(Int$iso)[9], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[10], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[11], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[12], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[14], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[15], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[16], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
      lines(Int[Int$iso == levels(Int$iso)[17], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
      axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = lbls_by)), labels = lbls)
      abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      abline(h = seq(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE) / 5) * 5, 5), lty = 3, 
             col = adjustcolor("grey", alpha.f = 0.7))
      legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty = c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex = 0.6, box.lty = 1, 
             legend = levels(Int$iso)[c(13, 1:12, 14:17)])
      
      #dev.off()
    }, envir = df)
  }
}

