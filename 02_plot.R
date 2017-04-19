ylim_bdi = c(1/(5*0.01^2 + 0.99^2), 1/(5*0.2^2))
ylim_ifi <- c(0.4, 1.8) ## range(c(dat1$ifi, dat2$ifi, dat3$ifi, dat4$ifi))
ylim_dcsi <- c(0, 3) ## range(c(dat1$dcsi, dat2$dcsi, dat3$dcsi, dat4$dcsi))
ylim_tenor <- c(0, 1.2) ## range(c(dat1$tenor, dat2$tenor, dat3$tenor, dat4$tenor))

pdf("Fig4.pdf", height = 10, width = 14)
par(mar = c(4, 2.5, 2, 0.5))
layout(matrix(nrow = 5, ncol = 5, 
              c(1, 2, 17, 5, 6, 
                3, 4, 18, 7, 8,
                19, 20, 21, 22, 23,
                9, 10, 24, 13, 14, 
                11, 12, 25, 15, 16), 
              byrow = T), 
       heights = c(0.2, 0.2, 0.05, 0.2, 0.2), 
       widths = c(0.2, 0.2, 0.05, 0.2, 0.2))

dat <- dat1
levels(dat$role)
dat$role <- factor(as.character(dat$role), levels = c("neg", "amb", "pos", "fri"))
boxplot(dat$bdi ~ dat$role, ylim = ylim_bdi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
title("(a)", adj = 0, family = "serif", cex.main = 2.2, line = 0.7)
mtext(side = 3, family = "serif", "BDI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$ifi ~ dat$role, ylim = ylim_ifi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "IFI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$dcsi ~ dat$role, ylim = ylim_dcsi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "DCSI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$tenor ~ dat$role, ylim = ylim_tenor, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "Tenor")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)

dat <- dat2
levels(dat$role)
dat$role <- factor(as.character(dat$role), levels = c("neg", "amb", "pos", "fri"))
boxplot(dat$bdi ~ dat$role, ylim = ylim_bdi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
title("(b)", adj = 0, family = "serif", cex.main = 2.2, line = 0.7)
mtext(side = 3, family = "serif", "BDI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$ifi ~ dat$role, ylim = ylim_ifi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "IFI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$dcsi ~ dat$role, ylim = ylim_dcsi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "DCSI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$tenor ~ dat$role, ylim = ylim_tenor, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "Tenor")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)

dat <- dat3
levels(dat$role)
dat$role <- as.character(dat$role)
dat$role <- factor(gsub(dat$role, pattern = "_", replacement = " "))
levels(dat$role)
dat$role <- factor(as.character(dat$role), levels = c("neg", "low pos", "mid pos", "high pos"))
boxplot(dat$bdi ~ dat$role, ylim = ylim_bdi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
title("(c)", adj = 0, family = "serif", cex.main = 2.2, line = 0.7)
mtext(side = 3, family = "serif", "BDI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$ifi ~ dat$role, ylim = ylim_ifi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "IFI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$dcsi ~ dat$role, ylim = ylim_dcsi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "DCSI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$tenor ~ dat$role, ylim = ylim_tenor, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "Tenor")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:4, levels(dat$role), family = "serif", cex.axis = 0.95)

dat <- dat4
levels(dat$role)
dat$role <- as.character(dat$role)
dat$role <- factor(gsub(dat$role, pattern = "_", replacement = " "))
levels(dat$role)
dat$role <- factor(as.character(dat$role), levels = c("neg", "amb", "low pos", "mid pos", "high pos"))
boxplot(dat$bdi ~ dat$role, ylim = ylim_bdi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
title("(d)", adj = 0, family = "serif", cex.main = 2.2, line = 0.7)
mtext(side = 3, family = "serif", "BDI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:5, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$ifi ~ dat$role, ylim = ylim_ifi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "IFI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:5, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$dcsi ~ dat$role, ylim = ylim_dcsi, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "DCSI")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:5, levels(dat$role), family = "serif", cex.axis = 0.95)
boxplot(dat$tenor ~ dat$role, ylim = ylim_tenor, main = "", xaxt = "n", yaxt = "n", frame = F); box(bty="l")
mtext(side = 3, family = "serif", "Tenor")
axis(2, las = 2, family = "serif", cex.axis = 0.95); axis(1, at = 1:5, levels(dat$role), family = "serif", cex.axis = 0.95)

dev.off()
shell.exec("Fig4.pdf")