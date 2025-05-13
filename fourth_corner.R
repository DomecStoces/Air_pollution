library(readxl)
library(ade4)

# Load sheets into a named list
fourth_corner <- lapply(c("sp", "env", "traits"), function(x) read_excel("fourth_corner.xlsx", sheet = x))
names(fourth_corner) <- c("sp", "env", "traits")

# Ensure no NA values
fourth_corner$sp[is.na(fourth_corner$sp)] <- 0
fourth_corner$traits[is.na(fourth_corner$traits)] <- 0
fourth_corner$env[is.na(fourth_corner$env)] <- 0

# Convert character variables in 'env' to factors
fourth_corner$env <- as.data.frame(lapply(fourth_corner$env, function(x) {
  if (is.character(x)) as.factor(x) else x
}))

#Convert variables in traits to factors instead of continuous Body.size
fourth_corner$traits <- as.data.frame(lapply(fourth_corner$traits, function(x) {
  if (is.character(x)) as.numeric(as.character(x)) else as.numeric(x)
}))

# RLQ Analysis
afcL <- dudi.coa(fourth_corner$sp, scannf = FALSE)
acpR <- dudi.hillsmith(fourth_corner$env, row.w = afcL$lw, scannf = FALSE)
acpQ <- dudi.mix(fourth_corner$traits, scannf = FALSE)
acpQ$lw <- afcL$cw
rlq_result <- rlq(acpR, afcL, acpQ, scannf = FALSE)


# Fourth-corner analysis
nrepet <- 100
four.comb <- fourthcorner(fourth_corner$env, fourth_corner$sp, fourth_corner$traits,
                          modeltype = 6, p.adjust.method.G = "none", 
                          p.adjust.method.D = "none", nrepet = nrepet)

# Plot fourth-corner results
plot(four.comb, alpha = 0.05, stat = "D2")
plot(four.comb, x.rlq = rlq_result, alpha = 0.05, stat = "D2", type = "biplot")

# RLQ-based fourth-corner significance test
Srlq <- fourthcorner2(fourth_corner$env, fourth_corner$sp, fourth_corner$traits,
                      modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)
Srlq$trRLQ

tiff('table.tiff', units="in", width=8, height=10, res=600)
plot(four.comb, alpha = 0.05, stat = "D2")
dev.off()

pdf('table.pdf', width=8, height=10)
plot(four.comb.aravo, alpha = 0.05, stat = "D2")
dev.off()
