# =============================================================================
# test-correlate.R
# =============================================================================
#
# Uitgebreide tests voor de correlate() functie en alle interne helpers.
# Georganiseerd per sectie:
#   A. pbivnorm (bivariate normale CDF)
#   B. Standaard correlaties (pearson, kendall, spearman)
#   C. Biseriale correlatie
#   D. Polyseriale correlatie
#   E. Polychorische correlatie
#   F. Tetrachorische correlatie
#   G. Input-validatie en foutmeldingen
#   H. Return-structuur en print-methode
#   I. NA-afhandeling
# =============================================================================


# =============================================================================
# A. BIVARIATE NORMALE CDF — pbivnorm()
# =============================================================================
# De bivariate normale CDF is de bouwsteen voor polychoric en tetrachoric.
# We testen tegen bekende exacte waarden en wiskundige identiteiten.

pbivnorm <- getFromNamespace("pbivnorm", "bootcamp")

# A1. Bij rho=0 zijn X en Y onafhankelijk: P(X<=0, Y<=0) = 0.5 * 0.5 = 0.25
expect_equal(pbivnorm(0, 0, 0), 0.25,
             info = "pbivnorm(0,0,0) moet 0.25 zijn (onafhankelijk)")

# A2. Bij rho=0 en andere waarden: P(X<=1, Y<=1) = pnorm(1)^2
expect_equal(pbivnorm(1, 1, 0), stats::pnorm(1)^2, tolerance = 1e-10,
             info = "pbivnorm(1,1,0) moet pnorm(1)^2 zijn")

# A3. Exacte formule voor x=y=0: P(X<=0, Y<=0, rho) = 0.25 + asin(rho)/(2*pi)
# Dit is een bekende identiteit voor de bivariate normaal.
for (rho in c(-0.9, -0.5, 0, 0.3, 0.5, 0.8, 0.95)) {
  expected <- 0.25 + asin(rho) / (2 * pi)
  actual <- pbivnorm(0, 0, rho)
  expect_equal(actual, expected, tolerance = 1e-4,
               info = paste0("pbivnorm(0,0,", rho, ") moet 0.25 + asin(rho)/(2pi) zijn"))
}

# A4. Grensgeval rho=1: P(X<=x, Y<=y, 1) = P(min(X,Y) <= min(x,y)) = pnorm(min(x,y))
expect_equal(pbivnorm(1, 2, 1), stats::pnorm(1),
             info = "pbivnorm met rho=1 moet pnorm(min(x,y)) zijn")
expect_equal(pbivnorm(-1, 0.5, 1), stats::pnorm(-1),
             info = "pbivnorm met rho=1 en negatieve x")

# A5. Grensgeval rho=-1: P(X<=x, Y<=y, -1) = max(0, pnorm(x) + pnorm(y) - 1)
expect_equal(pbivnorm(0, 0, -1), 0,
             info = "pbivnorm(0,0,-1) moet 0 zijn")
expect_equal(pbivnorm(5, 5, -1), max(0, stats::pnorm(5) + stats::pnorm(5) - 1),
             info = "pbivnorm met rho=-1 en grote waarden")

# A6. P(X <= Inf, Y <= Inf, rho) = 1 voor elke rho
expect_equal(pbivnorm(Inf, Inf, 0.5), 1, tolerance = 1e-10,
             info = "pbivnorm(Inf, Inf, rho) moet 1 zijn")

# A7. P(X <= -Inf, Y <= y, rho) = 0 voor elke y en rho
expect_equal(pbivnorm(-Inf, 0, 0.5), 0,
             info = "pbivnorm(-Inf, y, rho) moet 0 zijn")

# A8. Symmetrie: P(X<=x, Y<=y, rho) = P(Y<=y, X<=x, rho)
expect_equal(pbivnorm(1, 0.5, 0.3), pbivnorm(0.5, 1, 0.3), tolerance = 1e-8,
             info = "pbivnorm moet symmetrisch zijn in x en y")

# A9. Hoge correlatie regime (|rho| >= 0.925): testen dat de complementaire
#     methode werkt
expect_equal(pbivnorm(0, 0, 0.95), 0.25 + asin(0.95) / (2 * pi),
             tolerance = 1e-4,
             info = "pbivnorm werkt bij hoge correlatie (0.95)")
expect_equal(pbivnorm(0, 0, -0.95), 0.25 + asin(-0.95) / (2 * pi),
             tolerance = 1e-4,
             info = "pbivnorm werkt bij hoge negatieve correlatie (-0.95)")

# A10. Resultaat moet altijd in [0, 1] liggen
for (rho in c(-0.99, -0.5, 0, 0.5, 0.99)) {
  for (xy in list(c(-3, -3), c(-3, 3), c(3, -3), c(3, 3), c(0, 0))) {
    val <- pbivnorm(xy[1], xy[2], rho)
    expect_true(val >= 0 && val <= 1,
                info = paste0("pbivnorm(", xy[1], ",", xy[2], ",", rho,
                              ") = ", val, " moet in [0,1] liggen"))
  }
}


# =============================================================================
# B. STANDAARD CORRELATIES (pearson, kendall, spearman)
# =============================================================================

# B1. Pearson: vergelijk estimate, p-waarde en statistiek met cor.test()
res_pearson <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "pearson")
ref_pearson <- stats::cor.test(mtcars$mpg, mtcars$hp, method = "pearson")
expect_equal(res_pearson$estimate, unname(ref_pearson$estimate), tolerance = 1e-10,
             info = "Pearson estimate moet overeenkomen met cor.test()")
expect_equal(res_pearson$p.value, ref_pearson$p.value, tolerance = 1e-10,
             info = "Pearson p-waarde moet overeenkomen met cor.test()")
expect_equal(res_pearson$statistic, unname(ref_pearson$statistic), tolerance = 1e-10,
             info = "Pearson statistiek moet overeenkomen met cor.test()")
expect_equal(res_pearson$n, nrow(mtcars),
             info = "Pearson n moet gelijk zijn aan nrow(mtcars)")

# B2. Spearman: vergelijk estimate, p-waarde en statistiek met cor.test()
res_spearman <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "spearman")
ref_spearman <- stats::cor.test(mtcars$mpg, mtcars$hp, method = "spearman")
expect_equal(res_spearman$estimate, unname(ref_spearman$estimate), tolerance = 1e-10,
             info = "Spearman estimate moet overeenkomen met cor.test()")
expect_equal(res_spearman$p.value, ref_spearman$p.value, tolerance = 1e-10,
             info = "Spearman p-waarde moet overeenkomen met cor.test()")
expect_equal(res_spearman$statistic, unname(ref_spearman$statistic), tolerance = 1e-10,
             info = "Spearman statistiek moet overeenkomen met cor.test()")

# B3. Kendall: vergelijk estimate, p-waarde en statistiek met cor.test()
res_kendall <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "kendall")
ref_kendall <- stats::cor.test(mtcars$mpg, mtcars$hp, method = "kendall")
expect_equal(res_kendall$estimate, unname(ref_kendall$estimate), tolerance = 1e-10,
             info = "Kendall estimate moet overeenkomen met cor.test()")
expect_equal(res_kendall$p.value, ref_kendall$p.value, tolerance = 1e-10,
             info = "Kendall p-waarde moet overeenkomen met cor.test()")
expect_equal(res_kendall$statistic, unname(ref_kendall$statistic), tolerance = 1e-10,
             info = "Kendall statistiek moet overeenkomen met cor.test()")

# B4. Standaard: type "pearson" is default
res_default <- bootcamp::correlate(mtcars$mpg, mtcars$hp)
expect_equal(res_default$type, "pearson",
             info = "Default type moet 'pearson' zijn")

# B5. Perfecte positieve correlatie
expect_equal(bootcamp::correlate(1:10, 1:10)$estimate, 1,
             info = "Perfecte positieve correlatie moet 1 zijn")

# B6. Perfecte negatieve correlatie
expect_equal(bootcamp::correlate(1:10, 10:1)$estimate, -1,
             info = "Perfecte negatieve correlatie moet -1 zijn")

# B7. Tweede dataset: iris, om te bevestigen dat het niet alleen met mtcars werkt
res_iris <- bootcamp::correlate(iris$Sepal.Length, iris$Petal.Length)
ref_iris <- stats::cor.test(iris$Sepal.Length, iris$Petal.Length)
expect_equal(res_iris$estimate, unname(ref_iris$estimate), tolerance = 1e-10,
             info = "Pearson op iris moet overeenkomen met cor.test()")
expect_equal(res_iris$p.value, ref_iris$p.value, tolerance = 1e-10,
             info = "Pearson p-waarde op iris moet overeenkomen met cor.test()")
expect_equal(res_iris$n, nrow(iris),
             info = "Pearson n op iris moet gelijk zijn aan nrow(iris)")


# =============================================================================
# C. BISERIALE CORRELATIE
# =============================================================================

# C1. Biserial met mtcars: mpg (continu) vs am (binair)
res_biserial <- bootcamp::correlate(mtcars$mpg, mtcars$am, type = "biserial")
expect_true(abs(res_biserial$estimate) <= 1,
            info = "Biseriale correlatie moet in [-1, 1] liggen")
expect_equal(res_biserial$type, "biserial")
expect_equal(res_biserial$n, nrow(mtcars))

# C2. Referentiewaarde: handmatige berekening van de biseriale correlatie
# r_bis = r_pb * sqrt(p*q) / phi(qnorm(p))
r_pb <- stats::cor(mtcars$mpg, mtcars$am)
p_am <- mean(mtcars$am)
q_am <- 1 - p_am
z_am <- stats::qnorm(p_am)
phi_am <- stats::dnorm(z_am)
expected_biserial <- r_pb * sqrt(p_am * q_am) / phi_am
expect_equal(res_biserial$estimate, expected_biserial, tolerance = 1e-10,
             info = "Biserial moet exact overeenkomen met handberekende formule")

# C3. |r_biserial| >= |r_point_biserial|
expect_true(abs(res_biserial$estimate) >= abs(r_pb) - 1e-10,
            info = "|biserial| moet >= |point-biserial| zijn")

# C4. Auto-swap: als x binair is, wordt er geswapped
res_swap <- suppressMessages(
  bootcamp::correlate(mtcars$am, mtcars$mpg, type = "biserial")
)
expect_equal(abs(res_swap$estimate), abs(res_biserial$estimate), tolerance = 1e-10,
             info = "Biserial met geswapte x/y moet dezelfde absolute waarde geven")

# C5. Biserial met niet-0/1 binaire variabele (bijv. 1/2)
x_cont <- rnorm(100)
y_binary_12 <- sample(c(1, 2), 100, replace = TRUE)
res_12 <- bootcamp::correlate(x_cont, y_binary_12, type = "biserial")
expect_true(!is.na(res_12$estimate),
            info = "Biserial moet werken met niet-0/1 binaire variabelen")

# C6. Tweede dataset: mpg vs vs (ook binair)
res_bis_vs <- bootcamp::correlate(mtcars$mpg, mtcars$vs, type = "biserial")
r_pb_vs <- stats::cor(mtcars$mpg, mtcars$vs)
p_vs <- mean(mtcars$vs)
expected_bis_vs <- r_pb_vs * sqrt(p_vs * (1 - p_vs)) / stats::dnorm(stats::qnorm(p_vs))
expect_equal(res_bis_vs$estimate, expected_bis_vs, tolerance = 1e-10,
             info = "Biserial op mpg/vs moet overeenkomen met handberekende formule")

# C7. Bekende richting: mpg is hoger bij am=1 (handgeschakeld), dus positief
expect_true(res_biserial$estimate > 0,
            info = "Biserial mpg/am moet positief zijn (handgeschakeld = hoger mpg)")


# =============================================================================
# D. POLYSERIALE CORRELATIE
# =============================================================================
# We genereren data waarbij x en een latente y* gecorreleerd zijn,
# en y is de gediscretiseerde versie van y*.

set.seed(42)
n_poly <- 500
# Genereer gecorreleerde normalen met bekende correlatie ~0.7
z1 <- rnorm(n_poly)
z2 <- 0.7 * z1 + sqrt(1 - 0.7^2) * rnorm(n_poly)
# x is continu, y is gediscretiseerd naar 5 categorieen
x_poly <- z1
y_poly <- as.integer(cut(z2, breaks = c(-Inf, -1, 0, 1, Inf)))

# D1. De polyseriale correlatie moet positief zijn (gegeven positieve relatie)
res_polyserial <- bootcamp::correlate(x_poly, y_poly, type = "polyserial")
expect_true(res_polyserial$estimate > 0,
            info = "Polyseriale correlatie moet positief zijn bij positieve relatie")

# D2. De schatting moet in de buurt van 0.7 zijn (de ware latente correlatie)
# Met n=500 en 4 categorieen verwachten we redelijke nauwkeurigheid
expect_true(abs(res_polyserial$estimate - 0.7) < 0.15,
            info = "Polyseriale schatting moet in de buurt van de ware correlatie (0.7) zijn")

# D3. Resultaat in [-1, 1]
expect_true(abs(res_polyserial$estimate) <= 1,
            info = "Polyseriale correlatie moet in [-1, 1] liggen")

# D4. Auto-swap: als x de ordinale variabele is
res_poly_swap <- suppressMessages(
  bootcamp::correlate(y_poly, x_poly, type = "polyserial")
)
expect_true(abs(res_poly_swap$estimate - res_polyserial$estimate) < 0.01,
            info = "Polyserial met geswapte x/y moet vergelijkbaar resultaat geven")

# D5. Negatieve correlatie
set.seed(43)
z1_neg <- rnorm(500)
z2_neg <- -0.7 * z1_neg + sqrt(1 - 0.7^2) * rnorm(500)
y_neg_ord <- as.integer(cut(z2_neg, breaks = c(-Inf, -1, 0, 1, Inf)))
res_poly_neg <- bootcamp::correlate(z1_neg, y_neg_ord, type = "polyserial")
expect_true(res_poly_neg$estimate < 0,
            info = "Polyserial moet negatief zijn bij negatieve relatie")
expect_true(abs(res_poly_neg$estimate - (-0.7)) < 0.15,
            info = "Polyserial negatief moet in de buurt van -0.7 zijn")

# D6. Grotere steekproef: betere recovery
set.seed(44)
n_ps_big <- 2000
z1_ps_big <- rnorm(n_ps_big)
z2_ps_big <- 0.7 * z1_ps_big + sqrt(1 - 0.7^2) * rnorm(n_ps_big)
y_ps_big <- as.integer(cut(z2_ps_big, breaks = c(-Inf, -1, 0, 1, Inf)))
res_ps_big <- bootcamp::correlate(z1_ps_big, y_ps_big, type = "polyserial")
expect_true(abs(res_ps_big$estimate - 0.7) < 0.08,
            info = "Polyserial met n=2000 moet nauwkeurig 0.7 benaderen")


# =============================================================================
# E. POLYCHORISCHE CORRELATIE
# =============================================================================
# Genereer twee gecorreleerde normalen, discretiseer beide.

set.seed(123)
n_polych <- 500
z1_pc <- rnorm(n_polych)
z2_pc <- 0.6 * z1_pc + sqrt(1 - 0.6^2) * rnorm(n_polych)
x_ord <- as.integer(cut(z1_pc, breaks = c(-Inf, -0.5, 0.5, Inf)))
y_ord <- as.integer(cut(z2_pc, breaks = c(-Inf, -0.5, 0.5, Inf)))

# E1. De polychorische correlatie moet positief zijn
res_polychoric <- bootcamp::correlate(x_ord, y_ord, type = "polychoric")
expect_true(res_polychoric$estimate > 0,
            info = "Polychorische correlatie moet positief zijn bij positieve relatie")

# E2. De schatting moet in de buurt van 0.6 zijn
expect_true(abs(res_polychoric$estimate - 0.6) < 0.15,
            info = "Polychorische schatting moet in de buurt van 0.6 zijn")

# E3. Resultaat in [-1, 1]
expect_true(abs(res_polychoric$estimate) <= 1,
            info = "Polychorische correlatie moet in [-1, 1] liggen")

# E4. Symmetrie: correlate(x, y) moet gelijk zijn aan correlate(y, x)
res_polych_yx <- bootcamp::correlate(y_ord, x_ord, type = "polychoric")
expect_equal(res_polychoric$estimate, res_polych_yx$estimate, tolerance = 1e-6,
             info = "Polychorische correlatie moet symmetrisch zijn")

# E5. Negatieve correlatie: als we y inverteren
y_ord_neg <- max(y_ord) + 1L - y_ord
res_polych_neg <- bootcamp::correlate(x_ord, y_ord_neg, type = "polychoric")
expect_true(res_polych_neg$estimate < 0,
            info = "Polychorische correlatie moet negatief zijn bij geinverteerde y")

# E6. Kruisvalidatie: polychoric op 2x2 data moet tetrachoric benaderen
# Gebruik dezelfde gebalanceerde 2x2 tabel als in de tetrachoric tests
x_pc_2x2 <- c(rep(1L, 50), rep(2L, 50))
y_pc_2x2 <- c(rep(1L, 40), rep(2L, 10), rep(1L, 10), rep(2L, 40))
res_pc_2x2 <- bootcamp::correlate(x_pc_2x2, y_pc_2x2, type = "polychoric")
expected_pc_2x2 <- sin(0.3 * pi)
expect_equal(res_pc_2x2$estimate, expected_pc_2x2, tolerance = 1e-3,
             info = "Polychoric op 2x2 tabel moet tetrachoric benaderen")

# E7. Grotere steekproef: betere recovery van de ware correlatie
set.seed(789)
n_polych_big <- 2000
z1_big <- rnorm(n_polych_big)
z2_big <- 0.6 * z1_big + sqrt(1 - 0.6^2) * rnorm(n_polych_big)
x_ord_big <- as.integer(cut(z1_big, breaks = c(-Inf, -0.5, 0.5, Inf)))
y_ord_big <- as.integer(cut(z2_big, breaks = c(-Inf, -0.5, 0.5, Inf)))
res_pc_big <- bootcamp::correlate(x_ord_big, y_ord_big, type = "polychoric")
expect_true(abs(res_pc_big$estimate - 0.6) < 0.08,
            info = "Polychoric met n=2000 moet nauwkeurig 0.6 benaderen")


# =============================================================================
# F. TETRACHORISCHE CORRELATIE
# =============================================================================
# Genereer gecorreleerde binaire data via een bivariate normaal.

set.seed(456)
n_tet <- 1000
z1_tet <- rnorm(n_tet)
z2_tet <- 0.5 * z1_tet + sqrt(1 - 0.5^2) * rnorm(n_tet)
x_bin <- as.integer(z1_tet > 0)
y_bin <- as.integer(z2_tet > 0)

# F1. De tetrachorische correlatie moet positief zijn
res_tetrachoric <- bootcamp::correlate(x_bin, y_bin, type = "tetrachoric")
expect_true(res_tetrachoric$estimate > 0,
            info = "Tetrachorische correlatie moet positief zijn")

# F2. De schatting moet in de buurt van 0.5 zijn
expect_true(abs(res_tetrachoric$estimate - 0.5) < 0.15,
            info = "Tetrachorische schatting moet in de buurt van 0.5 zijn")

# F3. Resultaat in [-1, 1]
expect_true(abs(res_tetrachoric$estimate) <= 1,
            info = "Tetrachorische correlatie moet in [-1, 1] liggen")

# F4. Symmetrie
res_tet_yx <- bootcamp::correlate(y_bin, x_bin, type = "tetrachoric")
expect_equal(res_tetrachoric$estimate, res_tet_yx$estimate, tolerance = 1e-6,
             info = "Tetrachorische correlatie moet symmetrisch zijn")

# F5. mtcars: am vs vs (twee binaire variabelen)
res_tet_mtcars <- bootcamp::correlate(mtcars$am, mtcars$vs, type = "tetrachoric")
expect_true(abs(res_tet_mtcars$estimate) <= 1,
            info = "Tetrachorische correlatie van am/vs moet in [-1, 1] liggen")

# F6. Negatieve correlatie
y_bin_neg <- 1L - y_bin
res_tet_neg <- bootcamp::correlate(x_bin, y_bin_neg, type = "tetrachoric")
expect_true(res_tet_neg$estimate < 0,
            info = "Tetrachorische correlatie moet negatief zijn bij geinverteerde y")

# F7. Referentiewaarde: gebalanceerde 2x2 tabel met analytische oplossing
# Tabel: a=40, b=10, c=10, d=40 (n=100, 50/50 marginalen)
# Drempels: tau_x = tau_y = qnorm(0.5) = 0
# Celproportie (0,0) = 40/100 = 0.4
# Uit pbivnorm(0, 0, rho) = 0.25 + asin(rho)/(2*pi) = 0.4
#   => asin(rho) = 0.15 * 2 * pi = 0.3*pi
#   => rho = sin(0.3*pi) ≈ 0.8090
x_tet_ref <- c(rep(0L, 50), rep(1L, 50))
y_tet_ref <- c(rep(0L, 40), rep(1L, 10), rep(0L, 10), rep(1L, 40))
res_tet_ref <- bootcamp::correlate(x_tet_ref, y_tet_ref, type = "tetrachoric")
expected_tet <- sin(0.3 * pi)
expect_equal(res_tet_ref$estimate, expected_tet, tolerance = 1e-3,
             info = "Tetrachoric van gebalanceerde 2x2 tabel moet sin(0.3*pi) zijn")

# F8. Zwakke associatie: tabel dicht bij onafhankelijkheid (25/25/25/25)
x_tet_weak <- c(rep(0L, 50), rep(1L, 50))
y_tet_weak <- c(rep(0L, 25), rep(1L, 25), rep(0L, 25), rep(1L, 25))
res_tet_weak <- bootcamp::correlate(x_tet_weak, y_tet_weak, type = "tetrachoric")
expect_true(abs(res_tet_weak$estimate) < 0.05,
            info = "Tetrachoric bij onafhankelijkheid moet dicht bij 0 zijn")


# =============================================================================
# G. INPUT-VALIDATIE EN FOUTMELDINGEN
# =============================================================================

# G1. x moet numeriek zijn
expect_error(bootcamp::correlate(letters[1:5], 1:5),
             pattern = "numerieke vector",
             info = "Fout bij niet-numerieke x")

# G2. y moet numeriek zijn
expect_error(bootcamp::correlate(1:5, letters[1:5]),
             pattern = "numerieke vector",
             info = "Fout bij niet-numerieke y")

# G3. x en y moeten gelijke lengte hebben
expect_error(bootcamp::correlate(1:5, 1:4),
             pattern = "dezelfde lengte",
             info = "Fout bij ongelijke lengtes")

# G4. Minimaal 3 observaties
expect_error(bootcamp::correlate(1:2, 2:1),
             pattern = "minimaal 3",
             info = "Fout bij te weinig observaties")

# G5. NAs geven fout als na.rm = FALSE
expect_error(bootcamp::correlate(c(1, 2, NA, 4), c(4, 3, 2, 1)),
             pattern = "Ontbrekende waarden",
             info = "Fout bij NAs zonder na.rm")

# G6. Biserial: fout als geen variabele binair is
expect_error(bootcamp::correlate(rnorm(50), rnorm(50), type = "biserial"),
             pattern = "binair",
             info = "Biserial fout bij twee continue variabelen")

# G7. Biserial: fout als beide variabelen binair zijn
expect_error(
  bootcamp::correlate(sample(0:1, 50, TRUE), sample(0:1, 50, TRUE),
                      type = "biserial"),
  pattern = "tetrachoric",
  info = "Biserial fout bij twee binaire variabelen")

# G8. Tetrachoric: fout als x niet binair is
expect_error(bootcamp::correlate(1:10, sample(0:1, 10, TRUE), type = "tetrachoric"),
             pattern = "binair",
             info = "Tetrachoric fout bij niet-binaire x")

# G9. Polychoric: fout als x niet ordinaal is
expect_error(bootcamp::correlate(rnorm(50), sample(1:3, 50, TRUE),
                                 type = "polychoric"),
             pattern = "gehele getallen",
             info = "Polychoric fout bij niet-ordinale x")

# G10. Constante variabele
expect_error(bootcamp::correlate(rep(5, 10), 1:10),
             pattern = "constant",
             info = "Fout bij constante x")

# G11. Waarschuwing bij pearson + twee binaire variabelen
expect_warning(
  bootcamp::correlate(sample(0:1, 50, TRUE), sample(0:1, 50, TRUE),
                      type = "pearson"),
  pattern = "tetrachoric",
  info = "Waarschuwing bij pearson met twee binaire variabelen")

# G12. Waarschuwing bij pearson + een binaire variabele
expect_warning(
  bootcamp::correlate(rnorm(50), sample(0:1, 50, TRUE), type = "pearson"),
  pattern = "biserial",
  info = "Waarschuwing bij pearson met een binaire variabele")


# =============================================================================
# H. RETURN-STRUCTUUR EN PRINT-METHODE
# =============================================================================

verwachte_namen <- c("estimate", "type", "n", "p.value", "statistic", "method")

# H1-H4. Controleer return-structuur voor ALLE typen
# Pearson
res_h_pear <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "pearson")
expect_true(inherits(res_h_pear, "bootcamp_correlate"),
            info = "Pearson: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_pear)),
            info = "Pearson: alle verwachte elementen aanwezig")
expect_true(is.numeric(res_h_pear$estimate),
            info = "Pearson: estimate moet numeriek zijn")
expect_true(is.numeric(res_h_pear$n) && res_h_pear$n == round(res_h_pear$n),
            info = "Pearson: n moet een geheel getal zijn")
expect_equal(res_h_pear$type, "pearson",
             info = "Pearson: type moet 'pearson' zijn")
expect_true(!is.na(res_h_pear$p.value),
            info = "Pearson: p-waarde moet beschikbaar zijn")
expect_true(!is.na(res_h_pear$statistic),
            info = "Pearson: statistiek moet beschikbaar zijn")

# Spearman
res_h_spear <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "spearman")
expect_true(inherits(res_h_spear, "bootcamp_correlate"),
            info = "Spearman: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_spear)),
            info = "Spearman: alle verwachte elementen aanwezig")
expect_equal(res_h_spear$type, "spearman",
             info = "Spearman: type moet 'spearman' zijn")
expect_true(!is.na(res_h_spear$p.value),
            info = "Spearman: p-waarde moet beschikbaar zijn")

# Kendall
res_h_kend <- bootcamp::correlate(mtcars$mpg, mtcars$hp, type = "kendall")
expect_true(inherits(res_h_kend, "bootcamp_correlate"),
            info = "Kendall: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_kend)),
            info = "Kendall: alle verwachte elementen aanwezig")
expect_equal(res_h_kend$type, "kendall",
             info = "Kendall: type moet 'kendall' zijn")
expect_true(!is.na(res_h_kend$p.value),
            info = "Kendall: p-waarde moet beschikbaar zijn")

# Biserial
res_h_bis <- bootcamp::correlate(mtcars$mpg, mtcars$am, type = "biserial")
expect_true(inherits(res_h_bis, "bootcamp_correlate"),
            info = "Biserial: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_bis)),
            info = "Biserial: alle verwachte elementen aanwezig")
expect_equal(res_h_bis$type, "biserial",
             info = "Biserial: type moet 'biserial' zijn")
expect_true(is.na(res_h_bis$p.value),
            info = "Biserial: p-waarde moet NA zijn")
expect_true(is.na(res_h_bis$statistic),
            info = "Biserial: statistiek moet NA zijn")

# Polyserial (hergebruik data uit sectie D)
res_h_ps <- bootcamp::correlate(x_poly, y_poly, type = "polyserial")
expect_true(inherits(res_h_ps, "bootcamp_correlate"),
            info = "Polyserial: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_ps)),
            info = "Polyserial: alle verwachte elementen aanwezig")
expect_equal(res_h_ps$type, "polyserial",
             info = "Polyserial: type moet 'polyserial' zijn")
expect_true(is.na(res_h_ps$p.value),
            info = "Polyserial: p-waarde moet NA zijn")
expect_true(is.na(res_h_ps$statistic),
            info = "Polyserial: statistiek moet NA zijn")

# Polychoric (hergebruik data uit sectie E)
res_h_pc <- bootcamp::correlate(x_ord, y_ord, type = "polychoric")
expect_true(inherits(res_h_pc, "bootcamp_correlate"),
            info = "Polychoric: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_pc)),
            info = "Polychoric: alle verwachte elementen aanwezig")
expect_equal(res_h_pc$type, "polychoric",
             info = "Polychoric: type moet 'polychoric' zijn")
expect_true(is.na(res_h_pc$p.value),
            info = "Polychoric: p-waarde moet NA zijn")

# Tetrachoric (hergebruik data uit sectie F)
res_h_tet <- bootcamp::correlate(x_bin, y_bin, type = "tetrachoric")
expect_true(inherits(res_h_tet, "bootcamp_correlate"),
            info = "Tetrachoric: klasse moet 'bootcamp_correlate' zijn")
expect_true(all(verwachte_namen %in% names(res_h_tet)),
            info = "Tetrachoric: alle verwachte elementen aanwezig")
expect_equal(res_h_tet$type, "tetrachoric",
             info = "Tetrachoric: type moet 'tetrachoric' zijn")
expect_true(is.na(res_h_tet$p.value),
            info = "Tetrachoric: p-waarde moet NA zijn")

# H5. Print-methode geeft geen fout voor elk type
for (res_print in list(res_h_pear, res_h_spear, res_h_kend,
                        res_h_bis, res_h_ps, res_h_pc, res_h_tet)) {
  expect_silent(capture.output(print(res_print)),
                info = paste("print() mag geen fout geven voor type", res_print$type))
}

# H6. Print-methode retourneert het object invisible
out <- capture.output(ret <- print(res_h_pear))
expect_true(inherits(ret, "bootcamp_correlate"),
            info = "print() moet het object invisible retourneren")


# =============================================================================
# I. NA-AFHANDELING
# =============================================================================

# --- I1-I3. Pearson met NAs ---
x_na <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
y_na <- c(10, NA, 8, 7, 6, 5, 4, 3, 2, 1)
res_na <- bootcamp::correlate(x_na, y_na, na.rm = TRUE)
expect_equal(res_na$n, 8,
             info = "Pearson NA: n moet het aantal complete observaties zijn")
compl <- complete.cases(x_na, y_na)
ref_na <- stats::cor(x_na[compl], y_na[compl])
expect_equal(res_na$estimate, ref_na, tolerance = 1e-10,
             info = "Pearson NA: schatting moet overeenkomen met handmatig gefilterd")
ref_na_test <- stats::cor.test(x_na[compl], y_na[compl])
expect_equal(res_na$p.value, ref_na_test$p.value, tolerance = 1e-10,
             info = "Pearson NA: p-waarde moet overeenkomen met handmatig gefilterd")

# --- I4. Spearman met NAs ---
res_na_spear <- bootcamp::correlate(x_na, y_na, type = "spearman", na.rm = TRUE)
ref_na_spear <- stats::cor.test(x_na[compl], y_na[compl], method = "spearman")
expect_equal(res_na_spear$estimate, unname(ref_na_spear$estimate), tolerance = 1e-10,
             info = "Spearman NA: schatting moet overeenkomen met handmatig gefilterd")
expect_equal(res_na_spear$n, 8,
             info = "Spearman NA: n moet 8 zijn na verwijdering")

# --- I5. Kendall met NAs ---
res_na_kend <- bootcamp::correlate(x_na, y_na, type = "kendall", na.rm = TRUE)
ref_na_kend <- stats::cor.test(x_na[compl], y_na[compl], method = "kendall")
expect_equal(res_na_kend$estimate, unname(ref_na_kend$estimate), tolerance = 1e-10,
             info = "Kendall NA: schatting moet overeenkomen met handmatig gefilterd")
expect_equal(res_na_kend$n, 8,
             info = "Kendall NA: n moet 8 zijn na verwijdering")

# --- I6. Biserial met NAs ---
set.seed(500)
x_na_bis <- c(rnorm(48), NA, NA)
y_na_bis <- c(sample(0:1, 48, TRUE), NA, 1)
res_na_bis <- bootcamp::correlate(x_na_bis, y_na_bis, type = "biserial", na.rm = TRUE)
expect_true(!is.na(res_na_bis$estimate),
            info = "Biserial NA: moet een niet-NA resultaat geven")
expect_equal(res_na_bis$n, sum(complete.cases(x_na_bis, y_na_bis)),
             info = "Biserial NA: n moet het aantal complete cases zijn")
# Vergelijk met handmatige berekening op gefilterde data
compl_bis <- complete.cases(x_na_bis, y_na_bis)
x_bis_clean <- x_na_bis[compl_bis]
y_bis_clean <- y_na_bis[compl_bis]
res_bis_manual <- bootcamp::correlate(x_bis_clean, y_bis_clean, type = "biserial")
expect_equal(res_na_bis$estimate, res_bis_manual$estimate, tolerance = 1e-10,
             info = "Biserial NA: resultaat moet gelijk zijn aan berekening op gefilterde data")

# --- I7. Polyserial met NAs ---
set.seed(501)
x_na_ps <- c(rnorm(48), NA, NA)
y_na_ps <- c(sample(1:4, 48, TRUE), NA, 3L)
res_na_ps <- bootcamp::correlate(x_na_ps, y_na_ps, type = "polyserial", na.rm = TRUE)
expect_true(!is.na(res_na_ps$estimate),
            info = "Polyserial NA: moet een niet-NA resultaat geven")
expect_equal(res_na_ps$n, sum(complete.cases(x_na_ps, y_na_ps)),
             info = "Polyserial NA: n moet het aantal complete cases zijn")

# --- I8. Polychoric met NAs ---
set.seed(502)
x_na_pc <- c(sample(1:3, 48, TRUE), NA, NA)
y_na_pc <- c(sample(1:3, 48, TRUE), NA, 2L)
res_na_pc <- bootcamp::correlate(x_na_pc, y_na_pc, type = "polychoric", na.rm = TRUE)
expect_true(!is.na(res_na_pc$estimate),
            info = "Polychoric NA: moet een niet-NA resultaat geven")
expect_equal(res_na_pc$n, sum(complete.cases(x_na_pc, y_na_pc)),
             info = "Polychoric NA: n moet het aantal complete cases zijn")
# Vergelijk met handmatige berekening op gefilterde data
compl_pc <- complete.cases(x_na_pc, y_na_pc)
res_pc_manual <- bootcamp::correlate(x_na_pc[compl_pc], y_na_pc[compl_pc],
                                     type = "polychoric")
expect_equal(res_na_pc$estimate, res_pc_manual$estimate, tolerance = 1e-10,
             info = "Polychoric NA: resultaat moet gelijk zijn aan berekening op gefilterde data")

# --- I9. Tetrachoric met NAs ---
set.seed(503)
x_na_tet <- c(sample(0:1, 48, TRUE), NA, NA)
y_na_tet <- c(sample(0:1, 48, TRUE), NA, 1L)
res_na_tet <- bootcamp::correlate(x_na_tet, y_na_tet, type = "tetrachoric", na.rm = TRUE)
expect_true(!is.na(res_na_tet$estimate),
            info = "Tetrachoric NA: moet een niet-NA resultaat geven")
expect_equal(res_na_tet$n, sum(complete.cases(x_na_tet, y_na_tet)),
             info = "Tetrachoric NA: n moet het aantal complete cases zijn")
# Vergelijk met handmatige berekening op gefilterde data
compl_tet <- complete.cases(x_na_tet, y_na_tet)
res_tet_manual <- bootcamp::correlate(x_na_tet[compl_tet], y_na_tet[compl_tet],
                                      type = "tetrachoric")
expect_equal(res_na_tet$estimate, res_tet_manual$estimate, tolerance = 1e-10,
             info = "Tetrachoric NA: resultaat moet gelijk zijn aan berekening op gefilterde data")

# --- I10. na.rm = FALSE moet een fout geven voor elk type ---
expect_error(bootcamp::correlate(c(1, 2, NA, 4), c(4, 3, 2, 1)),
             pattern = "Ontbrekende waarden",
             info = "na.rm=FALSE moet een fout geven bij NAs (pearson)")
expect_error(bootcamp::correlate(c(1, 2, NA, 4), c(4, 3, 2, 1), type = "spearman"),
             pattern = "Ontbrekende waarden",
             info = "na.rm=FALSE moet een fout geven bij NAs (spearman)")
expect_error(bootcamp::correlate(c(1, 2, NA, 4), c(4, 3, 2, 1), type = "kendall"),
             pattern = "Ontbrekende waarden",
             info = "na.rm=FALSE moet een fout geven bij NAs (kendall)")

# --- I11. Te weinig observaties na NA-verwijdering ---
x_mostly_na <- c(1, NA, NA, NA, NA)
y_mostly_na <- c(NA, NA, NA, NA, 5)
expect_error(bootcamp::correlate(x_mostly_na, y_mostly_na, na.rm = TRUE),
             pattern = "minder dan 3",
             info = "Te weinig observaties na NA-verwijdering moet een fout geven")

# --- I12. NA alleen in x, alleen in y, en in beide ---
x_na_x_only <- c(NA, 2, 3, 4, 5, 6)
y_na_x_only <- c(6, 5, 4, 3, 2, 1)
res_x_only <- bootcamp::correlate(x_na_x_only, y_na_x_only, na.rm = TRUE)
expect_equal(res_x_only$n, 5,
             info = "NA alleen in x: n moet 5 zijn")

x_na_y_only <- c(1, 2, 3, 4, 5, 6)
y_na_y_only <- c(NA, 5, 4, 3, 2, 1)
res_y_only <- bootcamp::correlate(x_na_y_only, y_na_y_only, na.rm = TRUE)
expect_equal(res_y_only$n, 5,
             info = "NA alleen in y: n moet 5 zijn")

# Pairwise deletion: NAs op verschillende posities in x en y
x_na_both <- c(NA, 2, 3, 4, 5, 6)
y_na_both <- c(6, 5, NA, 3, 2, 1)
res_both <- bootcamp::correlate(x_na_both, y_na_both, na.rm = TRUE)
expect_equal(res_both$n, 4,
             info = "NAs in x en y op verschillende posities: n moet 4 zijn")
