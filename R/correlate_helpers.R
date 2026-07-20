
# =============================================================================
# correlate_helpers.R
# =============================================================================
#
# Interne helper-functies voor de correlate() functie.
# Dit bestand bevat de rekenkundige kern van alle niet-standaard correlatietypen:
#   - pbivnorm():                        Bivariate normale CDF
#   - compute_standard_correlation():     Pearson, Kendall, Spearman (wrapper)
#   - compute_biserial_correlation():     Biseriale correlatie
#   - compute_polyserial_correlation():   Polyseriale correlatie (ML)
#   - compute_polychoric_correlation():   Polychorische correlatie (ML)
#   - compute_tetrachoric_correlation():  Tetrachorische correlatie (ML)
#
# ONTWERP-KEUZES:
# - Geen externe dependencies: alles is geimplementeerd met base R en stats::
# - pbivnorm() gebruikt het Drezner-Wesolowsky (1990) algoritme met
#   Gauss-Legendre kwadratuur (20 punten). Dit is dezelfde aanpak als het
#   pbivnorm CRAN-pakket (gebaseerd op Alan Genz's Fortran code).
# - Polychoric en polyserial gebruiken twee-staps ML-schatting:
#   stap 1: drempelwaarden schatten uit marginale verdelingen
#   stap 2: rho optimaliseren via stats::optim(method = "Brent")
#   Dit is de standaardaanpak (ook gebruikt in het polycor-pakket).
# - Tetrachoric is een speciaal geval van polychoric (2x2 tabel).
#   De cosinus-pi benadering wordt gebruikt als startwaarde voor de optimizer.
#
# BEKENDE BEPERKINGEN / TODO:
# - pbivnorm() is pure R en dus trager dan compiled Fortran. Voor enkele
#   bivariate berekeningen is dit geen probleem, maar voor grote
#   correlatiematrices zou een C/Fortran implementatie beter zijn.
# - pbivnorm() gebruikt directe kwadratuur voor alle rho-waarden. Bij
#   |rho| > ~0.99 kan de nauwkeurigheid licht afnemen, maar dit is
#   voldoende voor de optimizer die rho beperkt tot [-0.999, 0.999].
# - De polychoric/polyserial schattingen zijn twee-staps (drempels vast,
#   alleen rho optimaliseren). Een volledige ML-schatting (joint optimalisatie
#   van rho + alle drempels) zou nauwkeuriger zijn maar veel complexer.
# - Er wordt geen standaardfout of betrouwbaarheidsinterval berekend voor
#   de ML-gebaseerde correlaties. Dit zou in de toekomst via de Hessian
#   of bootstrap kunnen worden toegevoegd.
# - Polyserial ondersteunt alleen Pearson-gebaseerde schatting (geen
#   Spearman/Kendall variant).
# =============================================================================




# =============================================================================
# BIVARIATE NORMALE CDF — pbivnorm()
# =============================================================================

#' Bivariate normale cumulatieve verdelingsfunctie
#'
#' Berekent P(X <= x, Y <= y) voor een standaard bivariate normale verdeling
#' met correlatie rho, via het Drezner-Wesolowsky (1990) algoritme.
#'
#' Het algoritme gebruikt 20-punts Gauss-Legendre kwadratuur van de
#' integraalkernel.
#'
#' Referentie: Drezner, Z. & Wesolowsky, G.O. (1990). On the computation of
#' the bivariate normal integral. Journal of Statistical Computation and
#' Simulation, 35, 101-107.
#'
#' @param x numeriek, bovengrens voor de eerste variabele
#' @param y numeriek, bovengrens voor de tweede variabele
#' @param rho numeriek in [-1, 1], de correlatie
#'
#' @return numeriek, de bivariate normale CDF-waarde P(X <= x, Y <= y)
#'
#' @keywords internal
pbivnorm <- function(x, y, rho) {

  # --- Speciale gevallen: Inf / -Inf ---
  if (is.infinite(x) || is.infinite(y)) {
    if (x == -Inf || y == -Inf) return(0)
    if (x == Inf && y == Inf) return(1)
    if (x == Inf) return(stats::pnorm(y))
    if (y == Inf) return(stats::pnorm(x))
  }

  # --- Speciale gevallen: exacte oplossingen ---
  if (rho == 0) return(stats::pnorm(x) * stats::pnorm(y))
  if (rho == 1) return(stats::pnorm(min(x, y)))
  if (rho == -1) return(max(0, stats::pnorm(x) + stats::pnorm(y) - 1))

  # --- Gauss-Legendre kwadratuur nodes en gewichten ---
  # 20-punts op [0, 1]. Bron: Abramowitz & Stegun, tabel 25.4.
  gl_nodes <- c(
    0.0176140071391521, 0.0406014298003869, 0.0626720483341091,
    0.0832767415767048, 0.1019301198172404, 0.1181945319615184,
    0.1316886384491766, 0.1420961093183821, 0.1491729864726037,
    0.1527533871307258
  )
  gl_weights <- c(
    0.9931285991850949, 0.9639719272779138, 0.9122344282513259,
    0.8391169718222188, 0.7463319064601508, 0.6360536807265150,
    0.5108670019508271, 0.3737060887154196, 0.2277858511416451,
    0.0765265211334973
  )
  w <- c(gl_nodes, gl_nodes)
  t_nodes <- c(1 - gl_weights, 1 + gl_weights) / 2

  pbivnorm_low_corr(x, y, rho, w, t_nodes)
}


#' Bivariate normaal CDF voor lage correlatie (|rho| < 0.925)
#'
#' Directe Gauss-Legendre kwadratuur van de integraalkernel.
#' De transformatie brengt de integratie over [0, rho] naar een
#' gewogen som van functie-evaluaties.
#'
#' @param x,y bovengrenzen
#' @param rho correlatie
#' @param w kwadratuurgewichten
#' @param t_nodes kwadratuurknopen op [0, 1]
#'
#' @return numerieke CDF-waarde
#'
#' @keywords internal
pbivnorm_low_corr <- function(x, y, rho, w, t_nodes) {
  # Schaal de knopen naar het interval [0, rho]
  # De substitutie is: s = rho * t, waar t in [0, 1]
  rho_t <- rho * t_nodes

  # Evalueer de integraalkernel op elk kwadratuurpunt:
  #   f(s) = exp(-(x^2 + y^2 - 2*s*x*y) / (2*(1 - s^2))) / sqrt(1 - s^2)
  # Dit is de kern van de bivariate normale dichtheid, geparametriseerd
  # door de hulpcorrelatie s.
  one_minus_sq <- 1 - rho_t^2
  kernel_vals <- exp(-(x^2 + y^2 - 2 * rho_t * x * y) / (2 * one_minus_sq)) /
    sqrt(one_minus_sq)

  # De kwadratuur-benadering: integraal ≈ (rho/2) * sum(w_i * f(t_i))
  # Gedeeld door 2*pi (de normalisatieconstante van de bivariate normaal).
  bvn <- (rho / 2) * sum(w * kernel_vals)

  # Voeg het product van de marginale CDF's toe (basisterm)
  # Phi_2(x, y, rho) = Phi(x)*Phi(y) + integraalterm
  result <- stats::pnorm(x) * stats::pnorm(y) + bvn / (2 * pi)

  # Clamp naar [0, 1] voor numerieke veiligheid
  max(0, min(1, result))
}






# =============================================================================
# COMPUTE FUNCTIES — Elke correlatietype heeft zijn eigen compute-functie
# =============================================================================


#' Bereken standaard correlatie (Pearson, Kendall, Spearman)
#'
#' Wrapper rond \code{\link[stats]{cor.test}} die het resultaat omzet naar
#' het standaard return-formaat van \code{correlate()}.
#'
#' Keuze: we gebruiken cor.test() in plaats van cor() omdat cor.test() ook
#' p-waarde, teststatistiek en betrouwbaarheidsinterval levert. Dit maakt
#' het return-object rijker zonder extra berekeningen.
#'
#' @param x,y numerieke vectoren
#' @param type een van "pearson", "kendall", "spearman"
#'
#' @return lijst met $estimate, $type, $n, $p.value, $statistic, $method
#'
#' @keywords internal
compute_standard_correlation <- function(x, y, type) {
  result <- stats::cor.test(x, y, method = type)

  list(
    estimate  = unname(result$estimate),
    type      = type,
    n         = length(x),
    p.value   = result$p.value,
    statistic = unname(result$statistic),
    method    = result$method
  )
}


#' Bereken biseriale correlatie
#'
#' Berekent de biseriale correlatie tussen een continue variabele \code{x}
#' en een binaire variabele \code{y}.
#'
#' De biseriale correlatie corrigeert de punt-biseriale correlatie (= Pearson)
#' voor de kunstmatige dichotomisering van een onderliggende continue,
#' normaal verdeelde variabele. De formule is:
#'
#' \deqn{r_{bis} = r_{pb} \cdot \frac{\sqrt{p \cdot q}}{\phi(z_p)}}
#'
#' waar:
#' \itemize{
#'   \item \eqn{r_{pb}} = punt-biseriale correlatie (= Pearson correlatie)
#'   \item \eqn{p} = proportie observaties met y=1
#'   \item \eqn{q = 1 - p}
#'   \item \eqn{\phi(z_p)} = de dichtheid van de standaard normaal op het punt
#'     \eqn{z_p = \Phi^{-1}(p)}
#' }
#'
#' AANNAME: de onderliggende continue variabele die gedichtomiseerd is tot y
#' volgt een normale verdeling. Als deze aanname niet klopt, is de biseriale
#' correlatie niet zinvol.
#'
#' OPMERKING: de biseriale correlatie kan in theorie buiten [-1, 1] vallen
#' door de correctiefactor. We clampen het resultaat naar [-1, 1].
#'
#' @param x numerieke vector (continu)
#' @param y numerieke vector (binair, gecodeerd als 0/1)
#'
#' @return lijst met $estimate, $type, $n, $p.value, $statistic, $method
#'
#' @keywords internal
compute_biserial_correlation <- function(x, y) {
  n <- length(x)

  # Stap 1: Bereken de punt-biseriale correlatie (= gewone Pearson)
  r_pb <- stats::cor(x, y)

  # Stap 2: Bereken de proportie in de "1" groep
  p <- mean(y)
  q <- 1 - p

  # Stap 3: Bereken de correctiefactor
  # z_p = het punt op de standaard normaal waar P(Z <= z_p) = p
  # phi(z_p) = de dichtheidsfunctie op dat punt
  # De correctie sqrt(p*q) / phi(z_p) is altijd >= 1, waardoor
  # |r_bis| >= |r_pb|. Dit weerspiegelt dat de "echte" correlatie
  # hoger zou zijn als y niet gedichtomiseerd was.
  z_p <- stats::qnorm(p)
  phi_z <- stats::dnorm(z_p)

  # Bescherming tegen deling door nul (als p=0 of p=1, is phi_z ~0)
  if (phi_z < 1e-10) {
    warning(paste0("De binaire variabele is (bijna) constant ",
                   "(p = ", round(p, 4), "). ",
                   "De biseriale correlatie is niet betrouwbaar."))
    r_bis <- NA_real_
  } else {
    r_bis <- r_pb * sqrt(p * q) / phi_z
  }

  # Stap 4: Clamp naar [-1, 1]
  # De biseriale correlatie kan theoretisch buiten deze grenzen vallen
  # door de correctiefactor, maar waarden buiten [-1, 1] zijn niet
  # interpreteerbaar als correlatie.
  if (!is.na(r_bis)) {
    r_bis <- max(-1, min(1, r_bis))
  }

  list(
    estimate  = r_bis,
    type      = "biserial",
    n         = n,
    p.value   = NA_real_,
    statistic = NA_real_,
    method    = "Biserial correlation (corrected point-biserial)"
  )
}


#' Bereken polyseriale correlatie via maximum likelihood
#'
#' Berekent de polyseriale correlatie tussen een continue variabele \code{x}
#' en een ordinale variabele \code{y}.
#'
#' De polyseriale correlatie schat de Pearson-correlatie tussen \code{x} en
#' een latente continue variabele \eqn{Y^*} die ten grondslag ligt aan de
#' ordinale variabele \code{y}. Het model veronderstelt dat \eqn{Y^*} normaal
#' verdeeld is en dat de ordinale categorien ontstaan door drempelwaarden
#' (thresholds) op \eqn{Y^*}.
#'
#' METHODE: Twee-staps maximum likelihood (Olsson, Drasgow & Dorans, 1982):
#' \enumerate{
#'   \item Schat drempelwaarden uit de marginale verdeling van y
#'   \item Optimaliseer rho via de log-likelihood met stats::optim(method="Brent")
#' }
#'
#' De log-likelihood is: \code{sum_i log P(Y = y_i | x_i, rho, tau)}
#' waar \code{P(Y = k | x, rho, tau) = Phi((tau_k - rho*x) / sqrt(1-rho^2))
#'                                - Phi((tau_(k-1) - rho*x) / sqrt(1-rho^2))}
#'
#' KEUZE: we gebruiken Brent's methode (1D optimalisatie op [-0.999, 0.999])
#' in plaats van een meer algemene optimizer, omdat we maar 1 parameter
#' optimaliseren (rho). Brent is hiervoor snel en betrouwbaar.
#'
#' OPMERKING: x wordt gestandaardiseerd voor de berekening om numerieke
#' stabiliteit te verbeteren.
#'
#' @param x numerieke vector (continu)
#' @param y numerieke vector (ordinaal, gehele getallen)
#'
#' @return lijst met $estimate, $type, $n, $p.value, $statistic, $method
#'
#' @keywords internal
compute_polyserial_correlation <- function(x, y) {
  n <- length(x)

  # --- Stap 1: Bereid data voor ---
  # Hercodeer y naar opeenvolgende gehele getallen 1, 2, ..., K
  # Dit is nodig zodat we de categorien als indices kunnen gebruiken.
  y_levels <- sort(unique(y))
  K <- length(y_levels)
  y_coded <- match(y, y_levels)  # nu: y_coded in {1, 2, ..., K}

  # Standaardiseer x (mean=0, sd=1) voor numerieke stabiliteit
  # De correlatie is schaal-invariant, dus dit verandert het resultaat niet.
  x_std <- (x - mean(x)) / stats::sd(x)

  # --- Stap 2: Schat drempelwaarden (thresholds) uit de marginale verdeling ---
  # De drempels tau_1, ..., tau_{K-1} worden geschat als de kwantielen van de
  # standaard normaal die corresponderen met de cumulatieve proporties van y.
  # Voorbeeld: als 30% van y in categorie 1 valt, dan tau_1 = qnorm(0.30)
  cumprops <- cumsum(table(y_coded) / n)
  # Verwijder de laatste (die is altijd 1.0, en qnorm(1) = Inf)
  tau <- stats::qnorm(cumprops[-K])
  # Voeg -Inf en +Inf toe als grenzen: tau_0 = -Inf, tau_K = +Inf
  tau_extended <- c(-Inf, tau, Inf)

  # --- Stap 3: Definieer de log-likelihood functie ---
  # Voor elke observatie i berekenen we P(Y = y_i | x_i, rho):
  # P(Y = k | x, rho) = Phi((tau_k - rho*x) / sqrt(1-rho^2))
  #                    - Phi((tau_{k-1} - rho*x) / sqrt(1-rho^2))
  # De totale log-likelihood is sum(log(P(Y=y_i | x_i, rho)))
  polyserial_negloglik <- function(rho) {
    # sqrt(1 - rho^2) is de conditionele standaarddeviatie
    denom <- sqrt(1 - rho^2)

    # Voor elke observatie: bereken de probability van de waargenomen categorie
    upper_idx <- y_coded + 1   # index in tau_extended voor bovengrens
    lower_idx <- y_coded       # index in tau_extended voor ondergrens

    # Phi((tau_upper - rho * x) / denom) - Phi((tau_lower - rho * x) / denom)
    prob_upper <- stats::pnorm((tau_extended[upper_idx] - rho * x_std) / denom)
    prob_lower <- stats::pnorm((tau_extended[lower_idx] - rho * x_std) / denom)
    probs <- prob_upper - prob_lower

    # Floor op 1e-15 om log(0) te voorkomen
    probs <- pmax(probs, 1e-15)

    # Return NEGATIEVE log-likelihood (want optim minimaliseert)
    -sum(log(probs))
  }

  # --- Stap 4: Startwaarde ---
  # Gebruik de Pearson correlatie van x en y_coded als startwaarde.
  # Dit is een grove benadering maar geeft de optimizer een goede richting.
  start_rho <- stats::cor(x_std, y_coded)
  # Clamp naar (-0.99, 0.99) om randproblemen te voorkomen
  start_rho <- max(-0.99, min(0.99, start_rho))

  # --- Stap 5: Optimaliseer ---
  # Brent's methode is ideaal voor 1D optimalisatie op een begrensd interval.
  opt <- stats::optim(
    par     = start_rho,
    fn      = polyserial_negloglik,
    method  = "Brent",
    lower   = -0.999,
    upper   = 0.999
  )

  # Controleer convergentie
  if (opt$convergence != 0) {
    warning("Polyseriale correlatie: optimizer is niet geconvergeerd. ",
            "Resultaat kan onbetrouwbaar zijn.")
  }

  rho_hat <- opt$par

  list(
    estimate  = rho_hat,
    type      = "polyserial",
    n         = n,
    p.value   = NA_real_,
    statistic = NA_real_,
    method    = "Polyserial correlation (two-step ML)"
  )
}


#' Bereken polychorische correlatie via maximum likelihood
#'
#' Berekent de polychorische correlatie tussen twee ordinale variabelen.
#'
#' De polychorische correlatie schat de Pearson-correlatie tussen twee latente
#' continue, normaal verdeelde variabelen \eqn{X^*} en \eqn{Y^*} die ten
#' grondslag liggen aan de waargenomen ordinale variabelen.
#'
#' METHODE: Twee-staps maximum likelihood (Olsson, 1979):
#' \enumerate{
#'   \item Schat drempelwaarden uit de marginale verdelingen van x en y
#'   \item Optimaliseer rho door de log-likelihood te maximaliseren
#' }
#'
#' De log-likelihood is gebaseerd op de verwachte celfrequenties van de
#' kruistabel. De verwachte proportie in cel (j, k) is:
#' \deqn{\pi_{jk} = \Phi_2(\tau^x_j, \tau^y_k, \rho)
#'   - \Phi_2(\tau^x_{j-1}, \tau^y_k, \rho)
#'   - \Phi_2(\tau^x_j, \tau^y_{k-1}, \rho)
#'   + \Phi_2(\tau^x_{j-1}, \tau^y_{k-1}, \rho)}
#' waar \eqn{\Phi_2} de bivariate normale CDF is (pbivnorm).
#'
#' KEUZE: De twee-staps methode is minder efficient dan volledige ML
#' (joint optimalisatie van rho + drempels), maar veel eenvoudiger en
#' robuuster. Voor de meeste toepassingen in een onderwijspakket is dit
#' voldoende nauwkeurig.
#'
#' BEKENDE BEPERKING: Bij lege cellen in de kruistabel kan de schatting
#' minder nauwkeurig zijn. We voegen geen continuïteitscorrectie toe aan
#' de celfrequenties, maar de log-likelihood kijkt alleen naar cellen met
#' n > 0 en de kansen worden gefloord op 1e-15.
#'
#' @param x,y numerieke vectoren (ordinaal, gehele getallen)
#'
#' @return lijst met $estimate, $type, $n, $p.value, $statistic, $method
#'
#' @keywords internal
compute_polychoric_correlation <- function(x, y) {
  n <- length(x)

  # --- Stap 1: Hercodeer naar opeenvolgende gehele getallen ---
  x_levels <- sort(unique(x))
  y_levels <- sort(unique(y))
  x_coded <- match(x, x_levels)
  y_coded <- match(y, y_levels)
  K_x <- length(x_levels)
  K_y <- length(y_levels)

  # --- Stap 2: Kruistabel ---
  # De kruistabel is de basis voor de log-likelihood.
  # Rijen = categorien van x, kolommen = categorien van y.
  cont_table <- table(x_coded, y_coded)

  # --- Stap 3: Drempelwaarden (thresholds) uit marginale verdelingen ---
  # tau_x[k] = qnorm(cumulatieve proportie tot en met categorie k)
  cumprops_x <- cumsum(rowSums(cont_table) / n)
  cumprops_y <- cumsum(colSums(cont_table) / n)
  tau_x <- c(-Inf, stats::qnorm(cumprops_x[-K_x]), Inf)
  tau_y <- c(-Inf, stats::qnorm(cumprops_y[-K_y]), Inf)

  # --- Stap 4: Log-likelihood functie ---
  # Voor elke cel (j, k) met n_jk > 0:
  #   pi_jk = Phi_2(tau_x[j+1], tau_y[k+1], rho)
  #         - Phi_2(tau_x[j],   tau_y[k+1], rho)
  #         - Phi_2(tau_x[j+1], tau_y[k],   rho)
  #         + Phi_2(tau_x[j],   tau_y[k],   rho)
  # Loglik = sum( n_jk * log(pi_jk) )

  # Identificeer niet-lege cellen (voor efficientie)
  cells <- which(cont_table > 0, arr.ind = TRUE)
  cell_counts <- cont_table[cells]

  polychoric_negloglik <- function(rho) {
    loglik <- 0
    for (idx in seq_len(nrow(cells))) {
      j <- cells[idx, 1]
      k <- cells[idx, 2]
      n_jk <- cell_counts[idx]

      # Bereken de verwachte proportie pi_jk via de inclusion-exclusion formule
      # met de bivariate normale CDF
      p11 <- pbivnorm(tau_x[j + 1], tau_y[k + 1], rho)
      p10 <- pbivnorm(tau_x[j],     tau_y[k + 1], rho)
      p01 <- pbivnorm(tau_x[j + 1], tau_y[k],     rho)
      p00 <- pbivnorm(tau_x[j],     tau_y[k],     rho)

      pi_jk <- p11 - p10 - p01 + p00

      # Floor om log(0) te voorkomen
      pi_jk <- max(pi_jk, 1e-15)

      loglik <- loglik + n_jk * log(pi_jk)
    }

    # Return NEGATIEVE log-likelihood
    -loglik
  }

  # --- Stap 5: Startwaarde ---
  # Gebruik Pearson correlatie van de gecodeerde waarden als grove benadering
  start_rho <- stats::cor(x_coded, y_coded)
  start_rho <- max(-0.99, min(0.99, start_rho))

  # --- Stap 6: Optimaliseer ---
  opt <- stats::optim(
    par     = start_rho,
    fn      = polychoric_negloglik,
    method  = "Brent",
    lower   = -0.999,
    upper   = 0.999
  )

  if (opt$convergence != 0) {
    warning("Polychorische correlatie: optimizer is niet geconvergeerd. ",
            "Resultaat kan onbetrouwbaar zijn.")
  }

  rho_hat <- opt$par

  list(
    estimate  = rho_hat,
    type      = "polychoric",
    n         = n,
    p.value   = NA_real_,
    statistic = NA_real_,
    method    = "Polychoric correlation (two-step ML)"
  )
}


#' Bereken tetrachorische correlatie
#'
#' Berekent de tetrachorische correlatie tussen twee binaire variabelen.
#'
#' De tetrachorische correlatie is een speciaal geval van de polychorische
#' correlatie voor twee binaire variabelen (2x2 tabel). Het schat de
#' Pearson-correlatie tussen twee latente normaal verdeelde variabelen.
#'
#' METHODE: We gebruiken twee benaderingen:
#' \enumerate{
#'   \item Cosinus-pi benadering als startwaarde:
#'     \eqn{r_{tet} = \cos(\pi / (1 + \sqrt{OR}))}
#'     waar OR de odds ratio is. Dit is de Bonett & Price (2005) formule.
#'   \item ML-verfijning via de polychorische log-likelihood (2x2 geval)
#' }
#'
#' KEUZE: We gebruiken de cosinus-pi benadering niet als eindresultaat maar
#' als startwaarde voor de ML-optimizer. Dit geeft maximale consistentie
#' met de polychorische schatting (waarvan tetrachoric een speciaal geval is)
#' terwijl de goede startwaarde zorgt voor snelle convergentie.
#'
#' BIJZONDER GEVAL: Als een cel van de 2x2 tabel 0 is, voegen we de
#' Haldane-Anscombe correctie toe (0.5 bij alle cellen) voor de startwaarde-
#' berekening. De ML-schatting zelf kan hier ook moeite mee hebben —
#' een lege cel impliceert potentieel een perfecte correlatie.
#'
#' @param x,y numerieke vectoren (binair)
#'
#' @return lijst met $estimate, $type, $n, $p.value, $statistic, $method
#'
#' @keywords internal
compute_tetrachoric_correlation <- function(x, y) {
  n <- length(x)

  # --- Stap 1: Maak de 2x2 kruistabel ---
  # Hercodeer naar 0/1 als dat nog niet zo is
  x_vals <- sort(unique(x))
  y_vals <- sort(unique(y))
  x_bin <- as.integer(x == x_vals[2])
  y_bin <- as.integer(y == y_vals[2])

  tab <- table(x_bin, y_bin)
  a <- tab[1, 1]  # (0, 0)
  b <- tab[1, 2]  # (0, 1)
  c <- tab[2, 1]  # (1, 0)
  d <- tab[2, 2]  # (1, 1)

  # --- Stap 2: Cosinus-pi benadering als startwaarde ---
  # OR = (a * d) / (b * c) = odds ratio
  # r_tet ≈ cos(pi / (1 + sqrt(OR)))
  # Bij een lege cel: Haldane-Anscombe correctie (+0.5 bij alle cellen)
  if (any(c(a, b, c, d) == 0)) {
    a_h <- a + 0.5
    b_h <- b + 0.5
    c_h <- c + 0.5
    d_h <- d + 0.5
  } else {
    a_h <- a
    b_h <- b
    c_h <- c
    d_h <- d
  }

  odds_ratio <- (a_h * d_h) / (b_h * c_h)

  # Speciale gevallen voor de odds ratio
  if (odds_ratio <= 0) {
    # Kan niet voorkomen met de Haldane correctie, maar voor de zekerheid
    start_rho <- 0
  } else if (is.infinite(odds_ratio)) {
    start_rho <- 0.99
  } else {
    start_rho <- cos(pi / (1 + sqrt(odds_ratio)))
  }

  # Clamp startwaarde
  start_rho <- max(-0.99, min(0.99, start_rho))

  # --- Stap 3: ML-schatting via polychorische log-likelihood ---
  # Dit is het 2x2 geval van de polychorische correlatie.
  # Drempelwaarden: tau_x = qnorm(P(X=0)), tau_y = qnorm(P(Y=0))
  p_x0 <- mean(x_bin == 0)
  p_y0 <- mean(y_bin == 0)
  tau_x <- stats::qnorm(p_x0)
  tau_y <- stats::qnorm(p_y0)

  # Cel-indices en counts voor de likelihood
  cells_idx <- which(tab > 0, arr.ind = TRUE)
  cell_counts <- tab[cells_idx]

  # Drempelvectoren (met -Inf en +Inf als grenzen)
  tau_x_vec <- c(-Inf, tau_x, Inf)
  tau_y_vec <- c(-Inf, tau_y, Inf)

  tetrachoric_negloglik <- function(rho) {
    loglik <- 0
    for (idx in seq_len(nrow(cells_idx))) {
      j <- cells_idx[idx, 1]
      k <- cells_idx[idx, 2]
      n_jk <- cell_counts[idx]

      pi_jk <- pbivnorm(tau_x_vec[j + 1], tau_y_vec[k + 1], rho) -
               pbivnorm(tau_x_vec[j],     tau_y_vec[k + 1], rho) -
               pbivnorm(tau_x_vec[j + 1], tau_y_vec[k],     rho) +
               pbivnorm(tau_x_vec[j],     tau_y_vec[k],     rho)

      pi_jk <- max(pi_jk, 1e-15)
      loglik <- loglik + n_jk * log(pi_jk)
    }
    -loglik
  }

  # --- Stap 4: Optimaliseer ---
  opt <- stats::optim(
    par     = start_rho,
    fn      = tetrachoric_negloglik,
    method  = "Brent",
    lower   = -0.999,
    upper   = 0.999
  )

  if (opt$convergence != 0) {
    warning("Tetrachorische correlatie: optimizer is niet geconvergeerd. ",
            "Resultaat kan onbetrouwbaar zijn.")
  }

  rho_hat <- opt$par

  list(
    estimate  = rho_hat,
    type      = "tetrachoric",
    n         = n,
    p.value   = NA_real_,
    statistic = NA_real_,
    method    = "Tetrachoric correlation (ML via bivariate normal)"
  )
}
