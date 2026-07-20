
# =============================================================================
# correlate.R
# =============================================================================
#
# Hoofdbestand voor de correlate() functie — een uniforme interface voor
# 7 soorten correlaties:
#   1. pearson     — standaard Pearson product-moment correlatie
#   2. kendall     — Kendall's tau (rangcorrelatie)
#   3. spearman    — Spearman's rho (rangcorrelatie)
#   4. biserial    — biseriale correlatie (continu x binair)
#   5. polyserial  — polyseriale correlatie (continu x ordinaal)
#   6. polychoric  — polychorische correlatie (ordinaal x ordinaal)
#   7. tetrachoric — tetrachorische correlatie (binair x binair)
#
# ARCHITECTUUR:
# - correlate() is de exported dispatcher-functie
# - validate_correlate_inputs() doet alle input-validatie
# - De compute-functies staan in correlate_helpers.R
# - print.bootcamp_correlate() geeft nette console-output
#
# ONTWERP-KEUZES:
# - x en y zijn altijd vectoren (geen matrix-input). Voor matrix-correlaties
#   bestaan corrMat() en corrNetwork() al in dit pakket.
# - Bij biserial/polyserial wordt automatisch geswapped als x de binaire/
#   ordinale variabele is (met een message). Dit is gebruiksvriendelijk
#   maar de gebruiker wordt wel geinformeerd.
# - Slimme waarschuwingen bij ongebruikelijke combinaties (bijv. Pearson
#   op twee binaire variabelen).
#
# BEKENDE BEPERKINGEN / TODO:
# - Geen standaardfout of CI voor ML-gebaseerde typen (biserial, polyserial,
#   polychoric, tetrachoric). Zou via bootstrap of Hessian kunnen.
# - Geen matrix-variant (correlate_matrix). Eventueel later toevoegen.
# - Polyserial/polychoric veronderstellen normaal verdeelde latente
#   variabelen. Er is geen test of diagnostiek hiervoor ingebouwd.
# =============================================================================




#' Bereken een correlatie
#'
#' Berekent de correlatie tussen twee variabelen met een keuze uit zeven
#' verschillende correlatietypen.
#'
#' \code{correlate()} biedt een uniforme interface voor het berekenen van
#' correlaties, van standaard Pearson tot gespecialiseerde typen zoals
#' polychorisch en tetrachorisch. De functie detecteert automatisch of de
#' invoervariabelen geschikt zijn voor het gevraagde type en geeft
#' waarschuwingen bij ongebruikelijke combinaties.
#'
#' @section Beschikbare typen:
#' \describe{
#'   \item{pearson}{Pearson product-moment correlatie. Geschikt voor twee
#'     continue variabelen. Veronderstelt een lineair verband.}
#'   \item{kendall}{Kendall's tau rangcorrelatie. Geschikt voor ordinale
#'     of continue variabelen. Robuuster dan Pearson bij niet-lineariteit.}
#'   \item{spearman}{Spearman's rho rangcorrelatie. Geschikt voor ordinale
#'     of continue variabelen. Gebaseerd op rangen.}
#'   \item{biserial}{Biseriale correlatie. Geschikt wanneer een variabele
#'     continu is en de andere binair (0/1). Corrigeert de punt-biseriale
#'     correlatie onder de aanname dat de binaire variabele een
#'     gedichtomiseerde normaal verdeelde variabele is.}
#'   \item{polyserial}{Polyseriale correlatie. Geschikt wanneer een variabele
#'     continu is en de andere ordinaal (gehele getallen). Schat de correlatie
#'     met de latente continue variabele via maximum likelihood.}
#'   \item{polychoric}{Polychorische correlatie. Geschikt wanneer beide
#'     variabelen ordinaal zijn. Schat de correlatie tussen de onderliggende
#'     latente normaal verdeelde variabelen via maximum likelihood.}
#'   \item{tetrachoric}{Tetrachorische correlatie. Geschikt wanneer beide
#'     variabelen binair zijn. Speciaal geval van polychorisch voor
#'     2x2 tabellen.}
#' }
#'
#' @section Wanneer welk type:
#' \tabular{lll}{
#'   \strong{x}      \tab \strong{y}      \tab \strong{Aanbevolen type}   \cr
#'   continu          \tab continu          \tab pearson, kendall, spearman \cr
#'   continu          \tab binair           \tab biserial                   \cr
#'   continu          \tab ordinaal         \tab polyserial                 \cr
#'   ordinaal         \tab ordinaal         \tab polychoric                 \cr
#'   binair           \tab binair           \tab tetrachoric
#' }
#'
#' @section Input-validatie:
#' De functie controleert of x en y geschikt zijn voor het gevraagde type:
#' \itemize{
#'   \item Bij \code{biserial}: precies een variabele moet binair zijn. Als
#'     \code{x} binair is en \code{y} continu, worden ze automatisch omgewisseld
#'     (met een melding).
#'   \item Bij \code{polyserial}: een variabele moet ordinaal zijn (gehele
#'     getallen). Automatische swap als x ordinaal is.
#'   \item Bij \code{polychoric}: beide variabelen moeten ordinaal zijn.
#'   \item Bij \code{tetrachoric}: beide variabelen moeten binair zijn.
#'   \item Bij \code{pearson/kendall/spearman} met twee binaire variabelen:
#'     een waarschuwing suggereert tetrachoric.
#' }
#'
#' @param x numerieke vector
#' @param y numerieke vector (zelfde lengte als \code{x})
#' @param type karakter, het type correlatie. Een van \code{"pearson"}
#'   (standaard), \code{"kendall"}, \code{"spearman"}, \code{"biserial"},
#'   \code{"polyserial"}, \code{"polychoric"}, of \code{"tetrachoric"}.
#' @param na.rm logisch. Als \code{TRUE}, worden ontbrekende waarden (NAs)
#'   verwijderd via pairwise complete-case deletion. Als \code{FALSE} (standaard),
#'   geeft de functie een fout als er NAs aanwezig zijn.
#'
#' @return Een lijst van klasse \code{"bootcamp_correlate"} met de volgende
#'   elementen:
#'   \describe{
#'     \item{estimate}{numeriek, de geschatte correlatie}
#'     \item{type}{karakter, het gebruikte correlatietype}
#'     \item{n}{geheel getal, het aantal gebruikte observaties}
#'     \item{p.value}{numeriek, de p-waarde (alleen beschikbaar voor
#'       pearson/kendall/spearman; \code{NA} voor de overige typen)}
#'     \item{statistic}{numeriek, de teststatistiek (alleen beschikbaar voor
#'       pearson/kendall/spearman; \code{NA} voor de overige typen)}
#'     \item{method}{karakter, beschrijving van de gebruikte methode}
#'   }
#'
#' @references
#' Pearson, K. (1895). Notes on regression and inheritance in the case of
#' two parents. \emph{Proceedings of the Royal Society of London}, 58, 240--242.
#'
#' Kendall, M.G. (1938). A new measure of rank correlation.
#' \emph{Biometrika}, 30(1/2), 81--93.
#'
#' Spearman, C. (1904). The proof and measurement of association between
#' two things. \emph{American Journal of Psychology}, 15(1), 72--101.
#'
#' Pearson, K. (1909). On a new method of determining correlation between a
#' measured character A, and a character B. \emph{Biometrika}, 7(1/2), 96--105.
#' (biseriale correlatie)
#'
#' Olsson, U., Drasgow, F. & Dorans, N.J. (1982). The polyserial correlation
#' coefficient. \emph{Psychometrika}, 47(3), 337--347.
#'
#' Olsson, U. (1979). Maximum likelihood estimation of the polychoric
#' correlation coefficient. \emph{Psychometrika}, 44(4), 443--460.
#'
#' Drezner, Z. & Wesolowsky, G.O. (1990). On the computation of the bivariate
#' normal integral. \emph{Journal of Statistical Computation and Simulation},
#' 35, 101--107.
#'
#' @seealso \code{\link{corrMat}} voor een correlatiematrix,
#'   \code{\link{corrNetwork}} voor een netwerkvisualisatie van correlaties,
#'   \code{\link{partial_corr}} voor partiele correlaties,
#'   \code{\link[stats]{cor.test}} voor de standaard R-implementatie.
#'
#' @export
#'
#' @examples
#' # --- Standaard correlaties ---
#'
#' # Pearson correlatie (standaard)
#' correlate(mtcars$mpg, mtcars$hp)
#'
#' # Spearman rangcorrelatie
#' correlate(mtcars$mpg, mtcars$hp, type = "spearman")
#'
#' # Kendall's tau
#' correlate(mtcars$mpg, mtcars$hp, type = "kendall")
#'
#' # --- Gespecialiseerde correlaties ---
#'
#' # Biseriale correlatie (continu vs. binair)
#' correlate(mtcars$mpg, mtcars$am, type = "biserial")
#'
#' # Polyseriale correlatie (continu vs. ordinaal)
#' correlate(mtcars$mpg, mtcars$gear, type = "polyserial")
#'
#' # Tetrachorische correlatie (binair vs. binair)
#' correlate(mtcars$am, mtcars$vs, type = "tetrachoric")
#'
#' # Polychorische correlatie (ordinaal vs. ordinaal)
#' correlate(mtcars$gear, mtcars$carb, type = "polychoric")
#'
#' # --- Werken met het resultaat ---
#'
#' # Het resultaat is een lijst; je kunt individuele elementen ophalen
#' res <- correlate(mtcars$mpg, mtcars$hp)
#' res$estimate   # de correlatiecoefficient
#' res$p.value    # de p-waarde
#' res$n          # het aantal observaties
#'
#' # --- Automatische swap ---
#'
#' # Bij biserial: als x binair is, worden x en y automatisch omgewisseld
#' # (de continue variabele wordt altijd x)
#' correlate(mtcars$am, mtcars$mpg, type = "biserial")
#'
#' # --- Omgaan met ontbrekende waarden ---
#'
#' x <- c(1, 2, NA, 4, 5)
#' y <- c(5, 4, 3, NA, 1)
#' correlate(x, y, na.rm = TRUE)
correlate <- function(x, y,
                      type = c("pearson", "kendall", "spearman",
                               "biserial", "polyserial",
                               "polychoric", "tetrachoric"),
                      na.rm = FALSE) {

  # --- Stap 1: Match het type argument ---
  type <- match.arg(type)

  # --- Stap 2: Valideer de invoer ---
  # validate_correlate_inputs() controleert types, lengtes, NAs,
  # en of x/y passen bij het gevraagde correlatietype.
  # Het kan x en y swappen (bijv. als x binair is bij biserial).
  validated <- validate_correlate_inputs(x, y, type, na.rm)
  x <- validated$x
  y <- validated$y

  # --- Stap 3: Dispatch naar de juiste compute-functie ---
  result <- switch(type,
    pearson   = compute_standard_correlation(x, y, "pearson"),
    kendall   = compute_standard_correlation(x, y, "kendall"),
    spearman  = compute_standard_correlation(x, y, "spearman"),
    biserial  = compute_biserial_correlation(x, y),
    polyserial = compute_polyserial_correlation(x, y),
    polychoric = compute_polychoric_correlation(x, y),
    tetrachoric = compute_tetrachoric_correlation(x, y)
  )

  # --- Stap 4: Voeg klasse toe voor de print-methode ---
  class(result) <- "bootcamp_correlate"

  result
}




# =============================================================================
# INPUT VALIDATIE
# =============================================================================

#' Valideer invoer voor correlate()
#'
#' Controleert of \code{x} en \code{y} geschikt zijn voor het gevraagde
#' correlatietype. Geeft duidelijke foutmeldingen en waarschuwingen.
#'
#' Deze functie doet het volgende:
#' \enumerate{
#'   \item Basiscontroles: numeriek, gelijke lengte, minimaal 3 observaties
#'   \item NA-afhandeling: verwijderen (na.rm=TRUE) of fout (na.rm=FALSE)
#'   \item Type-specifieke controles: zijn x en y geschikt voor het type?
#'   \item Slimme waarschuwingen: suggesties voor een beter type
#'   \item Automatische swap: bij biserial/polyserial als x de "verkeerde" is
#' }
#'
#' KEUZE: "binair" wordt gedefinieerd als precies 2 unieke waarden.
#' We eisen niet dat dit 0/1 is — de compute-functies hercoderen zelf.
#'
#' KEUZE: "ordinaal" wordt gedefinieerd als: alle waarden zijn gehele getallen
#' (x == round(x)). Dit is een pragmatische definitie; in werkelijkheid
#' kan een continue variabele ook gehele waarden hebben. De gebruiker
#' moet zelf beoordelen of het type zinvol is.
#'
#' @param x,y numerieke vectoren
#' @param type karakter, het gevraagde correlatietype
#' @param na.rm logisch, moeten NAs verwijderd worden?
#'
#' @return lijst met $x en $y (schone vectoren, mogelijk geswapped)
#'
#' @keywords internal
validate_correlate_inputs <- function(x, y, type, na.rm) {

  # --- Basiscontroles ---
  if (!is.numeric(x)) {
    stop("'x' moet een numerieke vector zijn")
  }
  if (!is.numeric(y)) {
    stop("'y' moet een numerieke vector zijn")
  }
  if (length(x) != length(y)) {
    stop("'x' en 'y' moeten dezelfde lengte hebben (x heeft ", length(x),
         " elementen, y heeft ", length(y), " elementen)")
  }
  if (length(x) < 3) {
    stop("Er zijn minimaal 3 observaties nodig (huidige lengte: ", length(x), ")")
  }

  # --- NA-afhandeling ---
  if (na.rm) {
    # Pairwise complete-case deletion: verwijder rijen waar x OF y NA is
    complete <- stats::complete.cases(x, y)
    n_removed <- sum(!complete)
    if (n_removed > 0) {
      x <- x[complete]
      y <- y[complete]
      message(n_removed, " observatie(s) verwijderd vanwege ontbrekende waarden")
    }
    if (length(x) < 3) {
      stop("Na verwijdering van ontbrekende waarden zijn er minder dan 3 ",
           "complete observaties over (n = ", length(x), ")")
    }
  } else {
    if (anyNA(x) || anyNA(y)) {
      stop("Ontbrekende waarden gevonden in x of y. ",
           "Gebruik na.rm = TRUE om deze te verwijderen, ",
           "of verwijder ze handmatig.")
    }
  }

  # --- Detecteer variabele-eigenschappen ---
  x_n_unique <- length(unique(x))
  y_n_unique <- length(unique(y))
  x_binary <- (x_n_unique == 2)
  y_binary <- (y_n_unique == 2)
  # "Ordinaal" = gehele waarden. We controleren of ALLE waarden gelijk zijn
  # aan hun afgeronde versie.
  x_integer_valued <- all(x == round(x))
  y_integer_valued <- all(y == round(y))

  # --- Slimme waarschuwingen voor standaard typen ---
  # Als de gebruiker pearson/kendall/spearman vraagt maar de data suggereren
  # dat een ander type beter zou zijn, geven we een waarschuwing.
  if (type %in% c("pearson", "kendall", "spearman")) {
    if (x_binary && y_binary) {
      warning("Beide variabelen zijn binair. ",
              "Overweeg type = 'tetrachoric' voor de tetrachorische correlatie.")
    } else if (x_binary || y_binary) {
      warning("Een van de variabelen is binair. ",
              "Overweeg type = 'biserial' voor de biseriale correlatie.")
    }
  }

  # --- Type-specifieke validatie ---

  if (type == "biserial") {
    # Precies een variabele moet binair zijn
    if (x_binary && y_binary) {
      stop("Beide variabelen zijn binair. ",
           "Gebruik type = 'tetrachoric' in plaats van 'biserial'.")
    }
    if (!x_binary && !y_binary) {
      stop("Voor de biseriale correlatie moet precies een variabele binair ",
           "zijn (precies 2 unieke waarden). ",
           "x heeft ", x_n_unique, " unieke waarden, ",
           "y heeft ", y_n_unique, " unieke waarden.")
    }
    # Auto-swap: y moet de binaire variabele zijn
    if (x_binary && !y_binary) {
      message("Opmerking: x en y zijn omgewisseld zodat de binaire ",
              "variabele in y staat (conventie voor biseriale correlatie).")
      tmp <- x; x <- y; y <- tmp
    }
    # Hercodeer y naar 0/1 als dat nog niet zo is
    y_vals <- sort(unique(y))
    y <- as.integer(y == y_vals[2])
  }

  if (type == "polyserial") {
    # Een variabele moet "ordinaal" (gehele waarden) zijn,
    # de andere "continu" (niet alle waarden geheel, of veel unieke waarden)
    #
    # KEUZE: we definieren "continu" pragmatisch als: niet-integer OF
    # meer dan 10 unieke waarden. Dit is een heuristiek.
    x_appears_ordinal <- x_integer_valued && x_n_unique <= 10
    y_appears_ordinal <- y_integer_valued && y_n_unique <= 10

    if (x_appears_ordinal && y_appears_ordinal) {
      # Beide zien er ordinaal uit — dan is polychoric beter
      warning("Beide variabelen lijken ordinaal. ",
              "Overweeg type = 'polychoric'. ",
              "We gaan door met polyserial (y als ordinaal).")
    }
    if (!x_appears_ordinal && !y_appears_ordinal) {
      stop("Voor de polyseriale correlatie moet een variabele ordinaal ",
           "zijn (gehele getallen, max ~10 unieke waarden). ",
           "Geen van beide variabelen lijkt ordinaal te zijn.")
    }
    # Auto-swap: y moet de ordinale variabele zijn
    if (x_appears_ordinal && !y_appears_ordinal) {
      message("Opmerking: x en y zijn omgewisseld zodat de ordinale ",
              "variabele in y staat (conventie voor polyseriale correlatie).")
      tmp <- x; x <- y; y <- tmp
    }
  }

  if (type == "polychoric") {
    # Beide variabelen moeten ordinaal zijn (gehele waarden)
    if (!x_integer_valued) {
      stop("Voor de polychorische correlatie moeten beide variabelen ",
           "ordinaal zijn (gehele getallen). ",
           "'x' bevat niet-gehele waarden.")
    }
    if (!y_integer_valued) {
      stop("Voor de polychorische correlatie moeten beide variabelen ",
           "ordinaal zijn (gehele getallen). ",
           "'y' bevat niet-gehele waarden.")
    }
  }

  if (type == "tetrachoric") {
    # Beide variabelen moeten binair zijn
    if (!x_binary) {
      stop("Voor de tetrachorische correlatie moeten beide variabelen ",
           "binair zijn (precies 2 unieke waarden). ",
           "'x' heeft ", x_n_unique, " unieke waarden.")
    }
    if (!y_binary) {
      stop("Voor de tetrachorische correlatie moeten beide variabelen ",
           "binair zijn (precies 2 unieke waarden). ",
           "'y' heeft ", y_n_unique, " unieke waarden.")
    }
  }

  # --- Controleer op constante variabelen ---
  # Bij pearson/kendall/spearman: sd=0 geeft problemen
  if (stats::sd(x) == 0) {
    stop("'x' is constant (alle waarden zijn gelijk). ",
         "Een correlatie is niet gedefinieerd.")
  }
  if (stats::sd(y) == 0) {
    stop("'y' is constant (alle waarden zijn gelijk). ",
         "Een correlatie is niet gedefinieerd.")
  }

  list(x = x, y = y)
}




# =============================================================================
# PRINT METHODE
# =============================================================================

#' Print een bootcamp_correlate object
#'
#' Geeft een nette samenvatting van het correlatieresultaat weer in de console.
#'
#' @param x een object van klasse \code{"bootcamp_correlate"},
#'   zoals geretourneerd door \code{\link{correlate}}
#' @param digits geheel getal, het aantal decimalen voor de uitvoer.
#'   Standaard is 4.
#' @param ... extra argumenten (worden genegeerd)
#'
#' @return \code{x}, onzichtbaar (invisible). Dit is de standaardconventie
#'   voor print-methoden in R.
#'
#' @export
print.bootcamp_correlate <- function(x, digits = 4, ...) {

  cat("\n")
  cat("  ", x$method, "\n")
  cat("  ", strrep("-", nchar(x$method)), "\n")
  cat("  Estimate: ", formatC(x$estimate, digits = digits, format = "f"), "\n")
  cat("  n:        ", x$n, "\n")
  cat("  Type:     ", x$type, "\n")

  # Toon p-waarde en statistiek alleen als ze beschikbaar zijn
  # (d.w.z. niet NA). Dit is het geval voor pearson/kendall/spearman.
  if (!is.na(x$statistic)) {
    cat("  Statistic:", formatC(x$statistic, digits = digits, format = "f"), "\n")
  }
  if (!is.na(x$p.value)) {
    # Gebruik wetenschappelijke notatie voor hele kleine p-waarden
    if (x$p.value < 0.001) {
      cat("  p-value:  ", formatC(x$p.value, format = "e", digits = 3), "\n")
    } else {
      cat("  p-value:  ", formatC(x$p.value, digits = digits, format = "f"), "\n")
    }
  }

  cat("\n")
  invisible(x)
}
