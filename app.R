# =============================================================================
#  Simoo — Dairy Herd Simulation  v5  (formerly DairyHerdSim)
#  ANG107 Genetics and Breeding Lab  |  UC Davis
#
#  New in v5:
#   • Simulation terminates at Year 9 and auto-navigates to a Final Report
#   • Final Report: congratulations banner, Year 1 vs Final Year comparison
#     for all 4 traits, inbreeding & lethal allele trends, True BV reveal,
#     and downloadable Final Report workbook (.xlsx)
#   • App renamed "Simoo"
#
#  New in v4:
#  ANG107 Genetics and Breeding Lab  |  UC Davis
#
#  New in v4:
#   • 10 AI bulls, organised in 3 reliability tiers:
#       High (proven, 150-300 daughters), Moderate (40-80), Low/young (5-30)
#   • Per-trait reliability shown for all 4 traits in bull catalogue,
#     herd report, and heifer selection tables
#   • Birth year (and age) shown for every bull in the catalogue
#   • Heifer selection now shows Dam ID alongside Sire ID
#   • All Excel exports include the new reliability and birth-year columns
#
#  New in v3:
#   • Configurable start year and number of years
#   • Manual bull selection (pick specific bulls from catalogue)
#   • Manual cow culling (select individual cows to remove)
#   • Manual heifer admission (choose which replacement heifers enter)
#   • Auto-strategy mode still available as an alternative
#   • Home-raised bulls (select male calves to enter catalogue next year)
#   • Excel downloads for herd, bulls, and full analysis workbook
#   • All four trait phenotypes shown in herd report
#   • Herd-born females use prefix "HC", males use "HB" (founders use "C")
#
#  Deploy:  shiny::runApp("DairyHerdSim_ShinyApp/")
#           or upload folder to shinyapps.io
#
#  Packages:
#  install.packages(c("shiny","shinydashboard","AlphaSimR",
#                     "ggplot2","dplyr","tidyr","DT","openxlsx"))
# =============================================================================

library(shiny)
library(shinydashboard)
library(AlphaSimR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(openxlsx)


# ─── BIOLOGICAL PARAMETERS ───────────────────────────────────────────────────
#
#  Traits: milk (lbs/lactation), fat (lbs/lactation), protein (lbs/lactation),
#          methane (g/day)
#  Note: lower methane PTA is better for the environment; the merit weight
#  for methane is NEGATIVE so animals with lower CH4 emission score higher.

TRAIT_NAMES   <- c("milk", "fat", "protein", "methane")
N_TRAITS      <- 4
pop_mean      <- c(milk=20000, fat=800,  protein=700,  methane=450)
h2            <- c(milk=0.20,  fat=0.20, protein=0.20, methane=0.30)
pheno_sd      <- c(milk=3200,  fat=120,  protein=80,   methane=15)
repeatability <- c(milk=0.50,  fat=0.55, protein=0.55, methane=0.40)
Vp            <- pheno_sd^2
Vg            <- h2 * Vp;   names(Vg)  <- TRAIT_NAMES
Vpe           <- (repeatability - h2) * Vp;  names(Vpe) <- TRAIT_NAMES
Vte           <- (1 - repeatability)  * Vp;  names(Vte) <- TRAIT_NAMES
gen_cor <- matrix(c(
   1.00, 0.40, 0.83, 0.28,
   0.40, 1.00, 0.60, 0.62,
   0.83, 0.60, 1.00, 0.40,
   0.28, 0.62, 0.40, 1.00),
  nrow=4, byrow=TRUE, dimnames=list(TRAIT_NAMES, TRAIT_NAMES))
INBR_DEPR     <- c(milk=-200, fat=0,    protein=0,    methane=-0.5)
# Methane weight is NEGATIVE: lower CH4 emission → higher merit
MERIT_WEIGHTS <- c(milk=0.05, fat=0.50, protein=0.35, methane=-0.10)
LETHAL_FREQ   <- 0.02
# Methane is measured only in specialized farms (breath-test / SF6 equipment).
# This fraction (15 %) scales down the effective number of records for methane
# so that its reliability is appropriately lower than milk/fat/protein, even
# though methane h² (0.30) is higher.  Students can see the trade-off between
# heritability and data availability when comparing trait reliabilities.
METH_REC_FRAC <- 0.15
MAX_COW_AGE   <- 8
N_AI_BULLS    <- 10
# ── Annual AI semen budget ────────────────────────────────────────────────────
# Each year the student receives BUDGET_PER_COW × herd_size dollars to spend
# on AI semen.  Three simple tiers reflect real-world market logic:
#
#   ≥150 daughters (PROVEN / high rel)    → $25 / dose
#   40–149 daughters (SAMPLED / mod rel)  → $15 / dose
#   <40  daughters (YOUNG / low rel)      → $ 8 / dose
#   Home-raised bulls                     → $ 2 / dose  (handling only)
#
# Pedagogical tension: with $20/cow budget and 100 cows = $2,000/yr
#   All proven  ($25): $2,500 → 25% over budget (must mix or cut bulls)
#   Mix proven + sampled → ~$20 avg → right on budget
#   All young   ($ 8): $800  → lots of room, but low reliability
BUDGET_PER_COW <- 20   # $ per cow per year

compute_dose_price <- function(n_dau, source = "AI") {
  # Compute tier price from daughters first (preserves vector length of n_dau)
  price <- ifelse(n_dau >= 150, 25, ifelse(n_dau >= 40, 15, 8))
  # Then override home-raised bulls with $2 (source may be scalar or vector)
  ifelse(source == "Herd", 2, price)
}

# Total AI cost given selected bulls and number of cows actually bred
compute_ai_cost <- function(sel_bull_df, n_bred) {
  if (is.null(sel_bull_df) || nrow(sel_bull_df) == 0 || n_bred == 0) return(0)
  doses_per_bull <- n_bred / nrow(sel_bull_df)
  sum(sel_bull_df$dose_price * doses_per_bull, na.rm = TRUE)
}

fmtd <- function(x) paste0("$", formatC(round(x, 0), format="f", digits=0, big.mark=","))


# ─── HELPER FUNCTIONS ────────────────────────────────────────────────────────

# ── Kinship matrix helpers ───────────────────────────────────────────────────
#
# We maintain a symmetric kinship matrix φ(i,j) = probability that a random
# allele drawn from i and a random allele drawn from j are IBD.
#
# Key identities:
#   φ(i,i) = 0.5*(1 + F_i)           (self-kinship; 0.5 when F=0)
#   F_offspring = φ(sire, dam)         (inbreeding = kinship between parents)
#   φ(o,k)      = 0.5*(φ(s,k)+φ(d,k)) (kinship of offspring with any animal k)
#
# Founders (no known parents in the simulation) start with F=0, φ(i,i)=0.5,
# and φ(i,j)=0 for all pairs (assumed unrelated).

# Create an empty kinship matrix for a set of founder IDs
make_founder_kinship <- function(ids) {
  n <- length(ids)
  m <- matrix(0, nrow=n, ncol=n, dimnames=list(ids, ids))
  diag(m) <- 0.5
  m
}

# Expand an existing kinship matrix with one new individual.
# Returns list(mat = expanded matrix, F = inbreeding of new individual).
# If sire_id or dam_id is missing/unknown, the new individual is treated
# as an unrelated founder (F=0).
expand_kinship <- function(kin_mat, new_id, sire_id=NA_character_, dam_id=NA_character_) {
  n       <- nrow(kin_mat)
  all_ids <- rownames(kin_mat)
  has_ped <- !is.na(sire_id) && !is.na(dam_id) &&
             sire_id %in% all_ids && dam_id %in% all_ids
  if (has_ped) {
    F_new      <- kin_mat[sire_id, dam_id]         # φ(sire,dam) = F(offspring)
    kin_others <- 0.5*(kin_mat[sire_id,]+kin_mat[dam_id,])  # φ(o,k) for all k
  } else {
    F_new      <- 0
    kin_others <- rep(0, n)
  }
  kin_self <- 0.5*(1 + F_new)
  new_mat  <- matrix(0, nrow=n+1, ncol=n+1,
                     dimnames=list(c(all_ids,new_id), c(all_ids,new_id)))
  new_mat[seq_len(n), seq_len(n)] <- kin_mat
  new_mat[seq_len(n),  n+1]       <- kin_others
  new_mat[n+1, seq_len(n)]        <- kin_others
  new_mat[n+1, n+1]               <- kin_self
  list(mat=new_mat, F=F_new)
}

assign_lethal <- function(n, freq=LETHAL_FREQ) {
  p <- 1-freq; q <- freq
  sample(c("NN","Nn","nn"), n, replace=TRUE, prob=c(p^2, 2*p*q, q^2))
}

inherit_lethal <- function(sg, dg) {
  ga <- function(g) if(g=="NN") c("N","N") else if(g=="Nn") c("N","n") else c("n","n")
  al <- sort(c(sample(ga(sg),1), sample(ga(dg),1)), decreasing=TRUE)
  paste0(al, collapse="")
}

sample_pe <- function(n) {
  m <- sapply(TRAIT_NAMES, function(tr) rnorm(n, 0, sqrt(Vpe[tr])))
  if(n==1) matrix(m, nrow=1, dimnames=list(NULL,TRAIT_NAMES)) else m
}

generate_record <- function(bv, pe, inbr=0) {
  te  <- sapply(TRAIT_NAMES, function(tr) rnorm(1, 0, sqrt(Vte[tr])))
  phe <- pop_mean + bv + pe + te + INBR_DEPR*(inbr*100)
  phe["milk"]    <- max(phe["milk"],    5000)
  phe["fat"]     <- max(phe["fat"],       10)
  phe["protein"] <- max(phe["protein"],   10)
  phe["methane"] <- max(phe["methane"],   50)
  phe
}

estimate_pta <- function(rec_mean, n_recs, true_bv, pa_rel=NULL) {
  # Effective record count from own records (methane scaled by recording fraction)
  eff_n            <- rep(n_recs, N_TRAITS)
  names(eff_n)     <- TRAIT_NAMES
  eff_n["methane"] <- eff_n["methane"] * METH_REC_FRAC

  # If Parent Average reliability is available, convert it to equivalent records
  # and add to own records:  n_PA_equiv = r²_PA·(1-h²) / [h²·(1-r²_PA)]
  # This ensures reliability never drops below the PA and grows smoothly.
  if (!is.null(pa_rel)) {
    pa_rel <- pmin(pa_rel[TRAIT_NAMES], 0.9999)   # align names, avoid /0
    n_pa_equiv <- pa_rel * (1 - h2) / (h2 * (1 - pa_rel))
    eff_n <- eff_n + n_pa_equiv
  }

  r2       <- (eff_n*h2) / (eff_n*h2 + (1-h2))
  true_pta <- true_bv / 2
  noise    <- sapply(seq_along(TRAIT_NAMES), function(i)
    rnorm(1, 0, sqrt(1-r2[i]) * sqrt(Vg[i]) / 2))
  names(noise) <- TRAIT_NAMES
  list(pta=r2*true_pta+noise, reliability=r2)
}

compute_merit <- function(pta_vec) sum((pta_vec/pheno_sd*100)*MERIT_WEIGHTS)

fmt0 <- function(x) formatC(round(x,0), format="f", digits=0)
fmt1 <- function(x) formatC(round(x,1), format="f", digits=1)
fmt3 <- function(x) formatC(round(x,3), format="f", digits=3)
fpct <- function(x) paste0(formatC(round(x*100,1), format="f", digits=1), "%")

# Back-date bull birth years so proven bulls are realistically older.
# High rel (≥150 daughters) → 6-10 yrs before ref_year
# Moderate  (40-79)         → 3-6  yrs before ref_year
# Low/young (<40)           → 0-2  yrs before ref_year
bull_birth_years <- function(ndau_vec, ref_year) {
  sapply(ndau_vec, function(n) {
    if      (n >= 150) ref_year - sample(6:10, 1)
    else if (n >=  40) ref_year - sample(3:6,  1)
    else               ref_year - sample(0:2,  1)
  })
}


# ─── INITIALIZE SIMULATION ───────────────────────────────────────────────────

initialize_sim <- function(seed=42, start_year=1, n_cows_target=100, n_years=5) {
  set.seed(seed)
  n_cows_target <- as.integer(n_cows_target)

  founderPop <- quickHaplo(nInd=500, nChr=10, segSites=500)
  SP         <- SimParam$new(founderPop)
  SP$addTraitA(nQtlPerChr=50, mean=pop_mean, var=Vg, corA=gen_cor)
  SP$setTrackPed(isTrackPed=TRUE)
  SP$setSexes("yes_sys")
  SP$setVarE(h2=h2)

  founders  <- newPop(founderPop, simParam=SP)
  all_fem   <- founders[founders@sex=="F"]
  all_mal   <- founders[founders@sex=="M"]
  n_heifers_init <- max(5L, round(n_cows_target * 0.25))
  n_need    <- n_cows_target + n_heifers_init
  herd_all  <- all_fem[sample(all_fem@nInd, min(n_need, all_fem@nInd))]
  herd_cows <- herd_all[seq_len(min(n_cows_target, herd_all@nInd))]
  init_heif <- if (herd_all@nInd > n_cows_target)
                 herd_all[(n_cows_target+1):herd_all@nInd]
               else NULL
  ai_bulls  <- all_mal[sample(all_mal@nInd, min(N_AI_BULLS, all_mal@nInd))]

  cow_bv  <- bv(herd_cows, simParam=SP); colnames(cow_bv)  <- TRAIT_NAMES
  bull_bv <- bv(ai_bulls,  simParam=SP); colnames(bull_bv) <- TRAIT_NAMES

  cow_ages <- sample(2:5, herd_cows@nInd, replace=TRUE)
  # Founder cows keep "C" prefix; herd-born females will use "HC"
  cow_ids  <- paste0("C", formatC(seq_len(herd_cows@nInd), width=3, flag="0"))
  bull_ids <- paste0("B", formatC(seq_len(ai_bulls@nInd),  width=3, flag="0"))

  cow_pe_mat  <- sample_pe(herd_cows@nInd)
  cow_records <- vector("list", herd_cows@nInd)
  for (i in seq_len(herd_cows@nInd)) {
    recs <- matrix(NA, nrow=cow_ages[i], ncol=N_TRAITS,
                   dimnames=list(NULL,TRAIT_NAMES))
    for (lac in seq_len(cow_ages[i]))
      recs[lac,] <- generate_record(cow_bv[i,], cow_pe_mat[i,], 0)
    cow_records[[i]] <- recs
  }

  cow_pta <- cow_rel <- matrix(NA, herd_cows@nInd, N_TRAITS,
                                dimnames=list(NULL,TRAIT_NAMES))
  for (i in seq_len(herd_cows@nInd)) {
    res <- estimate_pta(colMeans(cow_records[[i]]),
                        nrow(cow_records[[i]]), cow_bv[i,])
    cow_pta[i,] <- res$pta; cow_rel[i,] <- res$reliability
  }

  # ── Reliability tiers for the 20 AI founders ─────────────────────────────────
  # High (proven, many daughters): ~7 bulls  → 150-300 daughters
  # Moderate (young sires):        ~7 bulls  → 40-80  daughters
  # Low (genomic / PA only):       ~6 bulls  → 5-30   daughters
  n_high_f <- 4L; n_mod_f <- 3L; n_low_f <- 3L   # sums to N_AI_BULLS = 10
  founder_ndau <- c(sample(150:300, n_high_f, replace=TRUE),
                    sample(40:80,   n_mod_f,  replace=TRUE),
                    sample(5:30,    n_low_f,  replace=TRUE))
  founder_ndau <- founder_ndau[sample(N_AI_BULLS)]   # shuffle tier order

  bull_pta <- bull_rel <- matrix(NA, ai_bulls@nInd, N_TRAITS,
                                  dimnames=list(NULL,TRAIT_NAMES))
  for (j in seq_len(ai_bulls@nInd)) {
    res <- estimate_pta(rep(0,N_TRAITS), founder_ndau[j], bull_bv[j,])
    bull_pta[j,] <- res$pta; bull_rel[j,] <- res$reliability
  }

  # herd_df now stores last-lactation phenotype for ALL four traits
  herd_df <- data.frame(
    id=cow_ids, age=cow_ages,
    sire_id="Unknown", dam_id="Unknown",   # founders: parents not tracked
    lethal=assign_lethal(herd_cows@nInd), inbr=0,
    merit=apply(cow_pta,1,compute_merit),
    milk_pta=cow_pta[,"milk"],   fat_pta=cow_pta[,"fat"],
    prot_pta=cow_pta[,"protein"], meth_pta=cow_pta[,"methane"],
    milk_rel=cow_rel[,"milk"],   fat_rel=cow_rel[,"fat"],
    prot_rel=cow_rel[,"protein"], meth_rel=cow_rel[,"methane"],
    # PA reliability = 0 for founders (no known parents in this simulation)
    milk_pa_rel=0, fat_pa_rel=0, prot_pa_rel=0, meth_pa_rel=0,
    n_recs=cow_ages,
    last_milk=sapply(cow_records, function(r) r[nrow(r),"milk"]),
    last_fat =sapply(cow_records, function(r) r[nrow(r),"fat"]),
    last_prot=sapply(cow_records, function(r) r[nrow(r),"protein"]),
    last_meth=sapply(cow_records, function(r) r[nrow(r),"methane"]),
    stringsAsFactors=FALSE)

  bull_df <- data.frame(
    id=bull_ids, source="AI",
    sire_id="Unknown", dam_id="Unknown",   # founders: parents not tracked
    lethal=assign_lethal(ai_bulls@nInd), inbr=0,
    merit=apply(bull_pta,1,compute_merit),
    milk_pta=bull_pta[,"milk"],  fat_pta=bull_pta[,"fat"],
    prot_pta=bull_pta[,"protein"], meth_pta=bull_pta[,"methane"],
    milk_rel=bull_rel[,"milk"],  fat_rel=bull_rel[,"fat"],
    prot_rel=bull_rel[,"protein"], meth_rel=bull_rel[,"methane"],
    n_daughters=founder_ndau,
    birth_year=bull_birth_years(founder_ndau, start_year),
    dose_price=compute_dose_price(founder_ndau, "AI"),
    stringsAsFactors=FALSE)

  cow_true_bv  <- as.data.frame(cow_bv);  cow_true_bv$id  <- cow_ids
  bull_true_bv <- as.data.frame(bull_bv); bull_true_bv$id <- bull_ids

  summary_df <- data.frame(
    year=start_year, n_cows=nrow(herd_df),
    mean_milk_pta=mean(herd_df$milk_pta),
    mean_fat_pta =mean(herd_df$fat_pta),
    mean_prot_pta=mean(herd_df$prot_pta),
    mean_meth_pta=mean(herd_df$meth_pta),
    mean_merit=mean(herd_df$merit),
    mean_milk_phe=mean(herd_df$last_milk),
    mean_fat_phe =mean(herd_df$last_fat,  na.rm=TRUE),
    mean_prot_phe=mean(herd_df$last_prot, na.rm=TRUE),
    mean_meth_phe=mean(herd_df$last_meth, na.rm=TRUE),
    mean_inbr=0,
    mean_true_bv_milk=mean(cow_true_bv$milk),
    mean_true_bv_fat =mean(cow_true_bv$fat),
    mean_true_bv_prot=mean(cow_true_bv$protein),
    mean_true_bv_meth=mean(cow_true_bv$methane),
    carrier_freq=(sum(herd_df$lethal=="Nn")+2*sum(herd_df$lethal=="nn"))/
                 (2*nrow(herd_df)),
    lethal_calves=0, total_calves=0, annual_ai_cost=0)

  # ── Pre-populate heifer pool with founder heifers (available from gen 2) ──────
  if (!is.null(init_heif) && init_heif@nInd > 0) {
    nh        <- init_heif@nInd
    hf_bv     <- bv(init_heif, simParam=SP); colnames(hf_bv) <- TRAIT_NAMES
    hf_ids    <- paste0("FH", formatC(seq_len(nh), width=3, flag="0"))
    hf_lethal <- assign_lethal(nh)
    hf_pe     <- sample_pe(nh)
    hf_pta    <- hf_rel <- matrix(NA, nh, N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
    for (i in seq_len(nh)) {
      res <- estimate_pta(rep(0, N_TRAITS), 0, hf_bv[i,])
      hf_pta[i,] <- res$pta; hf_rel[i,] <- res$reliability
    }
    hf_df <- data.frame(
      id         = hf_ids,
      sire_id    = "Founder", dam_id = "Founder",
      lethal     = hf_lethal, inbr = 0,
      milk_pta   = hf_pta[,"milk"],   fat_pta  = hf_pta[,"fat"],
      prot_pta   = hf_pta[,"protein"], meth_pta = hf_pta[,"methane"],
      milk_rel   = hf_rel[,"milk"],   fat_rel  = hf_rel[,"fat"],
      prot_rel   = hf_rel[,"protein"], meth_rel = hf_rel[,"methane"],
      entry_year = start_year + 1L,   # available after first advance
      stringsAsFactors = FALSE)
    init_heifer_pool <- list(
      df  = hf_df,
      bvs = hf_bv,
      pes = hf_pe)
  } else {
    init_heifer_pool <- list(
      df  = data.frame(),
      bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)),
      pes = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)))
  }
  kinship_mat <- make_founder_kinship(
    c(cow_ids, bull_ids, if (!is.null(init_heif) && init_heif@nInd > 0)
        paste0("FH", formatC(seq_len(init_heif@nInd), width=3, flag="0")) else character(0)))

  list(
    year=start_year, start_year=start_year, n_years=n_years,
    herd_df=herd_df, bull_df=bull_df,
    cow_records=cow_records, cow_true_bv=cow_true_bv,
    bull_true_bv=bull_true_bv, summary_df=summary_df,
    kinship_mat=kinship_mat,
    next_cow_id=herd_cows@nInd+1, next_bull_id=ai_bulls@nInd+1,
    lethal_calves=0, total_calves=0,
    n_cows_target=n_cows_target, finished=FALSE,
    heifer_pool=init_heifer_pool,
    bull_pool=list(
      df  = data.frame(),
      bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
    )
  )
}


# ─── PHASE 1: CULL + BREED → returns interim with available heifers ──────────
#   cull_ids     : cow IDs to remove (character vector)
#   use_bull_ids : bull IDs to use for mating (character vector)
#
#  ID prefixes:
#   "C"  — founder cows (set at initialization)
#   "HC" — herd-born female calves (new females born during simulation)
#   "HB" — herd-born male calves selected to become home bulls

phase1_breed <- function(state, cull_ids, use_bull_ids,
                         mating_plan = NULL,   # named char: names=cow_id, values=bull_id
                         budget      = Inf) {  # total $ budget for AI semen
  herd_df      <- state$herd_df
  bull_df      <- state$bull_df
  cow_records  <- state$cow_records
  cow_true_bv  <- state$cow_true_bv
  lethal_calves <- state$lethal_calves
  total_calves  <- state$total_calves
  next_cow_id   <- state$next_cow_id
  # Backward compat: old saves may lack per-trait reliability columns
  for (tr_col in c("fat_rel","prot_rel","meth_rel")) {
    if (!tr_col %in% names(bull_df)) bull_df[[tr_col]] <- bull_df$milk_rel
    if (!tr_col %in% names(herd_df)) herd_df[[tr_col]] <- herd_df$milk_rel
  }
  # Kinship matrix — create fresh one if loading an old save that lacks it
  kinship_mat <- state$kinship_mat
  if (is.null(kinship_mat)) {
    founder_ids <- c(state$herd_df$id, state$bull_df$id)
    kinship_mat <- make_founder_kinship(founder_ids)
  }

  # Apply culls
  keep_idx    <- which(!herd_df$id %in% cull_ids)
  herd_df     <- herd_df[keep_idx, ]
  cow_records <- cow_records[keep_idx]
  cow_true_bv <- cow_true_bv[keep_idx, ]

  # Selected bulls in merit order
  sel_bulls <- bull_df %>%
    filter(id %in% use_bull_ids) %>%
    arrange(desc(merit))
  if (nrow(sel_bulls) == 0) stop("No valid bulls selected.")

  # ── Budget enforcement: limit how many cows can be bred ─────────────────────
  # Cost per mating = average dose price across selected bulls.
  # If total cost > budget, only the top-merit cows get a mating slot.
  if (!"dose_price" %in% names(sel_bulls))
    sel_bulls$dose_price <- compute_dose_price(sel_bulls$n_daughters,
                              if ("source" %in% names(sel_bulls)) sel_bulls$source else "AI")
  avg_dose_price <- mean(sel_bulls$dose_price, na.rm=TRUE)
  n_total        <- nrow(herd_df)
  n_can_breed    <- if (is.finite(budget) && avg_dose_price > 0)
                      min(n_total, floor(budget / avg_dose_price))
                    else n_total

  # Sort herd by merit descending; top n_can_breed cows get bred, rest open
  merit_order     <- order(herd_df$merit, decreasing=TRUE)
  breed_rows      <- merit_order[seq_len(n_can_breed)]
  open_rows       <- if (n_can_breed < n_total) merit_order[(n_can_breed+1):n_total] else integer(0)

  herd_breed      <- herd_df[breed_rows, , drop=FALSE]
  cow_rec_breed   <- cow_records[breed_rows]
  cow_bv_breed    <- cow_true_bv[breed_rows, , drop=FALSE]

  # ── Build bull assignment per bred cow ───────────────────────────────────────
  # specific mode: use mating_plan lookup; fallback to round-robin for unassigned
  if (!is.null(mating_plan) && length(mating_plan) > 0) {
    bull_assign <- vapply(herd_breed$id, function(cid) {
      bid <- mating_plan[[cid]]
      if (!is.null(bid) && bid %in% sel_bulls$id) bid
      else sel_bulls$id[1]   # fallback: best bull
    }, character(1))
  } else {
    bull_assign <- rep(sel_bulls$id, length.out=nrow(herd_breed))
  }

  # Generate calves — BV estimated as Parent Average (PA = (sire PTA + dam PTA)/2)
  # Females: wait 2 years in heifer_pool before milking herd
  # Males: wait 1 year in bull_pool before entering AI catalogue
  h_ids <- h_bvs <- h_inbr <- h_let <- h_pe <- h_pa_pta <- h_pa_rel <- h_sire <- h_dam <- NULL
  m_ids <- m_bvs <- m_inbr <- m_let <- m_pa_pta <- m_pa_rel <- m_sire <- m_dam <- NULL

  for (ci in seq_len(nrow(herd_breed))) {
    dam  <- herd_breed[ci, ]
    bull <- sel_bulls[sel_bulls$id == bull_assign[ci], ]
    if (nrow(bull) == 0) next

    dam_bv  <- as.numeric(cow_bv_breed[ci, TRAIT_NAMES])
    bull_bv <- as.numeric(bull[1, paste0(c("milk","fat","prot","meth"),"_pta")] * 2)
    calf_bv <- (dam_bv + bull_bv)/2 + rnorm(N_TRAITS, 0, sqrt(0.5*Vg))

    calf_let <- inherit_lethal(bull$lethal[1], dam$lethal)
    total_calves <- total_calves + 1
    if (calf_let=="nn") { lethal_calves <- lethal_calves+1; next }

    is_female  <- runif(1) >= 0.5

    # True inbreeding: F(calf) = kinship(sire, dam) from the pedigree NRM.
    # Assign calf ID now so we can add it to the kinship matrix regardless of sex.
    calf_tmp_id <- paste0("TMP", next_cow_id)   # placeholder; real ID set below
    exp_kin <- expand_kinship(kinship_mat,
                              new_id  = calf_tmp_id,
                              sire_id = bull$id[1],
                              dam_id  = dam$id)
    calf_inbr  <- exp_kin$F
    kinship_mat <- exp_kin$mat   # matrix now contains the calf (under tmp ID)

    # Parent Average PTA: best genetic estimate before any record
    sire_pta_v <- setNames(
      as.numeric(bull[1, c("milk_pta","fat_pta","prot_pta","meth_pta")]),
      TRAIT_NAMES)
    dam_pta_v  <- setNames(
      as.numeric(dam[,   c("milk_pta","fat_pta","prot_pta","meth_pta")]),
      TRAIT_NAMES)
    pa_pta_i   <- (sire_pta_v + dam_pta_v) / 2
    # PA reliability per trait = (sire rel + dam rel) / 4  [one-parent accuracy]
    pa_rel_i   <- c(
      milk    = (bull$milk_rel[1]  + dam$milk_rel)  / 4,
      fat     = (bull$fat_rel[1]   + dam$fat_rel)   / 4,
      protein = (bull$prot_rel[1]  + dam$prot_rel)  / 4,
      methane = (bull$meth_rel[1]  + dam$meth_rel)  / 4
    )

    if (is_female) {
      # "HC" prefix = Herd-born female Calf (distinguishable from founder "C" cows)
      cid <- paste0("HC", formatC(next_cow_id, width=3, flag="0"))
      next_cow_id <- next_cow_id + 1
      # Rename the TMP row/col in the kinship matrix to the real calf ID
      rownames(kinship_mat)[rownames(kinship_mat)==calf_tmp_id] <- cid
      colnames(kinship_mat)[colnames(kinship_mat)==calf_tmp_id] <- cid
      pe_i <- sample_pe(1)
      h_ids    <- c(h_ids, cid)
      h_bvs    <- rbind(h_bvs, calf_bv)
      h_inbr   <- c(h_inbr, calf_inbr)
      h_let    <- c(h_let,  calf_let)
      h_pe     <- rbind(h_pe, pe_i)
      h_pa_pta <- rbind(h_pa_pta, pa_pta_i)
      h_pa_rel <- rbind(h_pa_rel, pa_rel_i)   # matrix: rows=heifers, cols=traits
      h_sire   <- c(h_sire, bull$id[1])
      h_dam    <- c(h_dam,  dam$id)
    } else {
      # "HB" prefix = Herd-born male Bull candidate (enters catalogue Year N+1)
      mid <- paste0("HB", formatC(next_cow_id, width=3, flag="0"))
      next_cow_id <- next_cow_id + 1
      # Rename the TMP row/col in the kinship matrix to the real calf ID
      rownames(kinship_mat)[rownames(kinship_mat)==calf_tmp_id] <- mid
      colnames(kinship_mat)[colnames(kinship_mat)==calf_tmp_id] <- mid
      m_ids    <- c(m_ids, mid)
      m_bvs    <- rbind(m_bvs, calf_bv)
      m_inbr   <- c(m_inbr, calf_inbr)
      m_let    <- c(m_let,  calf_let)
      m_pa_pta <- rbind(m_pa_pta, pa_pta_i)
      m_pa_rel <- rbind(m_pa_rel, pa_rel_i)   # matrix: rows=males, cols=traits
      m_sire   <- c(m_sire, bull$id[1])
      m_dam    <- c(m_dam, dam$id)
    }
  }

  # Build heifer display dataframe (PA-based, no first-lactation record yet)
  n_h <- length(h_ids)
  if (n_h > 0) {
    colnames(h_bvs)    <- TRAIT_NAMES
    colnames(h_pe)     <- TRAIT_NAMES
    colnames(h_pa_pta) <- TRAIT_NAMES
    heifer_df <- data.frame(
      id=h_ids, lethal=h_let, inbr=round(h_inbr,4),
      merit=apply(h_pa_pta, 1, compute_merit),
      milk_pta=h_pa_pta[,"milk"], fat_pta=h_pa_pta[,"fat"],
      prot_pta=h_pa_pta[,"protein"], meth_pta=h_pa_pta[,"methane"],
      milk_rel =round(h_pa_rel[,"milk"],    3),
      fat_rel  =round(h_pa_rel[,"fat"],     3),
      prot_rel =round(h_pa_rel[,"protein"], 3),
      meth_rel =round(h_pa_rel[,"methane"], 3),
      sire=h_sire, dam=h_dam,
      stringsAsFactors=FALSE)
    htbv <- as.data.frame(h_bvs); htbv$id <- h_ids
  } else {
    heifer_df <- data.frame(); htbv <- data.frame()
  }

  # Build male calf display dataframe (PA-based)
  n_m <- length(m_ids)
  if (n_m > 0) {
    colnames(m_bvs)    <- TRAIT_NAMES
    colnames(m_pa_pta) <- TRAIT_NAMES
    male_df <- data.frame(
      id=m_ids, lethal=m_let, inbr=round(m_inbr,4),
      merit=apply(m_pa_pta, 1, compute_merit),
      milk_pta=m_pa_pta[,"milk"], fat_pta=m_pa_pta[,"fat"],
      prot_pta=m_pa_pta[,"protein"], meth_pta=m_pa_pta[,"methane"],
      milk_rel =round(m_pa_rel[,"milk"],    3),
      fat_rel  =round(m_pa_rel[,"fat"],     3),
      prot_rel =round(m_pa_rel[,"protein"], 3),
      meth_rel =round(m_pa_rel[,"methane"], 3),
      sire=m_sire, dam=m_dam,
      stringsAsFactors=FALSE)
  } else {
    male_df <- data.frame()
  }

  list(
    culled_herd_df     = herd_df,
    culled_cow_records = cow_records,
    culled_cow_true_bv = cow_true_bv,
    available_heifers  = heifer_df,
    heifer_bvs   = if(n_h>0) h_bvs    else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    heifer_inbr  = h_inbr,
    heifer_let   = h_let,
    heifer_pe    = if(n_h>0) h_pe     else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    heifer_pa_pta= if(n_h>0) h_pa_pta else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    heifer_pa_rel= if(n_h>0) h_pa_rel else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    heifer_ids   = h_ids,
    heifer_sire  = if(n_h>0) h_sire   else character(0),
    heifer_dam   = if(n_h>0) h_dam    else character(0),
    heifer_tbv   = htbv,
    available_males = male_df,
    male_bvs     = if(n_m>0) m_bvs    else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    male_inbr    = m_inbr,
    male_let     = m_let,
    male_pa_pta  = if(n_m>0) m_pa_pta else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    male_pa_rel  = if(n_m>0) m_pa_rel else matrix(nrow=0,ncol=N_TRAITS,dimnames=list(NULL,TRAIT_NAMES)),
    male_ids     = m_ids,
    lethal_calves = lethal_calves,
    total_calves  = total_calves,
    next_cow_id   = next_cow_id,
    kinship_mat   = kinship_mat,   # updated with all calves born this round
    n_bred        = nrow(herd_breed),
    n_open        = length(open_rows)   # cows left open due to budget constraint
  )
}


# ─── PHASE 2: POOL HEIFERS + ADMIT MATURE POOL + AGE COWS + ADVANCE YEAR ─────
#
#   admit_heifer_ids : female calf IDs to raise → heifer_pool (enter herd Year N+2)
#   raise_bull_ids   : male calf IDs to raise   → bull_pool   (enter catalogue Year N+1)
#
#   Heifers born Year N → 2-year rearing → milking herd Year N+2
#   Home bulls born Year N → 1-year rearing → AI catalogue Year N+1
#   Home bulls are labelled source="Herd" and are NOT subject to the N_AI_BULLS cap

phase2_finalize <- function(state, interim, admit_heifer_ids,
                             select_heifer_ids = NULL,   # IDs from pool to admit now; NULL = all ready
                             raise_bull_ids = character(0), end_year, annual_ai_cost=0) {
  herd_df      <- interim$culled_herd_df
  cow_records  <- interim$culled_cow_records
  cow_true_bv  <- interim$culled_cow_true_bv
  bull_df      <- state$bull_df
  next_bull_id <- state$next_bull_id
  new_yr       <- state$year + 1
  # Kinship matrix passed forward from phase1 (includes all calves born this year)
  kinship_mat  <- interim$kinship_mat
  if (is.null(kinship_mat)) {
    all_ids <- c(herd_df$id, bull_df$id)
    kinship_mat <- make_founder_kinship(all_ids)
  }

  # ── Heifer pool: initialise if missing (supports old save files) ─────────────
  heifer_pool <- state$heifer_pool
  if (is.null(heifer_pool))
    heifer_pool <- list(
      df  = data.frame(),
      bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)),
      pes = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
    )

  # ── Bull pool: initialise if missing (supports old save files) ───────────────
  bull_pool <- state$bull_pool
  if (is.null(bull_pool))
    bull_pool <- list(
      df  = data.frame(),
      bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
    )

  # ── 1) ENQUEUE: add selected female calves (enter herd in 2 years) ───────────
  if (length(admit_heifer_ids) > 0) {
    h_idx <- which(interim$heifer_ids %in% admit_heifer_ids)
    if (length(h_idx) > 0) {
      # heifer_pa_rel may be a matrix (new format) or vector (old saves → compat)
      hp_rel <- interim$heifer_pa_rel
      if (!is.matrix(hp_rel)) {
        hp_rel <- matrix(rep(hp_rel, N_TRAITS), ncol=N_TRAITS,
                         dimnames=list(NULL, TRAIT_NAMES))
      }
      new_df <- data.frame(
        id         = interim$heifer_ids[h_idx],
        sire_id    = if(length(interim$heifer_sire)>=max(h_idx)) interim$heifer_sire[h_idx] else "Unknown",
        dam_id     = if(length(interim$heifer_dam) >=max(h_idx)) interim$heifer_dam [h_idx] else "Unknown",
        lethal     = interim$heifer_let[h_idx],
        inbr       = interim$heifer_inbr[h_idx],
        milk_pta   = interim$heifer_pa_pta[h_idx, "milk"],
        fat_pta    = interim$heifer_pa_pta[h_idx, "fat"],
        prot_pta   = interim$heifer_pa_pta[h_idx, "protein"],
        meth_pta   = interim$heifer_pa_pta[h_idx, "methane"],
        milk_rel   = hp_rel[h_idx, "milk"],
        fat_rel    = hp_rel[h_idx, "fat"],
        prot_rel   = hp_rel[h_idx, "protein"],
        meth_rel   = hp_rel[h_idx, "methane"],
        entry_year = state$year + 2,
        stringsAsFactors = FALSE)
      heifer_pool$df  <- bind_rows(heifer_pool$df, new_df)
      heifer_pool$bvs <- rbind(heifer_pool$bvs,
                               interim$heifer_bvs[h_idx,,drop=FALSE])
      heifer_pool$pes <- rbind(heifer_pool$pes,
                               interim$heifer_pe [h_idx,,drop=FALSE])
    }
  }

  # ── 1b) ENQUEUE male calves selected to raise as home bulls (enter Year N+1) ──
  if (length(raise_bull_ids) > 0) {
    m_idx <- which(interim$male_ids %in% raise_bull_ids)
    if (length(m_idx) > 0) {
      # male_pa_rel may be a matrix (new format) or vector (old saves → compat)
      mp_rel <- interim$male_pa_rel
      if (!is.matrix(mp_rel)) {
        mp_rel <- matrix(rep(mp_rel, N_TRAITS), ncol=N_TRAITS,
                         dimnames=list(NULL, TRAIT_NAMES))
      }
      bpool_df <- data.frame(
        id         = interim$male_ids[m_idx],
        lethal     = interim$male_let[m_idx],
        inbr       = interim$male_inbr[m_idx],
        milk_pta   = interim$male_pa_pta[m_idx, "milk"],
        fat_pta    = interim$male_pa_pta[m_idx, "fat"],
        prot_pta   = interim$male_pa_pta[m_idx, "protein"],
        meth_pta   = interim$male_pa_pta[m_idx, "methane"],
        milk_rel   = mp_rel[m_idx, "milk"],
        fat_rel    = mp_rel[m_idx, "fat"],
        prot_rel   = mp_rel[m_idx, "protein"],
        meth_rel   = mp_rel[m_idx, "methane"],
        sire       = interim$available_males$sire[match(interim$male_ids[m_idx], interim$male_ids)],
        dam        = interim$available_males$dam [match(interim$male_ids[m_idx], interim$male_ids)],
        birth_year = state$year,
        entry_year = state$year + 1,
        stringsAsFactors = FALSE)
      bull_pool$df  <- bind_rows(bull_pool$df, bpool_df)
      bull_pool$bvs <- rbind(bull_pool$bvs,
                             interim$male_bvs[m_idx,,drop=FALSE])
    }
  }

  # ── 1c) DEQUEUE bull pool: home bulls whose entry_year has arrived ────────────
  if (nrow(bull_pool$df) > 0) {
    b_enter_idx <- which(bull_pool$df$entry_year == new_yr)
    if (length(b_enter_idx) > 0) {
      bp_df  <- bull_pool$df[b_enter_idx, ]
      bp_bvs <- bull_pool$bvs[b_enter_idx,,drop=FALSE]
      enter_bull_df <- data.frame(
        id          = bp_df$id,
        source      = "Herd",
        sire_id     = if("sire" %in% names(bp_df)) bp_df$sire else "Unknown",
        dam_id      = if("dam"  %in% names(bp_df)) bp_df$dam  else "Unknown",
        lethal      = bp_df$lethal,
        inbr        = bp_df$inbr,
        merit       = 0,   # placeholder; recomputed below
        milk_pta    = bp_df$milk_pta,
        fat_pta     = bp_df$fat_pta,
        prot_pta    = bp_df$prot_pta,
        meth_pta    = bp_df$meth_pta,
        milk_rel    = bp_df$milk_rel,
        fat_rel     = if("fat_rel"  %in% names(bp_df)) bp_df$fat_rel  else bp_df$milk_rel,
        prot_rel    = if("prot_rel" %in% names(bp_df)) bp_df$prot_rel else bp_df$milk_rel,
        meth_rel    = if("meth_rel" %in% names(bp_df)) bp_df$meth_rel else bp_df$milk_rel,
        n_daughters = 0L,
        birth_year  = if("birth_year" %in% names(bp_df)) bp_df$birth_year else (new_yr - 1L),
        dose_price  = 2,   # home-raised bulls: handling cost only
        stringsAsFactors = FALSE)
      enter_bull_df$merit <- apply(
        enter_bull_df[, c("milk_pta","fat_pta","prot_pta","meth_pta")], 1,
        function(r) compute_merit(setNames(as.numeric(r), TRAIT_NAMES)))
      bull_df <- bind_rows(bull_df, enter_bull_df)
      # Remove from pool
      b_remaining <- setdiff(seq_len(nrow(bull_pool$df)), b_enter_idx)
      if (length(b_remaining) == 0) {
        bull_pool <- list(
          df  = data.frame(),
          bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
        )
      } else {
        bull_pool$df  <- bull_pool$df[b_remaining, ]
        bull_pool$bvs <- bull_pool$bvs[b_remaining,,drop=FALSE]
      }
    }
  }

  # ── 2) DEQUEUE: admit pool heifers whose entry_year has arrived ──────────────
  if (nrow(heifer_pool$df) > 0) {
    ready_idx <- which(heifer_pool$df$entry_year == new_yr)
    # Filter to student-selected heifers; if NULL admit all ready ones
    enter_idx <- if (!is.null(select_heifer_ids) && length(select_heifer_ids) > 0)
                   ready_idx[heifer_pool$df$id[ready_idx] %in% select_heifer_ids]
                 else ready_idx
    # Remove the ready-but-not-selected heifers from pool (they age out)
    drop_idx  <- setdiff(ready_idx, enter_idx)
    if (length(drop_idx) > 0) {
      keep_all <- setdiff(seq_len(nrow(heifer_pool$df)), drop_idx)
      if (length(keep_all) == 0) {
        heifer_pool <- list(
          df  = data.frame(),
          bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)),
          pes = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)))
      } else {
        heifer_pool$df  <- heifer_pool$df[keep_all,  ]
        heifer_pool$bvs <- heifer_pool$bvs[keep_all, , drop=FALSE]
        heifer_pool$pes <- heifer_pool$pes[keep_all, , drop=FALSE]
      }
      # Recompute enter_idx after shrinking the pool
      enter_idx <- which(heifer_pool$df$entry_year == new_yr)
    }
    if (length(enter_idx) > 0) {
      enter_df  <- heifer_pool$df[enter_idx, ]
      enter_bvs <- heifer_pool$bvs[enter_idx,,drop=FALSE]
      enter_pes <- heifer_pool$pes[enter_idx,,drop=FALSE]

      heifer_entries <- vector("list", length(enter_idx))
      pta_list       <- vector("list", length(enter_idx))
      for (ei in seq_along(enter_idx)) {
        bv_i  <- as.numeric(enter_bvs[ei, TRAIT_NAMES])
        pe_i  <- as.numeric(enter_pes[ei, TRAIT_NAMES])
        rec_i <- matrix(generate_record(bv_i, pe_i, enter_df$inbr[ei]),
                        nrow=1, dimnames=list(NULL, TRAIT_NAMES))
        heifer_entries[[ei]] <- rec_i
        # Pass the PA reliability so that first-record PTA starts from the
        # prior PA information rather than resetting to a raw single-record rel.
        pa_rel_ei <- c(milk    = enter_df$milk_rel[ei],
                       fat     = enter_df$fat_rel[ei],
                       protein = enter_df$prot_rel[ei],
                       methane = enter_df$meth_rel[ei])
        pta_list[[ei]] <- estimate_pta(rec_i[1,], 1, bv_i, pa_rel=pa_rel_ei)
      }

      # All four last-lactation traits stored from first record
      enter_herd_df <- data.frame(
        id       = enter_df$id,
        age      = 1L,
        sire_id  = if("sire_id" %in% names(enter_df)) enter_df$sire_id else "Unknown",
        dam_id   = if("dam_id"  %in% names(enter_df)) enter_df$dam_id  else "Unknown",
        lethal   = enter_df$lethal,
        inbr     = enter_df$inbr,
        merit    = sapply(pta_list, function(x) compute_merit(x$pta)),
        milk_pta = sapply(pta_list, function(x) x$pta["milk"]),
        fat_pta  = sapply(pta_list, function(x) x$pta["fat"]),
        prot_pta = sapply(pta_list, function(x) x$pta["protein"]),
        meth_pta = sapply(pta_list, function(x) x$pta["methane"]),
        milk_rel = sapply(pta_list, function(x) x$reliability["milk"]),
        fat_rel  = sapply(pta_list, function(x) x$reliability["fat"]),
        prot_rel = sapply(pta_list, function(x) x$reliability["protein"]),
        meth_rel = sapply(pta_list, function(x) x$reliability["methane"]),
        # Store PA reliability permanently so aging loop can keep combining it
        milk_pa_rel = enter_df$milk_rel,
        fat_pa_rel  = enter_df$fat_rel,
        prot_pa_rel = enter_df$prot_rel,
        meth_pa_rel = enter_df$meth_rel,
        n_recs   = 1L,
        last_milk= sapply(heifer_entries, function(r) r[1,"milk"]),
        last_fat = sapply(heifer_entries, function(r) r[1,"fat"]),
        last_prot= sapply(heifer_entries, function(r) r[1,"protein"]),
        last_meth= sapply(heifer_entries, function(r) r[1,"methane"]),
        stringsAsFactors = FALSE)

      enter_tbv <- as.data.frame(enter_bvs)
      colnames(enter_tbv) <- TRAIT_NAMES
      enter_tbv$id <- enter_df$id

      herd_df     <- bind_rows(herd_df, enter_herd_df)
      cow_records <- c(cow_records, heifer_entries)
      cow_true_bv <- bind_rows(cow_true_bv, enter_tbv)

      remaining <- setdiff(seq_len(nrow(heifer_pool$df)), enter_idx)
      if (length(remaining) == 0) {
        heifer_pool <- list(
          df  = data.frame(),
          bvs = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES)),
          pes = matrix(nrow=0, ncol=N_TRAITS, dimnames=list(NULL,TRAIT_NAMES))
        )
      } else {
        heifer_pool$df  <- heifer_pool$df[remaining, ]
        heifer_pool$bvs <- heifer_pool$bvs[remaining,,drop=FALSE]
        heifer_pool$pes <- heifer_pool$pes[remaining,,drop=FALSE]
      }
    }
  }

  # ── Backward-compat: add missing columns for saves without them ──────────────
  if (!"last_fat"  %in% names(herd_df)) herd_df$last_fat  <- NA_real_
  if (!"last_prot" %in% names(herd_df)) herd_df$last_prot <- NA_real_
  if (!"last_meth" %in% names(herd_df)) herd_df$last_meth <- NA_real_
  if (!"sire_id"   %in% names(herd_df)) herd_df$sire_id   <- "Unknown"
  if (!"dam_id"    %in% names(herd_df)) herd_df$dam_id    <- "Unknown"
  if (!"sire_id"   %in% names(bull_df)) bull_df$sire_id   <- "Unknown"
  if (!"dam_id"    %in% names(bull_df)) bull_df$dam_id    <- "Unknown"
  # Per-trait reliability (added in v4)
  for (tr_col in c("fat_rel","prot_rel","meth_rel")) {
    if (!tr_col %in% names(herd_df)) herd_df[[tr_col]] <- herd_df$milk_rel
    if (!tr_col %in% names(bull_df)) bull_df[[tr_col]] <- bull_df$milk_rel
  }
  if (!"birth_year" %in% names(bull_df)) bull_df$birth_year <- NA_integer_
  if (!"dose_price" %in% names(bull_df))
    bull_df$dose_price <- compute_dose_price(bull_df$n_daughters,
                                             if("source" %in% names(bull_df)) bull_df$source else "AI")
  # PA reliability columns (added in v4 — founders and old saves default to 0)
  for (pa_col in c("milk_pa_rel","fat_pa_rel","prot_pa_rel","meth_pa_rel"))
    if (!pa_col %in% names(herd_df)) herd_df[[pa_col]] <- 0

  # ── Age all milking cows + new lactation + update PTAs and last phenotypes ────
  herd_df$age    <- herd_df$age    + 1L
  herd_df$n_recs <- herd_df$n_recs + 1L

  for (i in seq_len(nrow(herd_df))) {
    bv_i  <- as.numeric(cow_true_bv[i, TRAIT_NAMES])
    pe_i  <- colMeans(cow_records[[i]]) - pop_mean - bv_i
    new_r <- generate_record(bv_i, pe_i, herd_df$inbr[i])
    cow_records[[i]] <- rbind(cow_records[[i]],
      matrix(new_r, nrow=1, dimnames=list(NULL,TRAIT_NAMES)))
    # Pass the PA reliability so it is always combined with own records
    pa_rel_i <- c(milk    = herd_df$milk_pa_rel[i],
                  fat     = herd_df$fat_pa_rel[i],
                  protein = herd_df$prot_pa_rel[i],
                  methane = herd_df$meth_pa_rel[i])
    res <- estimate_pta(colMeans(cow_records[[i]]),
                        nrow(cow_records[[i]]), bv_i, pa_rel=pa_rel_i)
    herd_df$milk_pta[i]  <- res$pta["milk"]
    herd_df$fat_pta[i]   <- res$pta["fat"]
    herd_df$prot_pta[i]  <- res$pta["protein"]
    herd_df$meth_pta[i]  <- res$pta["methane"]
    herd_df$milk_rel[i]  <- res$reliability["milk"]
    herd_df$fat_rel[i]   <- res$reliability["fat"]
    herd_df$prot_rel[i]  <- res$reliability["protein"]
    herd_df$meth_rel[i]  <- res$reliability["methane"]
    herd_df$merit[i]     <- compute_merit(res$pta)
    # Update all four last-lactation phenotypes
    last_row <- cow_records[[i]][nrow(cow_records[[i]]),]
    herd_df$last_milk[i] <- last_row["milk"]
    herd_df$last_fat[i]  <- last_row["fat"]
    herd_df$last_prot[i] <- last_row["protein"]
    herd_df$last_meth[i] <- last_row["methane"]
  }

  # ── New AI bulls enter catalogue: 3 per year, one per reliability tier ───────
  # High (150-300 daughters), Moderate (40-80), Low / young (5-30)
  new_bull_ndau <- c(sample(150:300,1), sample(40:80,1), sample(5:30,1))
  new_bull_ndau <- new_bull_ndau[sample(3)]   # shuffle tier order each year
  for (nb in seq_len(3)) {
    nb_bv <- sapply(TRAIT_NAMES, function(tr)
      rnorm(1, 0.3*sqrt(Vg[tr]), sqrt(Vg[tr])))
    res <- estimate_pta(rep(0,N_TRAITS), new_bull_ndau[nb], nb_bv)
    bid <- paste0("B", formatC(next_bull_id, width=3, flag="0"))
    next_bull_id <- next_bull_id + 1
    kinship_mat <- expand_kinship(kinship_mat, new_id=bid)$mat
    bull_df <- bind_rows(bull_df, data.frame(
      id=bid, source="AI", sire_id="Unknown", dam_id="Unknown",
      lethal=assign_lethal(1), inbr=0,
      merit=compute_merit(res$pta),
      milk_pta=res$pta["milk"],  fat_pta=res$pta["fat"],
      prot_pta=res$pta["protein"], meth_pta=res$pta["methane"],
      milk_rel=res$reliability["milk"], fat_rel=res$reliability["fat"],
      prot_rel=res$reliability["protein"], meth_rel=res$reliability["methane"],
      n_daughters=new_bull_ndau[nb],
      birth_year=bull_birth_years(new_bull_ndau[nb], new_yr),
      dose_price=compute_dose_price(new_bull_ndau[nb], "AI"),
      stringsAsFactors=FALSE))
  }
  # Cap only AI bulls to N_AI_BULLS; preserve ALL home-raised (Herd) bulls
  herd_bulls  <- bull_df %>% filter(source == "Herd")
  ai_bulls_df <- bull_df %>% filter(source == "AI") %>%
    arrange(desc(merit)) %>% slice_head(n=N_AI_BULLS)
  bull_df <- bind_rows(ai_bulls_df, herd_bulls)

  summary_df <- bind_rows(state$summary_df, data.frame(
    year=new_yr, n_cows=nrow(herd_df),
    mean_milk_pta=mean(herd_df$milk_pta),
    mean_fat_pta =mean(herd_df$fat_pta),
    mean_prot_pta=mean(herd_df$prot_pta),
    mean_meth_pta=mean(herd_df$meth_pta),
    mean_merit=mean(herd_df$merit),
    mean_milk_phe=mean(herd_df$last_milk),
    mean_fat_phe =mean(herd_df$last_fat,  na.rm=TRUE),
    mean_prot_phe=mean(herd_df$last_prot, na.rm=TRUE),
    mean_meth_phe=mean(herd_df$last_meth, na.rm=TRUE),
    mean_inbr=mean(herd_df$inbr),
    mean_true_bv_milk=mean(cow_true_bv$milk),
    mean_true_bv_fat =mean(cow_true_bv$fat),
    mean_true_bv_prot=mean(cow_true_bv$protein),
    mean_true_bv_meth=mean(cow_true_bv$methane),
    carrier_freq=(sum(herd_df$lethal=="Nn")+2*sum(herd_df$lethal=="nn"))/
                 (2*nrow(herd_df)),
    lethal_calves=interim$lethal_calves, total_calves=interim$total_calves,
    annual_ai_cost=annual_ai_cost))

  list(
    year=new_yr, start_year=state$start_year, n_years=state$n_years,
    herd_df=herd_df, bull_df=bull_df,
    cow_records=cow_records, cow_true_bv=cow_true_bv,
    bull_true_bv=state$bull_true_bv, summary_df=summary_df,
    kinship_mat=kinship_mat,
    next_cow_id=interim$next_cow_id, next_bull_id=next_bull_id,
    lethal_calves=interim$lethal_calves, total_calves=interim$total_calves,
    n_cows_target=state$n_cows_target,
    heifer_pool=heifer_pool,
    bull_pool=bull_pool,
    finished=(new_yr >= end_year)
  )
}


# ─── AUTO-ADVANCE WRAPPER (uses strategy dropdowns) ──────────────────────────

advance_auto <- function(state, cow_strategy, bull_strategy, end_year) {
  herd_df  <- state$herd_df
  bull_df  <- state$bull_df
  n_target <- state$n_cows_target
  n_cull   <- max(0, nrow(herd_df) - n_target + round(n_target*0.25))
  mandatory <- herd_df$id[herd_df$age >= MAX_COW_AGE]

  cull_ids <- switch(cow_strategy,
    "Age then Merit" = {
      extra <- max(0, n_cull - length(mandatory))
      opt   <- herd_df %>% filter(!id %in% mandatory) %>%
                arrange(merit) %>% slice_head(n=extra) %>% pull(id)
      c(mandatory, opt)
    },
    "Age-based"   = herd_df %>% arrange(desc(age)) %>% slice_head(n=n_cull) %>% pull(id),
    "Merit-based" = herd_df %>% arrange(merit) %>% slice_head(n=n_cull) %>% pull(id),
    "Cull Carriers" = {
      carriers <- herd_df %>% filter(lethal=="Nn") %>% arrange(merit) %>% pull(id)
      non_c    <- herd_df %>% filter(lethal=="NN") %>% arrange(merit) %>% pull(id)
      head(c(mandatory, carriers, non_c), n_cull)
    }
  )
  use_bull_ids <- switch(bull_strategy,
    "Top 3 bulls"    = bull_df %>% arrange(desc(merit)) %>% slice_head(n=3) %>% pull(id),
    "Top 5 bulls"    = bull_df %>% arrange(desc(merit)) %>% slice_head(n=5) %>% pull(id),
    "All bulls"      = bull_df$id,
    "Avoid top bull" = bull_df %>% arrange(desc(merit)) %>% slice(-1) %>% pull(id)
  )
  budget_auto <- BUDGET_PER_COW * nrow(state$herd_df)
  interim  <- phase1_breed(state, cull_ids, use_bull_ids, budget=budget_auto)
  sel_bulls <- state$bull_df %>% filter(id %in% use_bull_ids)
  ai_cost   <- compute_ai_cost(sel_bulls, interim$n_bred)
  phase2_finalize(state, interim, interim$heifer_ids,
                  raise_bull_ids=character(0), end_year,
                  annual_ai_cost=ai_cost)
}


# =============================================================================
#  UI
# =============================================================================

ui <- dashboardPage(skin="blue",

  dashboardHeader(title=span("🐄 Simoo"), titleWidth=200),

  dashboardSidebar(
    width=200,
    sidebarMenu(id="tabs",
      menuItem("Setup",     tabName="setup",     icon=icon("sliders")),
      menuItem("My Herd",   tabName="herd",      icon=icon("cow")),
      menuItem("Decisions", tabName="decisions", icon=icon("list-check")),
      menuItem("Progress",  tabName="progress",  icon=icon("chart-line"))
    ),
    br(),
    div(style="padding:0 10px 0 10px;", uiOutput("sidebar_status_ui"))
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo,.skin-blue .main-header .navbar
        { background-color:#002855; }
      .skin-blue .main-sidebar { background-color:#1a1a2e; }
      /* ── Generation banner ─────────────────────────────────────── */
      .gen-banner {
        background:linear-gradient(90deg,#002855,#004080);
        color:white; border-radius:8px; padding:12px 18px;
        margin-bottom:16px; display:flex; align-items:center; gap:16px;
      }
      .gen-banner .gen-num { font-size:2.2em; font-weight:900;
                              color:#DAAA00; line-height:1; }
      .gen-banner .gen-sub { font-size:0.82em; color:#c0d4f0; margin:0; }
      /* ── Trait cards ──────────────────────────────────────────── */
      .trait-card {
        background:white; border-radius:8px; padding:14px 16px;
        box-shadow:0 2px 8px rgba(0,40,85,0.10); border-top:4px solid;
        margin-bottom:14px; min-height:130px;
      }
      .trait-card .tc-icon { font-size:1.6em; }
      .trait-card .tc-name { font-size:0.8em; font-weight:700; color:#002855;
                              text-transform:uppercase; letter-spacing:0.5px; }
      .trait-card .tc-val  { font-size:1.7em; font-weight:900; color:#002855;
                              line-height:1.1; margin:4px 0 2px; }
      .trait-card .tc-unit { font-size:0.7em; color:#888; }
      .trait-card .tc-desc { font-size:0.78em; color:#555;
                              margin-top:6px; line-height:1.35; }
      /* ── Advance button area ──────────────────────────────────── */
      .advance-area {
        background:linear-gradient(90deg,#1b5e20,#2e7d32);
        border-radius:8px; padding:16px 20px;
        display:flex; align-items:center; gap:16px; margin-top:12px;
      }
      .advance-area .adv-tip { font-size:0.82em; color:#c8e6c9; margin:0; }
      /* ── Save box ─────────────────────────────────────────────── */
      .save-box { background:#fffbf0; border:1px solid #e8d88a;
                  border-radius:9px; padding:14px; }
    "))),

    tabItems(

      # ── SETUP ────────────────────────────────────────────────────────────────
      tabItem(tabName="setup",
        fluidRow(column(12,
          div(style="background:linear-gradient(135deg,#002855,#004080);
                     color:white;border-radius:10px;padding:22px 28px;
                     margin-bottom:20px;display:flex;align-items:center;gap:20px;",
            div(style="font-size:3.5em;line-height:1;", "🐄"),
            div(
              h1(style="margin:0 0 4px;color:#DAAA00;font-size:2em;
                         font-weight:900;letter-spacing:2px;", "Simoo"),
              p(style="margin:0;font-size:0.95em;color:#c0d8f0;",
                "Dairy Herd Genetics Simulator · UC Davis · ANG 107")
            )
          )
        )),
        fluidRow(
          box(title=tagList(icon("sliders"), " Configure Simulation"),
              width=4, status="primary", solidHeader=TRUE,
            fluidRow(
              column(6, textInput("student_name", "Your Name",  value="")),
              column(6, textInput("herd_name",    "Herd Name",  value="My Herd"))
            ),
            fluidRow(
              column(6, numericInput("n_cows_target", "Herd Size",
                                    value=30, min=10, max=500, step=5)),
              column(6, numericInput("n_years", "Generations",
                                    value=5, min=1, max=20))
            ),
            fluidRow(
              column(6, numericInput("seed", "Random Seed",
                                    value=42, min=1, max=9999)),
              column(6, numericInput("start_year", "Start Year",
                                    value=1, min=1, max=100))
            ),
            hr(style="margin:10px 0;"),
            actionButton("init_btn", tagList(icon("play"), " Start Simulation"),
                         class="btn-success btn-lg", width="100%")
          ),
          box(title=tagList(icon("circle-info"), " Traits at a Glance"),
              width=5, status="info", solidHeader=TRUE,
            tags$table(style="width:100%;font-size:0.88em;border-collapse:collapse;",
              tags$thead(tags$tr(style="border-bottom:2px solid #dee2e6;",
                tags$th(style="padding:5px 6px;", "Trait"),
                tags$th(style="padding:5px 6px;", "Unit"),
                tags$th(style="padding:5px 6px;", "Goal"),
                tags$th(style="padding:5px 6px;", "What it measures")
              )),
              tags$tbody(
                tags$tr(style="border-bottom:1px solid #eee;",
                  tags$td(style="padding:6px;font-weight:700;color:#002855;", "🥛 Milk"),
                  tags$td(style="padding:6px;color:#666;", "lbs/day"),
                  tags$td(style="padding:6px;color:#166534;font-weight:700;", "↑ More"),
                  tags$td(style="padding:6px;color:#555;", "Daily milk production (305-d lactation)")),
                tags$tr(style="border-bottom:1px solid #eee;",
                  tags$td(style="padding:6px;font-weight:700;color:#92400e;", "🧈 Fat"),
                  tags$td(style="padding:6px;color:#666;", "lbs/day"),
                  tags$td(style="padding:6px;color:#166534;font-weight:700;", "↑ More"),
                  tags$td(style="padding:6px;color:#555;", "Daily fat yield — value for cheese")),
                tags$tr(style="border-bottom:1px solid #eee;",
                  tags$td(style="padding:6px;font-weight:700;color:#1565c0;", "🧪 Protein"),
                  tags$td(style="padding:6px;color:#666;", "lbs/day"),
                  tags$td(style="padding:6px;color:#166534;font-weight:700;", "↑ More"),
                  tags$td(style="padding:6px;color:#555;", "Daily protein yield — dairy product value")),
                tags$tr(
                  tags$td(style="padding:6px;font-weight:700;color:#b91c1c;", "🌿 Methane"),
                  tags$td(style="padding:6px;color:#666;", "g/day"),
                  tags$td(style="padding:6px;color:#b91c1c;font-weight:700;", "↓ Less"),
                  tags$td(style="padding:6px;color:#555;", "Enteric CH₄ — environmental footprint"))
              )
            ),
            br(),
            p(style="font-size:0.82em;color:#555;",
              strong("Bull pricing: "),
              span(style="color:#b91c1c;font-weight:700;", "$25"), " proven (≥150 daughters)  · ",
              span(style="color:#92400e;font-weight:700;", "$15"), " sampled (40–149)  · ",
              span(style="color:#166534;font-weight:700;", "$8"), " young (<40)  · ",
              span(style="color:#3730a3;font-weight:700;", "$2"), " home bull",
              br(),
              "Annual budget = $20 × herd size. Over-budget → lowest-merit cows go open."
            )
          ),
          column(3,
            uiOutput("setup_status_panel_ui"),
            br(),
            div(class="save-box",
              h5(tagList(icon("floppy-disk"), " Save & Load"),
                 style="margin-top:0;font-weight:700;color:#7a6000;"),
              p(style="font-size:0.83em;color:#666;margin-bottom:8px;",
                "Save your progress and continue in a future session."),
              downloadButton("save_btn", tagList(icon("download"), " Save Progress"),
                             class="btn-warning btn-sm", style="width:100%;margin-bottom:8px;"),
              fileInput("load_file", NULL, accept=".rds",
                        buttonLabel="Load Saved State", placeholder="No file chosen"),
              uiOutput("load_status")
            )
          )
        )
      ),

      # ── MY HERD ──────────────────────────────────────────────────────────────
      tabItem(tabName="herd",
        fluidRow(column(12, uiOutput("herd_gen_banner_ui"))),
        fluidRow(
          column(3, uiOutput("trait_card_milk_ui")),
          column(3, uiOutput("trait_card_fat_ui")),
          column(3, uiOutput("trait_card_prot_ui")),
          column(3, uiOutput("trait_card_meth_ui"))
        ),
        fluidRow(
          box(title=tagList(icon("table"), " Cow Roster"), width=12,
              status="primary", solidHeader=TRUE,
            p(style="font-size:0.82em;color:#555;margin-bottom:8px;",
              "C = founder · HC = herd-born female · Click any column header to sort.  ",
              span(style="background:#fde8e8;color:#b91c1c;font-weight:700;
                          padding:2px 6px;border-radius:3px;",
                   "Red rows: must cull (age ≥ 8)"),
              " — go to Decisions to remove them."
            ),
            DTOutput("herd_roster_tbl"),
            br(),
            div(style="display:flex;gap:8px;",
              downloadButton("download_herd_btn",
                             tagList(icon("file-excel"), " Download Herd (.xlsx)"),
                             class="btn-success btn-sm"),
              downloadButton("download_bulls_btn",
                             tagList(icon("file-excel"), " Download Bulls (.xlsx)"),
                             class="btn-warning btn-sm")
            )
          )
        )
      ),

      # ── DECISIONS ────────────────────────────────────────────────────────────
      tabItem(tabName="decisions",
        fluidRow(column(12, uiOutput("decisions_gen_banner_ui"))),
        fluidRow(
          box(title=tagList(icon("mars"), " Step 1 — Choose Bulls to Use"),
              width=7, status="warning", solidHeader=TRUE,
            p(style="font-size:0.85em;color:#555;margin-bottom:8px;",
              strong("Click rows to select"), " the bulls you want to use.",
              " Matings will be distributed equally across all selected bulls."
            ),
            div(style="display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;",
              span(style="background:#fde8e8;color:#b91c1c;font-weight:700;
                          padding:3px 9px;border-radius:12px;font-size:0.8em;",
                   "● $25 Proven ≥150 dau"),
              span(style="background:#fef9c3;color:#92400e;font-weight:700;
                          padding:3px 9px;border-radius:12px;font-size:0.8em;",
                   "● $15 Sampled 40–149"),
              span(style="background:#dcfce7;color:#166534;font-weight:700;
                          padding:3px 9px;border-radius:12px;font-size:0.8em;",
                   "● $8 Young <40")
            ),
            DTOutput("bull_sel_tbl"),
            br(),
            uiOutput("bull_sel_summary_ui")
          ),
          box(title=tagList(icon("trash"), " Step 2 — Choose Cows to Cull"),
              width=5, status="danger", solidHeader=TRUE,
            p(style="font-size:0.85em;color:#555;margin-bottom:8px;",
              strong("Click rows to select"), " cows to remove from the herd.",
              br(),
              span(style="background:#fde8e8;color:#b91c1c;font-weight:700;
                          padding:2px 5px;border-radius:3px;",
                   "Red = must cull (age ≥ 8)"),
              " — always removed even if not selected.",
              br(),
              "Also consider: lethal carriers (Nn), low gPTA, old cows."
            ),
            DTOutput("cull_sel_tbl"),
            br(),
            uiOutput("cull_sel_summary_ui")
          )
        ),
        fluidRow(column(12,
          box(title=tagList(icon("cow"), " Step 3 — Choose Heifers to Enter the Herd"),
              width=12, status="success", solidHeader=TRUE,
            uiOutput("heifer_pool_info_ui"),
            DTOutput("heifer_sel_tbl"),
            br(),
            uiOutput("heifer_sel_summary_ui")
          )
        )),
        fluidRow(column(12,
          div(class="advance-area",
            div(style="flex-shrink:0;",
              actionButton("advance_btn",
                           tagList(icon("chevron-right"), " Advance to Next Generation"),
                           class="btn-warning btn-lg",
                           style="font-weight:700;font-size:1.05em;")
            ),
            uiOutput("advance_tip_ui")
          )
        ))
      ),

      # ── PROGRESS ─────────────────────────────────────────────────────────────
      tabItem(tabName="progress",
        fluidRow(column(12,
          h3(tagList(icon("chart-line"), " Genetic Progress"),
             style="margin-bottom:16px;color:#002855;")
        )),
        fluidRow(
          box(title="Genetic Trend — Mean gPTA per Trait",
              width=12, status="primary", solidHeader=TRUE,
            p(style="font-size:0.82em;color:#555;margin-bottom:4px;",
              "Solid line = mean gPTA (estimated). Dashed = true mean BV (shown when simulation ends). ",
              "Each panel has its own Y-axis. Methane: lower is better."),
            plotOutput("genetic_trend_plot", height="380px")
          )
        ),
        fluidRow(
          box(title="Inbreeding Trend", width=4, status="danger", solidHeader=TRUE,
            plotOutput("inbr_plot", height="260px")
          ),
          box(title="Generation Summary", width=8, status="info", solidHeader=TRUE,
            DTOutput("progress_summary_tbl")
          )
        ),
        uiOutput("final_summary_ui")
      )

    )  # end tabItems
  )    # end dashboardBody
)      # end dashboardPage


# =============================================================================
#  SERVER
# =============================================================================

server <- function(input, output, session) {

  sim <- reactiveValues(state = NULL)
  dec <- reactiveValues(bull_df = NULL, cull_df = NULL, heifer_df = NULL)

  # ── helpers ──────────────────────────────────────────────────────────────────

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ── end_year ─────────────────────────────────────────────────────────────────

  end_year_r <- reactive({
    st <- sim$state
    if (!is.null(st) && !is.null(st$start_year) && !is.null(st$n_years))
      st$start_year + st$n_years
    else
      input$start_year + input$n_years
  })

  # ── Initialize simulation ────────────────────────────────────────────────────

  observeEvent(input$init_btn, {
    req(input$student_name != "", input$herd_name != "")
    prog2 <- Progress$new(session, min=0, max=1)
    prog2$set(message="Initializing herd…", value=0.5)
    on.exit(prog2$close(), add=TRUE)

    st <- tryCatch(
      initialize_sim(
        n_cows_target = as.integer(input$n_cows_target),
        n_years       = as.integer(input$n_years),
        start_year    = as.integer(input$start_year),
        seed          = as.integer(input$seed)
      ),
      error = function(e) {
        showNotification(paste("Init error:", e$message), type="error", duration=8)
        NULL
      }
    )
    prog2$set(value=1.0)
    if (is.null(st)) return()
    sim$state <- st
    showNotification("Herd initialized! Go to My Herd to see your animals.", type="message", duration=4)
    updateTabItems(session, "tabs", "herd")
  })

  # ── Save / Load ───────────────────────────────────────────────────────────────

  output$save_btn <- downloadHandler(
    filename = function() paste0("simoo_save_", Sys.Date(), ".rds"),
    content  = function(file) {
      saveRDS(list(
        state        = sim$state,
        student_name = isolate(input$student_name),
        herd_name    = isolate(input$herd_name)
      ), file)
    }
  )

  observeEvent(input$load_file, {
    req(input$load_file)
    saved <- tryCatch(readRDS(input$load_file$datapath), error=function(e) NULL)
    if (is.null(saved) || is.null(saved$state)) {
      output$load_status <- renderUI(div(class="text-danger", "Invalid save file."))
      return()
    }
    sim$state <- saved$state
    if (!is.null(saved$student_name)) updateTextInput(session, "student_name", value=saved$student_name)
    if (!is.null(saved$herd_name))    updateTextInput(session, "herd_name",    value=saved$herd_name)
    output$load_status <- renderUI(div(class="text-success", icon("check"), " Loaded!"))
    updateTabItems(session, "tabs", "herd")
  })

  # ── Sidebar status ────────────────────────────────────────────────────────────

  output$sidebar_status_ui <- renderUI({
    st <- sim$state
    if (is.null(st)) return(div(style="color:#aaa;font-size:0.8em;", "Not started"))
    gen <- st$year - st$start_year + 1
    tot <- st$n_years
    div(style="color:#DAAA00;font-size:0.85em;text-align:center;",
        icon("dna"), sprintf(" Gen %d of %d", gen, tot))
  })

  # ── Setup status panel ────────────────────────────────────────────────────────

  output$setup_status_panel_ui <- renderUI({
    st <- sim$state
    if (is.null(st)) {
      return(div(class="save-box",
        p(icon("info-circle"), " Fill in your name and herd, then click",
          strong(" Initialize Simulation"), " to begin.")))
    }
    gen <- st$year - st$start_year + 1
    n_cows <- nrow(st$herd_df)
    div(class="save-box", style="border-color:#002855;",
      p(icon("check-circle", style="color:green;"), strong(" Simulation active")),
      p(sprintf("Generation %d of %d  |  %d cows  |  Year %d",
                 gen, st$n_years, n_cows, st$year))
    )
  })

  # ── Generation banner helper ──────────────────────────────────────────────────

  gen_banner_html <- function(st, tab_label = "") {
    if (is.null(st)) return(NULL)
    gen <- st$year - st$start_year + 1
    tot <- st$n_years
    finished <- isTRUE(st$finished)
    label <- if (finished) "COMPLETE" else sprintf("Generation %d of %d", gen, tot)
    div(class="gen-banner",
      div(class="gen-num", label),
      if (!finished) div(class="gen-sub", sprintf("Year %d  ·  %d cows in herd", st$year, nrow(st$herd_df)))
    )
  }

  output$herd_gen_banner_ui      <- renderUI(gen_banner_html(sim$state, "herd"))
  output$decisions_gen_banner_ui <- renderUI(gen_banner_html(sim$state, "decisions"))

  # ── Trait cards (My Herd tab) ─────────────────────────────────────────────────

  LAC_DAYS <- 305  # standard lactation length for per-day conversion

  make_trait_card <- function(st, pta_col, phe_col, label, color,
                               unit, pop_avg_day, per_day, desc) {
    if (is.null(st)) return(NULL)
    herd <- st$herd_df
    # Actual production
    phe_vals <- herd[[phe_col]]
    scale    <- if (per_day) 1/LAC_DAYS else 1
    phe_mn   <- round(mean(phe_vals, na.rm=TRUE) * scale, 1)
    phe_sd   <- round(sd(phe_vals,   na.rm=TRUE) * scale, 1)
    # gPTA (genetic deviation)
    pta_vals <- herd[[pta_col]]
    pta_mn   <- round(mean(pta_vals, na.rm=TRUE) * scale, 2)
    pta_sign <- if (pta_mn >= 0) "+" else ""
    div(class="trait-card", style=sprintf("border-top-color:%s;", color),
      div(style="font-size:0.75em;font-weight:700;color:#666;text-transform:uppercase;
                 letter-spacing:0.5px;margin-bottom:2px;", label),
      div(style=sprintf("font-size:1.6em;font-weight:900;color:%s;line-height:1.1;", color),
          sprintf("%.1f %s", phe_mn, unit)),
      div(style="font-size:0.72em;color:#888;margin-top:2px;",
          sprintf("Avg production  |  SD: %.1f %s", phe_sd, unit)),
      tags$hr(style="margin:6px 0;border-color:#eee;"),
      div(style="font-size:0.8em;",
        span(style="color:#555;", "Mean gPTA: "),
        span(style=sprintf("font-weight:700;color:%s;", if(pta_mn >= 0) "#1b5e20" else "#b71c1c"),
             sprintf("%s%.2f %s", pta_sign, pta_mn, unit)),
        span(style="color:#999;font-size:0.88em;",
             sprintf("  (pop avg %.1f)", pop_avg_day))
      ),
      div(style="font-size:0.72em;color:#888;margin-top:4px;", desc)
    )
  }

  output$trait_card_milk_ui <- renderUI(make_trait_card(sim$state,
    "milk_pta", "last_milk", "🥛 Milk", "#1565c0",
    "lbs/day", 20000/LAC_DAYS, TRUE,
    "Higher gPTA = better genetics for daily milk"))
  output$trait_card_fat_ui  <- renderUI(make_trait_card(sim$state,
    "fat_pta",  "last_fat",  "🧈 Fat",  "#f9a825",
    "lbs/day", 800/LAC_DAYS, TRUE,
    "Fat content valuable for cheese and butter"))
  output$trait_card_prot_ui <- renderUI(make_trait_card(sim$state,
    "prot_pta", "last_prot", "🧪 Protein", "#2e7d32",
    "lbs/day", 700/LAC_DAYS, TRUE,
    "Protein drives fluid milk and product value"))
  output$trait_card_meth_ui <- renderUI(make_trait_card(sim$state,
    "meth_pta", "last_meth", "🌿 Methane", "#6a1b9a",
    "g/day", 450, FALSE,
    "Lower gPTA = less methane = better for environment"))

  # ── Herd roster table ─────────────────────────────────────────────────────────

  output$herd_roster_tbl <- renderDT({
    st <- sim$state
    req(st)
    herd <- st$herd_df
    df <- data.frame(
      ID              = herd$id,
      Age             = herd$age,
      `Milk (lb/d)`   = round(herd$last_milk / LAC_DAYS, 1),
      `Fat (lb/d)`    = round(herd$last_fat  / LAC_DAYS, 2),
      `Prot (lb/d)`   = round(herd$last_prot / LAC_DAYS, 2),
      `Meth (g/d)`    = round(herd$last_meth, 1),
      `Milk gPTA`     = round(herd$milk_pta  / LAC_DAYS, 2),
      `Fat gPTA`      = round(herd$fat_pta   / LAC_DAYS, 3),
      `Prot gPTA`     = round(herd$prot_pta  / LAC_DAYS, 3),
      `Meth gPTA`     = round(herd$meth_pta,  1),
      Merit           = round(herd$merit, 2),
      Lethal       = ifelse(is.na(herd$lethal) | herd$lethal == "NN", "No", herd$lethal),
      `Must Cull`  = ifelse(herd$age >= MAX_COW_AGE | (!is.na(herd$lethal) & herd$lethal == "nn"), "YES", ""),
      check.names  = FALSE,
      stringsAsFactors = FALSE
    )
    datatable(
      df, rownames=FALSE, selection="none",
      options=list(pageLength=15, dom="ftp", order=list(list(1,"desc"))),
      class="compact stripe"
    ) |>
      formatStyle(
        "Must Cull",
        backgroundColor = styleEqual("YES", "#ffebee"),
        fontWeight = styleEqual("YES", "bold"),
        color = styleEqual("YES", "#c62828")
      ) |>
      formatStyle(
        "Lethal",
        color = styleEqual("YES", "#e65100"),
        fontWeight = styleEqual("YES", "bold")
      )
  }, server=FALSE)

  # ── Herd download ─────────────────────────────────────────────────────────────

  output$download_herd_btn <- downloadHandler(
    filename = function() sprintf("herd_gen%d.xlsx", sim$state$year - sim$state$start_year + 1),
    content  = function(file) {
      st <- isolate(sim$state)
      req(st)
      openxlsx::write.xlsx(st$herd_df, file)
    }
  )

  output$download_bulls_btn <- downloadHandler(
    filename = function() sprintf("bulls_gen%d.xlsx", sim$state$year - sim$state$start_year + 1),
    content  = function(file) {
      st <- isolate(sim$state)
      req(st)
      openxlsx::write.xlsx(st$bull_df, file)
    }
  )

  # ── Decisions: populate dec tables when tab opens ─────────────────────────────

  observeEvent(input$tabs, {
    if (input$tabs != "decisions") return()
    st <- sim$state
    if (is.null(st) || isTRUE(st$finished)) return()

    # Bull catalogue for this generation
    bd <- st$bull_df
    bd$dose_price <- compute_dose_price(bd$n_daughters, bd$source)
    dec$bull_df <- bd

    # Cow table for culling decisions
    herd <- st$herd_df
    must <- herd$age >= MAX_COW_AGE | (!is.na(herd$lethal) & herd$lethal == "nn")
    herd$must_cull <- must
    dec$cull_df <- herd

    # Heifers ready to enter (entry_year == year + 1, i.e. born 2 years ago)
    hp <- st$heifer_pool$df
    if (!is.null(hp) && nrow(hp) > 0) {
      ready <- hp[hp$entry_year == (st$year + 1), ]
      dec$heifer_df <- if (nrow(ready) > 0) ready else data.frame()
    } else {
      dec$heifer_df <- data.frame()
    }
  })

  # ── Bull selection table ──────────────────────────────────────────────────────

  output$bull_sel_tbl <- renderDT({
    bd <- dec$bull_df
    req(bd)
    df <- data.frame(
      ID          = bd$id,
      Merit       = round(bd$merit,          1),
      `Milk gPTA` = round(bd$milk_pta,       1),
      `Fat gPTA`  = round(bd$fat_pta,        1),
      `Prot gPTA` = round(bd$prot_pta,       1),
      `Meth gPTA` = round(bd$meth_pta,       1),
      `Rel %`     = round(bd$milk_rel * 100, 0),
      Daughters   = bd$n_daughters,
      `Dose $`    = sprintf("$%d", round(bd$dose_price)),
      Source      = bd$source,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    datatable(
      df, rownames=FALSE, selection="multiple",
      options=list(pageLength=15, dom="t", order=list(list(1,"desc"))),
      class="compact stripe hover"
    ) |>
      formatStyle(
        "Rel %",
        background = styleColorBar(c(0,100), "#c8e6c9"),
        backgroundSize = "100% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle(
        "Source",
        color = styleEqual("Herd", "#1565c0"),
        fontWeight = styleEqual("Herd", "bold")
      )
  }, server=FALSE)

  output$bull_sel_summary_ui <- renderUI({
    bd <- dec$bull_df
    sel <- input$bull_sel_tbl_rows_selected
    st  <- sim$state
    req(bd, st)
    budget <- BUDGET_PER_COW * nrow(st$herd_df)
    if (length(sel) == 0) {
      return(div(style="color:#888;", "Select bulls from the table above (click rows)."))
    }
    chosen <- bd[sel, ]
    avg_p  <- mean(chosen$dose_price)
    n_can  <- floor(budget / avg_p)
    div(
      p(strong(sprintf("%d bull(s) selected", length(sel))),
        sprintf(" | Avg dose: $%.0f | Budget: $%d | ~%d cows can be bred",
                avg_p, budget, min(n_can, nrow(st$herd_df)))),
      if (avg_p * nrow(st$herd_df) > budget)
        p(style="color:#e65100;", icon("exclamation-triangle"),
          " High price — some cows may be left open.")
    )
  })

  # ── Cull selection table ──────────────────────────────────────────────────────

  output$cull_sel_tbl <- renderDT({
    cd <- dec$cull_df
    req(cd)
    df <- data.frame(
      ID             = cd$id,
      Age            = cd$age,
      `Milk (lb/d)`  = round(cd$last_milk / LAC_DAYS, 1),
      `Fat (lb/d)`   = round(cd$last_fat  / LAC_DAYS, 2),
      `Prot (lb/d)`  = round(cd$last_prot / LAC_DAYS, 2),
      `Meth (g/d)`   = round(cd$last_meth, 1),
      `Milk gPTA`    = round(cd$milk_pta  / LAC_DAYS, 2),
      `Fat gPTA`     = round(cd$fat_pta   / LAC_DAYS, 3),
      `Prot gPTA`    = round(cd$prot_pta  / LAC_DAYS, 3),
      `Meth gPTA`    = round(cd$meth_pta,  1),
      Merit          = round(cd$merit, 2),
      Lethal      = ifelse(is.na(cd$lethal) | cd$lethal == "NN", "No", cd$lethal),
      `Must Cull` = ifelse(cd$must_cull, "YES", "No"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    datatable(
      df, rownames=FALSE, selection="multiple",
      options=list(pageLength=20, dom="ftp", order=list(list(1,"desc"))),
      class="compact stripe hover"
    ) |>
      formatStyle(
        "Must Cull",
        backgroundColor = styleEqual("YES", "#ffebee"),
        fontWeight = styleEqual("YES", "bold"),
        color = styleEqual("YES", "#c62828")
      ) |>
      formatStyle(
        "Lethal",
        color = styleEqual("YES", "#e65100"),
        fontWeight = styleEqual("YES", "bold")
      )
  }, server=FALSE)

  output$cull_sel_summary_ui <- renderUI({
    cd  <- dec$cull_df
    sel <- input$cull_sel_tbl_rows_selected
    req(cd)
    must_n <- sum(cd$must_cull, na.rm=TRUE)
    extra  <- if (length(sel) > 0) {
      cd_sel <- cd[sel, ]
      extra_n <- sum(!cd_sel$must_cull)
      extra_n
    } else 0L
    div(
      p(sprintf("%d cow(s) must be culled (age ≥ %d or lethal carrier).", must_n, MAX_COW_AGE)),
      if (length(sel) > 0)
        p(sprintf("You selected %d additional cow(s) to cull voluntarily.", extra)),
      if (must_n == 0 && length(sel) == 0)
        p(style="color:#888;", "No mandatory culls this round. You may optionally select cows to remove.")
    )
  })

  # ── Heifer pool info banner ───────────────────────────────────────────────────

  output$heifer_pool_info_ui <- renderUI({
    hf <- dec$heifer_df
    st <- sim$state
    if (is.null(st)) return(NULL)
    if (is.null(hf) || nrow(hf) == 0) {
      return(div(style="color:#666;font-size:0.88em;padding:8px 0;margin-bottom:8px;",
        icon("info-circle"),
        " No heifers are ready to enter the herd this generation.",
        " (Heifers born this round will be available in 2 generations.)"
      ))
    }
    div(style="color:#1b5e20;font-size:0.88em;margin-bottom:8px;",
      icon("circle-check"),
      sprintf(" %d heifer(s) are ready to enter. Select which ones to admit.",
              nrow(hf)),
      " Consider their gPTA, inbreeding, and lethal status."
    )
  })

  # ── Heifer selection table ────────────────────────────────────────────────────

  output$heifer_sel_tbl <- renderDT({
    hf <- dec$heifer_df
    req(!is.null(hf) && nrow(hf) > 0)
    df <- data.frame(
      ID          = hf$id,
      Sire        = hf$sire_id,
      Dam         = hf$dam_id,
      `Milk gPTA` = round(hf$milk_pta / LAC_DAYS, 2),
      `Fat gPTA`  = round(hf$fat_pta  / LAC_DAYS, 3),
      `Prot gPTA` = round(hf$prot_pta / LAC_DAYS, 3),
      `Meth gPTA` = round(hf$meth_pta, 1),
      `Inbr %`    = round(hf$inbr * 100, 2),
      Lethal      = ifelse(is.na(hf$lethal) | hf$lethal == "NN", "No", hf$lethal),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(
      df, rownames=FALSE, selection="multiple",
      options=list(pageLength=20, dom="t",
                   order=list(list(3, "desc"))),   # sort by Milk gPTA desc
      class="compact stripe hover"
    ) |>
      formatStyle(
        "Lethal",
        color      = styleEqual(c("Nn","nn"), c("#e65100","#c62828")),
        fontWeight = styleEqual(c("Nn","nn"), c("bold","bold"))
      ) |>
      formatStyle(
        "Inbr %",
        background         = styleColorBar(c(0, 10), "#ffe0b2"),
        backgroundSize     = "100% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  }, server=FALSE)

  output$heifer_sel_summary_ui <- renderUI({
    hf  <- dec$heifer_df
    sel <- input$heifer_sel_tbl_rows_selected
    st  <- sim$state
    req(st)
    n_ready <- if (!is.null(hf)) nrow(hf) else 0L
    n_sel   <- length(sel)
    n_cows  <- nrow(st$herd_df)
    n_must_cull <- sum(st$herd_df$age >= MAX_COW_AGE |
                       (!is.na(st$herd_df$lethal) & st$herd_df$lethal == "nn"))
    projected <- n_cows - n_must_cull - length(input$cull_sel_tbl_rows_selected) + n_sel
    if (n_ready == 0) return(NULL)
    div(
      p(sprintf("%d of %d heifer(s) selected to enter.", n_sel, n_ready)),
      p(style="color:#555;",
        sprintf("Projected herd size after cull + heifer entry: ~%d cows.", max(0, projected)))
    )
  })

  output$advance_tip_ui <- renderUI({
    st <- sim$state
    if (is.null(st)) return(div(style="color:#aaa;", "Initialize simulation first."))
    if (isTRUE(st$finished)) return(div(style="color:#2e7d32;font-weight:bold;",
                                         icon("flag-checkered"), " Simulation complete! Check Progress tab."))
    div(style="color:rgba(255,255,255,0.8);font-size:0.9em;",
        "Select bulls (Step 1) and cows to cull (Step 2), then click Advance Generation.")
  })

  # ── Advance button ────────────────────────────────────────────────────────────

  observeEvent(input$advance_btn, {
    st  <- sim$state
    bd  <- dec$bull_df
    cd  <- dec$cull_df
    hfd <- dec$heifer_df
    req(st, bd, cd)

    # 1. Bull IDs
    sel_bulls <- input$bull_sel_tbl_rows_selected
    bull_ids <- if (length(sel_bulls) > 0) bd$id[sel_bulls] else character(0)
    if (length(bull_ids) == 0) {
      showNotification("Please select at least one bull to use for breeding.", type="warning", duration=5)
      return()
    }

    # 2. Cull IDs = mandatory + optional selections
    must_ids  <- cd$id[cd$must_cull]
    extra_sel <- input$cull_sel_tbl_rows_selected
    extra_ids <- if (length(extra_sel) > 0) cd$id[extra_sel] else character(0)
    cull_ids  <- unique(c(must_ids, extra_ids))

    prog <- Progress$new(session, min=0, max=1)
    prog$set(message="Breeding generation…", value=0.1)
    on.exit(prog$close(), add=TRUE)

    interim <- tryCatch(
      phase1_breed(
        state        = st,
        use_bull_ids = bull_ids,
        cull_ids     = cull_ids,
        budget       = BUDGET_PER_COW * nrow(st$herd_df)
      ),
      error = function(e) {
        showNotification(paste("Breeding error:", e$message), type="error", duration=8)
        NULL
      }
    )
    if (is.null(interim)) return()

    n_bred  <- interim$n_bred %||% nrow(st$herd_df)
    n_open2 <- interim$n_open %||% 0L

    prog$set(message="Finalizing herd…", value=0.7)

    # 3. Heifer IDs to admit from the pool (selected by student)
    heifer_sel  <- input$heifer_sel_tbl_rows_selected
    admit_ids   <- if (!is.null(hfd) && nrow(hfd) > 0 && length(heifer_sel) > 0)
                     hfd$id[heifer_sel]
                   else character(0)
    if (!is.null(hfd) && nrow(hfd) > 0 && length(admit_ids) == 0)
      showNotification("No heifers selected — herd may shrink this generation.",
                       type="warning", duration=5)

    new_state <- tryCatch(
      phase2_finalize(
        state             = st,
        interim           = interim,
        admit_heifer_ids  = interim$heifer_ids,  # always enqueue ALL new female calves
        select_heifer_ids = admit_ids,            # student's selection from existing pool
        raise_bull_ids    = character(0),
        end_year          = end_year_r()
      ),
      error = function(e) {
        showNotification(paste("Finalize error:", e$message), type="error", duration=8)
        NULL
      }
    )
    if (is.null(new_state)) return()

    prog$set(value=1.0)
    sim$state <- new_state

    if (n_open2 > 0)
      showNotification(
        sprintf("%d cow(s) were left open (budget limit or no semen available).", n_open2),
        type="warning", duration=6
      )

    if (isTRUE(sim$state$finished)) {
      showNotification("Simulation complete! Check the Progress tab.", type="message", duration=6)
      updateTabItems(session, "tabs", "progress")
    } else {
      gen_next <- sim$state$year - sim$state$start_year + 1
      showNotification(
        sprintf("Generation advanced! Now in Generation %d.", gen_next),
        type="message", duration=4
      )
      updateTabItems(session, "tabs", "herd")
    }
  })

  # ── Progress: genetic trend reactive ─────────────────────────────────────────

  trend_data_r <- reactive({
    st <- sim$state
    req(st)
    sdf <- st$summary_df
    req(!is.null(sdf) && nrow(sdf) > 0)
    data.frame(
      Generation = seq_len(nrow(sdf)),
      Year       = sdf$year,
      Milk  = sdf$mean_milk_pta,
      Fat   = sdf$mean_fat_pta,
      Prot  = sdf$mean_prot_pta,
      Meth  = sdf$mean_meth_pta,
      Inbr  = sdf$mean_inbr
    )
  })

  output$genetic_trend_plot <- renderPlot({
    df <- trend_data_r()
    req(df)
    long <- tidyr::pivot_longer(df, cols=c(Milk,Fat,Prot,Meth),
                                names_to="Trait", values_to="Value")
    ggplot(long, aes(x=Generation, y=Value, color=Trait, group=Trait)) +
      geom_line(linewidth=1.1) +
      geom_point(size=2.5) +
      scale_color_manual(values=c(Milk="#1565c0",Fat="#f9a825",Prot="#2e7d32",Meth="#6a1b9a")) +
      labs(title="Genetic Trend by Trait", x="Generation", y="Mean gPTA", color="Trait") +
      theme_minimal(base_size=13) +
      theme(legend.position="bottom",
            panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold", color="#002855"))
  })

  output$inbr_plot <- renderPlot({
    df <- trend_data_r()
    req(df)
    ggplot(df, aes(x=Generation, y=Inbr*100)) +
      geom_line(color="#c62828", linewidth=1.1) +
      geom_point(color="#c62828", size=2.5) +
      labs(title="Average Inbreeding (%)", x="Generation", y="Inbreeding (%)") +
      theme_minimal(base_size=13) +
      theme(panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold", color="#002855"))
  })

  # ── Progress summary table ────────────────────────────────────────────────────

  output$progress_summary_tbl <- renderDT({
    df <- trend_data_r()
    req(df)
    df$`Inbr %` <- round(df$Inbr * 100, 2)
    df$Milk  <- round(df$Milk,  1)
    df$Fat   <- round(df$Fat,   1)
    df$Prot  <- round(df$Prot,  1)
    df$Meth  <- round(df$Meth,  1)
    df$Inbr  <- NULL
    df$Year  <- NULL
    datatable(df, rownames=FALSE, selection="none",
              options=list(dom="t", pageLength=10),
              class="compact stripe")
  }, server=FALSE)

  # ── Final summary ─────────────────────────────────────────────────────────────

  output$final_summary_ui <- renderUI({
    st <- sim$state
    if (is.null(st) || !isTRUE(st$finished)) return(NULL)
    df <- trend_data_r()
    if (is.null(df) || nrow(df) < 2) return(NULL)
    first <- df[1,]; last <- df[nrow(df),]
    div(class="save-box", style="border-color:#002855;margin-top:20px;",
      h4(icon("flag-checkered"), " Simulation Complete!"),
      p(strong("Genetic progress over all generations:")),
      tags$ul(
        tags$li(sprintf("Milk:    %+.1f lbs  (%.1f → %.1f)", last$Milk-first$Milk, first$Milk, last$Milk)),
        tags$li(sprintf("Fat:     %+.1f lbs  (%.1f → %.1f)", last$Fat-first$Fat,   first$Fat,  last$Fat)),
        tags$li(sprintf("Protein: %+.1f lbs  (%.1f → %.1f)", last$Prot-first$Prot, first$Prot, last$Prot)),
        tags$li(sprintf("Methane: %+.1f units (%.1f → %.1f)", last$Meth-first$Meth, first$Meth, last$Meth)),
        tags$li(sprintf("Inbreeding: %.2f%% → %.2f%%", first$Inbr*100, last$Inbr*100))
      )
    )
  })

}  # end server

# =============================================================================
shinyApp(ui, server)
