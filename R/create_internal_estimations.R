
#-----------------------------------------------------------------------
## mean estimation
#' @importFrom stats model.frame
#' @importFrom stats na.pass
#' @importFrom stats weights
get_mean <- function(vars, design, na.rm=FALSE, deff=FALSE,...){
  "
  Args:
  vars = formula
  design = survey design
  "

  if (!inherits(design,"survey.design"))
    stop("design is not a survey design")

  x <- model.frame(vars, design$variables,na.action=na.pass)
  x <- as.matrix(x)

  ## eliminar NAs en caso de ser indicado
  if(na.rm){
    nas <- rowSums(is.na(x))
    design <- design[nas==0,]
    x <- x[nas==0,,drop=FALSE]
  }

  ## Estimacion media ------------------------
  ### \hat{\mu} = sum(wi xi)/ sum(wi)

  wi <- 1/design$prob   # pesos
  sum_wi <- sum(wi)     # sumatoria wi
  mu_hat <- sum(x*wi)/sum_wi   # \hat{\mu}

  ## Linealizacion ------------------------
  ### ui = (wi * (xi-\hat{\mu}))/sum(wi)

  x <- sweep(x, 2, mu_hat) # Centrar x restando la media (residuos x_i -\hat(mu)).
  ui <- unname(x*wi/sum_wi)

  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }


  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]], fpc = fpc, nPSU = design$nPSU)

  estimation <- data.frame(est = mu_hat, se = sqrt(v)) %>%
    dplyr::mutate(cv = .data$se/.data$est)  ## add cv

  ## get deff
  if(deff){

    nobs<-NROW(design$cluster[[1]])
    Vx_hat <- var_point(x, design, na.rm=na.rm)  # estimador de Var(x)
    vsrs <- Vx_hat / nobs
    vsrs <- vsrs * (sum_wi - nobs) / sum_wi

    estimation$deff <- v / vsrs

  }


  return(estimation)
}



#-----------------------------------------------------------------------
## total estimation
get_total <- function(vars, design, na.rm=FALSE, deff=FALSE,...){
  "
  Args:
  vars = formula
  design = survey design
  "

  if (!inherits(design,"survey.design"))
    stop("design is not a survey design")

  ## obtener matriz
  x <- model.frame(vars, design$variables,na.action=na.pass)
  x <- as.matrix(x)

  if(na.rm){
    nas <- rowSums(is.na(x))
    design <- design[nas==0,]
    x <- x[nas==0,,drop=FALSE]
  }

  ## Estimacion total ------------------------
  ### \hat{\T} = sum(wi xi)

  wi <- 1/design$prob   # pesos
  T_hat <- sum(x*wi)   # \hat{\mu}

  ## Linealizacion ------------------------
  ### ui = x*wi  se queda igual

  ui <- unname(x*wi)

  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }


  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]], fpc = fpc, nPSU = design$nPSU)

  estimation <- data.frame(est = T_hat, se = sqrt(v)) %>%
    dplyr::mutate(cv = .data$se/.data$est)  ## add cv

  ## get deff
  if(deff){

    #nobs<-NROW(design$cluster[[1]])
    # nobs <- length(weights(design))
    # N <- sum(wi)
    # Vx_hat <- var_point(x, design, na.rm=na.rm)  # estimador de Var(x)
    # vsrs <- Vx_hat * sum(weights(design)^2)
    # vsrs <- vsrs * (N - nobs) / N
    #
    # estimation$deff <- v / vsrs


    w <- weights(design)
    nobs <- nrow(design$variables)
    N <- sum(w)

    Vx_hat <- var_point(x, design, na.rm=na.rm)

    #vsrs <- Vx_hat * sum(w^2)
    vsrs <- Vx_hat * (N^2 / nobs)
    vsrs <- vsrs * (N - nobs) / N

    estimation$deff <- v / vsrs


  }

  return(estimation)
}




#-----------------------------------------------------------------------
## ratio estimation

get_ratio <- function(numerator, denominator, design, na.rm=FALSE, deff=FALSE,...){
  "
  Args:
  vars = formula
  design = survey design
  "

  if (!inherits(design,"survey.design"))
    stop("design is not a survey design")


  x_num <- model.frame(numerator, design$variables,na.action=na.pass)
  x_num <- as.matrix(x_num)

  x_den <- model.frame(denominator, design$variables,na.action=na.pass)
  x_den <- as.matrix(x_den)

  ## eliminar NAs en caso de ser indicado
  if(na.rm){
    nas <- rowSums(is.na(x_num))
    design <- design[nas==0,]
    x <- x[nas==0,,drop=FALSE]
  }

  ## Estimacion ratio ------------------------
  ### \hat{\R} = sum(wi num_i) /sum(wi den_i)

  wi <- 1/design$prob   # pesos
  T_hat_num <- sum(x_num*wi)
  T_hat_den <- sum(x_den*wi)

  R_hat <- T_hat_num/ T_hat_den

  ## Linealizacion ------------------------

  # r <- (numerator[,i]-rval$ratio[i,j]*denominator[,j])/sum(denominator[,j]/design$prob)
  ui <- unname((wi * (x_num-R_hat*x_den))/T_hat_den)


  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }


  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]], fpc = fpc, nPSU = design$nPSU)
  estimation <- data.frame(est = R_hat, se = sqrt(v)) %>%
    dplyr::mutate(cv = .data$se/.data$est)  ## add cv


  ## get deff
  if (deff) {

    g <- ui / wi
    nobs <- nrow(design$variables)
    N <- sum(wi)

    Vg_hat <- var_point(g, design, na.rm=FALSE)

    vsrs <- Vg_hat * (N^2 / nobs)
    vsrs <- vsrs * (N - nobs) / N

    estimation$deff <- as.numeric(v / vsrs)
  }


  return(estimation)
}



#-----------------------------------------------------------------------
## varianza mediante linealizacion de taylor 1er orden (score)

linearization_variance <- function(ui, strata, psu, fpc=NULL, lonely.psu = getOption("survey.lonely.psu"), nPSU = NULL){

  df <- data.frame(strata = strata,
                   psu = psu,
                   ei = ui)
  # agrupar a nivel PSU dentro de cada estrato
  df_psu <- df %>%
    dplyr::group_by(.data$strata, .data$psu) %>%
    dplyr::summarise(E_hi = sum(.data$ei), .groups = "drop")


  # nPSU observadas por estrato en el subset actual
  nh_obs <- df_psu %>%
    dplyr::count(.data$strata, name = "nh_obs")


  if (is.null(nPSU)) {
    # caso simple sin dominios
    df_npsu <- nh_obs %>%
      dplyr::mutate(nPSU = nh_obs)

  }else{
    ## se espera que sea un df
    df_npsu <- nh_obs %>%
      dplyr::left_join(nPSU, by = "strata")
  }


  ## revisamos casos que no son partes del subset pero si de la muestra
  df_npsu <- df_npsu %>%
    dplyr::mutate(dropped = pmax(.data$nPSU - .data$nh_obs, 0L))

  if(sum(df_npsu$dropped)>0){

    zeros <- df_npsu %>%
      dplyr::filter(.data$dropped > 0) %>%
      tidyr::uncount(.data$dropped, .remove = FALSE) %>%
      dplyr::group_by(.data$strata) %>%
      dplyr::mutate(psu = NA )%>%
      dplyr::ungroup() %>%
      dplyr::transmute(strata, psu, E_hi = 0)

    df_psu <- dplyr::bind_rows(df_psu, zeros)
  }

  # Calculo de sumatoria de para la varianza
  var_by_stratum <- df_psu %>%
    dplyr::left_join(df_npsu, by = "strata") %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      nPSU = unique(.data$nPSU),
      nh_obs = unique(.data$nh_obs),
      mean_E = sum(.data$E_hi)/ .data$nPSU,      # mean(E_hi),
      ss = sum((.data$E_hi - .data$mean_E)^2),   # centramos
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      nh_obs = nh_obs,
      lonely_nPSU = (nPSU == 1),
      lonely_nobs = (nh_obs == 1),
      df = dplyr::if_else(nPSU > 1, nPSU / (nPSU - 1), 1)
    )


  ## procesamiento para estratos con una sola upm
  lonely.psu <- match.arg(lonely.psu, c("adjust", "average", "remove", "certainty", "fail"))
  lonely_idx    <- which(var_by_stratum$lonely_nPSU)

  if(length(ui)==1){
    if(is.na(ui)){
      return(NA_real_)
    }
  }

  if (length(lonely_idx) > 0) {

    if (lonely.psu == "fail") {
      stop("Stratum has only one sampling unit (lonely PSU).")
    }

    if (lonely.psu == "adjust") {

      var_by_stratum$ss[lonely_idx] <- var_by_stratum$mean_E[lonely_idx]^2

    } else if (lonely.psu == "remove") {
      # sacamos los estratos lonely de la contribución de varianza
      var_by_stratum$ss[lonely_idx] <- 0

    } else if (lonely.psu == "certainty") {
      # lonely certainty: var = 0
      var_by_stratum$ss[lonely_idx] <- 0

    }
  }


  var_by_stratum <- var_by_stratum %>%
    dplyr::mutate(s2 = .data$ss* .data$df)

  ## verificamos correccion por poblacion finita
  if(!is.null(fpc)){
    var_by_stratum <- var_by_stratum %>%
      dplyr::left_join(fpc, by = 'strata') %>%
      dplyr::mutate(fpc = ifelse(.data$nPSU/.data$Nh == 1, 0, (1-(.data$nPSU/.data$Nh))),
                    s2 = .data$s2 * .data$fpc)
  }

  if (nrow(var_by_stratum) == 0) return(NA_real_)

  V_total <- sum(var_by_stratum$s2) ## revisar bien los NAs

  if (lonely.psu == "average") {

    # v_total ya calculado sumando aportes por estrato (lonely aportan 0)
    p <- mean(var_by_stratum$lonely_nobs & !var_by_stratum$lonely_nobs)

    V_total <- V_total / (1 - p)
  }


  return(V_total)

}


#-----------------------------------------------------------------------
## funcion general
get_FUN_domain <- function(vars, denominator= NULL, design, fun_est, domains=NULL, na.rm=FALSE, deff=FALSE){


  if(!is.null(domains)){

    #domains <- convert_to_formula(domains)
    byfactors <- model.frame(domains, model.frame(design), na.action=na.pass)
    byfactor <- interaction(byfactors)
    uniquelevels <- sort(unique(byfactor))

    dom_vals <- sort(uniquelevels)
    # design$nPSU <- dplyr::tibble(strata = design$strata[[1]],
    #                              cluster = design$cluster[[1]]) %>%
    #   dplyr::group_by(.data$strata) %>%
    #   dplyr::reframe(nPSU = dplyr::n_distinct(.data$cluster))

    design$nPSU <- data.frame(nPSU= design$fpc$sampsize, strata = design$strata[[1]]) %>% dplyr::distinct()

    res <- lapply(dom_vals, function(d) {
      subd <- subset(design, byfactor %in% d)
      fun_est(vars, design = subd, na.rm = na.rm, deff = deff, denominator = denominator)
    })

    byfactors$byfactor <- byfactor
    byfactors <- byfactors %>%
      dplyr::distinct() %>%
      dplyr::arrange(byfactor) %>%
      dplyr::select(-byfactor)

    res <- dplyr::bind_rows(res)
    # rownames(res) <- dom_vals

    if(deff){
      byfactors[c('est', 'se', 'cv', 'deff')] <- res
    }else{
      byfactors[c('est', 'se', 'cv')] <- res
    }
    res <- byfactors


  }else{
    res <- fun_est(vars, design= design, na.rm = na.rm, deff = deff, denominator= denominator)
  }

  return(res)
}


# var MAS -----------------------------------------------------------------

var_point <- function(x, design, na.rm=FALSE){

  w <- stats::weights(design)

  ok <- (w != 0) & !is.na(x)
  if (na.rm) { x <- x[ok]; w <- w[ok] } else if (any(!ok)) return(NA_real_)

  n <- length(x)
  if (n <= 1) return(NA_real_)

  psum <- sum(w)
  xbar <- sum(w*x)/psum
  # estimador de varianza tipo Kish:
  z <- (x - xbar)^2 * (n/(n-1))
  zbar <- sum(w*z)/psum
  zbar

}



