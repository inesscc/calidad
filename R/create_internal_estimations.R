
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

  wi <- stats::weights(design) #1/design$prob   # pesos
  sum_wi <- sum(wi)     # sumatoria wi
  mu_hat <- sum(x*wi/sum_wi)   # \hat{\mu}

  ## Linealizacion ------------------------
  ### ui = (wi * (xi-\hat{\mu}))/sum(wi)

  x <- sweep(x, 2, mu_hat) # Centrar x restando la media (residuos x_i -\hat(mu)).
  ui <- unname(x*wi/sum_wi)

  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      psu = design$cluster[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }

  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]],
                              fpc = fpc, nPSU = design$nPSU)

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

  #wi <- 1/design$prob   # pesos
  T_hat <- sum(x*weights(design)) # sum(x/design$prob)   # \hat{\mu}

  ## Linealizacion ------------------------
  ### ui = x*wi  se queda igual

  ui <- unname(x*weights(design)) #unname(x/design$prob)

  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      psu = design$cluster[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }

  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]],
                              fpc = fpc, nPSU = design$nPSU)

  estimation <- data.frame(est = T_hat, se = sqrt(v)) %>%
    dplyr::mutate(cv = .data$se/.data$est)  ## add cv

  ## get deff ------------------------
  if(deff){

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
    x_num <- x_num[nas==0,,drop=FALSE]
  }

  ## Estimacion ratio ------------------------
  ### \hat{\R} = sum(wi num_i) /sum(wi den_i)

  #wi <- 1/design$prob   # pesos
  T_hat_num <- sum(x_num/design$prob )
  T_hat_den <- sum(x_den/design$prob )

  R_hat <- T_hat_num/ T_hat_den

  ## Linealizacion ------------------------
  ui <- unname(( (x_num-R_hat*x_den)/design$prob )/T_hat_den)


  ## revisar fpc -----------------------
  fpc <- design$fpc$popsize
  if(!is.null(fpc)){
    fpc <- data.frame(strata = design$strata[[1]],
                      psu = design$cluster[[1]],
                      Nh = unname(fpc)) %>%
      dplyr::distinct()
  }

  ## Varianza ------------------------
  v <- linearization_variance(ui = ui, strata = design$strata[[1]], psu = design$cluster[[1]],
                              fpc = fpc, nPSU = design$nPSU)

  estimation <- data.frame(est = R_hat, se = sqrt(v)) %>%
    dplyr::mutate(cv = .data$se/.data$est)  ## add cv


  ## get deff
  if (deff) {

    g <- ui * design$prob # /wi
    nobs <- nrow(design$variables)
    N <- sum(1/design$prob)

    Vg_hat <- var_point(g, design, na.rm=FALSE)

    vsrs <- Vg_hat * (N^2 / nobs)
    vsrs <- vsrs * (N - nobs) / N

    estimation$deff <- as.numeric(v / vsrs)
  }


  return(estimation)
}

#-----------------------------------------------------------------------
## varianza mediante linealizacion de taylor 1er orden (score)

#'@keywords internal
linearization_variance <- function(ui, strata, psu, fpc=NULL, lonely.psu = getOption("survey.lonely.psu"), nPSU = NULL){

  if(length(ui)==1){
    if(is.na(ui)){
      return(NA_real_)
    }
  }

  df <- data.frame(strata = strata,
                   psu = psu,
                   ei = ui)

  df_psu <- df %>%
    dplyr::group_by(.data$strata, .data$psu) %>% # agrupar a nivel PSU dentro de cada estrato
    dplyr::summarise(E_hi = sum(.data$ei), .groups = "drop")


  # nPSU observadas por estrato en el subset actual
  nh_obs <- df_psu %>%
    dplyr::count(.data$strata, name = "nh_obs")


  ## revisamos casos que no son partes del subset pero si de la muestra
  df_npsu <- nh_obs %>%
    dplyr::left_join(nPSU, by = "strata") %>%  # unimos size de strata
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

  df_psu <- df_psu %>%
    dplyr::left_join(df_npsu, by = "strata") %>%
    dplyr::mutate(lonely_nPSU = (nPSU == 1))

  ## Revision fpc
  if(!is.null(fpc)){
    df_psu <- df_psu %>%
      dplyr::left_join(fpc, by = c('strata', 'psu')) %>%
      dplyr::mutate(fpc = ifelse(.data$nPSU/.data$Nh == 1, 0, (.data$Nh-.data$nPSU)/.data$Nh)
                    ,lonely_nPSU = ifelse(.data$Nh==1, FALSE, .data$lonely_nPSU)
      )
  }else{
    df_psu['fpc'] <- 1
  }


  df_psu2 <- df_psu %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(
      mean_E = sum(.data$E_hi) / .data$nPSU,
      scale = dplyr::if_else(nPSU > 1, fpc * nPSU / (nPSU - 1), fpc * 1)
    ) %>%
    dplyr::ungroup()



  ## procesamiento para estratos con una sola upm
  lonely.psu <- match.arg(lonely.psu, c("adjust", "average", "remove", "certainty", "fail"))
  lonely_idx <- sum(df_psu2$lonely_nPSU)

  if (lonely_idx > 0 & lonely.psu == "fail") {
    stop("At least one stratum contains only one PSU.")
  }

  if (lonely.psu == "adjust") {
    ## promedio de todo
    center_stratum <- sum(df_psu2$E_hi)/sum(nPSU$nPSU) # sum(df_psu2$nPSU)

    df_psu2 <- df_psu2 %>%
      dplyr::mutate(mean_E = ifelse(.data$lonely_nPSU, center_stratum, .data$mean_E)
                    )
  }

  df_psu2 <- df_psu2 %>%
    dplyr::mutate(E_hi_center =.data$E_hi - .data$mean_E)

  var_by_stratum <- df_psu2 %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(s2 = sum((sqrt(.data$scale)*.data$E_hi_center)^2),
                     lonely_nPSU = unique(.data$lonely_nPSU))


  if (nrow(var_by_stratum) == 0) return(NA_real_)

  V_total <- sum(var_by_stratum$s2)

  if (lonely.psu == "average") {

    # v_total ya calculado sumando aportes por estrato (lonely aportan 0)
    p <- length(var_by_stratum$lonely_nPSU)/sum(!var_by_stratum$lonely_nPSU)

    V_total <- V_total* p
  }

  return(V_total)

}

#-----------------------------------------------------------------------

## general
get_FUN_domain <- function(vars, denominator= NULL, design, fun_est, domains=NULL, na.rm=FALSE, deff=FALSE){

  if(!is.null(domains)){

    byfactors <- model.frame(domains, model.frame(design), na.action=na.pass)
    byfactor <- interaction(byfactors)

    dropped <- weights(design, "sampling") == 0

    uniquelevels <- sort(unique(byfactor[!dropped]))

    res <- lapply(uniquelevels, function(d) {
      idx <- (byfactor %in% d) & (!dropped)
      subd <- design[idx, ]
      subd$nPSU <- data.frame(nPSU= subd$fpc$sampsize, strata = subd$strata[[1]]) %>% dplyr::distinct()
      fun_est(vars, design=subd, na.rm=na.rm, deff=deff, denominator=denominator)
    })

    by_out <- byfactors[match(uniquelevels, byfactor), , drop=FALSE]
    res <- dplyr::bind_rows(res)
    res <- dplyr::bind_cols(by_out, res)

  } else{
    design$nPSU <- data.frame(nPSU= design$fpc$sampsize, strata = design$strata[[1]]) %>% dplyr::distinct()
    res <- fun_est(vars, design= design, na.rm = na.rm, deff = deff, denominator= denominator)
  }
  return(res)
}


# var MAS -----------------------------------------------------------------

var_point <- function(x, design, na.rm=FALSE){

  w <- stats::weights(design)

  ok <- (w != 0) & !is.na(x)  # n

  n <- sum(ok) #length(x)
  if (n <= 1) return(NA_real_)

  x <- x[ok]
  w <- w[ok]

  psum <- sum(w)
  xbar <- sum(w*x)/psum
  # estimador de varianza tipo Kish:
  z <- (x - xbar)^2 * (n/(n-1))
  zbar <- sum(w*z)/psum
  zbar

}



