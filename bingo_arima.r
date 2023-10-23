#' Nosso objetivo é descobrir qual a ordem do ARIMA usando análise descritiva

# gerando os dados --------------------------------------------------------

ar <- sample(0:1, 1)
ma <- sample(0:1, 1)
dif <- sample(0:1, 1)

ar <- 1
dif <- 0
ma <- 1

ar_parm <- runif(ar, min = .3, max = .4)
ma_parm <- runif(ma, min = .3, max = .4)

dados <- data.frame(
  mes = 1:(300 + dif),
  vendas = arima.sim(list(
    order = c(ar, dif, ma),
    ma = ma_parm,
    ar = ar_parm
  ), n = 300)
)

# montando os dados -------------------------------------------------------

dados_tsibble <- dados |>
  dplyr::mutate(
    mes = as.Date("1995-05-01") + months(mes),
    mes = tsibble::yearmonth(mes)
  ) |>
  tsibble::as_tsibble(index = mes)

# descritiva --------------------------------------------------------------

dados_tsibble |>
  feasts::gg_tsdisplay(vendas, plot_type = "partial")

tseries::adf.test(dados_tsibble$vendas)

dados_tsibble |>
  fabletools::features(
    vendas,
    list(
      feasts::unitroot_kpss,
      feasts::unitroot_ndiffs
    )
  )

dados_tsibble |>
  dplyr::mutate(dif_vendas = tsibble::difference(vendas)) |>
  feasts::gg_tsdisplay(dif_vendas, plot_type = "partial", lag_max = 30)

dados_tsibble |>
  dplyr::mutate(dif_vendas = tsibble::difference(vendas)) |>
  dplyr::mutate(dif_vendas2 = tsibble::difference(dif_vendas)) |>
  feasts::gg_tsdisplay(dif_vendas2, plot_type = "partial", lag_max = 30)

# modelagem ---------------------------------------------------------------

fit <- dados_tsibble |>
  fabletools::model(
    # arima_manual = fable::ARIMA(vendas ~ 1 + pdq(2,0,2) + PDQ(0,0,0)),
    stepwise = fable::ARIMA(vendas ~ 1 + PDQ(0,0,0)),
    search = fable::ARIMA(vendas ~ 1 + PDQ(0,0,0), stepwise = FALSE)
  )

dplyr::glimpse(fit)

fit |>
  broom::glance() |>
  dplyr::select(.model, AIC) |>
  dplyr::arrange(AIC)

fit |>
  broom::augment() |>
  dplyr::filter(.model == "stepwise") |>
  feasts::gg_tsdisplay(.resid, plot_type = "partial", lag_max = 30)

# gabarito ----------------------------------------------------------------

c(ar, dif, ma)
ar_parm
ma_parm
