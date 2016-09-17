#' heemod: Health Economic Evaluation MODelling
#' 
#' \code{heemod} is an R toolset for health economic 
#' evaluation modelling. It aims to provide a simple and 
#' consistent interface for Markov models specification and
#' comparison. Non-homogeneous Markov models (with time
#' varying properties) are supported.
#' 
#' @docType package
#' @name heemod
#'   
#' @importFrom dplyr filter_
#' @importFrom dplyr mutate_
#' @importFrom dplyr do_
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarise_
#' @importFrom dplyr as.tbl
#' @importFrom dplyr data_frame
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr "%>%"
#' @importFrom dplyr desc
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_if
#' @importFrom dplyr funs
#'   
#' @importFrom lazyeval lazy
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval as.lazy_dots
#' @importFrom lazyeval lazy_eval
#'   
#' @importFrom pryr standardise_call
#'   
#' @importFrom utils head
#' @importFrom utils modifyList
#' @importFrom utils globalVariables
#' @importFrom utils as.roman
#'   
#' @importFrom stats pnorm
#' @importFrom stats qbeta
#' @importFrom stats qbinom
#' @importFrom stats qgamma
#' @importFrom stats qlnorm
#' @importFrom stats qnorm
#' @importFrom stats terms
#' @importFrom stats setNames
#' @importFrom stats reorder
#' @importFrom stats na.omit
#' @importFrom stats update
#'   
#' @importFrom graphics par
#'   
#' @importFrom diagram plotmat
#'   
#' @importFrom mvnfast rmvn
#'   
#' @importFrom logitnorm qlogitnorm
#'   
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 xlim
#'   
#' @importFrom tidyr gather_
#'   
#' @importFrom memoise memoise
#' @importFrom memoise timeout
#'   
#' @importFrom rgho get_gho_data
#'   
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#'   
#' @importFrom tools file_ext
#' 
#' @importFrom Hmisc wtd.quantile
#' @importFrom Hmisc wtd.mean
#' @importFrom Hmisc wtd.var
#' 
#' @importFrom grDevices dev.off
#' @importFrom grDevices cairo_pdf
#' @importFrom grDevices png
#' 
#' @importFrom graphics plot
#'   
#' @importFrom tibble tibble
#' @importFrom tibble tibble_
NULL
