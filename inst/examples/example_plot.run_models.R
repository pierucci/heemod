
## These examples require \code{res_mod} from the hip replacement model discussed in
## `vignette("non-homogeneous", package = "heemod")`.

\dontrun{
  plot(res_mod)

  plot(res_mod, model = "all")
  plot(res_mod, model = "all", panels = "by_state")

  plot(res_mod, model = "all", include_states = c("RevisionTHR", "SuccessR"))
  plot(res_mod, model = "all", panels = "by_state", include_states = c("RevisionTHR", "SuccessR"))
 
  plot(res_mod, model = 2, panel = "by_state", include_states = c("RevisionTHR", "SuccessR"))
  
}
