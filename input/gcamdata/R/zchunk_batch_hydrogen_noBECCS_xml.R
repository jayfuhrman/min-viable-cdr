# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_hydrogen_noBECCS_xml
#'
#' Construct XML data structure for \code{hydrogen_noBECCS.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_noBECCS.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen_noBECCS.xml.R} (energy XML).
module_energy_batch_hydrogen_noBECCS_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.Supplysector_h2",
              "L225.SubsectorLogit_h2",
              "L225.SubsectorShrwt_h2",
              "L225.SubsectorShrwtFllt_h2",
              "L225.SubsectorInterp_h2",
              "L225.SubsectorInterpTo_h2",
              "L225.StubTech_h2",
              "L225.GlobalTechCoef_h2",
              "L225.GlobalTechCost_h2",
              "L225.GlobalTechShrwt_h2",
              "L225.PrimaryRenewKeyword_h2",
              "L225.AvgFossilEffKeyword_h2",
              "L225.GlobalTechCapture_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_noBECCS.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2")
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2")
    L225.SubsectorShrwt_h2 <- get_data(all_data, "L225.SubsectorShrwt_h2")
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2")
    L225.SubsectorInterp_h2 <- get_data(all_data, "L225.SubsectorInterp_h2")
    L225.SubsectorInterpTo_h2 <- get_data(all_data, "L225.SubsectorInterpTo_h2")
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2") # Modify
    L225.GlobalTechEff_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2") %>% #our HFTO development removed efficiencies and replaced them with coefficients.
      mutate(coefficient = 1/coefficient) %>% #for compatibility with this chunk, we take the inverse of the calculated coefficients and simply pass downstream
      rename(efficiency = coefficient)
    L225.GlobalTechCost_h2 <- get_data(all_data, "L225.GlobalTechCost_h2") # Modify
    L225.GlobalTechShrwt_h2 <- get_data(all_data, "L225.GlobalTechShrwt_h2") # Modify
    L225.PrimaryRenewKeyword_h2 <- get_data(all_data, "L225.PrimaryRenewKeyword_h2")
    L225.AvgFossilEffKeyword_h2 <- get_data(all_data, "L225.AvgFossilEffKeyword_h2") # Modify
    L225.GlobalTechCapture_h2 <- get_data(all_data, "L225.GlobalTechCapture_h2") # Modify

    # ===================================================
    # Filter out BECCS for H2 production
    L225.StubTech_h2 %>%
      filter(stub.technology != "biomass to H2 CCS") -> L225.StubTech_h2_noBECCS

    L225.GlobalTechEff_h2 %>%
      filter(technology != "biomass to H2 CCS") -> L225.GlobalTechEff_h2_noBECCS

    L225.GlobalTechCost_h2 %>%
      filter(technology != "biomass to H2 CCS") -> L225.GlobalTechCost_h2_noBECCS

    L225.GlobalTechShrwt_h2 %>%
      filter(technology != "biomass to H2 CCS") -> L225.GlobalTechShrwt_h2_noBECCS

    L225.AvgFossilEffKeyword_h2 %>%
      filter(technology != "biomass to H2 CCS") -> L225.AvgFossilEffKeyword_h2_noBECCS

    L225.GlobalTechCapture_h2 %>%
      filter(technology != "biomass to H2 CCS") -> L225.GlobalTechCapture_h2_noBECCS

    # Produce outputs
    create_xml("hydrogen_noBECCS.xml") %>%
      add_logit_tables_xml(L225.Supplysector_h2, "Supplysector") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2, "SubsectorLogit") -> hydrogen_noBECCS.xml

    if(!is.null(L225.SubsectorShrwt_h2)) {
      hydrogen_noBECCS.xml %>%
        add_xml_data(L225.SubsectorShrwt_h2, "SubsectorShrwt") ->
        hydrogen_noBECCS.xml
    }
    if(!is.null(L225.SubsectorShrwtFllt_h2)) {
      hydrogen_noBECCS.xml %>%
        add_xml_data(L225.SubsectorShrwtFllt_h2, "SubsectorShrwtFllt") ->
        hydrogen_noBECCS.xml
    }
    if(!is.null(L225.SubsectorInterp_h2)) {
      hydrogen_noBECCS.xml %>%
        add_xml_data(L225.SubsectorInterp_h2, "SubsectorInterp") ->
        hydrogen_noBECCS.xml
    }
    if(!is.null(L225.SubsectorInterpTo_h2)) {
      hydrogen_noBECCS.xml %>%
        add_xml_data(L225.SubsectorInterpTo_h2, "SubsectorInterpTo") ->
        hydrogen_noBECCS.xml
    }

    hydrogen_noBECCS.xml <- hydrogen_noBECCS.xml %>%
      add_xml_data(L225.StubTech_h2_noBECCS, "StubTech") %>%
      add_xml_data(L225.GlobalTechEff_h2_noBECCS, "GlobalTechEff") %>%
      add_xml_data(L225.GlobalTechCost_h2_noBECCS, "GlobalTechCost") %>%
      add_xml_data(L225.GlobalTechShrwt_h2_noBECCS, "GlobalTechShrwt") %>%
      add_xml_data(L225.PrimaryRenewKeyword_h2, "PrimaryRenewKeyword") %>%
      add_xml_data(L225.AvgFossilEffKeyword_h2_noBECCS, "AvgFossilEffKeyword") %>%
      add_xml_data(L225.GlobalTechCapture_h2_noBECCS, "GlobalTechCapture") %>%
      add_precursors("L225.Supplysector_h2",
                     "L225.SubsectorLogit_h2",
                     "L225.SubsectorShrwt_h2",
                     "L225.SubsectorShrwtFllt_h2",
                     "L225.SubsectorInterp_h2",
                     "L225.SubsectorInterpTo_h2",
                     "L225.StubTech_h2",
                     "L225.GlobalTechCoef_h2",
                     "L225.GlobalTechCost_h2",
                     "L225.GlobalTechShrwt_h2",
                     "L225.PrimaryRenewKeyword_h2",
                     "L225.AvgFossilEffKeyword_h2",
                     "L225.GlobalTechCapture_h2") ->
      hydrogen_noBECCS.xml

    return_data(hydrogen_noBECCS.xml)
  } else {
    stop("Unknown command")
  }
}
