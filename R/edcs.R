#' Return information on chemicals of interest
#'
#' @returns A \link[data.table]{data.table} object. Each entry has
#' information on full and short names, chemical class, SMILES, PubChem, CTD,
#' and ExposomeExplorer IDs, eventual parental compound.
#'
#' @export
edcs_information <- function() {
  template <- list(
    full_name = "",
    short_name = "",
    class = "",
    smiles = "",
    pubchem_cid = "",
    ctd_id = "",
    exposome_explorer_id = "",
    parental_compound = ""
  )
  .names <- names(template)

  edcs_info <- list(
    dedtp = list(
      full_name = "diethyl dithiophosphate",
      short_name = "DEDTP",
      class = "OP pesticide metabolites",
      smiles = "CCOP(=S)(OCC)S",
      pubchem_cid = "9274",
      ctd_id = "C000654497",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    dep = list(
      full_name = "diethyl phosphate",
      short_name = "DEP",
      class = "OP pesticide metabolites",
      smiles = "CCOP(=O)(O)OCC",
      pubchem_cid = "654",
      ctd_id = "C034789",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    detp = list(
      full_name = "diethyl thiophosphate",
      short_name = "DETP",
      class = "OP pesticide metabolites",
      smiles = "CCOP(=S)([O-])OCC",
      pubchem_cid = "3683036",
      ctd_id = "C035638",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    dmdtp = list(
      full_name = "dimethyl dithiophosphate",
      short_name = "DMDTP",
      class = "OP pesticide metabolites",
      smiles = "CSP(=O)(O)SC",
      pubchem_cid = "NA",
      ctd_id = "Unsure",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    dmp = list(
      full_name = "dimethyl phosphate",
      short_name = "DMP",
      class = "OP pesticide metabolites",
      smiles = "COP(=O)(O)OC",
      pubchem_cid = "13134",
      ctd_id = "C007477",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    dmtp = list(
      full_name = "dimethyl thiophosphate",
      short_name = "DMTP",
      class = "OP pesticide metabolites",
      smiles = "COP(=S)(O)OC",
      pubchem_cid = "168140",
      ctd_id = "C040340",
      exposome_explorer_id = "NA",
      parental_compound = "NA"
    ),
    bpa = list(
      full_name = "bisphenol A",
      short_name = "BPA",
      class = "Phenols",
      smiles = "CC(C)(C1=CC=C(C=C1)O)C2=CC=C(C=C2)O",
      pubchem_cid = "6623",
      ctd_id = "C006780",
      exposome_explorer_id = "1418",
      parental_compound = "NA"
    ),
    etpa = list(
      full_name = "ethyl-paraben",
      short_name = "ETPA",
      class = "Phenols",
      smiles = "CCOC(=O)C1=CC=C(C=C1)O",
      pubchem_cid = "8434",
      ctd_id = "C012313",
      exposome_explorer_id = "1422",
      parental_compound = "NA"
    ),
    mepa = list(
      full_name = "methyl-paraben",
      short_name = "MEPA",
      class = "Phenols",
      smiles = "COC(=O)C1=CC=C(C=C1)O",
      pubchem_cid = "7456",
      ctd_id = "C015358",
      exposome_explorer_id = "1421",
      parental_compound = "NA"
    ),
    bupa = list(
      full_name = "n‑butyl‑paraben",
      short_name = "BUPA",
      class = "Phenols",
      smiles = "CCCCOC(=O)C1=CC=C(C=C1)O",
      pubchem_cid = "7184",
      ctd_id = "C038091",
      exposome_explorer_id = "1424",
      parental_compound = "NA"
    ),
    oxbe = list(
      full_name = "oxybenzone",
      short_name = "OXBE",
      class = "Phenols",
      smiles = "COC1=CC(=C(C=C1)C(=O)C2=CC=CC=C2)O",
      pubchem_cid = "4632",
      ctd_id = "C005290",
      exposome_explorer_id = "1419",
      parental_compound = "NA"
    ),
    prpa = list(
      full_name = "propyl-paraben",
      short_name = "PRPA",
      class = "Phenols",
      smiles = "CCCOC(=O)C1=CC=C(C=C1)O",
      pubchem_cid = "7175",
      ctd_id = "C006068",
      exposome_explorer_id = "1423",
      parental_compound = "NA"
    ),
    trcs = list(
      full_name = "triclosan",
      short_name = "TRCS",
      class = "Phenols",
      smiles = "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl",
      pubchem_cid = "5564",
      ctd_id = "D014260",
      exposome_explorer_id = "1420",
      parental_compound = "NA"
    ),
    mbzp = list(
      full_name = "mono benzyl phthalate",
      short_name = "MBzP",
      class = "Phthalate metabolites",
      smiles = "C1=CC=C(C=C1)COC(=O)C2=CC=CC=C2C(=O)O",
      pubchem_cid = "31736",
      ctd_id = "C103325",
      exposome_explorer_id = "1397",
      parental_compound = "BBzP"
    ),
    mecpp = list(
      full_name = "mono‑2‑ethyl 5‑carboxypentyl phthalate",
      short_name = "MECPP",
      class = "Phthalate metabolites",
      smiles = "CCC(CCCC(=O)O)COC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "148386",
      ctd_id = "C051450",
      exposome_explorer_id = "1403",
      parental_compound = "DEHP"
    ),
    mehhp = list(
      full_name = "mono‑2‑ethyl‑5‑hydroxyhexyl phthalate",
      short_name = "MEHHP",
      class = "Phthalate metabolites",
      smiles = "CCC(CCC(C)O)COC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "170295",
      ctd_id = "C479069",
      exposome_explorer_id = "1402",
      parental_compound = "DEHP"
    ),
    meohp = list(
      full_name = "mono‑2‑ethyl‑5‑oxohexyl phthalate",
      short_name = "MEOHP",
      class = "Phthalate metabolites",
      smiles = "CCC(CCC(=O)C)COC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "119096",
      ctd_id = "C080276",
      exposome_explorer_id = "1401",
      parental_compound = "DEHP"
    ),
    mehp = list(
      full_name = "mono‑2‑ethylhexyl phthalate",
      short_name = "MEHP",
      class = "Phthalate metabolites",
      smiles = "CCCCC(CC)COC(=O)C1=CC=CC=C1C(=O)[O-]",
      pubchem_cid = "21924291",
      ctd_id = "C016599",
      exposome_explorer_id = "Unsure",
      parental_compound = "DEHP"
    ),
    ohminp = list(
      full_name = "mono‑4‑methyl‑7‑hydroxyoctyl phthalate",
      short_name = "oh-MiNP",
      class = "Phthalate metabolites",
      smiles = "CC(O)CCC(C)CCCOC(=O)C1=CC=CC=C1C(O)=O",
      pubchem_cid = "102401880",
      ctd_id = "NA",
      exposome_explorer_id = "1451",
      parental_compound = "MiNP"
    ),
    oxominp = list(
      full_name = "mono‑4‑methyl‑7‑oxooctyl phthalate",
      short_name = "oxo-MiNP",
      class = "Phthalate metabolites",
      smiles = "CC(CCCOC(=O)C1=CC=CC=C1C(O)=O)CCC(C)=O",
      pubchem_cid = "102401881",
      ctd_id = "NA",
      exposome_explorer_id = "1492",
      parental_compound = "MiNP"
    ),
    mibp = list(
      full_name = "mono‑iso‑butyl phthalate",
      short_name = "MiBP",
      class = "Phthalate metabolites",
      smiles = "CC(C)COC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "92272",
      ctd_id = "C575690",
      exposome_explorer_id = "1399",
      parental_compound = "DiBP"
    ),
    mnbp = list(
      full_name = "mono‑n‑butyl phthalate",
      short_name = "MnBP",
      class = "Phthalate metabolites",
      smiles = "CCCCOC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "8575",
      ctd_id = "C028577",
      exposome_explorer_id = "1398",
      parental_compound = "DnBP"
    ),
    mep = list(
      full_name = "monoethyl phthalate",
      short_name = "MEP",
      class = "Phthalate metabolites",
      smiles = "CCOC(=O)C1=CC=CC=C1C(=O)O",
      pubchem_cid = "75318",
      ctd_id = "C581825",
      exposome_explorer_id = "1396",
      parental_compound = "DEP"
    )
  )

  edcs_info_df <- stack(edcs_info)
  edcs_info_df$col_names <- .names
  edcs_info_df <- tidylog::pivot_wider(edcs_info_df,
                                       names_from = col_names,
                                       values_from = values
  )
  edcs_info_df <- tidylog::rename(edcs_info_df,
                                  chem_id = ind
  )

  return(edcs_info_df)
}
################################################################################
