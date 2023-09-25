#' Generate project object (class `list`).
#'
#' A project object defines all the project related variables, like table names
#' and encryption methods.
#'
#' @param ... Any parameter and value to be updated in the default project list.
#' @return A list with:
#'  Connections:
#'  -----------
#'  - conx: A RODBC (DB2 connection) object.
#'  - ConceptLibrary: A list with:
#'    - conx: An HttpClient connection object to the concept library
#'      (\href{`https://conceptlibrary.saildatabank.com'}).
#'
#'  - number: Project number
#'  - schemaV: Name of the project's V schema.
#'  - schemaW: Name of the project's W schema.
#'  - encryption: 'E' or 'PE' (default).
#'  - echo: If TRUE, executed SQL queries will be printed.
#'  - table_names: A list with the name of source table_names. Includes: ADDE_DEATHS,
#'    EDDS_EDDS, OPDW_OUTPATIENTS, OPDW_OUTPATIENTS_DIAG, OPDW_OUTPATIENTS_OPER,
#'    PEDW_DIAG, PEDW_EPISODE, PEDW_OPER, PEDW_SPELL, PEDW_SUPERSPELL,
#'    WDSD_AR_PERS, WDSD_AR_PERS_ADD, WDSD_AR_PERS_GP,
#'    WLGP_GP_REG, WLGP_GP_EVENT, WLGP_PATIENT_ALF. The function will try to
#'    automatically identified these table_names in the V schema, but this is not
#'    warrantied to work. The list also includes table ALF which, if not "NA",
#'    it represents your study population. It can contain index dates and other
#'    parameters to be used during filtering through joining (see
#'    `filter.reference`).
#'
#' @seealso prj.set_up, filter.reference
#'
#' @author Marcos del Pozo Banos
#' @export
#'
prj.init <- function(number, ...){

  # Get SAIL log in details
  details = SAILDBUtils::getLogin(userName = Sys.info()["user"])

  # Allocate the project object
  prj = list(

    # SAIL project number
    number = number,

    # A connection object with SAIL Databank
    conx = SAILDBUtils::SAILConnect(username = details[[1]],
                                    password = details[[2]]),

    # A connection object with the Concept Library
    ConceptLibrary = list(
      'conx' = ConceptLibraryClient::connect_to_API(user = details[[1]],
                                                    password = details[[2]],
                                                    # url="https://phenotypes.healthdatagateway.org",
                                                    url="http://conceptlibrary.serp.ac.uk",
                                                    public = FALSE)
    ),

    # Tables
    tables = data.frame(
      # Full name
      full_name = c(
        NA,
        NA,
        NA, NA, NA,
        NA, NA, NA, NA, NA,
        NA, NA,
        NA, NA, NA,

        # New tables
        NA, NA, NA
      ),
      # Nick name
      nickname = c(
        "adde",
        "edds",
        "opdw_op", "opdw_diag", "opdw_oper",
        "pedw_diag", "pedw_epi", "pedw_oper", "pedw_spell", "pedw_supspell",
        "wdsd_pers", "wdsd_add",
        "wlgp_reg", "wlgp_ev", "wlgp_alf",

        # New tables
        "pedw_admission", "pedw_s_diag", "pedw_s_oper"
      ),
      # Common name
      row.names = c(
        "ADDE_DEATHS",
        "EDDS_EDDS",
        "OPDW_OUTPATIENTS", "OPDW_OUTPATIENTS_DIAG", "OPDW_OUTPATIENTS_OPER",
        "PEDW_DIAG", "PEDW_EPISODE", "PEDW_OPER", "PEDW_SPELL", "PEDW_SUPERSPELL",
        "WDSD_AR_PERS", "WDSD_CLEAN_ADD_WALES",
        "WLGP_CLEAN_GP_REG", "WLGP_GP_EVENT", "WLGP_PATIENT_ALF",

        # New tables
        "PEDW_ADMISSIONS", "PEDW_SINGLE_DIAG", "PEDW_SINGLE_OPER"
      )
    ),

    # A data.frame containing details of pre-defined variables in the project.
    vars = data.frame(
      description = c(
        "Name of the view schema",
        "Name of the write schema",
        "Project encryption (PE|E)",
        "Week of birth",
        "Age at event",
        "Age at start of event",
        "Age at end of event",
        "Sex",
        "Date of death",
        "Age at death",
        "Code specifying linkage strength from -1 (weakes) to 3 (strongest)",
        "Anonymous Linkage Field",
        "[TBD] Full name of the calling table (e.g., the WLGP_GP_EVENT table if calling `tab.WLGP_GP_EVENT.events`)",
        "[TBD] Nickname of the calling table (e.g., the WLGP_GP_EVENT table if calling `tab.WLGP_GP_EVENT.events`)",
        "[TBD] Common name of the calling table (e.g., 'WLGP_GP_EVENT' if calling `tab.WLGP_GP_EVENT.events`)",
        "[TBD] Date of the event",
        "[TBD] Start date of the event",
        "[TBD] End date of the event",
        "[TBD] Event key",
        "[TBD] Event code (e.g., Read codes in WLGP_GP_EVENT table)",
        "[TBD] A list of code strings (e.g., Read codes) separated by comma",
        "Query filtering codes",
        "[TBD] Additional event-related information"
        ),
      definition = c(
        paste('SAIL', number, 'V', sep=''),
        paste('SAILW', number, 'V', sep=''),
        "PE",
        "wdsd_pers.WOB",
        "years_between(@v{DT}, @v{WOB})",
        "years_between(@v{START_DT}, @v{WOB})",
        "years_between(@v{END_DT}, @v{WOB})",
        "case wdsd_pers.GNDR_CD when 1 then 'M' when 2 then 'F' else 'U' end",
        "adde.DEATH_DT",
        "years_between(@v{DOD}, @v{WOB})",
        " case @v{tab}.ALF_STS_CD
            when  '1' then 3  -- Deterministic
            when  '2' then 3  -- Deterministic
            when  '4' then 3  -- Deterministic
            when '39' then 2  -- Probabilistic w. score > 0.9
            when '35' then 1  -- Probabilistic w. score > 0.5 but < 0.9
            when '99' then 0  -- Probabilistic w. score < 0.5
            else -1
          end",
        "@v{tab}.ALF_@v{enc}",
        paste0("[TBD] @T{<TABLE>}"),
        paste0("[TBD] @t{<TABLE>}"),
        paste0("[TBD] '<TABLE>'"),
        "[TBD] @v{tab}.event_dt",
        "[TBD] @v{tab}.event_start_dt",
        "[TBD] @v{tab}.event_end_dt",
        "[TBD] ...",
        "[TBD] @v{tab}.event_code",
        "[TBD] ...",
        "@v{CODE} in (@v{code_list})",
        "''"
        ),
      row.names = c(
        "schemaV",
        "schemaW",
        "enc",
        "WOB",
        "AGE",
        "START_AGE",
        "END_AGE",
        "SEX",
        "DOD",
        "DOD_AGE",
        "LNK_STS_CD",
        "ALF",
        "TAB",
        "tab",
        "SRC",
        "DT",
        "START_DT",
        "END_DT",
        "KEY",
        "CODE",
        "code_list",
        "CODE_FILTER",
        "NOTES"
        )
      )
  );

  # Automatically identify project views from the project's view schema. When
  # multiple versions of the same table exist, use the most recent one (this
  # assumes that the latest one is the last one when ordered)
  views = SAILDBUtils::runSQL(
    prj$conx,
    query = GetoptLong::qq("select * from SYSCAT.VIEWS where VIEWSCHEMA = '@{prj$vars['schemaV', 'definition']}'"))
  for (tab in row.names(prj$tables)){
    x = sort(views$VIEWNAME[grepl(paste0(tab, "(_CLEANSED|_MEDIAN)?(_[0-9*])?([^_a-zA-Z])"), views$VIEWNAME)], decreasing = TRUE);
    if (length(x) > 0){
      prj$tables[tab, "full_name"] = paste(prj$vars['schemaV', 'definition'], as.character(x[[1]]), sep=".");
    }
  }

  # Additional arguments passed as input
  prj = utils.prj_update(prj, list(...));

  # Make it of type VELA.prj
  class(prj) <- "VELA.prj"

  return(prj)
}


#' Check if variable is of type VELA.prj
#'
#' @seealso prj.init
#'
#' @author Marcos del Pozo Banos
#' @export
#'
is.VELA.prj <- function(x){
  return(inherits(x, "VELA.prj"))
}
