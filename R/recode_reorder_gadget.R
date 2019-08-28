#' @importFrom magrittr %>%
recode_reorder <- function() {

  # get data.frame objects from GlobalEnv
  obj <- ls(envir = .GlobalEnv)
  s_obj <- rlang::syms(obj)
  names(s_obj) <- obj

  df_objects <-
    purrr::map_df(s_obj, ~{is.data.frame(eval(.x))}) %>%
    tidyr::gather("object", "is_df") %>%
    dplyr::filter(is_df)

  # UI ----
  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("pre {
                    border-radius: 0px;
                    border-width: 0px;
                    font-size: 14px;
                    }")
      ),
      shiny::tags$style(
        shiny::HTML(".form-group {
                    margin-bottom: 0px;
                    }")
      ),
      shiny::tags$style(
        shiny::HTML(".form-control {
                    border-radius: 0px;
                    box-shadow: inset 0 0px 0px
                    }")
      ),
      shiny::tags$style(
        shiny::HTML(".selectize-input {
                    border-radius: 0px;
                    }")
      )
    ),

    miniUI::gadgetTitleBar("Recode & Reorder", right = NULL),

    miniUI::miniContentPanel(padding = 15, scrollable = TRUE,
                             shiny::validate(
                               shiny::need(nrow(df_objects) > 0, message = "No data.frame objects in Global Environment.")
                             ),
                             shiny::fillCol(flex = c(1,5),
                                            shiny::fillRow(
                                              shiny::selectInput(inputId = "inp_select_data",
                                                                 label = "Select data",
                                                                 choices = df_objects[["object"]]),
                                              shiny::uiOutput("select_variables")
                                            ),
                                            shiny::fillRow(
                                              shiny::uiOutput("create_level_outputs"),
                                              shiny::verbatimTextOutput("txt_code")
                                            )
                             )
    ),

    miniUI::miniButtonBlock(
      shiny::actionButton(inputId = "inp_add_code",
                          label = "INSERT CODE",
                          class="btn btn-primary")
    )
  )

  # SERVER ----
  server <- function(input, output) {

    # UI - select_variables ----
    output$select_variables <- shiny::renderUI({
      inp_data <- shiny::req(input$inp_select_data)
      s_data <- rlang::sym(inp_data)

      clnm <- colnames(eval(s_data))

      shiny::selectInput(inputId = "inp_select_var",
                         label = "Select variable",
                         choices = clnm)
    })

    # R - get_factor_levels ----
    get_factor_levels <- shiny::reactive({
      inp_data <- shiny::isolate(shiny::req(input$inp_select_data))
      inp_vars <- shiny::req(input$inp_select_var)

      s_data <- rlang::sym(inp_data)

      eval(s_data)[[inp_vars]] %>%
        as.factor() %>%
        levels()
    })

    # Observe ----
    shiny::observe({
      factor_levels <- get_factor_levels()
      ids <- 1:length(factor_levels)
      fct_id <- paste0("fct_", ids)
      inp_id <- paste0("inp_", ids)

      lapply(ids, function(i) {
        output[[fct_id[i]]] <- shiny::renderUI({
          tagList(
            shiny::tags$div(style="display:inline-block;",
                            shiny::textInput(inputId = inp_id[i],
                                             label = " ",
                                             value = factor_levels[i],
                                             width = "100%")),
            shiny::tags$div(style="display:inline-block", shiny::icon("equals"), factor_levels[i])
          )

        })
      })
    })

    # UI - create_level_outputs ----
    output$create_level_outputs <- shiny::renderUI({
      factor_levels <- get_factor_levels()
      ids <- 1:length(factor_levels)

      shinyjqui::jqui_sortable(
        shiny::tags$div(id = 'nivoi',
                        lapply(ids, function(i) {
                          shiny::uiOutput(paste0('fct_', i))
                        })
        )
      )
    })

    # R - get_fct_df ----
    get_fct_df <- shiny::reactive({
      da <- shiny::req(input$nivoi_order)
      factor_levels <- get_factor_levels()

      ids <- 1:length(factor_levels)
      fct_id <- paste0("fct_", ids)
      inp_id <- paste0("inp_", ids)

      fct_id_levels <-
        dplyr::tibble(
          fct_id = fct_id,
          orig_lvls = factor_levels
        )

      da %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(inp_id = stringr::str_replace(id, "fct_", "inp_")) %>%
        dplyr::left_join(fct_id_levels, by = c("id" = "fct_id"))
    })

    # R - get_fct_df_new ----
    get_fct_df_new <- shiny::reactive({
      fct_df <- get_fct_df()

      fct_df %>%
        dplyr::mutate(new_lvls = purrr::map_chr(inp_id, ~{shiny::req(input[[.x]])})) %>%
        dplyr::group_by(new_lvls) %>%
        dplyr::mutate(
          collapse_part =
            paste('"', orig_lvls, '"', sep = "", collapse = ", ")
        ) %>%
        dplyr::ungroup()

    })

    # R - get_collapse ----
    get_collapse <- shiny::reactive({
      fct_df <- get_fct_df_new()
      fac_var <- shiny::req(input$inp_select_var)
      space <- paste0(rep(" ", 20 + nchar(fac_var)), collapse = "")

      collapse_vec <-
        fct_df %>%
        dplyr::distinct(new_lvls, collapse_part) %>%
        stringr::str_glue_data('{space}`{new_lvls}` = c({collapse_part})') %>%
        paste(collapse = ",\n")

      collapse_vec <- paste("fct_collapse(", fac_var, ",\n", collapse_vec, ")",
                            sep = "", collapse = "")
      return(collapse_vec)
    })

    # R - get_relevel ----
    get_relevel <- shiny::reactive({
      fct_df <- get_fct_df_new()
      fac_var <- shiny::req(input$inp_select_var)
      space <- paste0(rep(" ", 19 + nchar(fac_var)), collapse = "")

      relevel_vec <-
        fct_df %>%
        dplyr::distinct(new_lvls) %>%
        dplyr::pull(new_lvls)
      relevel_vec <- paste(space, '"', relevel_vec, '"', sep = "", collapse = ",\n")
      relevel_vec <- paste("fct_relevel(", fac_var, ",\n", relevel_vec, ")",
                           sep = "", collapse = "")
      return(relevel_vec)
    })

    # R - get_collapse_relevel ----
    get_collapse_relevel <- shiny::reactive({
      relevel_fct <- get_relevel()
      collapse_fct <- get_collapse()
      inp_data <- shiny::isolate(shiny::req(input$inp_select_data))
      inp_vars <- shiny::req(input$inp_select_var)

      paste0(inp_data, " <- ", inp_data, " %>% \n", "  dplyr::mutate(\n",
             "    ", inp_vars, " = as.factor(", inp_vars, "),\n",
             "    ", inp_vars, " = ", collapse_fct, ",\n",
             "    ", inp_vars, " = ", relevel_fct,  ")\n\n")
    })

    # Print - txt_code ----
    output$txt_code <- shiny::renderPrint({
      cat(
        get_collapse_relevel()
      )
    })

    observeEvent(input$inp_add_code, {
      recreo_txt <- get_collapse_relevel()

      doc_info <- rstudioapi::getActiveDocumentContext()
      loc_row <- doc_info$selection[[1]]$range[[1]][[1]]

      rstudioapi::insertText(location = c(loc_row, 1),
                             text = recreo_txt,
                             id = doc_info$id)
    })

    observeEvent(input$done, {
      recreo_txt <- get_collapse_relevel()

      doc_info <- rstudioapi::getActiveDocumentContext()
      loc_row <- doc_info$selection[[1]]$range[[1]][[1]]

      rstudioapi::insertText(location = c(loc_row, 1),
                             text = recreo_txt,
                             id = doc_info$id)
      stopApp()
    })



  }

  # runGadget ----
  shiny::runGadget(ui, server,
                   viewer = shiny::dialogViewer("Recode & Reorder",
                                                width = 1100,
                                                height = 800)
  )
}
