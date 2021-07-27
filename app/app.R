library(shiny)
library(shinydashboard)
import::from(magrittr, "%>%")

dados <- readr::read_rds("data/base_final.rds")

# Internações SRAG
mod_int_srag <- readr::read_rds("data/modelo_internacoes_srag.rds")
ef_int_srag <-  glmmTMB::fixef(mod_int_srag)$cond %>%
  dplyr::bind_rows() %>%
  janitor::clean_names()

# Internações Covid
mod_int_covid <- readr::read_rds("data/modelo_internacoes_covid.rds")
ef_int_covid <-  glmmTMB::fixef(mod_int_covid)$cond %>%
  dplyr::bind_rows() %>%
  janitor::clean_names()

# Óbitos Covid
mod_ob_covid <- readr::read_rds("data/modelo_obitos_covid.rds")
ef_ob_covid <-  glmmTMB::fixef(mod_ob_covid)$cond %>%
  dplyr::bind_rows() %>%
  janitor::clean_names()

da_clustered <- readr::read_rds("data/da_clustered.rds")

muni_vulneraveis <- da_clustered %>%
  dplyr::filter(cluster == 4) %>%
  dplyr::pull(code_muni)
muni_vulneraveis <- dados %>%
  dplyr::filter(code_muni %in% muni_vulneraveis)

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "green",

  # title ----
  dashboardHeader(title = "Covid e Poluição"),
  # sidebar ----
  dashboardSidebar(sidebarMenu(
    id = "sidebarid",
    menuItem("Internações", tabName = "page1"),
    menuItem("Óbitos", tabName = "page2"),
    shinyWidgets::pickerInput(
      "mes", "Mês",
      choices = 1:12,
      selected = 7:10,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        container = "body",
        width = "100%",
        style = "btn-light text-dark",
        selectAllText = "Todos",
        deselectAllText = "Nenhum",
        selectedTextFormat = "count > 1",
        dropupAuto = FALSE
      )
    ),
    shinyWidgets::pickerInput(
      "uf", "UF",
      choices = sort(unique(dados$uf)),
      selected = sort(unique(dados$uf)),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        container = "body",
        width = "100%",
        style = "btn-light text-dark",
        selectAllText = "Todos",
        deselectAllText = "Nenhum",
        selectedTextFormat = "count > 1",
        dropupAuto = FALSE
      )
    )
    # conditionalPanel(
    #   'input.sidebarid == "page2"',
    # )
  )),

    # body ----
  dashboardBody(tabItems(

    # internações ----
    tabItem(
      tabName = "page1",
      fluidRow(
        shinydashboard::tabBox(
          width = 12,
          id = "internacoes",
          title = "Efeito de dias acima de 25 por mês em internações",

          shiny::tabPanel(
            "Geral e UF",
            div(
              h4("Geral"),
              reactable::reactableOutput("ef_pol_geral")
            ),
            div(
              h4("Por UF"),
              reactable::reactableOutput("ef_pol_uf"),
              downloadButton("download_uf", "Download")
            )
          ),
          shiny::tabPanel(
            "Municípios vulneráveis",
            reactable::reactableOutput("ef_pol_vulneraveis"),
          ),
          shiny::tabPanel(
            "Todos os municípios",
            reactable::reactableOutput("ef_pol_municipios"),
            downloadButton("download", "Download")
          )
        )
      )
    ),

    # óbitos ----
    tabItem(
      tabName = "page2",
      fluidRow(
        shinydashboard::tabBox(
          width = 12,
          id = "obitos",
          title = "Efeito de dias acima de 25 por mês em óbitos",

          shiny::tabPanel(
            "Geral e UF",
            div(
              h4("Geral"),
              reactable::reactableOutput("ef_pol_geral_obitos")
            ),
            div(
              h4("Por UF"),
              reactable::reactableOutput("ef_pol_uf_obitos")
            )
          ),
          shiny::tabPanel(
            "Municípios vulneráveis",
            reactable::reactableOutput("ef_pol_vulneraveis_obitos"),
          ),
          shiny::tabPanel(
            "Todos os municípios",
            reactable::reactableOutput("ef_pol_municipios_obitos"),
            downloadButton("download_obitos", "Download")
          )
        )
      )
    )

  ))
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {

  da_uf <- reactive({
    dados %>%
      dplyr::filter(mes %in% input$mes) %>%
      dplyr::group_by(uf) %>%
      dplyr::summarise(
        media_dias = round(mean(dias_acima_25), 2)
      )
  })

  da_vulneraveis <- reactive({
    muni_vulneraveis %>%
      dplyr::filter(mes %in% input$mes) %>%
      dplyr::group_by(code_muni) %>%
      dplyr::summarise(
        dias_acima_25 = round(mean(dias_acima_25), 2)
      ) %>%
      dplyr::inner_join(dplyr::select(da_clustered, code_muni, muni_nm, uf), "code_muni")
  })

  output$ef_pol_geral <- reactable::renderReactable({
    dados %>%
      dplyr::filter(mes %in% input$mes, uf %in% input$uf) %>%
      dplyr::summarise(media_dias = round(mean(dias_acima_25), 2)) %>%
      dplyr::mutate(
        pct_int_srag = exp(ef_int_srag$dias_acima_25)^media_dias - 1,
        pct_int_srag = scales::percent(pct_int_srag, .1),
        pct_int_covid = exp(ef_int_covid$dias_acima_25)^media_dias - 1,
        pct_int_covid = scales::percent(pct_int_covid, .1)
      ) %>%
      reactable::reactable(
        columns = list(
          media_dias = reactable::colDef("Média de dias acima de 25"),
          pct_int_srag = reactable::colDef("% a mais de internações SRAG"),
          pct_int_covid = reactable::colDef("% a mais de internações Covid")
        )
      )
  })
  output$ef_pol_geral_obitos <- reactable::renderReactable({
    dados %>%
      dplyr::filter(mes %in% input$mes, uf %in% input$uf) %>%
      dplyr::summarise(media_dias = round(mean(dias_acima_25), 2)) %>%
      dplyr::mutate(
        pct_ob_covid = exp(ef_ob_covid$dias_acima_25)^media_dias - 1,
        pct_ob_covid = scales::percent(pct_ob_covid, .1)
      ) %>%
      reactable::reactable(
        columns = list(
          media_dias = reactable::colDef("Média de dias acima de 25"),
          pct_ob_covid = reactable::colDef("% a mais de óbitos Covid")
        )
      )
  })

  output$ef_pol_uf <- reactable::renderReactable({
    da_uf() %>%
      dplyr::mutate(
        pct_int_srag = exp(ef_int_srag$dias_acima_25)^media_dias - 1,
        pct_int_srag = scales::percent(pct_int_srag, .1),
        pct_int_covid = exp(ef_int_covid$dias_acima_25)^media_dias - 1,
        pct_int_covid = scales::percent(pct_int_covid, .1)
      ) %>%
      dplyr::arrange(dplyr::desc(media_dias)) %>%
      reactable::reactable(
        columns = list(
          uf = reactable::colDef("UF"),
          media_dias = reactable::colDef("Média de dias acima de 25"),
          pct_int_srag = reactable::colDef("% a mais de internações SRAG"),
          pct_int_covid = reactable::colDef("% a mais de internações Covid")
        )
      )
  })
  output$ef_pol_uf_obitos <- reactable::renderReactable({
    da_uf() %>%
      dplyr::mutate(
        pct_ob_covid = exp(ef_ob_covid$dias_acima_25)^media_dias - 1,
        pct_ob_covid = scales::percent(pct_ob_covid, .1)
      ) %>%
      dplyr::arrange(dplyr::desc(media_dias)) %>%
      reactable::reactable(
        columns = list(
          uf = reactable::colDef("UF"),
          media_dias = reactable::colDef("Média de dias acima de 25"),
          pct_ob_covid = reactable::colDef("% a mais de óbitos Covid")
        )
      )
  })

  output$ef_pol_vulneraveis <- reactable::renderReactable({
    da_vulneraveis() %>%
      dplyr::mutate(
        pct_int_srag = exp(ef_int_srag$dias_acima_25)^dias_acima_25 - 1,
        pct_int_srag = scales::percent(pct_int_srag, .1),
        pct_int_covid = exp(ef_int_covid$dias_acima_25)^dias_acima_25 - 1,
        pct_int_covid = scales::percent(pct_int_covid, .1)
      ) %>%
      dplyr::relocate(dias_acima_25, .before = pct_int_srag) %>%
      dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
      reactable::reactable(
        columns = list(
          code_muni = reactable::colDef("Código IBGE"),
          muni_nm = reactable::colDef("Município"),
          dias_acima_25 = reactable::colDef("Média de dias acima de 25 (jul-out)"),
          pct_int_srag = reactable::colDef("% a mais de internações SRAG"),
          pct_int_covid = reactable::colDef("% a mais de internações Covid")
        )
      )
  })
  output$ef_pol_vulneraveis_obitos <- reactable::renderReactable({
    da_vulneraveis() %>%
      dplyr::mutate(
        pct_ob_covid = exp(ef_ob_covid$dias_acima_25)^dias_acima_25 - 1,
        pct_ob_covid = scales::percent(pct_ob_covid, .1)
      ) %>%
      dplyr::relocate(dias_acima_25, .before = pct_ob_covid) %>%
      dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
      reactable::reactable(
        columns = list(
          code_muni = reactable::colDef("Código IBGE"),
          muni_nm = reactable::colDef("Município"),
          dias_acima_25 = reactable::colDef("Média de dias acima de 25 (jul-out)"),
          pct_ob_covid = reactable::colDef("% a mais de óbitos Covid")
        )
      )
  })

  da_municipios <- reactive({
    dados %>%
      dplyr::filter(mes %in% input$mes, uf %in% input$uf) %>%
      dplyr::group_by(code_muni) %>%
      dplyr::summarise(
        dias_acima_25 = round(mean(dias_acima_25), 2)
      ) %>%
      dplyr::inner_join(dplyr::select(da_clustered, code_muni, muni_nm, uf), "code_muni")
  })

  output$ef_pol_municipios <- reactable::renderReactable({
    da_municipios() %>%
      dplyr::mutate(
        pct_int_srag = exp(ef_int_srag$dias_acima_25)^dias_acima_25 - 1,
        pct_int_srag = scales::percent(pct_int_srag, .1),
        pct_int_covid = exp(ef_int_covid$dias_acima_25)^dias_acima_25 - 1,
        pct_int_covid = scales::percent(pct_int_covid, .1)
      ) %>%
      dplyr::relocate(dias_acima_25, .before = pct_int_srag) %>%
      dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
      reactable::reactable(
        columns = list(
          code_muni = reactable::colDef("Código IBGE"),
          muni_nm = reactable::colDef("Município"),
          uf = reactable::colDef("UF"),
          dias_acima_25 = reactable::colDef("Média de dias acima de 25 (jul-out)"),
          pct_int_srag = reactable::colDef("% a mais de internações SRAG"),
          pct_int_covid = reactable::colDef("% a mais de internações Covid")
        ),
        filterable = TRUE
      )
  })
  output$ef_pol_municipios_obitos <- reactable::renderReactable({
    da_municipios() %>%
      dplyr::mutate(
        pct_ob_covid = exp(ef_ob_covid$dias_acima_25)^dias_acima_25 - 1,
        pct_ob_covid = scales::percent(pct_ob_covid, .1)
      ) %>%
      dplyr::relocate(dias_acima_25, .before = pct_ob_covid) %>%
      dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
      reactable::reactable(
        columns = list(
          code_muni = reactable::colDef("Código IBGE"),
          muni_nm = reactable::colDef("Município"),
          uf = reactable::colDef("UF"),
          dias_acima_25 = reactable::colDef("Média de dias acima de 25 (jul-out)"),
          pct_ob_covid = reactable::colDef("% a mais de óbitos Covid")
        ),
        filterable = TRUE
      )
  })

  output$download <- downloadHandler(
    filename = "srag_covid_poluicao.csv",
    content = function(file) {
      da_municipios() %>%
        dplyr::mutate(
          pct_int_srag = exp(ef_int_srag$dias_acima_25)^dias_acima_25 - 1,
          pct_int_covid = exp(ef_int_covid$dias_acima_25)^dias_acima_25 - 1
        ) %>%
        dplyr::relocate(dias_acima_25, .before = pct_int_srag) %>%
        dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
        readr::write_csv(file)
    }
  )
  output$download_uf <- downloadHandler(
    filename = "srag_covid_poluicao_uf.csv",
    content = function(file) {
      da_uf() %>%
        dplyr::mutate(
          pct_int_srag = exp(ef_int_srag$dias_acima_25)^media_dias - 1,
          pct_int_covid = exp(ef_int_covid$dias_acima_25)^media_dias - 1
        ) %>%
        dplyr::arrange(dplyr::desc(media_dias)) %>%
        readr::write_csv(file)
    }
  )
  output$download_obitos <- downloadHandler(
    filename = "srag_covid_poluicao_obitos.csv",
    content = function(file) {
      da_municipios() %>%
        dplyr::mutate(
          pct_ob_covid = exp(ef_ob_covid$dias_acima_25)^dias_acima_25 - 1
        ) %>%
        dplyr::relocate(dias_acima_25, .before = pct_ob_covid) %>%
        dplyr::arrange(dplyr::desc(dias_acima_25)) %>%
        readr::write_csv(file)
    }
  )

}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)
