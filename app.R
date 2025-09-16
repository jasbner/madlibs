library(shiny)
library(bslib)
library(shinyvalidate)
library(glue)

generate_story <- function(noun, verb, adjective, adverb) {
  glue::glue(
    "Once upon a time, there was a {adjective} {noun} who loved to
    {verb} {adverb}. It was the funniest thing ever!"
  )
}

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#6c5ce7",
    secondary = "#00b894",
    success = "#00cec9",
    info = "#74b9ff",
    warning = "#fdcb6e",
    danger = "#d63031",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),

  title = "Mad Libs Game",

  sidebar = sidebar(
    title = "Fill in the blanks:",
    width = 350,

    card(
      card_header(
        class = "bg-primary text-white",
        "Word Inputs"
      ),
      card_body(
        textInput(
          "noun1",
          "Enter a noun:",
          placeholder = "e.g., cat, pizza, unicorn"
        ),
        textInput(
          "verb",
          "Enter a verb:",
          placeholder = "e.g., dance, jump, sing"
        ),
        textInput(
          "adjective",
          "Enter an adjective:",
          placeholder = "e.g., silly, purple, gigantic"
        ),
        textInput(
          "adverb",
          "Enter an adverb:",
          placeholder = "e.g., quickly, happily, mysteriously"
        ),
        br(),
        div(
          class = "alert alert-info",
          icon("lightbulb"),
          " Your story updates as you type!"
        )
      )
    )
  ),

  layout_columns(
    col_widths = c(12),
    card(
      card_header(
        class = "bg-gradient text-white",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);",
        h3("Your Mad Libs Story:", class = "m-0")
      ),
      card_body(
        div(
          id = "story-container",
          style = "min-height: 150px; font-size: 1.2em; line-height: 1.6;",
          uiOutput("story")
        )
      )
    ),

    card(
      card_header("Tips for Fun Mad Libs"),
      card_body(
        tags$ul(
          tags$li("A noun is a person, place, or thing"),
          tags$li("A verb is an action word"),
          tags$li("An adjective describes a noun"),
          tags$li(
            "An adverb describes how something is done (often ends in -ly)"
          )
        )
      )
    )
  ),

  tags$head(
    tags$style(HTML(
      "
      .card {
        transition: all 0.3s ease;
        margin-bottom: 20px;
      }
      .card:hover {
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      #story-container {
        padding: 20px;
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        border-radius: 10px;
        color: #2c3e50;
      }
      .invalid-feedback {
        color: #d63031;
        font-size: 0.875em;
        margin-top: 0.25rem;
      }
    "
    ))
  )
)

server <- function(input, output, session) {
  # Initialize validation rules
  iv <- InputValidator$new()

  # Add validation rules for each input
  iv$add_rule("noun1", sv_required())
  iv$add_rule(
    "noun1",
    ~ {
      if (length(strsplit(., " ")[[1]]) > 2) {
        "Please enter just one noun (1-2 words max)"
      }
    }
  )

  iv$add_rule("verb", sv_required())
  iv$add_rule(
    "verb",
    ~ {
      if (length(strsplit(., " ")[[1]]) > 2) {
        "Please enter just one verb (1-2 words max)"
      }
    }
  )

  iv$add_rule("adjective", sv_required())
  iv$add_rule(
    "adjective",
    ~ {
      if (length(strsplit(., " ")[[1]]) > 2) {
        "Please enter just one adjective (1-2 words max)"
      }
    }
  )

  iv$add_rule("adverb", sv_required())
  iv$add_rule(
    "adverb",
    ~ {
      if (length(strsplit(., " ")[[1]]) > 1) {
        "Please enter just one adverb (1 word)"
      }
    }
  )

  # Enable validation
  iv$enable()

  # Create reactive story that updates live
  output$story <- renderUI({
    # Check if all inputs are valid
    if (!iv$is_valid()) {
      return(
        div(
          class = "text-muted",
          style = "font-style: italic;",
          icon("pencil"),
          " Fill in all the blanks above to see your story..."
        )
      )
    }

    # Generate and display the story
    story_text <- generate_story(
      input$noun1,
      input$verb,
      input$adjective,
      input$adverb
    )

    div(
      class = "animated-story",
      style = "animation: fadeIn 0.5s;",
      HTML(
        paste0(
          "<p style='font-size: 1.1em;'>",
          icon("book-open", class = "text-primary"),
          " ",
          story_text,
          "</p>"
        )
      ),
      tags$style(
        "
        @keyframes fadeIn {
          from { opacity: 0; }
          to { opacity: 1; }
        }
      "
      )
    )
  })
}

shinyApp(ui = ui, server = server)
