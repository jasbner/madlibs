library(shiny)
library(bslib)
library(shinyvalidate)
library(glue)

# Multiple story templates for variety
story_templates <- list(
  adventure = list(
    title = "The Great Adventure",
    template = "Once upon a time, a {adjective} explorer named Captain {noun1} decided to {verb} through the {noun2} forest. Moving {adverb}, they discovered a {adjective2} treasure chest filled with magical {noun3}. 'This is {adverb2} amazing!' they exclaimed, doing a little victory {verb2}."
  ),

  space = list(
    title = "Space Mission",
    template = "Commander {noun1} was the most {adjective} astronaut in the galaxy. Their mission was to {verb} to the {adjective2} planet of {noun2}. The spaceship moved {adverb} through space while {noun3} danced {adverb2} outside the window. When they landed, the aliens began to {verb2} in celebration!"
  ),

  kitchen = list(
    title = "Kitchen Chaos",
    template = "Chef {noun1} was preparing a {adjective} meal when disaster struck! The {noun2} began to {verb} {adverb} across the kitchen counter. '{adjective2} pasta!' shouted the chef, chasing after runaway {noun3} that were {verb2}ing {adverb2} around the room."
  ),

  school = list(
    title = "School Day Surprise",
    template = "At {adjective} Elementary School, student {noun1} had to {verb} {adverb} to class. Their backpack was full of {adjective2} {noun2}, and their favorite {noun3} was {verb2}ing {adverb2} in their locker. What a wonderfully weird day!"
  ),

  pet = list(
    title = "The Amazing Pet",
    template = "My pet {noun1} is absolutely {adjective}! Every morning, it likes to {verb} {adverb} around the {adjective2} yard. The neighbors always laugh when they see my {noun2} trying to {verb2} {adverb2} with the {noun3} next door."
  )
)

generate_story <- function(template_key, inputs) {
  template <- story_templates[[template_key]]

  # Replace placeholders with user inputs
  story_text <- glue::glue(
    template$template,
    noun1 = inputs$noun1 %||% "[noun]",
    noun2 = inputs$noun2 %||% "[noun]",
    noun3 = inputs$noun3 %||% "[noun]",
    verb = inputs$verb %||% "[verb]",
    verb2 = inputs$verb2 %||% "[verb]",
    adjective = inputs$adjective %||% "[adjective]",
    adjective2 = inputs$adjective2 %||% "[adjective]",
    adverb = inputs$adverb %||% "[adverb]",
    adverb2 = inputs$adverb2 %||% "[adverb]"
  )

  return(list(title = template$title, story = story_text))
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

  title = "Mad Libs Adventure Game",

  sidebar = sidebar(
    title = "Create Your Story:",
    width = 400,

    # Story template selector
    card(
      card_header(
        class = "bg-secondary text-white",
        "Choose Your Adventure"
      ),
      card_body(
        selectInput(
          "story_template",
          "Pick a story theme:",
          choices = list(
            "🏴‍☠️ The Great Adventure" = "adventure",
            "🚀 Space Mission" = "space",
            "👨‍🍳 Kitchen Chaos" = "kitchen",
            "🏫 School Day Surprise" = "school",
            "🐕 The Amazing Pet" = "pet"
          ),
          selected = "adventure"
        )
      )
    ),

    card(
      card_header(
        class = "bg-primary text-white",
        "Fill in the Blanks"
      ),
      card_body(
        # Nouns
        h5("Nouns (person, place, or thing):", class = "text-primary"),
        textInput(
          "noun1",
          "Name or thing #1:",
          placeholder = "e.g., Batman, pizza, rainbow"
        ),
        textInput(
          "noun2",
          "Place or thing #2:",
          placeholder = "e.g., kitchen, mountain, spaceship"
        ),
        textInput(
          "noun3",
          "Plural thing #3:",
          placeholder = "e.g., cookies, aliens, books"
        ),

        hr(),

        # Verbs
        h5("Verbs (action words):", class = "text-success"),
        textInput(
          "verb",
          "Action #1:",
          placeholder = "e.g., jump, fly, dance"
        ),
        textInput(
          "verb2",
          "Action #2:",
          placeholder = "e.g., sing, run, wiggle"
        ),

        hr(),

        # Adjectives
        h5("Adjectives (describing words):", class = "text-warning"),
        textInput(
          "adjective",
          "Description #1:",
          placeholder = "e.g., silly, enormous, sparkly"
        ),
        textInput(
          "adjective2",
          "Description #2:",
          placeholder = "e.g., mysterious, bright, fuzzy"
        ),

        hr(),

        # Adverbs
        h5("Adverbs (how something is done):", class = "text-info"),
        textInput(
          "adverb",
          "How #1:",
          placeholder = "e.g., quickly, carefully, loudly"
        ),
        textInput(
          "adverb2",
          "How #2:",
          placeholder = "e.g., gracefully, sneakily, wildly"
        ),

        br(),
        div(
          class = "alert alert-success",
          icon("magic"),
          " Your story updates automatically as you type!"
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
        h3(uiOutput("story_title"), class = "m-0")
      ),
      card_body(
        div(
          id = "story-container",
          style = "min-height: 200px; font-size: 1.3em; line-height: 1.8;",
          uiOutput("story")
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          class = "bg-info text-white",
          "📝 Quick Grammar Guide"
        ),
        card_body(
          tags$div(
            tags$strong("Noun:"),
            " Person, place, or thing",
            br(),
            tags$em("Examples: dog, teacher, Paris, happiness"),
            br(),
            br(),

            tags$strong("Verb:"),
            " Action or doing word",
            br(),
            tags$em("Examples: run, think, laugh, create"),
            br(),
            br(),

            tags$strong("Adjective:"),
            " Describes a noun",
            br(),
            tags$em("Examples: red, huge, funny, delicious"),
            br(),
            br(),

            tags$strong("Adverb:"),
            " Describes how something is done",
            br(),
            tags$em("Examples: quickly, quietly, beautifully, carefully")
          )
        )
      ),

      card(
        card_header(
          class = "bg-warning text-white",
          "💡 Pro Tips"
        ),
        card_body(
          tags$ul(
            tags$li("Be creative and silly - the funnier, the better!"),
            tags$li("Try unusual combinations for hilarious results"),
            tags$li("Most adverbs end in '-ly' but not all do"),
            tags$li(
              "Switch story themes to create completely different adventures"
            ),
            tags$li("Read your story out loud - it's even funnier that way!")
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
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        transform: translateY(-2px);
      }
      #story-container {
        padding: 25px;
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        border-radius: 15px;
        color: #2c3e50;
        border: 3px solid #e0e6ed;
      }
      .form-control:focus {
        border-color: #6c5ce7;
        box-shadow: 0 0 0 0.2rem rgba(108, 92, 231, 0.25);
      }
      .alert {
        border-radius: 10px;
      }
      .story-complete {
        animation: bounceIn 0.6s;
        border: 2px solid #00cec9;
        background: linear-gradient(135deg, #fff 0%, #f0fff4 100%);
      }
      @keyframes bounceIn {
        0% { transform: scale(0.3); opacity: 0; }
        50% { transform: scale(1.05); }
        70% { transform: scale(0.9); }
        100% { transform: scale(1); opacity: 1; }
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .animated-story {
        animation: fadeIn 0.5s ease-out;
      }
    "
    ))
  )
)

server <- function(input, output, session) {
  # Initialize validation rules
  iv <- InputValidator$new()

  # Add validation rules for each input
  inputs_to_validate <- c(
    "noun1",
    "noun2",
    "noun3",
    "verb",
    "verb2",
    "adjective",
    "adjective2",
    "adverb",
    "adverb2"
  )

  for (input_id in inputs_to_validate) {
    iv$add_rule(input_id, sv_required())

    # Word count validation based on input type
    if (grepl("adverb", input_id)) {
      iv$add_rule(
        input_id,
        ~ {
          if (length(strsplit(trimws(.), " ")[[1]]) > 2) {
            "Please enter 1-2 words only"
          }
        }
      )
    } else {
      iv$add_rule(
        input_id,
        ~ {
          if (length(strsplit(trimws(.), " ")[[1]]) > 3) {
            "Please enter 1-3 words only"
          }
        }
      )
    }
  }

  # Enable validation
  iv$enable()

  # Dynamic story title
  output$story_title <- renderUI({
    if (
      !is.null(input$story_template) &&
        input$story_template %in% names(story_templates)
    ) {
      return(story_templates[[input$story_template]]$title)
    }
    return("Your Mad Libs Story")
  })

  # Create reactive story that updates live
  output$story <- renderUI({
    # Check if all inputs are valid and not empty
    all_inputs <- list(
      noun1 = input$noun1,
      noun2 = input$noun2,
      noun3 = input$noun3,
      verb = input$verb,
      verb2 = input$verb2,
      adjective = input$adjective,
      adjective2 = input$adjective2,
      adverb = input$adverb,
      adverb2 = input$adverb2
    )

    empty_inputs <- sum(sapply(all_inputs, function(x) {
      is.null(x) || trimws(x) == ""
    }))

    if (!iv$is_valid() || empty_inputs > 0) {
      progress_pct <- round((9 - empty_inputs) / 9 * 100)

      return(
        div(
          class = "text-muted text-center",
          style = "font-style: italic; padding: 30px;",
          icon("edit", style = "font-size: 2em; margin-bottom: 15px;"),
          h4("Fill in the blanks to create your story!", class = "mb-3"),
          div(
            class = "progress mb-3",
            style = "height: 20px;",
            div(
              class = "progress-bar bg-success progress-bar-striped progress-bar-animated",
              style = paste0("width: ", progress_pct, "%"),
              paste0(progress_pct, "% Complete")
            )
          ),
          p(paste(empty_inputs, "more fields to go!"))
        )
      )
    }

    # Generate and display the complete story
    if (!is.null(input$story_template)) {
      result <- generate_story(input$story_template, all_inputs)

      div(
        class = "animated-story story-complete",
        style = "text-align: center; padding: 20px;",
        HTML(
          paste0(
            "<div style='font-size: 1.4em; margin-bottom: 15px;'>",
            icon(
              "book-open",
              class = "text-primary",
              style = "font-size: 1.2em;"
            ),
            "</div>",
            "<p style='font-size: 1.2em; line-height: 1.8; text-align: left;'>",
            result$story,
            "</p>",
            "<div style='margin-top: 20px; padding-top: 15px; border-top: 2px solid #e0e6ed;'>",
            "<small class='text-muted'>",
            icon("refresh"),
            " Try changing some words or pick a different story theme for more fun!",
            "</small>",
            "</div>"
          )
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
