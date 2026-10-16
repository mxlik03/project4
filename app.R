library(shiny)

# ============================================================
# Differentiability Explorer
# ============================================================

# Cube root that works for negative x-values
cube_root <- function(x) {
  sign(x) * abs(x)^(1 / 3)
}

# Function choices
function_choices <- c(
  "Absolute Value: |x|" = "abs",
  "Smooth Function: x^2 + sin(x)" = "smooth",
  "Vertical Tangent: cube root of x" = "cuberoot",
  "Piecewise Function" = "piecewise"
)

# Evaluate selected function
f <- function(choice, x) {

  if (choice == "abs") {
    return(abs(x))
  }

  if (choice == "smooth") {
    return(x^2 + sin(x))
  }

  if (choice == "cuberoot") {
    return(cube_root(x))
  }

  if (choice == "piecewise") {
    return(
      ifelse(
        x < 0,
        x + 1,
        x^2
      )
    )
  }
}

# Graph title
function_name <- function(choice) {

  if (choice == "abs") {
    return("f(x) = |x|")
  }

  if (choice == "smooth") {
    return("f(x) = x^2 + sin(x)")
  }

  if (choice == "cuberoot") {
    return("f(x) = cube root of x")
  }

  if (choice == "piecewise") {
    return("f(x) = x + 1 if x < 0, x^2 if x >= 0")
  }
}


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(

  titlePanel("Differentiability Explorer"),

  sidebarLayout(

    sidebarPanel(

      selectInput(
        inputId = "functionChoice",
        label = "Choose a function:",
        choices = function_choices,
        selected = "abs"
      ),

      sliderInput(
        inputId = "xRange",
        label = "Select x-range:",
        min = -10,
        max = 10,
        value = c(-5, 5),
        step = 0.5
      ),

      sliderInput(
        inputId = "a",
        label = "Choose point a:",
        min = -5,
        max = 5,
        value = 0,
        step = 0.05
      ),

      sliderInput(
        inputId = "h",
        label = "Choose increment h:",
        min = 0.0001,
        max = 1,
        value = 0.1,
        step = 0.0001
      ),

      checkboxInput(
        inputId = "showSecant",
        label = "Show secant line",
        value = TRUE
      ),

      checkboxInput(
        inputId = "showTangent",
        label = "Show tangent estimate",
        value = TRUE
      ),

      hr(),

      strong("Guide:"),

      p(
        "Move a toward the point you want to investigate. ",
        "Shrink h and compare the left and right derivative estimates."
      )
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Visualization",

          br(),

          plotOutput(
            outputId = "functionPlot",
            height = "500px"
          ),

          br(),

          h4("Conclusion"),

          textOutput("conclusion")
        ),

        tabPanel(
          "Numerical Diagnostics",

          br(),

          h3("Derivative Estimates"),

          tableOutput("diagnostics"),

          hr(),

          h3("Convergence Table"),

          p(
            "This table shows how the derivative estimates behave ",
            "as h becomes smaller."
          ),

          tableOutput("convergenceTable")
        ),

        tabPanel(
          "Theory & Guide",

          br(),

          h3("Theory"),

          p(
            "A function is differentiable at a point when its slope approaches ",
            "one finite value as we look closer and closer to that point. ",
            "This is described by the limit of the difference quotient as h ",
            "approaches 0. If a function is differentiable at a point, it must ",
            "also be continuous there because a break or jump would prevent a ",
            "single tangent slope from existing. However, continuity does not ",
            "guarantee differentiability. For example, x^2 + sin(x) is smooth ",
            "and differentiable everywhere. The function |x| is continuous at ",
            "0, but it is not differentiable there because the left slope ",
            "approaches -1 while the right slope approaches 1. Numerical ",
            "estimates can help test differentiability, but very small h-values ",
            "can introduce rounding errors and unstable behavior near corners ",
            "or vertical tangents."
          ),

          hr(),

          h3("Help / Guide"),

          tags$ol(
            tags$li(
              "Choose one of the example functions."
            ),

            tags$li(
              "Adjust the x-range so the point you want to study is visible."
            ),

            tags$li(
              "Move a to the point you want to test."
            ),

            tags$li(
              "Shrink h and compare the left and right derivative estimates."
            ),

            tags$li(
              "Turn the secant and tangent lines on or off to compare them visually."
            ),

            tags$li(
              "If the left and right estimates approach the same finite number, ",
              "the function is likely differentiable."
            ),

            tags$li(
              "If the estimates approach different values, become extremely large, ",
              "or fail to stabilize, the function is likely not differentiable."
            )
          )
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # Keep point a inside the chosen x-range
  observeEvent(input$xRange, {

    current_a <- isolate(input$a)

    if (current_a < input$xRange[1]) {
      current_a <- input$xRange[1]
    }

    if (current_a > input$xRange[2]) {
      current_a <- input$xRange[2]
    }

    updateSliderInput(
      session = session,
      inputId = "a",
      min = input$xRange[1],
      max = input$xRange[2],
      value = current_a
    )
  })


  # ==========================================================
  # MAIN CALCULATIONS
  # ==========================================================

  calculations <- reactive({

    a <- input$a
    h <- input$h

    fa <- f(
      input$functionChoice,
      a
    )

    f_left <- f(
      input$functionChoice,
      a - h
    )

    f_right <- f(
      input$functionChoice,
      a + h
    )

    left_derivative <- (fa - f_left) / h

    right_derivative <- (f_right - fa) / h

    tangent_slope <- (f_right - f_left) / (2 * h)

    list(
      a = a,
      h = h,
      fa = fa,
      f_left = f_left,
      f_right = f_right,
      left = left_derivative,
      right = right_derivative,
      tangent = tangent_slope
    )
  })


  # ==========================================================
  # DIFFERENTIABILITY TEST
  # ==========================================================

  differentiability_info <- reactive({

    values <- calculations()

    left <- values$left
    right <- values$right

    difference <- abs(left - right)

    tolerance <- max(
      0.05,
      0.08 * max(
        1,
        abs(left),
        abs(right)
      )
    )

    vertical_tangent <- (
      abs(left) > 100 ||
      abs(right) > 100
    )

    likely_differentiable <- (
      is.finite(left) &&
      is.finite(right) &&
      !vertical_tangent &&
      difference <= tolerance
    )

    list(
      difference = difference,
      tolerance = tolerance,
      vertical_tangent = vertical_tangent,
      likely_differentiable = likely_differentiable
    )
  })


  # ==========================================================
  # GRAPH
  # ==========================================================

  output$functionPlot <- renderPlot({

    values <- calculations()
    info <- differentiability_info()

    x <- seq(
      from = input$xRange[1],
      to = input$xRange[2],
      length.out = 1000
    )

    y <- f(
      input$functionChoice,
      x
    )

    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = "x",
      ylab = "f(x)",
      main = function_name(input$functionChoice)
    )

    grid()

    # Point a
    points(
      values$a,
      values$fa,
      pch = 19,
      cex = 1.2
    )

    text(
      values$a,
      values$fa,
      labels = "a",
      pos = 4
    )


    # ----------------------------
    # Secant line
    # ----------------------------

    if (input$showSecant) {

      secant_slope <- values$right

      secant_x <- input$xRange

      secant_y <- (
        values$fa +
        secant_slope * (secant_x - values$a)
      )

      lines(
        secant_x,
        secant_y,
        lty = 2,
        lwd = 2
      )

      points(
        values$a + values$h,
        values$f_right,
        pch = 17
      )
    }


    # ----------------------------
    # Tangent estimate
    # ----------------------------

    if (
      input$showTangent &&
      info$likely_differentiable &&
      is.finite(values$tangent) &&
      abs(values$tangent) < 100
    ) {

      tangent_x <- input$xRange

      tangent_y <- (
        values$fa +
        values$tangent * (tangent_x - values$a)
      )

      lines(
        tangent_x,
        tangent_y,
        lty = 3,
        lwd = 2
      )
    }


    # ----------------------------
    # Legend
    # ----------------------------

    legend_names <- c("Function")
    legend_types <- c(1)

    if (input$showSecant) {
      legend_names <- c(
        legend_names,
        "Secant line"
      )

      legend_types <- c(
        legend_types,
        2
      )
    }

    if (
      input$showTangent &&
      info$likely_differentiable
    ) {
      legend_names <- c(
        legend_names,
        "Tangent estimate"
      )

      legend_types <- c(
        legend_types,
        3
      )
    }

    legend(
      "topleft",
      legend = legend_names,
      lty = legend_types,
      lwd = 2,
      bty = "n"
    )
  })


  # ==========================================================
  # NUMERICAL DIAGNOSTICS
  # ==========================================================

  output$diagnostics <- renderTable({

    values <- calculations()

    data.frame(
      Quantity = c(
        "a",
        "h",
        "f(a)",
        "f(a-h)",
        "f(a+h)",
        "Left derivative estimate",
        "Right derivative estimate",
        "Tangent slope estimate"
      ),

      Value = round(
        c(
          values$a,
          values$h,
          values$fa,
          values$f_left,
          values$f_right,
          values$left,
          values$right,
          values$tangent
        ),
        6
      )
    )
  },
  striped = TRUE,
  bordered = TRUE
  )


  # ==========================================================
  # CONCLUSION
  # ==========================================================

  output$conclusion <- renderText({

    values <- calculations()
    info <- differentiability_info()

    if (info$vertical_tangent) {

      return(
        paste(
          "Likely not differentiable at a =",
          values$a,
          "- the derivative estimates become very large,",
          "which suggests a vertical tangent."
        )
      )
    }

    if (info$likely_differentiable) {

      return(
        paste(
          "Likely differentiable at a =",
          values$a,
          "- the left and right derivative estimates are close."
        )
      )
    }

    paste(
      "Likely not differentiable at a =",
      values$a,
      "- the left and right derivative estimates do not agree."
    )
  })


  # ==========================================================
  # CONVERGENCE TABLE
  # ==========================================================

  output$convergenceTable <- renderTable({

    values <- calculations()

    h_values <- c(
      input$h,
      input$h / 2,
      input$h / 10,
      input$h / 100
    )

    results <- data.frame(
      h = numeric(0),
      Left = numeric(0),
      Right = numeric(0),
      Centered = numeric(0)
    )

    for (current_h in h_values) {

      fa <- f(
        input$functionChoice,
        values$a
      )

      f_left <- f(
        input$functionChoice,
        values$a - current_h
      )

      f_right <- f(
        input$functionChoice,
        values$a + current_h
      )

      left_estimate <- (
        fa - f_left
      ) / current_h

      right_estimate <- (
        f_right - fa
      ) / current_h

      centered_estimate <- (
        f_right - f_left
      ) / (2 * current_h)

      new_row <- data.frame(
        h = round(
          current_h,
          7
        ),

        Left = round(
          left_estimate,
          6
        ),

        Right = round(
          right_estimate,
          6
        ),

        Centered = round(
          centered_estimate,
          6
        )
      )

      results <- rbind(
        results,
        new_row
      )
    }

    results
  },
  striped = TRUE,
  bordered = TRUE
  )
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)