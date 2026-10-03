library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------------
# 1. STUDENT DATA
# -----------------------------

students <- data.frame(
  Name = c("Arun", "Priya", "Rahul", "Meena", "Kavin"),
  Mathematics = c(85, 72, 90, 65, 78),
  Science = c(88, 75, 92, 70, 80),
  English = c(82, 78, 85, 68, 75),
  Attendance = c(92, 85, 95, 75, 88),
  Study_Hours = c(4, 3, 5, 2, 4)
)

# -----------------------------
# 2. CALCULATE AVERAGE
# -----------------------------

students$Average <- rowMeans(
  students[, c("Mathematics", "Science", "English")]
)

# -----------------------------
# 3. PASS / FAIL
# -----------------------------

students$Status <- ifelse(
  students$Average >= 50,
  "Pass",
  "Fail"
)

# -----------------------------
# 4. PERFORMANCE LEVEL
# -----------------------------

students$Performance <- ifelse(
  students$Average >= 80,
  "Excellent",
  ifelse(
    students$Average >= 60,
    "Good",
    ifelse(
      students$Average >= 50,
      "Average",
      "Needs Improvement"
    )
  )
)

# -----------------------------
# 5. USER INTERFACE
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Student Performance Analyzer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "student",
        "Select Student:",
        choices = students$Name
      )
      
    ),
    
    mainPanel(
      
      h3("Student Performance"),
      
      textOutput("student_name"),
      textOutput("average"),
      textOutput("attendance"),
      textOutput("status"),
      textOutput("performance"),
      
      br(),
      
      plotOutput("marks_plot"),
      
      br(),
      
      plotOutput("attendance_plot")
    )
  )
)

# -----------------------------
# 6. SERVER
# -----------------------------

server <- function(input, output) {
  
  selected_student <- reactive({
    
    students %>%
      filter(Name == input$student)
    
  })
  
  # Student name
  output$student_name <- renderText({
    
    paste(
      "Student:",
      selected_student()$Name
    )
    
  })
  
  # Average
  output$average <- renderText({
    
    paste(
      "Average Marks:",
      round(selected_student()$Average, 2)
    )
    
  })
  
  # Attendance
  output$attendance <- renderText({
    
    paste(
      "Attendance:",
      selected_student()$Attendance,
      "%"
    )
    
  })
  
  # Pass / Fail
  output$status <- renderText({
    
    paste(
      "Status:",
      selected_student()$Status
    )
    
  })
  
  # Performance Level
  output$performance <- renderText({
    
    paste(
      "Performance Level:",
      selected_student()$Performance
    )
    
  })
  
  # Subject marks graph
  output$marks_plot <- renderPlot({
    
    student <- selected_student()
    
    marks <- data.frame(
      Subject = c(
        "Mathematics",
        "Science",
        "English"
      ),
      Marks = c(
        student$Mathematics,
        student$Science,
        student$English
      )
    )
    
    ggplot(
      marks,
      aes(x = Subject, y = Marks)
    ) +
      geom_col() +
      ylim(0, 100) +
      labs(
        title = "Subject-wise Marks",
        x = "Subject",
        y = "Marks"
      )
    
  })
  
  # Attendance vs average graph
  output$attendance_plot <- renderPlot({
    
    ggplot(
      students,
      aes(
        x = Attendance,
        y = Average
      )
    ) +
      geom_point(size = 4) +
      labs(
        title = "Attendance vs Average Marks",
        x = "Attendance (%)",
        y = "Average Marks"
      )
    
  })
  
}

# -----------------------------
# 7. RUN SHINY APPLICATION
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)