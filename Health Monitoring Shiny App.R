library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

# Define DASS 42 questions
dass42_questions <- c(
  "Saya merasa kesulitan bernapas (misalnya, pernapasan yang sangat cepat, kehabisan napas, tanpa aktivitas fisik)",
  "Saya merasa tidak mungkin ada sesuatu yang dapat membuat saya merasa lebih baik",
  "Saya merasa tidak mendapat dorongan untuk melakukan apapun",
  "Saya merasa sulit untuk rileks",
  "Saya merasa murung dan sedih",
  "Saya merasa tidak mampu menghadapi kesulitan saya",
  "Saya merasa gemetar (misalnya, tangan)",
  "Saya merasa sangat sulit untuk tenang setelah sesuatu mengganggu saya",
  "Saya merasa cenderung panik",
  "Saya merasa sulit untuk duduk diam karena gelisah",
  "Saya merasa sulit untuk tenang setelah marah",
  "Saya merasa sangat tidak sabar",
  "Saya merasa sangat sensitif",
  "Saya merasa cepat tersinggung",
  "Saya merasa kesulitan untuk mengatasi hambatan-hambatan",
  "Saya merasa sangat kesal",
  "Saya merasa sangat cemas",
  "Saya merasa mudah marah",
  "Saya merasa jantung saya berdegup kencang",
  "Saya merasa mudah terkejut",
  "Saya merasa sangat emosional",
  "Saya merasa gugup",
  "Saya merasa sangat khawatir tentang berbagai hal",
  "Saya merasa tidak mampu mengendalikan perasaan saya",
  "Saya merasa sangat kesulitan tidur",
  "Saya merasa sangat gelisah",
  "Saya merasa hidup ini tidak berarti",
  "Saya merasa sangat tertekan",
  "Saya merasa tidak ada hal yang bisa membuat saya bahagia",
  "Saya merasa hidup ini sangat tidak menyenangkan",
  "Saya merasa sangat lelah",
  "Saya merasa bahwa saya sedang sangat tertekan",
  "Saya merasa sangat cemas tanpa alasan yang jelas",
  "Saya merasa sangat tegang",
  "Saya merasa sangat gelisah saat harus menunggu sesuatu",
  "Saya merasa sangat khawatir tentang berbagai hal yang mungkin terjadi",
  "Saya merasa mudah takut",
  "Saya merasa sangat kesulitan untuk tidur",
  "Saya merasa sangat cemas",
  "Saya merasa sangat stres",
  "Saya merasa sangat tertekan",
  "Saya merasa sangat gelisah"
)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Analyzing DASS 42 Results", titleWidth = 280),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dass", icon = icon("question-circle")),
      menuItem("Kuesioner", tabName = "data", icon = icon("edit")),
      menuItem("Hasil", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Sumber Daya", tabName = "resources", icon = icon("book"))
    ),
    
    tags$head(tags$style(HTML('
      .skin-blue .main-header .navbar {
        background-color: #254336;
      }
      .skin-blue .main-header .logo {
        background-color: #254336;
        color: white;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #254336;
      }
      .skin-blue .main-sidebar {
        background-color: #6B8A7A;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #6B8A7A;
        color: white;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #6B8A7A;
        color: black;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #B7B597;
        color: white;
      }
      .content-wrapper, .right-side {
        background-color: #DAD3BE;
      }
    '))),
    
    tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap");
      body {
       font-family: "Roboto", sans-serif;
       font-size: 16px;
      }
                              ')))
    ,
    
    width = 200
  ),
  dashboardBody(
    tabItems(
      # Halaman Definisi
      tabItem(tabName = "dass",
              tags$head(tags$style(HTML('
              .custom-title {
              font-size: 40px; 
              font-weight: bold;
              }
                                        '))),
              tags$div(style = "text-align: center;",
                       tags$h1("Welcome to MENTAL CHECK!", class = "custom-title")),
              tags$p(style = "text-align: center;",
                     "Mental Check dirancang untuk membantu Anda menilai kondisi emosional Anda terkait dengan depresi, kecemasan, dan stres."),
              fluidRow(
                box(title = "Deskripsi DASS 42", width = 12, solidHeader = TRUE,
                    tags$div(style = "text-align: justify;",
                             "DASS (Depression Anxiety Stress Scale) merupakan salah satu alat ukur yang bisa digunakan untuk mengukur kondisi emosional negatif seseorang meliputi depresi, kecemasan, dan stres. 
               Kuesioner pengukuran DASS terdiri dari 42 pertanyaan yang berkaitan dengan stres, kecemasan, dan depresi seseorang.
               DASS-42 digunakan untuk menilai gejala emosi negatif seseorang yang dirancang untuk mengukur keberadaan dan tingkat keparahan gejala depresi, kecemasan, dan stres.
               Pengukuran dan hasilnya mencerminkan pengalaman seseorang tersebut selama 7 hari sebelumnya.
               Scale-42 (DASS-42) terdiri dari tiga skala DASS. Setiap skala terdiri dari 14 item, dan memiliki sub-skala dari 2-5 item.
               Skala depresi digunakan untuk menilai putus asa, dysphoria, devaluasi hidup, anhedonia, inersia kurang minat / keterlibatan, dan sikap mencela diri. Skala kecemasan digunakan untuk menilai
               pengalaman subjektif dari pengaruh kecemasan, gairah otonom, efek otot rangka, dan kecemasan situasional. Skala stres digunakan untuk menilai tingkat kronis gairah yang non-spesifik
               seperti mudah tersinggung atau over-reaktif, gairah saraf, kesulitan untuk rileks atau bersantai, dan menjadi mudah marah atau gelisah, dan tidak sabar.")),
                valueBoxOutput("sumDepresiPR"),
                valueBoxOutput("sumDepresiLK"),
                valueBoxOutput("sumStres"),
                valueBoxOutput("sumKecemasanPR"),
                valueBoxOutput("sumKecemasanLK")
              )
      ),
      
      # Kuesioner tab
      tabItem(tabName = "data",
              tabBox(id = "t1", width = 12,
                     tabPanel("Pertanyaan DASS 42",
                              tags$head(tags$style(HTML('
                              .small-font {
                              font-size: 12px; 
                              }
                                                        '))),
                              h4("Mohon jawab setiap pertanyaan dengan jujur dan sejujurnya. 
                              Terima kasih atas partisipasi Anda!"),
                              textOutput("question"),
                              radioButtons("response", "Respon:", 
                                           choices = list("Tidak Pernah" = 0,
                                                          "Kadang-Kadang" = 1,
                                                          "Sering" = 2,
                                                          "Sering Sekali" = 3),
                                           selected = NULL),
                              textOutput("warning"),
                              actionButton("prevBtn", "Sebelumnya", class = "btn btn-primary"),
                              actionButton("nextBtn", "Berikutnya", class = "btn btn-primary"),
                              actionButton("submitBtn", "Submit", class = "btn btn-success")
                     )
              )
      ),
      
      # Hasil tab
      tabItem(tabName = "viz",
              tabBox(id = "t2", width = 12,
                     tabPanel("Hasil", value = "results",
                              h3("Hasil Anda"),
                              fluidRow(
                                box(title = h4("Stress"), solidHeader = TRUE, width = 4,
                                    textOutput("stressOutput"), style = "font-size: 40px; font-weight: bold; color: white; background-color: #5F939A;"
                                ),
                                box(title = h4("Anxiety"), solidHeader = TRUE, width = 4,
                                    textOutput("anxietyOutput"), style = "font-size: 40px; font-weight: bold; color: white; background-color: #A0937D;"
                                ),
                                box(title = h4("Depression"), solidHeader = TRUE, width = 4,
                                    textOutput("depOutput"), style = "font-size: 40px; font-weight: bold; color: white; background-color: #5F6769;"
                                ),
                                align="center"),
                              plotlyOutput("resultPlot"),
                              img(src = "https://i.pinimg.com/originals/1e/4b/e7/1e4be720d7beea605002f4b2f7e67476.gif", height = "200px")
                     ),
                     tabPanel("Saran", value = "suggestions",
                              h3("Saran"),
                              textOutput("suggestionText"),
                              imageOutput("suggestionImage")
                     )
              )
      ),
      
      # Sumber Daya tab
      tabItem(tabName = "resources",
              fluidPage(
                titlePanel("Sumber Daya"),
                p("Berikut adalah beberapa sumber daya yang dapat membantu Anda mempelajari lebih lanjut tentang stres, kecemasan, dan depresi:"),
                tags$ul(
                  tags$li(tags$a(href = "https://www.who.int/news-room/fact-sheets/detail/depression", "WHO: Depresi")),
                  tags$li(tags$a(href = "https://www.who.int/news-room/fact-sheets/detail/anxiety-disorders", "WHO: Gangguan Kecemasan")),
                  tags$li(tags$a(href = "https://www.mayoclinic.org/diseases-conditions/stress/symptoms-causes/syc-20353585", "Mayo Clinic: Manajemen Stres")),
                  tags$li(tags$a(href = "https://www.helpguide.org/articles/stress/stress-management.htm", "HelpGuide: Manajemen Stres")),
                  tags$li(tags$a(href = "https://www.mentalhealth.org.uk/a-to-z/a/anxiety", "Mental Health Foundation: Kecemasan")),
                  tags$li(tags$a(href = "https://www.nimh.nih.gov/health/topics/depression/index.shtml", "NIMH: Depresi")),
                  tags$li(tags$a(href = "https://www.youtube.com/watch?v=ydO-5e68AF4", "DASS 21")),
                  tags$li(tags$a(href = "https://www.youtube.com/watch?v=RZ2itg-TvzA", "DASS 42"))
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session){
  
  output$sumDepresiPR <- renderValueBox({
    valueBox(
      "2,9%", "Depresi Perempuan (Our World in Data, 2019)", icon = icon("venus"), color = "olive"  # Perbaiki tanda kutip dan nilai warna
    )
  })
  
  output$sumDepresiLK <- renderValueBox({
    valueBox(
      "2%", "Depresi Laki-Laki (Our World in Data, 2019)", icon = icon("mars"), color = "olive"  # Perbaiki tanda kutip dan nilai warna
    )
  })
  
  output$sumStres <- renderValueBox({
    valueBox(
      "71,6%", "Prevalensi Stres (WHO, 2018)", icon = icon("truck-medical"), color = "olive")
  })
  
  output$sumKecemasanPR <- renderValueBox({
    valueBox(
      "4,5%", "Kecemasan Perempuan (Our World in Data, 2019)", icon = icon("pills"), color = "olive")
  })
  
  output$sumKecemasanLK <- renderValueBox({
    valueBox(
      "2,7%", "Kecemasan Laki-Laki (Our World in Data, 2019)", icon = icon("stethoscope"), color = "olive")
  })
  
  questionIndex <- reactiveVal(1)
  responses <- reactiveValues(data = rep(NA, length(dass42_questions)))
  
  observeEvent(input$nextBtn, {
    if (is.null(input$response)) {
      output$warning <- renderText("Mohon pilih satu")
    } else {
      responses$data[questionIndex()] <- as.numeric(input$response)
      if (questionIndex() < length(dass42_questions)) {
        questionIndex(questionIndex() + 1)
        updateRadioButtons(session, "response", selected = responses$data[questionIndex()])
        output$warning <- renderText("")
      }
    }
  })
  
  observeEvent(input$prevBtn, {
    if (questionIndex() > 1) {
      responses$data[questionIndex()] <- as.numeric(input$response)
      questionIndex(questionIndex() - 1)
      updateRadioButtons(session, "response", selected = responses$data[questionIndex()])
      output$warning <- renderText("")
    }
  })
  
  observeEvent(input$submitBtn, {
    if (any(is.na(responses$data))) {
      output$warning <- renderText("Mohon selesaikan semua pertanyaan")
    } else {
      results <- calculateResults(responses$data)
      output$stressOutput <- renderText({
        paste(results$stress, "-", categorizeResults(results$stress))
      })
      
      output$anxietyOutput <- renderText({
        paste(results$anxiety, "-", categorizeResults(results$anxiety))
      })
      
      output$depOutput <- renderText({
        paste(results$depression, "-", categorizeResults(results$depression))
      })
      
      output$suggestionText <- renderText({
        calculateSuggestions(results)
      })
      
      output$resultPlot <- renderPlotly({
        plotResults(results)
      })
      
      output$suggestionImage <- renderImage({
        if (results$stress + results$anxiety + results$depression < 50) {
          list(src = "https://www.mindful.org/wp-content/uploads/Mindful-Breathing-Exercise-768x432.jpg", contentType = "image/jpeg", alt = "Meditasi Kesadaran")
        } else if (results$stress + results$anxiety + results$depression < 100) {
          list(src = "https://cdn.mos.cms.futurecdn.net/FMWsLRV7ZL2KrSjmLnsNLc.jpg", contentType = "image/jpeg", alt = "Latihan Pernapasan Dalam")
        } else {
          list(src = "https://www.verywellmind.com/thmb/-NOa92vGOtCo9NWT3eMFSnnJPNY=/2114x1410/filters:no_upscale():max_bytes(150000):strip_icc()/GettyImages-478897966-5c8907cc46e0fb0001ea59f8.jpg", contentType = "image/jpeg", alt = "Relaksasi Otot Progresif")
        }
      }, deleteFile = FALSE)
      
      updateTabsetPanel(session, "t2", selected = "results")
      output$warning <- renderText("")
    }
  })
  
  output$question <- renderText({
    dass42_questions[questionIndex()]
  })
}

# Function to calculate results
calculateResults <- function(responses) {
  stress <- sum(responses[1:14], na.rm = TRUE)
  anxiety <- sum(responses[15:28], na.rm = TRUE)
  depression <- sum(responses[29:42], na.rm = TRUE)
  list(stress = stress, anxiety = anxiety, depression = depression)
}

# Function to categorize results
categorizeResults <- function(score) {
  if (score < 10) {
    "Rendah"
  } else if (score < 20) {
    "Sedang"
  } else {
    "Tinggi"
  }
}

# Function to plot results
plotResults <- function(results) {
  df <- data.frame(
    Type = c("Stress", "Anxiety", "Depression"),
    Score = c(results$stress, results$anxiety, results$depression)
  )
  p <- ggplot(df, aes(x = Type, y = Score, fill = Type, text = paste(Type, ":", Score))) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "DASS 42 Results", x = "Type", y = "Score") +
    scale_fill_brewer(palette = "Set3") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "x unified",
      yaxis = list(title = "Score"),
      xaxis = list(title = "Type")
    ) %>%
    config(displayModeBar = FALSE)
}

# Function to calculate suggestions
calculateSuggestions <- function(results) {
  points <- results$stress + results$anxiety + results$depression
  if (points < 50) {
    "Hasil Anda menunjukkan tingkat stres, kecemasan, dan depresi yang rendah. Anda dapat mencoba meditasi kesadaran untuk membantu menjaga kesehatan mental Anda. Meditasi kesadaran adalah praktik di mana Anda memusatkan perhatian Anda pada saat ini, tanpa penilaian. Penelitian telah menunjukkan bahwa meditasi kesadaran dapat membantu mengurangi stres dan meningkatkan kesejahteraan secara keseluruhan. Cobalah mencari tempat yang tenang, duduk dengan nyaman, dan fokus pada napas Anda. Biarkan pikiran datang dan pergi tanpa terjebak di dalamnya.
    Pernyataan tersebut terkait dengan konsep mindfulness, yang didefinisikan sebagai kesadaran yang tidak terinterupsi dan tidak terdistorsi terhadap pengalaman sekarang oleh Jon Kabat-Zinn, seorang ahli mindfulness dan profesor biologi molekuler di Universitas Massachusetts. "
  } else if (points < 100) {
    "Hasil Anda menunjukkan tingkat stres, kecemasan, dan depresi yang sedang. Latihan pernapasan dalam dapat membantu menenangkan pikiran Anda dan mengurangi ketegangan. Ambil napas dalam-dalam melalui hidung, tahan selama beberapa detik, dan hembuskan perlahan melalui mulut. Ulangi proses ini beberapa kali hingga Anda merasa lebih rileks. Penelitian menunjukkan bahwa latihan pernapasan dalam dapat mengaktifkan respons relaksasi tubuh, mengurangi detak jantung, dan menurunkan tekanan darah.Pernyataan tersebut terkait dengan konsep respiratory sinus arrhythmia (RSA), yang didefinisikan sebagai perubahan detak jantung yang terkait dengan pernapasan. RSA dipengaruhi oleh aktivitas parasympathetic nervous system (PNS), yang berfungsi mengaktifkan respons relaksasi tubuh dan mengurangi stres. Latihan pernapasan dalam dapat meningkatkan RSA, mengurangi detak jantung, dan menurunkan tekanan darah, sehingga membantu mengurangi stres dan meningkatkan kesejahteraan secara keseluruhan."
  } else {
    "Hasil Anda menunjukkan tingkat stres, kecemasan, dan depresi yang tinggi. Pertimbangkan untuk mencoba relaksasi otot progresif. Teknik ini melibatkan pengencangan dan pengenduran setiap kelompok otot dalam tubuh secara sistematis. Mulailah dengan otot-otot kaki Anda dan perlahan naik ke atas, berfokus pada satu kelompok otot pada satu waktu. Penelitian telah menunjukkan bahwa relaksasi otot progresif dapat membantu mengurangi ketegangan fisik dan mental, serta meningkatkan kualitas tidur. Pernyataan tersebut terkait dengan konsep progressive muscle relaxation (PMR), yang didefinisikan sebagai teknik relaksasi yang melibatkan pengencangan dan pengenduran setiap kelompok otot dalam tubuh secara sistematis. PMR dipengaruhi oleh teori autonomic nervous system (ANS), yang berfungsi mengatur respons stres dan relaksasi. PMR dapat membantu mengurangi ketegangan fisik dan mental, serta meningkatkan kualitas tidur, karena mengaktifkan respons relaksasi tubuh dan mengurangi respons stres."
  }
}



shinyApp(ui,server)