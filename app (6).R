## =========================================================
## SUPERSTORE SALES DASHBOARD - SHINY APP (Self-contained)
## =========================================================

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

## ---- LOAD & CLEAN DATA ----
url <- "https://github.com/leonism/sample-superstore/raw/refs/heads/master/data/superstore.csv"
superstore <- read.csv(url)

superstore$Order.Date <- as.Date(superstore$Order.Date, format = "%m/%d/%Y")
superstore$Ship.Date <- as.Date(superstore$Ship.Date, format = "%m/%d/%Y")

superstore_clean <- superstore %>%
  filter(!is.na(Sales)) %>%
  distinct()

superstore_clean$Postal.Code[superstore_clean$City == "Burlington" &
                                superstore_clean$State == "Vermont"] <- 5401

superstore_clean$Row.ID <- as.integer(superstore_clean$Row.ID)
superstore_clean$Segment <- as.factor(superstore_clean$Segment)
superstore_clean$Region <- as.factor(superstore_clean$Region)
superstore_clean$Category <- as.factor(superstore_clean$Category)
superstore_clean$Sub.Category <- as.factor(superstore_clean$Sub.Category)

## ---- COLOR PALETTE ----
PALETTE <- c("#2E86AB", "#06A77D", "#F18F01", "#C73E1D", "#8E44AD", "#34495E")

kpi_box <- function(title, value, color) {
  div(style = paste0(
        "background-color:", color, "; color:white; border-radius:10px; ",
        "padding:18px; text-align:center; box-shadow:2px 2px 8px rgba(0,0,0,0.15);"),
      h5(title, style = "margin:0; font-weight:400; opacity:0.9;"),
      h2(value, style = "margin:6px 0 0 0; font-weight:700;")
  )
}

## ---- UI ----

ui <- navbarPage(
  title = "Superstore Sales Dashboard",
  theme = shinytheme("flatly"),
  collapsible = TRUE,

  tabPanel("Overview",
    fluidPage(
      br(),
      h3("Business Snapshot", style = "font-weight:700; color:#2E4053;"),
      p("Key performance indicators across all 9,994 orders (2015-2018).",
        style = "color:#7B8794;"),
      br(),
      fluidRow(
        column(4, kpi_box("Total Sales", textOutput("kpi_sales"), PALETTE[1])),
        column(4, kpi_box("Total Profit", textOutput("kpi_profit"), PALETTE[2])),
        column(4, kpi_box("Total Orders", textOutput("kpi_orders"), PALETTE[3]))
      ),
      br(),
      fluidRow(
        column(4, kpi_box("Profit Margin", textOutput("kpi_margin"), PALETTE[4])),
        column(4, kpi_box("Total Customers", textOutput("kpi_customers"), PALETTE[5])),
        column(4, kpi_box("Avg Discount", textOutput("kpi_discount"), PALETTE[6]))
      ),
      br()
    )
  ),

  tabPanel("Region Analysis",
    fluidPage(
      br(),
      h3("Profit by Region", style = "font-weight:700; color:#2E4053;"),
      sidebarLayout(
        sidebarPanel(
          style = "background-color:#F8F9FA; border-radius:10px;",
          p("Profit varies significantly by region.", style = "color:#444;"),
          p("Central has the lowest average profit, linked to a higher average discount rate.",
            style = "color:#444;"),
          h4("ANOVA Test", style = "color:#2E86AB;"),
          verbatimTextOutput("anova_output")
        ),
        mainPanel(
          plotOutput("region_plot", height = "380px"),
          br(),
          tableOutput("region_table")
        )
      )
    )
  ),

  tabPanel("Category Analysis",
    fluidPage(
      br(),
      h3("Category & Sub-Category Profitability", style = "font-weight:700; color:#2E4053;"),
      sidebarLayout(
        sidebarPanel(
          style = "background-color:#F8F9FA; border-radius:10px;",
          selectInput("category_filter", "Filter by Category:",
                      choices = c("All", levels(superstore_clean$Category))),
          hr(),
          p("Tables and Bookcases are the only loss-making sub-categories.",
            style = "color:#C73E1D; font-weight:600;")
        ),
        mainPanel(
          plotOutput("category_plot", height = "320px"),
          br(),
          tableOutput("category_table"),
          h4("Sub-Category Breakdown", style = "color:#2E86AB;"),
          plotOutput("subcategory_plot", height = "420px")
        )
      )
    )
  ),

  tabPanel("Discount Impact",
    fluidPage(
      br(),
      h3("Discount vs Profit", style = "font-weight:700; color:#2E4053;"),
      sidebarLayout(
        sidebarPanel(
          style = "background-color:#F8F9FA; border-radius:10px;",
          selectInput("subcat_filter", "Focus on Sub-Category:",
                      choices = c("All", "Tables", "Bookcases")),
          h4("Correlation Test", style = "color:#2E86AB;"),
          verbatimTextOutput("cor_output"),
          h4("Regression Model", style = "color:#2E86AB;"),
          verbatimTextOutput("regression_output")
        ),
        mainPanel(
          plotOutput("discount_plot", height = "500px")
        )
      )
    )
  ),

  tabPanel("Segment Analysis",
    fluidPage(
      br(),
      h3("Customer Segment Comparison", style = "font-weight:700; color:#2E4053;"),
      mainPanel(
        width = 12,
        plotOutput("segment_plot", height = "380px"),
        br(),
        tableOutput("segment_table")
      )
    )
  ),

  tabPanel("Time Trend",
    fluidPage(
      br(),
      h3("Sales Over Time", style = "font-weight:700; color:#2E4053;"),
      mainPanel(
        width = 12,
        plotOutput("monthly_plot", height = "380px"),
        br(),
        h4("Yearly Summary", style = "color:#2E86AB;"),
        tableOutput("yearly_table")
      )
    )
  )
)

## ---- SERVER ----

server <- function(input, output) {

  output$kpi_sales <- renderText({
    paste0("$", format(round(sum(superstore_clean$Sales)), big.mark = ","))
  })

  output$kpi_profit <- renderText({
    paste0("$", format(round(sum(superstore_clean$Profit)), big.mark = ","))
  })

  output$kpi_orders <- renderText({
    format(n_distinct(superstore_clean$Order.ID), big.mark = ",")
  })

  output$kpi_margin <- renderText({
    margin <- sum(superstore_clean$Profit) / sum(superstore_clean$Sales) * 100
    paste0(round(margin, 2), "%")
  })

  output$kpi_customers <- renderText({
    format(n_distinct(superstore_clean$Customer.ID), big.mark = ",")
  })

  output$kpi_discount <- renderText({
    paste0(round(mean(superstore_clean$Discount) * 100, 1), "%")
  })

  region_summary <- superstore_clean %>%
    group_by(Region) %>%
    summarise(
      Total_Sales = round(sum(Sales)),
      Total_Profit = round(sum(Profit)),
      Avg_Profit = round(mean(Profit), 2),
      Avg_Discount = round(mean(Discount), 3)
    ) %>%
    arrange(desc(Total_Profit))

  output$region_plot <- renderPlot({
    ggplot(region_summary, aes(x = reorder(Region, -Total_Profit), y = Total_Profit, fill = Region)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = PALETTE) +
      labs(title = "Total Profit by Region", x = NULL, y = "Total Profit ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16))
  })

  output$region_table <- renderTable({
    region_summary
  })

  output$anova_output <- renderPrint({
    anova_result <- aov(Profit ~ Region, data = superstore_clean)
    summary(anova_result)
  })

  output$category_plot <- renderPlot({
    cat_summary <- superstore_clean %>%
      group_by(Category) %>%
      summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit)) %>%
      arrange(desc(Total_Profit))

    ggplot(cat_summary, aes(x = reorder(Category, -Total_Profit), y = Total_Profit, fill = Category)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = PALETTE) +
      labs(title = "Total Profit by Category", x = NULL, y = "Total Profit ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16))
  })

  output$category_table <- renderTable({
    superstore_clean %>%
      group_by(Category) %>%
      summarise(
        Total_Sales = round(sum(Sales)),
        Total_Profit = round(sum(Profit)),
        Avg_Discount = round(mean(Discount), 3)
      ) %>%
      arrange(desc(Total_Profit))
  })

  output$subcategory_plot <- renderPlot({
    data <- superstore_clean
    if (input$category_filter != "All") {
      data <- data %>% filter(Category == input$category_filter)
    }

    subcat_summary <- data %>%
      group_by(Sub.Category) %>%
      summarise(Total_Profit = sum(Profit)) %>%
      arrange(Total_Profit)

    ggplot(subcat_summary, aes(x = reorder(Sub.Category, Total_Profit),
                                y = Total_Profit,
                                fill = Total_Profit > 0)) +
      geom_bar(stat = "identity", width = 0.65) +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "#C73E1D", "TRUE" = "#06A77D")) +
      labs(title = "Profit by Sub-Category", x = NULL, y = "Total Profit ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16))
  })

  output$discount_plot <- renderPlot({
    data <- superstore_clean
    if (input$subcat_filter != "All") {
      data <- data %>% filter(Sub.Category == input$subcat_filter)
    }

    ggplot(data, aes(x = Discount, y = Profit)) +
      geom_point(alpha = 0.35, color = "#2E86AB", size = 2) +
      geom_smooth(method = "lm", color = "#C73E1D", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#34495E") +
      labs(title = paste("Discount vs Profit -", input$subcat_filter),
           x = "Discount", y = "Profit ($)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16))
  })

  output$cor_output <- renderPrint({
    cor.test(superstore_clean$Discount, superstore_clean$Profit)
  })

  output$regression_output <- renderPrint({
    model <- lm(Profit ~ Sales + Discount + Quantity, data = superstore_clean)
    summary(model)
  })

  output$segment_plot <- renderPlot({
    segment_summary <- superstore_clean %>%
      group_by(Segment) %>%
      summarise(Total_Profit = sum(Profit)) %>%
      arrange(desc(Total_Profit))

    ggplot(segment_summary, aes(x = reorder(Segment, -Total_Profit), y = Total_Profit, fill = Segment)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = PALETTE) +
      labs(title = "Total Profit by Segment", x = NULL, y = "Total Profit ($)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16))
  })

  output$segment_table <- renderTable({
    superstore_clean %>%
      group_by(Segment) %>%
      summarise(
        Total_Sales = round(sum(Sales)),
        Total_Profit = round(sum(Profit)),
        Avg_Order_Value = round(mean(Sales), 2),
        Profit_Margin = round(sum(Profit) / sum(Sales), 3)
      ) %>%
      arrange(desc(Total_Profit))
  })

  output$monthly_plot <- renderPlot({
    monthly_data <- superstore_clean
    monthly_data$Order.Month <- format(monthly_data$Order.Date, "%Y-%m")

    monthly_sales <- monthly_data %>%
      group_by(Order.Month) %>%
      summarise(Total_Sales = sum(Sales)) %>%
      arrange(Order.Month)

    ggplot(monthly_sales, aes(x = as.Date(paste0(Order.Month, "-01")), y = Total_Sales)) +
      geom_line(color = "#06A77D", linewidth = 1.1) +
      geom_point(color = "#06A77D", size = 1.8) +
      labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales ($)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16))
  })

  output$yearly_table <- renderTable({
    yearly_data <- superstore_clean
    yearly_data$Order.Year <- format(yearly_data$Order.Date, "%Y")

    yearly_summary <- yearly_data %>%
      group_by(Order.Year) %>%
      summarise(Total_Sales = round(sum(Sales)), Total_Profit = round(sum(Profit)))

    yearly_summary$Sales_Growth_Pct <- c(NA, round(diff(yearly_summary$Total_Sales) /
                                          yearly_summary$Total_Sales[-nrow(yearly_summary)] * 100, 1))

    yearly_summary
  })
}

## ---- RUN APP ----
shinyApp(ui = ui, server = server)
