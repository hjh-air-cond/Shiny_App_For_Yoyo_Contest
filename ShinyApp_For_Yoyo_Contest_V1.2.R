setwd('D://R-Pro//悠悠球技术分项目')

library(shiny)
library(shinyauthr)
library(DT)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(zip)

# 创建一个模拟的用户数据库
users <- data.frame(
  user = c("admin", "tech_judge1", "tech_judge2", "tech_judge3", "tech_judge4", "tech_judge5", "tech_judge6",
           "perf_judge1", "perf_judge2", "perf_judge3", "perf_judge4", "chief_judge"),
  password = sapply(c("adminpass", "password1", "password2", "password3", "password4", "password5", "password6",
                      "password7", "password8", "password9", "password10", "password11"), sodium::password_store),
  role = c("admin", rep("tech", 6), rep("perf", 4), "chief"),
  stringsAsFactors = FALSE
)

# 持久化数据文件路径
player_data_file <- "player_data.rds"
tech_score_data_file <- "tech_score_data.rds"
perf_score_data_file <- "perf_score_data.rds"

# 初始化数据文件
if (!file.exists(player_data_file)) {
  saveRDS(data.frame(选手姓名 = character(), stringsAsFactors = FALSE), player_data_file)
}
if (!file.exists(tech_score_data_file)) {
  saveRDS(data.frame(裁判 = character(), 选手姓名 = character(), 比赛项目 = character(), 技术分加分 = numeric(), 技术分减分 = numeric(), 技术分 = numeric(), 确认 = logical(), stringsAsFactors = FALSE), tech_score_data_file)
}
if (!file.exists(perf_score_data_file)) {
  saveRDS(data.frame(裁判 = character(), 选手姓名 = character(), 比赛项目 = character(), 表演分1 = numeric(), 表演分2 = numeric(), 表演分3 = numeric(), 表演分4 = numeric(), 表演分5 = numeric(), 表演分6 = numeric(), 表演分7 = numeric(), 表演分8 = numeric(), 确认 = logical(), stringsAsFactors = FALSE), perf_score_data_file)
}

# 读取数据
load_player_data <- function() {
  readRDS(player_data_file)
}
load_tech_score_data <- function() {
  readRDS(tech_score_data_file)
}
load_perf_score_data <- function() {
  readRDS(perf_score_data_file)
}

# 保存数据
save_player_data <- function(data) {
  saveRDS(data, player_data_file)
}
save_tech_score_data <- function(data) {
  saveRDS(data, tech_score_data_file)
}
save_perf_score_data <- function(data) {
  saveRDS(data, perf_score_data_file)
}

# 清除所有数据
clear_all_data <- function() {
  saveRDS(data.frame(选手姓名 = character(), stringsAsFactors = FALSE), player_data_file)
  saveRDS(data.frame(裁判 = character(), 选手姓名 = character(), 比赛项目 = character(), 技术分加分 = numeric(), 技术分减分 = numeric(), 确认 = logical(), stringsAsFactors = FALSE), tech_score_data_file)
  saveRDS(data.frame(裁判 = character(), 选手姓名 = character(), 比赛项目 = character(), 表演分1 = numeric(), 表演分2 = numeric(), 表演分3 = numeric(), 表演分4 = numeric(), 表演分5 = numeric(), 表演分6 = numeric(), 表演分7 = numeric(), 表演分8 = numeric(), 确认 = logical(), stringsAsFactors = FALSE), perf_score_data_file)
}

# 模拟的示例数据
# example_tech_data <- data.frame(
#   裁判 = rep(paste0("tech_judge", 1:6), each = 3),
#   选手姓名 = rep(paste0("选手", 1:3), 6),
#   比赛项目 = rep("1A", 18),
#   技术分加分 = sample(50:100, 18, replace = TRUE),
#   技术分减分 = sample(0:50, 18, replace = TRUE),
#   确认 = TRUE,
#   stringsAsFactors = FALSE
# )
# 
# example_perf_data <- data.frame(
#   裁判 = rep(paste0("perf_judge", 1:4), each = 3),
#   选手姓名 = rep(paste0("选手", 1:3), 4),
#   比赛项目 = rep("1A", 12),
#   表演分1 = sample(0:10, 12, replace = TRUE),
#   表演分2 = sample(0:10, 12, replace = TRUE),
#   表演分3 = sample(0:10, 12, replace = TRUE),
#   表演分4 = sample(0:10, 12, replace = TRUE),
#   表演分5 = sample(0:10, 12, replace = TRUE),
#   表演分6 = sample(0:10, 12, replace = TRUE),
#   表演分7 = sample(0:10, 12, replace = TRUE),
#   表演分8 = sample(0:10, 12, replace = TRUE),
#   确认 = TRUE,
#   stringsAsFactors = FALSE
# )

# save(example_tech_data, example_perf_data, file = 'example_score.Rdata')
# write.csv(example_tech_data, 'example_tech_data.csv')
# write.csv(example_perf_data, 'example_perf_data.csv')
load('example_score.Rdata')


# 去除最高最低分并计算平均分的函数
calculate_mean_without_extremes <- function(scores) {
  if (length(scores) > 2) {
    sorted_scores <- sort(scores)
    return(mean(sorted_scores[-c(1, length(sorted_scores))]))
  } else {
    return(mean(scores))
  }
}

# 全局变量保存总分结果
total_summary_global <- NULL

# UI
ui <- fluidPage(
  titlePanel("悠悠球比赛分数记录系统"),
  shinyauthr::loginUI(id = "login"),
  uiOutput("app_ui")
)

# Server
server <- function(input, output, session) {
  # 用户登录模块
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = users,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE
  )
  
  # 用户登录后显示主界面
  output$app_ui <- renderUI({
    req(credentials()$user_auth)
    
    role <- credentials()$info$role
    
    if (role == "admin") {
      fluidPage(
        textInput("new_player", "输入新选手姓名", ""),
        actionButton("add_player", "添加选手"),
        DTOutput("playerTable")
      )
    } else if (role != "chief") {
      sidebarLayout(
        sidebarPanel(
          uiOutput("playerSelect"),
          selectInput("event", "比赛项目", choices = c("1A", "2A", "3A", "4A", "5A")),
          if (role == "tech") {
            list(
              numericInput("tech_add", "技术分加分", value = 0, min = 0, max = 100, step = 0.1),
              numericInput("tech_sub", "技术分减分", value = 0, min = 0, max = 100, step = 0.1),
              tags$hr(),
              tags$p("请对每一位选手进行评分并确认分数，才能进行下一位选手的评分。")
            )
          } else {
            list(
              sliderInput("perf1", "表演分1", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf2", "表演分2", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf3", "表演分3", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf4", "表演分4", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf5", "表演分5", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf6", "表演分6", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf7", "表演分7", min = 0, max = 10, value = 0, step = 1),
              sliderInput("perf8", "表演分8", min = 0, max = 10, value = 0, step = 1),
              tags$hr(),
              tags$p("请对每一位选手进行评分并确认分数，才能进行下一位选手的评分。")
            )
          },
          actionButton("add", "添加得分"),
          actionButton("confirm", "确认得分")
        ),
        mainPanel(
          DTOutput("scoreTable")
        )
      )
    } else {
      fluidPage(
        actionButton("calculate", "计算总分"),
        actionButton("load_example_data", "加载示例数据"),
        checkboxInput("exclude_extremes", "去掉最高最低分"),
        downloadButton("download_total_scores", "下载总分表"),
        DTOutput("totalScoreTable"),
        selectInput("judge_select", "选择裁判", choices = c("tech_judge1", "tech_judge2", "tech_judge3", "tech_judge4", "tech_judge5", "tech_judge6",
                                                        "perf_judge1", "perf_judge2", "perf_judge3", "perf_judge4")),
        DTOutput("selectedJudgeTable"),
        downloadButton("download_selected_judge_scores", "下载选中裁判打分表"),
        downloadButton("download_judge_scores", "下载所有裁判打分表"),
        actionButton("clear_data", "清除所有数据")
      )
    }
  })
  
  # 添加选手姓名
  observeEvent(input$add_player, {
    req(credentials()$user_auth)
    role <- credentials()$info$role
    
    if (role == "admin") {
      new_player <- input$new_player
      if (new_player != "" && !new_player %in% load_player_data()$选手姓名) {
        new_data <- rbind(load_player_data(), data.frame(选手姓名 = new_player, stringsAsFactors = FALSE))
        save_player_data(new_data)
        updatePlayerChoices()
      }
    }
  })
  
  # 更新选手选择
  updatePlayerChoices <- function() {
    choices <- load_player_data()$选手姓名
    updateSelectInput(session, "player", choices = choices)
  }
  
  # 动态更新选手选择
  output$playerSelect <- renderUI({
    req(credentials()$user_auth)
    role <- credentials()$info$role
    if (role != "admin" && role != "chief") {
      selectInput("player", "选手姓名", choices = load_player_data()$选手姓名)
    }
  })
  
  # 实时更新选手姓名数据
  output$playerTable <- renderDT({
    invalidateLater(2000, session)
    req(credentials()$user_auth)
    role <- credentials()$info$role
    
    if (role == "admin") {
      datatable(load_player_data(), options = list(pageLength = 10))
    }
  })
  
  # 添加得分到数据框
  observeEvent(input$add, {
    req(credentials()$user_auth)
    role <- credentials()$info$role
    user <- credentials()$info$user
    
    if (role == "tech") {
      data <- load_tech_score_data()
      if (any(data$裁判 == user & data$选手姓名 == input$player & data$比赛项目 == input$event)) {
        showModal(modalDialog(
          title = "错误",
          "您已为该选手评分，不能再次评分。",
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (any(data$确认 == FALSE & data$裁判 == user)) {
        showModal(modalDialog(
          title = "错误",
          "请先确认当前选手的得分。",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        new_data <- data.frame(
          裁判 = user, 
          选手姓名 = input$player, 
          比赛项目 = input$event, 
          技术分加分 = input$tech_add, 
          技术分减分 = input$tech_sub, 
          技术分 = input$tech_add - input$tech_sub, 
          确认 = FALSE, 
          stringsAsFactors = FALSE)
        save_tech_score_data(rbind(data, new_data))
      }
    } else {
      data <- load_perf_score_data()
      if (any(data$裁判 == user & data$选手姓名 == input$player & data$比赛项目 == input$event)) {
        showModal(modalDialog(
          title = "错误",
          "您已为该选手评分，不能再次评分。",
          easyClose = TRUE,
          footer = NULL
        ))
      } else if (any(data$确认 == FALSE & data$裁判 == user)) {
        showModal(modalDialog(
          title = "错误",
          "请先确认当前选手的得分。",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        new_data <- data.frame(
          裁判 = user, 
          选手姓名 = input$player, 
          比赛项目 = input$event, 
          表演分1 = input$perf1, 
          表演分2 = input$perf2, 
          表演分3 = input$perf3, 
          表演分4 = input$perf4, 
          表演分5 = input$perf5, 
          表演分6 = input$perf6, 
          表演分7 = input$perf7, 
          表演分8 = input$perf8, 
          确认 = FALSE, 
          stringsAsFactors = FALSE)
        save_perf_score_data(rbind(data, new_data))
      }
    }
  })
  
  # 确认得分
  observeEvent(input$confirm, {
    req(credentials()$user_auth)
    role <- credentials()$info$role
    user <- credentials()$info$user
    
    if (role == "tech") {
      data <- load_tech_score_data()
      data$确认[data$裁判 == user & data$选手姓名 == input$player & data$比赛项目 == input$event] <- TRUE
      save_tech_score_data(data)
    } else {
      data <- load_perf_score_data()
      data$确认[data$裁判 == user & data$选手姓名 == input$player & data$比赛项目 == input$event] <- TRUE
      save_perf_score_data(data)
    }
  })
  
  # 实时更新得分数据
  output$scoreTable <- renderDT({
    invalidateLater(2000, session)
    req(credentials()$user_auth)
    role <- credentials()$info$role
    user <- credentials()$info$user
    
    if (role == "tech") {
      data <- load_tech_score_data()
      datatable(data[data$裁判 == user, ], options = list(pageLength = 10))
    } else {
      data <- load_perf_score_data()
      datatable(data[data$裁判 == user, ], options = list(pageLength = 10))
    }
  })
  
  # 计算并显示总分
  observeEvent(input$calculate, {
    req(credentials()$user_auth)
    role <- credentials()$info$role
    
    tech_data <- load_tech_score_data() %>% filter(确认 == TRUE)
    perf_data <- load_perf_score_data() %>% filter(确认 == TRUE)
    
    # 技术分计算
    if (nrow(tech_data) > 0) {
      tech_data <- tech_data %>%
        group_by(裁判, 选手姓名, 比赛项目) %>%
        summarise(原始技术分 = 技术分加分 - 技术分减分) %>%
        ungroup() %>%
        group_by(裁判) %>%
        mutate(技术满分 = max(原始技术分)) %>%
        ungroup() %>%
        mutate(技术总分 = ifelse(技术满分 > 0, 原始技术分 / 技术满分 * 60, 0)) %>%
        group_by(选手姓名, 比赛项目) %>%
        summarise(技术平均分 = if (input$exclude_extremes) {
          calculate_mean_without_extremes(技术总分)
        } else {
          mean(技术总分, na.rm = TRUE)
        })
    } else {
      tech_data <- data.frame(选手姓名 = character(), 比赛项目 = character(), 技术平均分 = numeric())
    }
    
    # 表演分计算
    if (nrow(perf_data) > 0) {
      perf_data <- perf_data %>%
        group_by(选手姓名, 比赛项目) %>%
        summarise(across(starts_with("表演分"), ~ if (input$exclude_extremes) {
          calculate_mean_without_extremes(.x)
        } else {
          mean(.x, na.rm = TRUE)
        })) %>%
        rowwise() %>%
        mutate(表演总分 = sum(c_across(starts_with("表演分"))) / 2) %>%
        ungroup() %>%
        select(选手姓名, 比赛项目, 表演分1, 表演分2, 表演分3, 表演分4, 表演分5, 表演分6, 表演分7, 表演分8, 表演总分)
    } else {
      perf_data <- data.frame(选手姓名 = character(), 比赛项目 = character(), 表演分1 = numeric(), 表演分2 = numeric(), 表演分3 = numeric(), 表演分4 = numeric(), 表演分5 = numeric(), 表演分6 = numeric(), 表演分7 = numeric(), 表演分8 = numeric(), 表演总分 = numeric())
    }
    
    # 合并技术分和表演分
    total_summary <- merge(tech_data, perf_data, by = c("选手姓名", "比赛项目"), all = TRUE)
    total_summary[is.na(total_summary)] <- 0
    total_summary$总分 <- total_summary$技术平均分 + total_summary$表演总分
    
    total_summary_global <<- total_summary
    
    output$totalScoreTable <- renderDT({
      datatable(total_summary, options = list(pageLength = 10))
    })
  })
  
  # 加载示例数据并计算总分
  observeEvent(input$load_example_data, {
    save_tech_score_data(example_tech_data)
    save_perf_score_data(example_perf_data)
    
    tech_data <- example_tech_data
    perf_data <- example_perf_data
    
    # 技术分计算
    tech_data <- tech_data %>%
      group_by(裁判, 选手姓名, 比赛项目) %>%
      summarise(原始技术分 = 技术分加分 - 技术分减分) %>%
      ungroup() %>%
      group_by(裁判) %>%
      mutate(技术满分 = max(原始技术分)) %>%
      ungroup() %>%
      mutate(技术总分 = ifelse(技术满分 > 0, 原始技术分 / 技术满分 * 60, 0)) %>%
      group_by(选手姓名, 比赛项目) %>%
      summarise(技术平均分 = if (input$exclude_extremes) {
        calculate_mean_without_extremes(技术总分)
      } else {
        mean(技术总分, na.rm = TRUE)
      })
    
    # 表演分计算
    perf_data <- perf_data %>%
      group_by(选手姓名, 比赛项目) %>%
      summarise(across(starts_with("表演分"), ~ if (input$exclude_extremes) {
        calculate_mean_without_extremes(.x)
      } else {
        mean(.x, na.rm = TRUE)
      })) %>%
      rowwise() %>%
      mutate(表演总分 = sum(c_across(starts_with("表演分"))) / 2) %>%
      ungroup() %>%
      select(选手姓名, 比赛项目, 表演分1, 表演分2, 表演分3, 表演分4, 表演分5, 表演分6, 表演分7, 表演分8, 表演总分)
    
    # 合并技术分和表演分
    total_summary <- merge(tech_data, perf_data, by = c("选手姓名", "比赛项目"), all = TRUE)
    total_summary[is.na(total_summary)] <- 0
    total_summary$总分 <- total_summary$技术平均分 + total_summary$表演总分
    
    total_summary_global <<- total_summary
    
    output$totalScoreTable <- renderDT({
      datatable(total_summary, options = list(pageLength = 10))
    })
  })
  
  # 清除所有数据的二次确认功能
  observeEvent(input$clear_data, {
    req(credentials()$user_auth)
    role <- credentials()$info$role
    
    if (role == "chief") {
      showModal(modalDialog(
        title = "确认清除所有数据",
        "您确定要清除所有数据吗？此操作无法撤销。",
        footer = tagList(
          modalButton("取消"),
          actionButton("confirm_clear", "确认")
        )
      ))
    }
  })
  
  # 确认清除所有数据
  observeEvent(input$confirm_clear, {
    clear_all_data()
    removeModal()
    showModal(modalDialog(
      title = "成功",
      "所有数据已清除。",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # 下载总分表
  output$download_total_scores <- downloadHandler(
    filename = function() {
      paste("总分表", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      total_summary <- total_summary_global
      if (is.null(total_summary)) {
        showModal(modalDialog(
          title = "错误",
          "请先计算总分。",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      exclude_extremes_text <- ifelse(input$exclude_extremes, "已去掉最高和最低分", "未去掉最高和最低分")
      total_summary <- total_summary %>% 
        mutate(技术平均分 = round(技术平均分, 2),
               表演总分 = round(表演总分, 2),
               总分 = round(总分, 2)) %>% 
        rename(`技术平均分(去最高最低)` = 技术平均分, `表演总分(去最高最低)` = 表演总分) %>%
        mutate(去除情况 = exclude_extremes_text)
      
      table_grob <- tableGrob(total_summary, theme = ttheme_default(
        core = list(
          fg_params = list(col = "white"),
          bg_params = list(fill = "blue", col = NA)
        ),
        colhead = list(
          fg_params = list(col = "white"),
          bg_params = list(fill = "blue", col = NA)
        ),
        rowhead = list(
          fg_params = list(col = "white"),
          bg_params = list(fill = "blue", col = NA)
        )
      ))
      
      png(file, width = 1000, height = 700)
      grid.draw(table_grob)
      dev.off()
    }
  )
  
  # 下载选中裁判的打分表
  output$download_selected_judge_scores <- downloadHandler(
    filename = function() {
      paste("裁判打分表_", input$judge_select, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      judge <- input$judge_select
      
      tech_data <- load_tech_score_data() %>% filter(裁判 == judge)
      perf_data <- load_perf_score_data() %>% filter(裁判 == judge)
      
      judge_data <- bind_rows(tech_data, perf_data)
      
      write.csv(judge_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # 下载所有裁判的打分表
  output$download_judge_scores <- downloadHandler(
    filename = function() {
      paste("所有裁判打分表", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tech_data <- load_tech_score_data()
      perf_data <- load_perf_score_data()
      
      temp_dir <- tempdir()
      tech_file <- file.path(temp_dir, "技术分.csv")
      perf_file <- file.path(temp_dir, "表演分.csv")
      
      write.csv(tech_data, tech_file, row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(perf_data, perf_file, row.names = FALSE, fileEncoding = "UTF-8")
      
      zip::zip(zipfile = file, files = c(tech_file, perf_file))
    }
  )
  
  # 根据选择的裁判展示其打分表
  output$selectedJudgeTable <- renderDT({
    req(input$judge_select)
    judge <- input$judge_select
    
    if (grepl("tech_judge", judge)) {
      data <- load_tech_score_data() %>% filter(裁判 == judge) %>%
        mutate(原始技术分 = 技术分加分 - 技术分减分,
               比例分 = 原始技术分 / max(原始技术分) * 60) %>%
        select(选手姓名, 比赛项目, 技术分加分, 技术分减分, 原始技术分, 比例分)
    } else {
      data <- load_perf_score_data() %>% filter(裁判 == judge) %>%
        mutate(表演总分 = rowSums(select(., starts_with("表演分")))) %>%
        select(选手姓名, 比赛项目, starts_with("表演分"), 表演总分)
    }
    
    datatable(data, options = list(pageLength = 10))
  })
}

# 创建 Shiny 应用
shinyApp(ui = ui, server = server)
