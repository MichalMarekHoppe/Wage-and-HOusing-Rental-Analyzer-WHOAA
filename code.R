## Wage and HOusing Rental Analyzer (WHORA) or WHOAA
## by Dr Michał Marek Hoppe
## version 1.2.0

print_whoaa <-  
  function (graph_width = 2500, # graph width in px
            graph_res = 400, # graph resolution
            currency = "SGD" # currency printed
            ) {
    
    suppressWarnings(library(ggplot2))
    # functions ----
    #months difference
    months_diff <- function(start_date, end_date) {
      start_year <- as.numeric(format(start_date, "%Y"))
      start_month <- as.numeric(format(start_date, "%m"))
      end_year <- as.numeric(format(end_date, "%Y"))
      end_month <- as.numeric(format(end_date, "%m"))
      year_diff <- end_year - start_year
      month_diff <- end_month - start_month
      total_month_diff <- year_diff * 12 + month_diff
      
      return(total_month_diff)
    }
    #add months
    add_months <- function(date, n) {
      year <- 
        as.numeric(format(date, "%Y"))
      month <- 
        as.numeric(format(date, "%m"))
      new_month <- 
        month + n
      new_year <- 
        year + (new_month - 1) %/% 12
      new_month <- 
        (new_month - 1) %% 12 + 1
      new_date <- 
        as.Date(paste(new_year, new_month, "01", sep = "-"))
      return(new_date)
    }
    # print graphs
    print_graphs <- function (p, width, height, res, name, folder = "",
                              svg = F) {
      folder <- ifelse(folder == "",
                       "",
                       paste0(folder, "/"))
      
      if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE, showWarnings = FALSE)
      }
      # print pdf
      ggplot2::ggsave(paste0(folder, name, ".pdf"),
                      plot = p,
                      width = width,
                      height = height,
                      dpi = res,
                      units = "px",
                      bg = "white")
      cat("Printed pdf.", "\n")
      # print svg
      if (svg) {
        ggplot2::ggsave(paste0(folder, name, ".svg"),
                        p,
                        width = width,
                        height = height,
                        dpi = res,
                        device = "svg",
                        units = "px",
                        bg = "transparent")
        cat("Printed svg.", "\n")
      }
      # print png
      png(filename = paste0(folder, name, ".png"),
          width = width,
          height = height,
          res = res,
          units = "px",
          bg = "white")
      options(warn = -1)
      print(p)
      invisible(dev.off())
      options(warn = 0)
      cat("Printed png.", "\n")
    }
    # do house-waste pie
    pie <- 
      function(center = c(0, 0), radius = 1,
               angle = 90, n = 100) {
        angle <- angle %% 360
        start_angle1 <- 90
        end_angle1 <- start_angle1 - angle
        angles1 <- seq(start_angle1, end_angle1, length.out = n)
        start_angle2 <- end_angle1
        end_angle2 <- start_angle1 - 360
        angles2 <- seq(start_angle2, end_angle2, length.out = n)
        radians1 <- angles1 * pi / 180
        radians2 <- angles2 * pi / 180
        x1 <- center[1] + radius * cos(radians1)
        y1 <- center[2] + radius * sin(radians1)
        x2 <- center[1] + radius * cos(radians2)
        y2 <- center[2] + radius * sin(radians2)
        df1 <- 
          data.frame(
            x = c(center[1], x1, center[1]),
            y = c(center[2], y1, center[2]),
            slice = "Slice 1"
          )
        df2 <- 
          data.frame(
            x = c(center[1], x2, center[1]),
            y = c(center[2], y2, center[2]),
            slice = "Slice 2"
          )
        return(list(slice1 = df1, slice2 = df2))
      }
    # pie label XY calculator
    new_xy <- function(x, y, angle_degrees, r) {
      angle_radians <- 
        (90 - angle_degrees) * pi / 180
      new_x <- 
        x + r * cos(angle_radians)
      new_y <- 
        y + r * sin(angle_radians)
      return(data.frame(x = new_x,
                        y = new_y))
    }
    
    ## Load files ----
    files <-
      list.files()[grepl("whoaa_", list.files())]
    files <-
      files[grepl(".csv", files)]
    
    for (input_file in files) {
      person <-
        gsub("^.*_", "", gsub(".csv", "", input_file))
      ## Set PARAMETERS ----
      par <- 
        list(ratio = 0.8,
             sizes = list(width = 5000,
                          printing_width = graph_width,
                          plot_h_mar = 20,
                          plot_bottom_mar = 0,
                          plot_top_mar = 50,
                          job_label = 30,
                          year_label_line = 40,
                          line_label_dist = 10,
                          labs = 400,
                          gap = 250,
                          height_2 = 1500,
                          trend_gap = 4),
             margin = list(left = 600,
                           right = 800,
                           top = 200,
                           bottom = 200),
             colours = list(background = "grey96",
                            vline_year = "white",
                            year_label_line = "grey55",
                            line_renum = "#2F4F4F",
                            line_renum_shadow = "white",
                            line_dom = "#A56958",
                            line_fraction = "grey33",
                            line_5yr = "#6A5C54",
                            line_2yr = "grey25",
                            line_trend1 = "grey33",
                            labs_text = "grey66",
                            two_fold_green = "#8A9A5B",
                            line_inflation = "darkred",
                            poly_inflation = "darkred",
                            copyright = "grey90",
                            title = "grey33",
                            year_pass = "grey50",
                            pie_1 = "#2F4F4F",
                            pie_2 = "grey75"),
             text = list(jobs = 2.5,
                         year_label_text = 2,
                         line_renum_label = 2,
                         salary_values = 2,
                         fraction_numbers = 1.75, 
                         labs = 2.5,
                         trend_years = 1.75,
                         copyright = 1.75,
                         title = 2.5,
                         year_pass = 2,
                         pie_perc = 2),
             size = list(job_points = 1.25,
                         line_label = 60,
                         curve_label_vshift = -0.66),
             thickness = list(vline_year = 0.5,
                              line_renum = 0.5,
                              line_renum_label = 0.25,
                              line_renum_shadow = 2.5,
                              year_label_line = 0.25,
                              line_fraction = 0.25,
                              line_fraction_type = "dashed",
                              line_trend = 0.33,
                              line_trend_type = "dashed",
                              lab_label = 0.25,
                              job_label_connector = 0.25,
                              job_label_connector_type = "dotted",
                              line_inflation = 0.25,
                              line_inflation_type = "dotted"),
             alpha = list(background = 1,
                          line_renum = 0.75,
                          line_dom = 0.75,
                          line_shadow = 0.5,
                          line_shadow_trends = 0.25,
                          fraction_numbers = 0.75,
                          line_trends = 0.75,
                          line_trend1 = 0.5,
                          trend_years_start_label = 0.5,
                          two_fold_green = 0.066,
                          job_label_connector = 1,
                          line_inflation = 0,
                          poly_inflation = 0.1,
                          pie = 1),
             logical = list(inflation = TRUE,
                            label_fraction_dist = 0.1,
                            pie_x = 0.05,
                            pie_y = 0.95,
                            pie_r = 0.1,
                            label_sep = 0.8,
                            currency = currency,
                            person = person))
      # scale sizes
      par$sizes$total_width <- 
        par$margin$left + par$sizes$plot_h_mar + par$sizes$width +
        par$sizes$plot_h_mar + par$margin$right
      par$sizes$printing_height <-
        par$sizes$printing_width * par$ratio
      par$sizes$total_height <-
        par$margin$bottom + par$sizes$plot_bottom_mar + 
        par$sizes$height + par$sizes$plot_top_mar + 
        par$sizes$gap + par$sizes$height_2 + par$margin$top
      par$logical$height_scaler <-
        par$sizes$total_width * par$ratio / par$sizes$total_height
      
      # scale hights
      if (T) {
        par$margin$bottom <-
          par$margin$bottom * par$logical$height_scaler
        par$sizes$plot_bottom_mar <-
          par$sizes$plot_bottom_mar * par$logical$height_scaler
        par$sizes$height <-
          par$sizes$height * par$logical$height_scaler
        par$sizes$plot_top_mar <-
          par$sizes$plot_top_mar * par$logical$height_scaler
        par$sizes$gap <- 
          par$sizes$gap * par$logical$height_scaler
        par$sizes$height_2 <-
          par$sizes$height_2 * par$logical$height_scaler
        par$margin$top <-
          par$margin$top * par$logical$height_scaler
      }
      
      ## Wrangle data ----
      renum <- 
        list()
      renum$data <- 
        read.csv(input_file)
      
      renum$data$date <-
        as.Date(paste0("20", substr(renum$data$date, 1, 2), 
                       "-", substr(renum$data$date, 3, 4),
                       "-01"))
      renum$dates$min <-
        min(renum$data$date)
      renum$dates$max <-
        as.Date(paste0(substr(format(Sys.Date(), format = "%Y%m"), 1, 4),
                       "-", substr(format(Sys.Date(), format = "%Y%m"), 5, 6),
                       "-", "01"))
      renum$dates$delta <- 
        months_diff(renum$dates$min, renum$dates$max)
      
      #setup granular sheet
      renum$a <-
        as.data.frame(matrix(ncol = 5,
                             nrow = renum$dates$delta + 1,
                             dimnames = list(NULL, 
                                             c("date", "year", "month",
                                               "renum", "dom"))))
      renum$a$date <-
        as.Date(renum$a$date)
      #fill-in
      for (i in seq_along(rownames(renum$a))) {
        if (i == 1) {
          renum$a[i, "date"] <-
            renum$dates$min
        } else {
          renum$a[i, "date"] <-
            add_months(renum$dates$min, i - 1)
        }
      }
      renum$a$year <-
        format(renum$a$date, format = "%Y")
      renum$a$month <-
        format(renum$a$date, format = "%m")
      renum$a$renum_raw <-
        renum$data[match(renum$a$date,
                         renum$data$date),"renum"]
      renum$a$renum <-
        (renum$a$renum_raw / 
           (ceiling(max(renum$a$renum_raw,
                        na.rm = TRUE)/1000) * 1000)) * par$sizes$height
      renum$a$dom_raw <-
        renum$data[match(renum$a$date,
                         renum$data$date),"dom"]
      renum$a$dom <-
        (renum$a$dom_raw / max(renum$a$renum_raw, na.rm = TRUE)) * par$sizes$height
      renum$a$job <-
        renum$data[match(renum$a$date,
                         renum$data$date),"job"]
      renum$a$job <-
        gsub(" ", "\n", renum$a$job)
      #add x
      renum$a$x <-
        (1:nrow(renum$a))-1
      renum$a$x_norm <-
        (renum$a$x / max(renum$a$x)) * par$sizes$width
      
      #fill-in NAs
      for (i in seq_along(rownames(renum$a))) {
        if (i != 1) {
          #renumeration
          if (is.na(renum$a[i, "renum"])) {
            renum$a[i, "renum"] <- 
              renum$a[i - 1, "renum"]
            renum$a[i, "renum_raw"] <- 
              renum$a[i - 1, "renum_raw"]
          }
          #living
          if (is.na(renum$a[i, "dom"])) {
            renum$a[i, "dom"] <- 
              renum$a[i - 1, "dom"]
            renum$a[i, "dom_raw"] <- 
              renum$a[i - 1, "dom_raw"]
          }
        }
      } 
      
      #create proper curves
      #salary curve
      renum$salary <-
        data.frame(x = NA, y = NA)
      for (i in seq_along(rownames(renum$a))) {
        if (i == 1) {
          renum$salary[i,] <-
            c(renum$a[i, "x_norm"], 
              renum$a[i, "renum"])
        } else {
          renum$salary <-
            rbind(renum$salary,
                  c(renum$a[i, "x_norm"],
                    renum$a[i, "renum"]))
          if (i != max(seq_along(rownames(renum$a))) &
              renum$a[i, "renum"] != renum$a[i + 1, "renum"]) {
            renum$salary <-
              rbind(renum$salary,
                    c(renum$a[i + 1, "x_norm"],
                      renum$a[i, "renum"]))
          }
        }
      }
      #dom curve
      renum$dom <-
        data.frame(x = NA, y = NA)
      for (i in seq_along(rownames(renum$a))) {
        if (i == 1) {
          renum$dom[i,] <-
            c(renum$a[i, "x_norm"], 
              renum$a[i, "dom"])
        } else {
          renum$dom <-
            rbind(renum$dom,
                  c(renum$a[i, "x_norm"],
                    renum$a[i, "dom"]))
          if (i != max(seq_along(rownames(renum$a))) &
              renum$a[i, "dom"] != renum$a[i + 1, "dom"]) {
            renum$dom <-
              rbind(renum$dom,
                    c(renum$a[i + 1, "x_norm"],
                      renum$a[i, "dom"]))
          }
        }
      }
      
      #add fraction data
      renum$a$fraction <-
        renum$a$dom / renum$a$renum
      renum$a$fraction_norm <-
        renum$a$fraction * max(renum$a$renum)
      
      # add past intervals 
      par$logical$years <-
        c("max",
          seq(2, c(as.numeric(max(renum$a$year)) - 
                     as.numeric(min(renum$a$year))), by = 3))
      par$logical$years <-
        par$logical$years[1:(length(par$logical$years) - 1)]
      
      par$colours$trend_line_years <-
        colorRampPalette(c(par$colours$line_renum,
                           par$colours$line_dom))(length(par$logical$years))
      
      ## add year trend data ----
      for (y in par$logical$years) {
        if (y != "max") {
          y <-
            as.double(y)
          for (i in 1:nrow(renum$a)) {
            if (i > y * 12) {
              renum$a[i, "yrXXX"] <-
                renum$a[i, "renum"] / renum$a[(i - y * 12), "renum"]
            }
          }
          colnames(renum$a)[grep("XXX", colnames(renum$a))] <-
            paste0("yr", y)
        } else if (y == "max"){
          for (i in 1:nrow(renum$a)) {
            renum$a[i, "yrXXX"] <-
              renum$a[i, "renum"] / renum$a[1, "renum"]
          }
        }
        colnames(renum$a)[grep("XXX", colnames(renum$a))] <-
          paste0("yr", y)
      }
      # normalize
      cols <- 
        colnames(renum$a)[grep("yr", colnames(renum$a))]
      for (y in cols) {
        renum$a[, paste0(y, "_norm")] <-
          (renum$a[, y] - 1) / (max(renum$a[,cols],
                                    na.rm = TRUE) - 1) * 0.95 *
          par$sizes$height_2
        renum$a[, paste0(y, "_norm")][!is.na(renum$a[, paste0(y, "_norm")])] <- 
          renum$a[, paste0(y, "_norm")][!is.na(renum$a[, paste0(y, "_norm")])] + 
          par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap +
          grep(y, cols) * par$sizes$trend_gap
      }
      
      renum$a$job_y <-
        ifelse(renum$a$job != "",
               renum$a$renum,
               NA)
      
      ## inflation ----
      if (par$logical$inflation) {
        inflation <-
          read.csv("inflation.csv")
        rownames(inflation) <- NULL
        inflation <-
          inflation[inflation$years >= min(renum$a$year) &
                      inflation$years <= max(renum$a$year),]
        inflation$rate <-
          NA
        for (i in 1:nrow(inflation)) {
          if (i == 1) {
            inflation[i, "rate"] <-
              1
          } else {
            inflation[i, "rate"] <-
              inflation[i - 1, "rate"] * (1 + inflation[i, "inflation"]/100)
          }
        }
        #inflation curve
        renum$inflation <-
          data.frame(x = NA, y = NA)
        for (i in seq_along(rownames(renum$a))) {
          if (i == 1) {
            renum$inflation[i,] <-
              c(renum$a[i, "x_norm"], 
                inflation[match(renum$a[i, "year"],
                                inflation$years), "rate"])
          } else {
            renum$inflation <-
              rbind(renum$inflation,
                    c(ifelse(i < (nrow(renum$a) - 1),
                             mean(c(renum$a[i, "x_norm"], renum$a[i + 1, "x_norm"])),
                             renum$a[i, "x_norm"]), 
                      inflation[match(renum$a[i, "year"],
                                      inflation$years), "rate"]))
            if (i != max(seq_along(rownames(renum$a))) &
                renum$a[i, "year"] != renum$a[i + 1, "year"]) {
              renum$inflation <-
                rbind(renum$inflation,
                      c(mean(c(renum$a[i, "x_norm"], renum$a[i + 1, "x_norm"])),
                        inflation[match(renum$a[i + 1, "year"],
                                        inflation$years), "rate"]))
            }
            if (i == max(seq_along(rownames(renum$a)))) {
              renum$inflation <-
                rbind(renum$inflation,
                      c(renum$a[i, "x_norm"] + par$sizes$plot_h_mar,
                        renum$inflation[nrow(renum$inflation), "y"]))
            }
          }
        }
        renum$inflation$y <-
          ((renum$inflation$y - 1) / (max(renum$a[, cols], na.rm = TRUE) - 1) * 0.95 *
             par$sizes$height_2) + par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap
        renum$inflation$y[renum$inflation$y < renum$inflation$y[1]] <-
          renum$inflation$y[1]
      }
      
      ## PLOT: START ----
      ## PLOT: base ----
      p <- ggplot() + 
        theme_void() +
        scale_x_continuous(limits = c(-par$margin$left - par$sizes$plot_h_mar,
                                      par$sizes$width + par$sizes$plot_h_mar + 
                                        par$margin$right),
                           expand = c(0, 0)) +
        scale_y_continuous(limits = c(-par$margin$bottom - par$sizes$plot_bottom_mar,
                                      par$sizes$height + par$sizes$plot_top_mar + 
                                        par$sizes$gap + 
                                        par$sizes$height_2 +  
                                        par$margin$top),
                           expand = c(0, 0))
      ## PLOT: background 1 ----
      p[["layers"]][[length(p[["layers"]]) + 1]] <- 
        geom_rect(aes(xmin = -par$sizes$plot_h_mar,
                      xmax = par$sizes$width + par$sizes$plot_h_mar, 
                      ymin = -par$sizes$plot_bottom_mar,
                      ymax = par$sizes$height + par$sizes$plot_top_mar), 
                  fill = par$colours$background,
                  alpha = par$alpha$background)
      #watermark
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(data = data.frame(c(NA, NA)),
                  aes(par$sizes$width + par$sizes$plot_h_mar,
                      0,
                      label = "created by Michał Marek Hoppe \u00A9 2024"),
                  size = par$text$copyright, hjust = 0, vjust = 1,
                  angle = 90,
                  colour = par$colours$copyright)
      
      # background 2
      p[["layers"]][[length(p[["layers"]]) + 1]] <- 
        geom_rect(aes(xmin = -par$sizes$plot_h_mar,
                      xmax = par$sizes$width + par$sizes$plot_h_mar, 
                      ymin = par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap,
                      ymax = par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap +
                        + par$sizes$height_2), 
                  fill = par$colours$background,
                  alpha = par$alpha$background)
      
      ## PLOT: year vlines ----
      for (i in seq_along(rownames(renum$a))) {
        if (i != nrow(renum$a) & 
            renum$a[i, "year"] != renum$a[i + 1, "year"]) {
          # v-line
          p[["layers"]][[length(p[["layers"]])+1]] <- 
            annotate("segment",
                     x = mean(c(renum$a[i, "x_norm"], renum$a[i + 1, "x_norm"])), 
                     xend = mean(c(renum$a[i, "x_norm"], renum$a[i + 1, "x_norm"])),
                     y = -par$sizes$plot_bottom_mar,
                     yend = par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap +
                       par$sizes$height_2,
                     color = par$colours$vline_year,
                     linewidth = par$thickness$vline_year)
        } 
      }
      
      ## PLOT: dom line ----
      #shadow
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_line(data = renum$dom, aes(x + 0.25, y),
                  colour = par$colours$line_renum_shadow,
                  linewidth = par$thickness$line_renum_shadow,
                  alpha = par$alpha$line_shadow)
      #line
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_line(data = renum$dom, aes(x + 0.25, y),
                  colour = par$colours$line_dom,
                  linewidth = par$thickness$line_renum,
                  alpha = par$alpha$line_dom)
      
      ## PLOT: year labels ----
      for (i in seq_along(rownames(renum$a))) {
        if (renum$a[i, "year"] != renum$a[i + 1, "year"] | 
            is.na(renum$a[i, "year"] != renum$a[i + 1, "year"])) {
          # year label
          p[["layers"]][[length(p[["layers"]])+1]] <- 
            annotate("segment",
                     x = min(renum$a[grep(renum$a[i, "year"], 
                                          renum$a[, "year"]), "x_norm"]), 
                     xend = max(renum$a[grep(renum$a[i, "year"], 
                                             renum$a[, "year"]), "x_norm"]),
                     y = -par$sizes$plot_bottom_mar - par$sizes$year_label_line,
                     yend = -par$sizes$plot_bottom_mar - par$sizes$year_label_line,
                     color = par$colours$year_label_line,
                     linewidth = par$thickness$year_label_line)
          # year label text
          p[["layers"]][[length(p[["layers"]])+1]] <- 
            geom_text(aes(x = !!mean(renum$a[grep(renum$a[i, "year"], 
                                                  renum$a[, "year"]), "x_norm"]),
                          y = -par$sizes$plot_bottom_mar - 
                            par$sizes$year_label_line - par$sizes$year_label_line,
                          label = !!renum$a[i, "year"]),
                      size = par$text$year_label_text,
                      vjust = 1, hjust = 0.5,
                      colour = par$colours$year_label_line)
        }
      }
      ## PLOT: y-scale #1 ----
      for (i in 1:(ceiling(max(renum$a$renum_raw)/1000)+1)-1) {
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_text(data = data.frame(NA),
                    aes(-par$sizes$plot_h_mar,
                        !!(i*1000/max(renum$a$renum_raw) * par$sizes$height),
                        label = !!paste0(ifelse(i == 0, 
                                                0,
                                                i*1000), " -")),
                    size = par$text$year_label_text,
                    hjust = 1, vjust = 0,
                    colour = par$colours$year_label_line,
                    na.rm = TRUE)
      }
      # add y-scale #2
      for (i in seq(from = 1,
                    to = floor(max(renum$a$yrmax) / 0.5) * 0.5,
                    by = 0.5)) {
        p[["layers"]][[length(p[["layers"]]) + 1]] <-
          geom_text(data = data.frame(NA),
                    aes(-par$sizes$plot_h_mar,
                        !!(((i - 1)/(max(renum$a[, cols],
                                         na.rm = TRUE) - 1)) * 0.95 * par$sizes$height_2) +
                          par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap,
                        label = !!paste0(i, " -")),
                    size = par$text$year_label_text,
                    hjust = 1, vjust = 0,
                    colour = par$colours$year_label_line,
                    na.rm = TRUE)
      }
      ## 2-fold salary increase base green 
      for (f in seq(2, floor(max(renum$a[,cols], na.rm = TRUE)))) {
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          geom_rect(aes(xmin = -par$sizes$plot_h_mar,
                        xmax = par$sizes$width + par$sizes$plot_h_mar, 
                        ymin = !!(((f - 1)/(max(renum$a[, cols],
                                                na.rm = TRUE) - 1)) * 0.95 * par$sizes$height_2) +
                          par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap,
                        ymax = par$sizes$height + par$sizes$plot_top_mar + 
                          par$sizes$gap + par$sizes$height_2), 
                    fill = par$colours$two_fold_green,
                    alpha = par$alpha$two_fold_green)
      }
      
      ## PLOT: years+ pass ----
      p[["layers"]][[length(p[["layers"]]) + 1]] <- 
        geom_text(data = data.frame(x = renum$a[renum$a$month == "07" &
                                                  renum$a$year != min(renum$a$year), "x_norm"] -
                                      par$sizes$width/nrow(renum$a)/2,
                                    delta = sapply(renum$a[renum$a$month == "07" &
                                                             renum$a$year != min(renum$a$year), "year"],
                                                   as.double) -
                                      as.double(min(renum$a$year))),
                  aes(x, par$sizes$height + par$sizes$plot_top_mar +
                        par$sizes$gap + par$sizes$height_2 + par$sizes$line_label_dist,
                      label = paste0(delta, "+")), 
                  hjust = 0, vjust = 0,
                  colour = par$colours$year_pass,
                  size = par$text$year_pass)
      
      ## PLOT: y-1 lab ----
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(data = data.frame(NA),
                  aes(-par$sizes$plot_h_mar - par$sizes$labs,
                      par$sizes$plot_top_mar + 
                        (par$sizes$height) / 2,
                      label = paste0("Remuneration (",
                                     par$logical$currency,")")),
                  size = par$text$labs,
                  hjust = 0.5, vjust = 0.25, angle = 90,
                  colour = par$colours$labs_text,
                  na.rm = TRUE)
      ## PLOT: y-2 lab ----
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(data = data.frame(NA),
                  aes(-par$sizes$plot_h_mar -par$sizes$labs,
                      par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap +
                        par$sizes$height_2 / 2,
                      label = "Salary increase (fold)"),
                  size = par$text$labs,
                  hjust = 0.5, vjust = 0.25, angle = 90,
                  colour = par$colours$labs_text,
                  na.rm = TRUE)
      ## PLOT: x lab ----
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(data = data.frame(NA),
                  aes(par$sizes$width/2,
                      -par$sizes$labs * 0.75,
                      label = "Years"),
                  size = par$text$labs,
                  hjust = 0.5, vjust = 0.33,
                  colour = par$colours$labs_text,
                  na.rm = TRUE)
      
      ## RENUM curve ---- 
      #shadow
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_line(data = renum$salary, aes(x, y),
                  colour = par$colours$line_renum_shadow,
                  linewidth = par$thickness$line_renum_shadow,
                  alpha = par$alpha$line_shadow)
      #line
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_line(data = renum$salary, aes(x, y),
                  colour = par$colours$line_renum,
                  linewidth = par$thickness$line_renum,
                  alpha = par$alpha$line_renum)
      #job points
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_point(data = renum$a, aes(x_norm, job_y),
                   colour = par$colours$line_renum,
                   size = par$size$job_points,
                   na.rm = TRUE)
      
      ## PLOT: job descriptions ----
      for (i in c(1:length(renum$a$job))[!is.na(renum$a$job)]) {
        #job dotted connecting lines
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          annotate("segment",
                   x = renum$a[i, "x_norm"], 
                   xend = renum$a[i, "x_norm"] + 
                     (par$sizes$height + par$sizes$plot_top_mar +
                        par$sizes$job_label - renum$a[i, "job_y"]) / 2,
                   y = renum$a[i, "job_y"],
                   yend = par$sizes$height + par$sizes$plot_top_mar +
                     par$sizes$job_label,
                   color = par$colours$line_renum,
                   linewidth = par$thickness$job_label_connector,
                   linetype = par$thickness$job_label_connector_type,
                   alpha = par$alpha$job_label_connector)
        #job lables
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_text(data = data.frame(c(NA, NA)), 
                    aes(!!(renum$a[i, "x_norm"] + 
                             (par$sizes$height + par$sizes$plot_top_mar +
                                par$sizes$job_label - renum$a[i, "job_y"]) / 2),
                        !!(par$sizes$height + par$sizes$plot_top_mar +
                             par$sizes$job_label)),
                    label = renum$a[i, "job"], lineheight = 0.75,
                    size = par$text$jobs, hjust = -0.05, vjust = 0,
                    colour = par$colours$line_renum,
                    na.rm = TRUE)
      }
      
      #add salary labels
      collect <- TRUE
      label <- list()
      for (i in 1:nrow(renum$a)) {
        if (collect) {
          label$salary <-
            renum$a[i, "renum_raw"]
          label$y <- 
            renum$a[i, "renum"]
          label$start <- renum$a[i, "x_norm"]
          collect <- FALSE
        }
        if (renum$a[i, "renum_raw"] == label$salary) {
          label$stop <- renum$a[i, "x_norm"]
          if (i == nrow(renum$a)) {
            p[["layers"]][[length(p[["layers"]])+1]] <-
              geom_text(data = data.frame(x = NA,
                                          y = NA),
                        aes(x = !!mean(c(label$start, label$stop)),
                            y = !!label$y,
                            label = !!label$salary),
                        size = par$text$salary_values,
                        vjust = par$size$curve_label_vshift, hjust = 0.5,
                        colour = par$colours$line_renum)
          }
        } else {
          p[["layers"]][[length(p[["layers"]])+1]] <-
            geom_text(data = data.frame(x = NA,
                                        y = NA),
                      aes(x = !!mean(c(label$start, label$stop)),
                          y = !!label$y,
                          label = !!label$salary),
                      size = par$text$salary_values,
                      vjust = par$size$curve_label_vshift, hjust = 0.5,
                      colour = par$colours$line_renum)
          collect <- TRUE
        }
      }
      
      #add fraction labels
      collect <- TRUE
      label <- list()
      for (i in 1:nrow(renum$a)) {
        if (collect) {
          label$salary <-
            renum$a[i, "fraction"]
          label$y <- 
            renum$a[i, "fraction_norm"]
          label$start <- renum$a[i, "x_norm"]
          collect <- FALSE
        }
        if (renum$a[i, "fraction"]== label$salary) {
          label$stop <- renum$a[i, "x_norm"]
          if (i == nrow(renum$a)) {
            p[["layers"]][[length(p[["layers"]])+1]] <-
              geom_text(data = data.frame(x = NA,
                                          y = NA),
                        aes(x = !!mean(c(label$start, label$stop)),
                            y = !!label$y,
                            label = !!paste0(round(label$salary * 100, 0), "%")),
                        size = par$text$fraction_numbers,
                        vjust = par$size$curve_label_vshift, hjust = 0.5,
                        colour = par$colours$line_fraction,
                        alpha = par$alpha$fraction_numbers)
          }
        } else {
          p[["layers"]][[length(p[["layers"]])+1]] <-
            geom_text(data = data.frame(x = NA,
                                        y = NA),
                      aes(x = !!mean(c(label$start, label$stop)),
                          y = !!label$y,
                          label = !!paste0(round(label$salary * 100, 0), "%")),
                      size = ifelse(round(label$salary * 100, 0) != 0, 
                                    par$text$fraction_numbers,
                                    0),
                      vjust = par$size$curve_label_vshift, hjust = 0.5,
                      colour = par$colours$line_fraction,
                      alpha = par$alpha$fraction_numbers)
          collect <- TRUE
        }
      }
      
      ## fraction line ----
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_line(data = renum$a, aes(x_norm, fraction_norm),
                  colour = par$colours$line_fraction,
                  linetype = par$thickness$line_fraction_type,
                  linewidth = par$thickness$line_fraction)
      
      ## create plot 1 separated labels ----
      plot_1_labels <-
        data.frame(y = c(renum$a[nrow(renum$a), "renum"],
                         renum$a[nrow(renum$a), "dom"],
                         renum$a[nrow(renum$a), "fraction_norm"]),
                   y_sep = c(renum$a[nrow(renum$a), "renum"],
                             renum$a[nrow(renum$a), "dom"],
                             renum$a[nrow(renum$a), "fraction_norm"]),
                   label = c("Remuneration",
                             "Housing\nexpenses",
                             "Income spent\non housing"),
                   colour = c(par$colours$line_renum,
                              par$colours$line_dom,
                              par$colours$line_fraction))
      plot_1_labels <-
        plot_1_labels[order(plot_1_labels$y,
                            decreasing = TRUE),]
      for (i in seq_along(rownames(plot_1_labels))) {
        if (i < nrow(plot_1_labels)) {
          while ((plot_1_labels[i, "y_sep"] - plot_1_labels[i + 1, "y_sep"]) < 
                 par$sizes$height * par$logical$label_fraction_dist) {
            plot_1_labels[i, "y_sep"] <- 
              plot_1_labels[i, "y_sep"] + 
              par$sizes$height * par$logical$label_fraction_dist/10
            plot_1_labels[i + 1, "y_sep"] <- 
              plot_1_labels[i + 1, "y_sep"] - 
              par$sizes$height * par$logical$label_fraction_dist/10
          }
        }
      }
      # label
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        geom_text(data = plot_1_labels,
                  aes(x = par$sizes$width + par$sizes$plot_h_mar + 
                        par$sizes$line_label_dist + par$size$line_label + 
                        par$sizes$line_label_dist,
                      y = y_sep,
                      label = label,
                      colour = colour),
                  size = par$text$line_renum_label,
                  vjust = 0.4, hjust = 0,
                  alpha = par$alpha$line_renum,
                  lineheight = par$logical$label_sep,
                  show.legend = FALSE) 
      p <- p + scale_color_identity()
      # connectors
      for (i in seq_along(rownames(plot_1_labels))) {
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          annotate("segment",
                   x = par$sizes$width + par$sizes$plot_h_mar + par$sizes$line_label_dist, 
                   xend = par$sizes$width + par$sizes$plot_h_mar + 
                     par$sizes$line_label_dist + par$size$line_label,
                   y = plot_1_labels[i, "y"],
                   yend = plot_1_labels[i, "y_sep"],
                   color = plot_1_labels[i, "colour"],
                   linewidth = par$thickness$line_renum_label,
                   alpha = par$alpha$line_renum)
      }
      ## create plot 2 separated labels ----
      plot_2_labels <-
        data.frame(y = c(unlist(renum$a[nrow(renum$a), intersect(grep("yr", colnames(renum$a)),
                                                                 grep("norm", colnames(renum$a)))]),
                         renum$inflation[nrow(renum$inflation), "y"]),
                   y_sep = c(unlist(renum$a[nrow(renum$a), intersect(grep("yr", colnames(renum$a)),
                                                                     grep("norm", colnames(renum$a)))]),
                             renum$inflation[nrow(renum$inflation), "y"]),
                   label = c(ifelse(par$logical$years != "max",
                                    paste0(gsub("yr", "", par$logical$years), "-years"),
                                    "Max"),
                             "Compounding\ninflation"),
                   colour = c(par$colours$trend_line_years,
                              par$colours$poly_inflation))
      plot_2_labels <-
        plot_2_labels[order(plot_2_labels$y,
                            decreasing = TRUE),]
      n <- 1
      while (n == 1) {
        n <- 0
        for (i in seq_along(rownames(plot_2_labels))) {
          if (i < nrow(plot_2_labels)) {
            while ((plot_2_labels[i, "y_sep"] - plot_2_labels[i + 1, "y_sep"]) < 
                   par$sizes$height_2 * par$logical$label_fraction_dist) {
              plot_2_labels[i, "y_sep"] <- 
                plot_2_labels[i, "y_sep"] + 
                par$sizes$height_2 * par$logical$label_fraction_dist/10
              plot_2_labels[i + 1, "y_sep"] <- 
                plot_2_labels[i + 1, "y_sep"] - 
                par$sizes$height_2 * par$logical$label_fraction_dist/10
              n <- 1
            }
          }
        }
      }
      # label
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        geom_text(data = plot_2_labels,
                  aes(x = par$sizes$width + par$sizes$plot_h_mar + 
                        par$sizes$line_label_dist + par$size$line_label + 
                        par$sizes$line_label_dist,
                      y = y_sep,
                      label = label,
                      colour = colour),
                  size = par$text$line_renum_label,
                  vjust = 0.4, hjust = 0,
                  alpha = par$alpha$line_renum,
                  lineheight = par$logical$label_sep,
                  show.legend = FALSE) 
      # connectors
      for (i in seq_along(rownames(plot_2_labels))) {
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          annotate("segment",
                   x = par$sizes$width + par$sizes$plot_h_mar + par$sizes$line_label_dist, 
                   xend = par$sizes$width + par$sizes$plot_h_mar + 
                     par$sizes$line_label_dist + par$size$line_label,
                   y = plot_2_labels[i, "y"],
                   yend = plot_2_labels[i, "y_sep"],
                   color = plot_2_labels[i, "colour"],
                   linewidth = par$thickness$line_renum_label,
                   alpha = par$alpha$line_renum)
      }
      
      ## trend top curves ----
      ## inflation ----
      if (par$logical$inflation) {
        #poly
        p[["layers"]][[length(p[["layers"]])+1]] <- 
          geom_polygon(data = rbind(renum$inflation,
                                    c(renum$inflation[nrow(renum$inflation), "x"],
                                      renum$inflation[1, "y"]),
                                    c(renum$inflation[1, "x"],
                                      renum$inflation[1, "y"])),
                       aes(x, y), 
                       fill = par$colours$poly_inflation,
                       alpha = par$alpha$poly_inflation)
        
        #line
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_line(data = renum$inflation, aes(x, y),
                    colour = par$colours$line_inflation,
                    linewidth = par$thickness$line_inflation,
                    linetype = par$thickness$line_inflation_type,
                    alpha = par$alpha$line_inflation)
      }
      ## top curves
      n <- 0
      for(y in par$logical$years) {
        renum$a$numbers <- 
          renum$a[, paste0("yr", y, "_norm")] #+ (n * par$sizes$trend_gap)
        #line
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_line(data = renum$a, aes(x_norm,
                                        numbers),
                    colour = par$colours$trend_line_years[grep(y, par$logical$years)],
                    linewidth = par$thickness$line_trend,
                    alpha = par$alpha$line_trends,
                    na.rm = TRUE)
        n <- n +1
      }
      #add start points
      n <- 0
      for(y in par$logical$years) {
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_point(data = data.frame(c(NA, NA)),
                     aes(!!renum$a[,"x_norm"][!is.na(renum$a[, paste0("yr",
                                                                      y, "_norm")])][1],
                         !!(renum$a[, paste0("yr", y, "_norm")][!is.na(renum$a[, paste0("yr",
                                                                                        y,"_norm")])][1])),
                     colour = par$colours$trend_line_years[grep(y, par$logical$years)],
                     size = par$size$job_points)
        #trend lables
        p[["layers"]][[length(p[["layers"]])+1]] <-
          geom_text(data = data.frame(c(NA, NA)),
                    aes(!!renum$a[,"x_norm"][!is.na(renum$a[, paste0("yr",
                                                                     y, "_norm")])][1],
                        !!(renum$a[, paste0("yr", y, "_norm")][!is.na(renum$a[, paste0("yr",
                                                                                       y,"_norm")])][1]),
                        label = !!ifelse(y == "max",
                                         "Max trend start",
                                         paste0(y, "-year trend start"))),
                    size = par$text$trend_years, hjust = 0, vjust = -0.75,
                    colour = par$colours$trend_line_years[grep(y, par$logical$years)],
                    alpha = par$alpha$trend_years_start_label, 
                    na.rm = TRUE)
        n <- n +1
      }
      # baseline
      p[["layers"]][[length(p[["layers"]])+1]] <- 
        annotate("segment",
                 x = -par$sizes$plot_h_mar, 
                 xend = par$sizes$width + par$sizes$plot_h_mar,
                 y = par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap,
                 yend = par$sizes$height + par$sizes$plot_top_mar + par$sizes$gap,
                 color = par$colours$line_trend1,
                 linewidth = par$thickness$line_renum_label,
                 linetype = par$thickness$line_trend_type,
                 alpha = par$alpha$line_trend1)
      
      ## PLOT: title ----
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(data = data.frame(c(NA, NA)),
                  aes(par$sizes$plot_h_mar * 3,
                      par$sizes$height + par$sizes$plot_top_mar +
                        par$sizes$gap + par$sizes$height_2 - par$sizes$plot_h_mar * 3,
                      label = paste0(par$logical$person, "'s ",
                                     "Wage and HOusing Rental AnAlyzer (WHOAA)")),
                  size = par$text$title, hjust = 0, vjust = 1,
                  colour = par$colours$title)
      
      ## PLOT: pie ----
      house_frac <-
        sum(renum$a$dom_raw)/sum(renum$a$renum_raw)
      
      pie_slices <- 
        pie(center = c(par$sizes$width * par$logical$pie_x,
                       par$sizes$height * par$logical$pie_y),
            radius = par$sizes$height * par$logical$pie_r,
            angle = house_frac * 360)
      
      p <-
        p + 
        geom_polygon(data = pie_slices$slice1,
                     aes(x = x, y = y),
                     color = NA, fill = par$colours$pie_1,
                     show.legend = FALSE, 
                     alpha = par$alpha$pie) +
        geom_polygon(data = pie_slices$slice2,
                     aes(x = x, y = y),
                     color = NA, fill = par$colours$pie_2,
                     show.legend = FALSE,
                     alpha = par$alpha$pie) +
        geom_text(data = data.frame(x = par$sizes$width * par$logical$pie_x ,
                                    y = par$sizes$height * par$logical$pie_y +
                                      par$sizes$height * par$logical$pie_r +
                                      par$sizes$year_label_line),
                  aes(x = x, y = y),
                  label = "Total housing\ncosts:",
                  size = par$text$pie_perc,
                  colour = par$colours$pie_1,
                  lineheight = par$logical$label_sep,
                  vjust = 0, hjust = 0.5) +
        geom_text(data = new_xy(par$sizes$width * par$logical$pie_x,
                                par$sizes$height * par$logical$pie_y,
                                house_frac * 360 / 2,
                                par$sizes$height * par$logical$pie_r * 1.1),
                  aes(x = x, y = y),
                  label = paste0(round(house_frac * 100), "%"),
                  size = par$text$pie_perc,
                  colour = par$colours$pie_1,
                  vjust = 0.5, hjust = 0) 
      suppressWarnings(print_graphs(p,
                                    par$sizes$printing_width,
                                    par$sizes$printing_height,
                                    graph_res, paste0("WHOAA_", person),
      ))
    }
  }

print_whoaa()
