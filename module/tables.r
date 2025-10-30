box::use(
    tibble[as_tibble],
    dplyr[mutate, filter, select, across],
    tidyselect[where, everything]
)

center_text = function(text, width, pos = FALSE) {
    if (pos) {
        padding = pmax(width - nchar(text), 0)
    } else {
        padding = width - nchar(text)
    }
    left_pad = floor(padding / 2)
    right_pad = ceiling(padding / 2)
    paste0(paste0(rep(" ", left_pad), collapse = ""), text, paste0(rep(" ", right_pad), collapse = ""))
}

format_row = function(row, col_widths, left_align_first = FALSE, pos = FALSE) {
    formatted = sapply(seq_along(row), function(i) {
        if (i == 1 && left_align_first) {
            format(row[i], width = col_widths[i], justify = "left")
        } else {
            center_text(row[i], col_widths[i], pos = pos)
        }
    })
    paste0("  ", paste(formatted, collapse = "   "), "  ")
}

draw_table = function(data, 
                      left_align_first = FALSE, 
                      pos = FALSE, 
                      digits = 3,
                      digits_by_col = NULL,
                      scientific = FALSE,
                      na_print = "",
                      min_width = NULL,
                      border_char = "─",
                      show_row_names = FALSE) {
    
    data = as_tibble(data)
    
    if (show_row_names) {
        data = data |> 
            mutate(row_names = rownames(as.data.frame(data)), .before = 1)
    }
    
    data = mutate(data, across(where(is.factor), as.character))
    data = mutate(data, across(everything(), \(x) ifelse(is.na(x), na_print, x)))
    
    if (!is.null(digits_by_col)) {
        for (col_name in names(digits_by_col)) {
            if (col_name %in% names(data) && is.numeric(data[[col_name]])) {
                data[[col_name]] = if (scientific) {
                    format(data[[col_name]], digits = digits_by_col[[col_name]], scientific = TRUE)
                } else {
                    format(round(data[[col_name]], digits = digits_by_col[[col_name]]), 
                           nsmall = digits_by_col[[col_name]])
                }
            }
        }
    }
    
    data = mutate(data, across(where(is.numeric), function (x) {
        if (all(x %% 1 == 0, na.rm = TRUE)) {
            as.character(x)
        } else if (scientific) {
            format(x, digits = digits, scientific = TRUE)
        } else {
            format(round(x, digits = digits), nsmall = digits)
        }
    }))
    
    # Then convert every columns to string
    data = mutate(data, across(everything(), as.character))
    
    # Calculate column widths
    col_names = colnames(data)
    data_chars = as.matrix(data)
    
    col_widths = pmax(nchar(col_names), apply(data_chars, 2, function(x) max(nchar(x))))
    
    # Apply minimum width if specified
    if (!is.null(min_width)) {
        col_widths = pmax(col_widths, min_width)
    }
    
    # Calculate total width for border
    total_width = sum(as.numeric(col_widths)) + 3 * (length(col_widths) - 1) + 4
    
    # Create border
    horizontal_line = paste0(rep(border_char, total_width), collapse = "")
    
    # Print table
    cat(horizontal_line, "\n")
    cat(format_row(col_names, col_widths, left_align_first = left_align_first, pos = pos), "\n")
    cat(horizontal_line, "\n")
    apply(data_chars, 1, function(row) 
        cat(format_row(row, col_widths, left_align_first = left_align_first, pos = pos), "\n"))
    cat(horizontal_line, "\n")
}
