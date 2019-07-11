##### Vivid house colours (called by other scripts)

# Define house colours
vivid_base <- c(turquoise = rgb(red = 32, green = 196, blue = 244, maxColorValue = 255), #turquoise
                orange = rgb(red = 247, green = 149, blue = 91, maxColorValue = 255), #orange
                lavender = rgb(red = 163, green = 154, blue = 183, maxColorValue = 255), #lavender
                light_green = rgb(red = 159, green = 209, blue = 139, maxColorValue = 255), #light_green
                light_yellow = rgb(red = 255, green = 219, blue = 87, maxColorValue = 255), #light_yellow
                teal = rgb(red = 86, green = 148, blue = 151, maxColorValue = 255), #teal
                gray = rgb(red = 128, green = 128, blue = 128, maxColorValue = 255)) #gray

vivid_light_80 <- c(turquoise_light80 = rgb(red = 210, green = 243, blue = 253, maxColorValue = 255), #turquoise 80% lighter
                    orange_light80 = rgb(red = 253, green = 233, blue = 221, maxColorValue = 255), #orange 80% lighter
                    lavender_light80 = rgb(red = 236, green = 234, blue = 240, maxColorValue = 255), #lavender 80% lighter
                    light_green_light80 = rgb(red = 235, green = 245, blue = 231, maxColorValue = 255), #light_green 80% lighter
                    light_yellow_light80 = rgb(red = 255, green = 248, blue = 221, maxColorValue = 255), #light_yellow 80% lighter
                    teal_light80 = rgb(red = 220, green = 234, blue = 235, maxColorValue = 255), #teal 80% lighter
                    gray_light80 = rgb(red = 230, green = 230, blue = 230, maxColorValue = 255)) #gray 80% lighter


vivid_light_60 <- c(turquoise_light60 = rgb(red = 166, green = 232, blue = 251, maxColorValue = 255), #turquoise 60% lighter
                    orange_light60 = rgb(red = 252, green = 213, blue = 188, maxColorValue = 255), #orange 60% lighter
                    lavender_light60 = rgb(red = 218, green = 215, blue = 225, maxColorValue = 255), #lavender 60% lighter
                    light_green_light60 = rgb(red = 216, green = 236, blue = 208, maxColorValue = 255), #light green 60% lighter
                    light_yellow_light60 = rgb(red = 255, green = 240, blue = 187, maxColorValue = 255), #light yellow 60% lighter
                    teal_light60 = rgb(red = 185, green = 214, blue = 215, maxColorValue = 255), #teal 60% lighter
                    gray_light60 = rgb(red = 204, green = 204, blue = 204, maxColorValue = 255)) #gray 60% lighter

vivid_light_40 <- c(turquoise_light40 = rgb(red = 121, green = 220, blue = 249, maxColorValue = 255), #turquoise 40% lighter
                    orange_light40 = rgb(red = 251, green = 191, blue = 155, maxColorValue = 255), #orange 40% lighter
                    lavender_light40 = rgb(red = 200, green = 194, blue = 211, maxColorValue = 255), #lavender 40% lighter
                    light_green_light40 = rgb(red = 197, green = 227, blue = 185, maxColorValue = 255), #light_green 40% lighter
                    light_yellow_light40 = rgb(red = 255, green = 232, blue = 153, maxColorValue = 255), #light_yellow 40% lighter
                    teal_light40 = rgb(red = 151, green = 193, blue = 196, maxColorValue = 255), #teal 40% lighter
                    gray_light40 = rgb(red = 179, green = 179, blue = 179, maxColorValue = 255)) #gray 40% lighter

vivid_dark_25 <- c(turquoise_dark_25 = rgb(red = 10, green = 154, blue = 197, maxColorValue = 255), #turquoise 25% darker
                   orange_dark_25 = rgb(red = 241, green = 98, blue = 12, maxColorValue = 255), #orange 25% darker
                   lavender_dark_25 = rgb(red = 118, green = 105, blue = 148, maxColorValue = 255), #lavender 25% darker
                   light_green_dark_25 = rgb(red = 106, green = 185, blue = 77, maxColorValue = 255), #light_green 25% darker
                   light_yellow_dark_25 = rgb(red = 255, green = 198, blue = 0, maxColorValue = 255), #light_yellow 25% darker
                   teal_dark_25 = rgb(red = 65, green = 112, blue = 114, maxColorValue = 255), #teal 25% darker
                   gray_dark_25 = rgb(red = 96, green = 96, blue = 96, maxColorValue = 255)) #gray 25% darker


vivid_dark_50 <- c(turquoise_dark50 = rgb(red = 6, green = 103, blue = 132, maxColorValue = 255), #turquoise 50% darker
                   orange_dark50 = rgb(red = 160, green = 65, blue = 7, maxColorValue = 255), #orange 50% darker
                   lavender_dark50 = rgb(red = 79, green = 70, blue = 98, maxColorValue = 255), #lavender 50% darker
                   light_green_dark50 = rgb(red = 70, green = 125, blue = 49, maxColorValue = 255), #light green 50% darker
                   light_yellow_dark50 = rgb(red = 170, green = 132, blue = 0, maxColorValue = 255), #light yellow 50% darker
                   teal_dark50 = rgb(red = 44, green = 74, blue = 75, maxColorValue = 255), #teal 50% darker
                   gray_dark50 = rgb(red = 64, green = 64, blue = 64, maxColorValue = 255)) #gray 50% darker


# Define colour palette cycle order for bar charts (# = 21) - this is ugly but appears to work
vivid_house_colours <- c(vivid_dark_50, vivid_dark_25, vivid_base, vivid_light_40, vivid_light_60) #combine all colours into a long list
vivid_house_colours2 <- c(vivid_house_colours[seq(1, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(2, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(3, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(4, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(5, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(6, length(vivid_house_colours), by = 7)],
                          vivid_house_colours[seq(7, length(vivid_house_colours), by = 7)]) #reorder colours in base palette colour order
