smu_blue <- '#354c97'
smu_red <- '#cc0000'

princeton_orange <- rgb(231, 117, 1, maxColorValue=255)
smu_teal <- rgb(89, 195, 195, maxColorValue = 255)


plot_colors <- list(P=smu_red,
                    Q=smu_blue,
                    err_A=smu_teal,
                    err_B=princeton_orange
                    )


get_PQ_mix <- colorRamp(c(plot_colors$P,plot_colors$Q)) ## a function on (0,1) for amount of red

smu_purple <-  rgb(get_PQ_mix(.5), max=255)



gray <- c('#f8f9fa', '#e9ecef', '#dee2e6', '#ced4da', '#adb5bd',
          '#6c757d', '#495057', '#343a40', '#212529')
