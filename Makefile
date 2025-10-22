# List all generated figures
FIGURES = figures/combined_plot.png \
          figures/spectral_plot.png \
          figures/3d_shell_plot.html figures/3d_shell_plot_files

REPORT = clustering.html

# Default target: generate figures and render report
all: $(REPORT)

# making fig directory
figures/:
	mkdir -p figures

# Generate 2D plots
figures/combined_plot.png: clustering.R figures/
	Rscript clustering.R

figures/spectral_plot.png: clustering.R figures/
	Rscript clustering.R

# Generate interactive 3D plot
figures/3d_shell_plot.html figures/3d_shell_plot_files: clustering.R figures/
	Rscript clustering.R

# Render RMarkdown report (depends on all figures)
$(REPORT): clustering.Rmd $(FIGURES)
	Rscript -e "rmarkdown::render('clustering.Rmd', output_file = 'clustering.html', output_format = 'html_document')"

# Clean generated files
clean:
	rm -f clustering.html clustering.pdf
	rm -f figures/*.png
	rm -rf figures/3d_shell_plot_files
	rm -f figures/3d_shell_plot.html
	rm -rf figures

# Declare phony targets so Make always runs them
.PHONY: all clean

