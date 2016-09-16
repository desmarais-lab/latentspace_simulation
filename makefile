TEXCMD := pdflatex -interaction=batchmode

all: graphs manuscript.pdf

upload:
	rsync -azv --delete --files-from=upload.txt $(PWD) zmj102@aci-b.aci.ics.psu.edu:work/lsm_applications

download:
	rm -rf reg
g	rsync -azv --files-from=download.txt zmj102@aci-b.aci.ics.psu.edu:work/lsm_bruce $(PWD)

graphs:
	rscript src/analysis.R

manuscript.pdf: manuscript.tex
	$(TEXCMD) $<
	bibtex manuscript.aux
	$(TEXCMD) $<
	$(TEXCMD) $<
	find . | egrep ".*((\.(aux|log|blg|bbl|out|DS_Store)))$$" | xargs rm
	rm -rf auto

diff.pdf: manuscript.tex
	latexdiff manuscript.tex versions/$(ls versions -Art | tail -n 1) > diff.tex
	$(TEXCMD) diff.tex
	bibtex diff.aux
	$(TEXCMD) diff.tex
	cp manuscript.tex "manuscript" $("manuscript" date +"%Y%m%d_%H%M%S")
	find . | egrep ".*((\.(aux|log|blg|bbl|out|DS_Store)))$$" | xargs rm
	rm -rf auto


