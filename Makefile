
default: addone.pdf hgen.pdf

%.svg: %.dot
	dot -Tsvg $< >$@

%.pdf: %.dot
	dot -Tpdf $< >$@

%.png: %.dot
	dot -Tpng $< >$@

hgen.dot: lambda.hs
	runhaskell lambda.hs > hgen.dot

# 1227x1448
