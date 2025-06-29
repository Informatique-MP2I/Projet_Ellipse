# Makefile - Ellipse
SRC := Grimaud_Ellipse
EXE := ellipse

.PHONY: clean

$(EXE): $(SRC).ml
	ocamlopt -o $@ $<

clean:
	rm *.cmi *.cmo *.cmx *.o $(EXE)
