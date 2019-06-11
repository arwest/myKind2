
OCB_FLAGS   = -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

.PHONY: all clean native

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

