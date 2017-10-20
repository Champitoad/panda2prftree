NAME := panda2prftree
#NAME := form
TARGET := native
#TARGET := inferred.mli
EXE := $(NAME).$(TARGET)

FLAGS := -use-menhir -use-ocamlfind -quiet -pkg xml-light,batteries
CFLAGS := ''
LFLAGS := ''

all: $(EXE)

%.$(TARGET): %.ml
	@ocamlbuild $(FLAGS) -cflags $(CFLAGS) -lflags $(LFLAGS) $@

run: $(EXE)
	@./$< $(MAKECMDGOALS)

clean:
	@ocamlbuild -clean
