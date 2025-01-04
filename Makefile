# Répertoires
BIN_DIR = bin
SRC_DIR = src
OBJ_DIR = obj

# Nom de l'exécutable final
EXEC = $(BIN_DIR)/tp_parser

# Fichiers sources
LEXER = $(SRC_DIR)/lexer.mll
PARSER = $(SRC_DIR)/parser.mly
SOURCES = $(SRC_DIR)/ast.ml $(SRC_DIR)/main.ml

# Fichiers générés par ocamllex et ocamlyacc
PARSER_ML = $(OBJ_DIR)/parser.ml
PARSER_MLI = $(OBJ_DIR)/parser.mli
LEXER_ML = $(OBJ_DIR)/lexer.ml

# Fichiers objets
OBJS = $(OBJ_DIR)/ast.cmo $(OBJ_DIR)/parser.cmo $(OBJ_DIR)/lexer.cmo $(OBJ_DIR)/main.cmo

# Commandes OCaml
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

# Options de compilation
OCAMLC_FLAGS = -I $(OBJ_DIR)

# Règle par défaut
all: $(EXEC)

# Création du dossier obj si nécessaire
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

# Générer l'exécutable
$(EXEC): $(OBJS)
	mkdir -p $(BIN_DIR)
	$(OCAMLC) $(OCAMLC_FLAGS) -o $(EXEC) $(OBJS)

# Générer les fichiers à partir de parser.mly
$(PARSER_ML) $(PARSER_MLI): $(PARSER) | $(OBJ_DIR)
	$(OCAMLYACC) -b $(OBJ_DIR)/parser $(PARSER)

# Générer les fichiers à partir de lexer.mll
$(LEXER_ML): $(LEXER) | $(OBJ_DIR)
	$(OCAMLLEX) -o $(LEXER_ML) $(LEXER)

# Compiler l'interface parser.mli
$(OBJ_DIR)/parser.cmi: $(PARSER_MLI) | $(OBJ_DIR)
	$(OCAMLC) -I $(OBJ_DIR) -c -o $(OBJ_DIR)/parser.cmi $(PARSER_MLI)

# Compiler les fichiers objets
$(OBJ_DIR)/%.cmo: $(SRC_DIR)/%.ml | $(OBJ_DIR)
	$(OCAMLC) $(OCAMLC_FLAGS) -c -o $@ $<

# Règle spécifique pour parser.cmo
$(OBJ_DIR)/parser.cmo: $(PARSER_ML) $(OBJ_DIR)/parser.cmi
	$(OCAMLC) $(OCAMLC_FLAGS) -c -o $@ $(PARSER_ML)

# Règle spécifique pour lexer.cmo
$(OBJ_DIR)/lexer.cmo: $(LEXER_ML)
	$(OCAMLC) $(OCAMLC_FLAGS) -c -o $@ $(LEXER_ML)

# Nettoyage des fichiers générés
clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR) $(SRC_DIR)/*.conflicts */parser.ml */parser.mli */lexer.ml