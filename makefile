all: executables Onenn_sols Onenn_reports

# SEMILLA ALEATORIA
SEED=42

# COMPILACIÓN
# Usará la herramienta stack para compilar con Haskell.
ST=stack exec\
--package split \
--package options \
--package random \
--package random-shuffle \
--package accelerate-llvm-ptx \
--package normaldistribution

# Stack proporciona un compilador y un intérprete con las librerías
# solicitadas. El intérprete carga además todo el código fuente.
STK=$(ST) -- ghc -O2 -fllvm -isrc -odir obj -hidir obj
STKGHCI=$(ST) -- ghci -isrc -odir obj -hidir obj src/*
# Proporciona un intérprete con el código cargado al ejecutar 'make ghci'
ghci:
	$(STKGHCI)




# 5-fold. Crea el programa que parte un conjunto de datos en cinco
# conjuntos iguales.  Cada vez que varíe el programa o el conjunto de
# datos se deberán volver a crear.
bin/fivefold: src/Fivefold.hs src/Base.hs src/Input.hs
	$(STK) $< -o $@ -main-is Fivefold

data/%.arff.part1: data/%.arff bin/fivefold
	bin/fivefold $<
data/%.arff.part2: data/%.arff bin/fivefold
	bin/fivefold $<
data/%.arff.part3: data/%.arff bin/fivefold
	bin/fivefold $<
data/%.arff.part4: data/%.arff bin/fivefold
	bin/fivefold $<
data/%.arff.part5: data/%.arff bin/fivefold
	bin/fivefold $<

parts1: $(addsuffix .arff.part1, $(basename $(wildcard data/*.arff)))
parts2: $(addsuffix .arff.part2, $(basename $(wildcard data/*.arff)))
parts3: $(addsuffix .arff.part3, $(basename $(wildcard data/*.arff)))
parts4: $(addsuffix .arff.part4, $(basename $(wildcard data/*.arff)))
parts5: $(addsuffix .arff.part5, $(basename $(wildcard data/*.arff)))


# Scorer. Dado un conjunto de test junto con el training y solución
# que conforman el clasificador que ha encontrado un algoritmo,
# muestra por pantalla los resultados del algoritmo.
bin/scorer: src/Scorer.hs src/Base.hs src/Input.hs
	$(STK) $< -o $@ -main-is Scorer


# Construcción de ejecutables, lo separamos del resto porque cada uno
# tiene unas dependencias distintas.
bin/Onenn: src/Onenn.hs src/Base.hs src/Input.hs src/TemplateMain.hs
	$(STK) $^ -o $@ -main-is Onenn
bin/Relief: src/Relief.hs src/Base.hs src/Input.hs src/TemplateMain.hs
	$(STK) $^ -o $@ -main-is Relief
bin/LocalSearch: src/LocalSearch.hs src/Base.hs src/Input.hs src/TemplateMain.hs src/LeaveOneOut.hs
	$(STK) $^ -o $@ -main-is LocalSearch


# Describimos todo lo que queremos hacer para cada uno de los algoritmos
# que evaluamos de forma paramétrica en el algoritmo. Toda la siguiente
# sección es paramétrica y se repetirá para cada uno de los algoritmos.
# Se define
#  - Cálculo de las soluciones.
#  - Cálculo de la bondad de las soluciones.
#  - Presentación de un report final.
define VALIDATION =
%.arff.$(1).sol1: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED) > $$@
%.arff.$(1).sol2: bin/$(1) %.arff.part1 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED) > $$@
%.arff.$(1).sol3: bin/$(1) %.arff.part2 %.arff.part1 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED) > $$@
%.arff.$(1).sol4: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part1 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED) > $$@
%.arff.$(1).sol5: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part1
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED) > $$@

$(1)_sols1: $$(addsuffix .arff.$(1).sol1, $$(basename $$(wildcard data/*.arff)))
$(1)_sols2: $$(addsuffix .arff.$(1).sol2, $$(basename $$(wildcard data/*.arff)))
$(1)_sols3: $$(addsuffix .arff.$(1).sol3, $$(basename $$(wildcard data/*.arff)))
$(1)_sols4: $$(addsuffix .arff.$(1).sol4, $$(basename $$(wildcard data/*.arff)))
$(1)_sols5: $$(addsuffix .arff.$(1).sol5, $$(basename $$(wildcard data/*.arff)))
$(1)_sols: $(1)_sols1 $(1)_sols2 $(1)_sols3 $(1)_sols4 $(1)_sols5

%.arff.$(1).report1: bin/scorer %.arff.$(1).sol1 %.arff.part1 %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$(wordlist 1, 3,$$^),$$^) | $$< $$(word 2,$$^) $$(word 3,$$^) > $$@
%.arff.$(1).report2: bin/scorer %.arff.$(1).sol2 %.arff.part2 %.arff.part1 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$(wordlist 1, 3,$$^),$$^) | $$< $$(word 2,$$^) $$(word 3,$$^) > $$@
%.arff.$(1).report3: bin/scorer %.arff.$(1).sol3 %.arff.part3 %.arff.part2 %.arff.part1 %.arff.part4 %.arff.part5
	cat $$(filter-out $$(wordlist 1, 3,$$^),$$^) | $$< $$(word 2,$$^) $$(word 3,$$^) > $$@
%.arff.$(1).report4: bin/scorer %.arff.$(1).sol4 %.arff.part4 %.arff.part2 %.arff.part3 %.arff.part1 %.arff.part5
	cat $$(filter-out $$(wordlist 1, 3,$$^),$$^) | $$< $$(word 2,$$^) $$(word 3,$$^) > $$@
%.arff.$(1).report5: bin/scorer %.arff.$(1).sol5 %.arff.part5 %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part1
	cat $$(filter-out $$(wordlist 1, 3,$$^),$$^) | $$< $$(word 2,$$^) $$(word 3,$$^) > $$@

%.arff.$(1).report: %.arff.$(1).report1 %.arff.$(1).report2 %.arff.$(1).report3 %.arff.$(1).report4 %.arff.$(1).report5
	cat $$^ > $$@

$(1)_reports: $$(addsuffix .arff.$(1).report, $$(basename $$(wildcard data/*.arff)))
endef

$(foreach i,Onenn Relief LocalSearch,$(eval $(call VALIDATION,$(i))))




# Limpieza
clean-parts:
	@echo "Borrando particiones de los datasets..."
	rm -rf data/*.part?
clean-sols:
	@echo "Borrando soluciones calculadas..."
	rm -rf data/*.sol?
clean-executables:
	@echo "Borrando ejecutables..."
	rm -rf bin/*
clean-all: clean-parts clean-sols clean-executables

executables: bin/fivefold bin/scorer bin/Onenn


.PHONY:\
clean-parts clean-sols\
ghci parts1 parts2 parts3 parts4 parts5 executables\
Onenn_sols1 Onenn_sols2 Onenn_sols3 Onenn_sols4 Onenn_sols5 Onenn_sols
