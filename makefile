all: executables Onenn_sols Onenn_reports

# SEMILLAS ALEATORIAS
SEED1=0
SEED2=1
SEED3=2
SEED4=3
SEED5=4

# PROFILING. Activar
#   --profile, para stack
#   -prof, para GHC
#   -p, para el ejecutable

# COMPILACIÓN
# Usará la herramienta stack para compilar con Haskell.
ST=stack exec\
--resolver lts-11.2 \
--package vector-strategies \
--package split \
--package options \
--package random \
--package random-shuffle \
--package accelerate-llvm-ptx \
--package normaldistribution \
--package monad-loops \
--package multiset

# Stack proporciona un compilador y un intérprete con las librerías
# solicitadas. El intérprete carga además todo el código fuente.
STK=$(ST) -- ghc -O3 -Wall -fforce-recomp -fllvm -threaded -funfolding-use-threshold=16 -isrc -odir obj -hidir obj
STKGHCI=$(ST) -- ghci -isrc -odir obj -hidir obj src/*
# Proporciona un intérprete con el código cargado al ejecutar 'make ghci'
ghci:
	$(STKGHCI)

EXEFLG=+RTS -N4



# 5-fold. Crea el programa que parte un conjunto de datos en cinco
# conjuntos iguales.  Cada vez que varíe el programa o el conjunto de
# datos se deberán volver a crear.
bin/fivefold: src/Fivefold.hs src/Input.hs
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
SRC_GEN=src/Genetic.hs src/Individual.hs src/Base.hs src/Input.hs src/TemplateMain.hs src/LeaveOneOut.hs src/Population.hs

bin/Onenn: src/Onenn.hs src/Base.hs src/Input.hs src/TemplateMain.hs
	$(STK) $^ -o $@ -main-is Onenn
bin/Relief: src/Relief.hs src/Base.hs src/Input.hs src/TemplateMain.hs
	$(STK) $^ -o $@ -main-is Relief
bin/LocalSearch: src/LocalSearch.hs src/Base.hs src/Input.hs src/TemplateMain.hs src/LeaveOneOut.hs
	$(STK) $^ -o $@ -main-is LocalSearch
bin/LocalSearch2: src/LocalSearch2.hs src/Base.hs src/Input.hs src/TemplateMain.hs src/LeaveOneOut.hs
	$(STK) $^ -o $@ -main-is LocalSearch2
bin/Ageca: src/Ageca.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is Ageca
bin/Ageblx: src/Ageblx.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is Ageblx
bin/Aggca: src/Aggca.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is Aggca
bin/Aggblx: src/Aggblx.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is Aggblx
bin/AmAll: src/AmAll.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is AmAll
bin/AmBest: src/AmBest.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is AmBest
bin/AmProb: src/AmProb.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is AmProb
bin/AmNew: src/AmNew.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is AmNew
bin/SimulatedAnnealing: src/SimulatedAnnealing.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is SimulatedAnnealing
bin/ILS: src/ILS.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is ILS
bin/DERand: src/DERand.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is DERand
bin/DECurrent: src/DECurrent.hs $(SRC_GEN)
	$(STK) $^ -o $@ -main-is DECurrent


# Describimos todo lo que queremos hacer para cada uno de los algoritmos
# que evaluamos de forma paramétrica en el algoritmo. Toda la siguiente
# sección es paramétrica y se repetirá para cada uno de los algoritmos.
# Se define
#  - Cálculo de las soluciones.
#  - Cálculo de la bondad de las soluciones.
#  - Presentación de un report final.
define VALIDATION =
%.arff.$(1).sol1: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED1) $$(EXEFLG) > $$@
%.arff.$(1).sol2: bin/$(1) %.arff.part1 %.arff.part3 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED2) $$(EXEFLG) > $$@
%.arff.$(1).sol3: bin/$(1) %.arff.part2 %.arff.part1 %.arff.part4 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED3) $$(EXEFLG) > $$@
%.arff.$(1).sol4: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part1 %.arff.part5
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED4) $$(EXEFLG) > $$@
%.arff.$(1).sol5: bin/$(1) %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part1
	cat $$(filter-out $$<,$$^) | bin/$(1) $$(SEED5) $$(EXEFLG) > $$@

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

data/$(1).tex: data/ozone-320.arff.$(1).report data/parkinsons.arff.$(1).report data/spectf-heart.arff.$(1).report
	./toLatex.sh $(1)
endef

$(foreach i,Onenn Relief LocalSearch LocalSearch2 Ageca Ageblx Aggca Aggblx AmAll AmBest AmProb AmNew SimulatedAnnealing ILS DERand DECurrent,$(eval $(call VALIDATION,$(i))))




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

executables: bin/fivefold bin/scorer bin/Onenn bin/Relief bin/LocalSearch bin/LocalSearch2 bin/Ageca bin/Ageblx


.PHONY:\
clean-parts clean-sols\
ghci parts1 parts2 parts3 parts4 parts5 executables\
Onenn_sols1 Onenn_sols2 Onenn_sols3 Onenn_sols4 Onenn_sols5 Onenn_sols
