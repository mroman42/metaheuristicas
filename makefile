all: executables Onenn_sols Onenn_reports

# COMPILACIÓN
# Usará la herramienta stack para compilar con Haskell.
ST=stack exec\
--package split \
--package options \
--package random \
--package accelerate-llvm-ptx \
--package normaldistribution

# Stack proporciona un compilador y un intérprete con las librerías
# solicitadas. El intérprete carga además todo el código fuente.
STK=$(ST) -- ghc -isrc -odir obj -hidir obj
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



# One-nn es el algoritmo trivial. Para un algoritmo se crean:
#  - soluciones
#  - reports
define VALIDATION =
bin/Onenn: src/Onenn.hs src/Base.hs src/Input.hs
	$(STK) $^ -o $@ -main-is Onenn

%.arff.Onenn.sol1: bin/Onenn %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part5
	cat $(filter-out $<,$^) | bin/Onenn > $@
%.arff.Onenn.sol2: bin/Onenn %.arff.part1 %.arff.part3 %.arff.part4 %.arff.part5
	cat $(filter-out $<,$^) | bin/Onenn > $@
%.arff.Onenn.sol3: bin/Onenn %.arff.part2 %.arff.part1 %.arff.part4 %.arff.part5
	cat $(filter-out $<,$^) | bin/Onenn > $@
%.arff.Onenn.sol4: bin/Onenn %.arff.part2 %.arff.part3 %.arff.part1 %.arff.part5
	cat $(filter-out $<,$^) | bin/Onenn > $@
%.arff.Onenn.sol5: bin/Onenn %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part1
	cat $(filter-out $<,$^) | bin/Onenn > $@

Onenn_sols1: $(addsuffix .arff.Onenn.sol1, $(basename $(wildcard data/*.arff)))
Onenn_sols2: $(addsuffix .arff.Onenn.sol2, $(basename $(wildcard data/*.arff)))
Onenn_sols3: $(addsuffix .arff.Onenn.sol3, $(basename $(wildcard data/*.arff)))
Onenn_sols4: $(addsuffix .arff.Onenn.sol4, $(basename $(wildcard data/*.arff)))
Onenn_sols5: $(addsuffix .arff.Onenn.sol5, $(basename $(wildcard data/*.arff)))
Onenn_sols: Onenn_sols1 Onenn_sols2 Onenn_sols3 Onenn_sols4 Onenn_sols5

%.arff.Onenn.report1: bin/scorer %.arff.Onenn.sol1 %.arff.part1 %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part5
	cat $(filter-out $(wordlist 1, 3,$^),$^) | $< $(word 2,$^) $(word 3,$^) > $@
%.arff.Onenn.report2: bin/scorer %.arff.Onenn.sol2 %.arff.part2 %.arff.part1 %.arff.part3 %.arff.part4 %.arff.part5
	cat $(filter-out $(wordlist 1, 3,$^),$^) | $< $(word 2,$^) $(word 3,$^) > $@
%.arff.Onenn.report3: bin/scorer %.arff.Onenn.sol3 %.arff.part3 %.arff.part2 %.arff.part1 %.arff.part4 %.arff.part5
	cat $(filter-out $(wordlist 1, 3,$^),$^) | $< $(word 2,$^) $(word 3,$^) > $@
%.arff.Onenn.report4: bin/scorer %.arff.Onenn.sol4 %.arff.part4 %.arff.part2 %.arff.part3 %.arff.part1 %.arff.part5
	cat $(filter-out $(wordlist 1, 3,$^),$^) | $< $(word 2,$^) $(word 3,$^) > $@
%.arff.Onenn.report5: bin/scorer %.arff.Onenn.sol5 %.arff.part5 %.arff.part2 %.arff.part3 %.arff.part4 %.arff.part1
	cat $(filter-out $(wordlist 1, 3,$^),$^) | $< $(word 2,$^) $(word 3,$^) > $@

%.arff.Onenn.report: %.arff.Onenn.report1 %.arff.Onenn.report2 %.arff.Onenn.report3 %.arff.Onenn.report4 %.arff.Onenn.report5
	cat $^ > $@

Onenn_reports: $(addsuffix .arff.Onenn.report, $(basename $(wildcard data/*.arff)))
endef

$(foreach i,1,$(eval $(call VALIDATION,$(i))))




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
clean: clean-parts clean-sols clean-executables

executables: bin/fivefold bin/scorer bin/Onenn


.PHONY:\
clean-parts clean-sols\
ghci parts1 parts2 parts3 parts4 parts5 executables\
Onenn_sols1 Onenn_sols2 Onenn_sols3 Onenn_sols4 Onenn_sols5 Onenn_sols
