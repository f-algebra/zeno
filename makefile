EXTS = -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -XViewPatterns -XOverloadedStrings -XTypeFamilies -XBangPatterns -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable -XDoRec
FLAGS = -package ghc -threaded -fwarn-missing-signatures -funbox-strict-fields -hidir obj -odir obj -isrc -itest $(EXTS)
POWER = -O2
NORMAL = -O
MAIN = src/Main.hs

.PHONY : all power ghci clean

all:
	ghc --make -o zeno $(NORMAL) $(FLAGS) $(MAIN)

power:
	ghc --make -o zeno $(POWER) $(FLAGS) $(MAIN)

ghci:
	ghci -fobject-code $(NORMAL) $(FLAGS) $(MAIN)

clean:
	rm -rf obj/

