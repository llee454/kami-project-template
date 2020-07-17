#!/usr/bin/env bash
#
# Builds the Ice Storm tool chain test module.
# coq_makefile -Q ../Kami/ Kami -Q ../coq-record-update/src/ RecordUpdate -Q . IceStomTest IceStormTest.v > Makefile
#
# Note: you may have to manually remove the CustomExtract import from CustomExtract.hs

make && \
cd ../Kami && ./fixHaskell.sh ../BuildSim && cd ../BuildSim && \
cp CustomExtract.bak CustomExtract.hs && \
ghc -O2 --make -i../Kami HaskellSim.hs -o HaskellSim && \
echo "done"
