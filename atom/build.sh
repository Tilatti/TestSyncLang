GHC=ghc
CC=gcc

CSRC_PREFIX=test

# Clean the previous compilation

rm *.hi *.exe
rm ${CSRC_PREFIX}.c ${CSRC_PREFIX}.h

# Compile the Atom program

${GHC} --make main.hs opts.hs StringEmbed.hs \
	HWComponents/Common HWComponents/Bus.hs HWComponents/Basic.hs HWComponents/Latch.hs

if [ $? -ne 0 ]; then
	exit 1
fi

# Atom program generate the C source code 

./main --generate ${CSRC_PREFIX}

if [ $? -ne 0 ]; then
	exit 1
fi

# Compile the C source code

${CC} ${CSRC_PREFIX}.c -o ./simu.exe

if [ $? -ne 0 ]; then
	exit 1
fi

./simu.exe
