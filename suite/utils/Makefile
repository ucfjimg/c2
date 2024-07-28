#
# Build utility library for test checks which require
# assembly.
# 
ASM_SRCS=rsp.s
O=$(ASM_SRCS:.s=.o)
LIB=libutil.a

$(LIB): $(O)
	ar r $(LIB) $?
	ranlib $(LIB)

clean:
	-rm $(LIB) $(O)

