CC=gcc
CFLAGS=-Wall -std=c99 -I/Library/Frameworks/R.framework/Versions/3.6/Resources/include
LDFLAGS=
all:
	$(CC) $(CFLAGS) $(LDFLAGS) -c bslearn.c
	$(CC) $(CFLAGS) $(LDFLAGS) -dynamiclib bslearn.o -o bslearn.dylib 

clean:
	rm -f *.o *~
