ALL: boot
	./boot ml.ml

boot: boot.c
	$(CC) -Wall -Werror -O2 -g -oboot boot.c
