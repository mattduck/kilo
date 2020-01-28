.PHONY: valgrind format

kilo: *.c
	$(CC) kilo.c point.c history.c -o kilo -Wall -Wextra -pedantic -std=c99

valgrind: kilo
	valgrind --log-file=valgrind.log --leak-check=full --show-leak-kinds=all --track-origins=yes ./kilo kilo-org.c

format:
	clang-format -i *.c *.h
