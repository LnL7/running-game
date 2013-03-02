.PHONY: run test tags clean

build:
	raco make test/game-start.rkt
	raco exe -o bin/start test/game-start.rkt

run:
	racket test/game-start.rkt

test:
	find . -name "*-test.rkt" -exec racket {} \;

tags:
	ctags --langmap=scheme:.rkt -R .

clean:
	rm -f resources/*.txt
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
