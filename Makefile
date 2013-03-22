.PHONY: run test tags clean

build:
	raco make run.rkt
	raco exe -o bin/run run.rkt

run:
	racket test/game-start.rkt

test:
	find . -name "*-test.rkt" -exec racket {} \;

tags:
	ctags --langmap=scheme:.rkt -R .

clean:
	rm -f resources/*.txt
	git checkout HEAD -- resources/level-*
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
