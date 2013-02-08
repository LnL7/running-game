.PHONY: run test tags clean

run:
	racket test/game-start.rkt

test:
	find . -name "*-test.rkt" -exec racket {} \;

tags:
	ctags --langmap=scheme:.rkt -R .

clean:
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
