.PHONY: test clean

test:
	racket test/*_test.rkt

clean:
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
