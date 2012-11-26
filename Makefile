.PHONY: test tags clean

test:
	racket test/*_test.rkt

tags:
	ctags --langmap=scheme:.rkt -R .

clean:
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
