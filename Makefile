.PHONY: test tags clean

test:
	find . -name "*-test.rkt" -exec racket {} \;

tags:
	ctags --langmap=scheme:.rkt -R .

clean:
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
