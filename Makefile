NAME	=	funEvalExpr

all:
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

run-tests:
	stack --jobs=4 test

clean:
	rm $(NAME)

.PHONY:	all
