NAME	=	funEvalExpr

all:
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

run-tests:
	stack test

clean:
	rm $(NAME)

.PHONY:	all
