NAME	=	funEvalExpr

all:
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

clean:
	rm $(NAME)

.PHONY:	all
