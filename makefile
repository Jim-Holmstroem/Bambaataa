bambaataa:
	stack build && stack exec Bambaataa

redis:
	docker run --net=host -d redis
redis-cli:
	docker run -it --net=host redis redis-cli
