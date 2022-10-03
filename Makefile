.PHONY: all
all:
	docker build src/docker --tag spotify_datasci
	docker container rm -f spotify_datasci
	docker run \
		--rm \
		-p 8787:8787 \
		--name spotify_datasci \
		--volume ${PWD}:/home/rstudio \
		--env-file .env \
		spotify_datasci