FROM alpine

EXPOSE 9090
ENV OPAMYES 1
RUN set -ex
RUN apk add --no-cache --purge -U \
       opam sudo make bash gcc musl-dev \
       gmp gmp-dev libressl-dev linux-headers m4 perl zlib-dev git
RUN adduser -D irmin
RUN mkdir /data
RUN chown irmin /data
RUN opam init
RUN opam switch 4.06.1
RUN eval $(opam config env) ; \
    opam update ; \
    opam install dune digestif ; \
    opam pin add checkseum.dev https://github.com/dinosaure/checkseum.git ; \
    opam pin add git.dev https://github.com/mirage/ocaml-git.git          ; \
    opam pin add git-http.dev https://github.com/mirage/ocaml-git.git     ; \
    opam pin add git-unix.dev https://github.com/mirage/ocaml-git.git     ; \
    opam pin add irmin.dev https://github.com/mirage/irmin.git            ; \
    opam pin add irmin-git.dev https://github.com/mirage/irmin.git        ; \
    opam pin add irmin-unix.dev https://github.com/mirage/irmin.git       ; \
    opam pin add irmin-rpc https://github.com/zshipko/irmin-rpc.git       ; \
    mv ~/.opam/4.06.1/bin/irmin /                                         ; \
    mv ~/.opam/4.06.1/bin/irmin-rpc/                                      ; \
    apk del opam make bash gcc musl-dev gmp-dev libressl-dev \
       linux-headers m4 perl zlib-dev                                     ; \
    rm -rf /var/cache/apk/*                                               ; \
    rm -rf ~/.opam                                                        ;
USER irmin
WORKDIR /home/irmin
ENTRYPOINT [ "/irmin-rpc" ]
VOLUME /data
CMD [ "-a", "0.0.0.0", \
      "-p", "9998", \
      "-s", "git", \
      "-c", "string", \
      "--root", "/data", \
      "-f", "address.txt", \
      "-k", "key.pem" ]

