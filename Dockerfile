# base
FROM ocaml/opam2:debian-stable as base
RUN sudo apt-get update
RUN sudo apt-get install -y m4 libgmp-dev perl libev-dev capnproto libcapnp-dev pkg-config
RUN git -C /home/opam/opam-repository pull
RUN opam update

RUN opam install conf-libev irmin-unix

COPY . .
RUN opam config exec -- opam pin add irmin-rpc . --locked
RUN opam config exec -- opam pin add irmin-rpc-unix . --locked

# irmin-rpc
FROM debian
EXPOSE 9090
COPY --from=base /home/opam/.opam/4.10/bin/irmin-rpc .
COPY --from=base /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/
COPY --from=base /usr/lib/x86_64-linux-gnu/libev* /usr/lib/
VOLUME /data
ENTRYPOINT [ "/irmin-rpc" ]
CMD [ "-a", "0.0.0.0", \
      "-p", "9090", \
      "-s", "git", \
      "-c", "string", \
      "--root", "/data", \
      "-f", "address.txt", \
      "-k", "key.pem" ]

