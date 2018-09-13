# capnp
FROM alpine as capnp

RUN mkdir -p /src
RUN apk update && apk add autoconf automake libtool linux-headers git g++ make
RUN cd /src && git clone https://github.com/capnproto/capnproto.git
WORKDIR /src/capnproto/c++
RUN ./setup-autotools.sh
RUN autoreconf -i
RUN ./configure
RUN make -j6 check
RUN make install
RUN which capnp

# base
FROM ocaml/opam2:alpine as base
RUN sudo apk add --update m4 gmp gmp-dev perl
RUN git -C /home/opam/opam-repository pull
RUN opam update

WORKDIR /src
COPY --from=capnp /usr/local/bin/capnp /usr/local/bin/capnp
COPY --from=capnp /usr/local/bin/capnpc /usr/local/bin/capnpc
COPY --from=capnp /usr/local/lib/libcapnpc-0.8-dev.so /usr/local/lib/libcapnpc-0.8-dev.so
COPY --from=capnp /usr/local/lib/libcapnp-json-0.8-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/lib/libcapnp-0.8-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/lib/libkj-0.8-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/include/capnp /usr/local/include/capnp
COPY irmin-rpc.opam irmin-rpc-unix.opam ./
RUN opam config exec -- opam pin add digestif.dev --dev
RUN opam config exec -- opam pin add checkseum.dev --dev
RUN opam config exec -- opam pin add git.dev --dev
RUN opam config exec -- opam pin add git-http.dev --dev
RUN opam config exec -- opam pin add git-unix.dev --dev
RUN opam config exec -- opam pin add irmin.dev --dev
RUN opam config exec -- opam pin add irmin-http.dev --dev
RUN opam config exec -- opam pin add irmin-git.dev --dev
RUN opam config exec -- opam pin add irmin-unix.dev --dev
COPY . .
RUN opam config exec -- opam pin add irmin-rpc . --locked
RUN opam config exec -- opam pin add irmin-rpc-unix . --locked

# irmin-rpc
FROM alpine
EXPOSE 9090
COPY --from=base /home/opam/.opam/4.07/bin/irmin-rpc .
COPY --from=base /usr/lib/* /usr/lib/
ENTRYPOINT [ "/irmin-rpc" ]
VOLUME /data
CMD [ "-a", "0.0.0.0", \
      "-p", "9090", \
      "-s", "git", \
      "-c", "string", \
      "--root", "/data", \
      "-f", "address.txt", \
      "-k", "key.pem" ]

