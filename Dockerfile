# capnp
FROM debian as capnp

RUN mkdir -p /src
RUN apt-get update && apt-get install -y autoconf automake libtool git g++ make
RUN cd /src && git clone https://github.com/capnproto/capnproto.git
WORKDIR /src/capnproto/c++
RUN ./setup-autotools.sh
RUN autoreconf -i
RUN ./configure
RUN make -j6 check
RUN make install
RUN which capnp

# base
FROM ocaml/opam2:debian as base
RUN sudo apt-get update
RUN sudo apt-get install -y m4 libgmp-dev perl libev-dev libcapnproto-dev capnproto
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
RUN opam config exec -- opam pin add irmin.dev --dev
RUN opam config exec -- opam pin add irmin-graphql.dev https://github.com/mirage/irmin.git
RUN opam config exec -- opam pin add irmin-http.dev --dev
RUN opam config exec -- opam pin add irmin-mem.dev --dev
RUN opam config exec -- opam pin add irmin-fs.dev --dev
RUN opam config exec -- opam pin add irmin-git.dev --dev
RUN opam config exec -- opam pin add irmin-unix.dev --dev
COPY . .
RUN opam config exec -- opam pin add irmin-rpc . --locked
RUN opam config exec -- opam pin add irmin-rpc-unix . --locked

# irmin-rpc
FROM debian
EXPOSE 9090
COPY --from=base /home/opam/.opam/4.07/bin/irmin-rpc .
COPY --from=base /usr/lib/libgmp* /usr/lib/
VOLUME /data
ENTRYPOINT [ "/irmin-rpc" ]
CMD [ "-a", "0.0.0.0", \
      "-p", "9090", \
      "-s", "git", \
      "-c", "string", \
      "--root", "/data", \
      "-f", "address.txt", \
      "-k", "key.pem" ]

