irmin-rpc — CapNProto RPC server for Irmin
-------------------------------------------------------------------------------
%%VERSION%%

irmin-rpc is TODO

irmin-rpc is distributed under the ISC license.

Homepage: https://github.com/zshipko/irmin-rpc

## Installation

irmin-rpc can be installed with `opam`:

    opam install irmin-rpc

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
irmin-rpc`.

[doc]: https://zshipko.github.io/irmin-rpc/doc

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    dune runtest
