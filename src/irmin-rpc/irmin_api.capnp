@0x96075cba27939f6f;

using Hash = Data;
using Contents = Data;
using Endpoint = Data;
using Key = Text;

struct Node {
  step  @0  :Text;
  tree  @1  :Tree;
}

struct Tree {
  key @0 :Key;
  union {
    contents @1 :Contents;
    node @2 :List(Node);
  }
}

struct Info {
  author   @0  :Text;
  message  @1  :Text;
  date     @2  :Int64;
}

interface Commit {
  struct Value {
    hash     @0  :Hash;
    info     @1  :Info;
    parents  @2  :List(Hash);
    tree     @3  :Tree;
  }

  read     @0  () -> (value :Value);

  tree     @1  () -> (tree :Tree);
  parents  @2  () -> (hashes :List(Hash));
  info     @3  () -> (info :Info);
  hash     @4  () -> (hash :Hash);
}

interface Sync {
  struct PushResult {
    union {
      okEmpty            @0  :Void;
      okHead             @1  :Hash;
      errorDetachedHead  @2  :Void;
      errorMsg           @3  :Text;
    }
  }

  push   @0  (endpoint :Endpoint) -> (result :PushResult);
  pull   @1  (endpoint :Endpoint, info :Info) -> (result :Commit);
  clone  @2  (endpoint :Endpoint) -> (result :Commit);
}

interface Store {
  find      @0  (key :Text) -> (contents :Contents);
  findTree  @1  (key :Text) -> (tree :Tree);
  set       @2  (key :Text, info :Info, contents :Contents) -> ();
  setTree   @3  (key :Text, info :Info, tree :Tree) -> ();
  remove    @4  (key :Text, info :Info) -> ();

  # Merge API on stores
  struct MergeResult {
    union {
      ok        @0  :Void;
      errorMsg  @1  :Text;
    }
  }

  mergeWithBranch  @5  (branch :Text, info :Info) -> (result :MergeResult);

  sync  @6  () -> (sync :Sync);
}

interface Repo {
  master    @0  () -> (store :Store);
  ofBranch  @1  (branch :Text) -> (store :Store);

  branchList    @2  () -> (branches :List(Text));
  branchRemove  @3  (branch :Text) -> ();
  branchSet     @4  (branch :Text, commit :Commit) -> ();

  commitOfHash  @5  (hash :Hash) -> (commit :Commit);
}

# The top-level interface of an RPC server
interface Irmin {

  # Each RPC server monitors exactly one repository
  repo  @0  () -> (repo :Repo);

  # Check availabilty of server
  ping @1 () -> ();
}
