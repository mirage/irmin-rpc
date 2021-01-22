@0x96075cba27939f6f;

using Hash = Data;
using Contents = Data;
using Endpoint = Data;
using Key = Text;

# Irmin.Info.t
struct Info {
  author   @0  :Text;
  message  @1  :Text;
  date     @2  :Int64;
}


# Store.Tree
interface Tree {
  struct Node {
    step  @0  :Text;
    tree  @1  :Concrete;
  }

  struct Concrete {
    key @0 :Key;
    union {
      contents @1 :Hash;
      node @2 :List(Node);
    }
  }

  find @0 (key :Key) -> (contents :Contents);
  getTree @1 (key :Key) -> (tree :Tree);
  add @2 (key :Key, contents :Contents) -> (tree :Tree);
  addTree @3 (key :Key, tree :Tree) -> (tree :Tree);
  mem @4 (key :Key) -> (exists :Bool);
  memTree @5 (key :Key) -> (exists :Bool);
  getConcrete @6 () -> (concrete :Concrete);
  hash @7 () -> (hash :Hash);
  findHash @8 (key :Key) -> (hash :Hash);
  remove   @9  (key :Key) -> (tree :Tree);
}

interface Commit {
  struct Value {
    hash     @0  :Hash;
    info     @1  :Info;
    parents  @2  :List(Hash);
    tree     @3  :Tree.Concrete;
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

interface Pack {
  struct IntegrityCheckResult {
    union {
      noError        @0 :Void;
      fixed          @1 :Int64;
      cannotFix      @2 :Text;
      corrupted      @3 :Int64;
    }
  }

  integrityCheck @0 (pack :Pack, autoRepair :Bool) -> (result :IntegrityCheckResult);
}

interface Store {
  find      @0  (key :Key) -> (contents :Contents);
  getTree  @1  (key :Key) -> (tree :Tree);
  set       @2  (key :Key, info :Info, contents :Contents) -> ();
  setTree   @3  (key :Key, info :Info, tree :Tree) -> ();
  remove    @4  (key :Key, info :Info) -> ();
  mem       @5  (key :Key) -> (exists :Bool);
  memTree   @6  (key :Key) -> (exists :Bool);

  # Merge API on stores
  struct MergeResult {
    union {
      ok        @0  :Void;
      errorMsg  @1  :Text;
    }
  }

  mergeWithBranch  @7  (branch :Text, info :Info) -> (result :MergeResult);

  sync  @8  () -> (sync :Sync);
  pack @9 () -> (pack :Pack);
  lastModified  @10 (key :Key) -> (commit :Commit);
  findHash @11 (key :Key) -> (hash :Hash);
  contentsOfHash @12  (hash :Hash) -> (contents :Contents);
}

interface Repo {
  master    @0  () -> (store :Store);
  ofBranch  @1  (branch :Text) -> (store :Store);

  branchList    @2  () -> (branches :List(Text));
  branchRemove  @3  (branch :Text) -> ();
  branchSet     @4  (branch :Text, commit :Commit) -> ();

  commitOfHash   @5  (hash :Hash) -> (commit :Commit);
  contentsOfHash @6  (hash :Hash) -> (contents :Contents);
  emptyTree @7 () -> (tree :Tree);
}

# The top-level interface of an RPC server
interface Irmin {

  # Each RPC server monitors exactly one repository
  repo  @0  () -> (repo :Repo);

  # Check availabilty of server
  ping @1 () -> ();
}
