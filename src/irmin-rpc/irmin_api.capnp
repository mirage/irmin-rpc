@0x96075cba27939f6f;

# Store.hash
using Hash = Data;

# Store.contents
using Contents = Data;
using Endpoint = Data;
using Key = Data;
using Step = Data;

# Irmin.info
struct Info {
  author   @0  :Text;
  message  @1  :Text;
  date     @2  :Int64;
}


# Store.Tree
interface Tree {
  struct Node {
    step  @0  :Step;
    tree  @1  :Concrete;
  }

  # Store.Tree.Concrete
  struct Concrete {
    key @0 :Key;
    union {
      contents @1 :Hash;
      node @2 :List(Node);
    }
  }

  # Get the contents associated with `key`
  find @0 (key :Key) -> (contents :Contents);

  # Get the tree with a root at `key`
  findTree @1 (key :Key) -> (tree :Tree);

  # Check if there is a value associated with `key`
  mem @2 (key :Key) -> (exists :Bool);

  # Check if there is a tree associated with `key`
  memTree @3 (key :Key) -> (exists :Bool);

  # Get a concrete representation of an Irmin tree
  getConcrete @4 () -> (concrete :Concrete);

  # Get tree hash
  hash @5 () -> (hash :Hash);

  # Find hash for value associated with `key`
  findHash @6 (key :Key) -> (hash :Hash);

  list @7 (key :Key) -> (items :List(Step));

  # Check if tree exists
  check       @8 () -> (bool :Bool);
}

# Writeable trees
interface Tx {
  # Add a value
  add @0 (key :Key, contents :Contents) -> ();

  # Add a tree
  addTree @1 (key :Key, tree :Tree) -> ();

  # Remove a value from the tree
  remove   @2  (key :Key) -> ();

  tree @3 () -> (tree :Tree);

  addContents @4 (key :Key, hash :Hash) -> ();
}

interface Commit {
  struct Value {
    hash     @0  :Hash;
    info     @1  :Info;
    parents  @2  :List(Hash);
    tree     @3  :Tree.Concrete;
  }

  read     @0  () -> (value :Value);

  # Get commit tree
  tree     @1  () -> (tree :Tree);

  # Get commit parent hashes
  parents  @2  () -> (hashes :List(Hash));

  # Get commit info
  info     @3  () -> (info :Info);

  # Get commit hash
  hash     @4  () -> (hash :Hash);

  # Check if commit exists
  check    @5 () -> (bool :Bool);
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

  # Push to a remote endpoint
  push   @0  (endpoint :Endpoint) -> (result :PushResult);

  # Pull from a remote endpoint
  pull   @1  (endpoint :Endpoint, info :Info) -> (result :Commit);

  # Clone a remote endpoint
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

  # Check the integrity of pack and index files
  integrityCheck @0 (pack :Pack, autoRepair :Bool) -> (result :IntegrityCheckResult);
}

interface Store {
  # Find the value for `key`
  find      @0  (key :Key) -> (contents :Contents);

  # Get the tree with a root at `key`
  findTree  @1  (key :Key) -> (tree :Tree);

  # Set `key` to `contents`
  set       @2  (key :Key,  contents :Contents, info :Info) -> ();

  # Set tree with a root at `key`
  setTree   @3  (key :Key, tree :Tree, info :Info) -> ();

  # Remove the value associated with `key`
  remove    @4  (key :Key, info :Info) -> ();

  # Check if contents exist at `key`
  mem       @5  (key :Key) -> (exists :Bool);

  # Check if a tree exists with a root at `key`
  memTree   @6  (key :Key) -> (exists :Bool);

  # Merge API on stores
  struct MergeResult {
    union {
      ok        @0  :Void;
      errorMsg  @1  :Text;
    }
  }

  # Merge the current branch with another branch
  mergeWithBranch  @7  (branch :Text, info :Info) -> (result :MergeResult);

  # Access `Irmin.Sync` functionality`
  sync  @8  () -> (sync :Sync);

  # Access `Irmin_pack` functionality
  pack @9 () -> (pack :Pack);

  # Get the commit that last modified `key`
  lastModified  @10 (key :Key) -> (commit :Commit);

  # Get a hash of the contents stored at `key`
  findHash @11 (key :Key) -> (hash :Hash);

  testAndSet @12 (key :Key, test :Contents, set :Contents, info :Info) -> ();
  testAndSetTree @13 (key :Key, test :Tree, set :Tree, info :Info) -> ();
}

interface Repo {
  # Get a Store handle for the main branch
  master    @0  () -> (store :Store);

  # Get a Store handle for the given branch
  ofBranch  @1  (branch :Text) -> (store :Store);

  # List all branches
  branchList    @2  () -> (branches :List(Text));

  # Remove a branch
  branchRemove  @3  (branch :Text) -> ();

  # Create a new branch using the given branch name and `commit`
  branchSet     @4  (branch :Text, commit :Commit) -> ();

  # Find commit for `hash`
  commitOfHash   @5  (hash :Hash) -> (commit :Commit);

  # Find contents for `hash`
  contentsOfHash @6  (hash :Hash) -> (contents :Contents);

  # Create an empty tree
  emptyTree @7 () -> (tree :Tree);

  # Create commit
  createCommit @8 (info :Info, parents :List(Hash), tree :Tree) -> (commit :Commit);

  # Add contents and return hash
  importContents @9 (values :List(Contents)) -> (hash :List(Hash));

  # Convert a concrete tree to `Tree`
  treeOfConcrete @10 (concrete :Tree.Concrete) -> (tree :Tree);


  tx @11 (tree :Tree) -> (tx :Tx);
}

# The top-level interface of an RPC server
interface Irmin {

  # Each RPC server monitors exactly one repository
  repo  @0  () -> (repo :Repo);

  # Check availability of server
  ping @1 () -> ();
}
