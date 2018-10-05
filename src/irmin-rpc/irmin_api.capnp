@0x893d5c0632fd10a9;

interface Irmin {
  struct Branch {
      name @0 :Text;
      head @1 :Commit;
  }

  struct Info {
      author @0 :Text;
      message @1 :Text;
      date @2 :Int64;
  }

  enum Kind {
      contents @0;
      node @1;
  }

  struct Node {
      step @0 :Text;
      tree @1 :Tree;
  }

  struct Tree {
      key @0 :Text;
      union {
          contents @1 :Data;
          node @2 :List(Node);
      }
  }

  struct Commit {
      hash @0 :Data;
      info @1 :Info;
      parents @2 :List(Commit);
      tree @3 :Tree;
  }

  find @0 (branch :Text, key :Text) -> (result :Data);
  set @1 (branch :Text, key :Text, value :Data, author :Text, message :Text) -> (result :Commit);
  remove @2 (branch :Text, key :Text, author :Text, message :Text) -> (result :Commit);
  findTree @3 (branch :Text, key :Text) -> (result :Tree);
  setTree @4 (branch :Text, key :Text, tree :Tree, author :Text, message :Text) -> (result :Commit);
  push @5 (branch :Text, remote :Text) -> ();
  pull @6 (branch :Text, remote :Text, author :Text, message :Text) -> (result :Commit);
  clone @7 (branch :Text, remote :Text) -> (result :Commit);
  merge @8 (branchFrom :Text, branchInto :Text, author :Text, message :Text) -> (result :Commit);
  commitInfo @9 (hash :Data) -> (result :Info);
  snapshot @10 (branch :Text) -> (result :Data);
  revert @11 (branch :Text, hash :Data) -> (result :Bool);
  branches @12 () -> (result :List(Text));
  commitHistory @13 (hash :Data) -> (result :List(Data));
  removeBranch @14 (branch :Text);
}
