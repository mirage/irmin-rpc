@0x893d5c0632fd10a9;

interface Irmin {
  struct Branch {
      name @0 :Text;
      head @1 :Commit;
  }

  struct Commit {
      hash @0 :Data;
      info @1 :Info;
      parents @2 :List(Commit);
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

  master @0 () -> (result :Branch);
  getBranch @1 (name :Text) -> (result :Branch);
  get @2 (branch :Text, key :Text) -> (result :Data);
  set @3 (branch :Text, key :Text, value :Data) -> (result :Bool);
  remove @4 (branch :Text, key :Text) -> ();
  getTree @5 (branch :Text, key :Text) -> (result :Tree);
}
