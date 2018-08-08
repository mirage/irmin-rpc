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
      key @0 :List(Text);
      union {
          contents @1 :Data;
          node @2 :List(Node);
      }
  }

  master @0 () -> (result :Branch);
  getBranch @1 (name :Text) -> (result :Branch);
  get @2 (branch :Branch, key :List(Text)) -> (result :Data);
  set @3 (branch :Branch, key :List(Text), value :Data) -> (result :Bool);
  remove @4 (branch :Branch, key :List(Text)) -> ();
  getTree @5 (branch :Branch, key :List(Text)) -> (result :Tree);
}
