open Irmin_rpc.Private.Utils

module String = struct
  let test_to_list () =
    let check input output =
      String.to_list input |> Alcotest.(check (list char)) input output
    in
    check "" [];
    check "x" [ 'x' ];
    check "abc" [ 'a'; 'b'; 'c' ];
    ()

  let test_is_substring () =
    let check a b output =
      String.is_substring a b |> Alcotest.(check bool) b output
    in
    check "" "" true;
    check "" "abc" true;
    check "x" "x" true;
    check "foo" "foo" true;
    check "bar" "foo bar baz" true;
    check "bad" "foo bar baz" false;
    ()
end

let suite =
  let test_case name fn = Alcotest_lwt.test_case_sync name `Quick fn in
  [
    test_case "String.to_list" String.test_to_list;
    test_case "String.is_substring" String.test_is_substring;
  ]
