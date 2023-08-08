open OUnit2
open Problems.Lists

let test_length _ =
  assert_equal 0 (length []);
  assert_equal 1 (length [ 1 ]);
  assert_equal 3 (length [ 1; 2; 3 ])

let test_nth _ =
  assert_equal (nth 2 [ 1; 2; 3 ]) (Some 2);
  assert_equal (nth 8 []) None

let test_last _ =
  assert_equal (last []) None;
  assert_equal (last [ 1; 2; 3 ]) (Some 3);
  assert_equal (last [ "a"; "b" ]) (Some "b")

let test_last_two _ =
  assert_equal (last_two []) None;
  assert_equal (last_two [ 1 ]) None;
  assert_equal (last_two [ 1; 2; 3; 4 ]) (Some (3, 4))

let suite =
  "ExampleTestList"
  >::: [
         "test_length" >:: test_length;
         "test_nth" >:: test_nth;
         "test_last" >:: test_last;
         "test_last_two" >:: test_last_two;
       ]

let () = run_test_tt_main suite
