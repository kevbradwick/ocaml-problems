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

let test_rev _ =
  assert_equal [] (rev []);
  assert_equal [ 3; 2; 1 ] (rev [ 1; 2; 3 ]);
  assert_equal [ 1 ] (rev [ 1 ])

let test_is_palindrome _ =
  assert_equal true (is_palindrome []);
  assert_equal true (is_palindrome [ "a"; "b"; "a" ]);
  assert_equal false (is_palindrome [ "a"; "b" ])

let test_flatten _ =
  assert_equal
    [ "a"; "b"; "c"; "d"; "e" ]
    (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])

let test_compress _ =
  assert_equal [ "a" ] (compress [ "a"; "a" ]);
  assert_equal [ "a"; "b"; "c" ] (compress [ "a"; "b"; "b"; "c" ]);
  assert_equal [] (compress [])

let test_pack _ =
  assert_equal [ [ "a" ]; [ "b"; "b" ] ] (pack [ "a"; "b"; "b" ]);
  assert_equal [] (pack []);
  assert_equal [ [ "a" ] ] (pack [ "a" ])

let test_encode _ =
  assert_equal
    [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
    (encode
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])

let test_encode_modified _ = assert_equal [ One "a" ] (encode_modified [ "a" ])

let test_decode _ =
  assert_equal [ "a"; "b"; "b"; "c" ]
    (decode [ One "a"; Many (2, "b"); One "c" ])

let suite =
  "ExampleTestList"
  >::: [
         "test_length" >:: test_length;
         "test_nth" >:: test_nth;
         "test_last" >:: test_last;
         "test_last_two" >:: test_last_two;
         "test_rev" >:: test_rev;
         "test_is_palindrome" >:: test_is_palindrome;
         "test_flatten" >:: test_flatten;
         "test_compress" >:: test_compress;
         "test_pack" >:: test_pack;
         "test_encode" >:: test_encode;
         "test_encode_modified" >:: test_encode_modified;
         "test_decode" >:: test_decode;
       ]

let () = run_test_tt_main suite
