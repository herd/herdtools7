let seq_split_at_specs = [
  ("Empty sequence", (1, []), ([], []))
; ("Negative elements", (-41, [1]), ([], [1]))
; ("Some elements", (-41, [41; 441; 4441]), ([], [41; 441; 4441]))
; ("Zero elements", (0, [41; 441; 4441]), ([], [41; 441; 4441]))
; ("Some elements", (2, [41; 441; 4441]), ([41; 441], [4441]))
; ("All elements", (41, [41; 441; 4441]), ([41; 441; 4441], []))
]

let seq_split_at_tests =
  let test (n, input_lst) expected () =
    let input_seq = List.to_seq input_lst in
    let expected = (fst expected |> List.to_seq, snd expected |> List.to_seq) in
    let result = Base.Seq.split_at n input_seq in
    Alcotest.(check @@ pair (seq int) (seq int))
      "Seqs must match" expected result
  in
  List.map
    (fun (name, args, expected) ->
      let name = Printf.sprintf "split_at: %s" name in
      (name, `Quick, test args expected)
    )
    seq_split_at_specs

let seq_for_every_element_tests = [
  ("Empty sequence", ((fun _ -> false), []), true)
; ("All are zero [0]", ((fun x -> x = 0), [0]), true)
; ("All are zero [1; 0]", ((fun x -> x = 0), [1; 0]), false)
; ("All are zero [0; 1]", ((fun x -> x = 0), [0; 1]), false)
]

let seq_for_every_element_tests =
  let evaluation_test () =
    let result = ref 0 in
    let sum = (fun x -> result := x + !result; false) in
    ignore (Base.Seq.for_every_element sum ([1; 2; 3; 4] |> List.to_seq) : bool) ;
    Alcotest.(check int) "Function was called for all elements" 10 !result
  in
  let test (p, input_lst) expected () =
    let input_seq = List.to_seq input_lst in
    let result = Base.Seq.for_every_element p input_seq in
    Alcotest.(check bool) "Booleans must match" expected result
  in
  List.map
    (fun (name, args, expected) ->
       let name = Printf.sprintf "for_every_element: %s" name in
       (name, `Quick, test args expected)
    )
    seq_for_every_element_tests
    |> List.cons ("for_every_element: all elements are evaluated", `Quick, evaluation_test)

  let seq_tests = ("Seq", List.concat [seq_split_at_tests; seq_for_every_element_tests])

let () = Alcotest.run "Base library" [seq_tests]
