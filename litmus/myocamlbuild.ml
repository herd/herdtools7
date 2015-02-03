open Ocamlbuild_plugin;;
open Command;;

let sh = A"sh";;
let pp = A"./pp2ml.sh";;

dispatch begin function
  | After_rules ->
      rule "pp: .mly.tpl -> mly"
        ~prods:["%.mly";]
        ~deps:["pp2ml.sh";"%.mly.tpl";]
      begin fun env _build ->
        Cmd(S[sh; pp; P(env "%.mly.tpl")])
      end ;
      rule "pp: .mll.tpl -> mll"
        ~prods:["%.mll";]
        ~deps:["pp2ml.sh";"%.mll.tpl";]
      begin fun env _build ->
        Cmd(S[sh; pp; P(env "%.mll.tpl")])
      end ;
      ()
  | _ -> ()
end
