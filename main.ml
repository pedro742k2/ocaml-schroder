(* Operações e conversão com inteiros do tipo Z.t *)
let ( +! ) = Z.add
let ( *! ) = Z.mul
let ( -! ) = Z.sub
let ( /! ) = Z.div
let ( !~ ) = Z.of_int

(* Inteiros do tipo Z.t estáticos *)
let one   = !~1 
let two   = !~2

(*
Função (s1) da primeira fórmula.
Parâmetros:
  - "n" é o valor a ser calculado
*)
let s1 n =
  (* Contador *)
  let count = ref 0 in
  (* Função da fórmula *)
  let rec formula n =
    (* Incremento do contador a cada chamada *)
    incr count;
      match n with
      | 0 -> 1
      | 1 -> 2
      | _ ->
      (* Variável acumuladora do somatório *)
      let sum = ref 0 in
      (* Somatório com ciclo for*)
      for k=1 to (n-2) do
        sum := !sum + (formula(k) * formula(n-k-1));
      done;
      (* Retorno do resultado final da fórmula em n *)
      3*formula(n-1) + !sum
  (* Chama a função com a fórmula *)
  in let res = formula(n)
  (* Retorna um tuplo com o resultado (res) e o contador de chamadas (count) *)
  in (res, !count)

(*
Função (s2) da segunda fórmula.
Parâmetros:
  - "n" é o valor a ser calculado
*)
let s2 n =
  (* Contador *)
  let count = ref 0 in
  (* Função da fórmula *)
  let rec formula n =
    (* Incremento do contador a cada chamada *)
    incr count;
    match n with
    | 0 -> 1
    | 1 -> 2
    | _ -> 
    (* Expressão *)
    ((6*n-3)*formula(n-1) - (n-2)*formula(n-2)) / (n+1);
  (* Chama a função com a fórmula *)
  in let res = formula(n)
  (* Retorna um tuplo com o resultado (res) e o contador de chamadas (count) *)
  in (res, !count);;

(*
Função (s2_opt) baseada na segunda fórmula (s2), mas otimizada.
Parâmetros:
  - "n" é o valor a ser calculado
*)
let s2_opt n =
  (* "Hash table" para armazenar valores pré-calculados, de forma a evitar calculos repetidos.
  A tabela é inicializada com 10000 posições previamente disponíveis, sendo automaticamente incrementadas se necessário pelo programa. *)
  let hash_table = Hashtbl.create 10000 in
  (* Função da fórmula *)
  let rec formula n =
    match n with
    | 0 -> one
    | 1 -> two
    | _ ->
    (* exp1 = formula(n-1). Procura esse valor na tabela de hash. Caso não encontre, calcula o seu valor e adiciona-o à tabela *)
    let exp1 = try Hashtbl.find(hash_table)(n-1) with Not_found -> let res = formula(n-1) in let () = Hashtbl.add(hash_table)(n-1)(res) in res in

    (* exp2 = formula(n-2). Procura esse valor na tabela de hash. Caso não encontre, calcula o seu valor e adiciona-o à tabela *)
    let exp2 = try Hashtbl.find(hash_table)(n-2) with Not_found -> let res = formula(n-2) in let () = Hashtbl.add(hash_table)(n-2)(res) in res in

    (* Expressão *)
    ((!~(6*n-3))*!exp1 -! !~(n-2)*!exp2) /! !~(n+1);
  (* Chama a função com a fórmula *)
  in let res = formula(n) in res;;

(* Declaração da excessão caso o input seja inválido *)
exception Invalid_input

(* Leitura do stdin (dois valores inteiros separados por espaço) *)
let (a, b) = Scanf.scanf "%d %d" (fun x y -> (x, y));;

(* Verificação dos inputs e execução das fórmulas *)
let () =
  if (a < 0 || a > 20 || b < 0 || b > 10000)
  then raise Invalid_input else
    let (r1, c1) = s1 a in
    let (r2, c2) = s2 a in
    let r3 = s2_opt b in
    Printf.printf "%d %d\n%d %d\n"(r1)(c1)(r2)(c2); Z.print r3; print_endline "";;
