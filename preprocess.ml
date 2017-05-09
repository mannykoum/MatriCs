let process filename =
    let rec read_file filename =
      let file_regex = Str.regexp "<.+\\.neo"  in
      let inc_regex = Str.regexp "#include[ ]+<.+>" in
      let has_file l =
        let has_inc l =
          try ignore (Str.search_forward inc_regex l 0); true
          with Not_found -> false
        in
        if has_inc l then
          try ignore(Str.search_forward file_regex l 0); true
          with Not_found -> false
        else false
      in
    let lines = ref [] in
    let ic = open_in filename in
    try
      while true; do
        let l = input_line ic in
        let l = (if (has_file l) then ( Str.replace_first inc_regex
        (read_file (Str.string_after (Str.matched_string l) 1) )l ) else l) in
        lines:=  l :: !lines;
      done;
      String.concat "" (List.map (fun i -> i ^ String.make 1 '\n') (List.rev !lines))
    with End_of_file -> ignore(close_in ic);
    String.concat "" (List.map (fun i -> i ^ String.make 1 '\n') (List.rev !lines))
in read_file filename