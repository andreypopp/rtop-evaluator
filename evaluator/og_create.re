open Unix;

let execute = cmd => {
  let s = String.concat(" ", cmd);
  let ret = Unix.open_process_in(s);
  let output = ref("");
  try (
    while (true) {
      let l = input_line(ret);
      output := output^ ++ l ++ "\n";
    }
  ) {
  | End_of_file => ()
  };
  output^;
};

let filtered_files = dir => {
  let all_files = Array.to_list(Sys.readdir(dir));
  List.filter(
    filename =>
      filename != "index.html"
      && filename != "META"
      && filename != "opam"
      && filename != "opam.config",
    all_files,
  );
};

let read_file = filename => {
  let buffer_size = 8192;
  let buffer = Bytes.create(buffer_size);
  let fd_in = openfile(filename, [O_RDONLY], 0);
  let result = ref("");
  let rec read_loop = () =>
    switch (read(fd_in, buffer, 0, buffer_size)) {
    | 0 => ()
    | r =>
      result := result^ ++ buffer;
      read_loop();
    };

  read_loop();
  result^;
};

let write_file = (file, s) => {
  let channel = open_out(file);
  output_string(channel, s);
  close_out(channel);
};

let create_index_html = (og_folder, input, output_folder) => {
  let folder = Filename.concat(Sys.getcwd(), input);
  let files = Array.to_list(Sys.readdir(folder));
  let inputHTML = ref("");
  List.iter(
    f => {
      let filename = Filename.concat(folder, f);
      if (!Sys.is_directory(filename)) {
        inputHTML :=
          inputHTML^ ++ "<div data-ocaml>" ++ read_file(filename) ++ "</div>";
      };
    },
    files,
  );
  let index_file = read_file(Filename.concat(og_folder, "index.html"));
  let index_content =
    Str.replace_first(
      Str.regexp("<!-- replace me -->"),
      inputHTML^,
      index_file,
    );
  write_file(Filename.concat(output_folder, "index.html"), index_content);
};

let file_copy = (input_name, output_name) => {
  let buffer_size = 8192;
  let buffer = Bytes.create(buffer_size);
  if (Sys.file_exists(output_name)) {
    failwith("output name already exists:" ++ output_name);
  };
  let fd_in = openfile(input_name, [O_RDONLY], 0);
  let fd_out = openfile(output_name, [O_WRONLY, O_CREAT, O_TRUNC], 438);
  let rec copy_loop = () =>
    switch (read(fd_in, buffer, 0, buffer_size)) {
    | 0 => ()
    | r =>
      ignore(write(fd_out, buffer, 0, r));
      copy_loop();
    };

  copy_loop();
  close(fd_in);
  close(fd_out);
};

let create_temp_dir = () => {
  let tmp = Printf.sprintf("%.30f", Unix.gettimeofday());
  let tmp_dir = ref(Filename.concat(Filename.get_temp_dir_name(), tmp));
  while (Sys.file_exists(tmp_dir^)) {
    let tmp = Printf.sprintf("%.30f", Unix.gettimeofday());
    tmp_dir := Filename.concat(Filename.get_temp_dir_name(), tmp);
  };
  Unix.mkdir(tmp_dir^, 493);
  tmp_dir^;
};

let do_stuff = (owl, og, output, input, libs, deps, useDocs) => {
  let tmp_dir = create_temp_dir();

  let owl_files = filtered_files(owl);
  let og_files = filtered_files(og);

  if (Sys.file_exists(output) == false) {
    Unix.mkdir(output, 493);
  };

  /* copy the files to the output */
  List.iter(
    file =>
      file_copy(Filename.concat(owl, file), Filename.concat(tmp_dir, file)),
    owl_files,
  );
  List.iter(
    file =>
      file_copy(Filename.concat(og, file), Filename.concat(tmp_dir, file)),
    og_files,
  );
  let folder = Sys.getcwd();
  let files =
    List.filter(
      f => !Sys.is_directory(f),
      Array.to_list(Sys.readdir(folder)),
    );
  List.iter(
    file =>
      file_copy(
        Filename.concat(folder, file),
        Filename.concat(tmp_dir, file),
      ),
    files,
  );
  create_index_html(og, input, tmp_dir);

  /* packages that should be available in the gist tool */
  let export_packages =
    List.fold_left(
      (deps, acc) =>
        deps
        ++ (
          if (acc.[0] == '+') {
            " -jsopt " ++ acc;
          } else {
            " -export-package " ++ acc;
          }
        ),
      "",
      libs,
    );

  /* local libraries that should be available in the gist tool*/
  let deps2 =
    List.fold_left(
      (deps, acc) => deps ++ "-jsopt \" -I . --file " ++ acc ++ "\"",
      "",
      deps,
    );

  /* create the actual webworker */
  let result =
    execute([
      "cd " ++ tmp_dir,
      "&&",
      "jsoo_mktop",
      "-jsopt",
      "\"--disable genprim\"",
      /* "-g"; */
      /* "-jsopt"; "--source-map-inline"; */
      "-package",
      "str",
      "-package",
      "unix",
      /* "-jsopt"; "--no-inline"; */
      /* "-jsopt"; "--pretty"; */
      /* "-jsopt"; "--debug-info"; */
      /* "-jsopt";"--opt=3"; */
      "-jsopt",
      "+weak.js",
      "-jsopt",
      "+toplevel.js",
      "-jsopt",
      "+nat.js",
      "-jsopt",
      "+dynlink.js",
      export_packages,
      deps2,
      Filename.concat(owl, "ocaml_webworker.cma"),
      "-o",
      Filename.concat(tmp_dir, "ocaml_webworker.js"),
    ]);

  print_endline(result);

  /* create the cmi files */
  /* TODO: use cmti files instead when available */
  let cmi_files =
    List.map(file => Filename.chop_extension(file) ++ ".cmi", deps);
  let non_js =
    List.filter(
      file =>
        if (file.[0] != '+') {
          true;
        } else {
          false;
        },
      libs,
    );
  let js =
    List.filter(
      file =>
        if (file.[0] == '+') {
          true;
        } else {
          false;
        },
      libs,
    );
  if (List.length(non_js) > 0) {
    if (useDocs) {
      let cmti_bundler_results =
        execute(["cd", tmp_dir, "&&", "cmti-bundler"] @ non_js);
      print_endline(cmti_bundler_results);
      let cmi_result =
        execute(
          ["cd", tmp_dir, "&&", "jsoo_mkcmis", "./cmtis/*.cmi"]
          @ js
          @ cmi_files
          @ ["-o", Filename.concat(tmp_dir, "cmi.js")],
        );
      print_endline(cmi_result);
    } else {
      let cmi_result =
        execute(
          ["cd", tmp_dir, "&&", "jsoo_mkcmis"]
          @ non_js
          @ cmi_files
          @ ["-o", Filename.concat(tmp_dir, "cmi.js")],
        );
      print_endline(cmi_result);
    };
  } else {
    let cmi_js =
      execute(
        ["cd", tmp_dir, "&&", "jsoo_mkcmis"]
        @ js
        @ cmi_files
        @ ["-o", Filename.concat(tmp_dir, "cmi.js")],
      );
    print_endline(cmi_js);
  };

  let mv_result =
    execute([
      "mv",
      Filename.concat(tmp_dir, "*.js"),
      Filename.concat(tmp_dir, "*.html"),
      Filename.concat(tmp_dir, "*.svg"),
      Filename.concat(tmp_dir, "*.css"),
      output,
    ]);
  print_endline(mv_result);
};

let create = () =>
  try (
    {
      let owl = Findlib.package_directory("ocaml-webworker");
      let og = Findlib.package_directory("ocaml-gist");
      open Cmdliner;
      let output = {
        let doc = "Output folder";
        Arg.(value & opt(dir, ".") & info(["output", "o"], ~doc));
      };

      let input = {
        let doc = "Input folder";
        Arg.(value & opt(dir, ".") & info(["input", "i"], ~doc));
      };

      let deps = {
        let doc = "Dependency (should be a .cmo/.cma file)";
        Arg.(value & opt_all(file, []) & info(["dependency", "d"], ~doc));
      };

      let libs = {
        let doc = "Libraries";
        Arg.(value & opt_all(string, []) & info(["library", "l"], ~doc));
      };

      let useDocs = {
        let doc = "Show documentation (increases filesize considerably)";
        Arg.(value & flag & info(["doc"], ~doc));
      };

      let out = ref(Filename.current_dir_name);
      let input_ = ref(Filename.current_dir_name);
      let dependencies = ref([]);
      let libraries = ref([]);
      let useDocs_ = ref(false);
      let exec = (output, input, deps, libs, docs) => {
        out := output;
        input_ := input;
        dependencies := deps;
        libraries := libs;
        useDocs_ := docs;
        if (out^ == "."
            && input_^ == "."
            && dependencies^ == []
            && libraries^ == []
            && useDocs_^ == false) {
          ignore(Unix.system("og-create --help"));
        } else {
          /* sue me */
          do_stuff(
            owl,
            og,
            out^,
            input_^,
            libraries^,
            dependencies^,
            useDocs_^,
          );
        };
      };
      let t = Term.(const(exec) $ output $ input $ deps $ libs $ useDocs);
      ignore(Term.eval((t, Term.info("og-create"))));
    }
  ) {
  | [@implicit_arity] Findlib.No_such_package(p, msg) =>
    print_endline(p ++ ":" ++ msg)
  };

let () = create();
