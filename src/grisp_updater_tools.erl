-module(grisp_updater_tools).


%--- Exports -------------------------------------------------------------------

%% API functions
-export([main/1]).


%--- API Functions -------------------------------------------------------------

%% escript entry point
main([]) -> usage();
main(Args) ->
    case getopt:parse(opts_spec(), Args) of
        {ok, {OptList, []}} ->
            SysFiles = lists_getall(system_file, OptList),
            Cmd = command(OptList),
            Opts = proplists_to_map(OptList),
            Opts2 = Opts#{system_file => SysFiles},
            perform(Cmd, validate(Cmd, Opts2));
        {ok, {_Opts, Extra}} ->
            command_error("Unexpected extra parameters: ~p~n", [Extra]);
        {error, {invalid_option, Data}} ->
            command_error("Invalid option ~s~n", [Data]);
        {error, {Reason, Data}} ->
            command_error("Internal error ~w: ~p~n", [Reason, Data])
    end.


%--- Internal Functions --------------------------------------------------------

opts_spec() -> [
    {help,                    $h,               "help",  undefined,
         "Show usage"},
    {name,                    $n,               "name",  string,
         "Software product name"},
    {version,                 $v,            "version",  string,
         "Software product version"},
    {vcs,              undefined,                "vcs",  string,
         "Version control identifier"},
    {uuid,             undefined,               "uuid",  string,
         "Software update unique identifier"},
    {author,             undefined,            "author", string,
         "Author of the software update"},
    {desc,                    $d,               "desc",  string,
         "Software product description"},
    {tar,                     $t,                 "tar", {boolean, false},
         "Update block size"},
    {block_size,              $b,         "block-size", {integer, 4194304},
         "Update block size"},
    {key_file,         undefined,           "key-file",  string,
         "Signing key PEM file"},
    {bootloader_image, undefined,   "bootloader-image",  string,
         "Bootloader image"},
    {kernel_path,      undefined,        "kernel-path",  string,
         "Kernel path"},
    {kernel_image,     undefined,       "kernel-image",  string,
         "Kernel image"},
    {architecture,     undefined,       "architecture",  {string, "arm-grisp2-rtems"},
         "Architecture; e.g. arm-grisp2-rtems, x86_64-vendor-linux"},
    {platform,         undefined,           "platform",  string,
        "System platform; e.g. grisp2, kontron-albl-imx8mm"},
    {system_file,      undefined,        "system-file",  string,
         "Extra system file as NAME:SYSTEM_PATH:REAL_PATH[:REAL_URL]; "
         "e.g. key:/boot/rootfs_key:build/rootfs.ext2.encrypted.key:https://server.com/rootfs.ext2.encrypted.key"},
    {structure,        undefined,            undefined,  string,
         "Filesystem structure, either MBR or GPT, where offset are expressed in sectors (512 bytes); "
         "mbr=(START:SIZE[,])* e.g. mbr=24576:507904,548864:507904 "
         "gpt=(TYPE:UUID:START:SIZE[,])* e.g. gpt=L:9e38701c-cc06-4f03-bdb9-f5ff687476da:24576:507904,L:56ca8d1e-1e63-43e7-89af-b1d97dfff15f:548864:507904"},
    {rootfs_image,     undefined,            undefined,  string,
         "Root filesystem image"},
    {output_path,      undefined,            undefined,  string,
         "Output directory or taball file"}
].

command_error(Fmt, Args) ->
    io:format(standard_error, Fmt, Args),
    erlang:halt(1).

command(Opts) ->
    case lists:member(help, Opts) of
        true -> help;
        false -> package
    end.

validate(package, Opts) ->
    SysFiles = maps:get(system_file, Opts, []),
    required(Opts, name, "software product name"),
    required(Opts, version, "software product version"),
    Struct = required(Opts, structure, "filesystem structure"),
    required(Opts, rootfs_image, "rootfs image file"),
    required(Opts, output_path, "output directory or tarball file"),
    file_exists(Opts, rootfs_image, "root file-system image"),
    does_not_exist(Opts, output_path, "output directory or tarball file"),
    file_exists(Opts, bootloader_image, "bootloader image"),
    file_exists(Opts, kernel_image, "kernel image"),
    KeyFile = file_exists(Opts, key_file, "signing key PEM file"),
    required_if(Opts, kernel_image, kernel_path, "kernel path"),
    required_if(Opts, kernel_path, kernel_image, "kernel image"),
    {StructureType, StructureSpecs} = parse_structure(Struct),
    Opts#{
        key => load_key(KeyFile),
        system_file => parse_sysfiles(SysFiles),
        structure_type => StructureType,
        structure => StructureSpecs
    };
validate(_Cmd, Opts) ->
    Opts.

lists_getall(Key, List) ->
    lists_getall(Key, List, []).

lists_getall(_Key, [], Acc) ->
    lists:reverse(Acc);
lists_getall(Key, [{Key, Val} | Rest], Acc) ->
    lists_getall(Key, Rest, [Val | Acc]);
lists_getall(Key, [_ | Rest], Acc) ->
    lists_getall(Key, Rest, Acc).

required(Opts, Name, Desc) ->
    case maps:find(Name, Opts) of
        {ok, Value} -> Value;
        error -> command_error("Missing ~s argument~n", [Desc])
    end.

required_if(Opts, Dep, Name, Desc) ->
    case maps:find(Dep, Opts) of
        {ok, _} -> required(Opts, Name, Desc);
        error -> ok
    end.

file_exists(Opts, Name, Desc) ->
    case maps:find(Name, Opts) of
        error -> undefined;
        {ok, Filename} ->
            case filelib:is_file(Filename) of
                false ->
                    command_error("Specified ~s not found: ~s~n",
                                  [Desc, Filename]);
                true ->
                    case filelib:is_regular(Filename) of
                        false ->
                            command_error("Specified ~s not a file: ~s~n",
                                          [Desc, Filename]);
                        true ->
                            Filename
                    end
            end
    end.

does_not_exist(Opts, Name, Desc) ->
    case maps:find(Name, Opts) of
        error -> ok;
        {ok, Path} ->
            case filelib:is_file(Path) of
                true ->
                    command_error("Specified ~s already exists: ~s~n",
                                  [Desc, Path]);
                false ->
                    ok
            end
    end.

load_key(undefined) -> undefined;
load_key(PEMFile) ->
    try termseal:load_private_key(PEMFile)
    catch
        {read_error, Reason, Filename} ->
            command_error("Failed to read signing key (~p): ~s~n",
                          [Reason, Filename]);
        {key_not_found, Filename} ->
            command_error("Signing key PEM file not found: ~s~n",
                          [Filename]);
        {too_many_keys, Filename} ->
            command_error("Signing key PEM file with multiple keys: ~s~n",
                          [Filename])
    end.

perform(help, _Opts) -> usage();
perform(package, Opts) -> package(Opts).

% New in OTP 24
proplists_to_map(List) ->
    lists:foldr(
        fun
            ({K, V}, M) ->
                M#{K => V};
            %% if tuples with arity /= 2 appear before atoms or
            %% tuples with arity == 2, get_value/2,3 returns early
            (T, M) when 1 =< tuple_size(T) ->
                maps:remove(element(1, T), M);
            (K, M) when is_atom(K) ->
                M#{K => true};
            (_, M) ->
                M
        end,
        #{},
        List
    ).

parse_structure(undefined) ->
    undefined;
parse_structure("mbr=" ++ Struct) ->
    {mbr, [#{role => partition_role(Role), type => mbr_type(Type),
             start => list_to_integer(Start) * 512,
             size => list_to_integer(Size) * 512}
           || [Role, Type, Start, Size]
           <- [string_split(P, ":", all)
               || P <- string:split(Struct, ",", all)]]};
parse_structure("gpt=" ++ Struct) ->
    {gpt, [#{role => partition_role(Role), type => gpt_type(Type),
            id => gpt_id(Id), start => list_to_integer(Start) * 512,
            size => list_to_integer(Size) * 512}
           || [Role, Type, Id, Start, Size]
                <- [string:split(P, ":", all)
                    || P <- string:split(Struct, ",", all)]]};
parse_structure(Struct) ->
    command_error("Invalid specified structure: ~p~n", [Struct]).

partition_role("system") -> system;
partition_role("boot") -> boot;
partition_role("data") -> data;
partition_role("reserved") -> reserved.

mbr_type("dos") -> fat;
mbr_type("fat") -> fat.

gpt_type("R") -> reserved;
gpt_type("L") -> linux;
gpt_type("S") -> swap;
gpt_type("H") -> home;
gpt_type("U") -> efi;
gpt_type("V") -> llvm;
gpt_type("F") -> bdp;
gpt_type(Other) ->
    try
        uuid:string_to_uuid(Other),
        Other
    catch
        error:badarg ->
            command_error("Invalid GPT partition UUID type: ~p~n", [Other])
    end.

gpt_id(Id) ->
    try
        uuid:string_to_uuid(Id),
        Id
    catch
        error:badarg ->
            command_error("Invalid GPT partition UUID identifier: ~p~n", [Id])
    end.


parse_sysfiles(SysFiles) ->
    parse_sysfiles(SysFiles, []).

string_split(Val, Sep, all) ->
    string_split(Val, Sep, all, []);
string_split(Val, Sep, Count) ->
    string_split(Val, Sep, Count - 1, []).

string_split(Val, Sep, all, Acc) ->
    case string:split(Val, Sep) of
        [Val] -> lists:reverse([Val | Acc]);
        [Field, Rest] -> string_split(Rest, Sep, all, [Field | Acc])
    end;
string_split(Val, _Sep, 0, Acc) ->
    lists:reverse([Val | Acc]);
string_split(Val, Sep, Count, Acc) ->
    case string:split(Val, Sep) of
        [Val] -> lists:reverse([Val | Acc]);
        [Field, Rest] -> string_split(Rest, Sep, Count - 1, [Field | Acc])
    end.


parse_sysfiles([], Acc) ->
    lists:reverse(Acc);
parse_sysfiles([Val | Rest], Acc) ->
    case string_split(Val, ":", 4) of
        [Name, "/" ++ _ = Target, Source, Url] ->
            case uri_string:parse(Url) of
                #{scheme := Schem} when Schem =:= "http"; Schem =:= "https" ->
                    case filelib:is_file(Source) of
                        false -> command_error("System file not found: ~s~n", [Source]);
                        true ->
                            Spec = #{
                                name => list_to_binary(Name),
                                local => list_to_binary(Source),
                                target => list_to_binary(Target),
                                url => list_to_binary(Url)
                            },
                            parse_sysfiles(Rest, [Spec | Acc])
                    end;
                _ ->
                    command_error("Bad system file URL: ~s~n", [Url])
            end;
        [Name, "/" ++ _ = Target, Source] ->
            case filelib:is_file(Source) of
                false -> command_error("System file not found: ~s~n", [Source]);
                true ->
                    Spec = #{
                        name => list_to_binary(Name),
                        local => list_to_binary(Source),
                        target => list_to_binary(Target)
                    },
                    parse_sysfiles(Rest, [Spec | Acc])
            end;
        [_, Target, _] ->
            command_error("Bad system file target: ~s~n", [Target])
    end.

usage() ->
    getopt:usage(opts_spec(), "grisp_updater_tools"),
    erlang:halt(0).

package(Opts) ->
    #{
        output_path := OutputPath,
        tar := Tar,
        system_file := SystemFileSpecs0,
        structure_type := StructureType,
        structure := StructureSpecs,
        key := Key
    } = Opts,
    SystemFileSpecs = case Opts of
        #{kernel_image := KernelLocalPath, kernel_path := KernelTargetPath}
          when KernelLocalPath =/= undefined, KernelTargetPath =/= undefined ->
            [#{name => <<"kernel">>,
               local => KernelLocalPath,
               target => KernelTargetPath} | SystemFileSpecs0];
        _ ->
            SystemFileSpecs0
    end,
    PackagerOpts = #{
        tarball => Tar,
        name => maps:get(name, Opts),
        version => maps:get(version, Opts),
        vcs => maps:get(vcs, Opts, undefined),
        uuid => maps:get(uuid, Opts, undefined),
        author => maps:get(author, Opts, undefined),
        description => maps:get(desc, Opts, undefined),
        architecture => maps:get(architecture, Opts),
        platform => maps:get(platform, Opts, undefined),
        block_size => maps:get(block_size, Opts),
        key => Key,
        system => maps:get(rootfs_image, Opts, undefined),
        bootloader => maps:get(bootloader_image, Opts, undefined),
        files => SystemFileSpecs,
        StructureType => StructureSpecs
    },
    case grisp_update_packager:package(OutputPath, PackagerOpts) of
        ok ->
            erlang:halt(0);
        {error, Reason} ->
            command_error("Failed to generate package: ~p~n", [Reason]),
            erlang:halt(1)
    end.
