-module(grisp_updater_tools).


%--- Exports -------------------------------------------------------------------

%% API functions
-export([main/1]).


%--- Macros --------------------------------------------------------------------

-define(MIN_ZIP_PERCENT, 3).


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
    {desc,                    $d,               "desc",  string,
         "Software product description"},
    {block_size,              $b,         "block-size", {integer, 4194304},
         "Update block size"},
    {key_file,         undefined,           "key-file",  string,
         "Signing key PEM file"},
    {bootloader_image, undefined,   "bootloader-image",  string,
         "Bootloader image"},
    {bootloader_ver,   undefined, "bootloader-version",  string,
         "Bootloader version"},
    {kernel_path,      undefined,     "kernel-path",     string,
         "Kernel path"},
    {kernel_image,     undefined,       "kernel-image",  string,
         "Kernel image"},
    {architecture,     undefined,       "architecture",  {string, "arm-grisp2-rtems"},
         "Architecture; e.g. arm-grisp2-rtems, x86_64-vendor-linux"},
    {system_file,      undefined,        "system-file",    string,
         "Extra system file as NAME:SYSTEM_PATH:REAL_PATH[:REAL_URL]; "
         "e.g. key:/boot/rootfs_key:build/rootfs.ext2.encrypted.key:https://server.com/rootfs.ext2.encrypted.key"},
    {structure,        undefined,            undefined,  string,
         "Filesystem structure, either MBR or GPT; "
         "mbr=(START:SIZE[,])* e.g. mbr=24576:507904,548864:507904 "
         "gpt=(TYPE:UUID:START:SIZE[,])* e.g. gpt=L:9e38701c-cc06-4f03-bdb9-f5ff687476da:24576:507904,L:56ca8d1e-1e63-43e7-89af-b1d97dfff15f:548864:507904"},
    {rootfs_image,     undefined,            undefined,  string,
         "Root filesystem image"},
    {output_dir,       undefined,            undefined,  string,
         "Output directory"}
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
    Struct = maps:get(structure, Opts, undefined),
    SysFiles = maps:get(system_file, Opts, []),
    required(Opts, name, "software product name"),
    required(Opts, version, "software product version"),
    required(Opts, structure, "filesystem structure"),
    required(Opts, rootfs_image, "rootfs image file"),
    required(Opts, output_dir, "output directory"),
    file_exists(Opts, rootfs_image, "root file-system image"),
    does_not_exist(Opts, output_dir, "output directory"),
    file_exists(Opts, bootloader_image, "bootloader image"),
    file_exists(Opts, kernel_image, "kernel image"),
    KeyFile = file_exists(Opts, key_file, "signing key PEM file"),
    required_if(Opts, kernel_image, kernel_path, "kernel path"),
    required_if(Opts, kernel_path, kernel_image, "kernel image"),
    Opts#{structure => parse_structure(Struct),
          signing_key => load_key(KeyFile),
          system_file => parse_sysfiles(SysFiles)};
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
        {ok, _} -> ok;
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
    {mbr, [{partition_role(Role), mbr_type(Type),
            list_to_integer(Start), list_to_integer(Size)}
           || [Role, Type, Start, Size]
           <- [string_split(P, ":", all)
               || P <- string:split(Struct, ",", all)]]};
parse_structure("gpt=" ++ Struct) ->
    {gpt, [{partition_role(Role), uuid:string_to_uuid(gpt_type(Type)),
            uuid:string_to_uuid(UUID), list_to_integer(Start),
            list_to_integer(Size)}
           || [Role, Type, UUID, Start, Size]
                <- [string:split(P, ":", all)
                    || P <- string:split(Struct, ",", all)]]};
parse_structure(Struct) ->
    command_error("Invalid specified structure: ~p~n", [Struct]).

partition_role("system") -> system;
partition_role("boot") -> boot;
partition_role("data") -> data.

mbr_type("dos") -> dos.

gpt_type("L") -> "0fc63daf-8483-4772-8e79-3d69d8477de4";
gpt_type("S") -> "0657fd6d-a4ab-43c4-84e5-0933c84b4f4f";
gpt_type("H") -> "933ac7e1-2eb4-4f13-b844-0e14e2aef915";
gpt_type("U") -> "c12a7328-f81f-11d2-ba4b-00a0c93ec93b";
gpt_type("R") -> "a19d880f-05fc-4d3b-a006-743f0f84911e";
gpt_type("V") -> "e6d6d379-f507-44c2-a23c-238f2a3df928";
gpt_type("F") -> "ebd0a0a2-b9e5-4433-87c0-68b6b72699c7";
gpt_type(UUID) when is_list(UUID) -> UUID.

parse_sysfiles(SysFiles) ->
    parse_sysfiles(SysFiles, []).

string_split(Val, Sep, Count) ->
    string_split(Val, Sep, Count - 1, []).

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
                            Tup = {list_to_binary(Name), list_to_binary(Source),
                                   list_to_binary(Target), list_to_binary(Url)},
                            parse_sysfiles(Rest, [Tup | Acc])
                    end;
                _ ->
                    command_error("Bad system file URL: ~s~n", [Url])
            end;
        [Name, "/" ++ _ = Target, Source] ->
            case filelib:is_file(Source) of
                false -> command_error("System file not found: ~s~n", [Source]);
                true ->
                    Tup = {list_to_binary(Name), list_to_binary(Source),
                           list_to_binary(Target)},
                    parse_sysfiles(Rest, [Tup | Acc])
            end;
        [_, Target, _] ->
            command_error("Bad system file target: ~s~n", [Target])
    end.

usage() ->
    getopt:usage(opts_spec(), "grisp_updater_tools"),
    erlang:halt(0).

package(Opts) ->
    Manifest = create_manifest(Opts),
    save_plain_manifest(Manifest, Opts),
    save_sealed_manifest(Manifest, Opts),
    erlang:halt(0).

save_plain_manifest(Manifest, Opts) ->
    #{output_dir := OutpuDir} = Opts,
    ok = filelib:ensure_dir(filename:join(OutpuDir, ".")),
    ManifestFilename = filename:join(OutpuDir, "MANIFEST"),
    ManifestText = io_lib:format("%% coding: utf-8~n~tp.~n", [Manifest]),
    ManifestData = unicode:characters_to_binary(ManifestText),
    file:write_file(ManifestFilename, ManifestData).

save_sealed_manifest(Manifest, #{signing_key := Key} = Opts) ->
    #{output_dir := OutpuDir} = Opts,
    Box = termseal:seal(Manifest, Key),
    SealedManifestFilename = filename:join(OutpuDir, "MANIFEST.sealed"),
    file:write_file(SealedManifestFilename, Box).

create_manifest(Opts) ->
    #{
        name := ProductName,
        architecture := Arch,
        version := ProductVersionOpt,
        structure := FilesystemStructure,
        system_file := SysFiles,
        rootfs_image := RootFilesytemFilename,
        output_dir := OutpuDir
    } = Opts,
    ProductVersion = parse_version(ProductVersionOpt),
    ok = filelib:ensure_dir(filename:join(OutpuDir, ".")),
    RevManifest = structure(FilesystemStructure) ++ [
        {architecture, iolist_to_binary(Arch)},
        {description, unicode:characters_to_binary(maps:get(desc, Opts, ""))},
        {version, ProductVersion},
        {product, unicode:characters_to_binary(ProductName)},
        {format, {1, 0, 0}}
    ],
    Objs = bootloader_objects(Opts, OutpuDir, []),
    Objs2 = kernel_objects(Opts, OutpuDir, Objs),
    Objs3 = system_objects(Opts, OutpuDir, SysFiles, Objs2),
    Objs4 = rootfs_objects(Opts, OutpuDir, RootFilesytemFilename, Objs3),
    RevManifest2 = [{objects, lists:reverse(Objs4)} | RevManifest],
    lists:reverse(RevManifest2).

structure(undefined) ->
    [];
structure({mbr, Structure}) ->
    [{structure, {mbr, [
        {sector_size, 512},
        {partitions,  partitions_mbr(Structure, 0, [])}
    ]}}];
structure({gpt, Structure}) ->
    [{structure, {gpt, [
        {sector_size, 512},
        {partitions,  partitions_gpt(Structure, 0, [])}
    ]}}].

partitions_mbr([], _Id, Acc) -> lists:reverse(Acc);
partitions_mbr([{system, T, B, S} | Rest], Id, Acc) ->
    Item = {system, [{type, T}, {id, Id}, {start, B}, {size, S}]},
    partitions_mbr(Rest, Id + 1, [Item | Acc]);
partitions_mbr([{R, T, B, S} | Rest], Id, Acc) ->
    Item = {R, [{type, T}, {start, B}, {size, S}]},
    partitions_mbr(Rest, Id, [Item | Acc]).

partitions_gpt([], _Id, Acc) -> lists:reverse(Acc);
partitions_gpt([{system, T, U, B, S} | Rest], Id, Acc) ->
    Item = {system, [{type, uuid2bin(T)}, {id, Id}, {uuid, uuid2bin(U)},
                     {start, B}, {size, S}]},
    partitions_gpt(Rest, Id + 1, [Item | Acc]);
partitions_gpt([{R, T, U, B, S} | Rest], Id, Acc) ->
    Item = {R, [{type, uuid2bin(T)}, {uuid, uuid2bin(U)},
                {start, B}, {size, S}]},
    partitions_gpt(Rest, Id, [Item | Acc]).

uuid2bin(UUID) -> iolist_to_binary(uuid:uuid_to_string(UUID)).

bootloader_objects(#{bootloader_image := Filename}, OutputDir, Objs) ->
    {ok, Data} = file:read_file(Filename),
    Block = zlib:gzip(Data),
    BlockFilename = filename:join(OutputDir, <<"bootloader.gz">>),
    ok = file:write_file(BlockFilename, Block),
    [{bootloader, [
        {actions, [setup, bootloader]},
        {product, <<"barebox">>},
        {description, <<"Barebox Bootloader">>},
        {target, {raw, [{context, global}, {offset, 0}]}},
        {content, [
            {block, [
                {data_offset, 0},
                {data_size, byte_size(Data)},
                {data_hashes, [
                    {sha256, base64:encode(crypto:hash(sha256, Data))},
                    {crc32, erlang:crc32(Data)}
                ]},
                {block_format, gzip},
                {block_size, byte_size(Block)},
                {block_hashes, [
                    {sha256, base64:encode(crypto:hash(sha256, Block))},
                    {crc32, erlang:crc32(Block)}
                ]},
                {block_path, <<"bootloader.gz">>}
            ]}
        ]}
    ]} | Objs];
bootloader_objects(_Opts, _OutputDir, Objs) ->
    Objs.

kernel_objects(#{kernel_image := Filename, kernel_path := Path} = Opts,
               OutputDir, Objs) ->
    Blocks = generate_blocks(Opts, OutputDir, Filename, <<"kernel">>),
    [{kernel, [
        {actions, [setup, update]},
        {target, {file, [{context, system}, {path, iolist_to_binary(Path)}]}},
        {content, Blocks}
    ]} | Objs];
kernel_objects(_Opts, _OutputDir, Objs) ->
    Objs.

system_objects(_Opts, _OutputDir, [], Objs) ->
    Objs;
system_objects(Opts, OutputDir, [{Name, Local, Target, Url} | Rest], Objs) ->
    Blocks = generate_refblocks(Opts, Local, Url),
    Objs2 = [{binary_to_atom(Name), [
        {actions, [setup, update]},
        {target, {file, [{context, system}, {path, Target}]}},
        {content, Blocks}
    ]} | Objs],
    system_objects(Opts, OutputDir, Rest, Objs2);
system_objects(Opts, OutputDir, [{Name, Local, Target} | Rest], Objs) ->
    Blocks = generate_blocks(Opts, OutputDir, Local, Name),
    Objs2 = [{binary_to_atom(Name), [
        {actions, [setup, update]},
        {target, {file, [{context, system}, {path, Target}]}},
        {content, Blocks}
    ]} | Objs],
    system_objects(Opts, OutputDir, Rest, Objs2).

rootfs_objects(Opts, OutputDir, InputFilename, Objs) ->
    Blocks = generate_blocks(Opts, OutputDir, InputFilename, <<"rootfs">>),
    [{rootfs, [
        {actions, [setup, update]},
        {target, {raw, [{context, system}, {offset, 0}]}},
        {content, Blocks}
    ]} | Objs].

file_info(Filename) ->
    case file:open(Filename, [raw, read, binary]) of
        {error, Reason} ->
            command_error("Error opening file ~s: ~p~n", [Filename, Reason]);
        {ok, File} ->
            try
                {Size, Crc32, HashCtx} = file_info(Filename, File, 0,
                            erlang:crc32(<<>>), crypto:hash_init(sha256)),
                {Size, Crc32, crypto:hash_final(HashCtx)}
            after
                file:close(File)
            end
    end.

file_info(Filename, File, Size, Crc32, HashCtx) ->
    case file:read(File, 256 * 1024) of
        eof -> {Size, Crc32, HashCtx};
        {ok, Data} ->
            file_info(Filename, File, Size + byte_size(Data),
                      erlang:crc32(Crc32, Data),
                      crypto:hash_update(HashCtx, Data));
        {error, Reason} ->
            command_error("Error reading file ~s: ~p~n", [Filename, Reason])
    end.

generate_refblocks(_Opts, InputFilename, Url) ->
    {DataSize, DataCrc, DataHashBin} = file_info(InputFilename),
    [{block, [
        {data_offset, 0},
        {data_size, DataSize},
        {data_hashes, [
            {sha256, base64:encode(DataHashBin)},
            {crc32, DataCrc}
        ]},
        {block_format, raw},
        {block_path, Url}
    ]}].

generate_blocks(#{block_size := Size}, OutputDir, InputFilename, SubDir) ->
    {ok, File} = file:open(InputFilename, [read, raw, binary]),
    BlockDir = filename:join(OutputDir, SubDir),
    ok = filelib:ensure_dir(filename:join(BlockDir, ",")),
    generate_blocks_loop(Size, File, BlockDir, SubDir, 0, 0, #{}, []).

generate_blocks_loop(ReadSize, File, BlockDir, SubDir, Index, Offset, Cache, Blocks) ->
    case file:read(File, ReadSize) of
        eof -> lists:reverse(Blocks);
        {ok, Data} ->
            DataSize = byte_size(Data),
            DataBinHash = crypto:hash(sha256, Data),
            BlockSpec1 = [
                {data_size, DataSize},
                {data_hashes, [
                    {sha256, base64:encode(DataBinHash)},
                    {crc32, erlang:crc32(Data)}
                ]}
            ],
            {Cache2, Index2, BlockSpec} =
                case maps:find(DataBinHash, Cache) of
                    {ok, Spec} -> {Cache, Index, Spec};
                    error ->
                        ZipBlock = zlib:gzip(Data),
                        ZipSize = byte_size(ZipBlock),
                        MaxSize = DataSize - (DataSize * ?MIN_ZIP_PERCENT div 100),
                        {Path, Block, Spec} = case ZipSize =< MaxSize of
                            true ->
                                Filename = iolist_to_binary(io_lib:format("~3..0b.gz", [Index])),
                                RelPath = filename:join(SubDir, Filename),
                                FullPath = filename:join(BlockDir, Filename),
                                BinHash = crypto:hash(sha256, ZipBlock),
                                ZipHash = base64:encode(BinHash),
                                ZipCrc = erlang:crc32(ZipBlock),
                                BlockSpec2 = BlockSpec1 ++ [
                                    {block_format, gzip},
                                    {block_size, ZipSize},
                                    {block_hashes, [
                                        {sha256, ZipHash},
                                        {crc32, ZipCrc}
                                    ]},
                                    {block_path, RelPath}
                                ],
                                {FullPath, ZipBlock, BlockSpec2};
                            false ->
                                Filename = iolist_to_binary(io_lib:format("~3..0b", [Index])),
                                RelPath = filename:join(SubDir, Filename),
                                FullPath = filename:join(BlockDir, Filename),
                                BlockSpec2 = BlockSpec1 ++ [
                                    {block_format, raw},
                                    {block_path, RelPath}
                                ],
                                {FullPath, Data, BlockSpec2}
                        end,
                        ok = file:write_file(Path, Block),
                        {Cache#{DataBinHash => Spec}, Index + 1, Spec}
                end,
            generate_blocks_loop(ReadSize, File, BlockDir, SubDir, Index2,
                                 Offset + DataSize, Cache2, [
                {block, [{data_offset, Offset} | BlockSpec]} | Blocks])
    end.

parse_version({A, B, C})
  when is_integer(A), A >= 0, is_integer(B), B >= 0, is_integer(C), C >= 0 ->
    {A, B, C};
parse_version(N) when is_integer(N) ->
    {N, 0, 0};
parse_version(Str) when is_list(Str) ->
    parse_version(unicode:characters_to_binary(Str));
parse_version(Bin) when is_binary(Bin) ->
    case re:run(Bin, "^([0-9]*)\.([0-9]*)\.([0-9]*)$",
                [{capture, all, binary}]) of
        {match, [_, A, B, C]} ->
            {binary_to_integer(A), binary_to_integer(B), binary_to_integer(C)};
        _ ->
            Bin
    end.
