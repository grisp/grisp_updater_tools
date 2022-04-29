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
            Cmd = command(OptList),
            perform(Cmd, validate(Cmd, proplists_to_map(OptList)));
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

validate(package, #{structure := Struct} = Opts) ->
    required(Opts, name, "software product name"),
    required(Opts, version, "software product version"),
    required(Opts, structure, "filesystem structure"),
    required(Opts, rootfs_image, "rootfs image file"),
    required(Opts, output_dir, "output directory"),
    file_exists(Opts, rootfs_image, "root file-system image"),
    does_not_exist(Opts, output_dir, "output directory"),
    file_exists(Opts, bootloader_image, "bootloader image"),
    file_exists(Opts, kernel_image, "kernel image"),
    required_if(Opts, kernel_image, kernel_path, "kernel path"),
    required_if(Opts, kernel_path, kernel_image, "kernel image"),
    Opts#{structure => parse_structure(Struct)};
validate(_Cmd, Opts) ->
    Opts.

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
        error -> ok;
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
                            ok
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

parse_structure("mbr=" ++ Struct) ->
    {mbr, [{partition_role(Role), mbr_type(Type),
            list_to_integer(Start), list_to_integer(Size)}
           || [Role, Type, Start, Size]
           <- [string:split(P, ":", all)
               || P <- string:split(Struct, ",", all)]]};
parse_structure("gpt=" ++ Struct) ->
    {gpt, [{partition_role(Role), uuid:string_to_uuid(gpt_type(Type)),
            uuid:string_to_uuid(UUID), list_to_integer(Start),
            list_to_integer(Size)}
           || [Role, Type, UUID, Start, Size]
                <- [string:split(P, ":", all)
                    || P <- string:split(Struct, ",", all)]]}.

partition_role("system") -> system;
partition_role("boot") -> boot;
partition_role("user") -> user.

mbr_type("dos") -> dos.

gpt_type("L") -> "0fc63daf-8483-4772-8e79-3d69d8477de4";
gpt_type("S") -> "0657fd6d-a4ab-43c4-84e5-0933c84b4f4f";
gpt_type("H") -> "933ac7e1-2eb4-4f13-b844-0e14e2aef915";
gpt_type("U") -> "c12a7328-f81f-11d2-ba4b-00a0c93ec93b";
gpt_type("R") -> "a19d880f-05fc-4d3b-a006-743f0f84911e";
gpt_type("V") -> "e6d6d379-f507-44c2-a23c-238f2a3df928";
gpt_type("F") -> "ebd0a0a2-b9e5-4433-87c0-68b6b72699c7";
gpt_type(UUID) when is_list(UUID) -> UUID.

usage() ->
    getopt:usage(opts_spec(), "grisp_updater_tools"),
    erlang:halt(0).

package(#{architecture := Arch} = Opts) ->
    #{
        name := ProductName,
        version := ProductVersionOpt,
        structure := FilesystemStructure,
        rootfs_image := RootFilesytemFilename,
        output_dir := OutpuDir
    } = Opts,
    ProductVersion = parse_version(ProductVersionOpt),
    ok = filelib:ensure_dir(filename:join(OutpuDir, ".")),
    ManifestFilename = filename:join(OutpuDir, "MANIFEST"),
    RevManifest = [
        {structure, structure(FilesystemStructure)},
        {architecture, iolist_to_binary(Arch)},
        {description, unicode:characters_to_binary(maps:get(desc, Opts, ""))},
        {version, ProductVersion},
        {product, unicode:characters_to_binary(ProductName)},
        {format, {1, 0, 0}}
    ],
    Objs = bootloader_objects(Opts, OutpuDir, []),
    Objs2 = kernel_objects(Opts, OutpuDir, Objs),
    Objs3 = rootfs_objects(Opts, OutpuDir, RootFilesytemFilename, Objs2),
    RevManifest2 = [{objects, lists:reverse(Objs3)} | RevManifest],
    ManifestText = io_lib:format("%% coding: utf-8~n~tp.~n",
                                 [lists:reverse(RevManifest2)]),
    ManifestData = unicode:characters_to_binary(ManifestText),
    file:write_file(ManifestFilename, ManifestData),
    erlang:halt(0).

structure({mbr, Structure}) ->
    {mbr, [
        {sector_size, 512},
        {partitions,  partitions_mbr(Structure, 0, [])}
    ]};
structure({gpt, Structure}) ->
    {gpt, [
        {sector_size, 512},
        {partitions,  partitions_gpt(Structure, 0, [])}
    ]}.

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

rootfs_objects(Opts, OutputDir, InputFilename, Objs) ->
    Blocks = generate_blocks(Opts, OutputDir, InputFilename, <<"rootfs">>),
    [{rootfs, [
        {actions, [setup, update]},
        {target, {raw, [{context, system}, {offset, 0}]}},
        {content, Blocks}
    ]} | Objs].

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
