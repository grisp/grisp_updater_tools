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
    {structure,        undefined,            undefined,  string,
         "Filesystem structure as (START:SIZE[,])*; e.g. 24576:507904,548864:507904"},
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
    Opts#{structure => parse_structure(Struct)};
validate(_Cmd, Opts) ->
    Opts.

required(Opts, Name, Desc) ->
    case maps:find(Name, Opts) of
        {ok, _} -> ok;
        error -> command_error("Missing ~s argument~n", [Desc])
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

parse_structure(Struct) ->
    [{system, dos, A, B}
     || [A, B] <- [[list_to_integer(I) || I <- string:split(P, ":")]
                   || P <- string:split(Struct, ",")]].

usage() ->
    getopt:usage(opts_spec(), "grisp_updater_tools"),
    erlang:halt(0).

package(Opts) ->
    #{
        name := ProductName,
        version := ProductVersion,
        structure := FilesystemStructure,
        rootfs_image := RootFilesytemFilename,
        output_dir := OutpuDir
    } = Opts,
    ok = filelib:ensure_dir(filename:join(OutpuDir, ".")),
    ManifestFilename = filename:join(OutpuDir, "MANIFEST"),
    RevManifest = [
        {structure, [
            {vfat, [
                {sector_size, 512},
                {partitions, [
                    {R, [{type, T}, {start, B}, {size, S}]}
                    || {R, T, B, S} <- FilesystemStructure
                ]}
            ]}
        ]},
        {architecture, <<"arm-grisp2-linux">>},
        {description, unicode:characters_to_binary(maps:get(desc, Opts, ""))},
        {version, unicode:characters_to_binary(ProductVersion)},
        {product, unicode:characters_to_binary(ProductName)},
        {format, {1, 0, 0}}
    ],
    Objs = bootloader_objects(Opts, OutpuDir, []),
    Objs2 = rootfs_objects(Opts, OutpuDir, RootFilesytemFilename, Objs),
    RevManifest2 = [{objects, Objs2} | RevManifest],
    ManifestText = io_lib:format("%% coding: utf-8~n~tp.~n",
                                 [lists:reverse(RevManifest2)]),
    ManifestData = unicode:characters_to_binary(ManifestText),
    file:write_file(ManifestFilename, ManifestData),
    erlang:halt(0).

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

rootfs_objects(#{block_size := Size}, OutputDir, InputFilename, Objs) ->
    {ok, File} = file:open(InputFilename, [read, raw, binary]),
    BlockDir = filename:join(OutputDir, "rootfs"),
    ok = filelib:ensure_dir(filename:join(BlockDir, ",")),
    Blocks = rootfs_objects_blocks(Size, File, BlockDir, 0, 0, #{}, []),
    [{rootfs, [
        {actions, [setup, update]},
        {target, {raw, {context, system}, {offset, 0}}},
        {content, Blocks}
    ]} | Objs].

rootfs_objects_blocks(ReadSize, File, BlockDir, Index, Offset, Cache, Blocks) ->
    case file:read(File, ReadSize) of
        eof -> lists:reverse(Blocks);
        {ok, Data} ->
            DataSize = byte_size(Data),
            DataBinHash = crypto:hash(sha256, Data),
            {Cache2, Index2, BlockSize, BlockHash, BlockCrc, BlockRelPath} =
                case maps:find(DataBinHash, Cache) of
                    {ok, {S, H, C, P}} -> {Cache, Index, S, H, C, P};
                    error ->
                        Filename = iolist_to_binary(io_lib:format("~3..0b.gz", [Index])),
                        RelPath = filename:join(<<"rootfs">>, Filename),
                        FullPath = filename:join(BlockDir, Filename),
                        Block = zlib:gzip(Data),
                        ok = file:write_file(FullPath, Block),
                        BinHash = crypto:hash(sha256, Block),
                        Size = byte_size(Block),
                        Hash = base64:encode(BinHash),
                        Crc = erlang:crc32(Block),
                        {Cache#{DataBinHash => {Size, Hash, Crc, RelPath}},
                         Index + 1, Size, Hash, Crc, RelPath}
                end,
            rootfs_objects_blocks(ReadSize, File, BlockDir, Index2,
                                  Offset + DataSize, Cache2, [
                {block, [
                    {data_offset, Offset},
                    {data_size, DataSize},
                    {data_hashes, [
                        {sha256, base64:encode(DataBinHash)},
                        {crc32, erlang:crc32(Data)}
                    ]},
                    {block_format, gzip},
                    {block_size, BlockSize},
                    {block_hashes, [
                        {sha256, BlockHash},
                        {crc32, BlockCrc}
                    ]},
                    {block_path, BlockRelPath}
                ]} | Blocks])
    end.
