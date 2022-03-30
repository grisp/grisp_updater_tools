# Shell Disk and Image Abstraction Library
#
# For a script in the same directory, it should be sourced with:
#     source "$( dirname "$0" )/sdifa.sh"

SDIFA_DEBUG=${SDIFA_DEBUG:-1}

SDIFA_ARCH="$(uname -m)"
SDIFA_OS="$(uname -s)"
case "$SDIFA_OS" in
	"CYGWIN_NT-6.1") SDIFA_OS="cygwin";;
esac
SDIFA_OS="$(echo "$SDIFA_OS" | awk '{print tolower($0)}')"

# Session variables
SDIFA_IMAGE_SIZE=""
SDIFA_IMAGE_TEMPFILE=""
SDIFA_IMAGE_TEMPFILE2=""
SDIFA_IMAGE_FILE=""
SDIFA_IMAGE_LOOPDEV=""
SDIFA_IMAGE_DEVICE=""
SDIFA_IMAGE_PART_NUM=""
SDIFA_IMAGE_PARTS=(  )
SDIFA_IMAGE_PARTS_TRUNCATED=false


### Internal Functions #########################################################

sdifa_debug() {
	if [[ $SDIFA_DEBUG ]]; then
		echo "$*"
	fi
}

sdifa_error() {
	local code="$1"
	shift
	local msg="$*"
	echo "ERROR: $msg ($code)" 1>&2
	exit $code
}

_check_image_tempfile() {
	if [[ ! -f $SDIFA_IMAGE_TEMPFILE ]]; then
		sdifa_error 1 "Image file not found: $SDIFA_IMAGE_TEMPFILE"
	fi
}

_check_has_partition() {
	local part_num=${1:-""}
	if [[ $SDIFA_IMAGE_PART_NUM == "" ]]; then
		sdifa_error 1 "No partitions defined in current image"
	fi
	if [[ $part_num != "" ]]; then
		if (( $part_num >= $SDIFA_IMAGE_PART_NUM )); then
			sdifa_error 1 "Partition $part_num not defined in current image"
		fi
	fi
}

_darwin_setup_image_device() {
	if [[ ! -f $SDIFA_IMAGE_TEMPFILE ]]; then
		sdifa_error 1 "Image file not found: $SDIFA_IMAGE_TEMPFILE"
	fi
		if [[ -e $SDIFA_IMAGE_DEVICE ]]; then
		sdifa_error 1 "Image device already created: $SDIFA_IMAGE_DEVICE"
	fi
	SDIFA_IMAGE_DEVICE=$( _trim $( hdiutil attach -imagekey diskimage-class=CRawDiskImage -nomount "$SDIFA_IMAGE_TEMPFILE" ) )
}

_darwin_ensure_image_device() {
	if [[ ! -e $SDIFA_IMAGE_DEVICE ]]; then
		_darwin_setup_image_device
	fi
}

_darwin_fat_partition_fdisk_type() {
	case "$1" in
		dos) echo "0x0B";;
		*) sdifa_error 1 "Unsupported FAT partition type: $1";;
	esac
}

_linux_fat_partition_sfdisk_type() {
	case "$1" in
		dos) echo "b";;
		*) sdifa_error 1 "Unsupported FAT partition type: $1";;
	esac
}

_trap_catch() {
	local code="$1"
	local line="$2"
	sdifa_reset
	exit $code
}

_trim() {
	echo -e "$1" | sed -e 's/^[[:blank:]]*//g' -e 's/[[:blank:]]*$//g'
}

_xml_node_values() {
	local sep="$1"
	local xml="$2"
	echo -e "$xml" | sed -e "s|<\([^ ]*\)[^>]*>\([^<]*\)\(</\1>\)|\2$sep|g"
}

_darwin_disk_partitions() {
	local disk_dev="$1"
	local part_devs_xml=$( diskutil list -plist "$disk_dev" | xmllint --xpath "/plist/dict/key[.='AllDisksAndPartitions']/following-sibling::array/dict[1]/key[.='Partitions']/following-sibling::array/dict/key[.='DeviceIdentifier']/following-sibling::string" - )
	_xml_node_values  " " "$part_devs_xml"
}

_darwin_partition_mountpoint() {
	local part_dev="$1"
	local mountpoint_xml=$( diskutil info -plist "$part_dev" | xmllint --xpath "/plist/dict/key[.='MountPoint']/following-sibling::string[1]" - )
	_trim $( _xml_node_values  " " "$mountpoint_xml" )
}


### Public Functions ###########################################################

# Checks the OS is supported and all the dependencies are available.
sdifa_check() {
	case "$SDIFA_OS" in
		linux | darwin)
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Reset an image manipulation session
sdifa_reset() {
	local keep_image=${1:-false}
	if [[ $SDIFA_IMAGE_PART_NUM != "" ]]; then
		for (( i=0; i<$SDIFA_IMAGE_PART_NUM; i++ )); do
			local part_mountpoint="${SDIFA_IMAGE_PARTS[$i*7+2]}"
			if [[ -d $part_mountpoint ]]; then
				sdifa_unmount $i
			fi
		done
	fi
	SDIFA_IMAGE_PART_NUM=""
	SDIFA_IMAGE_PARTS=(  )
	SDIFA_IMAGE_PARTS_TRUNCATED=false
	if [[ -e $SDIFA_IMAGE_DEVICE ]]; then
		hdiutil detach "$SDIFA_IMAGE_DEVICE"
	fi
	SDIFA_IMAGE_DEVICE=""
	if [ -e "$SDIFA_IMAGE_LOOPDEV" ]; then
		sudo losetup -d "$SDIFA_IMAGE_LOOPDEV"
	fi
	SDIFA_IMAGE_LOOPDEV=""
	if [ -f "$SDIFA_IMAGE_TEMPFILE2" ]; then
		rm -f "$SDIFA_IMAGE_TEMPFILE2"
	fi
	SDIFA_IMAGE_TEMPFILE2=""
	if [ -f "$SDIFA_IMAGE_TEMPFILE" ]; then
		if [[ $keep_image == keep ]]; then
			mv "$SDIFA_IMAGE_TEMPFILE" "$SDIFA_IMAGE_FILE"
		else
			rm -f "$SDIFA_IMAGE_TEMPFILE"
		fi
	fi
	SDIFA_IMAGE_TEMPFILE=""
	SDIFA_IMAGE_FILE=""
	trap - 1 2 3 15 ERR EXIT
}

sdifa_filesize() {
	stat --printf="%s" "$1"
}

# Start and image creation session with an empty image.
# Arguments:
#   1: Image file path
#   2: Image size in mebibytes
sdifa_image_create() {
	local file_path="$1"
	local image_size="$2"
	sdifa_reset
	if [[ -e $file_path ]]; then
		sdifa_error 1 "Image file already exists: $file_path"
	fi
	SDIFA_IMAGE_SIZE="$image_size"
	SDIFA_IMAGE_FILE="$file_path"
	SDIFA_IMAGE_TEMPFILE="$( mktemp )"
	case "$SDIFA_OS" in
		linux | darwin)
			dd conv=sparse,notrunc if=/dev/zero of="$SDIFA_IMAGE_TEMPFILE" bs=1048576 count="$image_size" || exit $?
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
	trap '_trap_catch $? $LINENO' 1 2 3 15 ERR EXIT
	# trap sdifa_reset 1 2 3 15 ERR
}

# Write a file into the current image.
# Arguments:
#   1: Input file to be copied into the current image
#   2: Size of the block to copy (default to 1 mebibyte)
#   3: Number of block to skip at the start of current image (default to zero)
#   4: Number of block to skip at the start of input file (default to zero)
#   5: Number of block to copy (default to all the input file)
sdifa_image_write() {
	local input_file="$1"
	local input_size=""
	local bs="${2:-1048576}"
	local seek="${3:-0}"
	local skip="${4:-0}"
	local count="${5:-}"
	_check_image_tempfile
	if [[ ! -f $input_file ]]; then
		sdifa_error 1 "Input file not found: $input_file"
	fi
	count_arg=""
	if [[ $count != "" ]]; then
		count_arg="count=$count"
	fi
	case "$SDIFA_OS" in
		linux | darwin)
			dd conv=sparse,notrunc if="$input_file" of="$SDIFA_IMAGE_TEMPFILE" "bs=$bs" "skip=$skip" "seek=$seek" $count_arg || exit $?
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Create a FAT partition table on the current image.
# Sectors are assumed to be of 512 bytes.
# Partition type can only be 'dos'.
# If not specified a partition is not defined.
# Arguments:
#   1: Type of partition 1
#   2: Starting sector of partition 1
#   3: Number of sectors in partition 1
#   4: Type of partition 2
#   5: Starting sector of partition 2
#   6: Number of sectors in partition 2
#   7: Type of partition 3
#   8: Starting sector of partition 3
#   9: Number of sectors in partition 3
#  10: Type of partition 4
#  11: Starting sector of partition 4
#  12: Number of sectors in partition 4
sdifa_partition_fat() {
	declare -a specs
	local index=0
	local last=0
	local payload=""
	local type=""
	local start=""
	local size=""

	_check_image_tempfile
	while [[ $# > 0 ]]; do
		case "$1" in
			dos)
				if ! [[ "$2" =~ ^[0-9]+$ && "$3" =~ ^[0-9]+$ ]]; then
					sdifa_error 1 "Invalid FAT partition $index"
				fi
				if (( $2  < $last )); then
					sdifa_error 1 "Overlaping partition $index"
				fi
				# Needed for 'set -u' to not fail here due to the array being empty
				if (( ${#specs[@]} == 0 )); then
					specs=( $index "" "" $1 512 $2 $3 )
				else
					specs=( "${specs[@]}" $index "" "" $1 512 $2 $3 )
				fi
				index=$(( $index + 1 ))
				last=$(( $2 + $3 ))
				shift; shift; shift
				;;
			*)
				sdifa_error 1 "Unsupported FAT partition type: $1"
				;;
		esac
	done
	case "$SDIFA_OS" in
		linux)
			payload="unit: sectors\nlabel: dos\n"
			for (( i=0; i<$index; i++ )); do
				type=$( _linux_fat_partition_sfdisk_type "${specs[$i*7+3]}" )
				start="${specs[$i*7+5]}"
				size="${specs[$i*7+6]}"
				payload="${payload}${i}: start=${start}, size=${size}, type=${type}\n"
			done
			echo -ne "$payload" | sfdisk -f "$SDIFA_IMAGE_TEMPFILE" || exit $?
			SDIFA_IMAGE_PART_NUM="$index"
			SDIFA_IMAGE_PARTS=( "${specs[@]}" )
			;;
		darwin)
			for (( i=0; i<$index; i++ )); do
				type=$( _darwin_fat_partition_fdisk_type "${specs[$i*7+3]}" )
				start="${specs[$i*7+5]}"
				size="${specs[$i*7+6]}"
				payload="${payload}${start},${size},${type}\n"
			done
			for (( ; i<4; i++ )); do
				payload="${payload}0,0,0x00\n"
			done
			_darwin_ensure_image_device
			echo -ne "$payload" | fdisk -ry "$SDIFA_IMAGE_DEVICE" || exit $?
			local part_name_list=$(_darwin_disk_partitions "$SDIFA_IMAGE_DEVICE")
			local part_names=( )
			read -r -a part_names <<< "$part_name_list"
			for (( i=0; i<$index; i++ )); do
				if (( $i >= ${#part_names[@]} )); then
					edifa_error 1 "Partition $i of disk $SDIFA_IMAGE_DEVICE without corresponding device name"
				fi
				local part_dev="/dev/${part_names[i]}"
				if ! [[ -e $part_dev ]]; then
					sdifa_error 1 "Partition $i device $part_dev of disk $SDIFA_IMAGE_DEVICE not found"
				fi
				specs[$i*7+1]="$part_dev"
			done
			SDIFA_IMAGE_PART_NUM="$index"
			SDIFA_IMAGE_PARTS=( "${specs[@]}" )
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Format a partition of the current image.
# Arguments:
#   1: Partition number (starting at 0)
#   2: Filesystem type (vfat)
#   3: Volume ID
#   4: Volume name
#   5: Cluster size (default 8)
sdifa_format() {
	local part_num="$1"
	local type="$2"
	local vol_id="${3:-}"
	local vol_name="${4:-}"
	local cluster_size="${5:-8}"
	if [[ $type != "vfat" ]]; then
		sdifa_error 1 "Partition format $type not supported"
	fi
	if (( $part_num >= ${#SDIFA_IMAGE_PARTS[@]} )); then
		sdifa_error 1 "Partition $part_num not defined in current image"
	fi
	case "$SDIFA_OS" in
		linux)
			sdifa_error 1 "NOT IMPLEMENTED"
		;;
		darwin)
			local vol_id_arg=""
			local vol_name_arg=""
			if [[ $vol_id != "" ]]; then
				vol_id_arg="-I $vol_id"
			fi
			if [[ $vol_name != "" ]]; then
				vol_name_arg="-v $vol_name"
			fi
			local part_dev="${SDIFA_IMAGE_PARTS[$part_num*7+1]}"
			local part_sec_size="${SDIFA_IMAGE_PARTS[$part_num*7+4]}"
			newfs_msdos $vol_id_arg $vol_name_arg -c "$cluster_size" -S "$part_sec_size" "$part_dev"
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Mount a partition of the current image.
# Arguments:
#   1: Partition number (starting at 0)
sdifa_mount() {
	local part_num="$1"
	_check_has_partition "$part_num"
	case "$SDIFA_OS" in
		linux)
			sdifa_error 1 "NOT IMPLEMENTED"
			;;
		darwin)
			local part_dev="${SDIFA_IMAGE_PARTS[$part_num*7+1]}"
			local part_mountpoint="${SDIFA_IMAGE_PARTS[$part_num*7+2]}"
			if [[ -d $part_mountpoint ]]; then
				sdifa_error 1 "Partition $part_num already mounted at $part_mountpoint"
			fi
			diskutil mount "$part_dev"
			local mountpoint="$( _darwin_partition_mountpoint "$part_dev" )"
			SDIFA_IMAGE_PARTS[$part_num*7+2]="$mountpoint"
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Prints the mount point of a partition.
# Arguments:
#   1: Partition number (starting at 0)
sdifa_mountpoint() {
	local part_num="$1"
	_check_has_partition "$part_num"
	local part_mountpoint="${SDIFA_IMAGE_PARTS[$part_num*7+2]}"
	echo "$part_mountpoint"
}

# Unmount a partition of the current image.
# Arguments:
#   1: Partition number (starting at 0)
sdifa_unmount() {
	local part_num="$1"
	_check_has_partition "$part_num"
	case "$SDIFA_OS" in
		linux)
			sdifa_error 1 "NOT IMPLEMENTED"
			;;
		darwin)
			local part_dev="${SDIFA_IMAGE_PARTS[$part_num*7+1]}"
			local part_mountpoint="${SDIFA_IMAGE_PARTS[$part_num*7+2]}"
			if [[ -d $part_mountpoint ]]; then
				diskutil umount "$part_dev"
				SDIFA_IMAGE_PARTS[$part_num*7+2]=""
			fi
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Terminate the current image creation session.
# Arguments:
#    1: If the image should be compressed; 0, 1, true, false (default: false)
sdifa_commit() {
	_check_image_tempfile
	local compress="${1:-false}"
	if [[ $compress == "1" || $compress == "true" ]]; then
		SDIFA_IMAGE_TEMPFILE2="$( mktemp )"
		mv "$SDIFA_IMAGE_TEMPFILE" "$SDIFA_IMAGE_TEMPFILE2" || exit $?
		gzip -c "$SDIFA_IMAGE_TEMPFILE2" > "$SDIFA_IMAGE_TEMPFILE"  || exit $?
		rm -f "$SDIFA_IMAGE_TEMPFILE2" || exit $?
		SDIFA_IMAGE_TEMPFILE2=""
		SDIFA_IMAGE_FILE="${SDIFA_IMAGE_FILE}.gz"
	fi
	sdifa_reset keep
}

# Truncate the image to only contain the first partition
sdifa_truncate() {
	_check_image_tempfile
	_check_has_partition 0
	local sec="${SDIFA_IMAGE_PARTS[4]}"
	local start="${SDIFA_IMAGE_PARTS[5]}"
	local size="${SDIFA_IMAGE_PARTS[6]}"
	local trunc_size=$((  $sec * ( $start + $size ) ))
	local trunc_blocks=$((  $trunc_size / 1048576 ))
	if (( ( $trunc_size % 1048576 ) > 0 )); then
		sdifa_error 1 "Truncated image is not a multiple of 1 Mebibyte"
	fi
	SDIFA_IMAGE_TEMPFILE2="$( mktemp )"
	mv "$SDIFA_IMAGE_TEMPFILE" "$SDIFA_IMAGE_TEMPFILE2" || exit $?
	case "$SDIFA_OS" in
		linux | darwin)
			dd conv=sparse,notrunc if="$SDIFA_IMAGE_TEMPFILE2" of="$SDIFA_IMAGE_TEMPFILE" bs=1048576 count="$trunc_blocks" || exit $?
			rm -f "$SDIFA_IMAGE_TEMPFILE2" || exit $?
			SDIFA_IMAGE_TEMPFILE2=""
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}

# Extract a partition from the current image
# Arguments:
#   1: Partition number (starting at 0)
#   2: File the partition data should be extracted into
sdifa_extract() {
	local part_num="$1"
	local part_file="$2"
	_check_has_partition "$part_num"
	if [[ -f $part_file ]]; then
		sdifa_error 1 "Partition file aleready exists: $part_file"
	fi
	local part_sec="${SDIFA_IMAGE_PARTS[$part_num*7+4]}"
	local part_start="${SDIFA_IMAGE_PARTS[$part_num*7+5]}"
	local part_size="${SDIFA_IMAGE_PARTS[$part_num*7+6]}"
	local file_start=$(( $part_sec * $part_start ))
	local file_size=$(( $part_sec * $part_size ))
	local block_start=$(( $file_start / 1048576 ))
	local block_size=$(( $file_size / 1048576 ))
	if (( ( $file_start % 1048576 ) > 0 )); then
		sdifa_error 1 "Partition $part_num do not start at a multiple of 1 Mebibyte"
	fi
	if (( ( $file_size % 1048576 ) > 0 )); then
		sdifa_error 1 "Partition $part_num size is not a multiple of 1 Mebibyte"
	fi
	case "$SDIFA_OS" in
		linux | darwin)
			dd conv=sparse,notrunc if="$SDIFA_IMAGE_TEMPFILE" of="$part_file" bs=1048576 skip="$block_start" count="$block_size" || exit $?
			;;
		*)
			sdifa_error 1 "Unsupported OS: $SDIFA_OS"
			;;
	esac
}
