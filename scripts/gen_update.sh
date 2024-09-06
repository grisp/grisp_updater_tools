#!/bin/bash

source "$( dirname "$0" )/sdifa.sh"

set -o errtrace
set -o nounset

ROOT="$( cd $( dirname $0 ); pwd )"

GRISP_UPDATE_TOOLS="${ROOT}/grisp_updater_tools"
BAREBOX_IMG_FILENAME="barebox-phytec-phycore-imx6ull-emmc-512mb.img"
BAREBOX_RELDIR="barebox"
DEFAULT_BASENAME="grisp2"
IMAGE_FILE_EXT=".img"
DTB_FILE="imx6ul-grisp2.dtb"
REBAR_PROFILE="prod"

IMAGE_FILE=""
SOFTWARE_PACKAGE=""
EXTRA_EXT=""
TOOLCHAIN_ROOT=""
BAREBOX_IMG=""
ERLANG_APP_DIR="$PWD"
ERLANG_APP_NAME=""
ERLANG_APP_VSN=""
SIGNING_KEY=""
FORCE=0
TRUNCATE=1
RTEMS_SHELL=0
COMPRESS_IMAGE=0
PACKAGE_BLOCK_SIZE=4
KEEP_ROOTFS=0

function usage() {
	local code="${1:-0}"
	echo "USAGE: $0 [-h] [-d] [-f] [-s] [-z] -t TOOLCHAIN_ROOT -n APP_NAME -v APP_VSN [-r REBAR_PROFILE] [-i BOOTLOADER_IMG] [-a ERLANG_APP_DIR] [-i OUTPUT_IMG] [-p SOFTWARE_PACKAGE] [-b PACKAGE_BLOCK_SIZE] [-c DEVICE_TREE_FILE] [-K PEM_FILE]"
	echo "  -h: Show this help"
	echo "  -d: Show debugging"
	echo "  -r: rebar3 profile to use when deploying"
	echo "    default: $REBAR_PROFILE"
	echo "  -f: Force the overwrite of the output files"
	echo "  -s: Include the RTEMS shell application in the image"
	echo "  -z: Compress the output image; will add extention '.gz' to output file"
	echo "  -t TOOLCHAIN_ROOT: GRiSP 2 full toolchain directory"
	echo "    e.g. /opt/grisp/grisp2-rtems-toolchain"
	echo "  -n APP_NAME: Name of the erlang release do deploy"
	echo "    e.g. grisp_demo"
	echo "  -v APP_VSN: Version of the erlang release do deploy"
	echo "    e.g. 0.1.0"
	echo "  -i BOOTLOADER_IMG: GRiSP 2 bootloader image"
	echo "    default: TOOLCHAIN_ROOT/$BAREBOX_RELDIR/$BAREBOX_IMG_FILENAME"
	echo "  -a ERLANG_APP_DIR: Erlang application to be deployed on partition A"
	echo "    default: $ERLANG_APP_DIR"
	echo "  -i OUTPUT_IMG: Output image"
	echo "    default: ${DEFAULT_BASENAME}.APP_NAME.APP_VSN.${IMAGE_FILE_EXT}[.gz]"
	echo "  -p SOFTWARE_PACKAGE: Output update package"
	echo "    default: ${DEFAULT_BASENAME}.APP_NAME.APP_VSN.tar"
	echo "  -b PACKAGE_BLOCK_SIZE: Size of update blocks in MiB"
	echo "    default: $PACKAGE_BLOCK_SIZE"
	echo "  -c DEVICE_TREE_FILE: The name of the device tree file in the toolchain"
	echo "    default: ${DTB_FILE}"
	echo "  -k: Keep rootfs file. if -z is specified it will be compressed."
	echo "  -K: The private key to use to sign the update package"
	echo "e.g."
	echo "       $0 -sz -t /opt/grisp/grisp2-rtems-toolchain -r prod -a ../grisp_demo -n grisp_demo -v 0.1.0"
	echo
	exit $code
}

function error() {
	local code="$1"
	shift
	local msg="$@"
	echo "ERROR: $msg ($code)"
	usage
}

sdifa_check

while getopts "hdfszt:i:a:n:v:o:u:b:c:r:kK:" o; do
	case "${o}" in
		h)
			usage
			;;
		d)
			export DIAGNOSTIC=1
			set -x
			;;
		f)
			FORCE=1
			;;
		s)
			RTEMS_SHELL=1
			;;
		z)
			COMPRESS_IMAGE=1
			EXTRA_EXT=".gz"
			;;
		t)
			TOOLCHAIN_ROOT="${OPTARG}"
			;;
		i)
			BAREBOX_IMG="${OPTARG}"
			;;
		a)
			ERLANG_APP_DIR="${OPTARG}"
			;;
		n)
			ERLANG_APP_NAME="${OPTARG}"
			;;
		v)
			ERLANG_APP_VSN="${OPTARG}"
			;;
		o)
			IMAGE_FILE="${OPTARG}"
			;;
		u)
			SOFTWARE_PACKAGE="${OPTARG}"
			;;
		b)
			PACKAGE_BLOCK_SIZE="${OPTARG}"
			;;
		c)
			DTB_FILE="${OPTARG}"
			;;
		r)
			REBAR_PROFILE="${OPTARG}"
			;;
		k)
			KEEP_ROOTFS=1
			;;
		K)
			SIGNING_KEY="${OPTARG}"
			;;
		*)
			usage 1
			;;
	esac
done
shift $((OPTIND-1))

if [[ "$TOOLCHAIN_ROOT" == "" ]]; then
	error 1 "Missing toolchain parameter (-t)"
fi

if [[ "$BAREBOX_IMG" == "" ]]; then
	BAREBOX_IMG="$( cd "$TOOLCHAIN_ROOT/$BAREBOX_RELDIR"; pwd )/$BAREBOX_IMG_FILENAME"
fi

if [ ! -d "$TOOLCHAIN_ROOT" ]; then
	error 1 "GRiSP 2 toolchain not found at $TOOLCHAIN_ROOT"
fi

if [ ! -f "$BAREBOX_IMG" ]; then
	error 1 "GRiSP 2 bootloader not found at $BAREBOX_IMG"
fi

if [[ "$ERLANG_APP_NAME" == "" ]]; then
	error 1 "Missing Erlang application release name parameter (-n)"
fi

if [[ $ERLANG_APP_VSN == "" ]]; then
	error 1 "Missing Erlang application release version parameter (-v)"
fi

ERLANG_APP_DIR="$( cd $( dirname "$ERLANG_APP_DIR" ); pwd )/$( basename "$ERLANG_APP_DIR" )"

if [ ! -d "$ERLANG_APP_DIR" ]; then
	error 1 "Erlang application directory not found: $ERLANG_APP_DIR"
fi

if [ ! -f "${ERLANG_APP_DIR}/rebar.config" ]; then
	error 1 "Erlang application not found in $ERLANG_APP_DIR"
fi

if [[ $IMAGE_FILE == "" ]]; then
	IMAGE_FILE="${PWD}/${DEFAULT_BASENAME}.${ERLANG_APP_NAME}.${ERLANG_APP_VSN}${IMAGE_FILE_EXT}"
else
	IMAGE_FILE="$( cd $( dirname "$IMAGE_FILE" ); pwd )/$( basename "$IMAGE_FILE" )"
fi

if [ ! -d $(dirname "$IMAGE_FILE") ]; then
	error 1 "Ouput file directory not found: $(dirname "$IMAGE_FILE")"
fi

if [[ $SOFTWARE_PACKAGE == "" ]]; then
	SOFTWARE_PACKAGE="${PWD}/${DEFAULT_BASENAME}.${ERLANG_APP_NAME}.${ERLANG_APP_VSN}"
else
	SOFTWARE_PACKAGE="$( cd $( dirname "$SOFTWARE_PACKAGE" ); pwd )/$( basename "$SOFTWARE_PACKAGE" )"
fi

if [ ! -d $(dirname "$SOFTWARE_PACKAGE") ]; then
	error 1 "Ouput update package directory not found: $(dirname "$SOFTWARE_PACKAGE")"
fi

if ! [[ "$PACKAGE_BLOCK_SIZE" =~ ^[0-9]+$ ]]; then
	error 1 "Invalid update block size: $PACKAGE_BLOCK_SIZE"
fi

if [[ $SIGNING_KEY != "" ]]; then
	if [ ! -f "$SIGNING_KEY" ]; then
		error 1 "Signing key PEM file not found: $SIGNING_KEY [$(test -f "$SIGNING_KEY")]"
	fi
fi

if [[ $FORCE == 1 ]]; then
	rm -f "${IMAGE_FILE}${EXTRA_EXT}"
	rm -f "${SOFTWARE_PACKAGE}.tar"
else
	if [ -f "${IMAGE_FILE}${EXTRA_EXT}" ]; then
		error 1 "Output file already exists: ${IMAGE_FILE}${EXTRA_EXT}"
	fi
	if [ -f "${SOFTWARE_PACKAGE}.tgz" ]; then
		error 1 "Output update package already exists: ${SOFTWARE_PACKAGE}.tar"
	fi
fi

echo "**************************************************"
echo "*** BOOTLOADER:  $BAREBOX_IMG"
echo "*** ERLANG APP:  $ERLANG_APP_DIR"
echo "*** OUTPUT FILE: ${IMAGE_FILE}${EXTRA_EXT}"
echo "**************************************************"

echo "*** CREATING BASE IMAGE FILE..."
sdifa_image_create "$IMAGE_FILE" 516

echo "*** WRITING BOOTLOADER..."
sdifa_image_write "$BAREBOX_IMG"

echo "*** WRITING PARTITION TABLE..."
sdifa_partition_fat dos 8192 524288 dos 532480 524288

echo "*** FORMATING SYSTEM PARTITION A..."
sdifa_format 0 vfat "" GRISP2A

echo "*** MOUNTING SYSTEM PARTITION A..."
sdifa_mount 0
MOUNTPOINT=$( sdifa_mountpoint 0 )

echo "*** COMPILING ERLANG APPLICATION..."
cd "$ERLANG_APP_DIR"
rebar3 as "$REBAR_PROFILE" compile

echo "*** DEPLOYING ERLANG APPLICATION TO SYSTEM PARTITION A..."
rebar3 as "$REBAR_PROFILE" grisp deploy --destination="$MOUNTPOINT" --pre-script="true" --post-script="true" --relname="$ERLANG_APP_NAME" --relvsn="$ERLANG_APP_VSN"

if [[ $RTEMS_SHELL == 1 ]]; then
	echo "*** BUILDING RTEMS SHELL..."
	cd "$TOOLCHAIN_ROOT"
	make demo
	echo "*** DEPLOYING RTEMS DEMO TO SYSTEM PARTITION A..."
	cp demo/b-imx7/demo.zImage "${MOUNTPOINT}/shell.zImage"
	DTBFILE="$( cd "$MOUNTPOINT"; find . -name "$DTB_FILE" -print -quit )"
	if [ ! -f "${MOUNTPOINT}/${DTBFILE}" ]; then
		echo "ERROR: Device tree not found in deployed erlang application"
		false # jump to the cleanup hook
	fi
	cat > "${MOUNTPOINT}/loader/entries/rtems.conf" <<-EOF
		title        GRiSP RTEMS Shell
		version      0.1.0
		linux        /shell.zImage
		devicetree   $DTBFILE
		architecture ARM
	EOF
fi

echo "*** UNMOUNTING SYSTEM PARTITION A..."
sdifa_unmount 0

if [[ $TRUNCATE == 1 ]]; then
	echo "*** TRUNCATING SYSTEM PARTITION B..."
	sdifa_truncate
fi

echo "*** PREPARING SOFTWARE PACKAGE..."
rm -rf "${SOFTWARE_PACKAGE}"
ROOTFS_IMAGE="${SOFTWARE_PACKAGE}.rootfs"
BOOTLOADER_IMAGE="${SOFTWARE_PACKAGE}.bootloader"
rm -f "${ROOTFS_IMAGE}"
rm -f "${BOOTLOADER_IMAGE}"
sdifa_extract_partition 0 "${ROOTFS_IMAGE}"
BOOTLOADER_SIZE=$( sdifa_filesize "$BAREBOX_IMG" )
sdifa_extract 0 $BOOTLOADER_SIZE "${BOOTLOADER_IMAGE}"
$GRISP_UPDATE_TOOLS --name="$ERLANG_APP_NAME" \
                    --version="$ERLANG_APP_VSN" \
                    --bootloader-image="$BOOTLOADER_IMAGE" \
                    --tar \
                    ${SIGNING_KEY:+--key-file="$SIGNING_KEY"} \
                    "mbr=system:fat:8192:524288,system:fat:532480:524288" \
                    "${ROOTFS_IMAGE}" \
                    "${SOFTWARE_PACKAGE}.tar"

rm -f "${BOOTLOADER_IMAGE}"

if [[ $KEEP_ROOTFS == 1 ]]; then
	if [[ $COMPRESS_IMAGE == 1 ]]; then
		gzip -f "${ROOTFS_IMAGE}"
	fi
else
	rm -f "${ROOTFS_IMAGE}"
fi


if [[ $COMPRESS_IMAGE == 1 ]]; then
	echo "*** COMPRESSING OUTPUT IMAGE AND CLEANING UP..."
else
	echo "*** CLEANING UP..."
fi
sdifa_commit "$COMPRESS_IMAGE"

echo "**************************************************"
echo "*** DONE"
echo "**************************************************"
echo "*** - Copy the image to an SD card:"
echo "***     macOS: $ cp \"${IMAGE_FILE}${EXTRA_EXT}\" /Volumes/GRISP"
echo "***            $ diskutil umount /Volumes/GRISP"
echo "***     Linux: $ cp \"${IMAGE_FILE}${EXTRA_EXT}\" /media/$USER/GRISP"
echo "***            $ umount /media/$USER/GRISP"
echo "*** - Open a serial console to the GRiSP board"
echo "***     $ screen /dev/tty.usbserial-010031 115200"
echo "*** - Insert the SD card"
echo "*** - Reset the GRiSP board using the onboard reset button"
echo "*** - Enter into barbox console mode by pressing any key before 3 seconds"
echo "*** - Execute command:"
echo "***     $ uncompress /mnt/mmc/$( basename "${IMAGE_FILE}${EXTRA_EXT}" ) /dev/mmc1"
echo "*** - Remove the SD card"
echo "*** - Reset the GRiSP board again"
echo "**************************************************"

if [[ $RTEMS_SHELL == 1 ]]; then
	echo "*** To boot the RTEMS shell, drop to barebox console by pressing"
	echo "*** any key before 3 seconds, then run the command:"
	echo "***     $ boot -m /mnt/mmc1.0"
	echo "*** and select the entry 'GRiSP RTEMS Shell'"
	echo "**************************************************"
fi
