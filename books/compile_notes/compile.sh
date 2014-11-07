#! /bin/bash

# Description:
#   A tool to compile my kramdown markdown files into either HTML or LaTeX
#   PDFs.
#
# Use:
#   bash compile.sh help

help_message=$(cat << EOF
usage: bash compile.sh help|(html|latex INPUT_FILE)

	html: Convert ``INPUT_FILE`` to an HTML PDF.
	latex: Create ``INPUT_FILE`` to a LaTeX PDF.
	help: Print this help message and exit.
EOF
)

handler_user_args(){
	# Respond to command-line user arguments.
	#
	# Args:
	#   $* : All command-line arguments.

	misuse_exit(){
		>&2 echo "$help_message"
		exit 1
	}

	case $1 in
		html|latex)
			if [ $# -ne 2 ]; then
				>&2 echo "Insufficient arguments: missing INPUT_FILE."
				misuse_exit
			fi

			case $1 in
				html)
					compile_html $2
					;;

				latex)
					compile_latex $2
					;;
			esac
			;;
		help)
			echo "$help_message"
			;;
		*)
			>&2 echo "Unrecognized or missing command."
			misuse_exit
			;;
	esac
}

compile_latex(){
	# Compile the file to a LaTeX style PDF.
	#
	# Args:
	#   $1: The markdown file path.

	local filename=${1%.*}
	pandoc $1 \
		--include-in-header $EXEC_DIR/header.tex \
		-o $filename.pdf
}

compile_html(){
	# Compile the file to an HTML style PDF.
	#
	# Args:
	#   $1: The markdown file path.

	local filename=${1%.*}
	local compiled_html="$filename.html"
	pandoc --mathjax $1 \
		--template $EXEC_DIR/template.html \
		--highlight-style pygments \
		--variable exec_dir="$EXEC_DIR" \
		-o $compiled_html
	wkhtmltopdf --javascript-delay 2000 \
		$compiled_html $filename.pdf
	rm $compiled_html
}

EXEC_DIR="$(readlink -f ${0%/*})"
handler_user_args "$@"
