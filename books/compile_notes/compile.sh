help_message=$(cat << EOF
usage: bash compile.sh help|(html|latex INPUT_FILE)

	html: Convert ``INPUT_FILE`` to an HTML PDF.
	latex: Create ``INPUT_FILE`` to a LaTeX PDF.
	help: Print this help message and exit.
EOF
)


handler_user_args(){
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
					echo "Conv $2 to HTML PDF."
					;;

				latex)
					echo "Conv $2 to LaTeX PDF."
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

handler_user_args "$@"

# pandoc --mathjax 1_boolean_logic.md --template template.html --highlight-style pygments -o 1_boolean_logic.html
# pandoc --mathjax 1_boolean_logic.md --highlight-style pygments --include-in-header before.tex -o out.pdf
