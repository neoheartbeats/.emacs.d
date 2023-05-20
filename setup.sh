#!/usr/bin/env bash

siteLispDir="$HOME/.emacs.d/site-lisp"
fontsDIr="$HOME/.local/share/fonts"

make_orglisp(){
	[ -d ${siteLispDir} ] || mkdir -p ${siteLispDir}
	git clone https://git.tecosaur.net/tec/org-mode.git ${siteLispDir}/org-mode
	if [ "$?" -ne 0 ]; then echo "Failed."; exit 1; fi
	cd ${siteLispDir}/org-mode/ && make autoloads
	mv ${siteLispDir}/org-mode/lisp/ ${siteLispDir}/org-lisp/
	rm -rf ${siteLispDir}/org-mode/
}

create_dir(){ 
	echo -e "DONE.\n I am going to create following dirs to ensure this working:"
	echo -e '1. "~/Developer/"\n2. "~/Shelter/"'
	read -e -p "Whether to create the above folder? [Y/n]" args
	[[ -z "${args}" ]] && args="y"
	[[ "$args" == [Yy] ]] && mkdir -p $HOME/Shelter/{dates,notes,repos} $HOME/Developer
}

install_fonts(){
	if cat /etc/issue | grep -q -E -i "debian|ubuntu"; then
		[-d $fontsDir/PesMono ] && mkdir -p $fontsDir/PesMono
		FONTSURL="https://raw.githubusercontent.com/ilyaw39/PesMono/main/pes-mono.ttf"
		wget -q -N -t2 -T3 $FONTSURL -O $fontsDir/PesMono/pes-mono.ttf ||
			wget -q -N -t2 -T3 https://cdn.jsdelivr.net/gh/ilyaw39/PesMono@master/pes-mono.ttf -O $fontsDir/PesMono/pes-mono.ttf ||
			wget -q -N -t2 -T3 https://ghproxy.com/$FONTSURL -O $fontsDir/PesMono/pes-mono.ttf
		mkfontdir && fc-cache -f
		[ $(fc-list| grep -q pes-mono) ] || echo -e "\033[31mPesMono font installation failed.Please check.\033[0m"
	fi
}

install_dep(){
	if command -v brew >/dev/null 2>&1;then
		COMMAND="brew"
	elif cat /etc/issue | grep -q -E -i "debian|ubuntu";then
		COMMAND="sudo apt-get"
	else
		echo -e "\033[033;31mFor unsupported systems, please install the dependencies manually.\033[0m"
		exit 1
	fi
	[ $(command -v rg >/dev/null 2>&1) ] || $COMMAND install ripgrep
	[ $(command -v sqlite3 >/dev/null 2>&1) ] || $COMMAND install sqlite3
}

[ -d $HOME/Shelter/dates ] && [ -d $HOME/Shelter/notes ] && [ -d $HOME/Shelter/repos ] && \
	[ -d $HOME/Developer ] || create_dir
make_orglisp
install_fonts
install_dep

echo 'Please following the final steps before starting your Emacs:'
echo '1. Install the font "Pes Mono" from "https://github.com/ilyaw39/PesMono/".'
echo '2. Install the font SF Pro from Apple (for display for icons).'
echo '3. Ensure "ripgrep" and "sqlite3" is in your PATH.'
echo '4. Make sure TeX Live 2023 is correctly installed on your system.'
