# red=`tput setaf 1`
# green=`tput setaf 2`
# reset=`tput sgr0`
# echo "${red}red text ${green}green text${reset}"

color=`tput setaf 48`
red=`tput setaf 1`
reset=`tput setaf 7`

echo
echo "${red}----------------------${reset}"
echo "${red}DEPLOY BookLib App${reset}"
echo "${red}----------------------${reset}"

sed  's/URL/https:\/\/arcane-cliffs-95237.herokuapp.com\//' ./robot/files/config.txt | sed 's/SITE/REMOTE/'  > src/Configuration.elm

echo
echo "${color}Compiling .src/Main.elm with --optimize${reset}"
start=`date +%s`
elm make  --optimize ./src/Main.elm --output=Main.js
end=`date +%s`
runtime=$((end-start))
echo
echo "${magenta}Compile time: " $runtime " seconds${reset}"

echo
echo "${color}Uglifying ... ${reset}"
uglifyjs Main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=Main.min.js

echo
echo "${color}Uploading to cloud ...${reset}"
scp -r ./index-remote.html  root@206.189.184.194:/var/www/html/index.html
scp -r ./Main.min.js root@206.189.184.194:/var/www/html/
# scp -r ./Main.js root@206.189.184.194:/var/www/html/


echo
tput setaf 2; echo "${color}Done${reset}"
