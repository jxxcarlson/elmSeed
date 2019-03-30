sed  's/URL/http:\/\/localhost:4000/' ./robot/files/config.txt | sed 's/SITE/LOCAL/' > src/Configuration.elm
elm make --optimize src/Main.elm --output=Main.js